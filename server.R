
## TO DO:
# add export of influece measures
# fix the models without intercept (Error: arguments imply differing number of rows: 0, 2)


library(car)

source("df.summary.r")    # str.df function  (==  ezPrecis from ez-package)


# transform data.frame (used for confinit)
df.ci <- function(x) {
  res <- data.frame(Variable = rownames(x), x)
  rownames(res) <- NULL
  colnames(res) <- c("Variable", "LCI", "UCI")
  return(res)
}


shinyServer(function(input, output) {


#####################
##################### # load data
#####################

parseUserData <- reactive({        # based on code by Huidong Tian  - Flexible upload data
    inFile <- input$dataFile
    if(!is.null(inFile)) {
      # Determine document format;
      ptn <- "\\.[[:alnum:]]{1,5}$"
      suf <- tolower(regmatches(inFile$name, regexpr(ptn, inFile$name)))
      
      # Options for Excel documents;
      if(suf %in% c('.xls', '.xlsx')) {
        wb <- loadWorkbook(inFile$datapath)
        sheets <- getSheets(wb)
        dat <- readWorksheet(wb, sheets[1])
          ind <- sapply(dat, is.character)         # which columns are "chr"
          dat[ind] <- lapply(dat[ind], factor)     # convert them to factor
        return(dat)
      } 
      
      # Options for txt documents;
      if(suf %in% '.txt') {
        read.table(file = inFile$datapath, header=TRUE, sep="\t")
      }

    } else {return(NULL)}
})
  

userData <- reactive({ parseUserData() })



# Maximal number of observations to exclude
output$excl.n <- renderUI({
  if(!is.null( userData() )){
    n <- nrow( userData() )
    max.n <- ceiling(90*n / 100)         # approximately 10% of data should be always preserved
    current.excl <- ceiling(10*n / 100)  # by default exclude 10% of data
    return(
    sliderInput("n.excl", label = "Observations to exclude:",
          min = 1, max = max.n, value = current.excl, step=1)
    )
  } else {
    return(NULL)
  }
})



# Data preview
output$data.descr <- renderPrint(
  if(!is.null( userData() )){
    str.df( userData() )
  } else {
    print(data.frame(Warning="Please upload your data."))
  }
)


#####################
##################### # Build the model
#####################


formulaText <- reactive({
  ff <- try( as.formula(input$ff) )
  if( any(class(ff) == "try-error")) { return(NULL) }
  if( any(class(ff) != "try-error")) { return(ff) }
})


# build model
mod <- reactive({
  if(input$ff == "") { return(NULL) } 

  if(formulaText() != "") {
  if(input$family == "binomial")          { fit <- try( glm(formulaText(), data=userData(), family=binomial())) }
  if(input$family == "gaussian")          { fit <- try( glm(formulaText(), data=userData(), family=gaussian())) }
  if(input$family == "Gamma")             { fit <- try( glm(formulaText(), data=userData(), family=Gamma())) }
  if(input$family == "inverse.gaussian")  { fit <- try( glm(formulaText(), data=userData(), family=inverse.gaussian())) }
  if(input$family == "poisson")           { fit <- try( glm(formulaText(), data=userData(), family=poisson())) }
  if(input$family == "quasibinomial")     { fit <- try( glm(formulaText(), data=userData(), family=quasibinomial())) }
  if(input$family == "quasipoisson")      { fit <- try( glm(formulaText(), data=userData(), family=quasipoisson())) }
  
  if( any(class(fit) != "try-error")) { return(fit) }
  if( any(class(fit) != "try-error")) { return(NULL) }
  }
})

# print coefficients
output$regTab <- renderPrint({
  if(!is.null(input$ff)){
    summary(mod())
  } else {
    print(data.frame(Warning="Please select Model Parameters."))
  }
})


# Diagnostic plots
output$diagnost.1 <- renderPlot({
  if(!is.null(mod())){ plot(mod(), which=1) }
})
output$diagnost.2 <- renderPlot({
  if(!is.null(mod())){ plot(mod(), which=2) }
})
output$diagnost.3 <- renderPlot({
  if(!is.null(mod())){ plot(mod(), which=3) }
})
output$diagnost.4 <- renderPlot({
  if(!is.null(mod())){ plot(mod(), which=4) }
})



######################################
######################################  Exclusion
######################################

# Plot for which coefficient we should show?
output$show <- renderUI({
  if(!is.null( mod() )){
    terms <- names(coefficients(mod()))    # all terms included in the model
    selectInput("show.terms", "Coefficient to show", terms)
  } else {
    return(NULL)
  }
})


# Calculate diagnostic measures
influence.tab <- reactive({
  if(!is.null( mod() )){
    infls <- influence.measures(mod())    # Regression Deletion Diagnostics
    infls <- data.frame(infls$infmat,
      resid = residuals(mod()),
      rstandard = rstandard(mod()),
      rstudent = rstudent(mod()))
    return(infls)
  } else {
    return(NULL)
  }
})


# Order data according to influence measures
new.datt <- reactive({
  datt <- mod()$data      # extract data

  if(input$infl.measure == "cov.r") { datt$Influence <- influence.tab()$cov.r }
  if(input$infl.measure == "cook.d") { datt$Influence <- influence.tab()$cook.d }
  if(input$infl.measure == "hat") { datt$Influence <- influence.tab()$hat }
  if(input$infl.measure == "resid") { datt$Influence <- influence.tab()$resid }
  if(input$infl.measure == "rstandard") { datt$Influence <- influence.tab()$rstandard }
  if(input$infl.measure == "rstudent") { datt$Influence <- influence.tab()$rstudent }

  datt <- datt[order(abs(datt$Influence), decreasing = TRUE), ]
  return(datt)
})


output$influence.choosed <- renderPrint({
  if(input$infl.measure == "cov.r") { ii <- "Covariance ratio" }
  if(input$infl.measure == "cook.d") { ii <- "Cook's distance" }
  if(input$infl.measure == "hat") { ii <- "Diagonal elements of the hat matrix" }
  if(input$infl.measure == "resid") { ii <- "Absolute residuals" }
  if(input$infl.measure == "rstandard") { ii <- "Standardized residuals"  }
  if(input$infl.measure == "rstudent") { ii <- "Studentized residuals" }
  res <- paste("You choosed", ii, "as influence measure.", sep=" ")
  return(res)
})



# Reactive model formula
build.model <- reactive({
  if(input$family == "binomial")          { mm <- function(x){ glm(formulaText(), data=x, family=binomial()) }}
  if(input$family == "gaussian")          { mm <- function(x){ glm(formulaText(), data=x, family=gaussian()) }}
  if(input$family == "Gamma")             { mm <- function(x){ glm(formulaText(), data=x, family=Gamma()) }}
  if(input$family == "inverse.gaussian")  { mm <- function(x){ glm(formulaText(), data=x, family=inverse.gaussian()) }}
  if(input$family == "poisson")           { mm <- function(x){ glm(formulaText(), data=x, family=poisson()) }}
  if(input$family == "quasibinomial")     { mm <- function(x){ glm(formulaText(), data=x, family=quasibinomial()) }}
  if(input$family == "quasipoisson")      { mm <- function(x){ glm(formulaText(), data=x, family=quasipoisson()) }}
  return(mm)
})


# Exclude influential observations and remember regression coefficients & their confidence intervals
excl.coeffs <- reactive({
  datt <- new.datt()       # get sorted data
  fit <- build.model()     # get fitting function

  coefss <- list()
  coefss[[1]] <- data.frame(Obs = 0,       # initial model with all observations
                  df.ci(confint(mod())),           # 95% CI for the coefficients
                  Coef = coefficients(mod()))      # coefficients

  for(i in 1:input$n.excl){         # start to exclude cases
    tmp.mod <- try( fit( datt[-(1:i), ] ) )
      if(any(class(tmp.mod) != "try-error")) { tmp.cc <- df.ci(confint(tmp.mod)) }
      if(any(class(tmp.mod) == "try-error")) { tmp.cc <- data.frame(Variable=NA, LCI=NA, UCI=NA) }
    coefss[[1+i]] <- data.frame(Obs = i, tmp.cc, Coef = coefficients(tmp.mod))
    rm(tmp.mod, tmp.cc)
  }

  coefss <- do.call(rbind, coefss)
  rownames(coefss) <- NULL

  dropped.names <- data.frame(Number = 1:input$n.excl, Rowname = rownames(datt)[1:input$n.excl])
  # new.names <- dropped.names$Rowname[match(coefss$obs, dropped.names$Rowname)]
  terms <- names(coefficients(mod())) 
  new.names <- c(
    rep(0, length(terms)),                                                        # full model
    as.numeric( as.character(rep(dropped.names$Rowname, each=length(terms)) ))    # with dropped terms
    )
  coefss <- data.frame(ObservationID = new.names, coefss)   # add real row IDs
  return(coefss)
})


# Build plot for selected coefficients
output$plot.excl <- renderPlot({
  if( is.null(input$show.terms) ) { return() }                 # nothing selected
  if( !is.null(input$show.terms) ) {
    ds <- subset(excl.coeffs(), Variable == input$show.terms)  # Extract chosen coefficient

    y.lim <- pretty(  with(ds, c(Coef, LCI, UCI) ))

    plot(1:nrow(ds), ds$Coef,
         type="b", pch=16, lty=3,
         ylab=ds$Variable[1], ylim=c(y.lim[1], y.lim[length(y.lim)]),
         xlab="Excluded case ID", xaxt="n", las=2)
    axis(1, at=1:nrow(ds), labels=ds$ObservationID)
    len = .07                         # width of CI ticks
    for (i in 1:nrow(ds)) {           # plot CI
      arrows(i, ds$Coef[i],
             i, ds$UCI[i],
             angle=90, length=len)
      arrows(i, ds$Coef[i],
             i, ds$LCI[i],
             angle=90, length=len)
    }
  }

})



# Create data table with only influential observations
otlier.data <- reactive({
	datt <- new.datt()					# influential data
	terms <- all.vars( formula(mod()) )	# extract names of all variable used in model
	res <- datt[1:input$n.excl, terms]
	res <- data.frame(res, Influence.measure = datt$Influence[1:input$n.excl])
	return(res)
})

output$out.datt <- renderPrint({ otlier.data() })

# output$eee <- renderPrint({ otlier.data() })



# summary(fit)        # display results
# confint(fit)        # 95% CI for the coefficients
# exp(coef(fit))      # exponentiated coefficients
# exp(confint(fit))   # 95% CI for exponentiated coefficients
# predict(fit, type="response")     # predicted values
# residuals(fit, type="deviance")   # residuals 




# dbs_influential_obs <- which(apply(infls$is.inf, 1, any)) 
# dbs_sans_influential_obs <- dbs1[-dbs_influential_obs,]



######################################
######################################
######################################


#debugging:  print all names and values in input  (based on Dmitry Grapov code)
output$debug<- renderPrint({
    # inptut variables:
    obj <- names(input)
    input.obj <- lapply(1:length(obj), function(i) { input[[obj[i]]]})
    names(input.obj) <- obj

    return(list(input = input.obj))
           # , output = output.obj))
})


})

