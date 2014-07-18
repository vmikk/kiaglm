# Vuong Non-Nested Hypothesis Test
# based on [pscl v.1.04.4]

vuong.tst <- function(m1, m2, digits=getOption("digits"), mod.type=NULL){

	# m1 = model 1 = non-zero-inflated model
	# m2 = model 2 = zero-inflated model


  ## get predicted probabilities for both models
  m1y <- m1$y
  m2y <- m2$y
  m1n <- length(m1y)
  m2n <- length(m2y)

  if(m1n==0 | m2n==0) { return("Could not extract dependent variables from models") }
  if(m1n != m2n) {
    return( paste("Models appear to have different numbers of observations:",
               "Model 1 has ",m1n," observations;",
               "Model 2 has ",m2n," observations.",
               sep=" "))
   }
  
  if(any(m1y != m2y)) { return( "Models appear to have different values on dependent variables" ) }
  
  whichCol <- match(m1y,min(m1y):max(m1y))  ## which column, matrix of predicted probs
  
  m1p <- rep(NA,m1n)
  m2p <- rep(NA,m2n)
  p1 <- predprob(m1)   ## likelihood contributions, model 1, cond on MLEs
  p2 <- predprob(m2)   ## likelihood contributions, model 2
  for(i in 1:m1n){
    m1p[i] <- p1[i,whichCol[i]]  ## pick off correct column
    m2p[i] <- p2[i,whichCol[i]]
  }
  
  m <- log(m1p/m2p)  ## vector of likelihood ratios
  
  bad <- is.na(m) + is.nan(m) + (m==Inf) + (m==-Inf)
  # if(any(bad)){
  #   cat("NA or numerical zeros or ones encountered in fitted probabilities\n")
  #   cat("dropping these cases, but proceed with caution\n")
  # }

  ## gather up degrees of freedom
  k1 <- length(coef(m1))
  k2 <- length(coef(m2))

  ## test statistic: Long (1997) p248
  mbar <- mean(m[!bad])
  s <- sd(m[!bad])
  v <- sqrt(sum(!bad))*mbar/s

  # cat("(test-statistic is asymptotically distributed N(0,1) under the\n")
  # cat(" null that the models are indistinguishible)\n")
  
  if(v>0){
    vstat <- paste( paste("Vuong Non-Nested Hypothesis Test-Statistic =", round(v, 3)),
        ", p = ", round(1-pnorm(v), 4), sep="")

  	if(is.null(mod.type)){ txt <- "Model 1 is superior to Model 2.  " }
    if(!is.null(mod.type)){
      if(mod.type == "zip"){ txt <- "Standard Poisson regression is superior to a zero-inflated model.  " }
      if(mod.type == "zinb"){ txt <- "Negative binomial regression is superior to a zero-inflated model.  " }
    	if(mod.type == "phurd"){ txt <- "Standard Poisson regression is superior to a hurdle model.  " }
    	if(mod.type == "nbhurd"){ txt <- "Negative binomial regression is superior to a hurdle model.  " }
    }
  }

  if(v<0){
    vstat <- paste(  paste("Vuong Non-Nested Hypothesis Test-Statistic =", round(v, 3)),
            ", p = ", round(pnorm(v), 4), sep="")

	  if(is.null(mod.type)){ txt <- "Model 2 is superior to Model 1.  " }
    if(!is.null(mod.type)){
      if(mod.type == "zip"){ txt <- "Zero-inflated model is superior to a standard Poisson regression.  " }
      if(mod.type == "zinb"){ txt <- "Zero-inflated model is superior to a standard Negative binomial regression.  " }
      if(mod.type == "phurd"){ txt <- "Hurdle model is superior to a standard Poisson regression.  " }
      if(mod.type == "nbhurd"){ txt <- "Hurdle model is superior to a standard Negative binomial regression.  " }
    }
  }
  
  res <- paste(txt, vstat)
  return(res)
}

