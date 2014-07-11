## ui.r
## The user interface controls the layout and appearance of the app.


# Define UI for application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel(h3("GLM robustness")),


  sidebarPanel(
    fileInput("dataFile", "Upload file", accept="application/vnd.ms-excel"),
    textInput("ff", "Model formula", ""),
    helpText("Example:   y ~ x1 * x2"),
    radioButtons("family", label = "Family:",
          choices = list(
            "Gaussian" = "gaussian",
            "Inverse Gaussian" = "inverse.gaussian",
            "Gamma" = "Gamma",
            "Binomial" = "binomial",
            "Poisson" = "poisson",
            "Negative Binomial" = "negbinom",             # [MASS]      
            "Quasibinomial" = "quasibinomial",
            "Quasipoisson" = "quasipoisson",
            "Zero-Inflated Poisson" = "zip",              # [pscl]
            "Zero-Inflated Negative Binomial" = "zinb"    # [pscl]
            # ,
            # "Zero-Truncated Poisson" = "ztp",             # [VGAM]
            # "Zero-Truncated Negative Binomial" = "ztnb"   # [VGAM]
            # hurdl maybe ...
            # tweedie
            ),
          selected = "gaussian"),
    br(),
    uiOutput("excl.n"),
    br(),
    radioButtons("infl.measure", "Regression diagnostics to use:",
              list("Cook's distance" = "cook.d",
                   "Diagonal elements of the hat matrix" = "hat",
                   "Covariance ratio" = "cov.r",
                   "DFFITS" = "dffit",
                   "Absolute residuals" = "resid",
                   "Standardized residuals" = "rstandard",
                   "Studentized residuals" = "rstudent"
              ), selected = "cook.d"),
    helpText("Note: Only residuals for Zero-Inflated models is available."),
    br(),
    uiOutput("show"),
    submitButton("Build model")
  ),  # end of sidebarPanel


  # Tabs with results
  mainPanel(

    tabsetPanel(

      tabPanel("Full model results",
        h5("Uploaded data structure:"),
        verbatimTextOutput("data.descr"),
        br(),
        h5("Full model:"),
        verbatimTextOutput("regTab"),
        plotOutput("diagnost.1"),
        plotOutput("diagnost.2"),
        plotOutput("diagnost.3"),
        plotOutput("diagnost.4")
      ),

      tabPanel("Influential effect",
        verbatimTextOutput("influence.choosed"),
        h5("Regression coefficients and 95% confidence intervals:"),
        plotOutput("plot.excl"),
        p("First x-value (zero) = coefficient value from the full model."),
        p("Other cases were sequentially removed from data and the model were refitted without them."),
        br(),
        h5("This observations should be highly influential:"),
        verbatimTextOutput("inflll"),
        p("Note: Check the description of potentially influential observations on 'Description' tab."),
        br(),
        h5("Data table of all excluded observations:"),
        p("Cases (observations) are in the same order as on the exclusion plot (in the decreasing order of influence)."),
        p("Cases ID = row number from the provided data."),
        verbatimTextOutput("out.datt")
      ),

      tabPanel("Description",
        h5("Typical Workflow"),
        p("1. Upload your data: Tab-delimited text file with header (*.txt) or Excel file with one sheet (*.xls, *.xlsx)."),
        p("2. Specify the model you would like to fit (see Formula interface for hints)."),
        p("3. Check other settings (GLM family, maximum number of the most influential observations to exclude, criterion to estimate influence)."),
        p("4. Press 'Build model' button - your model will be fitted to the full data set. You may check the results on the 'Full model results' tab."),
        p("5. Go to the 'Influential effect' tab. This step could be computer-intensive (depending on the data size), so it may freeze for a while."),
        p("6. On the sidebar choose which coefficients you would like to plot."),
        p("7. To rebuild the plot for the other coefficient press 'Build model' again."),
        p("That's it."),
        br(),

        h5("Formula interface"),
        p("Here are several examples you may try (paste formula without quotes):"),
        p("'y ~ A + B'   - Additive model with 2 predictors (no interaction)."),
        p("'y ~ log(A) + sqrt(B)'   - The same but predictors are transformed."),
        p("'y ~ A*B'   - Model with main effects and interaction = 'y ~ A + B + A:B'."),
        p("'y ~ A*B*C'   - The same: main effects and interactions = 'y ~ A+B+C+A:B+A:C+B:C+A:B:C'."),
        p("'y ~(A+B+C)^2' - A, B, and C crossed to level 2: 'y ~ A+B+C+A:B+A:C+B:C'."),
        p("'y ~ A*B*C-A:B:C' - same as above: main effects plus 2-way interactions."),
        p("Models without intercept (e.g. 'y ~ A + B - 1') does not work for the moment - it'll be fixed soon."),
        p("Please use your 'real' varaiable names insted of 'y' or 'A' & 'B' from this examples."),
        p("With binomial data the response (y) can be either a vector or a matrix with two columns."),
        p("If the response is a vector it can be numeric with 0 for failure and 1 for success, or a factor with the first level representing 'failure' and all others representing 'success'."),
        p("Alternatively, the response can be a matrix where the first column is the number of 'successes' and the second column is the number of 'failures'. In this case one should specify model like ' cbind(success, failure) ~ A + B '. 
            Where 'success' & 'failure' are column names in the data."),
        br(),

        h5("Error distribution family - Model link function used"),
          p("Gaussian - Identity"),
          p("Inverse Gaussian - 1/mu^2"),
          p("Gamma - log"),
          p("Binomial - logit"),
          p("Poisson - log"),
          p("Negative Binomial - log"),
          p("Quasibinomial - logit"),
          p("Quasipoisson - log"),
          p("Zero-Inflated Poisson - log"),
          p("Zero-Inflated Negative Binomial - log"),
        br(),

        h5("Potentially influential observations"),
        p("Influential cases are defined as:"),
        p("  - any of its absolute dfbetas values are larger than 1, or"),
        p("  - its absolute dffits value is larger than 3*sqrt(k/(n-k)), or"),
        p("  - abs(1 - covratio) is larger than 3*k/(n-k), or"),
        p("  - its Cook's distance is larger than the 50% percentile of an F-distributio with k and n-k degrees of freedom, or"),
        p("  - its hatvalue is larger than 3*k/n."),
        br(),

        h5("Notes"),
        p("For GLMs (other than the Gaussian family with identity link) regression diagnostic measures are based on one-step approximations which may be inadequate if a case has high influence."),
        br(),

        h5("Future plans"),
        p("Add export of influece measures."),
        p("Fix handling of the models without intercept."),
        p("Add back-transformation of regression coefficients to the original scale (now they're on the scale of the link function)."),
        p("Add opportunity to choose other link functions (for example complementary log-log link function in binomial glm for presence-absence data)."),
        p("Add link-function selection based on lowest residual deviance (-2*logLikelihood)."),
        p("Add some stopping rules in exclusion algorithm (e.g. based on Mallow's Cp or something)."),
        br(),
        br(),
        br(),
        br(),
        br(),
        p("Author - Vladimir Mikryukov, 04.07.2014")
      ),

      tabPanel("Model selection cheat list",
        h5("Which to model to choose?"),

        h6("Continuous distributions"),
        p("Standard normal or linear regression - Gaussian"),
        p("Positive only continuous - Gamma or Inverse Gaussian"),

        h6("Count data"),
        p("Equidispersed count - Poisson"),
        # p("count, with the ancillary parameter a constant - Negative binomial ----????"),
        p("Overdispersed counts - Quasipoisson"),
        p("Overdispersed proportions - Quasibinomial"),
        # p("Positive continuous data with exact zeros - tweedie [package statmod]"),
        br(),
        p("Binary (1/0) response - Logistic"),
        p("Binomial distribution with m=1  - Binary-Bernoulli"),
        p("Proportional (y/m, where y = number of 1's) - Binomial "),
        br(),
        br(),
# http://statprob.com/encyclopedia/GLM.html


    # h6("Binary data:"),
		# p("Logistic regression (Family = Binomial) - When the response data (Y) are binary (taking on only values 0 and 1)."),
		# br(),
    # h6("Count data:"),
		p("Poisson Regression is used to model count variables. The variance in the Poisson model is identical to the mean."),
		p("Negative binomial regression is for modeling count variables, usually for over-dispersed count outcome variables.
			It can be considered as a generalization of Poisson regression since it has the same mean structure as Poisson regression and it has an extra parameter to model the over-dispersion. 
			For negative binomial regression the variance is assumed to be a quadratic function."),
		p("For a quasi-poisson regression the variance is assumed to be a linear function of the mean."),
		br(),
		p("Zero-inflated models attempt to account for excess zeros. In other words, two kinds of zeros are thought to exist in the data, 'true zeros' and 'excess zeros'. 
			Zero-inflated models estimate two equations simultaneously, one for the count model and one for the excess zeros."),
		p("Zero-inflated Poisson model has two parts, a poisson count model and the logit model for predicting excess zeros."),
		p("Zero-inflated Negative Binomial Regression does better with over dispersed data, i.e. variance much larger than the mean."),
    br(),
    p("Zero-truncated models used to model count data for which the value zero cannot occur. 
        	This class of models will surpass Ordinary Poisson & Negative Binomial regression which will predict zero counts even though there are no zero values."),
    p("Zero-truncated Poisson regression - Useful if you have no overdispersion"),
    p("Zero-truncated negative binomial regression  - When overdispersion exists."),
    br()
      )

      # ,
      # tabPanel("Debug",
      #   h5("Debugging info:", style = "color:darkred"),
      #   verbatimTextOutput("debug")         # return input and stored values
      # )

    )     # close tabsetPanel
  )       # close mainPanel
))


