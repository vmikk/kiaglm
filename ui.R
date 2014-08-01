## ui.r
## The user interface controls the layout and appearance of the app.


# Define UI for application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel(h4("GLM (Generalized Linear Models)")),


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
            "Zero-Inflated Poisson" = "zip",               # [pscl]
            "Zero-Inflated Negative Binomial" = "zinb",    # [pscl]
            "Poisson hurdle" = "phurd",		 			           # [pscl]
            "Negative binomial hurdle" = "nbhurd"		       # [pscl]
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
    helpText("Note: Only residuals for Zero-Inflated & Hurdle models is available."),
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
        textOutput("vuong"),
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
        p("The formula may include an offset term, e.g. 'y ~ A + offset(B)' where B will have the coefficient 1 rather than an estimated coefficient."),
        p("One can used arithmetical operators to create a latent variables, e.g. 'y ~ A + I(B+C)', the term B+C will be interpreted as the sum of B and C."),
        br(),
        br(),
        p("Zero-inflated and Hurdle models: If a formula of type 'y ~ x1 + x2' is supplied, it not only describes the count regression relationship of y and x1 & x2 but also implies 
        	that the same set of regressors is used for the zero component (zero-inflation or hurdle). 
        	This is could be made more explicit by equivalently writing the formula as 'y ~ x1 + x2 | x1 + x2'. 
        	Of course, a different set of regressors could be specified for the zero component, e.g., 'y ~ x1 + x2 | z1 + z2 + z3', 
        	giving the count data model y ~ x1 + x2 conditional on (|) the zero-inflation or hurdle model y ~ z1 + z2 + z3."),
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
          p("Zero-Inflated Poisson - log; binomial model with logit link for zero-inflation model"),
          p("Zero-Inflated Negative Binomial - log; binomial model with logit link for zero-inflation model"),
          p("Hurdle - log; distribution for the zero hurdle model - binomial model with logit link"),
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
        p("Vuong Non-Nested Hypothesis Test: null hypothesis that the models are indistinguishable. A large, positive test statistic provides evidence of the superiority of model 1 over model 2, while a large, negative test statistic is evidence of the superiority of model 2 over model 1."),
        br(),

        h5("Future plans"),
        p("Add export of influece measures."),
        p("Add batch comparison of likelihood from different models."),
        p("Fix handling of the models without intercept."),
        p("Add back-transformation of regression coefficients to the original scale (now they're on the scale of the link function)."),
        p("Add opportunity to choose other link functions (for example complementary log-log link function in binomial glm for presence-absence data)."),
        p("Add link-function selection based on lowest residual deviance (-2*logLikelihood)."),
        p("Add some stopping rules in exclusion algorithm (e.g. based on Mallow's Cp or something)."),
        p("Add computation of the Wald tests using sandwich standard errors for ordinary Poisson model (because Wald test results might be too optimistic due to a misspecification of the likelihoodin the case of over-dispersion."),
        br(),
        h5("Computational details"),
        p("Used R-packages: MASS, pscl, car, XLConnect."),
        br(),
        br(),
        br(),
        br(),
        p("Author - Vladimir Mikryukov (vmikryukov at gmail.com), 04.07.2014")
      ),

      tabPanel("Model selection cheat list",
        h5("Which model to choose?"),

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
    br(),
    p("Hurdle (zero-augmented or zero-altered) models: In addition to over-dispersion, many empirical count data sets exhibit more zero observations than would be allowed for by the Poisson model. 
    	One model class capable of capturing both properties is the hurdle model. 
    	They are two-component models: A truncated count component, such as Poisson, geometric or negative binomial, is employed for positive counts, 
    	and a hurdle component models zero vs. larger counts. 
    	For the latter, either a binomial model or a censored count distribution can be employed."),
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


