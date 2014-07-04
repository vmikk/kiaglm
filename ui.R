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
            "binomial" = "binomial",
            "gaussian" = "gaussian",
            "Gamma" = "Gamma",
            "inverse.gaussian" = "inverse.gaussian",
            "quasibinomial" = "quasibinomial",
            "quasipoisson" = "quasipoisson"),
          selected = "gaussian"),
    br(),
    uiOutput("excl.n"),
    br(),
    radioButtons("infl.measure", "Regression diagnostics to use:",
              list("Covariance ratio" = "cov.r",
                   "Cook's distance" = "cook.d",
                   "Diagonal elements of the hat matrix" = "hat",
                   "Absolute residuals" = "resid",
                   "Standardized residuals" = "rstandard",
                   "Studentized residuals" = "rstudent"
              ), selected = "cook.d"),
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
        # verbatimTextOutput("eee"),
        plotOutput("plot.excl")
      ),

      tabPanel("Description",
        h5("Typical Workflow"),
        p("1. Upload your data: Tab-delimited text file with header (*.txt) or Excel file with one sheet (*.xls, *.xlsx)."),
        p("2. Specify the model you would like to fit (see Formula interface for hints)."),
        p("3. Check other settings (GLM family, maximum number of the most influential observations to exclude, criterion to estimate influence)."),
        p("4. Press 'Build model' button - your model will be fitted to the full data set. You may check the results on the 'Full model results' tab."),
        p("5. Go to the 'Influential effect' tab."),
        p("6. On the sidebar choose which coefficients you would like to plot."),
        p("7. Press 'Build model' again.  This step could be computer-intensive, so it may freeze for a while."),
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
        br(),
        h5("Error distribution family - Model link function"),
          p("binomial - logit"),
          p("gaussian - identity"),
          p("Gamma - inverse"),
          p("inverse.gaussian - 1/mu^2"),
          p("poisson - log"),
          p("quasibinomial - logit"),
          p("quasipoisson - log"),
        br(),
        h5("Notes"),
        p("For GLMs (other than the Gaussian family with identity link) regression diagnostic measures are based on one-step approximations which may be inadequate if a case has high influence."),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        p("Author - Vladimir Mikryukov, 04.07.2014")
      ),

      tabPanel("Debug",
        h5("Debugging info:", style = "color:darkred"),
        verbatimTextOutput("debug")         # return input and stored values
      )

    )     # close tabsetPanel
  )       # close mainPanel
))
