# ui.R

fluidPage(
  h1("Distribution of the CO2 Budget Over Time"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "target",
        label = "Temperature Target",
        choices = c(sprintf("%.1f°C", budgetEstimation$target), "Custom"),
        selected = budgetEstimation$target[0]
      ),
      uiOutput("budgetID"),
      conditionalPanel(
        "input.budgetId == 'Custom'",
        numericInput(
          inputId = "budgetGt",
          label = "Global budget in Gt CO2",
          value = 350
        )
      ),
      selectInput(inputId = "budgetAllocation", label = "Budget Allocation", c("equal", "inertia")),
      textOutput("allocationExplanation"),
      br(),
      numericInput(
        inputId = "yearlyGt",
        label = "Global Yearly Emissions in Gt",
        value = 42
      ),
      conditionalPanel(
        "input.budgetAllocation == 'equal'",
        numericInput(
          inputId = "capitaCO2",
          label = "World Average CO2 per Capita",
          value = 5
        ),
        numericInput(
          inputId = "euCapitaCO2",
          label = "EU Average CO2 per Capita",
          value = 6.4
        )
      ),
      uiOutput("switchLinExp")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Plot",
          plotOutput("plot"),
          h4("Linear"),
          textOutput("linearReductionFactors"),
          h4("Switch"),
          textOutput("switchReductionFactors"),
          h4("Exponential"),
          textOutput("exponReductionFactors")
        ),
        tabPanel(
          "Details",
          uiOutput("globalBudgetText"),
          uiOutput("euBudget"),
          h3("Distribution Over Time"),
          p("Since the total budget is now set, we are left with distributing the budget over time. 
            Now the total amount of emissions equals the sum of all emissions over time. I.e. the are under the curve of yearly emissions.
            Therefore we are looking for functions enclosing an area equal to our total budget."),
          p("We also want our function to stat with the current emissions. (I.e. 100%=1 of current Emissions per year). 
            So any valid distribution should have the following properties: "),
          budgetDistributionFunctionProperties,
          p("For a linear function of the form"),
          linearFunctionAnsatz,
          p("This has a unique result, as b is set by the first requirement and a by the second. Similarly the number of possible exponential functions is one.
            In order to offer a compromise, the 'switch' function, starts as a linear function and switches to an exponential function at the set year."),
          a(href="https://github.com/FelixBenning/EmissionModel/tree/master/SwitchDistributionMaths/Explanation.pdf", target="_blank", "(Details: I ensured, that the swap was continuous and differentiable)")
        ),
        tabPanel(
          "Export",
          downloadButton("downloadData", "Download"),
          tableOutput("table")
        )
      )
    )
  )
)