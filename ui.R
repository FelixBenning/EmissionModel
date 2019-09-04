# ui.R

fluidPage(
  h1("Distribution of the CO2 Budget Over Time"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "target",
        label = "Temperature Target",
        choices = sprintf("%.1f°C", budgetEstimation$target),
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
        label = "Global yearly production in Gt",
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
          textOutput("reductionFactors"),
          h4("Exponential"),
          textOutput("exponReductionFactors")
        ),
        tabPanel(
          "Details",
          textOutput("euBudgY")
        )
      )
    )
  )
)