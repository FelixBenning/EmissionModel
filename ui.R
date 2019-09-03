# ui.R

fluidPage(
  sidebarLayout(
    sidebarPanel(
      uiOutput("switchLinExp"),
      selectInput(inputId = "budgetAllocation", label = "Budget Allocation", c("equal", "inertia")),
      textOutput("allocationExplanation"),
      br(),
      selectInput(
        inputId = "budgetName", 
        label = "Budget Estimation", 
        choices =  c(budgetEstimation$name, "Custom"), 
        selected = budgetEstimation$name[0]
      ),
      conditionalPanel(
        "input.budgetName == 'Custom'",
        numericInput(
          inputId = "budgetGt",
          label = "Global budget in Gt CO2",
          value = 350
        )
      ),
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
      )
    ),
    mainPanel(
      h1("Distribution of the Budget Over Time"),
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