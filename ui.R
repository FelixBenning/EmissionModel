# ui.R

fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "budgetAllocation", label = "Budget Allocation", c("equal", "inertia")),
      uiOutput("switchLinExp"),
      selectInput(
        inputId = "institute", 
        label = "Budget Estimation", 
        choices =  c(budgetEstimation$institute, "Custom"), 
        selected = budgetEstimation$institute[0]
      ),
      conditionalPanel(
        "input.institute == 'Custom'",
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
      textOutput("euBudgY"),
      plotOutput("plot"),
      textOutput("reductionFactors")
    )
  )
)