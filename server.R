library(ggplot2)

#server.R

function(input,output, session){
  output$allocationExplanation <- renderText({
    if(input$budgetAllocation == "equal") {
      "Every person on earth gets an equal share of the remaining budget."
    } else {
      "Countries get a share of the total budget proportional to their current production."
    }
  })
  
  budgetInfo <- reactive({budgetEstimation[which(budgetEstimation$name == input$budgetId), ]})
  
  worldBudget <- reactive({
    req(input$budgetId)
    if(input$budgetId == "Custom") {
      input$budgetGt
    } else if (!is.null(input$budgetId)) {
      b <- budgetInfo()$budgetEstimation - (currentYearDecimal() - budgetInfo()$year)*input$yearlyGt
      b
    }
  })
  
  worldBudgetYears <- reactive({worldBudget()/input$yearlyGt})
  euBudgY <- reactive({
    if(input$budgetAllocation == "inertia"){
      worldBudgetYears()
    } else {
      worldBudgetYears()*(input$capitaCO2 /input$euCapitaCO2)
    }
  })
  
  switchYFromNow <- reactive({
    req(input$switchYear)
    input$switchYear-currentYearDecimal()
    })
  
  switchParams <- reactive({calcSwitchParams(euBudgY(), switchYFromNow())})
  linParams <- reactive({calcLinParams(euBudgY())})
  expParams <- reactive({calcExpParams(euBudgY())})
  
  budgetRow <-reactive({
    budgetEstimation[budgetEstimation$name == input$budgetId,]
  })
 
  output$globalBudget <- renderUI({
    tagList(
      h3("Global CO2 Budget"), 
      " according to ", a(href=budgetRow()$source, input$budgetId), "in order to limit warming to ", budgetRow()$target, "°C",
       h4(sprintf("at the beginning of the year %.0f: %.0f Gt", budgetRow()$year , budgetRow()$budget)),
       h4(sprintf("now: %.0f Gt", worldBudget())),
       h4(sprintf("In years of current emissions: %.2f years", worldBudgetYears())),
       withMathJax(helpText("$$= \\frac{\\text{Global Budget[Gt]}}{\\text{Global Yearly Emissions[Gt/year]}}$$"))
    )
  })
  allocationHelpText<-reactive({
    if(input$budgetAllocation == "equal"){
      withMathJax(helpText(
        "Equal Allocation: 
        $$\\begin{align}
          &\\text{Carbon Budget per Person [t/person]} \\\\ 
          &= \\text{Global Budget in Years [years]}\\times \\text{World Average Emissions[t/(year $\\times$ person)]}
        \\end{align} $$ 
        thus
        $$\\begin{align}
          &\\text{EU Budget in Years [years]} \\\\
          &= \\frac{\\text{Carbon Budget per Person [t/person]}}{\\text{EU Average Emissions[t/(year $\\times$ person)]}} \\\\
          &= \\text{Global Budget in Years [years]} 
          \\frac{\\text{World Average Emissions[t/(year $\\times$ person)]}}{\\text{EU Average Emissions[t/(year $\\times$ person)]}}
        \\end{align}$$"
      ))
    } else if(input$budgetAllocation == "inertia"){
      withMathJax(helpText(
        "Inertia Allocation: 
         
        $$\\begin{align}
          &\\text{EU Budget [Gt]} \\\\
          &= \\text{Global Budget [Gt]} \\frac{\\text{EU Average Emissions[t/(year $\\times$ person)]}}{\\text{World Average Emissions[t/(year $\\times$ person)]}} \\\\
        \\end{align}$$
        thus
        $$\\begin{align}
          &\\text{EU Budget in Years [years]} \\\\
          &= \\frac{\\text{EU Budget [Gt]}}{\\text{EU Average Emissions[t/(year $\\times$ person)]}} \\\\
          &= \\frac{\\text{Global Budget [Gt]}}{\\text{World Average Emissions[t/(year $\\times$ person)]}}\\\\
          &= \\text{Global Budget in Years [years]}
        \\end{align}$$"
      ))
    }
  })
  output$euBudget <- renderUI({
    tagList(
      h3("EU CO2 Budget"),
      h4(sprintf("In years of current EU emission: %.2f years", euBudgY())),
      allocationHelpText()
    )
  })
  
  
  output$plot<-renderPlot({
    
    t<-seq(0,40,length=500)
    linear <- linearFunc(t, linParams())
    switch <- switchFunc(t, switchYFromNow(), switchParams())
    expon <- expFunc(t, expParams())
    
    df<-data.frame(year=t+currentYearDecimal(), linear=linear*100, switch = switch*100, expon = expon*100)
    ggplot(df)+
      geom_line(aes(x=year, y=linear, colour = "linear"))+
      geom_line(aes(x=year, y=switch, colour = "switch"))+
      geom_line(aes(x=year, y=expon, color="exponential"))+
      ylim(0,100)+
      labs(x = "year", y = "emissions as percentage of current yearly emissions", colour = "type")
  })
  output$reductionFactors<-renderText({
    sprintf(
    "%.2f%% (percentage points) reduction in the first %.2f years until %.0f. 
    Then exponential decrease by %.2f%% compared to the previous year, every year.", 
    -switchParams()$linFactor*100,
    switchYFromNow(), 
    switchYFromNow()+currentYearDecimal(), 
    (1-exp(switchParams()$logBase))*100
    )
  })
  
  output$linearReductionFactors <- renderText({
    sprintf(
      "%.2f%% (percentage points) reduction until the Year %.0f",
      -linParams()$linFactor*100,
      linParams()$zeroPoint+currentYearDecimal()
    )
  })
  
  output$exponReductionFactors <- renderText({
    sprintf(
      "%.2f%% reduction compared to the previous year, every year",
      -expParams()$expFactor*100
    )
  })
  
  output$budgetID <- renderUI({
    selectInput(
      inputId = "budgetId", 
      label = "CO2 Budget Estimation for that Target", 
      choices =  c(budgetEstimation[sprintf("%.1f°C", budgetEstimation$target) == input$target,]$name, "Custom"), 
      selected = budgetEstimation$name[0]
    )
  })
  
  output$switchLinExp <- renderUI({
    sliderInput(
      inputId = "switchYear", 
      label = "Switch from linear to exponential", 
      min = ceiling(currentYearDecimal()), 
      max = floor(currentYearDecimal() + 2*euBudgY()), 
      value = 2030, 
      step = 0.5, 
      sep="")
  })
  
}