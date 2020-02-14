library(ggplot2)

#server.R

function(input,output, session){

  ## Sidebar ##
  output$budgetID <- renderUI({
    selectInput(
      inputId = "budgetId", 
      label = "CO2 Budget Estimation for that Target", 
      choices =  c(budgetEstimation[sprintf("%.1f°C", budgetEstimation$target) == input$target,]$name, "Custom"), 
      selected = budgetEstimation$name[0]
    )
  })
  

  output$allocationExplanation <- renderText({
    if(input$budgetAllocation == "equal") {
      "Every person on earth gets an equal share of the remaining budget. This does not account for historical emissions or population growth."
    } else {
      "Countries get a share of the total budget proportional to their current emissions. E.g. if a country causes 20% of current emissions it gets 20% of the remaining budget"
    }
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

  ## reactive Variables ##
  budgetEntry <- reactive({
    budgetEstimation[budgetEstimation$name == input$budgetId, ]
  })
  
  worldBudget <- reactive({
    req(input$budgetId)
    if (input$budgetId != "Custom") {
      b <- budgetEntry()$budgetEstimation - (currentYearDecimal() - budgetEntry()$year)*input$yearlyGt
    } else {
      input$budgetGt
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
 
  ## Plot Tab ##
  timeSeries<-reactive({
    getTimeSeries()
  })
  
  dataFrame<-reactive({
    t<-timeSeries()$t
    linear <- linearFunc(t, linParams())
    switch <- switchFunc(t, switchYFromNow(), switchParams())
    expon <- expFunc(t, expParams())
    df<-data.frame(
      yeardecimal=timeSeries()$yeardecimal,
      year=timeSeries()$year,
      month=timeSeries()$month,
      linear=linear*100, 
      switch = switch*100, 
      expon = expon*100)
  })
  
  output$plot<-renderPlot({
    ggplot(dataFrame())+
      geom_line(aes(x=yeardecimal, y=linear, colour = "linear"))+
      geom_line(aes(x=yeardecimal, y=switch, colour = "switch"))+
      geom_line(aes(x=yeardecimal, y=expon, color="exponential"))+
      ylim(0,100)+
      labs(x = "year", y = "emissions as percentage of current yearly emissions", colour = "type")
  })
  output$switchReductionFactors<-renderText({
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
  
  ## Details Tab ##
  output$globalBudgetText <- renderUI({
    tagList(
      h3("Global CO2 Budget"), 
      " according to ", a(href=budgetEntry()$source, input$budgetId), "in order to limit warming to ", budgetEntry()$target, "°C",
       h4(sprintf("at the beginning of the year %.0f: %.0f Gt", budgetEntry()$year , budgetEntry()$budget)),
       h4(sprintf("now: %.0f Gt", worldBudget())),
       h4(sprintf("In years of current emissions: %.2f years", worldBudgetYears())),
      yearlyBudgetEstimationText
    )
  })
  allocationHelpText<-reactive({
    if(input$budgetAllocation == "equal"){
      budgetAllocationEqualText
    } else if(input$budgetAllocation == "inertia"){
      budgetAllocationInertiaText
    }
  })
  output$euBudget <- renderUI({
    tagList(
      h3("EU CO2 Budget"),
      h4(sprintf("In years of current EU emission: %.2f years", euBudgY())),
      allocationHelpText()
    )
  })
  
  ##Export Tab##
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("co2BudgetDistribution", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataFrame(), file, row.names = FALSE)
    }
  )
  
  output$table<-renderTable({
    dataFrame()
  })
}