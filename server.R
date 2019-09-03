library(ggplot2)

#server.R

function(input,output, session){
  output$allocationExplanation <- renderText({
    if(input$budgetAllocation == "equal") {
      "Every Person on earth gets an equal share of the remaining budget."
    } else {
      "Countries get a share of the total budget proportional to their current production."
    }
  })
  
  budgetInfo <- reactive({budgetEstimation[which(budgetEstimation$name == input$budgetName), ]})
  
  worldBudget <- reactive({
    if(input$budgetName == "Custom") {
      input$budgetGt
    } else {
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
    if(is.null(input$switchYear)) {
      2030 -currentYearDecimal()
    } else {
      input$switchYear-currentYearDecimal()
    }
    
    })
  
  switchParams <- reactive({calcSwitchParams(euBudgY(), switchYFromNow())})
  linParams <- reactive({calcLinParams(euBudgY())})
  expParams <- reactive({calcExpParams(euBudgY())})
  
  
  output$euBudgY <-renderText({
    sprintf("Budget of EU: %.1f years of current EU CO2 production", euBudgY())
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
  
  output$switchLinExp <- renderUI({
    sliderInput("switchYear", "Switch from linear to exponential", ceiling(currentYearDecimal()), floor(currentYearDecimal() + 2*euBudgY()), 2030, step = 0.5, sep="")
  })
  
}