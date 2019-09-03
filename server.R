library(ggplot2)

#server.R

function(input,output, session){
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
  
  ydelta <- reactive({
    if(is.null(input$switchYear)) {
      2030 -currentYearDecimal()
    } else {
      input$switchYear-currentYearDecimal()
    }
    
    })
  
  switchParams <- reactive({calcSwitchParams(euBudgY(), ydelta())})
  linParams <- reactive({calcLinParams(euBudgY())})
  expParams <- reactive({calcExpParams(euBudgY())})
  
  
  output$euBudgY <-renderText({
    sprintf("Budget of EU: %.1f years of current EU CO2 production", euBudgY())
  })
  
  output$plot<-renderPlot({
    
    t<-seq(0,40,length=500)
    linear <- linearFunc(t, linParams())
    switch <- switchFunc(t, ydelta(), switchParams())
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
    sprintf("%.2f %% (percentage points) reduction in the first %.2f years until %.0f. 
            Then exponential decrease by %.2f%%", 
            -switchParams()$linFactor*100,
            ydelta(), 
            ydelta()+currentYearDecimal(), 
            (1-exp(switchParams()$logBase))*100
    )
  })
  output$switchLinExp <- renderUI({
    sliderInput("switchYear", "Switch from linear to exponential", ceiling(currentYearDecimal()), floor(currentYearDecimal() + 2*euBudgY()), 2030, step = 0.5, sep="")
  })
  
}