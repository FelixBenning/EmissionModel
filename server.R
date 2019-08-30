library(shiny)
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
  
  ydelta <- reactive({input$switchYear-currentYearDecimal()})
  logBase <- reactive({
    (euBudgY()-ydelta()-sqrt((euBudgY()-ydelta())^2+4*(ydelta()*euBudgY()-ydelta()^2/2)))/(2*ydelta()*euBudgY()-ydelta()^2)
  })
  linFactor <- reactive({logBase()/(1-logBase()*ydelta())})
  expFactor <- reactive({exp(-logBase()*ydelta())/(1-logBase()*ydelta())})
  
  output$euBudgY <-renderText({
    sprintf("Budget of EU: %.1f years of current EU CO2 production", euBudgY())
  })
  
  output$plot<-renderPlot({
    t<-seq(0,50,length=100)
    y<-(t<ydelta())*(linFactor()*t+1) + (t>=ydelta())*expFactor()*exp(logBase()*t)
    factor<--1/(2*euBudgY())
    linear<-factor*t+1
    
    df<-data.frame(year=c(t,t)+currentYearDecimal(),emissions=c(y,linear)*100)
    ggplot(df, aes(x=year, y=emissions))+
      geom_line(linetype = "dashed")+
      geom_point()+
      ylim(0,100)
  })
  output$reductionFactors<-renderText({
    sprintf("%.2f %% (percentage points) reduction in the first %.2f years until %.0f. 
            Then exponential decrease by %.2f%%", 
            -linFactor()*100,
            ydelta(), 
            ydelta()+currentYearDecimal(), 
            (1-exp(logBase()))*100
    )
  })
  output$switchLinExp <- renderUI({
    sliderInput("switchYear", "Switch from linear to exponential", ceiling(currentYearDecimal()), floor(currentYearDecimal() + 2*euBudgY()), 2030)
  })
  
}