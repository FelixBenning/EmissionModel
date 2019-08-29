library(shiny)
library(ggplot2)

ui<-fluidPage(
  sidebarLayout(
    sidebarPanel(
        selectInput(inputId = "budgetAllocation", NULL, c("equal", "inertia")),
        uiOutput("switchLinExp"),
        numericInput(
          inputId = "budgetGt",
          label = "Global budget in Gt CO2",
          value = 350
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
  
server<-function(input,output, session){
  worldBudgetYears <- reactive({input$budgetGt/input$yearlyGt})
  euBudgY <- reactive({
    if(input$budgetAllocation == "inertia"){
      worldBudgetYears()
    } else {
      worldBudgetYears()*(input$capitaCO2 /input$euCapitaCO2)
    }
  })
  
  ydelta <- reactive({input$switchYear-2019})
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
    
    df<-data.frame(year=c(t,t)+2019,emissions=c(y,linear)*100)
    ggplot(df, aes(x=year, y=emissions))+
      geom_line(linetype = "dashed")+
      geom_point()+
      ylim(0,100)
  })
  output$reductionFactors<-renderText({
    sprintf("%.2f %% (percentage points) reduction in the first %s years until %s. 
            Then exponential decrease by %.2f%%", 
            -linFactor()*100,
            ydelta(), 
            ydelta()+2019, 
            (1-exp(logBase()))*100
            )
  })
  output$switchLinExp <- renderUI({
    sliderInput("switchYear", "Switch from linear to exponential", 2020, floor(2019 + 2*euBudgY()), 2030)
  })
  
}

shinyApp(ui,server)