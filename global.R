library(jsonlite)


budgetEstimation <- read_json("budgetEstimations.json", simplifyVector = TRUE)

calcSwitchParams <-function(budgetYears, switchTime) {
  rootTempVar <- sqrt((budgetYears-switchTime)^2+4*switchTime*(budgetYears-switchTime/2))
  denominator<- switchTime*(budgetYears + rootTempVar)
  
  linFactor <- budgetYears/denominator - 1/(budgetYears + rootTempVar) - rootTempVar/denominator
  switchValue<- linFactor*switchTime + 1
  logBase <- linFactor/switchValue
  
  params <- data.frame(linFactor = linFactor, logBase = logBase, switchValue = switchValue)
  return(params)
}

calcLinParams <- function(budgetYears) {
  factor<- -1/(2*budgetYears)
  zeroPoint <- 2*budgetYears
  params <- data.frame(linFactor = factor, zeroPoint = zeroPoint)
  return(params)
}

calcExpParams <- function(budgetYears) {
  params <- data.frame(expFactor = (-1/budgetYears))
  return(params)
}

switchFunc <- function(x, switchTime, params) {
  y <- (x<switchTime)*(params$linFactor*x+1) + (x>=switchTime)*params$switchValue*exp(params$logBase*(x-switchTime))
}

linearFunc <- function(x, params) {
  return((params$linFactor*x+1)*(x<params$zeroPoint))
}

expFunc <- function(x, params) {
  return(exp(params$expFactor*x))
}

currentYearDecimal <- function() {
  return(as.double(format(Sys.Date(), "%Y")) + (as.double(format(Sys.Date(), "%m"))-1)/12 + (as.double(format(Sys.Date(), "%d"))-1)/365)
}

getTimeSeries<- function() {
  roundedYear<-ceiling(currentYearDecimal())
  beforeRoundedYear<-rev(seq(from=roundedYear, to=currentYearDecimal(), by=-1/12))
  afterRoundedYear<-seq(from=roundedYear+1/12, to=currentYearDecimal()+40, by=1/12)
  yeardecimal<-c(beforeRoundedYear, afterRoundedYear)
  t<-yeardecimal-currentYearDecimal()
  df<-data.frame(yeardecimal=yeardecimal, t=t)
  df$year=as.integer(floor(df$yeardecimal))
  df$month=as.integer(round((df$yeardecimal-df$year)*12+1))
  return(df)
}

yearlyBudgetEstimationText <- withMathJax(helpText("$$= \\frac{\\text{Global Budget[Gt]}}{\\text{Global Yearly Emissions[Gt/year]}}$$"))

budgetAllocationEqualText <- withMathJax(helpText(
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

budgetAllocationInertiaText <- withMathJax(helpText(
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

budgetDistributionFunctionProperties <- withMathJax(
  "$$\\begin{align}
    (1) & \\qquad f(\\text{currentYear}) = 1 \\\\
    (2) & \\qquad \\int_{\\text{currentYear}}^\\infty f(x)dx = \\text{total budget}
    \\end{align}
  $$"
)

linearFunctionAnsatz <- withMathJax("$$f(\\text{currentYear} + x) = ax+b.$$")