library(jsonlite)


budgetEstimation <- read_json("budgetEstimations.json", simplifyVector = TRUE)

calcSwitchParams <-function(budgetYears, switchTime) {
  logBase <- (budgetYears-switchTime-sqrt((budgetYears-switchTime)^2+4*(switchTime*budgetYears-switchTime^2/2)))/(2*switchTime*budgetYears-switchTime^2)
  linFactor <- logBase/(1-logBase*switchTime)
  expFactor <- exp(-logBase*switchTime)/(1-logBase*switchTime)
  
  params <- data.frame(linFactor = linFactor, logBase = logBase, expFactor = expFactor)
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
  y <- (x<switchTime)*(params$linFactor*x+1) + (x>=switchTime)*params$expFactor*exp(params$logBase*x)
}

linearFunc <- function(x, params) {
  return((params$linFactor*x+1)*(x<params$zeroPoint))
}

expFunc <- function(x, params) {
  return(exp(params$expFactor*x))
}

currentYearDecimal <- function() {
  return(as.double(format(Sys.Date(), "%Y")) + as.double(format(Sys.Date(), "%m"))/12 + as.double(format(Sys.Date(), "%d"))/365)
}

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