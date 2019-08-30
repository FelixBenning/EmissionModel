library(jsonlite)


budgetEstimation <- read_json("budgetEstimations.json", simplifyVector = TRUE)


currentYearDecimal <- function() {
  return(as.double(format(Sys.Date(), "%Y")) + as.double(format(Sys.Date(), "%m"))/12 + as.double(format(Sys.Date(), "%d"))/365)
}