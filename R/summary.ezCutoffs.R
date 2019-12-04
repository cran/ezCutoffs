#' @rawNamespace S3method(summary,ezCutoffs)
summary.ezCutoffs <- function(object, ...) {
  print(object[["simulationParameters"]])
  cat("\n")
  print(object[["summary"]])
}