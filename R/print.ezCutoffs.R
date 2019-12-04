#' @export
print.ezCutoffs <- function(x, ...) {
  filter <- c(grep("Empirical", names(x$summary)), grep("Cutoff", names(x$summary)))
  print(x$summary[, filter])
}
