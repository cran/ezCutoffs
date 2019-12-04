#' @rawNamespace S3method(summary,wilc_result)
summary.wilc_result <- function(object, ...) {
  output <- data.frame(matrix(NA, length(object), 2))
  names(output) <- c("WilcoxonW", "p-value")
  for (i in 1:length(object)) {
    output[i, 1] <- object[[i]][["statistic"]]
    output[i, 2] <- object[[i]][["p.value"]]
    rownames(output)[i] <- object[[i]][[8]]
  }
  print(output, digits = 4)
}
