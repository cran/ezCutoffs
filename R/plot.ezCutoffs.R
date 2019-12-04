#' @export
plot.ezCutoffs <- function(x, fit_indices = NULL, type = 'histogram', ...) {

  summ <- x[['summary']]
  if (is.null(fit_indices)) fit_indices <- rownames(summ)
  
  summ$fit_index <- rownames(summ)
  names(summ)[grepl('Cutoff', names(summ))] <- 'Cutoff'
  summ <- subset(summ, subset = summ$fit_index %in% fit_indices,
    select = c('Empirical fit', 'Cutoff', 'fit_index'))
  summ <- stats::reshape(summ, timevar = 'Reference', 
    varying = list(c('Empirical fit', 'Cutoff')), v.names = 'value',
    times = c('Empirical', 'Cutoff'), idvar = 'fit_index',
    direction = 'long')
  
  disti <- x[['fitDistributions']]
  disti <- subset(disti, select = fit_indices)
  disti <- stats::reshape(disti, timevar = 'fit_index', 
    varying = list(names(disti)), v.names = 'Fit',
    direction = 'long',
    times = names(disti))
  
  # escaping global assignment hell (tidyverse problems)
  Reference <- summ$Reference
  value <- summ$value
  fit_index <- disti$fit_index
  Fit <- disti$Fit
  
  plottable <- ggplot2::ggplot(disti, ggplot2::aes(x = Fit, group = fit_index)) +
    ggplot2::labs(title = 'Simulated Fit Distribution') +
    ggplot2::facet_wrap(~ fit_index, scales = 'free')

  if (type == 'density') {
    plottable <- plottable + ggplot2::geom_density()
  } else {
    plottable <- plottable + ggplot2::geom_histogram()
  }
  
  plottable <- plottable + 
    ggplot2::geom_vline(data = summ, ggplot2::aes(xintercept = value, lty = Reference))
  
  return(plottable)
}