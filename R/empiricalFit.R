empiricalFit <- function(model, data, fit_indices, ...) {
  fit <- lavaan::sem(model = model, data = data, ...)
  empirical_fit <- lavaan::fitMeasures(fit, fit.measures = fit_indices)

  # parameters for simulation
  pop_model <- lavaan::parTable(fit)
  group_sizes <- lavaan::lavInspect(fit, what = "nobs")
  n <- sum(group_sizes)
  groups_var <- lavaan::lavInspect(fit, what = "group")
  n_groups <- lavaan::lavInspect(fit, what = "ngroups")
  group_labels <- lavaan::lavInspect(fit, what = "group.label")
  dots <- list(...)
  
  # stop if grouping variable wiht only one level has been selected
  if ((length(groups_var) > 0) & n_groups < 2) {
    stop("Less than 2 levels in the defined grouping variable.")
  }
  
  out <- mget(c('fit', 'empirical_fit', 'pop_model', 'group_sizes', 'n', 'groups_var', 'n_groups', 'group_labels', 'dots'))
  
  return(out)
}
