simulationStats <- function(fit_distributions, n_rep, fit_s_list, alpha_level, dots, group_labels) {
  
  n_conv <- sum(!is.na(fit_distributions[, 1]))
  s_est <- lavaan::lavInspect(fit_s_list[[1]], what = "call")$estimator
  if (length(s_est) == 0) {
    s_est <- lavaan::lavInspect(fit_s_list[[1]], what = "options")$estimator
  }
  n_sim <- 0                    
  i <- 1
  while (!(n_sim[1]>0)) {
    n_sim <- lavaan::lavInspect(fit_s_list[[i]], what = "nobs")
    i <- i+1
  }
  simulation_stats <- data.frame(matrix(c(n_rep, n_conv, s_est, alpha_level, n_sim), 1, (4+length(n_sim))))
  names(simulation_stats) <- c("#Runs", "#Converged", "Estimator", "Alpha", "TotalObservations")
  rownames(simulation_stats) <- ""
  if (length(n_sim) > 1) {
    names(simulation_stats)[5:(4+length(n_sim))] <- c(paste0('n_', group_labels))
  }
  
  simulation_stats[1, (ncol(simulation_stats)+1)] <- "listwise"
  names(simulation_stats)[(ncol(simulation_stats))] <- "Missing"
  
  if (is.null(dots$missing)==F) {
    simulation_stats$Missing <- dots$missing
  }
  return(simulation_stats)
}
