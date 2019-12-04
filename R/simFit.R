simFit <- function(model, data_s_list, n_rep, n_cores, fit_indices, ...) {
  message("\nModel Fitting\n")
  
  fit_s_list <- vector("list", length = n_rep)
  estimation <- function(i, ...) {
    lavaan::sem(model = model, data = data_s_list[[i]], ...)
  }
  
  # progress bar
  pb <- progress::progress_bar$new(
    format = "  |:bar| :percent elapsed = :elapsed  ~ :eta",
    total = n_rep, complete = "=", incomplete = " ", current = " ",
    width = 80, clear = F, show_after = 0
  )
  progress <- function(n) {
    pb$tick(tokens = list(trial = (1:n_rep)[n])) # token reported in progress bar
  }
  
  if (n_cores > 1) {
    cl <- parallel::makeCluster(n_cores)
    doSNOW::registerDoSNOW(cl)
    # foreach loop ------------------------------------------------------------
    fit_s_list <- foreach::foreach(i = 1:n_rep, .options.snow = list(progress = progress)) %dopar% {
      estimation(i, ...)
    }
    parallel::stopCluster(cl)
  } else {
    for (i in 1:n_rep) {
      fit_s <- estimation(i, ...)
      fit_s_list[[i]] <- fit_s
      progress(i)
    }
  }
  
  # extract fit_measures-----------------------------------------------------------
  fit_measures_s_list <- list()
  for (i in 1:n_rep) {
    fit_measures_s <- try(lavaan::fitmeasures(fit_s_list[[i]], fit.measures = fit_indices), silent = T)
    if (!inherits(fit_measures_s, "try-error")) { # if does not have issues
      fit_measures_s_list[[i]] <- fit_measures_s # save fit indices,
    } else { # put NA otherwise
      fit_measures_s_list[[i]] <- rep(NA, length(fit_indices))
    }
  }
  
  # get fit distributions------------------------------------------------------------
  ncols <- length(fit_indices)
  m_fit_measures <- matrix(NA, n_rep, ncols)
  
  for (i in 1:n_rep) {
    m_fit_measures[i, ] <- matrix(unlist(fit_measures_s_list[[i]]), ncol = ncols)
  }
  
  fit_distributions <- as.data.frame(m_fit_measures)
  names(fit_distributions)[1:ncols] <- fit_indices
  
  out <- mget(c('fit_s_list', 'fit_distributions'))
  return(out)
}
