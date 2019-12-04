dataGeneration <- function(fit, data, normality, n_groups, group_labels, groups_var, group_sizes, n_rep, pop_model) {
  message("Data Generation\n")
  
  if (normality == "empirical") { # With Skew/Kurt correction
    var_table <- lavaan::varTable(fit)
    skew <- moments::skewness(data[var_table$name], na.rm = TRUE)
    kurt <- moments::kurtosis(data[var_table$name], na.rm = TRUE)
  } else { # assume normality
    skew <- NULL
    kurt <- NULL
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
  
  # single replication
  replic <- function() {
    progress()
    lavaan::simulateData(model = pop_model, sample.nobs = group_sizes, group.label = group_labels, skewness = skew, kurtosis = kurt)
  }
  
  data_s_list <- vector("list", n_rep)
  data_s_list <- replicate(n_rep, replic(), simplify = F)
  
  #Rename variable if multigroup
  if (n_groups > 1) { # Multigroup
    for (i in 1:n_rep) {
      old_names <- names(data_s_list[[i]])
      new_names <- gsub("group", get("groups_var"), old_names)
      names(data_s_list[[i]]) <- new_names
    }
  }  
  return(data_s_list)
}
