get_key <- function(df, 
                    key_cols = names(df)) {
  # add check to see if all key_cols available
  unique_df <- df[, key_cols, drop=F] %>%
    data.table::as.data.table() %>%
    unique(by=key_cols)
  return(dplyr::tbl_df(unique_df))
}

stratify_df <- function(df, 
                        strat_cols,
                        n,
                        replace = TRUE
) {
  frac <- n/nrow(df)
  sample <- df %>% dplyr::group_by_(.dots=strat_cols) %>% 
    dplyr::sample_frac(frac, replace=replace)
  nsample <- nrow(sample) 
  nleft <- n- nsample
  if(nleft != 0) {
    ifelse(nleft > 0, {
      extras <- dplyr::sample_n(df, size = nleft)
      sample <- dplyr::bind_rows(sample, extras)
    }, {
      removals <- sample.int(nsample, abs(nleft))
      sample <- sample[-removals,]
    }
    )
    
  }
  return(sample)
}
resample_df <- function(df, 
                        key_cols,
                        strat_cols = NULL, 
                        n = NULL,
                        key_col_name = "KEY",
                        replace = TRUE) {
  # checks
  if (is.numeric(strat_cols)) {
    message("It looks you are trying to give a numeric value for strat_cols, 
            perhaps you were trying to specify the number to sample instead? 
            If no strat_cols are specified you must explicitly specify 'n = ...' 
            For example resample_df(Theoph, 'Subject', n = 20 )")
    message("-----------------------------------")
    stop("To set the number of samples please explicitly specify 'n = <num>'.")
  }
  
  names <- c(key_col_name,names(df))
  key <- get_key(df, key_cols)
  if(is.null(n)) n <- nrow(key)
  
  if(is.null(strat_cols)) {
    sample <- dplyr::sample_n(key, size = n, replace=replace)
    sample[[key_col_name]] <- 1:n
  } else {
    strat_key <- get_key(df, c(key_cols, strat_cols))
    if(nrow(strat_key) != nrow(key)) {
      warning("Non-unique keys introduced from stratification,
              check that all keys only have one stratification variable associated
              ")}
    sample <- stratify_df(strat_key, strat_cols, n, replace = replace)
    sample <- dplyr::ungroup(sample)
    sample <- sample[, key_cols, drop=F] 
    sample[[key_col_name]] <- 1:nrow(sample)
    }
  resampled_df <- dplyr::left_join(sample, df, by = key_cols)
  return(resampled_df[,names, drop=F])
  }
