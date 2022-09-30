#' @title Linear Models with SiZer
#' 
#' @description Fits separate linear models for each group of the line between the break points identified by `id_inflections` or `id_slope_changes`
#' 
#' @param data (dataframe) data object returned by `id_inflections` or `id_slope_changes`
#' @param x (character) column name in `data` that is the x-axis
#' @param y (character) column name in `data` that is the y-axis
#' @param group_col (character) column name in `data` that identifies sections of trendline that share a common slope (see object returned by `id_inflections` or `id_slope_changes`)
#' 
#' @importFrom magrittr %>%
#' 
#' @return (list) containing (1) the model summary statistics and (2) the the estimates of the intercept and line
#' 
#' @export
#' 
sizer_lm <- function(data = NULL, x = NULL, y = NULL, group_col = NULL){
  # Squelch visible bindings note
  . <- NULL
  
  # Error out if these aren't provided
  if(is.null(data) | is.null(group_col) | is.null(x) | is.null(y))
    stop("All arguments must be provided.")
  
  # Error out if the data are not both dataframes
  if(class(data) != "data.frame") 
    stop("Both the raw data and the extracted SiZer data must be data frames")
  
  # Error out if the column names are not characters 
  if(!is.character(x) | !is.character(y) | !is.character(group_col))
    stop("`x`, `y`, and `group_col` must be specified as characters")
  
  # Error out if the column names are not in the data object
  if(!x %in% names(data) | !y %in% names(data) | !group_col %in% names(data))
    stop("At least one of `x`, `y`, or `group_col` are not names in the provided `data` object")
  
  # Handle plots without break points
  if(length(unique(data[[group_col]])) == 1){
    # Just fit model
    model_fit <- stats::lm(data[[y]] ~ data[[x]])
    
    # Identify statistics and estimates
    stat_df <- data.frame(broom::glance(model_fit)) %>%
      dplyr::mutate(section = "No inflection points",
                    .before = dplyr::everything())
    est_df <- data.frame(broom::tidy(model_fit)) %>%
      dplyr::mutate(section = "No inflection points",
                    .before = dplyr::everything())
    
    # Make an empty list
    return_list <- list()
    
    # Put the data in it
    return_list[["Stat"]] <- stat_df
    return_list[["Estim"]] <- est_df
    
    # Return it
    return(return_list)
    
    # If there *are* break points
  } else {
    
    # Make an empty list to store each model
    model_fit_list <- list()
    
    # Fit a model for each
    for(chunk in unique(data[[group_col]])){
      
      # Fit model
      chunk_fit <- model_fit <- stats::lm(data[[y]] ~ data[[x]], subset = data[[group_col]] == chunk)
      
      # Grab statistics
      chk_stat_df <- data.frame(broom::glance(chunk_fit)) %>%
        dplyr::mutate(section = chunk, .before = dplyr::everything())
      
      # Grab estimates
      chk_est_df <- data.frame(broom::tidy(chunk_fit)) %>%
        dplyr::mutate(section = chunk, .before = dplyr::everything())
      
      # Add it to the list
      model_fit_list[[paste0("Stats_", chunk)]] <- chk_stat_df
      model_fit_list[[paste0("Estimates_", chunk)]] <- chk_est_df
    }
    
    # Unlist the statistic list
    stat_bit <- model_fit_list %>%
      # Identify all list elements that contain stats info
      purrr::keep(.p = stringr::str_detect(string = names(.),
                                           pattern = "Stats")) %>%
      # Unlist by selecting all columns of each list element
      purrr::map_dfr(.f = dplyr::select, dplyr::everything())
    
    # Unlist the estimate list too
    est_bit <- model_fit_list %>%
      purrr::keep(.p = stringr::str_detect(string = names(.), pattern = "Estimates")) %>%
      purrr::map_dfr(.f = dplyr::select, dplyr::everything())
    
    # Now that we have these as two complete dataframes (rather than however many "chunks" there were) we'll add them *back into a new list!*
    
    # Make an empty list
    return_list <- list()
    
    # Put the data in it
    return_list[["Stat"]] <- stat_bit
    return_list[["Estim"]] <- est_bit
    
    # Return it
    return(return_list) }
}
