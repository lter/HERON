#' @title Identify Slope Changes and Aggregate Across SiZer Bandwidths
#' 
#' @description Identifies all inflection points (i.e., changes in slope) across all bandwidths supplied to `SiZer::SiZer`. Averages within type of inflection point (i.e., +/- or -/+) to create 'aggregate' inflection points
#' 
#' @param sizer_object (SiZer) object returned by `SiZer::SiZer`
#' 
#' @importFrom magrittr %>%
#' 
#' @return (dataframe) slope type (flat, positive, or negative) identified by `SiZer::SiZer` at each x value aggregated across all bandwidths
#' 
#' @export
#' 
sizer_aggregate <- function(sizer_object = NULL){
  # Squelch visible bindings
  x <- h <- slope_becomes <- h_grid <- transition <- NULL
  change_type <- x_grid <- change_count <- sd <- NULL
  sd_x_v1 <- n_x_v1 <- mean_x_v1 <- diagnostic <- NULL
  se_x_v1 <- mean_x <- dist_to_next <- NULL
  
  # Error out if object isn't provided or isn't a SiZer object
  if(is.null(sizer_object) | methods::is(sizer_object, "SiZer") != TRUE)
    stop("`sizer_object` must be provided and must be class 'SiZer'")
  
  # Strip SiZer object content into dataframe
  sizer_raw <- as.data.frame(sizer_object)
  
  # Perform necessary wrangling
  sizer_data <- sizer_raw %>%
    # Rename columns
    dplyr::rename(x_grid = x, h_grid = h, slope_becomes = class) %>%
    # Drop all 'insufficient data' rows
    dplyr::filter(slope_becomes != "insufficient data") %>%
    # Within bandwidth levels (h_grid)
    dplyr::group_by(h_grid) %>%
    # Identify whether the next value is the same or different
    dplyr::mutate(transition = dplyr::case_when(
      # First identify start of each group
      is.na(dplyr::lag(slope_becomes, n = 1)) ~ 'start',
      # Now identify whether each value is the same as or different than previous
      slope_becomes == dplyr::lag(slope_becomes, n = 1) ~ 'same',
      slope_becomes != dplyr::lag(slope_becomes, n = 1) ~ 'change'
    )) %>%
    # Filter to retain only those rows that indicate a slope change
    dplyr::filter(transition == "change") %>%
    # Lets also identify what type of change the transition was
    dplyr::mutate(change_type = dplyr::case_when(
      transition == "change" & slope_becomes == "increasing" ~ 'change_to_positive',
      transition == "change" & slope_becomes == "flat" ~ 'change_to_zero',
      transition == "change" & slope_becomes == "decreasing" ~ 'change_to_negative')) %>%
    # Account for if multiple of the same change happen in a curve
    dplyr::group_by(h_grid, change_type) %>%
    dplyr::mutate(change_count = seq_along(unique(x_grid))) %>%
    # Ungroup for subsequent operations
    dplyr::ungroup() %>%
    # Group by change type
    dplyr::group_by(change_count, change_type) %>%
    # And average the x_grid value
    dplyr::summarise(slope_becomes = dplyr::first(slope_becomes),
                     mean_x_v1 = mean(as.numeric(x_grid), na.rm = T),
                     sd_x_v1 = sd(as.numeric(x_grid), na.rm = T),
                     n_x_v1 = dplyr::n(),
                     se_x_v1 = sd_x_v1 / n_x_v1,
                     .groups = 'keep') %>%
    # Ungroup
    dplyr::ungroup() %>%
    # Sort from lowest to highest X
    dplyr::arrange(mean_x_v1) %>%
    # Handle the same "change" occurring twice
    ## Identify these cases
    dplyr::mutate(diagnostic = cumsum(ifelse(slope_becomes != dplyr::lag(slope_becomes) | base::is.na(dplyr::lag(slope_becomes)), yes = 1, no = 0))) %>%
    ## Group by that diagnostic and the change type
    dplyr::group_by(change_type, diagnostic) %>%
    ## Summarize
    dplyr::summarise(change_count = dplyr::first(change_count),
                     slope_becomes = dplyr::first(slope_becomes),
                     mean_x = mean(as.numeric(mean_x_v1), na.rm = T),
                     sd_x = mean(as.numeric(sd_x_v1)),
                     n_x = sum(n_x_v1, na.rm = T),
                     se_x = mean(as.numeric(se_x_v1)),
                     .groups = 'keep') %>%
    ## Ungroup
    dplyr::ungroup() %>%
    ## Remove the diagnostic column
    dplyr::select(-diagnostic) %>%
    # Sort from lowest to highest X (again)
    dplyr::arrange(mean_x) %>%
    # Calculate distance to next one
    dplyr::mutate(dist_to_next = dplyr::lead(x = mean_x) - mean_x) %>%
    # Add half the distance between +/0 and 0/- to +/0 to get +/- inflection point
    dplyr::mutate(
      pos_to_neg = ifelse(change_type == "change_to_zero" &
                            dplyr::lead(change_type) == "change_to_negative",
                          yes = (mean_x + (dist_to_next / 2)),
                          no = NA),
      neg_to_pos = ifelse(change_type == "change_to_zero" &
                            dplyr::lead(change_type) == "change_to_positive",
                          yes = (mean_x + (dist_to_next / 2)),
                          no = NA) ) %>%
    # Remove 'dist to next' column
    dplyr::select(-dist_to_next) %>%
    # Make it a dataframe
    as.data.frame()
  
  # Return that data object
  return(sizer_data) }
