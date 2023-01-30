#' @title Identify Slope Changes for One SiZer Bandwidth
#' 
#' @description Identifies the inflection points (i.e., +/- or -/+ slope changes) for the *one* specified bandwidth. Note that this bandwidth need not be an exact match for what is given to the `sizer_object` argument but should be close 
#' 
#' @param sizer_object (SiZer) object returned by `SiZer::SiZer`
#' @param bandwidth (numeric) bandwidth within `sizer_object` to extract
#' 
#' @importFrom magrittr %>%
#' 
#' @return (dataframe) slope type (flat, positive, or negative) identified by `SiZer::SiZer` at each x value for the specified bandwidth
#' 
#' @export
#' 
sizer_slice <- function(sizer_object = NULL, bandwidth = NULL){
  # Squelch visible bindings note
  x <- h <- slope_becomes <- h_grid <- transition <- NULL
  change_type <- x_grid <- dist_to_next <- NULL
  
  # Error out if object isn't provided or isn't a SiZer object
  if(is.null(sizer_object) | methods::is(sizer_object, "SiZer") != TRUE)
    stop("`sizer_object` must be provided and must be class 'SiZer'")
  
  # Error out if bandwidth isn't provided
  if(is.null(bandwidth)) stop("`bandwidth` must be provided")
  
  # If bandwidth is not a number, coerce it into being one
  if(!is.numeric(bandwidth)){
    bandwidth <- suppressWarnings(as.numeric(bandwidth)) }
  
  # Error out if coercion to numeric deleted bandwidth
  if(is.na(bandwidth))
    stop("`bandwidth` must be numeric (or coercible to numeric)")
  
  # Strip SiZer object content into dataframe
  sizer_raw <- as.data.frame(sizer_object)
  
  # Error out if bandwidth is not included in original sizer_object
  if(bandwidth < base::min(sizer_raw$h) | bandwidth > base::max(sizer_raw$h))
    stop("`bandwidth` is not included in range of bandwidths specified in `SiZer::SiZer` call. Change `bandwidth` or add desired bandwidth to `SiZer::SiZer` call.")
  
  # Perform necessary wrangling
  sizer_data <- sizer_raw %>%
    # Rename columns
    dplyr::rename(x_grid = x, h_grid = h, slope_becomes = class) %>%
    # Drop all 'insufficient data' rows
    dplyr::filter(slope_becomes != "insufficient data") %>%
    # Find the bandwidth closest to the user-specified value
    dplyr::filter(base::abs(h_grid - bandwidth) == base::min(base::abs(h_grid - bandwidth))) %>%
    # Group within the single bandwidth
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
    # Calculate distance to next one
    dplyr::mutate(dist_to_next = dplyr::lead(x = x_grid) - x_grid) %>%
    # Add half the distance between +/0 and 0/- to +/0 to get +/- inflection point
    dplyr::mutate(
      pos_to_neg = ifelse(change_type == "change_to_zero" &
                            dplyr::lead(change_type) == "change_to_negative",
                          yes = (x_grid + (dist_to_next / 2)),
                          no = NA),
      neg_to_pos = ifelse(change_type == "change_to_zero" &
                            dplyr::lead(change_type) == "change_to_positive",
                          yes = (x_grid + (dist_to_next / 2)),
                          no = NA) ) %>%
    # Remove 'dist to next' column
    dplyr::select(-dist_to_next) %>%
    # Make it a dataframe
    as.data.frame()
  
  # Return that object
  return(sizer_data) }
