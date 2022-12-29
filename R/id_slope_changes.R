#' @title Identify Changes in Slope
#' 
#' @description "Breaks" a given continuous variable at the *slope changes* identified by `sizer_aggregate` or `sizer_slice` and creates a column (named `groups`) that contains these group designations. Additionally, it identifies the start and end value of X for each group in two other new columns (named `start` and `end`, respectively)
#' 
#' @param raw_data (dataframe) data object
#' @param x (character) column name in `raw_data` that is the x-axis
#' @param y (character) column name in `raw_data` that is the y-axis
#' @param sizer_data (dataframe) object returned by `sizer_slice` or `sizer_aggregate`
#' @param group_dig (number) digits to round group edges to
#' 
#' @importFrom magrittr %>%
#' 
#' @export
#' 
id_slope_changes <- function(raw_data = NULL, x = NULL, y = NULL,
                             sizer_data = NULL, group_dig = 6){
  # Squelch visible bindings note
  mean_x <- groups <- change_type <- slope_type_rough <- NULL
  rough_start <- rough_end <- simp_start <- simp_end <- NULL
  
  # Error out if these aren't provided
  if(is.null(raw_data) | is.null(sizer_data) | is.null(x) | is.null(y))
    stop("All arguments must be provided")
  
  # Error out if the data are not both dataframes
  if(class(raw_data) != "data.frame" | class(sizer_data) != "data.frame") 
    stop("Both the raw data and the extracted SiZer data must be data frames")
  
  # Error out if the column names are not characters 
  if(!is.character(x) | !is.character(y))
    stop("The x and y columns must be specified as characters")
  
  # Error out if the column names are not in the data object
  if(!x %in% names(raw_data) | !y %in% names(raw_data))
    stop("`x` and/or `y` is not in the provided `raw_data` object")
  
  # Rename the aggregate X column to make downstream operations easier
  if("mean_x" %in% names(sizer_data)){
    sizer_data <- sizer_data %>%
      dplyr::rename(x_grid = mean_x) }
  
  # Grab slope change points
  brk_pts <- c(sizer_data$x_grid)
  
  # Drop NAs
  brk_pts_actual <- brk_pts[!is.na(brk_pts)]
  
  # Make a simpler version of the sizer data (we'll need this later)
  sizer_simp <- sizer_data %>%
    # Break X by breakpoints identified in this dataframe
    dplyr::mutate(groups = base::cut(x = sizer_data$x_grid, 
                                     dig.lab = group_dig,
                                     breaks = c(-Inf, brk_pts_actual, Inf))) %>%
    # Identify what the slope *is* (rather than what it changes to)
    dplyr::mutate(slope_type_rough = dplyr::case_when(
      slope_becomes == "flat" ~ "increasing/decreasing",
      slope_becomes != "flat" ~ "approx. zero") ) %>%
    # Crop to needed columns
    dplyr::select(groups, change_type, slope_type_rough) %>%
    as.data.frame()
  
  # Create necessary columns
  data_mod <- raw_data %>%
    # Identify rough groups
    dplyr::mutate(
      groups = base::cut(x = raw_data[[x]],
                         dig.lab = group_dig,
                         breaks = c(-Inf, brk_pts_actual, Inf)),
      .after = tidyselect::all_of(x)) %>%
    # Attach slope types from simplified SiZer object
    dplyr::left_join(y = sizer_simp, by = "groups") %>%
    # Fill through the last group (it is only implied by the `cut` groups)
    tidyr::fill(change_type) %>%
    # Combine that column with the `slope_type_rough` column
    dplyr::mutate(slope_type = dplyr::case_when(
      !is.na(slope_type_rough) ~ slope_type_rough,
      is.na(slope_type_rough) &
        change_type == "change_to_zero" ~ "approx. zero",
      is.na(slope_type_rough) &
        change_type != "change_to_zero" ~ "increasing/decreasing"),
      .after = groups) %>%
    # Remove intermediary columns
    dplyr::select(-change_type, -slope_type_rough) %>%
    # Identify start / end years from the groups
    tidyr::separate(col = groups, sep = ",", remove = FALSE,
                    into = c('rough_start', 'rough_end')) %>%
    # Remove parentheses / brackets
    dplyr::mutate(
      simp_start = stringr::str_sub(
        string = rough_start, start = 2, end = nchar(rough_start)),
      simp_end = gsub(pattern = "]| ", replacement = "", 
                      x = rough_end)) %>%
    # Swap "Inf" and "-Inf" for the actual start/end X values
    dplyr::mutate(
      start = as.numeric(ifelse(test = simp_start == -Inf,
                                yes = dplyr::first(raw_data[[x]]),
                                no = floor(x = as.numeric(simp_start)))),
      end = as.numeric(ifelse(test = simp_end == Inf,
                              yes = dplyr::last(raw_data[[x]]),
                              no = floor(x = as.numeric(simp_end)))),
      .after = groups) %>%
    # Remove intermediary columns
    dplyr::select(-rough_start, -rough_end,
                  -simp_start, -simp_end) %>%
    # Make it a dataframe
    as.data.frame()
  
  # Return that modified dataframe
  return(data_mod) }
