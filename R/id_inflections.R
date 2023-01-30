#' @title Identify Inflection Points in Slope
#' 
#' @description "Breaks" a given continuous variable at the *inflection points* identified by `sizer_aggregate` or `sizer_slice` and creates a column (named `groups`) that contains these group designations. Additionally, it identifies the start and end value of X for each group in two other new columns (named `start` and `end`, respectively)
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
id_inflections <- function(raw_data = NULL, x = NULL, y = NULL,
                           sizer_data = NULL, group_dig = 6){
  # Squelch visible bindings note
  groups <- rough_start <- rough_end <- simp_start <- simp_end <- NULL
  
  # Error out if these aren't provided
  if(is.null(raw_data) | is.null(sizer_data) | is.null(x) | is.null(y))
    stop("All arguments must be provided.")
  
  # Error out if the data are not both dataframes
  if(methods::is(raw_data, "data.frame") != TRUE | methods::is(sizer_data, "data.frame") != TRUE)
    stop("Both the raw data and the extracted SiZer data must be data frames")
  
  # Error out if the column names are not characters 
  if(!is.character(x) | !is.character(y))
    stop("The x and y columns must be specified as characters")
  
  # Error out if the column names are not in the data object
  if(!x %in% names(raw_data) | !y %in% names(raw_data))
    stop("`x` and `y` are not names in the provided `raw_data` object")
  
  # Grab inflection points
  brk_pts <- c(sizer_data$pos_to_neg, sizer_data$neg_to_pos)
  
  # Drop NAs
  brk_pts_actual <- brk_pts[!is.na(brk_pts)]

  # Create necessary columns
  data_mod <- raw_data %>%
    # Identify rough groups
    dplyr::mutate(
      groups = base::cut(x = tidyselect::all_of(raw_data[[x]]),
                         dig.lab = group_dig,
                         breaks = c(-Inf, brk_pts_actual, Inf)),
      .after = tidyselect::all_of(x)) %>%
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
