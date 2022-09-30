#' @title Create SiZer `ggplot2` Plot
#' 
#' @description Creates a `ggplot2` plot of the trendline with slope changes identified by `SiZer` in dashed orange lines and inflection points (i.e., +/- or -/+ slope changes) identified in blue and red respectively
#' 
#' @param raw_data (dataframe) data object
#' @param x (character) column name in `raw_data` that is the x-axis
#' @param y (character) column name in `raw_data` that is the y-axis
#' @param sizer_data (dataframe) object returned by `id_slope_changes` or `id_inflections`
#' @param trendline (character) one of "smooth", "sharp", or "none". Smooth trendline is `geom_lm(method = "loess")`, sharp creates lines that are broken at slope changes/inflection points, "none" excludes the trendline
#' @param vline (character) one of "all", "inflections", "changes", or "none". "inflections' includes solid y-intercept lines at inflection points, "changes" includes dashed lines at slope change points, "all" includes both inflection and slope change vertical lines, "none" excludes vertical lines
#' @param sharp_colors (character) vector of length two that defines the colors to use for (1) slope increasing/decreasing and (2) flat slopes. This argument is only required or used when `trendline = sharp`
#' 
#' @export
#' 
sizer_ggplot <- function(raw_data = NULL, x = NULL, y = NULL,
                         sizer_data = NULL, trendline = "none",
                         vline = "all", 
                         sharp_colors = c("#DDDDDD", 'orange')){
  # Squlech visible bindings note
  groups <- slope_type <- NULL
  
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
    stop("`x` and `y` are not names in the provided `raw_data` object")
  
  # Error out for unsupported trendline call
  if(!trendline %in% c("smooth", "sharp", "none", FALSE))
    stop("`trendline` must be one of 'smooth', 'sharp', or 'none'")
  
  # Error out for unsupported vertical intercept call
  if(!vline %in% c("all", "inflections", "changes", "none", FALSE))
    stop("`vline` must be one of 'all', 'inflections', 'changes', or 'none'")
  
  # Error out if trendline is 'sharp' but color length is wrong
  if(trendline == "sharp" & length(sharp_colors) != 2)
    stop("`sharp_colors` must be a vector of two elements. The first is the color for the flat slope segments and the second is the color for the increasing/decreasing segments")
  
  # Warning if trendline is FALSE (rather than "none")
  if(trendline == FALSE){
    message("`trendline` should be set to 'none' if no trendline is desired. Defaulting to 'none'")
    trendline <- "none" }
  
  # Warning if vline is set to FALSE (rather than "none")
  if(vline == FALSE){
    message("`vline` should be set to 'none' if no x-intercepts are desired. Defaulting to 'none'")
    vline <- "none" }
  
  # Make the foundations of the actual plot
  p <- ggplot2::ggplot(data = raw_data,
                       ggplot2::aes_string(x = x, y = y)) +
    ggplot2::theme_classic()

  # Add x-intercepts for slope changes if desired
  if(vline %in% c("all", "changes")){
  p <- p + 
    ggplot2::geom_vline(xintercept = sizer_data$mean_x,
                        color = 'orange', linetype = 2, na.rm = TRUE) +
    ggplot2::geom_vline(xintercept = sizer_data$x_grid,
                        color = 'orange', linetype = 2, na.rm = TRUE) }
  
  # Add the positive to negative inflection point line(s) if one exists and they are desired
  if(!base::all(is.na(sizer_data$pos_to_neg)) &
     vline %in% c("all", "inflections")){
    p <- p +
      ggplot2::geom_vline(xintercept = sizer_data$pos_to_neg,
                          color = 'blue', na.rm = TRUE) }
  
  # Add *negative to positive* inflection point line(s) if one exists
  if(!base::all(is.na(sizer_data$neg_to_pos)) &
     vline %in% c("all", "inflections")){
    p <- p +
      ggplot2::geom_vline(xintercept = sizer_data$neg_to_pos,
                          color = 'red', na.rm = TRUE) }
  
  # Add the points to the plot (we want them "in front of" the vertical lines but "behind" the trendlines)
  p <- p +
    ggplot2::geom_point()
  
  # If `trendline` is 'smooth', add that trendline
  if(trendline == "smooth") {
    p <- p +
      ggplot2::geom_smooth(method = 'loess', formula = 'y ~ x', 
                           se = FALSE, color = 'black') }
  
  # If `trendline` is 'sharp', add that trendline
  if(trendline == "sharp"){
    p <- p +
      ggplot2::geom_smooth(ggplot2::aes(group = groups,
                                        color = slope_type),
                           method = stats::lm,
                           formula = 'y ~ x', se = FALSE) +
      ggplot2::scale_color_manual(values = sharp_colors) +
      ggplot2::theme(legend.position = 'none') }
  
  # Return the plot
  return(p) }
