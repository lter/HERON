#' @title Create SiZer Base R Plot
#' 
#' @description Creates the base R `plot` returned for `SiZer` objects and adds horizontal lines at three bandwidths
#' 
#' @param sizer_object (SiZer) object returned by `SiZer::SiZer`
#' @param bandwidth_vec (numeric) bandwidths to apply a horizontal line in the plot
#' 
#' @export
#' 
sizer_plot <- function(sizer_object = NULL, bandwidth_vec = c(3, 6, 9)){
  
  # Error out if object isn't provided or isn't a SiZer object
  if(is.null(sizer_object) | methods::is(sizer_object, "SiZer") != TRUE)
    stop("`sizer_object` must be provided and must be class 'SiZer'")
  
  # Error out if bandwidth vector is non-numeric
  if(is.numeric(bandwidth_vec) == FALSE)
    stop("`bandwidth_vec` must be numeric")
  
  # Make plot
  plot(sizer_object)
  
  # Add horizontal lines at bandwidths of interest
  for(band in bandwidth_vec){ graphics::abline(h = base::log10(band)) }
}
