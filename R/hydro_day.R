#' @title Convert from Calendar Years to Hydro Years
#' 
#' @description Hydrological years begin in October so this function converts calendar dates into hydrologic dates.
#' 
#' @param cal_date (date) calendar date to convert
#' @param start_month (numeric) number of month to start the hydro year
#' 
#' @export
#' 
hydro_day = function(cal_date = NULL, start_month = 10){
  
  # Error out if date is NULL
  if(is.null(cal_date))
    stop("`cal_date` must be defined")
  
  # Error out if start_month isn't a number
  if(!is.numeric(start_month))
    stop("`start_month` must be numeric")
  
  # Identify starting year
  start_yr <- lubridate::year(cal_date) - (lubridate::month(cal_date) < start_month)
  
  # Now generate a new date from that modified year
  start_date <- lubridate::make_date(start_yr, start_month, 1L)
  
  # Grab that as an integer
  as.integer(cal_date - start_date + 1L) }
