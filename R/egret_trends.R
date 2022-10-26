#' @title Describe Flow-Normalized Trends in Concentration and Flux
#' 
#' @description Accepts the list returned by `EGRET::runSeries` and runs `EGRET::tableChangeSingle` for concentration and flux. Fixes column names to avoid characters that will be lost on export to CSV and binds rows to export as a single object. See the "Metric" column in the returned object to identify which rows are concentration and which are flux
#' 
#' @param eList_series (list) list returned by `EGRET::runSeries`. Must include a dataframe called "Daily" with a column called "Date"
#' @param flux_unit (numeric) object of fluxUnit class. See `EGRET::printFluxUnitCheatSheet()` for allowed integer codes
#' 
#' @return (dataframe) object containing flow normalized trends for both concentration and flux (see "Metric" column to differentiate)
#' 
#' @importFrom magrittr %>%
#' 
#' @export
#' 
egret_trends <- function(eList_series = NULL, flux_unit = 8){
  
  # Error out for missing argument
  if(is.null(eList_series))
    stop("`eList_series` must be provided. See `?egret_conc_flux` for more details")
  
  # Error out if eList_series doesn't include a "Daily" object
  if(!"Daily" %in% names(eList_series) )
    stop("`eList_series` must include a 'Daily' dataframe")
  
  # Error out for unsupported `flux_unit`
  if(!is.numeric(flux_unit) | !flux_unit %in% 1:13)
    stop("`flux_unit` must be a number between 1 and 13. See `printFluxUnitCheatSheet()` for more details")
  
  # Grab daily discharge values from that
  daily_values <- eList_series$Daily
  
  # Get a vector of years from that dataframe
  year_vec <- lubridate::year(as.Date(daily_values$Date))
  
  # Find minimum and maximum year
  min_year <- as.numeric(x = min(year_vec, na.rm = TRUE))
  max_year <- as.numeric(x = max(year_vec, na.rm = TRUE))
  
  # Assemble that into a two-element vector
  year_pts <- c(min_year, max_year)
  
  # Calculate concentration trend
  conc <- EGRET::tableChangeSingle(eList = eList_series, fluxUnit = flux_unit,
                                      yearPoints = year_points, flux = FALSE)
  
  # Calculate flux trend
  flux <- EGRET::tableChangeSingle(eList = eList_series, fluxUnit = flux_unit,
                                   yearPoints = year_points, flux = TRUE)
  
  # Add a column indicating which is which
  conc$Metric <- "Concentration"
  flux$Metric <- "Flux"
  
  # Get flexibly modified column names
  ## Remove spaces and trailing brackets (replace with nothing)
  conc_nm_v1 <- gsub(x = names(conc), pattern = " |\\]", replacement = "")
  flux_nm_v1 <- gsub(x = names(flux), pattern = " |\\]", replacement = "")
  ## Exchange leading brackets and slashes for underscores
  conc_nm_v2 <- gsub(x = conc_nm_v1, pattern = "\\[|\\/|\\^", replacement = "_")
  flux_nm_v2 <- gsub(x = flux_nm_v1, pattern = "\\[|\\/|\\^", replacement = "_")
  # Change percent signs into that word
  conc_nm_v3 <- gsub(x = conc_nm_v2, pattern = "\\%", replacement = "percent")
  flux_nm_v3 <- gsub(x = flux_nm_v2, pattern = "\\%", replacement = "percent")
  
  # Fix the column names!
  names(conc) <- conc_nm_v3
  names(flux) <- flux_nm_v3
  
  # Bind these dataframes together
  trends <- conc %>% 
    dplyr::bind_rows(flux) %>%
    # And move the "Metric" column to the left
    dplyr::relocate(Metric, .before = dplyr::everything()) %>%
    # Make it a dataframe
    as.data.frame()
 
  # Return the trends object
  return(trends) }
