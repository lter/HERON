#' @title Generate Rough Report of `EGRET` Outputs
#' 
#' @description Accepts the lists returned by `EGRET::modelEstimation` and `EGRET::runSeries` and generates a PDF report containing several coarse plots generated from `EGRET` functions for dealing with these lists. Useful for quick assessment of broad patterns in response but more finely-constructed plots should be built elsewhere
#' 
#' @param eList_estim (list) list returned by `EGRET::modelEstimation`
#' @param eList_series (list) list returned by `EGRET::runSeries`
#' @param out_path (character) name to save report as (must be a PDF) and (optionally) the path to export (defaults to `getwd()`)
#' 
#' @export
#'
egret_report <- function(eList_estim = NULL, eList_series = NULL, out_path = file.path(getwd(), "EGRET_report.pdf")){

  # Error out for missing arguments
  if(is.null(eList_estim) | is.null(eList_series))
    stop("Both `eList_estim` and `eList_series` must be provided. See `?egret_report` for more details")
  
  # Error out for missing file suffix
  if(stringr::str_sub(string = out_path,
                      start = (nchar(out_path) - 3),
                      end = nchar(out_path)) != ".pdf")
    stop("`out_path` must include a '.pdf' suffix")
    
  # Create a PDF report
  grDevices::pdf(file = out_path)
  
  # Residual plots
  EGRET::fluxBiasMulti(eList = eList_estim)
  
  # Model fit
  EGRET::plotConcTimeDaily(eList = eList_series)
  
  # Concentration
  EGRET::plotConcHist(eList = eList_series)
  
  # Flux
  EGRET::plotFluxHist(eList = eList_series)
  
  # Data
  EGRET::multiPlotDataOverview(eList = eList_series)
  
  # Close off / export report
  grDevices::dev.off() }
