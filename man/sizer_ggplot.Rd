% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sizer_ggplot.R
\name{sizer_ggplot}
\alias{sizer_ggplot}
\title{Create SiZer `ggplot2` Plot}
\usage{
sizer_ggplot(
  raw_data = NULL,
  x = NULL,
  y = NULL,
  sizer_data = NULL,
  trendline = "none",
  vline = "all",
  sharp_colors = c("#bbbbbb", "green")
)
}
\arguments{
\item{raw_data}{(dataframe) data object}

\item{x}{(character) column name in `raw_data` that is the x-axis}

\item{y}{(character) column name in `raw_data` that is the y-axis}

\item{sizer_data}{(dataframe) object returned by `id_slope_changes` or `id_inflections`}

\item{trendline}{(character) one of "smooth", "sharp", or "none". Smooth trendline is `geom_lm(method = "loess")`, sharp creates lines that are broken at slope changes/inflection points, "none" excludes the trendline}

\item{vline}{(character) one of "all", "inflections", "changes", or "none". "inflections' includes solid y-intercept lines at inflection points, "changes" includes dashed lines at slope change points, "all" includes both inflection and slope change vertical lines, "none" excludes vertical lines}

\item{sharp_colors}{(character) vector of length two that defines the colors to use for (1) flat slopes and (2) slope increasing/decreasing. This argument is only required or used when `trendline = sharp` If inflection points are found, only the second color will be used}
}
\description{
Creates a `ggplot2` plot of the trendline with slope changes identified by `SiZer` in dashed orange lines and inflection points (i.e., +/- or -/+ slope changes) identified in blue and red respectively
}
