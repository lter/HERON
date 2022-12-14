% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sizer_aggregate.R
\name{sizer_aggregate}
\alias{sizer_aggregate}
\title{Identify Slope Changes and Aggregate Across SiZer Bandwidths}
\usage{
sizer_aggregate(sizer_object = NULL)
}
\arguments{
\item{sizer_object}{(SiZer) object returned by `SiZer::SiZer`}
}
\value{
(dataframe) slope type (flat, positive, or negative) identified by `SiZer::SiZer` at each x value aggregated across all bandwidths
}
\description{
Identifies all inflection points (i.e., changes in slope) across all bandwidths supplied to `SiZer::SiZer`. Averages within type of inflection point (i.e., +/- or -/+) to create 'aggregate' inflection points
}
