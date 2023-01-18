
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/HERON_hex.png" align="right" width="15%"/>

# `HERON` - *HE*lpers for *R*iver *O*bservatio*N*

<!-- badges: start -->

[![R-CMD-check](https://github.com/lter/HERON/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lter/HERON/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of HERON is to provide a suite of helpful supplementary
functions for workflows involving the `EGRET` and `SiZer` R packages.

## Installation

You can install the development version of HERON from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("lter/HERON")
```

## `HERON` Functions

Here are the functions currently included in `HERON`:

### `EGRET` Helper Functions

- **`egret_report`**: Generate a coarse PDF report containing plots of
  `EGRET` function outputs. Useful for quickly characterizing a suite of
  patterns in the data though allows little aesthetic customization and
  is thus best used only for this preliminary “first look”

- **`egret_trends`**: Calculates flow normalized concentration and flux
  using `EGRET::tableChangeSingle` and exports a single data object
  containing both datasets (includes a column named “Metric” to help
  users split the objects *post hoc*)

### `SiZer` Helper Functions

- **`sizer_slice`**: Extract the `SiZer`-identified slope for a
  specified bandwidth and returns this information as a dataframe

- **`sizer_aggregate`**: Aggregate `SiZer`-identified slope types across
  all bandwidths and return this information as a dataframe

- **`sizer_plot`**: Creates base R plot of `SiZer` object with
  horizontal lines at specified bandwidths

- **`id_slope_changes`**: Migrate slope changes identified by
  `SiZer::SiZer` information into the supplied data object

- **`id_inflections`**: Migrate inflection points implied by
  `SiZer::SiZer` information (“implied” because it only identifies slope
  changes to/from flat slopes) into the supplied data object

- **`sizer_ggplot`**: Creates `ggplot2` plot of trendline with
  inflection points and/or slope changes identified by `SiZer` included
  as vertical lines. Also allows specification of type of trendline to
  fit to data

- **`sizer_lm`**: Fits linear regressions on each “chunk” of the
  trendline (i.e., sections of trendline that share a common slope as
  identified by `id_inflections` or `id_slope_changes`)

### Misc. Other Functions

- **`hydro_day`**: Accepts a calendar date and converts it into an
  equivalent “hydro day”. Allows user to specify the first month of the
  hydrologic year as this may differ by hemisphere
