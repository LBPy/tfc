# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' @export
hello <- function() {
  print("Hello, world test!")
}


# devtools::install_github("hadley/devtools")
# has_devel()
#
# devtools::use_package("snow")
# devtools::use
# library(devtools)
# has_devel()
# devtools::load_all()
# .rs.restartR()
#
# use_package("snow")
# use_package("MonteCarlo")
# use_package("stabledist")
# use_package("parallel")
# use_package("rmutil")
# use_package("actuar")
#
# Authors@R: person("Lucas","Butsch",email = "lucas.butsch@gmail.com", role = c("aut", "cre"))
# x = sample(10)
# usethis::use_data(x)

#' Quantile of the D statistic with Median parameter estimation.
#'
#' A dataset contatining the 0.95 quantile of the D statistic gained by a 100k rep Monte-Carlo simulation.
#'
#' @format A data frame with 4 rows and 7 columns.
"quantile_D_Median"

#' Quantile of the D statistic with ML parameter estimation.
#'
#' A dataset containing the 0.95 quantile of the D statistic gained by a 100k rep Monte-Carlo simulation.
#'
#' @format A data frame with 4 rows and 7 columns.
"quantile_D_ML"

#' Quantile of the T1 statistic with Median parameter estimation.
#'
#' A dataset contatining the 0.95 quantile of the D statistic gained by a 100k rep Monte-Carlo simulation.
#'
#' @format A data frame with 4 rows and 7 columns.
"quantile_T1_Median"

#' Quantile of the T1 statistic with ML parameter estimation.
#'
#' A dataset containing the 0.95 quantile of the D statistic gained by a 100k rep Monte-Carlo simulation.
#'
#' @format A data frame with 4 rows and 7 columns.
"quantile_T1_ML"

#' Quantile of the T2 statistic with Median parameter estimation.
#'
#' A dataset contatining the 0.95 quantile of the D statistic gained by a 100k rep Monte-Carlo simulation.
#'
#' @format A data frame with 4 rows and 7 columns.
"quantile_T2_Median"

#' Quantile of the T2 statistic with ML parameter estimation.
#'
#' A dataset containing the 0.95 quantile of the D statistic gained by a 100k rep Monte-Carlo simulation.
#'
#' @format A data frame with 4 rows and 7 columns.
"quantile_T2_ML"

#' Quantile of the T3 statistic with Median parameter estimation.
#'
#' A dataset contatining the 0.95 quantile of the D statistic gained by a 100k rep Monte-Carlo simulation.
#'
#' @format A data frame with 4 rows and 7 columns.
"quantile_T3_Median"

#' Quantile of the T3 statistic with ML parameter estimation.
#'
#' A dataset containing the 0.95 quantile of the D statistic gained by a 100k rep Monte-Carlo simulation.
#'
#' @format A data frame with 4 rows and 7 columns.
"quantile_T3_ML"

#' Quantile of the T4 statistic with Median parameter estimation.
#'
#' A dataset contatining the 0.95 quantile of the D statistic gained by a 100k rep Monte-Carlo simulation.
#'
#' @format A data frame with 4 rows and 7 columns.
"quantile_T4_Median"

#' Quantile of the T4 statistic with ML parameter estimation.
#'
#' A dataset containing the 0.95 quantile of the D statistic gained by a 100k rep Monte-Carlo simulation.
#'
#' @format A data frame with 4 rows and 7 columns.
"quantile_T4_ML"

#' Quantile of the T5 statistic with Median parameter estimation.
#'
#' A dataset contatining the 0.95 quantile of the D statistic gained by a 100k rep Monte-Carlo simulation.
#'
#' @format A data frame with 4 rows and 7 columns.
"quantile_T5_Median"

#' Quantile of the T5 statistic with ML parameter estimation.
#'
#' A dataset containing the 0.95 quantile of the D statistic gained by a 100k rep Monte-Carlo simulation.
#'
#' @format A data frame with 4 rows and 7 columns.
"quantile_T5_ML"

#' Quantile of the KL statistic with Median parameter estimation.
#'
#' A dataset contatining the 0.95 quantile of the D statistic gained by a 100k rep Monte-Carlo simulation.
#'
#' @format A data frame with 4 rows and 1 columns.
"quantile_KL_Median"

#' Quantile of the KL statistic with ML parameter estimation.
#'
#' A dataset containing the 0.95 quantile of the D statistic gained by a 100k rep Monte-Carlo simulation.
#'
#' @format A data frame with 4 rows and 1 columns.
"quantile_KL_ML"

#' Quantile of the edf statistics with Median parameter estimation.
#'
#' A dataset contatining the 0.95 quantile of the D statistic gained by a 100k rep Monte-Carlo simulation.
#' Each row is for one of the classic test (KS, CM, AD,W) and the row is for the datasize (10,20,50,100,200)
#'
#' @format A data frame with 4 rows and 1 columns.
"quantile_edf_Median"

#' Quantile of the edf statistics with ML parameter estimation.
#'
#' A dataset containing the 0.95 quantile of the D statistic gained by a 100k rep Monte-Carlo simulation.
#' Each row is for one of the classic test (KS, CM, AD,W) and the row is for the datasize (10,20,50,100,200)
#'
#' @format A data frame with 5 rows and 3 columns.
"quantile_edf_ML"

#' tfc: A package for testing for Cauchy-distribution
#'
#' The tfc package provides multiple test and 3 ways to estimate paramteres of a Cauchy-Distribution.
#'
#' @section tfc functions:
#' \itemize{
#' \item \code{\link{D}}
#' \item \code{\link{distr}}
#' \item \code{\link{eise_est}}
#' \item \code{\link{median_est}}
#' \item \code{\link{ml_est}}
#' \item \code{\link{standardisiert}}
#' \item \code{\link{T1}}
#' \item \code{\link{T2}}
#' \item \code{\link{T3}}
#' \item \code{\link{T4}}
#' \item \code{\link{T5}}
#' \item \code{\link{testentscheid}}
#'}
#'
#' @section tfc data:
#' \itemize{
#' \item \code{\link{quantile_D_Median}}
#' \item \code{\link{quantile_D_ML}}
#' }
#'
#' @docType package
#' @name tfc
NULL


# usethis::use_vignette("my-vignette")
# install.packages("rmarkdown")
# library(rmarkdown)
# #usethis::use_testthat()
# test_that(D)
# test_that("Test that D computes the right value", expect_equal(D(y,l), n* integrate(function(t) sapply(t,f) , -Inf, Inf)$value))


#' @import stats utils
NULL

