#' Current and future ASMR estimates for BGC units in northern British Columbia
#'
#' A dataset containing normal and future ASMR estimates for 27 BGC units in northern BC, generated in
#' October 2019.
#'
#'
#' @format A list with 2 sublists: current and future.
#' Current refers to (1961-1990) normal period, as much as possible.  For some climate stations, we had to use data outside this range.
#'
#' \itemize{
#' \item class: ASMR classes, based on the asmrClass table in package.
#' \item asmr: AET/PET ratios.
#' \item stnYears: lists the 10 years used for each station to generate ASMR estimates.  Note: these were selected randomly.
#' * ...
#' }
#'
#' Note: If you re-run the code to generate asmr estimates using asmrSelect(), different years will be selected.
#' Note: BGC units not listed in the stnYears data frame have asmr estimates from adjacent BGC units within same subzone.
#' e.g., SBSwk3 estimates were copied from SBSwk1.
#'
#'

"asmrNorth"
#> [1] "asmrNorth"
