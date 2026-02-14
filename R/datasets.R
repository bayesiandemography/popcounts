
#' 'lx' Column from Abridged West Model Life Tables
#'
#' 'lx' columns from "abridged"
#' life tables from the "West" family of Coale-Demeny
#' model life tables. The life tables have
#' life expectancies at birth
#' of 35 years for females and 32.5 years for males,
#' corresponding to "level 7" in the original
#' West model life table system.
#' An "abridged" life table uses age groups
#' `"0"`, `"1-4"`, `"5-9"`, ... rather than
#' single-year age groups.
#'
#' The values in
#' `lx_west_7_complete` were not taken from the
#' original Coale-Demeny life tables, but instead from
#' a revised system constructed by the
#' United Nations Population Division. The revised
#' system extended the life table to age 130.
#' However, only values
#' up to age 100 are included here.
#'
#' @format A data frame with 202 rows and the
#' following variables:
#' - `sex` `"Female"`, and `"Male"`.
#' - `age` Abridged life table age groups
#'    up to `"100+"`.
#' - `lx` Number of people still alive at start of
#'   age interval.
#'
#' @seealso
#' [lx_west_7_complete] The "complete" 
#' equivalent of `lx_west_7_abridged`.
#'
#' @references
#' The original Coale-Demeny system is
#' described in Coale A, Demeny P, and Vaughn B. 1983.
#' Regional model life tables and stable populations.
#' 2nd ed. New York: Academic Press.
#'
#' The revised system is described in
#' Li, N., and P. Gerland (2011).
#' Modifying the Lee-Carter method to
#' project mortality changes up to 2100.
#' In Annual Meeting of the Population Association of America.
#' 
#' @source Calculated from
#' spreadsheet 'unpd_2011_mlt_130_2.5y_abridged.xlsx'
#' downloaded from the page "Model Life Tables"
#' on the United Nations Population Division
#' website on 19 January 2026.
"lx_west_7_abridged"



#' 'lx' Column from Complete West Model Life Tables
#'
#' 'lx' columns from "complete" (ie 1-year age group)
#' life tables from the "West" family of Coale-Demeny
#' model life tables. The life tables have
#' life expectancies at birth
#' of 35 years for females and 32.5 years for males,
#' corresponding to "level 7" in the original
#' West model life table system.
#'
#' The values in
#' `lx_west_7_complete` were not taken from the
#' original Coale-Demeny life tables, but instead from
#' a revised system constructed by the
#' United Nations Population Division. The revised
#' system extended the life table to age 130.
#' However, only values
#' up to age 100 are included here.
#'
#' @format A data frame with 202 rows and the
#' following variables:
#' - `sex` `"Female"`, and `"Male"`.
#' - `age` Single-year age groups, except for
#'    the final age group, `"100+"`.
#' - `lx` Number of people still alive at start of
#'   age interval.
#'
#' @seealso
#' [lx_west_7_abridged] The "abridged" 
#' equivalent of `lx_west_7_complete`,
#' using age groups `"0"`, `"1-4"`, `"5-9"`, ...
#'
#' @references
#' The original Coale-Demeny system is
#' described in Coale A, Demeny P, and Vaughn B. 1983.
#' Regional model life tables and stable populations.
#' 2nd ed. New York: Academic Press.
#'
#' The revised system is described in
#' Li, N., and P. Gerland (2011).
#' Modifying the Lee-Carter method to
#' project mortality changes up to 2100.
#' In Annual Meeting of the Population Association of America.
#' 
#' @source Calculated from
#' spreadsheet 'unpd_2011_mlt_130_2.5y_complete.xlsx'
#' downloaded from the page "Model Life Tables"
#' on the United Nations Population Division
#' website on 19 January 2026.
"lx_west_7_complete"
