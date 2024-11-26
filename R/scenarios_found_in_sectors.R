#' Generate filtering vector for scenarios in sectors
#'
#' For a portfolio-shaped data.frame (with columns "ald_sector", "scenario",
#' and "scenario_source"), generate a logical vector indicating whether each
#' row is in the specified sectors and scenarios.
#'
#' @param data data.frame with columns "ald_sector", "scenario", and
#' "scenario_source"
#' @param select_scenario_param Concatenated Scenario string (source and name,
#' ex. "WEO2022_NZE_2050"). Rows in `data` matching this scenario source and
#' name will be included in the output.
#' @param sectors A character vector of PACTA sector names. Rows in `data`
#' matching any of these sectors will be included in the output.
#' @return Logical vector, of length equal to the number of rows in data,
#' indicating whether each row is in the specified sectors and scenarios.
#' Suitible for use as a filtering function in `dplyr::filter()`.
scenarios_found_in_sectors <- function(data, select_scenario_param, sectors) {
  out <- (data$ald_sector %in% sectors) &
    (data$scenario == get_scenario(select_scenario_param)) &
    (data$scenario_source == get_scenario_source(select_scenario_param))
  out
}
