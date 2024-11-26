#' Get scenario name from concatenated scenario string
#'
#' Get the Scenario name from a concatenated scenario string (for example,
#' "NZE_2050" from "WEO2022_NZE_2050")
#'
#' @param scenario_parameter A string containing the scenario source and name,
#' separated by a single underscore
#' @return the scenario name part of the string
get_scenario <- function(scenario_parameter) {
  scenario <- unlist(stringr::str_split(scenario_parameter,"_", n = 2))[2]
  scenario
}

#' Get scenario source from concatenated scenario string
#'
#' Get the Scenario source from a concatenated scenario string (for example,
#' "WEO2022" from "WEO2022_NZE_2050")
#'
#' @param scenario_parameter A string containing the scenario source and name,
#' separated by a single underscore
#' @return the scenario source part of the string
get_scenario_source <- function(scenario_parameter) {
  source <- unlist(stringr::str_split(scenario_parameter,"_", n = 2))[1]
  source
}
