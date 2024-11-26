test_that("scenarios_found_in_sectors returns empty vector for empty data.frame", {
  test_portfolio <- data.frame(
    ald_sector = character(),
    scenario = character(),
    scenario_source = character()
  )
  results <- scenarios_found_in_sectors(
    data = test_portfolio,
    select_scenario_param = "SOURCE_SCENARIO",
    sectors = "Foo"
  )
  expect_identical(
    object = results,
    expected = logical()
  )
})

test_that("scenarios_found_in_sectors returns logical vector for populated data.frame", {
  test_portfolio <- data.frame(
    ald_sector = rep("Foo", 4L),
    scenario = rep("SCENARIO", 4L),
    scenario_source = rep("SOURCE", 4L)
  )
  results <- scenarios_found_in_sectors(
    data = test_portfolio,
    select_scenario_param = "SOURCE_SCENARIO",
    sectors = "Foo"
  )
  expect_identical(
    object = results,
    expected = c(TRUE, TRUE, TRUE, TRUE)
  )
})

test_that("scenarios_found_in_sectors rejects rows with incorrect sector", {
  test_portfolio <- data.frame(
    ald_sector = rep(c("Foo", "Bar"), 2L),
    scenario = rep("SCENARIO", 4L),
    scenario_source = rep("SOURCE", 4L)
  )
  results <- scenarios_found_in_sectors(
    data = test_portfolio,
    select_scenario_param = "SOURCE_SCENARIO",
    sectors = "Foo"
  )
  expect_identical(
    object = results,
    expected = c(TRUE, FALSE, TRUE, FALSE)
  )
})

test_that("scenarios_found_in_sectors rejects rows with incorrect source", {
  test_portfolio <- data.frame(
    ald_sector = rep("Foo", 4L),
    scenario = rep("SCENARIO", 4L),
    scenario_source = rep(c("SOURCE", "source2"), 2L)
  )
  results <- scenarios_found_in_sectors(
    data = test_portfolio,
    select_scenario_param = "SOURCE_SCENARIO",
    sectors = "Foo"
  )
  expect_identical(
    object = results,
    expected = c(TRUE, FALSE, TRUE, FALSE)
  )
})

test_that("scenarios_found_in_sectors rejects rows with incorrect scenario", {
  test_portfolio <- data.frame(
    ald_sector = rep("Foo", 4L),
    scenario = rep(c("SCENARIO", "scenario2"), 2L),
    scenario_source = rep("SOURCE", 4L)
  )
  results <- scenarios_found_in_sectors(
    data = test_portfolio,
    select_scenario_param = "SOURCE_SCENARIO",
    sectors = "Foo"
  )
  expect_identical(
    object = results,
    expected = c(TRUE, FALSE, TRUE, FALSE)
  )
})

test_that("scenarios_found_in_sectors accepts multiple sectors", {
  test_portfolio <- data.frame(
    ald_sector = c("Foo", "Bar", "Mlem", "Foo"),
    scenario = rep("SCENARIO", 4L),
    scenario_source = rep("SOURCE", 4L)
  )
  results <- scenarios_found_in_sectors(
    data = test_portfolio,
    select_scenario_param = "SOURCE_SCENARIO",
    sectors = c("Foo", "Bar")
  )
  expect_identical(
    object = results,
    expected = c(TRUE, TRUE, FALSE, TRUE)
  )
})

test_that("scenarios_found_in_sectors is order insensitive for sectors", {
  test_portfolio <- data.frame(
    ald_sector = c("Foo", "Bar", "Mlem", "Foo"),
    scenario = rep("SCENARIO", 4L),
    scenario_source = rep("SOURCE", 4L)
  )
  results <- scenarios_found_in_sectors(
    data = test_portfolio,
    select_scenario_param = "SOURCE_SCENARIO",
    sectors = rev(c("Foo", "Bar"))
  )
  expect_identical(
    object = results,
    expected = c(TRUE, TRUE, FALSE, TRUE)
  )
})

# test_that("scenarios_found_in_sectors throws error if missing required columns", {
#   test_portfolio <- data.frame()
#   expect_error(
#     object = {
#     scenarios_found_in_sectors(
#       data = test_portfolio,
#       select_scenario_param = "SOURCE_SCENARIO",
#       sectors = "Foo"
#     )
#     },
#     regexp = "data must contain columns 'ald_sector', 'scenario', and 'scenario_source'."
#   )
# })

# test_that("scenarios_found_in_sectors throws error select_scenario_param is not scalar", {
#   test_portfolio <- data.frame()
#   expect_error(
#     object = {
#     scenarios_found_in_sectors(
#       data = test_portfolio,
#       select_scenario_param = c("SOURCE_SCENARIO", "SOURCE2_SCENARIO2"),
#       sectors = "Foo"
#     )
#     },
#     regexp = "select_scenario_param must be a scalar string."
#   )
# })
