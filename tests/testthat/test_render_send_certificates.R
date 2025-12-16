library(testthat)
suppressWarnings(source("scripts/render_send_certificates.R"))

test_that("build_certificate_params handles beginner defaults", {
  params <- build_certificate_params(data.frame(
    name = "Beginner One",
    completion_date = "2025-10-07",
    level = "Beginner",
    use_case = NA_character_,
    use_case_focus = NA_character_
  ))

  expect_equal(params$participant_name, "Beginner One")
  expect_equal(params$level, "beginner")
  expect_equal(params$use_case, "Core workflow")
  expect_equal(params$use_case_focus, "Full LightLogR workflow")
})

test_that("build_certificate_params requires valid level", {
  expect_error(build_certificate_params(data.frame(level = "intermediate")),
               "beginner")
})

test_that("build_certificate_params keeps advanced use case info", {
  params <- build_certificate_params(data.frame(
    name = "Advanced Two",
    completion_date = "2025-10-07",
    level = "advanced",
    use_case = "Therapy lamps",
    use_case_focus = "Merging logs"
  ))

  expect_equal(params$use_case, "Therapy lamps")
  expect_equal(params$use_case_focus, "Merging logs")
})
