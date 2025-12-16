library(testthat)
source("scripts/render_and_send_certificates.R")

sample_row <- list(
  name = "Test User",
  email = "user@example.com",
  level = "advanced",
  completion_date = "2025-05-01",
  use_case = "Visual experience",
  use_case_focus = "Pilot VEET spectral and distance analysis"
)


test_that("prepare_certificate_params normalises levels and fills defaults", {
  params <- prepare_certificate_params(sample_row)

  expect_equal(params$participant_name, "Test User")
  expect_equal(params$level, "Advanced")
  expect_equal(params$use_case, "Visual experience")
  expect_equal(params$use_case_focus, "Pilot VEET spectral and distance analysis")
})

test_that("prepare_certificate_params validates missing names", {
  expect_error(
    prepare_certificate_params(list(email = "missing@example.com")),
    "must include a name"
  )
})

test_that("render_certificate produces a PDF when quarto is available", {
  skip_if_not_installed("quarto")
  skip_if(quarto::quarto_path() == "", "Quarto CLI is not available")

  tmp_dir <- file.path(tempdir(), "cert_render")
  dir.create(tmp_dir, showWarnings = FALSE)

  params <- list(
    participant_name = "Sample Participant",
    completion_date = "2025-01-01",
    level = "Beginner",
    use_case = "A Day in Daylight",
    use_case_focus = ""
  )

  pdf_path <- render_certificate(params, "certificate.qmd", tmp_dir)
  expect_true(fs::file_exists(pdf_path))
  expect_match(pdf_path, "\.pdf$")
})
