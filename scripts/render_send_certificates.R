# Render and send LightLogR course certificates
#
# Usage:
#   Rscript scripts/render_send_certificates.R participants.csv
#
# The CSV is expected to contain columns:
#   name,email,completion_date,level,use_case,use_case_focus
# Levels accepted: "beginner" or "advanced" (case-insensitive). For beginner
# rows, leave `use_case` and `use_case_focus` empty.
#
# The script renders `_certificate.qmd` for each participant, stores PDFs in
# `certificates/`, and sends them using `blastula`. SMTP credentials are read
# from environment variables to keep secrets out of version control.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(glue)
  library(purrr)
})

# Helper for infix replacement without importing rlang explicitly
`%||%` <- function(x, y) if (is.null(x) || is.na(x) || identical(x, "")) y else x

#' Build a clean parameter list for the Quarto document
#'
#' @param row_data A single-row data frame with the required columns.
#' @return Named list suitable for `quarto::quarto_render()` params.
build_certificate_params <- function(row_data) {
  level <- tolower(row_data$level %||% "")
  if (!level %in% c("beginner", "advanced")) {
    stop("level must be 'beginner' or 'advanced'")
  }

  use_case <- if (level == "advanced") row_data$use_case %||% "Advanced use case" else "Core workflow"
  use_case_focus <- if (level == "advanced") row_data$use_case_focus %||% "Applied LightLogR analysis" else "Full LightLogR workflow"

  list(
    participant_name = row_data$name %||% "Participant",
    completion_date = row_data$completion_date %||% format(Sys.Date()),
    level = level,
    use_case = use_case,
    use_case_focus = use_case_focus
  )
}

#' Render one certificate
#'
#' @param params Named list from `build_certificate_params()`.
#' @param output_dir Directory to place the PDF.
#' @return Path to the rendered PDF.
render_certificate <- function(params, output_dir = "certificates") {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  output_file <- file.path(
    output_dir,
    glue("certificate_{gsub('[^A-Za-z0-9]+', '_', params$participant_name)}.pdf")
  )

  quarto::quarto_render(
    input = "_certificate.qmd",
    execute_params = params,
    output_file = basename(output_file),
    output_dir = output_dir,
    as_job = FALSE
  )

  output_file
}

#' Send a certificate email via blastula
#'
#' @param pdf_path Path to the rendered certificate PDF.
#' @param recipient_email Email address to send to.
#' @param participant_name Name to personalize the body.
#' @param smtp_host, smtp_port, smtp_user, smtp_password SMTP credentials.
#'   Defaults are read from environment variables.
#' @return Invisibly returns the blastula message object.
send_certificate_email <- function(
  pdf_path,
  recipient_email,
  participant_name,
  smtp_host = Sys.getenv("SMTP_HOST"),
  smtp_port = as.integer(Sys.getenv("SMTP_PORT", unset = "465")),
  smtp_user = Sys.getenv("SMTP_USER"),
  smtp_password = Sys.getenv("SMTP_PASSWORD")
) {
  if (!requireNamespace("blastula", quietly = TRUE)) {
    stop("blastula must be installed to send emails.")
  }

  email <- blastula::compose_email(
    body = blastula::md(glue(
      "Hi {participant_name},\n\n",
      "Congrats on completing the LightLogR course! Your PDF certificate is attached.\n\n",
      "Best wishes,\nThe LightLogR team"
    )),
    footer = blastula::md("This message was sent automatically via the LightLogR webinar tools.")
  )

  blastula::smtp_send(
    email = email,
    from = smtp_user,
    to = recipient_email,
    subject = "Your LightLogR course certificate",
    attachments = pdf_path,
    credentials = blastula::creds_smtp(
      host = smtp_host,
      port = smtp_port,
      user = smtp_user,
      pass = smtp_password,
      use_ssl = TRUE
    )
  )
}

#' Main driver: render and send all certificates in a CSV
process_participants <- function(csv_path) {
  participants <- readr::read_csv(csv_path, show_col_types = FALSE) %>%
    mutate(level = tolower(level))

  walk(seq_len(nrow(participants)), function(i) {
    row_data <- participants[i, ]
    params <- build_certificate_params(row_data)
    pdf_path <- render_certificate(params)
    message(glue("Rendered {pdf_path}"))

    if (!is.null(row_data$email) && !identical(row_data$email, "")) {
      send_certificate_email(
        pdf_path = pdf_path,
        recipient_email = row_data$email,
        participant_name = params$participant_name
      )
      message(glue("Sent certificate to {row_data$email}"))
    } else {
      message(glue("Skipped email for {params$participant_name}: no email provided."))
    }
  })
}

if (identical(environment(), globalenv()) && !interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 1) {
    stop("Usage: Rscript scripts/render_send_certificates.R participants.csv")
  }
  process_participants(args[[1]])
}
