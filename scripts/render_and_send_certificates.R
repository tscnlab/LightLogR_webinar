# Helper utilities to render LightLogR course certificates and send them via email.
#
# Usage:
#   Rscript scripts/render_and_send_certificates.R \
#     --csv=data/participants.csv \
#     --template=certificate.qmd \
#     --output-dir=certificates \
#     --smtp-key=lightlogr_smtp
#
# The CSV is expected to contain the columns:
#   name, email, level ("Beginner"|"Advanced"), completion_date (YYYY-MM-DD),
#   use_case (for advanced participants), use_case_focus (free text for advanced participants)

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(rlang)
  library(glue)
  library(fs)
})

parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  defaults <- list(
    csv = "data/participants.csv",
    template = "certificate.qmd",
    output_dir = "certificates",
    smtp_key = "lightlogr_smtp",
    dry_run = "FALSE"
  )

  arg_pairs <- args[str_detect(args, "--")]
  parsed <- map_chr(arg_pairs, ~ str_remove(.x, "^--") |> str_split_fixed("=", 2) |> {
    if (ncol(.) == 2) paste(.[1, 1], .[1, 2], sep = "=") else character()
  })

  values <- defaults
  for (entry in parsed) {
    if (length(entry) == 0) next
    split <- str_split_fixed(entry, "=", 2)
    key <- split[1]
    value <- split[2]
    values[[key]] <- value
  }

  values$dry_run <- str_to_upper(values$dry_run) == "TRUE"
  values
}

read_participants <- function(path) {
  read_csv(path, show_col_types = FALSE) |>
    rename_with(str_replace_all, " ", "_") |>
    mutate(level = str_to_title(level))
}

prepare_certificate_params <- function(participant, defaults = list()) {
  stopifnot(is.list(participant))
  base_params <- list(
    participant_name = participant$name %||% participant$participant_name,
    completion_date = participant$completion_date %||% format(Sys.Date(), "%Y-%m-%d"),
    level = participant$level %||% "Beginner",
    use_case = participant$use_case %||% defaults$use_case %||% "A Day in Daylight",
    use_case_focus = participant$use_case_focus %||% defaults$use_case_focus %||%
      "Documented advanced use case"
  )

  if (is.na(base_params$participant_name) || base_params$participant_name == "") {
    abort("Each participant row must include a name.")
  }

  if (str_to_title(base_params$level) == "Advanced") {
    base_params$level <- "Advanced"
  } else {
    base_params$level <- "Beginner"
  }

  base_params
}

render_certificate <- function(params, template, output_dir) {
  fs::dir_create(output_dir)
  output_path <- path(output_dir, glue("certificate-{str_replace_all(params$participant_name, "[^A-Za-z0-9]", "_")}.pdf"))

  quarto::quarto_render(
    input = template,
    execute_params = params,
    output_file = output_path,
    quiet = TRUE
  )

  output_path
}

create_email <- function(recipient_email, attachment, params, smtp_key) {
  blastula::compose_email(
    body = blastula::md(glue(
      "Hi {params$participant_name},\\n\\n",
      "Congratulations on completing the LightLogR webinar series!\\n",
      "Your certificate for the {params$level} track is attached.\\n\\n",
      "Best wishes,\\nThe LightLogR Team"
    ))
  ) |>
    blastula::add_attachment(file = attachment) |>
    blastula::smtp_send(
      from = "certificates@lightlogr.org",
      to = recipient_email,
      subject = glue("Your LightLogR certificate ({params$level})"),
      credentials = blastula::creds_key(id = smtp_key)
    )
}

send_certificates <- function(csv, template, output_dir, smtp_key, dry_run = FALSE) {
  participants <- read_participants(csv)

  pwalk(participants, function(...) {
    participant <- list(...)
    params <- prepare_certificate_params(participant)
    pdf_path <- render_certificate(params, template, output_dir)

    if (dry_run) {
      message(glue("[dry-run] Rendered certificate for {params$participant_name}: {pdf_path}"))
      return(invisible(pdf_path))
    }

    create_email(participant$email, pdf_path, params, smtp_key)
    message(glue("Sent certificate to {participant$email}"))
  })
}

if (identical(environment(), globalenv())) {
  opts <- parse_args()
  send_certificates(
    csv = opts$csv,
    template = opts$template,
    output_dir = opts$output_dir,
    smtp_key = opts$smtp_key,
    dry_run = opts$dry_run
  )
}
