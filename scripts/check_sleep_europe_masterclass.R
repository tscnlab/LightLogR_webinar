# Verify the exact live cells and refresh the downloadable script and saved examples.
# Run from the project root: Rscript --vanilla scripts/check_sleep_europe_masterclass.R
# An optional MASTERCLASS_R_LIBRARY selects a separate validation library.
validation_library <- Sys.getenv("MASTERCLASS_R_LIBRARY")
if (nzchar(validation_library)) .libPaths(c(validation_library, .libPaths()))
Sys.setenv(R_USER_CACHE_DIR = file.path(tempdir(), "masterclass-cache"))

source_file <- "sleep-europe-masterclass.qmd"
lines <- readLines(source_file, warn = FALSE)
starts <- grep("^```\\{webr\\}", lines)
cells <- lapply(starts, function(start) {
  end <- start + which(lines[(start + 1):length(lines)] == "```")[1]
  body <- lines[(start + 1):(end - 1)]
  label <- sub("^#\\| label: *", "", grep("^#\\| label:", body, value = TRUE))
  list(label = label, code = body[!grepl("^#\\|", body)])
})
stopifnot(length(cells) == 14L, !anyDuplicated(vapply(cells, `[[`, "", "label")))

dir.create("assets/masterclass", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/masterclass", recursive = TRUE, showWarnings = FALSE)
script <- c(
  "# Sleep Europe 2026: Masterclass personal light exposure. Live code in reading order.",
  "# Install LightLogR, dplyr, tidyr, ggplot2, glcdp and gt in a local R environment first.",
  "# Download melidos-iztech-teaching.zip into the working directory as well.",
  "# Verified versions and source details: outputs/masterclass/verification.txt.",
  "# Changing an upstream cell requires rerunning the later dependent cells.",
  unlist(lapply(cells, function(cell) c("", paste("#", cell$label), cell$code))),
  "", "# Save software provenance with your analysis.", "sessionInfo()"
)
writeLines(script, "scripts/sleep-europe-masterclass.R")

environment <- new.env(parent = globalenv())
transcripts <- list()
for (cell in cells) {
  message("Checking ", cell$label)
  plot_files <- list("mc-plot" = c("example-timeline.png", "example-gaps.png"),
                    "mc-inspect" = "example-overview.png",
                    "mc-window" = "example-window-overview.png",
                    "mc-clean" = "example-clean-overview.png",
                    "mc-sleep-join" = "example-sleep-states.png")
  is_plot <- cell$label %in% names(plot_files)
  if (is_plot) {
    plot_directory <- tempfile(pattern = "masterclass-plots-")
    dir.create(plot_directory)
    png(file.path(plot_directory, "plot-%d.png"),
        width = if (cell$label == "mc-sleep-join") 2240 else 1600,
        height = if (cell$label == "mc-sleep-join") 720 else 576,
        res = 160, type = "cairo")
  }
  timing <- system.time({
    output <- capture.output({
      for (expression in parse(text = cell$code)) {
        result <- withVisible(eval(expression, envir = environment))
        if (result$visible && inherits(result$value, "gt_tbl")) {
          html <- gt::as_raw_html(result$value)
          table_file <- if (cell$label == "mc-sleep-metrics") "example-sleep-metrics.html" else "example-daily.html"
          writeLines(html, file.path("assets/masterclass", table_file))
          cat("Formatted table rendered to", table_file, "\n")
        } else if (result$visible) print(result$value)
      }
    })
  })
  if (is_plot) {
    dev.off()
    plot_paths <- list.files(plot_directory, pattern = "^plot-", full.names = TRUE)
    stopifnot(length(plot_paths) == length(plot_files[[cell$label]]))
    file.copy(plot_paths, file.path("assets/masterclass", plot_files[[cell$label]]), overwrite = TRUE)
  }
  transcripts[[cell$label]] <- c(output, paste("Elapsed seconds:", timing[["elapsed"]]))
}

# Verify scientific invariants in R, independently of presentation formatting.
with(environment, {
  stopifnot(all(as.numeric(count_difftime(raw)$difftime) == 10),
            !has_gaps(raw), !has_irregulars(raw),
            !anyDuplicated(raw[c("Id", "Datetime")]),
            identical(names(raw), c("Id", "Datetime", "MEDI")))
  stopifnot(nrow(selected) == 17280L, nrow(practice) == 16920L)
  stopifnot(all(is.finite(selected$MEDI)), all(selected$MEDI >= 0),
            all(selected$Datetime >= as.POSIXct("2023-08-29", tz = tz)),
            all(selected$Datetime < as.POSIXct("2023-08-31", tz = tz)))
  stopifnot(nrow(clean) == 2880L, sum(is.na(clean$MEDI)) == 60L)
  stopifnot(all(as.numeric(diff(clean$Datetime), units = "secs") == 60))
  stopifnot(inherits(daily$expected_h, "Duration"),
            inherits(daily$observed_h, "Duration"),
            all(as.numeric(daily$expected_h) / 3600 == 24),
            identical(as.numeric(daily$observed_h) / 3600, c(23, 24)),
            is.numeric(daily$coverage_pct),
            isTRUE(all.equal(daily$coverage_pct, c(100 * 23 / 24, 100))))
  stopifnot(identical(group_vars(day_data), c("Id", "Date")))
  threshold_col <- paste0("duration_above_", threshold_lx)
  threshold_h <- as.numeric(daily[[threshold_col]]) / 3600
  stopifnot(all(threshold_h <= as.numeric(daily$observed_h) / 3600))
  direct_hours <- clean |>
    dplyr::group_by(Id, day = as.Date(Datetime, tz = tz)) |>
    dplyr::summarise(hours = sum(MEDI >= threshold_lx, na.rm = TRUE) / 60,
                     .groups = "drop")
  stopifnot(isTRUE(all.equal(threshold_h, direct_hours$hours)))
  # Independent clock-window check of the requested permissive NA policy.
  window_support <- lapply(split(day_data, day_data$Date), function(day) {
    rolling_means <- vapply(seq_len(nrow(day) - 599L), function(i)
      mean(day$MEDI[i:(i + 599L)], na.rm = TRUE), numeric(1))
    first <- which.max(rolling_means)
    data.frame(Date = unique(day$Date), mean = rolling_means[first],
      onset = day$Datetime[first], offset = day$Datetime[first + 599L],
      expected_minutes = 600L,
      observed_minutes = sum(is.finite(day$MEDI[first:(first + 599L)])))
  }) |> bind_rows()
  stopifnot(isTRUE(all.equal(daily$brightest_10h_mean, window_support$mean)),
            identical(daily$brightest_10h_onset, window_support$onset),
            identical(daily$brightest_10h_offset, window_support$offset),
            identical(window_support$observed_minutes, c(540L, 600L)))
  write.csv(window_support, "outputs/masterclass/brightest-window-coverage.csv", row.names = FALSE)
  stopifnot(identical(package$commit, revision), nrow(glc_light) == 69120L)
  stopifnot(sum(!is.na(glc_light$MEDI)) == 58830L,
            identical(names(glc_light), c("Id", "Datetime", "MEDI")),
            identical(unique(glc_light$Id), "IZTECH_S011"))
  stopifnot(inherits(glc_light$Datetime, "POSIXct"), is.numeric(glc_light$MEDI))
  stopifnot(!anyDuplicated(glc_light$Datetime),
            all(as.numeric(diff(glc_light$Datetime), units = "secs") == 10),
            all(is.na(glc_light$MEDI) | (is.finite(glc_light$MEDI) & glc_light$MEDI >= 0)))
  stopifnot(nrow(diary) == 8L, nrow(cycles) == 14L, nrow(wear) == 9L,
            !anyNA(diary$sleepprep), !anyNA(diary$wake),
            all(diff(as.Date(diary$wake, tz = "Europe/Istanbul")) == 1),
            identical(diary$Date, as.Date(diary$wake, tz = "Europe/Istanbul")),
            identical(cycles$Date, rep(head(diary$Date, -1), each = 2)),
            identical(cycles$State, rep(c("wake", "sleep"), 7)),
            all(lubridate::int_length(cycles$Interval) > 0),
            all(head(lubridate::int_end(cycles$Interval), -1) ==
                  tail(lubridate::int_start(cycles$Interval), -1)),
            !any(wear$wear_state == "site_leave"),
            all(wear$start < wear$end), all(head(wear$end, -1) <= tail(wear$start, -1)))
  expected_rows <- sum(glc_light$Datetime >= min(lubridate::int_start(cycles$Interval)) &
                       glc_light$Datetime < max(lubridate::int_end(cycles$Interval)))
  stopifnot(nrow(cycle_light) == expected_rows, nrow(cycle_light) == 60660L,
            !anyDuplicated(cycle_light$Datetime),
            all(as.numeric(diff(cycle_light$Datetime), units = "secs") == 10))
  off_body <- cycle_light$wear_state %in% c("off", "sleep")
  stopifnot(all(is.na(cycle_light$MEDI[off_body])),
            identical(cycle_light$MEDI[!off_body], cycle_light$MEDI_recorded[!off_body]))
  direct_cycle <- waking |>
    summarise(expected_h = n() * 10 / 3600,
              observed_h = sum(!is.na(MEDI)) * 10 / 3600,
              threshold_h = sum(MEDI >= cycle_threshold_lx, na.rm = TRUE) * 10 / 3600,
              dose_lx_h = sum(MEDI, na.rm = TRUE) * 10 / 3600, .groups = "drop")
  for (column in c("expected_h", "observed_h")) {
    stopifnot(inherits(cycle_metrics[[column]], "Duration"),
              isTRUE(all.equal(as.numeric(cycle_metrics[[column]]) / 3600,
                               direct_cycle[[column]])))
  }
  stopifnot(isTRUE(all.equal(cycle_metrics$dose_lx_h, direct_cycle$dose_lx_h)))
  threshold_h <- as.numeric(cycle_metrics[[paste0("duration_above_", cycle_threshold_lx)]]) / 3600
  waking_intervals <- cycles |> filter(State == "wake")
  stopifnot(isTRUE(all.equal(as.numeric(cycle_metrics$expected_h) / 3600,
                            lubridate::int_length(waking_intervals$Interval) / 3600)),
            isTRUE(all.equal(threshold_h, direct_cycle$threshold_h)),
            identical(cycle_metrics$Date, head(diary$Date, -1)),
            all(threshold_h <= as.numeric(cycle_metrics$observed_h) / 3600),
            is.numeric(cycle_metrics$coverage_pct),
            isTRUE(all.equal(cycle_metrics$coverage_pct,
                            100 * direct_cycle$observed_h / direct_cycle$expected_h)),
            all(cycle_metrics$coverage_pct >= 0 & cycle_metrics$coverage_pct <= 100))
  # State changes and date changes use different boundaries; the final end is excluded.
  boundary_probe <- tibble::tibble(Id = rep(diary$Id[1], 4),
    Datetime = c(lubridate::int_start(cycles$Interval)[1:3],
                 tail(lubridate::int_end(cycles$Interval), 1))) |>
    group_by(Id) |> add_states(cycles, start.colname = Interval,
                              end.colname = Interval, bounds = "[)")
  stopifnot(nrow(boundary_probe) == 4L,
            identical(boundary_probe$Date, c(cycles$Date[1:3], as.Date(NA))),
            identical(boundary_probe$State, c("wake", "sleep", "wake", NA_character_)))
  # An unmatched participant must not acquire this participant's diary states.
  other_probe <- boundary_probe |> select(Id, Datetime) |>
    mutate(Id = "Different participant") |> group_by(Id) |>
    add_states(cycles, start.colname = Interval, end.colname = Interval, bounds = "[)")
  stopifnot(all(is.na(other_probe$Date)))
  write.csv(daily, "assets/masterclass/example-daily.csv", row.names = FALSE)
  write.csv(cycle_metrics,
            "assets/masterclass/example-sleep-metrics.csv", row.names = FALSE)
})

# Exercise edited daily cells with a restored hour, changed parameters, and no valid light.
daily_metric_code <- cells[[which(vapply(cells, `[[`, "", "label") == "mc-metrics")]]$code
test_environment <- list2env(as.list(environment), parent = globalenv())
for (label in c("mc-window", "mc-clean", "mc-metrics")) {
  code <- cells[[which(vapply(cells, `[[`, "", "label") == label)]]$code
  code <- sub("simulate_gap <- TRUE", "simulate_gap <- FALSE", code, fixed = TRUE)
  # Evaluate the exact expressions without auto-printing plots or gt tables.
  for (expression in parse(text = code)) invisible(eval(expression, envir = test_environment))
}
stopifnot(all(as.numeric(test_environment$daily$observed_h) / 3600 == 24),
          all(test_environment$daily$coverage_pct == 100),
          !anyNA(test_environment$clean$MEDI))
test_environment <- list2env(as.list(environment), parent = globalenv())
for (expression in parse(text = sub("threshold_lx <- 250", "threshold_lx <- 100",
                                    daily_metric_code, fixed = TRUE))) {
  invisible(eval(expression, envir = test_environment))
}
stopifnot(all(as.numeric(test_environment$daily$duration_above_100) >=
              as.numeric(environment$daily$duration_above_250)),
          identical(test_environment$daily$coverage_pct, environment$daily$coverage_pct),
          identical(test_environment$daily$brightest_10h_mean, environment$daily$brightest_10h_mean))
test_environment <- list2env(as.list(environment), parent = globalenv())
test_environment$clean$MEDI[as.Date(test_environment$clean$Datetime, tz = environment$tz) == min(environment$daily$Date)] <- NA_real_
no_light_error <- tryCatch({
  for (expression in parse(text = daily_metric_code)) invisible(eval(expression, envir = test_environment))
  FALSE
}, error = function(e) grepl("coverage\\$duration > 0", conditionMessage(e)))
stopifnot(no_light_error)

# Exercise the actual page cell with an all-missing cycle and a changed threshold.
sleep_metric_code <- cells[[which(vapply(cells, `[[`, "", "label") == "mc-sleep-metrics")]]$code
test_environment <- list2env(as.list(environment), parent = globalenv())
test_environment$cycle_light$MEDI[test_environment$cycle_light$Date == min(test_environment$cycle_light$Date)] <- NA_real_
no_light_error <- tryCatch({
  for (expression in parse(text = sleep_metric_code)) invisible(eval(expression, envir = test_environment))
  FALSE
}, error = function(e) grepl("wake_coverage\\$duration > 0", conditionMessage(e)))
stopifnot(no_light_error)
test_environment <- list2env(as.list(environment), parent = globalenv())
invisible(capture.output(eval(parse(text = sub("cycle_threshold_lx <- 250", "cycle_threshold_lx <- 100",
                                               sleep_metric_code, fixed = TRUE)), envir = test_environment)))
stopifnot(all(as.numeric(test_environment$cycle_metrics$duration_above_100) >=
              as.numeric(environment$cycle_metrics$duration_above_250)),
          identical(test_environment$cycle_metrics$dose_lx_h, environment$cycle_metrics$dose_lx_h),
          identical(test_environment$cycle_metrics$observed_h, environment$cycle_metrics$observed_h),
          identical(test_environment$cycle_metrics$mean_MEDI, environment$cycle_metrics$mean_MEDI),
          identical(test_environment$cycle_metrics$median_MEDI, environment$cycle_metrics$median_MEDI))

writeLines(transcripts[["mc-glc-open"]], "assets/masterclass/example-glc-open.txt")
writeLines(transcripts[["mc-glc-read"]], "assets/masterclass/example-glc-read.txt")
log <- c(
  "Command: Rscript --vanilla scripts/check_sleep_europe_masterclass.R",
  paste("Checked:", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  paste("Source:", source_file),
  paste("Input:", environment$filename),
  paste("Input MD5:", tools::md5sum(environment$filename)),
  paste("GLC repository:", environment$repository),
  paste("GLC commit:", environment$revision),
  paste("GLC archive:", environment$archive),
  paste("GLC archive MD5:", tools::md5sum(environment$archive)),
  paste("GLC file:", "data/files/sensor/IZTECH_S011_light_chest.csv"),
  paste("GLC sleep diary:", "data/files/longitudinal-reports/IZTECH_S011_sleepdiaries.csv"),
  paste("GLC wear log:", "data/files/longitudinal-reports/IZTECH_S011_wearlog.csv"),
  "", unlist(Map(function(name, value) c(name, value, ""), names(transcripts), transcripts)),
  "All analytical invariants, boundary joins, all-missing guards, and threshold-edit checks passed.", "", capture.output(sessionInfo())
)
writeLines(log, "outputs/masterclass/verification.txt")
message("All fourteen live cells and analytical invariants passed.")
