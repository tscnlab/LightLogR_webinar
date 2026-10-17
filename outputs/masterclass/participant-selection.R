.libPaths(c("/private/tmp/sleep-europe-masterclass/library", .libPaths()))
library(dplyr)
root <- "/private/tmp/masterclass-sleep/participant-screen"
paths <- list.files(file.path(root, "data/files/longitudinal-reports"), pattern = "sleepdiaries.csv$", full.names = TRUE)
results <- lapply(paths, function(path) {
  diary <- read.csv(path) |> mutate(across(c(sleepprep, wake), ~as.POSIXct(.x, tz = "Europe/Istanbul"))) |> arrange(wake)
  wear <- read.csv(sub("sleepdiaries", "wearlog", path)) |> mutate(across(c(start, end), ~as.POSIXct(.x, tz = "Europe/Istanbul"))) |> arrange(start)
  start <- head(diary$wake, -1)
  end <- tail(diary$sleepprep, -1)
  expected <- as.numeric(difftime(end, start, units = "hours"))
  off <- wear |> filter(state %in% c("off", "sleep"))
  # Disjoint reported off-body intervals intersect each diary-defined waking interval.
  overlaps <- vapply(seq_along(start), function(i) sum(pmax(0,
    pmin(as.numeric(end[i]), as.numeric(off$end)) - pmax(as.numeric(start[i]), as.numeric(off$start)))) / 3600, numeric(1))
  tibble(Id = diary$Id[1], entries = nrow(diary), site_leave = sum(wear$state == "site_leave"),
    consecutive = !anyNA(diary$wake) && all(diff(as.Date(diary$wake, tz = "Europe/Istanbul")) == 1),
    ordered = !anyNA(c(diary$sleepprep, diary$wake)) && all(diary$sleepprep < diary$wake) && all(start < end),
    overlapping_wear = any(head(wear$end, -1) > tail(wear$start, -1)),
    min_wake_h = min(expected), max_wake_h = max(expected),
    wake_off_h = sum(overlaps), wake_off_pct = 100 * sum(overlaps) / sum(expected),
    max_daily_off_pct = max(100 * overlaps / expected),
    wake_off_intervals = sum(overlaps > 0), wear_entries = nrow(wear))
}) |> bind_rows()
print(results, n = Inf, width = Inf)
write.csv(results, file.path(root, "screen-summary.csv"), row.names = FALSE)
writeLines(c("Command: Rscript --vanilla /private/tmp/masterclass-sleep/rank-participants.R",
  "Source: tscnlab/melidos-iztech-glc-dataset@abc456bdb418e6250b9c973813a190dc062c6e76",
  "Off-body coverage sums are used only for participants with non-overlapping wear intervals.",
  capture.output(sessionInfo())), file.path(root, "screen-method.txt"))
