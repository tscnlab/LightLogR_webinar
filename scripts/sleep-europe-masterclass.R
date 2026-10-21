# Sleep Europe 2026: Masterclass personal light exposure. Live code in reading order.
# Install LightLogR, dplyr, tidyr, ggplot2, glcdp and gt in a local R environment first.
# Download melidos-iztech-teaching.zip into the working directory as well.
# Verified versions and source details: outputs/masterclass/verification.txt.
# Changing an upstream cell requires rerunning the later dependent cells.

# mc-setup
library(LightLogR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(glcdp)
library(gt)

R.version.string
data.frame(
  package = c("LightLogR", "glcdp", "dplyr", "tidyr", "ggplot2", "gt"),
  version = sapply(c("LightLogR", "glcdp", "dplyr", "tidyr", "ggplot2", "gt"),
                   function(x) as.character(packageVersion(x)))
)

# mc-import
filename <- system.file(
  "extdata/205_actlumus_Log_1020_20230904101707532.txt.zip",
  package = "LightLogR"
)

tz <- "Europe/Berlin"

raw <- import$ActLumus(
  filename,
  tz = tz,
  manual.id = "P1",
  auto.plot = FALSE
) |> select(Id, Datetime, MEDI)

raw |> head(6)

# mc-inspect
raw |> count_difftime()
raw |> gap_finder() # try also: has_gaps(raw) or has_irregulars(raw)
raw |> gg_overview()
raw |> summary_overview()

# mc-window
simulate_gap <- TRUE

selected <- raw |> filter_Date(start = "2023-08-29", length = "2 days")

practice <- selected
if (simulate_gap) {
  practice <- selected |>
    filter_out(Datetime >= as.POSIXct("2023-08-29 12:00", tz = tz),
               Datetime <  as.POSIXct("2023-08-29 13:00", tz = tz))
}

gap_finder(practice)
count_difftime(practice)
gg_overview(practice)

# mc-clean
clean <- practice |>
  aggregate_Datetime(
    unit = "1 min", type = "floor",
    MEDI = if (n() == 6L) mean(MEDI) else NA_real_
  ) |>
  gap_handler(full.days = TRUE)

clean |> durations(MEDI, show.missing = TRUE)
gg_overview(clean) # no more implicitly missing data

# mc-plot
clean |>
  gg_days() |>
  gg_photoperiod(coordinates = c(48.52, 9.06), solarDep = 0) +
  labs(title = "Two days of recorded light exposure",
       subtitle = "One-minute means; missing minutes remain NA")

clean |> gg_gaps()

# mc-metrics
threshold_lx <- 250
brightest_duration <- "10 hours"

day_data <- clean |> add_Date_col(group.by = TRUE)
coverage <- day_data |>
  durations(MEDI, show.missing = TRUE)
stopifnot(all(coverage$duration > 0)) # each day needs observed light

daily <- day_data |>
  summarise(
    mean_MEDI = mean(MEDI, na.rm = TRUE),
    median_MEDI = median(MEDI, na.rm = TRUE),
    duration_above_threshold(MEDI, Datetime, threshold = threshold_lx,
                             na.rm = TRUE, as.df = TRUE),
    bright_dark_period(MEDI, Datetime, period = "brightest",
                       timespan = brightest_duration, na.rm = TRUE, as.df = TRUE),
    .groups = "drop"
  ) |>
  left_join(coverage, by = c("Id", "Date")) |>
  select(Id, Date, expected_h = total, observed_h = duration, mean_MEDI, median_MEDI, 5:9) |>
  mutate(coverage_pct = 100 * (observed_h / expected_h), .after = observed_h)

daily |> gt() |> fmt_number(5:7)

# mc-glc-open
repository <- "tscnlab/melidos-iztech-glc-dataset"
revision <- "abc456bdb418e6250b9c973813a190dc062c6e76"

archive <- "data/masterclass/melidos-iztech-teaching.zip"
if (!file.exists(archive)) archive <- "melidos-iztech-teaching.zip"
package_dir <- file.path(tempdir(), "masterclass-glc")
unzip(archive, exdir = package_dir)
package <- glc_open(package_dir)
stopifnot(package$commit == revision)
package
package$commit

glc_datasets(package) |>
  select(dataset_id, timezone, file_group_count) |>
  head(5)

# mc-glc-inspect
dataset_id <- "MELIDOS_IZTECH_S011"
stream_id <- "MELIDOS_IZTECH_S011:17"

glc_files(package, dataset_id = dataset_id, file_group = stream_id) |>
  select(file_group_id, device_location, timezone, data_state,
         temporal_value, temporal_unit) |>
  print(width = Inf)

glc_variables(package, file_group = stream_id, primary = TRUE) |>
  select(name, label, unit, term)

# mc-glc-read
light_collection <- glc_read(
  package, dataset_id = dataset_id, file_group = stream_id,
  variables = "MEDI"
)
glc_light <- glc_collect(light_collection) |>
  ungroup() |>
  transmute(Id = participant_Id, Datetime, MEDI) |>
  group_by(Id) |>
  arrange(Id, Datetime)

glc_light |> count_difftime()
glc_light |> durations(MEDI, show.missing = TRUE)

glc_light |> filter(!is.na(MEDI)) |> head(6)

# mc-sleep-diary
diary <- glc_read(
  package, dataset_id = dataset_id,
  file_group = "MELIDOS_IZTECH_S011:14",
  variables = c("sleepprep", "wake")
) |>
  glc_collect() |>
  ungroup() |>
  transmute(Id = participant_Id,
    sleepprep = as.POSIXct(sleepprep, tz = "Europe/Istanbul"),
    wake = as.POSIXct(wake, tz = "Europe/Istanbul"),
    Date = as.Date(wake, tz = "Europe/Istanbul")) |>
  arrange(Id, wake)

diary |> head(4)

# mc-sleep-cycles
cycles <- diary |>
  rename(sleep = sleepprep) |>
  pivot_longer(c(sleep, wake),
    names_to = "State", values_to = "Datetime") |>
  arrange(Id, Datetime) |>
  group_by(Id) |>
  mutate(Date = if_else(State == "wake", Date, as.Date(NA))) |>
  fill(Date) |>
  sc2interval(Datetime, State, full = FALSE) |>
  filter(!is.na(Date))

cycles |> head(4)

# mc-sleep-join
wear <- glc_read(package, dataset_id = dataset_id,
  file_group = "MELIDOS_IZTECH_S011:15",
  variables = c("start", "end", "state")) |>
  glc_collect() |>
  ungroup() |>
  transmute(Id = participant_Id,
    start = as.POSIXct(start, tz = "Europe/Istanbul"),
    end = as.POSIXct(end, tz = "Europe/Istanbul"), wear_state = state)

cycle_light <- glc_light |>
  group_by(Id) |>
  add_states(cycles,
    start.colname = Interval, end.colname = Interval, bounds = "[)") |>
  add_states(wear, bounds = "[)") |>
  filter(!is.na(Date)) |>
  mutate(MEDI_recorded = MEDI,
    MEDI = replace(MEDI, wear_state %in% c("off", "sleep"), NA_real_))

cycle_light |>
  gg_days() |>
  gg_states(State, aes_fill = State) +
  labs(fill = "Diary state", subtitle = "Reported off-body light values are masked")

# mc-sleep-compare
state_data <- cycle_light |> group_by(State)
state_data |> durations(MEDI_recorded) |> rename(recorded_h = duration) |>
  left_join(state_data |> durations(MEDI) |> rename(retained_h = duration), by = "State")

cycle_light |>
  gg_days(MEDI_recorded, col = "red", linewidth = 0.1) |>
  gg_states(wear_state, aes_fill = paste("Wear log:", wear_state),
            ymax = -0.1, alpha = 1) |>
  gg_states(State, aes_fill = State, ymin = 0) +
  geom_line(aes(y = MEDI)) +
  labs(fill = "State", subtitle = "Red: recorded light; black: after masking",
       caption = "Background: sleep diary. Bottom band: reported off-body periods.")

# mc-sleep-metrics
cycle_threshold_lx <- 250
waking <- cycle_light |>
  filter(State == "wake") |>
  group_by(Id, Date)

wake_coverage <- waking |>
  durations(MEDI, show.missing = TRUE)
stopifnot(all(wake_coverage$duration > 0))

cycle_metrics <- waking |>
  summarise(
    mean_MEDI = mean(MEDI, na.rm = TRUE),
    median_MEDI = median(MEDI, na.rm = TRUE),
    dose(MEDI, Datetime, epoch = "10 sec", na.rm = TRUE, as.df = TRUE),
    duration_above_threshold(MEDI, Datetime, threshold = cycle_threshold_lx,
      epoch = "10 sec", na.rm = TRUE, as.df = TRUE),
    .groups = "drop") |>
  left_join(wake_coverage, by = c("Id", "Date")) |>
  select(Id, Date, expected_h = total, observed_h = duration,
    mean_MEDI, median_MEDI, dose_lx_h = dose, starts_with("duration_above_")) |>
  mutate(coverage_pct = 100 * observed_h / expected_h, .after = observed_h)

cycle_metrics |> gt() |> fmt_number(5:8)

# Save software provenance with your analysis.
sessionInfo()
