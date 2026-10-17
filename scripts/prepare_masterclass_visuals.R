# Concept figures use illustrative data, not participant measurements.
# Run: MASTERCLASS_R_LIBRARY=/path/to/library Rscript --vanilla scripts/prepare_masterclass_visuals.R
validation_library <- Sys.getenv("MASTERCLASS_R_LIBRARY")
if (nzchar(validation_library)) .libPaths(c(validation_library, .libPaths()))
library(LightLogR)
library(dplyr)
library(ggplot2)

toy <- tibble::tibble(Id = "Illustration",
  Datetime = as.POSIXct("2026-01-01 12:00:00", tz = "UTC") + (0:7) * 60,
  MEDI = c(50, 300, 400, NA, NA, 0, 100, 500)) |>
  group_by(Id)
sparse <- toy |> filter(!is.na(MEDI))
restored <- gap_handler(sparse, epoch = "1 min")
stopifnot(identical(restored$MEDI, toy$MEDI))
clock <- format(toy$Datetime, "%H:%M", tz = "UTC")
grid <- bind_rows(
  restored |> mutate(stage = "Missing rows", label = if_else(is.na(MEDI), "no row", as.character(MEDI))),
  restored |> mutate(stage = "After gap_handler()", label = if_else(is.na(MEDI), "NA", as.character(MEDI)))
) |> mutate(minute = match(Datetime, toy$Datetime),
            stage = factor(stage, levels = c("After gap_handler()", "Missing rows")),
            kind = case_when(label == "no row" ~ "No row",
                             is.na(MEDI) ~ "Unknown", TRUE ~ "Recorded"))
base_theme <- theme_minimal(base_size = 15) +
  theme(plot.background = element_rect(fill = "#faf9f5", colour = NA),
        panel.grid = element_blank(), plot.title = element_text(face = "bold", colour = "#162d38"),
        plot.margin = margin(12, 18, 12, 12), legend.position = "none")
gap_plot <- ggplot(grid, aes(minute, stage)) +
  geom_tile(aes(fill = kind, colour = kind, linetype = kind),
            width = .9, height = .65, linewidth = .65) +
  geom_text(aes(label = label), size = 4.6, colour = "#162d38") +
  scale_fill_manual(values = c(`No row` = "#f9e2df", Unknown = "#f1eadc", Recorded = "#e5f1eb")) +
  scale_colour_manual(values = c(`No row` = "#be6159", Unknown = "#bbc8c2", Recorded = "#bbc8c2")) +
  scale_linetype_manual(values = c(`No row` = "dashed", Unknown = "solid", Recorded = "solid")) +
  scale_x_continuous(breaks = 1:8, labels = clock, position = "top", expand = expansion(add = .5)) +
  labs(x = NULL, y = NULL, title = "Restore the clock, not the light",
       subtitle = "Values are melanopic EDI (lx). A recorded zero remains a recorded zero.") +
  base_theme + theme(axis.text.y = element_text(face = "bold", colour = "#162d38"))
ggsave("assets/masterclass/concept-gaps.png", gap_plot, width = 10, height = 2.65, dpi = 180)

coverage <- durations(toy, MEDI, show.missing = TRUE)
observed_min <- as.numeric(coverage$duration) / 60
expected_min <- as.numeric(coverage$total) / 60
above_min <- as.numeric(duration_above_threshold(toy$MEDI, toy$Datetime,
  threshold = 250, epoch = "1 min", na.rm = TRUE)) / 60
toy_dose <- dose(toy$MEDI, toy$Datetime, epoch = "1 min", na.rm = TRUE)
stopifnot(observed_min == 6, expected_min == 8, above_min == 3, toy_dose == 22.5)
bars <- toy |> mutate(minute = row_number(), high = MEDI >= 250)
metric_plot <- ggplot(bars, aes(minute, MEDI)) +
  geom_rect(data = bars |> filter(is.na(MEDI)),
    aes(xmin = minute - .45, xmax = minute + .45), ymin = 0, ymax = 550,
    inherit.aes = FALSE, fill = "#f1eadc", colour = "#b89458", linetype = "dashed") +
  geom_col(aes(fill = high), width = .9, na.rm = TRUE) +
  geom_text(data = bars |> filter(is.na(MEDI)), aes(y = 290, label = "NA"), colour = "#755421") +
  geom_point(data = bars |> filter(MEDI == 0), shape = 21, size = 3, fill = "white") +
  geom_hline(yintercept = 250, linetype = "dashed", colour = "#a16d13") +
  annotate("label", x = .65, y = 250, label = "Threshold: 250 lx", hjust = 0,
           size = 4, colour = "#755421", fill = "#faf9f5", linewidth = 0.2) +
  scale_fill_manual(values = c(`TRUE` = "#126454", `FALSE` = "#b1c5cd")) +
  scale_x_continuous(breaks = 1:8, labels = clock, expand = expansion(add = .5)) +
  scale_y_continuous(limits = c(0, 570), breaks = c(0,250,500), expand = expansion(mult = c(.02,0))) +
  labs(x = "Each column represents one minute", y = "Melanopic EDI (lx)",
    title = "Same recording, different questions",
    subtitle = sprintf("%g min at or above threshold  |  %g min observed / %g min expected  |  Dose: %g lx·h",
                       above_min, observed_min, expected_min, toy_dose),
    caption = "Duration counts qualifying time. Dose also uses the height of each observed column. NA supplies neither.") +
  base_theme + theme(panel.grid.major.y = element_line(colour = "#e2e5df"), plot.caption = element_text(hjust = 0))
ggsave("assets/masterclass/concept-metrics.png", metric_plot, width = 10, height = 3.5, dpi = 180)

# Compare clock time with elapsed time since awakening on the same multi-day axis.
# All times below are illustrative, not participant measurements.
illustrated_wake <- c(7, 33, 56, 79)
illustrated_prep <- c(25, 49, 73)
illustrated_cycles <- tibble(start = head(illustrated_wake, -1),
  end = tail(illustrated_wake, -1), prep = illustrated_prep,
  Date = c("14 Apr", "15 Apr", "16 Apr")) |>
  mutate(label = paste0("Date = ", Date, "  ·  ", end - start, " h"))
illustrated_days <- tibble(start = c(0, 24, 48, 72), end = c(24, 48, 72, 96),
  label = c("14 April", "15 April", "16 April", "17 April"))
elapsed_ticks <- bind_rows(lapply(seq_len(nrow(illustrated_cycles)), function(i) {
  tibble(x = illustrated_cycles$start[i] + c(0, 6, 12, 18),
         label = c("0 h", "+6", "+12", "+18"))
}))
cycle_plot <- ggplot() +
  geom_rect(data = illustrated_days,
    aes(xmin = start, xmax = end, ymin = 3.7, ymax = 4.2),
    fill = "#eef1ee", colour = "#aab9b4", linewidth = .45) +
  geom_text(data = illustrated_days, aes(x = (start + end) / 2, y = 3.95, label = label),
    colour = "#162d38", fontface = "bold", size = 4.2) +
  annotate("segment", x = c(24, 48, 72), xend = c(24, 48, 72), y = 3.25, yend = 4.2,
    linetype = "dashed", colour = "#8a999e", linewidth = .4) +
  annotate("segment", x = c(24, 48, 72), xend = c(24, 48, 72), y = 1.06, yend = 2.57,
    linetype = "dashed", colour = "#8a999e", linewidth = .4) +
  annotate("text", x = 0, y = 4.65, hjust = 0, label = "LOCAL TIME  ·  calendar days start at midnight",
    colour = "#51616a", fontface = "bold", size = 4) +
  annotate("text", x = seq(0, 90, 6), y = 3.4,
    label = rep(c("00:00", "06:00", "12:00", "18:00"), 4), size = 3.4, colour = "#51616a") +
  annotate("text", x = 0, y = 2.92, hjust = 0,
    label = "SLEEP–WAKE TIME  ·  hours since awakening",
    colour = "#126454", fontface = "bold", size = 4) +
  geom_rect(data = illustrated_cycles,
    aes(xmin = start, xmax = prep, ymin = 1.95, ymax = 2.57), fill = "#cfe6dc") +
  geom_rect(data = illustrated_cycles,
    aes(xmin = prep, xmax = end, ymin = 1.95, ymax = 2.57), fill = "#162d38") +
  geom_text(data = illustrated_cycles, aes(x = (start + prep) / 2, y = 2.26),
    label = "wake", colour = "#126454", fontface = "bold", size = 4) +
  geom_text(data = illustrated_cycles, aes(x = (prep + end) / 2, y = 2.26),
    label = "sleep\nopportunity", colour = "white", size = 2.9, lineheight = .95) +
  geom_segment(data = elapsed_ticks, aes(x = x, xend = x, y = 1.95, yend = 1.83),
    colour = "#51616a", linewidth = .35) +
  geom_text(data = elapsed_ticks, aes(x = x, y = 1.62, label = label), size = 3.4, colour = "#126454") +
  geom_segment(data = illustrated_cycles,
    aes(x = start, xend = end, y = 1.06, yend = 1.06), colour = "#126454", linewidth = .55) +
  annotate("segment", x = illustrated_wake, xend = illustrated_wake,
    y = 1.06, yend = 1.28, colour = "#126454", linewidth = .55) +
  geom_text(data = illustrated_cycles, aes(x = (start + end) / 2, y = .73, label = label),
    colour = "#126454", fontface = "bold", size = 3.7) +
  annotate("text", x = 0, y = .06, hjust = 0, size = 3.6, colour = "#51616a",
    label = "Keep the awakening date across midnight. The cycle length can differ from 24 hours.") +
  coord_cartesian(xlim = c(-2, 98), ylim = c(-.18, 4.9), expand = FALSE, clip = "off") +
  labs(caption = "Illustrative times, not participant measurements. Sleep opportunity = sleepprep to next wake.") +
  theme_void(base_size = 14) +
  theme(plot.background = element_rect(fill = "#faf9f5", colour = NA),
    plot.margin = margin(8, 10, 12, 10),
    plot.caption = element_text(hjust = 0, colour = "#51616a", size = 10, margin = margin(t = 12)))
ggsave("assets/masterclass/concept-sleep-cycles.png", cycle_plot, width = 12, height = 4.9, dpi = 180)
writeLines(c("Illustrative concept figures; input values defined in scripts/prepare_masterclass_visuals.R",
             paste("Observed minutes:", observed_min), paste("Expected minutes:", expected_min),
             paste("Minutes at or above 250 lx:", above_min), paste("Dose lx h:", toy_dose),
             capture.output(print(illustrated_cycles)),
             capture.output(sessionInfo())), "outputs/masterclass/concept-figures.txt")
