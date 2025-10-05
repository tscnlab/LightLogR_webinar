grand_overview <- function(dataset, #light exposure dataset
                           coordinates, #latitude, longitude
                           location, #city, location, etc. (string)
                           site, #country code
                           site_color, #colors of sites
                           panels = c("all", "map", "overview", "photoperiod", "doubleplot"), #which panel to return
                           variable = MEDI, #column in dataset containing the plot value
                           Datetime.colname = Datetime, #column in dataset containing the datetime value
                           Id.colname = Id,  #column in dataset containing the Id
                           ov_y.text.size = 8,
                           photoperiod_sequence = 0.5,
                           map_projection = "+proj=eqc",
                           map_site_label_color = "black",
                           map_y_offset_label = 0.08,
                           dp_y.axis.label = "Melanopic EDI (lx)"
                           ) {

  panels <- match.arg(panels)
  
  lat_string <- 
    paste0(abs(round(coordinates[1], 1)), "°",
           ifelse(sign(coordinates[1]) == 1, "N", "S"))
  
  lon_string <- 
    paste0(abs(round(coordinates[2], 1)), "°",
           ifelse(sign(coordinates[1]) == 1, "E", "W"))
  
  coordinates_string <- 
    paste(lat_string, lon_string, sep = ", ")
  
  if (is.null(names(site_color)) || all(names(site_color) == "")) names(site_color) <- site
  
  # overview ----------------------------------------------------------------
  
  if (panels %in% c("all", "overview")) {
    lvl <- dataset |>
      ungroup() |>
      summarise(avg = min({{ Datetime.colname }}, na.rm = TRUE), .by = {{ Id.colname }}) |>
      arrange(desc(avg)) |>
      pull({{ Id.colname}})
    
    P_overview <-
      dataset |>
      mutate({{ Id.colname }} :=
               {{ Id.colname }} |>
               factor(levels = lvl)) |>
      gg_overview(col = site, Id.colname = {{ Id.colname }}) +
      labs(x = "Month") +
      scale_x_datetime(date_labels = "%b %Y") +
      scale_color_manual(values = site_color) +
      guides(colour = "none") +
      labs(y = "ID") +
      theme(axis.text.y = element_text(size = ov_y.text.size))
    
    if (panels == "overview")
      return(P_overview)
  }
  
  # map ----------------------------------------------------------------
  
  if (panels %in% c("all", "map")) {
    
    # 1) Build the coordinate strings
    lat_string <-
      paste0(abs(round(coordinates[1], 1)), "°",
             ifelse(sign(coordinates[1]) >= 0, "N", "S"))
    
    lon_string <-
      paste0(abs(round(coordinates[2], 1)), "°",
             ifelse(sign(coordinates[2]) >= 0, "E", "W"))
    
    coordinates_string <- paste(lat_string, lon_string, sep = ", ")
    
    # 2) Load world in geographic CRS and prepare colors
    world <- ne_countries(scale = "medium", returnclass = "sf")
    
    countries_colors <- tibble(
      country = site,
      color   = site_color[country],
      stringsAsFactors = FALSE
    )
    
    world$color <- ifelse(
      world$name %in% countries_colors$country,
      countries_colors$country[match(world$name, countries_colors$country)],
      NA
    )
    
    # 3) Locations in geographic CRS (EPSG:4326)
    location_info <- tibble(
      country  = site,
      location = location,
      lat      = coordinates[1],
      lon      = coordinates[2],
      color    = site,
      stringsAsFactors = FALSE
    ) |>
      mutate(label = paste0(location, ", ", country, " (", coordinates_string, ")"))
    
    locations <- st_as_sf(location_info, coords = c("lon", "lat"), crs = 4326)
    
    # 4) Project both layers to a planar CRS
    # robinson_crs <- paste0("+proj=", map_projection)
    robinson_crs <- map_projection
    world_proj     <- st_transform(world,    crs = robinson_crs)
    locations_proj <- st_transform(locations, crs = robinson_crs)
    
    bb <- st_bbox(world_proj)
    y_offset <- map_y_offset_label * (bb["ymax"] - bb["ymin"]) 
    
    # 5) Plot
    P_map <-
      ggplot() +
      geom_sf(data = world_proj, aes(fill = color),
              color = NA, size = 0.25, alpha = 0.5, show.legend = FALSE) +
      geom_sf(data = locations_proj, aes(fill = color),
              shape = 21, color = "black", size = 3, stroke = 0.2) +
      geom_sf_label(
        data = locations_proj,
        aes(label = label, fill = color),
        nudge_y = y_offset, 
        size = 3, 
        alpha = 0.75
      ) +
      geom_sf_text(
        data = locations_proj,
        aes(label = label),
        nudge_y = y_offset,
        size = 3,
        color = map_site_label_color
      ) +
      scale_fill_manual(values = site_color) +
      theme_void() +
      theme(legend.position = "none") +
      labs(x = NULL, y = NULL) +
      coord_sf(expand = FALSE)
    
    if (panels == "map") return(P_map)
  }
  
  # photoperiod ---------------------------------------------------------
  
  if (panels %in% c("all", "photoperiod")) {
    
    tz <- dataset |> pull({{Datetime.colname}}) |> tz()
    
    limits <- 
      photoperiod(coordinates,  
                  seq(from = as_date("2025-01-01"),
                      to = as_date("2025-12-31"), 
                      by = 1),
                  tz = tz
      ) |> pull(photoperiod)
    
    photoperiods <- 
      photoperiod(coordinates,  
                  dataset |> 
                    add_Date_col(Date.colname = .Date, group.by = TRUE) |> 
                    summarize(.groups = "drop") |> 
                    pull(.Date),
                  tz = tz
      )
    
    axis_name <- 
      glue("Possible and <b style = 'color:{site_color[site]}'>actual
           </b> photoperiod (hours) at {coordinates_string}")
    
    photoperiods_range <- limits |> range() |> round(1)
    
    photoperiods_seq <- 
      inject(seq(!!!photoperiods_range, by = photoperiod_sequence) |> 
               round(1))
    
    P_photoperiod <-
      photoperiods |> 
      ggplot(aes(x=photoperiod)) +
      geom_boxplot(aes(y= -1, col = site), ) +
      geom_histogram(bins = 25, 
                     alpha = 0.25, 
                     fill = "grey40",
                     data = tibble(photoperiod = limits)) +
      geom_histogram(bins = 25, aes(fill = site), alpha = 0.85) +
      scale_x_continuous(breaks = photoperiods_seq) +
      scale_color_manual(values = site_color) +
      scale_fill_manual(values = site_color) +
      labs( 
        y = "Number of days", 
        x = axis_name) +
      theme_cowplot() +
      guides(
        fill = "none", color = "none") +
      coord_cartesian(xlim = photoperiods_range) +
      theme(axis.title.x = ggtext::element_markdown())
    
    if (panels == "photoperiod")
      return(P_photoperiod)
  }
  
  # doubleplot -----------------------------------------------------------
  
  if (panels %in% c("all", "doubleplot")) {
    
    Brown_bracket <- primitive_bracket(
      # Keys determine what is displayed
      key = key_range_manual(start = c(0, 1.0001,250), 
                             end = c(1, 10, Inf), 
                             name = c("sleep", "evening", "daytime")),
      bracket = "square",
      theme = theme(
        legend.text = element_text(angle = 90, hjust = 0.5),
        axis.text.y.left = element_text(angle = 90, hjust = 0.5)
      )
    )
    
    P_doubleplot <-
      dataset |> 
      ungroup() |> 
      select({{ Datetime.colname }}, {{ variable }}) |> 
      aggregate_Date(
        unit = "15 mins",
        numeric.handler = \(x) median(x, na.rm = TRUE),
        upper95 = quantile({{ variable }}, 0.975, na.rm = TRUE),
        upper75 = quantile({{ variable }}, 0.875, na.rm = TRUE),
        upper50 = quantile({{ variable }}, 0.75, na.rm = TRUE),
        lower50 = quantile({{ variable }}, 0.125, na.rm = TRUE),
        lower75 = quantile({{ variable }}, 0.25, na.rm = TRUE),
        lower95= quantile({{ variable }}, 0.025, na.rm = TRUE)
      ) |> 
      add_photoperiod(coordinates) |> 
      gg_doubleplot(geom = "blank", 
                    facetting = FALSE, 
                    jco_col = FALSE,
                    x.axis.label = "Local time (HH:MM)",
                    y.axis.label = dp_y.axis.label
                    ) |> 
      gg_photoperiod() +
      geom_ribbon(aes(ymin = lower95, ymax = upper95, fill = site), alpha = 0.4) +
      geom_ribbon(aes(ymin = lower75, ymax = upper75, fill = site), alpha = 0.4) +
      geom_ribbon(aes(ymin = lower50, ymax = upper50, fill = site), alpha = 0.4) +
      geom_line(aes(y = {{ variable }}), linewidth = 1) +
      map(c(1,10,250), 
          \(x) geom_hline(aes(yintercept = x), col = "grey", linetype = "dashed")
      ) +
      scale_fill_manual(values = site_color) +
      coord_cartesian(ylim = c(0, 100000)) +
      guides(fill = "none", y = guide_axis_stack(Brown_bracket, "axis")) +
      # labs(x = NULL)
      labs(
        caption = glue(
          "<i>daytime</i>, <i>evening</i>, and <i>sleep</i> indicate 
          recommendations for healthy light exposure (Brown et al., 2022). 
          <b>Median</b> with <b style = 'color:{alpha(site_color[site], 
          alpha = 0.9)}'>50%</b>, <b style = 'color:{alpha(site_color[site], 
          alpha = 0.7)}'>75%</b>, or <b style = 'color:{alpha(site_color[site], 
          alpha = 0.5)}'>95%</b> of data."
        )
      ) +
      theme(plot.caption = ggtext::element_markdown())
    
    if (panels == "doubleplot")
      return(P_doubleplot)
  }
  
  # composition ----------------------------------------------------------
  
  (P_map + P_overview) / 
  (P_photoperiod + P_doubleplot + plot_layout(widths = c(1, 2))) +
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = "collect", heights = c(1.4,2)) &
  theme(axis.title = element_text(size = 10),
  axis.text = element_text(size = 10),
  plot.tag = element_text(size = 20, face = "plain")) 
  
}
