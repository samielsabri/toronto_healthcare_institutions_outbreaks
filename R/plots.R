# Plotting for the dashboard. Colours come from one fixed, validated palette
# (colour-blind safe on adjacent pairs; checked with the dataviz validator) and
# follow the entity: COVID-19 is the same blue on every chart.

PAL <- list(
  surface = "#fcfcfb", ink = "#0b0b0b", ink2 = "#52514e", muted = "#898781",
  grid = "#e1e0d9", axis = "#c3c2b7",
  current = "#2a78d6",            # the current season
  band = "#898781",               # comparison range (neutral wash)
  series = c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300"),
  ordinal = c("#86b6ef", "#5598e7", "#2a78d6", "#1c5cab", "#104281")  # blue 250 -> 650
)

AGENT_COLOURS <- stats::setNames(c(PAL$series, PAL$muted), AGENT_MIX_LEVELS)
SETTING_COLOURS <- stats::setNames(PAL$series[1:3], c("LTCH", "Retirement home", "Hospital"))

theme_dash <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = PAL$surface, colour = NA),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = PAL$grid, linewidth = 0.3),
      axis.line.x = ggplot2::element_line(colour = PAL$axis, linewidth = 0.3),
      axis.text = ggplot2::element_text(colour = PAL$muted),
      axis.title = ggplot2::element_text(colour = PAL$ink2),
      strip.text = ggplot2::element_text(colour = PAL$ink, face = "bold", hjust = 0),
      legend.position = "top", legend.justification = "left",
      legend.text = ggplot2::element_text(colour = PAL$ink2),
      legend.title = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.caption = ggplot2::element_text(colour = PAL$muted, hjust = 0),
      plot.caption.position = "plot"
    )
}

#' ISO week label for a season week (1 = ISO week 35).
iso_week_of_season_week <- function(w) ((w + 34 - 1) %% 52) + 1
season_week_axis <- function() {
  brks <- seq(1, 51, by = 5)  # ISO weeks 35, 40, 45, 50, 3, 8, ...
  ggplot2::scale_x_continuous(breaks = brks, labels = paste0("W", iso_week_of_season_week(brks)),
                              limits = c(1, 52), expand = ggplot2::expansion(mult = 0.01))
}

girafe_dash <- function(p, width = 9, height = 6) {
  # The widget keeps the plot's aspect ratio, so give its container a height
  # matching the plot (about 100 px per inch); otherwise it renders tiny.
  w <- ggiraph::girafe(
    ggobj = p, width_svg = width, height_svg = height,
    options = list(
      ggiraph::opts_tooltip(css = paste0("background:#fff;color:#0b0b0b;padding:6px 8px;",
                                         "border:1px solid rgba(11,11,11,.12);border-radius:4px;",
                                         "font-family:system-ui,sans-serif;font-size:12px;")),
      ggiraph::opts_hover(css = "stroke-width:3px;"),
      ggiraph::opts_toolbar(saveaspng = FALSE),
      ggiraph::opts_sizing(rescale = TRUE)
    )
  )
  w$height <- paste0(round(height * 100), "px")
  w
}

#' Current season weekly counts against the pre- and post-pandemic ranges.
#' Two columns, one per comparison range; rows are settings.
plot_current_vs_baseline <- function(weekly, ranges, current, type, provisional_weeks = 2) {
  cur <- dplyr::filter(weekly, .data$season == current$season, .data$type == !!type)
  rng <- dplyr::filter(ranges, .data$type == !!type)
  lab <- c("pre-pandemic" = "vs pre-pandemic range (2016/17 to early 2019/20)",
           "post-pandemic" = "vs post-pandemic range (2022/23 to 2025/26)")
  rng$panel <- factor(lab[rng$baseline], levels = lab)
  cur <- tidyr::crossing(cur, panel = factor(lab, levels = lab))
  cur$tip <- sprintf("%s, week %d (ISO W%02d)\n%d new outbreak%s",
                     cur$setting, cur$week, iso_week_of_season_week(cur$week), cur$n,
                     ifelse(cur$n == 1, "", "s"))
  rng$tip <- sprintf("%s, ISO W%02d\nRange %g-%g (median %g) across %d seasons",
                     rng$setting, iso_week_of_season_week(rng$week), rng$lo, rng$hi, rng$median, rng$n_seasons)
  prov_start <- current$season_week - provisional_weeks + 0.5

  ggplot2::ggplot() +
    ggplot2::annotate("rect", xmin = prov_start, xmax = current$season_week + 0.5,
                      ymin = -Inf, ymax = Inf, fill = PAL$band, alpha = 0.10) +
    ggplot2::geom_ribbon(data = rng, ggplot2::aes(.data$week, ymin = .data$lo, ymax = .data$hi),
                         fill = PAL$band, alpha = 0.22) +
    ggplot2::geom_line(data = rng, ggplot2::aes(.data$week, .data$median),
                       colour = PAL$muted, linewidth = 0.4) +
    ggiraph::geom_point_interactive(data = rng, ggplot2::aes(.data$week, .data$hi, tooltip = .data$tip),
                                    alpha = 0, size = 3) +
    ggplot2::geom_line(data = cur, ggplot2::aes(.data$week, .data$n), colour = PAL$current,
                       linewidth = 0.9, lineend = "round", linejoin = "round") +
    ggiraph::geom_point_interactive(data = cur, ggplot2::aes(.data$week, .data$n, tooltip = .data$tip),
                                    colour = PAL$surface, fill = PAL$current, shape = 21, size = 2.2, stroke = 0.6) +
    ggplot2::facet_grid(rows = ggplot2::vars(.data$setting), cols = ggplot2::vars(.data$panel),
                        scales = "free_y", switch = "y") +
    season_week_axis() +
    ggplot2::scale_y_continuous(breaks = scales::breaks_extended(n = 4), limits = c(0, NA)) +
    ggplot2::labs(x = "Week of onset (ISO week)", y = NULL,
                  caption = paste0("Blue: ", current$season, " so far. Grey band: lowest to highest weekly count ",
                                   "across the comparison seasons; grey line: median. ",
                                   "Shaded: last ", provisional_weeks, " weeks, provisional (late reports still arriving).")) +
    theme_dash() +
    ggplot2::theme(strip.placement = "outside", strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1))
}

#' Weekly respiratory outbreaks this season, stacked by agent.
plot_agent_mix_weekly <- function(mix) {
  mix$tip <- sprintf("ISO W%02d\n%s: %d", iso_week_of_season_week(mix$week), mix$agent, mix$n)
  ggplot2::ggplot(mix, ggplot2::aes(.data$week, .data$n, fill = .data$agent)) +
    ggiraph::geom_col_interactive(ggplot2::aes(tooltip = .data$tip, data_id = .data$agent, group = .data$agent),
                                  width = 0.7, colour = PAL$surface, linewidth = 0.5,
                                  position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::scale_fill_manual(values = AGENT_COLOURS, drop = FALSE) +
    ggplot2::scale_x_continuous(breaks = unique(mix$week),
                                labels = paste0("W", iso_week_of_season_week(unique(mix$week)))) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(x = "Week of onset (ISO week)", y = "New respiratory outbreaks") +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1)) +
    theme_dash()
}

#' Share of respiratory outbreaks by agent, each season.
plot_agent_mix_seasons <- function(by_season) {
  by_season$tip <- sprintf("%s\n%s: %d of %d (%.0f%%)", by_season$season, by_season$agent,
                           by_season$n, by_season$total, 100 * by_season$share)
  by_season$season <- factor(by_season$season, levels = rev(sort(unique(by_season$season))))
  ggplot2::ggplot(by_season, ggplot2::aes(.data$share, .data$season, fill = .data$agent)) +
    ggiraph::geom_col_interactive(ggplot2::aes(tooltip = .data$tip, data_id = .data$agent, group = .data$agent),
                                  width = 0.7, colour = PAL$surface, linewidth = 0.5,
                                  position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::scale_fill_manual(values = AGENT_COLOURS, drop = FALSE) +
    ggplot2::scale_x_continuous(labels = scales::label_percent(), expand = ggplot2::expansion(mult = c(0, 0.03))) +
    ggplot2::labs(x = "Share of respiratory outbreaks", y = NULL) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1)) +
    theme_dash() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line(colour = PAL$grid, linewidth = 0.3))
}

#' Kaplan-Meier curves of time to outbreak declared over, by a grouping.
plot_km <- function(data, group, colours, max_days = 60) {
  fit <- survival::survfit(stats::as.formula(paste("survival::Surv(duration_days, event) ~", group)),
                           data = data)
  tidy <- ggsurvfit::tidy_survfit(fit)
  tidy$strata <- factor(sub("^[^=]+=", "", tidy$strata), levels = names(colours))
  tidy$tip <- sprintf("%s\nDay %d: %.0f%% still ongoing\n(%d at risk)", tidy$strata, as.integer(tidy$time),
                      100 * tidy$estimate, as.integer(tidy$n.risk))
  ggplot2::ggplot(tidy, ggplot2::aes(.data$time, .data$estimate, colour = .data$strata,
                                     group = .data$strata)) +
    ggplot2::geom_step(linewidth = 0.8) +
    ggiraph::geom_point_interactive(ggplot2::aes(tooltip = .data$tip, data_id = .data$strata),
                                    size = 2, alpha = 0) +
    ggplot2::scale_colour_manual(values = colours, drop = TRUE) +
    ggplot2::scale_x_continuous(limits = c(0, max_days), breaks = seq(0, max_days, 10)) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
    ggplot2::labs(x = "Days since onset", y = "Outbreaks still ongoing") +
    theme_dash()
}

#' Share of facilities with 2+ outbreaks in a season, by setting.
plot_repeat_share <- function(rs) {
  rs$tip <- sprintf("%s, %s\n%d of %d facilities with outbreaks had 2+ separate episodes (%.0f%%)",
                    rs$setting, rs$season, rs$repeat_facilities, rs$facilities, 100 * rs$share_repeat)
  ggplot2::ggplot(rs, ggplot2::aes(.data$season, .data$share_repeat, colour = .data$setting,
                                   group = .data$setting)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggiraph::geom_point_interactive(ggplot2::aes(tooltip = .data$tip, data_id = .data$setting, fill = .data$setting),
                                    shape = 21, colour = PAL$surface, size = 2.6, stroke = 0.6) +
    ggplot2::scale_colour_manual(values = SETTING_COLOURS) +
    ggplot2::scale_fill_manual(values = SETTING_COLOURS, guide = "none") +
    ggplot2::scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
    ggplot2::labs(x = "Season", y = "Facilities with 2+ episodes") +
    theme_dash() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

#' Distribution of outbreaks per facility in one season, by setting.
plot_recurrence_distribution <- function(dist) {
  dist$tip <- sprintf("%s: %d facilit%s with %s episode%s (%.0f%%)", dist$setting, dist$facilities,
                      ifelse(dist$facilities == 1, "y", "ies"), dist$bucket,
                      ifelse(dist$bucket == "1", "", "s"), 100 * dist$share)
  ggplot2::ggplot(dist, ggplot2::aes(.data$share, .data$setting, fill = .data$bucket)) +
    ggiraph::geom_col_interactive(ggplot2::aes(tooltip = .data$tip, group = .data$bucket), width = 0.6, colour = PAL$surface,
                                  linewidth = 0.5, position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::scale_fill_manual(values = stats::setNames(PAL$ordinal, levels(dist$bucket)),
                               name = "Episodes in the season") +
    ggplot2::scale_x_continuous(labels = scales::label_percent(), expand = ggplot2::expansion(mult = c(0, 0.03))) +
    ggplot2::labs(x = "Share of facilities that had at least one outbreak", y = NULL) +
    theme_dash() +
    ggplot2::theme(legend.title = ggplot2::element_text(colour = PAL$ink2),
                   panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line(colour = PAL$grid, linewidth = 0.3))
}

#' This season's weekly outbreaks for one agent against its MEM thresholds,
#' with the last complete season as a faint reference line.
plot_onset_mem <- function(weekly, th, current, colour, label) {
  cur <- dplyr::filter(weekly, .data$season_start_year == current$season_start_year)
  ref <- dplyr::filter(weekly, .data$season_start_year == current$season_start_year - 1L)
  cur$tip <- sprintf("%s, ISO W%02d\n%d outbreak%s with %s%s", current$season,
                     iso_week_of_season_week(cur$week), cur$n, ifelse(cur$n == 1, "", "s"), label,
                     ifelse(cur$week == current$season_week, "\n(partial week, not used for the alert)", ""))
  ref$tip <- sprintf("%s, ISO W%02d\n%d outbreaks", ref$season, iso_week_of_season_week(ref$week), ref$n)
  lines <- tibble::tibble(
    y = c(th$epidemic, th$medium, th$high, th$very_high),
    lab = sprintf("%s (%.1f)", c("Epidemic threshold", "Medium", "High", "Very high"),
                  c(th$epidemic, th$medium, th$high, th$very_high))
  )
  ggplot2::ggplot() +
    ggplot2::annotate("rect", xmin = current$season_week - 0.5, xmax = current$season_week + 0.5,
                      ymin = -Inf, ymax = Inf, fill = PAL$band, alpha = 0.10) +
    ggplot2::geom_hline(data = lines, ggplot2::aes(yintercept = .data$y),
                        colour = PAL$muted, linewidth = 0.3) +
    ggplot2::geom_text(data = lines, ggplot2::aes(x = 52, y = .data$y, label = .data$lab),
                       hjust = 1, vjust = -0.4, size = 3, colour = PAL$ink2) +
    ggplot2::geom_line(data = ref, ggplot2::aes(.data$week, .data$n), colour = PAL$band,
                       linewidth = 0.5, alpha = 0.7) +
    ggiraph::geom_point_interactive(data = ref, ggplot2::aes(.data$week, .data$n, tooltip = .data$tip),
                                    alpha = 0, size = 3) +
    ggplot2::geom_line(data = cur, ggplot2::aes(.data$week, .data$n), colour = colour, linewidth = 0.9) +
    ggiraph::geom_point_interactive(data = cur, ggplot2::aes(.data$week, .data$n, tooltip = .data$tip),
                                    shape = 21, fill = colour, colour = PAL$surface, size = 2.4, stroke = 0.6) +
    season_week_axis() +
    ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::labs(x = "Week of onset (ISO week)", y = paste("Outbreaks with", label),
                  caption = paste0("Coloured: ", current$season, ". Grey: ", season_label(current$season_start_year - 1L),
                                   " for reference. Thresholds from the moving epidemic method, fitted on ",
                                   "2022/23 to ", season_label(current$season_start_year - 1L),
                                   ". Shaded: current week, incomplete.")) +
    theme_dash()
}

#' This season's weekly COVID-19 outbreaks against the post-pandemic range.
plot_covid_vs_range <- function(weekly, current) {
  cur <- dplyr::filter(weekly, .data$season_start_year == current$season_start_year)
  rng <- weekly |>
    dplyr::filter(.data$period == "post-pandemic", .data$season_start_year < current$season_start_year) |>
    dplyr::group_by(.data$week) |>
    dplyr::summarise(lo = min(.data$n), median = stats::median(.data$n), hi = max(.data$n), .groups = "drop")
  cur$tip <- sprintf("%s, ISO W%02d\n%d COVID-19 outbreak%s", current$season,
                     iso_week_of_season_week(cur$week), cur$n, ifelse(cur$n == 1, "", "s"))
  rng$tip <- sprintf("ISO W%02d\nPost-pandemic range %g-%g (median %g)",
                     iso_week_of_season_week(rng$week), rng$lo, rng$hi, rng$median)
  ggplot2::ggplot() +
    ggplot2::annotate("rect", xmin = current$season_week - 0.5, xmax = current$season_week + 0.5,
                      ymin = -Inf, ymax = Inf, fill = PAL$band, alpha = 0.10) +
    ggplot2::geom_ribbon(data = rng, ggplot2::aes(.data$week, ymin = .data$lo, ymax = .data$hi),
                         fill = PAL$band, alpha = 0.22) +
    ggplot2::geom_line(data = rng, ggplot2::aes(.data$week, .data$median), colour = PAL$muted, linewidth = 0.4) +
    ggiraph::geom_point_interactive(data = rng, ggplot2::aes(.data$week, .data$hi, tooltip = .data$tip),
                                    alpha = 0, size = 3) +
    ggplot2::geom_line(data = cur, ggplot2::aes(.data$week, .data$n), colour = AGENT_COLOURS[["COVID-19"]],
                       linewidth = 0.9) +
    ggiraph::geom_point_interactive(data = cur, ggplot2::aes(.data$week, .data$n, tooltip = .data$tip),
                                    shape = 21, fill = AGENT_COLOURS[["COVID-19"]], colour = PAL$surface,
                                    size = 2.4, stroke = 0.6) +
    season_week_axis() +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::labs(x = "Week of onset (ISO week)", y = "Outbreaks with COVID-19",
                  caption = paste0("Blue: ", current$season, ". Grey band: lowest to highest weekly count in 2022/23 to ",
                                   season_label(current$season_start_year - 1L), "; grey line: median. ",
                                   "No formal alert: COVID-19 has no stable seasonal pattern yet.")) +
    theme_dash()
}
