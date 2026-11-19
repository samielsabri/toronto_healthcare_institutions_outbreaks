# Decision aids for the seven open questions in reports/01_eda.md (section 6).
#
# Run this one section at a time in RStudio: put the cursor in a section and
# press Ctrl/Cmd + Alt + T ("Run current section"), or step through with
# Ctrl/Cmd + Enter. The outline pane (Ctrl/Cmd + Shift + O) lists the sections.
#
# Everything here is exploratory and reads only data/raw/. Nothing is written.

# ---- 0. Setup ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(lubridate)
  library(ggplot2)
})
source("R/read_raw.R")

d <- read_raw_files("data/raw") |>
  imap_dfr(~ standardize_raw_names(.x) |> mutate(file_year = as.integer(.y))) |>
  transmute(
    file_year,
    name = `Institution Name`, address = `Institution Address`,
    setting = `Outbreak Setting`, type = `Type of Outbreak`,
    agent1 = `Causative Agent-1`, agent2 = `Causative Agent-2`,
    began = as.Date(`Date Outbreak Began`),
    over  = as.Date(na_if(`Date Declared Over`, "")),
    duration = as.integer(over - began)
  )

# Respiratory season = ISO week 35 to ISO week 34.
# season_week runs 1..52 (or 53) starting at ISO week 35.
iso_weeks_in_year <- function(y) isoweek(as.Date(paste0(y, "-12-28")))
d <- d |>
  mutate(
    iso_year = isoyear(began), iso_week = isoweek(began),
    season_start = if_else(iso_week >= 35, iso_year, iso_year - 1L),
    season = paste0(season_start, "/", str_sub(season_start + 1, 3, 4)),
    season_week = if_else(iso_week >= 35, iso_week - 34L,
                          iso_week + iso_weeks_in_year(season_start) - 34L)
  )

# Crude address key, the same one used in the EDA (good enough to explore).
rough_addr <- function(x) {
  x <- toupper(x) |> str_replace("\\s*-.*$", "") |> str_replace_all("[.,]", " ")
  abbrev <- c("\\bSTREET\\b" = "ST", "\\bAVENUE\\b" = "AVE", "\\bROAD\\b" = "RD",
              "\\bDRIVE\\b" = "DR", "\\bEAST\\b" = "E", "\\bWEST\\b" = "W")
  str_squish(str_replace_all(x, abbrev))
}
d <- d |> mutate(addr_key = rough_addr(address))

# Crude agent grouping (Phase 2 will replace this with a reviewed lookup table).
agent_group <- function(x) {
  case_when(
    x == "" | x == "None" ~ NA_character_,
    str_detect(x, "COVID") ~ "COVID-19",
    str_detect(x, "^Influenza A") ~ "influenza A",
    str_detect(x, "^Influenza B") ~ "influenza B",
    str_detect(x, "Respiratory syncytial|^RSV") ~ "RSV",
    str_detect(x, "Rhinovirus|Enterovirus") ~ "rhino/entero",
    str_detect(x, "^Coronavirus") ~ "seasonal coronavirus",
    str_detect(x, "^Parainfluenza") ~ "parainfluenza",
    str_detect(x, "Metapneumovirus") ~ "hMPV",
    str_detect(x, "Norovirus") ~ "norovirus",
    x %in% c("Unable to identify", "Pending") ~ "unknown",
    TRUE ~ "other"
  )
}
# Split a field into individual agents, keeping "Enterovirus/ Rhinovirus" whole.
split_agents <- function(x) {
  x <- str_replace_all(x, "Enterovirus/ ?Rhinovirus", "ENTERO_RHINO")
  x <- str_replace_all(x, "Influenza A and B", "Influenza A, Influenza B")
  parts <- str_split(x, "\\s*(,|/|&)\\s*")
  map(parts, ~ str_replace_all(.x, "ENTERO_RHINO", "Enterovirus/Rhinovirus") |>
        str_squish() |> discard(~ .x %in% c("", "None")))
}
d <- d |>
  mutate(
    row = row_number(),
    agents = map2(split_agents(agent1), split_agents(agent2), c),
    groups_all = map(agents, ~ unique(agent_group(.x))),
    group_primary = map_chr(groups_all, 1),
    n_groups = lengths(groups_all),
    co_detection = n_groups > 1
  )

# =============================================================================
# ---- D1. Shelter and Transitional Care -------------------------------------
# Question: where do these two settings go?
# Proposal: Transitional Care -> hospital (subtype "transitional care");
#           Shelter -> flag and exclude from analyses.

# 1a. Every row, with its address.
d |> filter(setting %in% c("Shelter", "Transitional Care")) |>
  select(file_year, setting, name, address, type, agent1, began) |>
  arrange(setting, began) |> print(n = Inf)

# 1b. What settings do the SAME addresses carry in other outbreaks?
#     If a transitional care address is otherwise always "Hospital-Chronic
#     Care", folding it into hospital is consistent. If it is otherwise LTCH,
#     that argues for LTCH instead.
tc_addr <- d |> filter(setting %in% c("Shelter", "Transitional Care")) |> distinct(addr_key)
d |> semi_join(tc_addr, by = "addr_key") |>
  count(addr_key, setting) |>
  pivot_wider(names_from = setting, values_from = n, values_fill = 0) |>
  print(n = Inf)

# Things to weigh:
# - Size: 26 + 3 rows out of 7,085, so headline counts barely move either way.
# - Transitional care units hold hospital (ALC) patients managed under hospital
#   IPAC programmes, even when they sit inside an LTCH or retirement building.
# - Shelters are not healthcare institutions, and only appear 2016-2018, which
#   suggests the City dropped them from this dataset's scope after that.
# - Observed (Nov 2026 extract): the same Reactivation Care Centre units at
#   200 Church St are "Hospital-Chronic Care" in most years and "Transitional
#   Care" in 2023, i.e. the label looks like coding drift, not a new setting.

# =============================================================================
# ---- D2. Norovirus-like vs Norovirus ----------------------------------------
# Question: merge "Norovirus-like" (to 2022) with "Norovirus" (2023 on)?

# 2a. Enteric outbreaks by year and label. If "Norovirus-like" once included
#     clinically compatible but untested outbreaks, then after the switch some
#     of those would move to "Unable to identify", and its share would rise.
d |> filter(type == "Enteric") |>
  mutate(label = case_when(agent1 %in% c("Norovirus", "Norovirus-like") ~ agent1,
                           agent1 == "Unable to identify" ~ "Unable to identify",
                           TRUE ~ "other")) |>
  count(file_year, label) |>
  group_by(file_year) |> mutate(pct = round(100 * n / sum(n))) |> ungroup() |>
  select(-n) |>
  pivot_wider(names_from = label, values_from = pct, values_fill = 0) |>
  print(n = Inf)

# 2b. Do the durations look similar? (Clinical vs lab-confirmed outbreaks might
#     differ in length.)
d |> filter(agent1 %in% c("Norovirus", "Norovirus-like")) |>
  group_by(agent1) |>
  summarise(n = n(), median_days = median(duration, na.rm = TRUE),
            p25 = quantile(duration, .25, na.rm = TRUE),
            p75 = quantile(duration, .75, na.rm = TRUE))

# Things to weigh:
# - Merging gives one continuous norovirus series; not merging creates a
#   step change in 2023 that is purely a labelling artefact.
# - If 2a shows the unknown share jumping in 2023, the label change probably
#   also changed meaning. Merge anyway, but say so in Methods.
# - The Toronto Open Data page lists a contact for the dataset; asking them
#   what "Norovirus-like" meant is the most reliable way to settle this.

# =============================================================================
# ---- D3. Same unit, same onset, different agent ------------------------------
# Question: are these one outbreak (co-detection) or two?

pairs <- d |> group_by(name, began) |> filter(n() > 1) |> ungroup() |>
  arrange(name, began, type)

# 3a. Look at them. Do the two rows close on the same day? Identical closing
#     dates suggest one event recorded twice; different dates suggest two
#     outbreaks managed separately.
pairs |> select(file_year, name, type, agent1, began, over) |> print(n = Inf)

pairs |> group_by(name, began) |>
  summarise(types = paste(sort(unique(type)), collapse = "+"),
            same_over_date = n_distinct(over) == 1, .groups = "drop") |>
  count(types, same_over_date)

# 3b. Are they concentrated in particular years (a coding practice)?
pairs |> distinct(name, began, file_year) |> count(file_year)

# Things to weigh:
# - Respiratory + enteric pairs are clearly separate outbreaks; keep both.
# - Watch for false pairs: names truncated at 60 characters (to 2021) or ending
#   in " -" can make two different wards look like one unit (e.g. the
#   Scarborough Health Network COVID + COVID pairs in Jan 2022).
# - Compare with 4a: co-detections within one row fell from 9-22% of
#   respiratory outbreaks (2016-19) to 0-4% after 2020, while these pairs
#   cluster in 2022-2025. Recording practice seems to have moved from "one row,
#   two agents" to "two rows". Collapsing pairs makes the series more
#   consistent over time.
# - For respiratory pairs: TPH publishes them as two rows. Keeping two matches
#   the source. Collapsing them changes counts by about 19 outbreaks over
#   11 years, which is negligible for trends, so this is mostly about
#   consistency and how you describe it.

# =============================================================================
# ---- D4. Counting by agent when there are co-detections ---------------------
# Question: for agent-specific series, count an outbreak under (a) its first
# listed agent only, or (b) every agent detected?

# 4a. How common are co-detections, by year?
d |> filter(type == "Respiratory") |>
  group_by(file_year) |>
  summarise(n = n(), co_detections = sum(co_detection),
            pct = round(100 * mean(co_detection), 1))

# 4b. How much do flu / RSV / COVID season totals change between (a) and (b)?
by_primary <- d |> filter(type == "Respiratory") |>
  count(season, group = group_primary, name = "a_primary_only")
by_any <- d |> filter(type == "Respiratory") |>
  select(season, groups_all) |> unnest(groups_all) |>
  count(season, group = groups_all, name = "b_any_mention")
full_join(by_primary, by_any, by = c("season", "group")) |>
  filter(group %in% c("influenza A", "influenza B", "RSV", "COVID-19")) |>
  mutate(across(c(a_primary_only, b_any_mention), ~ coalesce(.x, 0L)),
         extra_under_b = b_any_mention - a_primary_only) |>
  arrange(group, season) |> print(n = Inf)

# 4c. Is COVID always listed first? Which agents does it get listed with?
d |> filter(co_detection, map_lgl(groups_all, ~ "COVID-19" %in% .x)) |>
  count(group_primary, other = map_chr(groups_all, ~ paste(setdiff(.x, "COVID-19"), collapse = "+")))

# Things to weigh:
# - Onset alerts ask "is there flu activity in institutions?", and every
#   outbreak where flu was found is evidence of that, so (b) fits alerts.
# - A stacked agent-mix chart needs each outbreak counted once to sum to the
#   total, so (a) fits that chart, with co-detections shown as their own band
#   if you prefer.
# - If the extra counts under (b) in 4b are tiny, the choice hardly matters.
#   Observed: it matters before 2020 (RSV 2016/17: 41 vs 66; flu B 2017/18:
#   95 vs 112) and barely after, because co-detections were recorded
#   differently (see D3).

# =============================================================================
# ---- D5. Facility definition ------------------------------------------------
# Question: is a "facility" a physical site (address) or an operator?

# 5a. Addresses whose outbreaks carry more than one setting.
d |> group_by(addr_key) |>
  filter(n_distinct(setting) > 1) |>
  count(addr_key, setting) |>
  pivot_wider(names_from = setting, values_from = n, values_fill = 0) |>
  print(n = 30)

# 5b. Shared sites: how many different name stems at one address?
d |> mutate(stem = str_replace(name, " - [^-]*$", "")) |>
  group_by(addr_key) |>
  summarise(outbreaks = n(), n_name_stems = n_distinct(stem),
            example_stems = paste(head(unique(stem), 3), collapse = " | ")) |>
  arrange(desc(n_name_stems)) |> print(n = 15)

# 5c. Look at one site in detail (change the address to explore others).
d |> filter(addr_key == "200 CHURCH ST") |>
  select(file_year, name, setting, began) |> arrange(began) |> print(n = 40)

# Things to weigh:
# - Recurrence asks "does the same place keep having outbreaks?". That is
#   about the building, its residents and its staff, which points to the site.
# - Operators (e.g. Unity Health, Extendicare) relabel and merge over time,
#   so operator-level keys are less stable than addresses.
# - Where one address holds both an LTCH and a retirement home, they are
#   separate licensed homes with separate staff and residents, so splitting
#   by setting is defensible.

# =============================================================================
# ---- D6. Pre-pandemic baseline window ---------------------------------------
# Question: 2016/17-2018/19 only, plus 2019/20 to ISO week 10, or all four?

# 6a. Weekly non-COVID respiratory outbreaks, season by season.
#     Look at where 2019/20 departs from the others.
weekly <- d |>
  filter(type == "Respiratory", group_primary != "COVID-19",
         season %in% c("2016/17", "2017/18", "2018/19", "2019/20")) |>
  count(season, season_week) |>
  complete(season, season_week = 1:52, fill = list(n = 0))

ggplot(weekly, aes(season_week, n, colour = season)) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = 10 + 52 - 34, linetype = "dashed") +  # ISO wk 10 2020
  annotate("text", x = 10 + 52 - 34, y = Inf, vjust = 1.5, hjust = -0.05,
           label = "ISO week 10, 2020", size = 3) +
  scale_x_continuous(breaks = c(1, 14, 27, 40, 52),
                     labels = c("wk 35", "wk 48", "wk 9", "wk 22", "wk 34")) +
  labs(x = "Season week", y = "New non-COVID respiratory outbreaks",
       title = "Pre-pandemic seasons, by week of onset") +
  theme_minimal()

# 6b. Same for COVID-19 outbreaks in 2019/20: when do they start?
d |> filter(season == "2019/20", group_primary == "COVID-19") |>
  count(iso_year, iso_week) |> head(8)

# 6c. Season totals for the candidate baseline seasons.
d |> filter(type == "Respiratory", group_primary != "COVID-19",
            season %in% c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20")) |>
  count(season)

# Things to weigh:
# - A range built from 3 seasons is narrow, and any single unusual season
#   (e.g. the large 2017/18 flu season) shapes it heavily. Adding 2019/20 up to
#   week 10 covers the usual flu peak, which helps.
# - After week 10 of 2020, pandemic measures (visitor bans, masking, COVID
#   testing) changed both transmission and testing, so later weeks are not
#   comparable.
# - 2015/16 only has data from January 2016, so it lacks the season's start.
# - Observed: 2019/20 tracks the other seasons to week 10, spikes in weeks
#   11-12 (pandemic-onset testing), then drops to ~0. The first COVID-19
#   outbreak onsets are in ISO week 12 of 2020 (6b).
# - The same issue affects the post-2022 range: 2022/23-2025/26 gives 4 full
#   seasons. Both onset methods (MEM, Farrington) become more stable as more
#   seasons accumulate, which is worth stating as a limitation.

# =============================================================================
# ---- D7. ISO week 53 --------------------------------------------------------
# Question: merge week 53 into week 52 for season-on-season charts?

# 7a. Which seasons have a week 53, and how many outbreaks start in it?
d |> filter(iso_week == 53) |> count(season, type)

# Things to weigh:
# - Only two seasons (2015/16, 2020/21) have a week 53, and both are outside
#   the likely baseline windows. Merging into week 52 keeps every season on
#   the same 52-week axis.
# - The alternative (dropping week 53) silently loses outbreaks; plotting by
#   date instead of week makes seasons hard to overlay.
