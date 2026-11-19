# Causative-agent harmonization.
#
# Each raw label in "Causative Agent-1" and "Causative Agent-2" is mapped to
# one or more analysis groups by data/reference/agent_lookup.csv. The lookup
# works on whole raw labels (not on regex-split tokens), so a reviewer can see
# exactly how every label in the data is classified. Any label missing from the
# lookup stops the pipeline: new spellings must be triaged, not guessed.
#
# Counting rule (decision D4): an outbreak counts once under EVERY agent group
# detected (agent_<group> flags). For charts where each outbreak must appear
# once, agent_mix_group is the single group, or "multiple" for co-detections.

AGENT_GROUPS <- c(
  influenza_a   = "Influenza A",
  influenza_b   = "Influenza B",
  rsv           = "RSV",
  covid19       = "COVID-19",
  rhino_entero  = "Rhinovirus/enterovirus",
  seasonal_cov  = "Seasonal coronavirus",
  parainfluenza = "Parainfluenza",
  hmpv          = "hMPV",
  norovirus     = "Norovirus",
  other         = "Other",
  unknown       = "Unknown"
)

load_agent_lookup <- function(path = "data/reference/agent_lookup.csv") {
  lookup <- readr::read_csv(path, col_types = readr::cols(.default = "c"), na = character())
  groups <- unlist(strsplit(lookup$agent_groups[lookup$agent_groups != ""], "|", fixed = TRUE))
  bad <- setdiff(groups, names(AGENT_GROUPS))
  if (length(bad)) stop("agent_lookup.csv uses unknown group(s): ", toString(bad), call. = FALSE)
  if (anyDuplicated(lookup$raw_label)) stop("agent_lookup.csv has duplicate raw labels.", call. = FALSE)
  lookup
}

#' Map raw labels to a list of group vectors. "" maps to no groups.
#' Errors if any non-blank label is not in the lookup.
map_agent_labels <- function(labels, lookup) {
  missing <- setdiff(unique(labels[labels != ""]), lookup$raw_label)
  if (length(missing)) {
    stop("Unmapped causative agent label(s); add to data/reference/agent_lookup.csv:\n  ",
         paste(missing, collapse = "\n  "), call. = FALSE)
  }
  groups <- lookup$agent_groups[match(labels, lookup$raw_label)]
  groups[is.na(groups)] <- ""  # blank label
  lapply(strsplit(groups, "|", fixed = TRUE), function(g) g[g != ""])
}

#' Combine agent-1 and agent-2 groups for each outbreak.
#'
#' Groups keep their order of appearance (agent-1 first). "unknown" is dropped
#' when a known agent is also present, so it never creates a co-detection.
combine_agent_groups <- function(groups1, groups2) {
  mapply(function(a, b) {
    g <- unique(c(a, b))
    if (length(g) > 1) g <- setdiff(g, "unknown")
    if (length(g) == 0) g <- "unknown"
    g
  }, groups1, groups2, SIMPLIFY = FALSE, USE.NAMES = FALSE)
}

#' Add harmonized agent columns.
#'
#' Requires agent1_raw and agent2_raw. Adds:
#'   agent_groups        pipe-separated groups, agent-1 first
#'   agent_group_primary first group listed in agent-1
#'   n_agent_groups      number of distinct groups
#'   co_detection        TRUE when more than one group was detected
#'   agent_mix_group     the single group, or "multiple"
#'   agent_<group>       one logical flag per group (decision D4 counting)
harmonize_agents <- function(df, lookup) {
  g1 <- map_agent_labels(df$agent1_raw, lookup)
  g2 <- map_agent_labels(df$agent2_raw, lookup)
  all_groups <- combine_agent_groups(g1, g2)
  n <- lengths(all_groups)
  df$agent_groups <- vapply(all_groups, paste, character(1), collapse = "|")
  df$agent_group_primary <- vapply(seq_along(g1), function(i) {
    if (length(g1[[i]])) g1[[i]][1] else all_groups[[i]][1]
  }, character(1))
  df$n_agent_groups <- n
  df$co_detection <- n > 1
  df$agent_mix_group <- ifelse(n > 1, "multiple", vapply(all_groups, `[`, character(1), 1))
  for (grp in names(AGENT_GROUPS)) {
    df[[paste0("agent_", grp)]] <- vapply(all_groups, function(g) grp %in% g, logical(1))
  }
  df
}
