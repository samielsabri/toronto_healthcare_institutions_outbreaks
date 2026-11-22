# Facility derivation (decision D5): a facility is a physical site, identified
# by its normalized street address, split by setting group where one address
# holds more than one setting. Operators are not used. One-off setting coding
# slips do not split a site (decision D5; see facility_setting_group()).
#
# Steps:
#   1. normalize_address(): rule-based cleaning of the free-text address.
#   2. data/reference/address_crosswalk.csv: hand-curated fixes for what the
#      rules cannot resolve (typos, multi-number campuses, number variants).
#   3. facility_id = canonical address + setting group.

STREET_ABBREVIATIONS <- c(
  "STREET" = "ST", "AVENUE" = "AVE", "ROAD" = "RD", "DRIVE" = "DR",
  "BOULEVARD" = "BLVD", "PARKWAY" = "PKWY", "PKY" = "PKWY", "CRESCENT" = "CRES",
  "COURT" = "CRT", "CT" = "CRT", "PLACE" = "PL", "TERRACE" = "TERR",
  "LANE" = "LN", "SQUARE" = "SQ", "CIRCLE" = "CIR", "HEIGHTS" = "HTS",
  "GARDENS" = "GDNS", "TRAIL" = "TRL", "GATE" = "GT",
  "EAST" = "E", "WEST" = "W", "NORTH" = "N", "SOUTH" = "S"
)

#' Normalize a free-text street address to a comparable key.
#'
#' - uppercases and removes punctuation (. , ' #);
#' - keeps a leading street-number range ("10-14", "10/14", "700 & 720",
#'   "705 - 707") and writes it as "10-14";
#' - drops any unit/ward suffix that follows a hyphen after the street name
#'   ("1001 Queen St W - Unit 1-2" -> "1001 QUEEN ST W");
#' - drops a trailing city/province ("TORONTO", "ON");
#' - abbreviates street types and directions (STREET -> ST, EAST -> E).
normalize_address <- function(x) {
  x <- toupper(stringr::str_squish(x))
  # Leading number or number range: 10, 10-14, 10 -14, 10/14, 700 & 720, 705 - 707
  num_re <- "^(\\d+[A-Z]?)(?:\\s*[-/&]\\s*(\\d+[A-Z]?))?\\s+"
  m <- stringr::str_match(x, num_re)
  number <- ifelse(is.na(m[, 1]), "",
                   ifelse(is.na(m[, 3]), m[, 2], paste0(m[, 2], "-", m[, 3])))
  rest <- ifelse(is.na(m[, 1]), x, substring(x, nchar(m[, 1]) + 1))
  # Unit suffix: everything from the first hyphen that has a space on either side
  rest <- stringr::str_replace(rest, "(\\s-|-\\s).*$", "")
  rest <- stringr::str_replace_all(rest, "[.,'#]", " ")
  rest <- stringr::str_replace(rest, "\\s+(TORONTO|ON|ONTARIO)(\\s+ON)?\\s*$", "")
  words <- strsplit(stringr::str_squish(rest), " ", fixed = TRUE)
  rest <- vapply(words, function(w) {
    hit <- w %in% names(STREET_ABBREVIATIONS)
    w[hit] <- STREET_ABBREVIATIONS[w[hit]]
    paste(w, collapse = " ")
  }, character(1))
  stringr::str_squish(paste(number, rest))
}

#' Read the hand-curated address crosswalk (normalized -> canonical address).
load_address_crosswalk <- function(path = "data/reference/address_crosswalk.csv") {
  readr::read_csv(path, col_types = readr::cols(.default = "c"), na = character())
}

#' Apply the crosswalk; addresses not listed map to themselves.
canonical_address <- function(addr_norm, crosswalk) {
  if (anyDuplicated(crosswalk$addr_norm)) {
    stop("address_crosswalk.csv has duplicate addr_norm entries.", call. = FALSE)
  }
  idx <- match(addr_norm, crosswalk$addr_norm)
  ifelse(is.na(idx), addr_norm, crosswalk$addr_canonical[idx])
}

#' Normalize an institution (unit) name for use as a key: uppercase, squished,
#' unified spacing around hyphens.
normalize_name <- function(x) {
  x <- toupper(stringr::str_squish(x))
  x <- stringr::str_replace_all(x, "\\s*-\\s*", " - ")
  stringr::str_squish(stringr::str_replace(x, "\\s-\\s*$", ""))
}

#' Facility-level name stem: the name with its last " - " segment removed.
#' Used only to pick a display name; the facility key is the address.
name_stem <- function(x) {
  x <- normalize_name(x)
  ifelse(stringr::str_detect(x, " - "), stringr::str_replace(x, " - [^-]*$", ""), x)
}

#' Make a URL- and file-safe ID from text.
slugify <- function(x) {
  x <- tolower(x)
  x <- stringr::str_replace_all(x, "[^a-z0-9]+", "-")
  stringr::str_replace_all(x, "^-|-$", "")
}

#' Setting group used to define the facility (decision D5).
#'
#' An outbreak whose setting group is a small minority at its address (at most
#' `max_n` outbreaks AND under `max_share` of the address's outbreaks, while
#' another setting is also recorded there) is assigned to the address's
#' dominant setting for facility purposes. This stops one-off setting coding
#' slips (e.g. a retirement residence coded "LTCH" once) from splitting a
#' building into two facilities. The outbreak's own setting_group is unchanged.
facility_setting_group <- function(address, setting_group, max_n = 2L, max_share = 0.10) {
  tab <- dplyr::count(tibble::tibble(address, setting_group), .data$address, .data$setting_group)
  tab <- dplyr::mutate(
    dplyr::group_by(tab, .data$address),
    share = .data$n / sum(.data$n),
    dominant = .data$setting_group[which.max(.data$n)],
    minority = dplyr::n() > 1 & .data$n <= max_n & .data$share < max_share
  )
  key <- paste(address, setting_group)
  idx <- match(key, paste(tab$address, tab$setting_group))
  ifelse(tab$minority[idx], tab$dominant[idx], setting_group)
}

#' Add facility columns to cleaned outbreaks.
#'
#' Requires columns: institution_name, institution_address, setting_group, began.
#' Adds: address_norm, facility_address, facility_setting_group,
#' flag_facility_setting_reassigned, facility_id, facility_name, unit_id.
add_facility <- function(df, crosswalk) {
  df <- dplyr::mutate(
    df,
    address_norm = normalize_address(.data$institution_address),
    facility_address = canonical_address(.data$address_norm, crosswalk),
    facility_setting_group = facility_setting_group(.data$facility_address, .data$setting_group),
    flag_facility_setting_reassigned = .data$facility_setting_group != .data$setting_group,
    facility_id = paste(slugify(.data$facility_address),
                        slugify(.data$facility_setting_group), sep = "--"),
    unit_id = paste(.data$facility_id, slugify(normalize_name(.data$institution_name)),
                    sep = "--")
  )
  # Display name: most frequent name stem at the facility, preferring recent
  # years (names were truncated at 60 characters until 2021).
  names_tbl <- df |>
    dplyr::mutate(stem = name_stem(.data$institution_name),
                  recent = lubridate::year(.data$began) >= 2022) |>
    dplyr::count(.data$facility_id, .data$stem, .data$recent) |>
    dplyr::arrange(.data$facility_id, dplyr::desc(.data$recent), dplyr::desc(.data$n), .data$stem) |>
    dplyr::distinct(.data$facility_id, .keep_all = TRUE) |>
    dplyr::transmute(.data$facility_id, facility_name = stringr::str_to_title(.data$stem))
  dplyr::left_join(df, names_tbl, by = "facility_id")
}
