lookup <- tibble::tribble(
  ~raw_label,                          ~agent_groups,              ~note,
  "COVID-19",                          "covid19",                  "",
  "Influenza A (H3)",                  "influenza_a",              "",
  "Influenza A and B",                 "influenza_a|influenza_b",  "",
  "Respiratory syncytial virus",       "rsv",                      "",
  "Rhinovirus, Coronavirus",           "rhino_entero|seasonal_cov", "",
  "Unable to identify",                "unknown",                  "",
  "None",                              "",                         ""
)

test_that("labels map to groups; blank and 'None' mean no agent", {
  g <- map_agent_labels(c("COVID-19", "", "None", "Influenza A and B"), lookup)
  expect_equal(g, list("covid19", character(0), character(0), c("influenza_a", "influenza_b")))
})

test_that("an unmapped label stops the pipeline and names the label", {
  expect_error(map_agent_labels(c("COVID-19", "Novel virus X"), lookup), "Novel virus X")
})

test_that("co-detection is detected within agent-1 and across agent-1/agent-2", {
  df <- tibble::tibble(
    agent1_raw = c("COVID-19", "Rhinovirus, Coronavirus", "Influenza A (H3)", "Influenza A (H3)"),
    agent2_raw = c("",         "",                        "Respiratory syncytial virus", "None")
  )
  out <- harmonize_agents(df, lookup)
  expect_equal(out$co_detection, c(FALSE, TRUE, TRUE, FALSE))
  expect_equal(out$n_agent_groups, c(1L, 2L, 2L, 1L))
  expect_equal(out$agent_groups, c("covid19", "rhino_entero|seasonal_cov", "influenza_a|rsv", "influenza_a"))
  expect_equal(out$agent_mix_group, c("covid19", "multiple", "multiple", "influenza_a"))
  expect_equal(out$agent_group_primary, c("covid19", "rhino_entero", "influenza_a", "influenza_a"))
})

test_that("decision D4: a co-detection counts under every group detected", {
  df <- tibble::tibble(agent1_raw = "Influenza A (H3)", agent2_raw = "Respiratory syncytial virus")
  out <- harmonize_agents(df, lookup)
  expect_true(out$agent_influenza_a)
  expect_true(out$agent_rsv)
  expect_false(out$agent_covid19)
})

test_that("unknown never creates a co-detection alongside a known agent", {
  df <- tibble::tibble(agent1_raw = c("Unable to identify", "Unable to identify"),
                       agent2_raw = c("", "COVID-19"))
  out <- harmonize_agents(df, lookup)
  expect_equal(out$agent_groups, c("unknown", "covid19"))
  expect_equal(out$co_detection, c(FALSE, FALSE))
})

test_that("the project lookup covers every label in the raw data", {
  real_lookup <- load_agent_lookup(root_path("data", "reference", "agent_lookup.csv"))
  raw <- stack_raw(read_raw_files(root_path("data", "raw")))
  expect_silent(map_agent_labels(c(raw$agent1_raw, raw$agent2_raw), real_lookup))
})

test_that("the project lookup only uses defined groups", {
  real_lookup <- load_agent_lookup(root_path("data", "reference", "agent_lookup.csv"))
  groups <- unlist(strsplit(real_lookup$agent_groups[real_lookup$agent_groups != ""], "|", fixed = TRUE))
  expect_true(all(groups %in% names(AGENT_GROUPS)))
  # Decision D2: both norovirus labels map to the same group
  expect_equal(real_lookup$agent_groups[real_lookup$raw_label %in% c("Norovirus", "Norovirus-like")],
               c("norovirus", "norovirus"))
  # COVID-19 and seasonal coronavirus are never mixed up
  expect_equal(real_lookup$agent_groups[real_lookup$raw_label == "Coronavirus*"], "seasonal_cov")
  expect_equal(real_lookup$agent_groups[real_lookup$raw_label == "COVID-19"], "covid19")
})
