test_that("SALM preserves geography codes, quarter ends, and availability", {
  out <- read_salm_file(fixture("salm_sa2.csv"), "sa2")
  expect_equal(unique(out$geography_code)[1], "001000001")
  expect_setequal(
    unique(out$date),
    as.Date(c("2025-12-31", "2026-03-31"))
  )
  expect_true(all(unique(out$classification_version) == "SA2 2021 ASGS"))
  expect_true(all(c("unavailable", "not_published") %in%
    out$observation_status))
  expect_false("suppressed" %in% out$observation_status)
  expect_match(unique(out$file_notes), "data are unavailable")
  expect_equal(unique(out$frequency), "Quarterly")
  expect_equal(unique(out$unit_multiplier), 1)
  expect_equal(
    unique(out$seasonal_adjustment),
    "seasonal_adjustment_not_stated"
  )
})

test_that("SALM preamble and complete item sets fail closed", {
  path <- fixture("salm_lga.csv")
  broken_preamble <- tempfile(fileext = ".csv")
  lines <- readLines(path, warn = FALSE)
  lines[[1L]] <- "Unrecognised preamble"
  writeLines(lines, broken_preamble)
  expect_error(
    read_salm_file(broken_preamble, "lga"),
    class = "readjsa_schema_error"
  )

  missing_item <- tempfile(fileext = ".csv")
  writeLines(lines <- readLines(path, warn = FALSE)[-4L], missing_item)
  expect_error(
    read_salm_file(missing_item, "lga"),
    class = "readjsa_schema_error"
  )
})

test_that("historical ASGS drift is preserved rather than relabelled", {
  old <- read_salm_file(fixture("salm_lga_asgs2016.csv"), "lga")
  current <- read_salm_file(fixture("salm_lga.csv"), "lga")
  expect_equal(unique(old$classification_version), "LGA 2016 ASGS")
  expect_equal(unique(current$classification_version), "LGA 2025 ASGS")
  expect_equal(unique(old$geography_code), "00050")
})

test_that("SALM top-level API attaches revised-series policy", {
  release <- release_row(
    "salm", "salm_lga", "salm_lga.csv",
    "https://www.dewr.gov.au/download/fixture/csv",
    reference_period = as.Date("2026-03-31")
  )
  out <- read_salm(
    "lga",
    release = release,
    cache_dir = tempfile("readjsa-salm-"),
    transport = copy_transport(fixture("salm_lga.csv"))
  )
  expect_match(unique(out$revision_policy), "revises the historical series")
  expect_equal(unique(out$terms_url), "https://www.dewr.gov.au/copyright")
})
