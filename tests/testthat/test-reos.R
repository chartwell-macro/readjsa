test_that("current REOS metadata and dynamic data headers parse all tables", {
  out <- read_reos_file(fixture("reos_current.xlsx"))
  expect_equal(length(unique(out$table)), 13L)
  expect_equal(nrow(out), 52L)
  expect_setequal(
    unique(out$observation_status),
    c("published", "not_published", "flagged")
  )
  flagged <- out[out$raw_value == "49%*", , drop = FALSE]
  expect_true(nrow(flagged) > 0L)
  expect_true(all(flagged$value == 0.49))
  expect_true(all(out$value >= 0 & out$value <= 1, na.rm = TRUE))
  expect_true(all(out$disaggregation_group == "Region type"))
  expect_true(all(out$classification_version ==
    "REOS release-vintage workbook disaggregation labels"))
  expect_equal(unique(out$unit), "proportion")
  expect_equal(unique(out$unit_multiplier), 1)
  expect_equal(unique(out$seasonal_adjustment), "not_seasonally_adjusted")
  expect_match(unique(out$table_title), "Fixture series")
  expect_match(unique(out$table_definition), "Explicit definition")
  expect_match(unique(out$workbook_notes), "not seasonally adjusted")
})

test_that("REOS mixed percentage cells canonicalise and scale drift fails", {
  expect_error(
    read_reos_file(fixture("reos_bad_scale.xlsx")),
    class = "readjsa_schema_error"
  )
  parsed <- readjsa:::.parse_number_status(
    c("0.24", "60%*", "n.p."),
    percent_as_proportion = TRUE
  )
  expect_equal(parsed$value[1:2], c(0.24, 0.60))
  expect_equal(
    parsed$observation_status,
    c("published", "flagged", "not_published")
  )
})

test_that("REOS table selection and top-level API are stable", {
  selected <- read_reos_file(fixture("reos_current.xlsx"), tables = "1.1")
  expect_equal(unique(selected$table), "1.1")
  expect_error(
    read_reos_file(fixture("reos_current.xlsx"), tables = character()),
    "at least one"
  )

  release <- release_row("reos", "reos_tables", "reos_current.xlsx")
  out <- read_reos(
    tables = "4.4",
    release = release,
    cache_dir = tempfile("readjsa-reos-"),
    transport = copy_transport(fixture("reos_current.xlsx"))
  )
  expect_equal(unique(out$table), "4.4")
  expect_true(all(nchar(out$raw_sha256) == 64L))
})
