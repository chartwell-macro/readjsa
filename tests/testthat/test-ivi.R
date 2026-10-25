test_that("each current IVI family parses to one stable observation schema", {
  files <- c(
    skill_states = "ivi_skill_states.xlsx",
    anzsco2_states = "ivi_anzsco2_states.xlsx",
    anzsco4_states = "ivi_anzsco4_states.xlsx",
    anzsco2_ivi_regions = "ivi_anzsco2_ivi_regions.xlsx",
    anzsco2_gccsa_sa4 = "ivi_anzsco2_gccsa_sa4.xlsx",
    anzsco4_remoteness_northern =
      "ivi_anzsco4_remoteness_northern.xlsx"
  )
  parsed <- Map(function(family, file) {
    read_ivi_file(fixture(file), family)
  }, names(files), unname(files))
  expect_true(all(vapply(parsed, nrow, integer(1)) > 0L))
  required <- c(
    "date", "value", "series_type", "occupation_code",
    "geography_type", "classification_version", "raw_sha256",
    "source_filename", "terms_url", "parser_version", "schema_version",
    "unit", "unit_multiplier", "frequency", "seasonal_adjustment",
    "file_notes", "licence"
  )
  expect_true(all(vapply(parsed, function(x) {
    all(required %in% names(x))
  }, logical(1))))
  expect_setequal(
    unique(parsed[[6]]$geography_type),
    c("jsa_remoteness", "jsa_northern_australia")
  )
  expect_equal(unique(parsed[[5]]$geography_code), "1GFIX")
  expect_equal(unique(parsed[[1]]$frequency), "Monthly")
  expect_setequal(
    unique(parsed[[1]]$seasonal_adjustment),
    c("trend", "seasonally_adjusted")
  )
  expect_equal(
    unique(parsed[[5]]$classification_version),
    "ANZSCO version not specified; ASGS GCCSA/SA4 version not specified"
  )
  expect_match(unique(parsed[[6]]$file_notes), "Synthetic current-shape")
})

test_that("caller provenance cannot replace observations or exact file hashes", {
  path <- fixture("ivi_skill_states.xlsx")
  expect_error(
    read_ivi_file(
      path, "skill_states",
      provenance = list(value = 999)
    ),
    class = "readjsa_provenance_error"
  )
  expect_error(
    read_ivi_file(
      path, "skill_states",
      provenance = list(release_date = "not-a-date")
    ),
    class = "readjsa_provenance_error"
  )
  expect_error(
    read_ivi_file(
      path, "skill_states",
      provenance = list(revision_policy = "caller invented")
    ),
    class = "readjsa_provenance_error"
  )
  expect_error(
    read_ivi_file(
      path, "skill_states",
      provenance = list(page_url = "https://example.com/publication")
    ),
    class = "readjsa_url_error"
  )
  expect_error(
    read_ivi_file(
      path, "skill_states",
      provenance = list(raw_sha256 = paste(rep("0", 64L), collapse = ""))
    ),
    class = "readjsa_provenance_error"
  )
  exact <- digest::digest(
    path, file = TRUE, algo = "sha256", serialize = FALSE
  )
  out <- read_ivi_file(
    path, "skill_states",
    provenance = list(raw_sha256 = exact)
  )
  expect_equal(unique(out$raw_sha256), exact)
})

test_that("IVI suppression and duplicate keys fail safely", {
  out <- read_ivi_file(
    fixture("ivi_anzsco4_states.xlsx"), "anzsco4_states"
  )
  expect_true("suppressed" %in% out$observation_status)
  expect_error(
    read_ivi_file(
      fixture("ivi_anzsco4_duplicate.xlsx"), "anzsco4_states"
    ),
    class = "readjsa_duplicate_error"
  )
  expect_error(
    read_ivi_file(fixture("ivi_skill_states.xlsx"), "anzsco4_states"),
    class = "readjsa_schema_error"
  )
  empty <- data.frame(
    level = character(), title = character(), state = character(),
    skill_level = character(), "Jan-26" = character(),
    check.names = FALSE
  )
  expect_error(
    readjsa:::.wide_observations(
      empty,
      c("level", "title", "state", "skill_level"),
      "empty IVI"
    ),
    class = "readjsa_schema_error"
  )
})

test_that("optimized wide reshape preserves exact date-major observation parity", {
  wide <- data.frame(
    code = c("A", "B"),
    title = c("Alpha", "Beta"),
    "46143" = c("1,200", "-"),
    "46174" = c("1,250*", "7.5"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  out <- readjsa:::.wide_observations(
    wide,
    c("code", "title"),
    "optimized parity fixture"
  )

  expect_identical(out$code, c("A", "B", "A", "B"))
  expect_identical(out$title, c("Alpha", "Beta", "Alpha", "Beta"))
  expect_identical(
    out$date,
    as.Date(c("2026-05-01", "2026-05-01", "2026-06-01", "2026-06-01"))
  )
  expect_equal(out$value, c(1200, NA, 1250, 7.5))
  expect_identical(
    out$observation_status,
    c("published", "suppressed", "flagged", "published")
  )
  expect_identical(out$raw_value, c("1,200", "-", "1,250*", "7.5"))

  malformed <- wide
  malformed[["46174"]][[2L]] <- "not numeric"
  expect_error(
    readjsa:::.wide_observations(
      malformed,
      c("code", "title"),
      "optimized malformed fixture"
    ),
    class = "readjsa_schema_error"
  )
})

test_that("optimized large IVI families retain exact fixture schema and values", {
  states <- read_ivi_file(
    fixture("ivi_anzsco4_states.xlsx"),
    "anzsco4_states"
  )
  regions <- read_ivi_file(
    fixture("ivi_anzsco2_ivi_regions.xlsx"),
    "anzsco2_ivi_regions"
  )

  expect_equal(nrow(states), 4L)
  expect_identical(
    states$date,
    as.Date(c("2026-05-01", "2026-05-01", "2026-06-01", "2026-06-01"))
  )
  expect_equal(states$value, c(100, 2, 101, NA))
  expect_identical(
    states$observation_status,
    c("published", "published", "published", "suppressed")
  )
  expect_identical(states$occupation_code, c("0", "1111", "0", "1111"))
  expect_true(all(states$geography_type == "state_or_territory"))
  expect_true(all(
    states$classification_version ==
      "ANZSCO version not specified; Australian states and territories"
  ))

  expect_equal(nrow(regions), 8L)
  expect_identical(
    regions$date,
    as.Date(rep(
      c("2026-05-01", "2026-05-01", "2026-06-01", "2026-06-01"),
      2L
    ))
  )
  expect_equal(regions$value, c(80, 8, 82, 9, 80, 8, 82, 9))
  expect_identical(
    regions$series_type,
    rep(c("three_month_average", "index"), each = 4L)
  )
  expect_true(all(regions$geography_type == "jsa_ivi_region"))
  expect_true(all(regions$geography_name == "Fixture Region"))
  expect_true(all(
    regions$classification_version ==
      "ANZSCO version not specified; JSA IVI Regions"
  ))
})

test_that("memory-bounded duplicate detection preserves missing-key semantics", {
  unique_rows <- data.frame(
    date = as.Date(c("2026-01-01", "2026-02-01", "2026-01-01")),
    code = c(NA_character_, NA_character_, "A"),
    value = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  expect_invisible(
    readjsa:::.check_duplicates(
      unique_rows,
      c("date", "code"),
      "missing-key unique fixture"
    )
  )
  duplicate_rows <- rbind(unique_rows, unique_rows[1L, , drop = FALSE])
  expect_error(
    readjsa:::.check_duplicates(
      duplicate_rows,
      c("date", "code"),
      "missing-key duplicate fixture"
    ),
    class = "readjsa_duplicate_error"
  )
})

test_that("upstream aliases remain accepted and top-level reads exact raw bytes", {
  release <- release_row(
    "ivi", "skill_states", "ivi_skill_states.xlsx"
  )
  out <- read_ivi(
    "skill",
    release = release,
    cache_dir = tempfile("readjsa-ivi-"),
    transport = copy_transport(fixture("ivi_skill_states.xlsx"))
  )
  expect_true(nrow(out) > 0L)
  expect_true(all(out$release_id == release$release_id))
  expect_true(all(out$retrieved_at > as.POSIXct("2020-01-01", tz = "UTC")))

  duplicate_release <- rbind(release, release)
  expect_error(
    read_ivi(
      "skill",
      release = duplicate_release,
      cache_dir = tempfile("readjsa-ivi-ambiguous-"),
      transport = copy_transport(fixture("ivi_skill_states.xlsx"))
    ),
    class = "readjsa_discovery_error"
  )

  stale <- release
  stale$reference_period <- as.Date("2026-05-01")
  expect_error(
    read_ivi(
      "skill",
      release = stale,
      cache_dir = tempfile("readjsa-ivi-stale-"),
      transport = copy_transport(fixture("ivi_skill_states.xlsx"))
    ),
    class = "readjsa_provenance_error"
  )

  truncated <- tempfile(fileext = ".xlsx")
  raw <- readBin(fixture("ivi_skill_states.xlsx"), "raw", n = 100L)
  writeBin(raw, truncated)
  expect_error(
    read_ivi_file(truncated, "skill_states"),
    class = "readjsa_schema_error"
  )
})
