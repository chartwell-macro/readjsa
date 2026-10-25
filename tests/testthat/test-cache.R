test_that("transport is atomic, hashed, and reverified on cache hit", {
  cache <- tempfile("readjsa-cache-")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  source <- fixture("ivi_skill_states.xlsx")
  release <- release_row("ivi", "skill_states", basename(source))
  calls <- 0L
  transport <- function(url, destination, timeout, max_bytes) {
    calls <<- calls + 1L
    file.copy(source, destination, overwrite = TRUE)
  }

  first <- fetch_jsa_release(release, cache, transport = transport)
  expect_false(first$cache_hit)
  expect_equal(first$raw_bytes, file.info(source)$size)
  expect_equal(first$final_url, release$source_url)
  expect_equal(
    first$raw_sha256,
    digest::digest(source, file = TRUE, algo = "sha256", serialize = FALSE)
  )
  second <- fetch_jsa_release(release, cache, transport = transport)
  expect_true(second$cache_hit)
  expect_equal(calls, 1L)

  writeBin(charToRaw("corrupt"), first$path)
  expect_warning(
    repaired <- fetch_jsa_release(release, cache, transport = transport),
    "raw bytes no longer match metadata"
  )
  expect_false(repaired$cache_hit)
  expect_equal(calls, 2L)
  expect_equal(repaired$raw_sha256, first$raw_sha256)
})

test_that("top-level reads expose observed raw-cache audit when requested", {
  cache <- tempfile("readjsa-cache-audit-")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  source <- fixture("ivi_skill_states.xlsx")
  release <- release_row("ivi", "skill_states", basename(source))
  calls <- 0L
  transport <- function(url, destination, timeout, max_bytes) {
    calls <<- calls + 1L
    file.copy(source, destination, overwrite = TRUE)
  }
  audit_name <- "READJSA_MEASUREMENT_CACHE_AUDIT"
  old_audit <- Sys.getenv(audit_name, unset = NA_character_)
  on.exit({
    if (is.na(old_audit)) {
      Sys.unsetenv(audit_name)
    } else {
      Sys.setenv(READJSA_MEASUREMENT_CACHE_AUDIT = old_audit)
    }
  }, add = TRUE)
  Sys.setenv(READJSA_MEASUREMENT_CACHE_AUDIT = "true")

  cold <- read_ivi(
    "skill_states",
    release = release,
    cache_dir = cache,
    transport = transport
  )
  warm <- read_ivi(
    "skill_states",
    release = release,
    cache_dir = cache,
    transport = transport
  )
  expect_identical(
    attr(cold, "readjsa_raw_cache_hits", exact = TRUE),
    FALSE
  )
  expect_identical(
    attr(warm, "readjsa_raw_cache_hits", exact = TRUE),
    TRUE
  )
  expect_identical(names(cold), names(warm))
  expect_equal(calls, 1L)

  Sys.unsetenv(audit_name)
  ordinary <- read_ivi(
    "skill_states",
    release = release,
    cache_dir = cache,
    transport = transport
  )
  expect_null(attr(ordinary, "readjsa_raw_cache_hits", exact = TRUE))
  expect_identical(names(ordinary), names(cold))
})

test_that("cache identity and final URL bind parser and official transport", {
  release <- release_row(
    "ivi", "skill_states", "ivi_skill_states.xlsx"
  )
  current <- readjsa:::.cache_identity(
    release, parser_version = "0.1.0", schema_version = "1"
  )
  next_parser <- readjsa:::.cache_identity(
    release, parser_version = "0.1.1", schema_version = "1"
  )
  expect_false(identical(current, next_parser))
  expect_match(current, "parser_version=0.1.0", fixed = TRUE)

  source <- fixture("ivi_skill_states.xlsx")
  redirected <- fetch_jsa_release(
    release,
    cache_dir = tempfile("readjsa-final-url-"),
    transport = function(url, destination, timeout, max_bytes) {
      file.copy(source, destination, overwrite = TRUE)
      "https://www.jobsandskills.gov.au/final/fixture.xlsx"
    }
  )
  expect_equal(
    redirected$final_url,
    "https://www.jobsandskills.gov.au/final/fixture.xlsx"
  )
  expect_error(
    fetch_jsa_release(
      release,
      cache_dir = tempfile("readjsa-bad-final-url-"),
      transport = function(url, destination, timeout, max_bytes) {
        file.copy(source, destination, overwrite = TRUE)
        "https://example.com/escaped.xlsx"
      }
    ),
    class = "readjsa_url_error"
  )
})

test_that("cache identity cannot alias another release specification", {
  cache <- tempfile("readjsa-cache-identity-")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  source <- fixture("ivi_skill_states.xlsx")
  first_release <- release_row("ivi", "skill_states", basename(source))
  second_release <- first_release
  second_release$reference_period <- as.Date("2026-05-01")
  second_release$source_filename <- "same-url-different-period.xlsx"
  calls <- 0L
  transport <- function(url, destination, timeout, max_bytes) {
    calls <<- calls + 1L
    file.copy(source, destination, overwrite = TRUE)
  }

  first <- fetch_jsa_release(first_release, cache, transport = transport)
  second <- fetch_jsa_release(second_release, cache, transport = transport)

  expect_false(second$cache_hit)
  expect_equal(calls, 2L)
  expect_false(identical(first$path, second$path))
  expect_equal(second$reference_period, as.Date("2026-05-01"))
  expect_equal(second$source_filename, "same-url-different-period.xlsx")
})

test_that("cache hits obey byte, age, count, and lock bounds", {
  cache <- tempfile("readjsa-cache-bounds-")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  dir.create(cache)
  unrelated <- file.path(cache, "keep-me.csv")
  writeLines("not readjsa cache data", unrelated)
  source <- fixture("ivi_skill_states.xlsx")
  release <- release_row("ivi", "skill_states", basename(source))
  calls <- 0L
  transport <- function(url, destination, timeout, max_bytes) {
    calls <<- calls + 1L
    file.copy(source, destination, overwrite = TRUE)
  }

  first <- fetch_jsa_release(
    release, cache, max_bytes = 1e6, max_cache_bytes = 2e6,
    transport = transport
  )
  expect_error(
    fetch_jsa_release(
      release, cache, max_bytes = 3, max_cache_bytes = 2e6,
      transport = transport
    ),
    class = "readjsa_size_error"
  )
  expect_equal(calls, 2L)

  metadata_path <- paste0(first$path, ".rds")
  metadata <- readRDS(metadata_path)
  metadata$retrieved_at <- as.POSIXct("2020-01-01", tz = "UTC")
  saveRDS(metadata, metadata_path, version = 3)
  refreshed <- fetch_jsa_release(
    release, cache, max_age = 1, transport = transport
  )
  expect_false(refreshed$cache_hit)

  other <- release
  other$release_id <- "ivi/skill_states/2026-05"
  other$reference_period <- as.Date("2026-05-01")
  other$source_url <- paste0(other$source_url, "?period=2026-05")
  other$source_filename <- "ivi-skill-may-2026.xlsx"
  fetch_jsa_release(
    other, cache, max_files = 1L, transport = transport
  )
  expect_length(
    list.files(cache, pattern = "\\.xlsx$", full.names = TRUE),
    1L
  )
  expect_true(file.exists(unrelated))

  lock_path <- paste0(
    fetch_jsa_release(
      release, cache, force = TRUE, max_files = 2L, transport = transport
    )$path,
    ".lock"
  )
  dir.create(lock_path)
  expect_error(
    fetch_jsa_release(
      release, cache, force = TRUE, lock_timeout = 0.1,
      transport = transport
    ),
    class = "readjsa_cache_error"
  )
})

test_that("transport retries transient failures and cache metadata fails loudly", {
  cache <- tempfile("readjsa-cache-retry-")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  source <- fixture("ivi_skill_states.xlsx")
  release <- release_row("ivi", "skill_states", basename(source))
  calls <- 0L
  flaky <- function(url, destination, timeout, max_bytes) {
    calls <<- calls + 1L
    if (calls < 3L) stop("transient transport failure")
    file.copy(source, destination, overwrite = TRUE)
  }
  snapshot <- fetch_jsa_release(
    release, cache, retries = 2L, transport = flaky
  )
  expect_equal(calls, 3L)
  expect_false(snapshot$cache_hit)

  writeLines("not an RDS", paste0(snapshot$path, ".rds"))
  expect_error(
    fetch_jsa_release(release, cache, transport = copy_transport(source)),
    class = "readjsa_cache_error"
  )
})

test_that("transport rejects bad hosts and oversized results", {
  release <- release_row(
    "ivi", "skill_states", "fixture.xlsx",
    "https://example.com/fixture.xlsx"
  )
  expect_error(fetch_jsa_release(release, tempfile()),
    class = "readjsa_url_error"
  )

  release$source_url <- "https://www.jobsandskills.gov.au/fixture.xlsx"
  expect_error(
    fetch_jsa_release(
      release, tempfile(), max_bytes = 3,
      transport = function(url, destination, timeout, max_bytes) {
        writeBin(charToRaw("too large"), destination)
      }
    ),
    class = "readjsa_size_error"
  )
  expect_error(
    readjsa:::.validate_official_url("http://www.jobsandskills.gov.au/file.xlsx"),
    class = "readjsa_url_error"
  )
  expect_error(
    readjsa:::.validate_official_url("https://example.com/file.xlsx"),
    class = "readjsa_url_error"
  )
  expect_error(
    readjsa:::.validate_official_url(
      "https://user@www.jobsandskills.gov.au/file.xlsx"
    ),
    class = "readjsa_url_error"
  )
  expect_error(
    readjsa:::.validate_official_url(
      "https://www.jobsandskills.gov.au:8443/file.xlsx"
    ),
    class = "readjsa_url_error"
  )

  injected <- release
  injected$raw_sha256 <- paste(rep("0", 64L), collapse = "")
  expect_error(
    fetch_jsa_release(
      injected, tempfile(),
      transport = copy_transport(fixture("ivi_skill_states.xlsx"))
    ),
    class = "readjsa_provenance_error"
  )
  malformed_dates <- release
  malformed_dates$release_date <- "not-a-date"
  malformed_dates$vintage_date <- "not-a-date"
  expect_error(
    fetch_jsa_release(malformed_dates, tempfile()),
    class = "readjsa_provenance_error"
  )
  malformed_dates$release_date <- "2026-02-30"
  malformed_dates$vintage_date <- "2026-02-30"
  expect_error(
    fetch_jsa_release(malformed_dates, tempfile()),
    class = "readjsa_provenance_error"
  )

  expect_error(
    fetch_jsa_release(
      release, tempfile(), timeout = c(1, 2),
      transport = copy_transport(fixture("ivi_skill_states.xlsx"))
    ),
    "must be scalar"
  )
  release$source_filename <- NA_character_
  expect_error(fetch_jsa_release(release, tempfile()), "cannot be blank")
  release$source_filename <- "fixture.xlsx"
  expect_error(
    fetch_jsa_release(release, c(tempfile(), tempfile())),
    "`cache_dir`"
  )
})
