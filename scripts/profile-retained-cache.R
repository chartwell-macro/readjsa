#!/usr/bin/env Rscript

# Single-sample diagnostic profiler for an already retained, verified cache.
#
# This is deliberately not a stage-3 benchmark. It makes no network requests,
# does not claim quiet-machine conditions, and cannot clear the performance
# gate. Each release is parsed in a fresh child process so the evidence exposes
# the genuine route/file peak instead of the cumulative all-nine-file heap.

abort <- function(...) {
  stop(sprintf(...), call. = FALSE)
}

if (!identical(
  Sys.getenv("READJSA_DIAGNOSTIC_PROFILE"),
  "retained-cache"
)) {
  abort(
    paste(
      "Set READJSA_DIAGNOSTIC_PROFILE=retained-cache to run the",
      "single-sample non-benchmark diagnostic."
    )
  )
}
if (.Platform$OS.type == "windows") {
  abort("readjsa retained-cache profiling requires a Unix-alike platform.")
}

time_command <- "/usr/bin/time"
if (!file.exists(time_command)) {
  abort("readjsa retained-cache profiling requires /usr/bin/time.")
}
platform <- Sys.info()[["sysname"]]
time_argument <- switch(
  platform,
  Darwin = "-l",
  Linux = "-v",
  abort("Unsupported profiler platform: %s.", platform)
)
worker <- file.path("scripts", "profile-retained-cache-worker.R")
if (!file.exists(worker)) {
  abort("Run this script from the readjsa package source directory.")
}

cache_dir <- Sys.getenv("READJSA_DIAGNOSTIC_CACHE_DIR", unset = "")
if (!nzchar(cache_dir) || !dir.exists(cache_dir)) {
  abort("READJSA_DIAGNOSTIC_CACHE_DIR must be an existing retained cache.")
}
cache_dir <- normalizePath(cache_dir, winslash = "/", mustWork = TRUE)
metadata_paths <- list.files(
  cache_dir,
  pattern = "^[0-9a-f]{64}\\.(xlsx|csv)\\.rds$",
  full.names = TRUE
)
if (length(metadata_paths) != 9L) {
  abort("The diagnostic requires exactly nine retained release metadata files.")
}

metadata <- lapply(metadata_paths, readRDS)
families <- vapply(metadata, `[[`, character(1), "family_id")
expected_families <- c(
  "skill_states",
  "anzsco4_states",
  "anzsco2_states",
  "anzsco2_ivi_regions",
  "anzsco2_gccsa_sa4",
  "anzsco4_remoteness_northern",
  "reos_tables",
  "salm_sa2",
  "salm_lga"
)
if (!setequal(families, expected_families) || anyDuplicated(families)) {
  abort("The retained cache does not contain the exact nine-family corpus.")
}
metadata_paths <- metadata_paths[match(expected_families, families)]

profile_one <- function(metadata_path) {
  result_path <- tempfile("readjsa-profile-result-", fileext = ".rds")
  log_path <- tempfile("readjsa-profile-time-", fileext = ".log")
  on.exit(unlink(c(result_path, log_path), force = TRUE), add = TRUE)
  command <- file.path(R.home("bin"), "Rscript")
  status <- system2(
    time_command,
    c(
      time_argument,
      shQuote(command),
      "--vanilla",
      shQuote(worker),
      shQuote(metadata_path),
      shQuote(result_path)
    ),
    stdout = log_path,
    stderr = log_path
  )
  if (!identical(as.integer(status), 0L) || !file.exists(result_path)) {
    abort(
      "Retained-cache worker failed for %s:\n%s",
      basename(metadata_path),
      paste(readLines(log_path, warn = FALSE), collapse = "\n")
    )
  }
  result <- readRDS(result_path)
  time_log <- readLines(log_path, warn = FALSE)
  if (identical(platform, "Darwin")) {
    peak_line <- grep(
      "^[[:space:]]*[0-9]+[[:space:]]+maximum resident set size",
      time_log,
      value = TRUE
    )
    peak_rss_mib <- suppressWarnings(as.numeric(sub(
      "^[[:space:]]*([0-9]+).*$",
      "\\1",
      tail(peak_line, 1L)
    ))) / 1024^2
  } else {
    peak_line <- grep(
      "Maximum resident set size \\(kbytes\\):",
      time_log,
      value = TRUE
    )
    peak_rss_mib <- suppressWarnings(as.numeric(sub(
      "^.*:[[:space:]]*([0-9]+).*$",
      "\\1",
      tail(peak_line, 1L)
    ))) / 1024
  }
  if (!nrow(result) || !is.finite(peak_rss_mib)) {
    abort("Retained-cache child produced incomplete diagnostic evidence.")
  }
  result$peak_rss_mib <- peak_rss_mib
  result$peak_increment_mib <- peak_rss_mib - result$rss_before_parse_mib
  result$evidence_class <-
    "diagnostic_retained_cache_single_sample_not_benchmark"
  result$recorded_at_utc <- format(
    Sys.time(),
    "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )
  result
}

rows <- lapply(metadata_paths, function(path) {
  metadata <- readRDS(path)
  message("Profiling retained family: ", metadata$family_id)
  profile_one(path)
})
evidence <- do.call(rbind, rows)

output <- Sys.getenv(
  "READJSA_DIAGNOSTIC_OUTPUT",
  unset = file.path("inst", "validation", "performance-diagnostic.csv")
)
output_parent <- dirname(output)
if (!dir.exists(output_parent) &&
    !dir.create(output_parent, recursive = TRUE, showWarnings = FALSE)) {
  abort("Could not create the diagnostic output directory.")
}
utils::write.csv(evidence, output, row.names = FALSE, na = "")
print(evidence[, c(
  "family_id", "observations", "returned_object_mib",
  "elapsed_seconds", "peak_rss_mib"
)], row.names = FALSE)
message("Diagnostic evidence written to ", normalizePath(output))
