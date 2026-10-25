#!/usr/bin/env Rscript

abort <- function(...) {
  stop(sprintf(...), call. = FALSE)
}

truthy <- function(value) {
  tolower(trimws(as.character(value))) %in% c("1", "true", "yes", "on")
}

if (!identical(Sys.getenv("READJSA_QUIET_MEASUREMENT_SLOT"), "confirmed")) {
  abort(
    paste(
      "Refusing to benchmark outside an explicitly confirmed quiet slot.",
      "Set READJSA_QUIET_MEASUREMENT_SLOT=confirmed to opt in."
    )
  )
}
if (!identical(tolower(Sys.getenv("READJSA_LIVE")), "true")) {
  abort("READJSA_LIVE=true is required for the live measurement harness.")
}
if (.Platform$OS.type == "windows") {
  abort("readjsa peak-RSS measurement requires a Unix-alike platform.")
}

time_command <- "/usr/bin/time"
if (!file.exists(time_command)) {
  abort("readjsa peak-RSS measurement requires /usr/bin/time.")
}
platform <- Sys.info()[["sysname"]]
time_argument <- switch(
  platform,
  Darwin = "-l",
  Linux = "-v",
  abort("Unsupported measurement platform: %s.", platform)
)
worker <- file.path("scripts", "measure-stage3-worker.R")
if (!file.exists(worker)) {
  abort("Run the measurement harness from the readjsa package directory.")
}
cache_policy_helper <- file.path("scripts", "measurement-cache-policy.R")
if (!file.exists(cache_policy_helper)) {
  abort("Measurement raw-cache policy helper is missing.")
}
sys.source(cache_policy_helper, envir = environment())

manifest <- data.frame(
  dataset_id = c(rep("ivi", 6L), "reos", "salm", "salm"),
  family_id = c(
    "skill_states",
    "anzsco4_states",
    "anzsco2_states",
    "anzsco2_ivi_regions",
    "anzsco2_gccsa_sa4",
    "anzsco4_remoteness_northern",
    "reos_tables",
    "salm_sa2",
    "salm_lga"
  ),
  stringsAsFactors = FALSE
)
deployment_idle_rss_mib <- suppressWarnings(as.numeric(Sys.getenv(
  "READJSA_DEPLOYMENT_IDLE_RSS_MIB",
  unset = "416"
)))
if (length(deployment_idle_rss_mib) != 1L ||
    !is.finite(deployment_idle_rss_mib) ||
    deployment_idle_rss_mib <= 0) {
  abort("READJSA_DEPLOYMENT_IDLE_RSS_MIB must be one positive MiB value.")
}

summarise_measurements <- function(raw) {
  successful <- raw[raw$status == "ok", , drop = FALSE]
  if (!nrow(successful)) {
    return(data.frame(
      dataset_id = character(),
      family_id = character(),
      mode = character(),
      repetitions = integer(),
      min_seconds = numeric(),
      median_seconds = numeric(),
      p95_seconds = numeric(),
      max_seconds = numeric(),
      max_peak_rss_mib = numeric(),
      max_peak_increment_mib = numeric(),
      max_projected_deployment_peak_mib = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  groups <- split(
    successful,
    interaction(
      successful$dataset_id,
      successful$family_id,
      successful$mode,
      drop = TRUE,
      lex.order = TRUE
    )
  )
  do.call(rbind, lapply(groups, function(group) {
    data.frame(
      dataset_id = group$dataset_id[[1L]],
      family_id = group$family_id[[1L]],
      mode = group$mode[[1L]],
      repetitions = nrow(group),
      min_seconds = min(group$elapsed_seconds),
      median_seconds = stats::median(group$elapsed_seconds),
      p95_seconds = unname(stats::quantile(
        group$elapsed_seconds,
        probs = 0.95,
        names = FALSE,
        type = 8
      )),
      max_seconds = max(group$elapsed_seconds),
      max_peak_rss_mib = max(group$peak_rss_mib),
      max_peak_increment_mib = max(group$peak_increment_mib),
      max_projected_deployment_peak_mib = max(
        group$projected_deployment_peak_mib
      ),
      stringsAsFactors = FALSE
    )
  }))
}

if (truthy(Sys.getenv("READJSA_MEASUREMENT_SELF_TEST", unset = "false"))) {
  fixture <- merge(
    manifest,
    expand.grid(
      mode = c("cold", "warm"),
      repetition = seq_len(3L),
      stringsAsFactors = FALSE
    ),
    by = NULL
  )
  fixture$status <- "ok"
  fixture$raw_cache_releases <- 1L
  fixture$raw_cache_hits <- as.integer(fixture$mode == "warm")
  fixture$raw_cache_misses <- as.integer(fixture$mode == "cold")
  fixture$elapsed_seconds <- seq_len(nrow(fixture)) / 10
  fixture$peak_rss_mib <- 100 + seq_len(nrow(fixture)) / 10
  fixture$rss_before_read_mib <- 90
  fixture$peak_increment_mib <-
    fixture$peak_rss_mib - fixture$rss_before_read_mib
  fixture$deployment_idle_rss_mib <- deployment_idle_rss_mib
  fixture$projected_deployment_peak_mib <-
    deployment_idle_rss_mib + fixture$peak_increment_mib
  fixture <- fixture[
    order(fixture$mode, fixture$repetition, fixture$family_id),
    ,
    drop = FALSE
  ]
  invisible(Map(
    function(mode, releases, hits, misses) {
      assert_measurement_cache_outcome(
        mode,
        list(
          raw_cache_releases = releases,
          raw_cache_hits = hits,
          raw_cache_misses = misses
        )
      )
    },
    fixture$mode,
    fixture$raw_cache_releases,
    fixture$raw_cache_hits,
    fixture$raw_cache_misses
  ))
  cold_hit_rejected <- inherits(
    tryCatch(
      assert_measurement_cache_outcome(
        "cold",
        list(
          raw_cache_releases = 1L,
          raw_cache_hits = 1L,
          raw_cache_misses = 0L
        )
      ),
      error = identity
    ),
    "error"
  )
  warm_miss_rejected <- inherits(
    tryCatch(
      assert_measurement_cache_outcome(
        "warm",
        list(
          raw_cache_releases = 1L,
          raw_cache_hits = 0L,
          raw_cache_misses = 1L
        )
      ),
      error = identity
    ),
    "error"
  )
  counts <- stats::xtabs(
    ~ family_id + mode,
    data = fixture
  )
  if (!identical(
    unname(dim(counts)),
    c(nrow(manifest), 2L)
  ) || any(counts < 3L)) {
    abort("Self-test fixture does not cover three cold and warm runs per family.")
  }
  summary <- summarise_measurements(fixture)
  summary_counts <- stats::xtabs(
    repetitions ~ family_id + mode,
    data = summary
  )
  stopifnot(
    nrow(summary) == nrow(manifest) * 2L,
    all(summary$repetitions >= 3L),
    all(summary_counts >= 3L),
    cold_hit_rejected,
    warm_miss_rejected,
    all(c(
      "family_id", "mode", "min_seconds", "median_seconds",
      "max_seconds", "max_peak_rss_mib", "max_peak_increment_mib",
      "max_projected_deployment_peak_mib"
    ) %in% names(summary))
  )
  cat(
    paste(
      "readjsa serialized per-family measurement harness self-test:",
      paste(
        "PASS (>=3 cold misses and >=3 warm hits in fresh processes",
        "per family; cache mismatches rejected)\n"
      )
    )
  )
  quit(save = "no", status = 0L)
}

required_packages <- c("pkgload")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  abort(
    "readjsa measurement requires: %s.",
    paste(missing_packages, collapse = ", ")
  )
}

repetitions <- suppressWarnings(as.integer(Sys.getenv(
  "READJSA_MEASUREMENT_REPETITIONS",
  unset = "3"
)))
if (length(repetitions) != 1L || is.na(repetitions) || repetitions < 3L) {
  abort("READJSA_MEASUREMENT_REPETITIONS must be an integer of at least three.")
}

legacy_output <- Sys.getenv(
  "READJSA_MEASUREMENT_OUTPUT",
  unset = file.path(
    Sys.getenv("TMPDIR", unset = "/tmp"),
    "readjsa-stage3-measurement"
  )
)
output_stem <- tools::file_path_sans_ext(legacy_output)
raw_output <- Sys.getenv(
  "READJSA_MEASUREMENT_RAW_OUTPUT",
  unset = paste0(output_stem, "-raw.csv")
)
summary_output <- Sys.getenv(
  "READJSA_MEASUREMENT_SUMMARY_OUTPUT",
  unset = paste0(output_stem, "-summary.csv")
)
log_dir <- Sys.getenv(
  "READJSA_MEASUREMENT_LOG_DIR",
  unset = paste0(output_stem, "-logs")
)
for (path in c(raw_output, summary_output)) {
  if (!identical(tolower(tools::file_ext(path)), "csv")) {
    abort("readjsa raw and summary measurement outputs must use .csv extensions.")
  }
  if (!dir.exists(dirname(path))) {
    abort("Measurement output parent directories must already exist.")
  }
}
if (identical(
  normalizePath(raw_output, mustWork = FALSE),
  normalizePath(summary_output, mustWork = FALSE)
)) {
  abort("readjsa raw and summary measurement outputs must be distinct files.")
}
if (!dir.exists(log_dir) &&
    !dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)) {
  abort("Could not create READJSA_MEASUREMENT_LOG_DIR.")
}
log_dir <- normalizePath(log_dir, winslash = "/", mustWork = TRUE)

cache_root <- trimws(Sys.getenv(
  "READJSA_MEASUREMENT_CACHE_ROOT",
  unset = ""
))
if (!nzchar(cache_root)) {
  cache_root <- tempfile("readjsa-stage3-cache-")
}
if (file.exists(cache_root)) {
  abort(
    paste(
      "READJSA_MEASUREMENT_CACHE_ROOT must not already exist.",
      "The harness owns independent empty cache lanes so every family",
      "has the requested cold runs before the same lanes are replayed warm."
    )
  )
}
if (!dir.create(cache_root, recursive = TRUE, showWarnings = FALSE)) {
  abort("Could not create the private readjsa measurement cache.")
}
cache_root <- normalizePath(cache_root, winslash = "/", mustWork = TRUE)
cache_lanes <- file.path(
  cache_root,
  sprintf("repetition-%02d", seq_len(repetitions))
)
created <- vapply(cache_lanes, function(path) {
  dir.create(path, recursive = FALSE, showWarnings = FALSE)
}, logical(1))
if (!all(created)) {
  abort("Could not create independent empty cold-cache lanes.")
}
keep_cache <- truthy(Sys.getenv(
  "READJSA_MEASUREMENT_KEEP_CACHE",
  unset = "false"
))
if (!keep_cache) {
  on.exit(unlink(cache_root, recursive = TRUE, force = TRUE), add = TRUE)
}

parse_peak_rss <- function(log_path) {
  lines <- readLines(log_path, warn = FALSE)
  if (identical(platform, "Darwin")) {
    match <- grep(
      "^[[:space:]]*[0-9]+[[:space:]]+maximum resident set size",
      lines,
      value = TRUE
    )
    value <- suppressWarnings(as.numeric(sub(
      "^[[:space:]]*([0-9]+).*$",
      "\\1",
      tail(match, 1L)
    ))) / 1024^2
  } else {
    match <- grep(
      "Maximum resident set size \\(kbytes\\):",
      lines,
      value = TRUE
    )
    value <- suppressWarnings(as.numeric(sub(
      "^.*:[[:space:]]*([0-9]+).*$",
      "\\1",
      tail(match, 1L)
    ))) / 1024
  }
  if (!is.finite(value) || value <= 0) {
    abort("Could not parse peak RSS from %s.", log_path)
  }
  value
}

run_family <- function(
  repetition,
  mode,
  dataset_id,
  family_id,
  cache_dir
) {
  log_path <- file.path(
    log_dir,
    sprintf("repetition-%02d-%s-%s.log", repetition, mode, family_id)
  )
  result_path <- tempfile("readjsa-measurement-result-", fileext = ".rds")
  on.exit(unlink(result_path, force = TRUE), add = TRUE)
  command <- file.path(R.home("bin"), "Rscript")
  started <- proc.time()[["elapsed"]]
  status_code <- system2(
    time_command,
    c(
      time_argument,
      shQuote(command),
      "--vanilla",
      shQuote(worker),
      shQuote(family_id),
      shQuote(result_path)
    ),
    stdout = log_path,
    stderr = log_path,
    env = c(
      "READJSA_LIVE=true",
      paste0("READJSA_CACHE_DIR=", cache_dir)
    )
  )
  elapsed_seconds <- proc.time()[["elapsed"]] - started
  status <- if (identical(as.integer(status_code), 0L) &&
      file.exists(result_path)) "ok" else "failed"
  worker_result <- if (identical(status, "ok")) {
    readRDS(result_path)
  } else {
    list(
      observations = NA_integer_,
      returned_object_mib = NA_real_,
      rss_before_read_mib = NA_real_,
      raw_cache_releases = NA_integer_,
      raw_cache_hits = NA_integer_,
      raw_cache_misses = NA_integer_
    )
  }
  if (identical(status, "ok")) {
    assert_measurement_cache_outcome(mode, worker_result)
  }
  peak_rss_mib <- if (identical(status, "ok")) {
    parse_peak_rss(log_path)
  } else {
    NA_real_
  }
  peak_increment_mib <- peak_rss_mib - worker_result$rss_before_read_mib
  data.frame(
    repetition = repetition,
    dataset_id = dataset_id,
    family_id = family_id,
    mode = mode,
    cache_policy = if (identical(mode, "cold")) {
      "independent_empty_lane_first_read_of_family"
    } else {
      "same_lane_verified_raw_cache_hit"
    },
    process_policy = "serialized_fresh_process_per_family_mode_repetition",
    status = status,
    observations = worker_result$observations,
    returned_object_mib = worker_result$returned_object_mib,
    raw_cache_releases = worker_result$raw_cache_releases,
    raw_cache_hits = worker_result$raw_cache_hits,
    raw_cache_misses = worker_result$raw_cache_misses,
    elapsed_seconds = unname(elapsed_seconds),
    rss_before_read_mib = worker_result$rss_before_read_mib,
    peak_rss_mib = peak_rss_mib,
    peak_increment_mib = peak_increment_mib,
    deployment_idle_rss_mib = deployment_idle_rss_mib,
    projected_deployment_peak_mib =
      deployment_idle_rss_mib + peak_increment_mib,
    log_path = log_path,
    recorded_at_utc = format(
      Sys.time(),
      "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    stringsAsFactors = FALSE
  )
}

rows <- list()
for (mode in c("cold", "warm")) {
  for (repetition in seq_len(repetitions)) {
    for (index in seq_len(nrow(manifest))) {
      message(sprintf(
        "readjsa quiet-slot %s repetition %d/%d: %s",
        mode,
        repetition,
        repetitions,
        manifest$family_id[[index]]
      ))
      rows[[length(rows) + 1L]] <- run_family(
        repetition,
        mode,
        manifest$dataset_id[[index]],
        manifest$family_id[[index]],
        cache_lanes[[repetition]]
      )
    }
  }
}

raw <- do.call(rbind, rows)
run_counts <- stats::xtabs(
  ~ family_id + mode,
  data = raw[raw$status == "ok", , drop = FALSE]
)
summary <- summarise_measurements(raw)
utils::write.csv(raw, raw_output, row.names = FALSE, na = "")
utils::write.csv(summary, summary_output, row.names = FALSE, na = "")

if (!identical(
  unname(dim(run_counts)),
  c(nrow(manifest), 2L)
) || any(run_counts < 3L)) {
  abort(
    paste(
      "Incomplete measurement matrix: each family requires at least",
      "three successful cold and three successful warm runs."
    )
  )
}

message("Raw readjsa per-family measurements: ", raw_output)
message("Summary readjsa per-family measurements: ", summary_output)
message("Per-family repetition logs: ", log_dir)

if (any(raw$status != "ok")) {
  abort(
    "readjsa live measurement failed for: %s.",
    paste(
      paste0(
        raw$family_id[raw$status != "ok"],
        "/",
        raw$mode[raw$status != "ok"]
      ),
      collapse = ", "
    )
  )
}
