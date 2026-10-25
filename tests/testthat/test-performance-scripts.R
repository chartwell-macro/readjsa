readjsa_source_root <- function() {
  local_root <- normalizePath(
    testthat::test_path("..", ".."),
    mustWork = FALSE
  )
  candidates <- c(
    local_root,
    file.path(
      system.file(package = "readjsa"),
      "..", "00_pkg_src", "readjsa"
    ),
    file.path(getwd(), "..", "00_pkg_src", "readjsa"),
    file.path(getwd(), "00_pkg_src", "readjsa")
  )
  matches <- candidates[
    file.exists(file.path(candidates, "DESCRIPTION")) &
      dir.exists(file.path(candidates, "scripts"))
  ]
  if (!length(matches)) {
    stop("Could not locate the readjsa source scripts.", call. = FALSE)
  }
  normalizePath(matches[[1L]])
}

readjsa_validation_file <- function(name) {
  path <- system.file("validation", name, package = "readjsa")
  if (!nzchar(path)) {
    path <- file.path(
      readjsa_source_root(),
      "inst", "validation", name
    )
  }
  path
}

test_that("retained-cache diagnostic is explicit and non-benchmark", {
  package_root <- readjsa_source_root()
  script <- file.path(package_root, "scripts", "profile-retained-cache.R")
  worker <- file.path(
    package_root,
    "scripts",
    "profile-retained-cache-worker.R"
  )
  expect_true(file.exists(script))
  expect_true(file.exists(worker))

  script_text <- readLines(script, warn = FALSE)
  expect_true(any(grepl(
    "READJSA_DIAGNOSTIC_PROFILE",
    script_text,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "diagnostic_retained_cache_single_sample_not_benchmark",
    script_text,
    fixed = TRUE
  )))
  expect_false(any(grepl(
    "READJSA_QUIET_MEASUREMENT_SLOT=confirmed",
    script_text,
    fixed = TRUE
  )))
  expect_silent(parse(file = script))
  expect_silent(parse(file = worker))
})

test_that("live validation and measurement are serialized per family", {
  package_root <- readjsa_source_root()
  measure <- file.path(package_root, "scripts", "measure-stage3.R")
  measure_worker <- file.path(
    package_root,
    "scripts",
    "measure-stage3-worker.R"
  )
  cache_policy <- file.path(
    package_root,
    "scripts",
    "measurement-cache-policy.R"
  )
  validate <- file.path(package_root, "scripts", "validate-live.R")
  expect_true(all(file.exists(c(
    measure,
    measure_worker,
    cache_policy,
    validate
  ))))
  expect_silent(parse(file = measure))
  expect_silent(parse(file = measure_worker))
  expect_silent(parse(file = cache_policy))
  expect_silent(parse(file = validate))

  measure_text <- readLines(measure, warn = FALSE)
  worker_text <- readLines(measure_worker, warn = FALSE)
  validate_text <- readLines(validate, warn = FALSE)
  expect_true(any(grepl(
    "serialized_fresh_process_per_family_mode_repetition",
    measure_text,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "independent_empty_lane_first_read_of_family",
    measure_text,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "peak_increment_mib",
    measure_text,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "projected_deployment_peak_mib",
    measure_text,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "rss_before_read_mib",
    worker_text,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "raw_cache_hits",
    worker_text,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "assert_measurement_cache_outcome(mode, worker_result)",
    measure_text,
    fixed = TRUE
  )))
  expect_true(any(grepl("counts < 3L", measure_text, fixed = TRUE)))
  expect_true(any(grepl(
    "run_counts < 3L",
    measure_text,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "READJSA_VALIDATE_FAMILY",
    worker_text,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "READJSA_VALIDATE_FAMILY",
    validate_text,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "observations <- NULL",
    validate_text,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "readjsa_raw_cache_hits",
    validate_text,
    fixed = TRUE
  )))
  expect_false(any(grepl(
    "ivi <- read_ivi",
    validate_text,
    fixed = TRUE
  )))
})

test_that("measurement policy behavior rejects false cold and warm evidence", {
  cache_policy <- file.path(
    readjsa_source_root(),
    "scripts",
    "measurement-cache-policy.R"
  )
  policy <- new.env(parent = baseenv())
  sys.source(cache_policy, envir = policy)

  cold_miss <- list(
    raw_cache_releases = 1L,
    raw_cache_hits = 0L,
    raw_cache_misses = 1L
  )
  warm_hit <- list(
    raw_cache_releases = 1L,
    raw_cache_hits = 1L,
    raw_cache_misses = 0L
  )
  expect_invisible(policy$assert_measurement_cache_outcome(
    "cold",
    cold_miss
  ))
  expect_invisible(policy$assert_measurement_cache_outcome(
    "warm",
    warm_hit
  ))
  expect_error(
    policy$assert_measurement_cache_outcome("cold", warm_hit),
    "cold measurement requires exactly one observed raw-cache miss",
    fixed = TRUE
  )
  expect_error(
    policy$assert_measurement_cache_outcome("warm", cold_miss),
    "warm measurement requires exactly one observed raw-cache hit",
    fixed = TRUE
  )
  expect_error(
    policy$assert_measurement_cache_outcome(
      "warm",
      list(raw_cache_releases = 1L)
    ),
    "omitted observed raw-cache counts",
    fixed = TRUE
  )
  expect_error(
    policy$assert_measurement_cache_outcome(
      "warm",
      list(
        raw_cache_releases = 2L,
        raw_cache_hits = 1L,
        raw_cache_misses = 1L
      )
    ),
    "warm measurement requires exactly one observed raw-cache hit",
    fixed = TRUE
  )

  audit_name <- "READJSA_MEASUREMENT_CACHE_AUDIT"
  old_audit <- Sys.getenv(audit_name, unset = NA_character_)
  on.exit({
    if (is.na(old_audit)) {
      Sys.unsetenv(audit_name)
    } else {
      Sys.setenv(READJSA_MEASUREMENT_CACHE_AUDIT = old_audit)
    }
  }, add = TRUE)
  Sys.setenv(READJSA_MEASUREMENT_CACHE_AUDIT = "outer")
  expect_identical(
    policy$with_measurement_cache_audit({
      expect_identical(
        Sys.getenv("READJSA_MEASUREMENT_CACHE_AUDIT"),
        "true"
      )
      "scoped"
    }),
    "scoped"
  )
  expect_identical(
    Sys.getenv("READJSA_MEASUREMENT_CACHE_AUDIT"),
    "outer"
  )
  expect_error(
    policy$with_measurement_cache_audit(stop("injected failure")),
    "injected failure",
    fixed = TRUE
  )
  expect_identical(
    Sys.getenv("READJSA_MEASUREMENT_CACHE_AUDIT"),
    "outer"
  )
})

test_that("pre-remediation retained-cache profile covers every exact family", {
  evidence_path <- readjsa_validation_file(
    "performance-diagnostic-before.csv"
  )
  expect_true(file.exists(evidence_path))
  evidence <- utils::read.csv(
    evidence_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  expect_equal(nrow(evidence), 9L)
  expect_setequal(
    evidence$family_id,
    jsa_release_manifest()$family_id
  )
  expect_true(all(
    evidence$evidence_class ==
      "diagnostic_retained_cache_single_sample_not_benchmark"
  ))
  expect_equal(sum(evidence$observations), 3072999)
  expect_equal(
    evidence$observations[
      evidence$family_id == "anzsco4_states"
    ],
    788364
  )
  expect_equal(
    evidence$observations[
      evidence$family_id == "anzsco2_ivi_regions"
    ],
    818292
  )
  expect_gt(max(evidence$peak_rss_mib), 1024)
})

test_that("post-remediation profile preserves outputs and remaining block", {
  before <- utils::read.csv(
    readjsa_validation_file("performance-diagnostic-before.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  after <- utils::read.csv(
    readjsa_validation_file("performance-diagnostic-after.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  order_before <- order(before$family_id)
  order_after <- order(after$family_id)
  before <- before[order_before, , drop = FALSE]
  after <- after[order_after, , drop = FALSE]

  parity_columns <- c(
    "dataset_id", "family_id", "raw_bytes", "raw_sha256",
    "observations", "columns", "returned_object_mib",
    "first_observation", "last_observation", "evidence_class"
  )
  expect_identical(after[parity_columns], before[parity_columns])
  expect_equal(sum(after$observations), 3072999)
  expect_gt(max(after$peak_rss_mib), 1024)
  expect_lt(max(after$peak_rss_mib), max(before$peak_rss_mib))
  expect_lt(
    after$peak_rss_mib[after$family_id == "anzsco4_states"],
    before$peak_rss_mib[before$family_id == "anzsco4_states"]
  )
  expect_lt(
    after$peak_rss_mib[after$family_id == "anzsco2_ivi_regions"],
    before$peak_rss_mib[before$family_id == "anzsco2_ivi_regions"]
  )
})

test_that("committed quiet-slot evidence is exact, complete, and recomputable", {
  raw_path <- readjsa_validation_file("quiet-slot-performance-raw.csv")
  summary_path <- readjsa_validation_file(
    "quiet-slot-performance-summary.csv"
  )
  expect_true(file.exists(raw_path))
  expect_true(file.exists(summary_path))
  expect_identical(
    digest::digest(
      raw_path,
      file = TRUE,
      algo = "sha256",
      serialize = FALSE
    ),
    "cc302deb1d75dd0ec2312f9306804750bbb247931630f3bb3db1cf499ba29a40"
  )
  expect_identical(
    digest::digest(
      summary_path,
      file = TRUE,
      algo = "sha256",
      serialize = FALSE
    ),
    "f0d8e3752d2c4d1e89b7be8ec961ecb1567af8ee3fe9a5e2065fb586ce9ce32e"
  )

  raw <- utils::read.csv(
    raw_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  summary <- utils::read.csv(
    summary_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  expect_equal(nrow(raw), 54L)
  expect_equal(nrow(summary), 18L)
  expect_true(all(raw$status == "ok"))
  counts <- stats::xtabs(~ family_id + mode, data = raw)
  expect_identical(unname(dim(counts)), c(9L, 2L))
  expect_true(all(counts == 3L))

  cold <- raw[raw$mode == "cold", , drop = FALSE]
  warm <- raw[raw$mode == "warm", , drop = FALSE]
  expect_true(all(
    cold$raw_cache_releases == 1L &
      cold$raw_cache_hits == 0L &
      cold$raw_cache_misses == 1L
  ))
  expect_true(all(
    warm$raw_cache_releases == 1L &
      warm$raw_cache_hits == 1L &
      warm$raw_cache_misses == 0L
  ))
  expect_true(all(
    cold$cache_policy == "independent_empty_lane_first_read_of_family"
  ))
  expect_true(all(
    warm$cache_policy == "same_lane_verified_raw_cache_hit"
  ))
  expect_true(all(
    raw$process_policy ==
      "serialized_fresh_process_per_family_mode_repetition"
  ))

  observations_by_family <- split(raw$observations, raw$family_id)
  expect_true(all(vapply(
    observations_by_family,
    function(value) length(unique(value)) == 1L,
    logical(1)
  )))
  expect_equal(sum(vapply(
    observations_by_family,
    function(value) unique(value)[[1L]],
    numeric(1)
  )), 3072999)

  successful <- raw[raw$status == "ok", , drop = FALSE]
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
  recomputed <- do.call(rbind, lapply(groups, function(group) {
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
  row.names(recomputed) <- NULL
  expect_identical(summary, recomputed)

  expect_equal(max(raw$elapsed_seconds), 4.369)
  expect_equal(max(raw$peak_rss_mib), 1438.3125)
  expect_equal(max(raw$peak_increment_mib), 1349.046875)
  expect_equal(max(raw$projected_deployment_peak_mib), 1765.046875)
  latency_failures <- unique(
    summary$family_id[summary$p95_seconds > 1.5]
  )
  memory_failures <- unique(summary$family_id[
    summary$max_projected_deployment_peak_mib > 1024
  ])
  expect_setequal(
    latency_failures,
    c(
      "anzsco2_gccsa_sa4",
      "anzsco2_ivi_regions",
      "anzsco2_states",
      "anzsco4_states",
      "salm_sa2"
    )
  )
  expect_setequal(
    memory_failures,
    c(
      "anzsco2_ivi_regions",
      "anzsco2_states",
      "anzsco4_states",
      "salm_sa2"
    )
  )
  expect_equal(sum(
    summary$p95_seconds > 1.5 |
      summary$max_projected_deployment_peak_mib > 1024
  ), 10L)

  expect_true(all(grepl(
    "^logs/[^/]+[.]log$",
    raw$log_path
  )))
  expect_true(all(grepl(
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9:]{8}Z$",
    raw$recorded_at_utc
  )))
  evidence_text <- c(
    readLines(raw_path, warn = FALSE),
    readLines(summary_path, warn = FALSE)
  )
  expect_false(any(grepl(
    "/private/|/Users/|readjsa-benchmark-run|/tmp/",
    evidence_text
  )))
})

test_that("committed performance ledger admits personal use without hiding failures", {
  status <- utils::read.csv(
    readjsa_validation_file("performance-status.csv"),
    stringsAsFactors = FALSE
  )
  expect_identical(
    status$status[status$gate == "overall"],
    "ACCEPTED_PERSONAL_SOURCE_SPECIFIC_WITH_MEMORY_WARNING"
  )
  expect_identical(
    status$status[status$gate == "quiet_latency_rss_matrix"],
    "MEASURED_PERSONAL_USE_ACCEPTED_WITH_WARNING"
  )
  expect_identical(
    status$status[status$gate == "quiet_latency_p95"],
    "FAIL_ACCEPTED_PERSONAL_USE"
  )
  expect_identical(
    status$status[status$gate == "quiet_deployment_memory"],
    "FAIL_ACCEPTED_PERSONAL_USE"
  )
  status_text <- paste(status$evidence, status$next_action, collapse = "\n")
  expect_match(
    status_text,
    "cff01b2b0709330b034ba74cbbd2a5f91d0bfb34423e3c00fc3c0a3bfe4dfb8c",
    fixed = TRUE
  )
  expect_match(
    status_text,
    "cc302deb1d75dd0ec2312f9306804750bbb247931630f3bb3db1cf499ba29a40",
    fixed = TRUE
  )
  expect_match(status_text, "all four memory-failing families", fixed = TRUE)
  expect_match(
    status_text,
    "all five latency-failing families",
    fixed = TRUE
  )
  expect_match(status_text, "exclude from default All", fixed = TRUE)
  expect_match(status_text, "streaming", fixed = TRUE)

  package_root <- readjsa_source_root()
  render_script <- file.path(
    package_root,
    "scripts",
    "render-stage3-evidence.R"
  )
  render_text <- readLines(render_script, warn = FALSE)
  expect_true(any(grepl(
    "ACCEPTED_PERSONAL_SOURCE_SPECIFIC_WITH_MEMORY_WARNING",
    render_text,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "non-benchmark evidence",
    render_text,
    fixed = TRUE
  )))
})
