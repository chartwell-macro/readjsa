with_measurement_cache_audit <- function(code) {
  old_audit <- Sys.getenv(
    "READJSA_MEASUREMENT_CACHE_AUDIT",
    unset = NA_character_
  )
  on.exit({
    if (is.na(old_audit)) {
      Sys.unsetenv("READJSA_MEASUREMENT_CACHE_AUDIT")
    } else {
      Sys.setenv(READJSA_MEASUREMENT_CACHE_AUDIT = old_audit)
    }
  }, add = TRUE)
  Sys.setenv(READJSA_MEASUREMENT_CACHE_AUDIT = "true")
  force(code)
}

assert_measurement_cache_outcome <- function(mode, worker_result) {
  if (length(mode) != 1L || is.na(mode) ||
      !mode %in% c("cold", "warm")) {
    stop("Measurement mode must be exactly `cold` or `warm`.", call. = FALSE)
  }
  if (!is.list(worker_result)) {
    stop("Measurement worker result must be a list.", call. = FALSE)
  }
  fields <- c(
    "raw_cache_releases",
    "raw_cache_hits",
    "raw_cache_misses"
  )
  if (!all(fields %in% names(worker_result))) {
    stop(
      paste(
        "Measurement worker result omitted observed raw-cache counts:",
        paste(setdiff(fields, names(worker_result)), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  observed <- vapply(fields, function(field) {
    value <- worker_result[[field]]
    if (length(value) != 1L || is.na(value) ||
        !is.numeric(value) || !is.finite(value) ||
        value < 0 || value != floor(value)) {
      stop(
        sprintf("Measurement worker `%s` must be one non-negative count.", field),
        call. = FALSE
      )
    }
    as.integer(value)
  }, integer(1))
  expected <- if (identical(mode, "cold")) {
    c(
      raw_cache_releases = 1L,
      raw_cache_hits = 0L,
      raw_cache_misses = 1L
    )
  } else {
    c(
      raw_cache_releases = 1L,
      raw_cache_hits = 1L,
      raw_cache_misses = 0L
    )
  }
  if (!identical(observed, expected)) {
    expected_outcome <- if (identical(mode, "cold")) {
      "exactly one observed raw-cache miss"
    } else {
      "exactly one observed raw-cache hit"
    }
    stop(
      sprintf(
        paste(
          "%s measurement requires %s;",
          "observed releases=%d, hits=%d, misses=%d."
        ),
        mode,
        expected_outcome,
        observed[["raw_cache_releases"]],
        observed[["raw_cache_hits"]],
        observed[["raw_cache_misses"]]
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}
