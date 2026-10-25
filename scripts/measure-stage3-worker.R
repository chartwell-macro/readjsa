#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 2L) {
  stop("Worker requires one family ID and one result path.", call. = FALSE)
}
family_id <- arguments[[1L]]
result_path <- arguments[[2L]]

required_packages <- c("pkgload", "ps")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    sprintf(
      "Measurement worker requires: %s.",
      paste(missing_packages, collapse = ", ")
    ),
    call. = FALSE
  )
}
pkgload::load_all(".", quiet = TRUE)
invisible(gc(FALSE))
rss_before_read_mib <- as.numeric(
  ps::ps_memory_info(ps::ps_handle())[["rss"]]
) / 1024^2
Sys.setenv(
  READJSA_LIVE = "true",
  READJSA_VALIDATE_FAMILY = family_id,
  READJSA_MEASUREMENT_CACHE_AUDIT = "true"
)
validation_environment <- new.env(parent = globalenv())
source(
  file.path("scripts", "validate-live.R"),
  local = validation_environment
)
result <- validation_environment$validation_result
if (!is.list(result) ||
    nrow(result$summary) != 1L ||
    !identical(result$summary$family_id[[1L]], family_id)) {
  stop("Measurement worker did not return exactly one family.", call. = FALSE)
}
saveRDS(
  list(
    observations = result$summary$observations[[1L]],
    returned_object_mib = result$summary$returned_object_mib[[1L]],
    rss_before_read_mib = rss_before_read_mib,
    raw_cache_releases = result$summary$raw_cache_releases[[1L]],
    raw_cache_hits = result$summary$raw_cache_hits[[1L]],
    raw_cache_misses = result$summary$raw_cache_misses[[1L]]
  ),
  result_path,
  version = 3
)
