#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 2L) {
  stop(
    "Worker requires one metadata path and one result path.",
    call. = FALSE
  )
}
metadata_path <- arguments[[1L]]
result_path <- arguments[[2L]]

required_packages <- c("digest", "pkgload", "ps")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    sprintf(
      "Retained-cache worker requires: %s.",
      paste(missing_packages, collapse = ", ")
    ),
    call. = FALSE
  )
}

pkgload::load_all(".", quiet = TRUE)
metadata <- readRDS(metadata_path)
path <- sub("\\.rds$", "", metadata_path)
actual_hash <- digest::digest(
  path,
  algo = "sha256",
  file = TRUE,
  serialize = FALSE
)
if (!identical(actual_hash, metadata$raw_sha256)) {
  stop("Retained raw file hash does not match metadata.", call. = FALSE)
}

process <- ps::ps_handle()
gc()
rss_before_parse_mib <-
  as.numeric(ps::ps_memory_info(process)[["rss"]]) / 1024^2
started <- proc.time()[["elapsed"]]
provenance <- readjsa:::.snapshot_provenance(metadata)
data <- switch(
  metadata$dataset_id,
  ivi = readjsa::read_ivi_file(
    path,
    family = metadata$family_id,
    provenance = provenance
  ),
  reos = readjsa::read_reos_file(
    path,
    tables = "all",
    provenance = provenance
  ),
  salm = readjsa::read_salm_file(
    path,
    geography = sub("^salm_", "", metadata$family_id),
    provenance = provenance
  ),
  stop("Unsupported retained dataset.", call. = FALSE)
)
elapsed_seconds <- proc.time()[["elapsed"]] - started
rss_after_parse_mib <-
  as.numeric(ps::ps_memory_info(process)[["rss"]]) / 1024^2

result <- data.frame(
  dataset_id = metadata$dataset_id,
  family_id = metadata$family_id,
  raw_bytes = metadata$raw_bytes,
  raw_sha256 = metadata$raw_sha256,
  observations = nrow(data),
  columns = ncol(data),
  returned_object_mib = as.numeric(object.size(data)) / 1024^2,
  first_observation = as.character(min(data$date)),
  last_observation = as.character(max(data$date)),
  elapsed_seconds = elapsed_seconds,
  rss_before_parse_mib = rss_before_parse_mib,
  rss_after_parse_mib = rss_after_parse_mib,
  stringsAsFactors = FALSE
)
saveRDS(result, result_path, version = 3)
