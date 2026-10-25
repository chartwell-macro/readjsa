if (!identical(tolower(Sys.getenv("READJSA_LIVE")), "true")) {
  stop("Set READJSA_LIVE=true to opt in to official live-source validation.")
}

library(readjsa)

manifest <- jsa_release_manifest()
selected_family <- trimws(Sys.getenv(
  "READJSA_VALIDATE_FAMILY",
  unset = ""
))
if (nzchar(selected_family)) {
  if (!selected_family %in% manifest$family_id) {
    stop(
      sprintf("Unknown READJSA_VALIDATE_FAMILY: %s.", selected_family),
      call. = FALSE
    )
  }
  manifest <- manifest[
    manifest$family_id == selected_family,
    ,
    drop = FALSE
  ]
}

datasets <- unique(manifest$dataset_id)
catalogues <- stats::setNames(
  lapply(datasets, discover_jsa_releases),
  datasets
)
cache_dir <- Sys.getenv("READJSA_CACHE_DIR", jsa_cache_dir())
cache_policy_helper <- file.path("scripts", "measurement-cache-policy.R")
if (!file.exists(cache_policy_helper)) {
  stop("Live validator raw-cache policy helper is missing.", call. = FALSE)
}
sys.source(cache_policy_helper, envir = environment())

validation_result <- with_measurement_cache_audit({

read_family <- function(dataset_id, family_id, catalogue) {
  switch(
    dataset_id,
    ivi = read_ivi(
      tables = family_id,
      release = catalogue,
      cache_dir = cache_dir
    ),
    reos = read_reos(
      tables = "all",
      release = catalogue,
      cache_dir = cache_dir
    ),
    salm = read_salm(
      geography = sub("^salm_", "", family_id),
      release = catalogue,
      cache_dir = cache_dir
    ),
    stop(sprintf("Unsupported readjsa dataset: %s.", dataset_id), call. = FALSE)
  )
}

summary_rows <- vector("list", nrow(manifest))
source_rows <- vector("list", nrow(manifest))
for (index in seq_len(nrow(manifest))) {
  dataset_id <- manifest$dataset_id[[index]]
  family_id <- manifest$family_id[[index]]
  observations <- read_family(
    dataset_id,
    family_id,
    catalogues[[dataset_id]]
  )
  if (!nrow(observations)) {
    stop(sprintf("%s returned no observations.", family_id), call. = FALSE)
  }
  raw_cache_outcomes <- attr(
    observations,
    "readjsa_raw_cache_hits",
    exact = TRUE
  )
  if (length(raw_cache_outcomes) != 1L ||
      !is.logical(raw_cache_outcomes) ||
      is.na(raw_cache_outcomes)) {
    stop(
      sprintf(
        "%s did not retain exactly one observed raw-cache outcome.",
        family_id
      ),
      call. = FALSE
    )
  }
  if (identical(dataset_id, "reos") &&
      !all(
        observations$value >= 0 & observations$value <= 1,
        na.rm = TRUE
      )) {
    stop("REOS values escaped the canonical [0, 1] range.", call. = FALSE)
  }
  hashes <- unique(observations$raw_sha256)
  final_urls <- unique(observations$final_url)
  if (length(hashes) != 1L || length(final_urls) != 1L) {
    stop(
      sprintf("%s did not retain one exact source identity.", family_id),
      call. = FALSE
    )
  }
  final_host <- tolower(curl::curl_parse_url(final_urls[[1L]])$host)
  if (!final_host %in% c(
    "www.jobsandskills.gov.au", "jobsandskills.gov.au",
    "www.dewr.gov.au", "dewr.gov.au"
  )) {
    stop(
      sprintf("%s redirected away from an official host.", family_id),
      call. = FALSE
    )
  }

  summary_rows[[index]] <- data.frame(
    dataset = dataset_id,
    family_id = family_id,
    observations = nrow(observations),
    source_files = length(hashes),
    raw_cache_releases = length(raw_cache_outcomes),
    raw_cache_hits = sum(raw_cache_outcomes),
    raw_cache_misses = sum(!raw_cache_outcomes),
    returned_object_mib = as.numeric(object.size(observations)) / 1024^2,
    first_observation = as.character(min(observations$date)),
    last_observation = as.character(max(observations$date)),
    stringsAsFactors = FALSE
  )
  source_rows[[index]] <- unique(observations[
    c("family_id", "source_url", "final_url", "raw_sha256")
  ])
  source_rows[[index]]$raw_cache_hit <- raw_cache_outcomes[[1L]]

  # Each family is an independent application route. Release the complete
  # observation object before parsing the next family so the validator does
  # not manufacture an all-nine-file working set that the app never retains.
  observations <- NULL
  invisible(gc(FALSE))
}

summary <- do.call(rbind, summary_rows)
sources <- do.call(rbind, source_rows)
if (nrow(sources) != nrow(manifest) ||
    length(unique(sources$raw_sha256)) != nrow(manifest)) {
  stop("Live validation did not preserve one unique raw file per route.")
}

print(summary, row.names = FALSE)
print(sources, row.names = FALSE)

list(
  summary = summary,
  sources = sources,
  selected_family = if (nzchar(selected_family)) selected_family else "all"
)
})
