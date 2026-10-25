#' Default raw release cache
#'
#' @return A platform-appropriate user cache directory.
#' @export
jsa_cache_dir <- function() {
  tools::R_user_dir("readjsa", which = "cache")
}

.official_hosts <- c(
  "www.jobsandskills.gov.au", "jobsandskills.gov.au",
  "www.dewr.gov.au", "dewr.gov.au"
)

.validate_official_url <- function(url, context = "Source URL") {
  if (length(url) != 1L || is.na(url) || !nzchar(trimws(url))) {
    .abort(context, " must be one non-empty URL.", class = "readjsa_url_error")
  }
  parsed <- tryCatch(
    curl::curl_parse_url(url),
    error = function(error) {
      .abort(
        context, " is invalid: ", conditionMessage(error),
        class = "readjsa_url_error"
      )
    }
  )
  if (
    !identical(tolower(parsed$scheme), "https") ||
      !tolower(parsed$host) %in% .official_hosts ||
      (!is.null(parsed$port) && !identical(parsed$port, "443")) ||
      !is.null(parsed$user) ||
      !is.null(parsed$password)
  ) {
    .abort(
      context, " must be HTTPS on an official JSA/DEWR host: ", url,
      class = "readjsa_url_error"
    )
  }
  invisible(url)
}

.validate_release <- function(release) {
  if (is.data.frame(release)) {
    if (nrow(release) != 1L) {
      .abort("`release` must contain exactly one catalogue row.")
    }
    release <- as.list(release[1L, , drop = FALSE])
    release <- lapply(release, function(x) x[[1L]])
  }
  if (!is.list(release)) .abort("`release` must be a catalogue row or list.")
  required <- c(
    "provider_id", "dataset_id", "family_id", "release_id",
    "reference_period", "release_date", "vintage_date", "page_modified_date",
    "source_url", "source_filename", "page_url", "terms_url", "licence",
    "attribution"
  )
  allowed <- required
  if (anyDuplicated(names(release))) {
    .abort("Release metadata field names must be unique.")
  }
  unknown <- setdiff(names(release), allowed)
  if (length(unknown)) {
    .abort(
      "Unsupported release metadata field(s): ",
      paste(unknown, collapse = ", "),
      class = "readjsa_provenance_error"
    )
  }
  missing <- setdiff(required, names(release))
  if (length(missing)) {
    .abort("Release metadata missing: ", paste(missing, collapse = ", "))
  }
  scalar <- vapply(release[required], length, integer(1)) == 1L
  if (!all(scalar)) {
    .abort(
      "Release identity fields must be scalar.",
      class = "readjsa_provenance_error"
    )
  }
  date_fields <- c(
    "reference_period", "release_date", "vintage_date",
    "page_modified_date"
  )
  release$reference_period <- .canonical_date(
    release$reference_period, "reference_period", allow_missing = FALSE
  )
  for (name in setdiff(date_fields, "reference_period")) {
    release[[name]] <- .canonical_date(release[[name]], name)
  }
  text_fields <- setdiff(required, date_fields)
  for (name in text_fields) {
    if (!is.na(release[[name]])) {
      release[[name]] <- as.character(release[[name]])
    } else {
      release[[name]] <- NA_character_
    }
  }
  dataset <- as.character(release$dataset_id)
  family <- as.character(release$family_id)
  manifest <- jsa_release_manifest()
  manifest_hit <- manifest$dataset_id == dataset &
    manifest$family_id == family
  if (sum(manifest_hit) != 1L) {
    .abort(
      "Release dataset/family is unsupported: ", dataset, "/", family, ".",
      class = "readjsa_provenance_error"
    )
  }
  expected_provider <- if (identical(dataset, "salm")) "dewr" else
    "jobs_skills_australia"
  expected_terms <- unname(.jsa_terms[expected_provider])
  if (
    !identical(as.character(release$provider_id), expected_provider) ||
      !identical(as.character(release$terms_url), expected_terms) ||
      !identical(
        as.character(release$licence),
        "CC BY 4.0 subject to stated exclusions"
      ) ||
      !identical(
        as.character(release$attribution),
        "\u00a9 Commonwealth of Australia"
      )
  ) {
    .abort(
      "Release provider, terms, or attribution conflicts with the manifest.",
      class = "readjsa_provenance_error"
    )
  }
  if (
    anyNA(release[c("release_id", "source_url", "source_filename")]) ||
      any(!nzchar(trimws(vapply(
        release[c("release_id", "source_url", "source_filename")],
        as.character,
        character(1)
      ))))
  ) {
    .abort("Release identity fields cannot be blank.")
  }
  expected_prefix <- paste0(dataset, "/", family, "/")
  if (!startsWith(as.character(release$release_id), expected_prefix)) {
    .abort(
      "Release ID does not identify its dataset/family.",
      class = "readjsa_provenance_error"
    )
  }
  reference_period <- release$reference_period
  release_date <- release$release_date
  vintage_date <- release$vintage_date
  if (
    is.na(reference_period) ||
      !identical(as.character(release_date), as.character(vintage_date))
  ) {
    .abort(
      "Release period is missing or release/vintage dates conflict.",
      class = "readjsa_provenance_error"
    )
  }
  .validate_official_url(as.character(release$source_url))
  .validate_official_url(as.character(release$page_url), "Publication page URL")
  release
}

.default_transport <- function(url, destination, timeout, max_bytes) {
  .validate_official_url(url)
  handle <- curl::new_handle()
  curl::handle_setopt(
    handle,
    timeout = timeout,
    connecttimeout = min(timeout, 10),
    maxfilesize_large = as.numeric(max_bytes),
    followlocation = TRUE,
    protocols_str = "https",
    redir_protocols_str = "https",
    useragent = "readjsa/0.1.0 (+official public data client)"
  )
  response <- curl::curl_fetch_disk(url, destination, handle = handle)
  if (response$status_code < 200L || response$status_code >= 300L) {
    .abort(
      "Raw release returned HTTP ", response$status_code, ": ", url,
      class = "readjsa_transport_error"
    )
  }
  .validate_official_url(response$url, "Redirect target")
  size <- file.info(destination)$size
  if (is.na(size) || size > max_bytes) {
    unlink(destination)
    .abort(
      "Raw release exceeded ", max_bytes, " bytes.",
      class = "readjsa_size_error"
    )
  }
  invisible(response$url)
}

.run_transport <- function(
    transport, url, destination, timeout, max_bytes, retries) {
  last_error <- NULL
  for (attempt in seq_len(retries + 1L)) {
    unlink(destination)
    transport_result <- NULL
    last_error <- tryCatch(
      {
        transport_result <- transport(url, destination, timeout, max_bytes)
        NULL
      },
      error = function(error) error
    )
    if (is.null(last_error)) return(transport_result)
    if (attempt <= retries) {
      Sys.sleep(min(0.1 * (2 ^ (attempt - 1L)), 0.5))
    }
  }
  stop(last_error)
}

.write_rds_atomic <- function(object, path) {
  temporary <- tempfile("readjsa-meta-", dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(object, temporary, version = 3)
  if (!file.rename(temporary, path)) {
    .abort("Could not atomically store cache metadata.")
  }
  invisible(path)
}

.cache_identity <- function(
    release,
    parser_version = .readjsa_version,
    schema_version = .readjsa_schema_version) {
  fields <- c(
    "provider_id", "dataset_id", "family_id", "release_id",
    "reference_period", "release_date", "vintage_date", "page_modified_date",
    "source_url", "source_filename", "page_url", "terms_url",
    "licence", "attribution"
  )
  values <- vapply(fields, function(name) {
    value <- release[[name]]
    if (is.null(value) || length(value) == 0L || is.na(value[[1L]])) {
      return("<NA>")
    }
    as.character(value[[1L]])
  }, character(1))
  paste(
    c(fields, "parser_version", "schema_version"),
    c(values, parser_version, schema_version),
    sep = "=", collapse = "\r"
  )
}

.acquire_cache_lock <- function(path, timeout) {
  started <- Sys.time()
  repeat {
    if (dir.create(path, showWarnings = FALSE)) return(invisible(path))
    info <- file.info(path)
    age <- as.numeric(difftime(Sys.time(), info$mtime, units = "secs"))
    if (!is.na(age) && age > max(60, timeout * 5)) {
      unlink(path, recursive = TRUE)
      next
    }
    if (as.numeric(difftime(Sys.time(), started, units = "secs")) >= timeout) {
      .abort(
        "Timed out waiting for the release cache lock.",
        class = "readjsa_cache_error"
      )
    }
    Sys.sleep(0.05)
  }
}

.read_cache_metadata <- function(path) {
  tryCatch(
    readRDS(path),
    error = function(error) {
      .abort(
        "Cache metadata is unreadable: ", conditionMessage(error),
        ". Use force=TRUE to replace it.",
        class = "readjsa_cache_error"
      )
    }
  )
}

.cache_entry_status <- function(
    metadata, release, path, key, max_bytes, max_age) {
  required <- c(
    "cache_key", "cache_identity", "raw_sha256", "raw_bytes",
    "retrieved_at", "final_url", "parser_version", "schema_version"
  )
  if (!is.list(metadata) || length(setdiff(required, names(metadata)))) {
    return("metadata schema changed")
  }
  identity <- .cache_identity(release)
  if (
    !identical(metadata$cache_key, key) ||
      !identical(metadata$cache_identity, identity)
  ) {
    return("release identity changed")
  }
  size <- unname(file.info(path)$size)
  if (is.na(size) || size <= 0 || size > max_bytes) {
    return("raw byte bound failed")
  }
  hash <- digest::digest(
    path, algo = "sha256", file = TRUE, serialize = FALSE
  )
  if (
    !identical(metadata$raw_sha256, hash) ||
      !identical(as.numeric(metadata$raw_bytes), as.numeric(size))
  ) {
    return("raw bytes no longer match metadata")
  }
  if (
    !identical(metadata$parser_version, .readjsa_version) ||
      !identical(metadata$schema_version, .readjsa_schema_version)
  ) {
    return("parser/schema version changed")
  }
  valid_final_url <- tryCatch(
    {
      .validate_official_url(metadata$final_url, "Final source URL")
      TRUE
    },
    error = function(error) FALSE
  )
  if (!valid_final_url) return("final source URL is not official")
  retrieved_at <- as.POSIXct(metadata$retrieved_at, tz = "UTC")
  age <- as.numeric(difftime(Sys.time(), retrieved_at, units = "secs"))
  if (is.na(age) || age < -60 || age > max_age) {
    return("cache age bound failed")
  }
  NA_character_
}

.prune_cache <- function(
    cache_dir, keep_path, max_files, max_cache_bytes, max_age) {
  raw_files <- list.files(
    cache_dir,
    pattern = "^[0-9a-f]{64}\\.(xlsx|csv)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  raw_files <- raw_files[file.exists(raw_files)]
  metadata_files <- list.files(
    cache_dir,
    pattern = "^[0-9a-f]{64}\\.(xlsx|csv)\\.rds$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  orphan_metadata <- metadata_files[
    !sub("\\.rds$", "", metadata_files) %in% raw_files
  ]
  if (length(orphan_metadata)) unlink(orphan_metadata)
  lock_paths <- list.files(
    cache_dir,
    pattern = "^[0-9a-f]{64}\\.(xlsx|csv)\\.lock$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(lock_paths)) {
    lock_age <- as.numeric(difftime(
      Sys.time(), file.info(lock_paths)$mtime, units = "secs"
    ))
    stale_locks <- lock_paths[!is.na(lock_age) & lock_age > max(60, max_age)]
    if (length(stale_locks)) unlink(stale_locks, recursive = TRUE)
  }
  if (!length(raw_files)) return(invisible(NULL))
  info <- file.info(raw_files)
  age <- as.numeric(difftime(Sys.time(), info$mtime, units = "secs"))
  expired <- raw_files[!is.na(age) & age > max_age & raw_files != keep_path]
  for (path in expired) {
    unlink(c(path, paste0(path, ".rds")))
  }
  raw_files <- setdiff(raw_files, expired)
  pair_bytes <- vapply(raw_files, function(path) {
    pair <- c(path, paste0(path, ".rds"))
    sum(file.info(pair[file.exists(pair)])$size, na.rm = TRUE)
  }, numeric(1))
  modified <- file.info(raw_files)$mtime
  removable <- raw_files[order(modified)]
  removable <- setdiff(removable, keep_path)
  while (
    length(raw_files) > max_files ||
      sum(pair_bytes[raw_files], na.rm = TRUE) > max_cache_bytes
  ) {
    if (!length(removable)) {
      unlink(c(keep_path, paste0(keep_path, ".rds")))
      .abort(
        "The retained release alone exceeds the cache bounds.",
        class = "readjsa_size_error"
      )
    }
    path <- removable[[1L]]
    removable <- removable[-1L]
    unlink(c(path, paste0(path, ".rds")))
    raw_files <- setdiff(raw_files, path)
  }
  invisible(NULL)
}

#' Fetch and verify one official raw release
#'
#' Downloads to a temporary file under a per-key writer lock, enforces transport
#' and cache bounds, hashes the exact raw bytes, and promotes the verified pair
#' into the cache. Cache hits are identity-checked and re-hashed before use.
#'
#' @param release One row returned by [discover_jsa_releases()].
#' @param cache_dir Raw-file cache directory.
#' @param force Ignore a valid cache entry.
#' @param timeout Wall-clock transport timeout in seconds.
#' @param max_bytes Maximum accepted raw-file size.
#' @param max_age Maximum cache-hit age in seconds.
#' @param max_files Maximum number of retained raw releases.
#' @param max_cache_bytes Maximum total bytes retained in the raw cache.
#' @param retries Number of retries after the first transport attempt.
#' @param lock_timeout Maximum seconds to wait for another writer of this key.
#' @param transport Optional test transport with signature
#'   `(url, destination, timeout, max_bytes)`.
#' @return A snapshot list containing `path` and exact provenance.
#' @export
fetch_jsa_release <- function(
    release,
    cache_dir = jsa_cache_dir(),
    force = FALSE,
    timeout = 60,
    max_bytes = 100 * 1024^2,
    max_age = 7 * 24 * 60^2,
    max_files = 100L,
    max_cache_bytes = 500 * 1024^2,
    retries = 2L,
    lock_timeout = 10,
    transport = .default_transport) {
  release <- .validate_release(release)
  if (
    length(force) != 1L || is.na(force) || !is.logical(force) ||
      !is.function(transport)
  ) {
    .abort("`force` must be logical and `transport` must be a function.")
  }
  bounds <- list(
    timeout = timeout, max_bytes = max_bytes, max_age = max_age,
    max_files = max_files, max_cache_bytes = max_cache_bytes,
    retries = retries, lock_timeout = lock_timeout
  )
  scalar <- vapply(bounds, length, integer(1)) == 1L
  if (!all(scalar)) {
    .abort("Transport and cache bounds must be scalar.")
  }
  numeric_args <- vapply(bounds, as.numeric, numeric(1))
  if (
    anyNA(numeric_args) ||
      any(!is.finite(numeric_args)) ||
      any(numeric_args[names(numeric_args) != "retries"] <= 0) ||
      retries < 0 ||
      retries != floor(retries) ||
      max_files != floor(max_files)
  ) {
    .abort("Transport and cache bounds must be positive finite scalars.")
  }
  if (
    length(cache_dir) != 1L ||
      is.na(cache_dir) ||
      !is.character(cache_dir) ||
      !nzchar(trimws(cache_dir))
  ) {
    .abort("`cache_dir` must be one non-empty path.")
  }
  retries <- as.integer(retries)
  max_files <- as.integer(max_files)
  if (max_bytes > max_cache_bytes) {
    .abort("`max_bytes` cannot exceed `max_cache_bytes`.")
  }
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(cache_dir)) .abort("Could not create cache directory.")
  extension <- tolower(tools::file_ext(release$source_filename))
  if (!extension %in% c("xlsx", "csv")) {
    extension <- if (grepl("/csv(?:$|[?#])", release$source_url)) "csv" else
      extension
  }
  if (!extension %in% c("xlsx", "csv")) {
    .abort("Unsupported raw file extension: ", extension)
  }
  identity <- .cache_identity(release)
  key <- digest::digest(
    identity,
    algo = "sha256", serialize = FALSE
  )
  path <- file.path(cache_dir, paste0(key, ".", extension))
  metadata_path <- paste0(path, ".rds")
  lock_path <- paste0(path, ".lock")
  .acquire_cache_lock(lock_path, lock_timeout)
  on.exit(unlink(lock_path, recursive = TRUE), add = TRUE)
  valid_cache <- FALSE
  if (!force && file.exists(path) && file.exists(metadata_path)) {
    metadata <- .read_cache_metadata(metadata_path)
    invalid_reason <- .cache_entry_status(
      metadata, release, path, key, max_bytes, max_age
    )
    valid_cache <- is.na(invalid_reason)
    if (
      !valid_cache &&
        !invalid_reason %in% c("raw byte bound failed", "cache age bound failed")
    ) {
      warning(
        "Invalid cached release (", invalid_reason, "); downloading it again.",
        call. = FALSE
      )
    }
  }
  if (!valid_cache) {
    temporary <- tempfile("readjsa-raw-", cache_dir, fileext = paste0(".", extension))
    on.exit(unlink(temporary), add = TRUE)
    transport_result <- .run_transport(
      transport, release$source_url, temporary, timeout, max_bytes, retries
    )
    final_url <- if (
      is.character(transport_result) &&
        length(transport_result) == 1L &&
        !is.na(transport_result) &&
        grepl("^https://", transport_result, ignore.case = TRUE)
    ) {
      transport_result
    } else {
      release$source_url
    }
    .validate_official_url(final_url, "Final source URL")
    if (!file.exists(temporary)) {
      .abort("Transport did not create a file.", class = "readjsa_transport_error")
    }
    bytes <- file.info(temporary)$size
    if (is.na(bytes) || bytes <= 0 || bytes > max_bytes) {
      .abort("Downloaded release violated the byte bound.",
        class = "readjsa_size_error"
      )
    }
    raw_sha256 <- digest::digest(
      temporary, algo = "sha256", file = TRUE, serialize = FALSE
    )
    retrieved_at <- as.POSIXct(Sys.time(), tz = "UTC")
    metadata <- c(release, list(
      cache_key = key,
      cache_identity = identity,
      raw_bytes = unname(bytes),
      raw_sha256 = raw_sha256,
      retrieved_at = retrieved_at,
      final_url = final_url,
      parser_version = .readjsa_version,
      schema_version = .readjsa_schema_version
    ))
    if (!file.rename(temporary, path)) {
      .abort("Could not atomically promote the downloaded release.")
    }
    .write_rds_atomic(metadata, metadata_path)
    stored <- .read_cache_metadata(metadata_path)
    invalid_reason <- .cache_entry_status(
      stored, release, path, key, max_bytes, max_age
    )
    if (!is.na(invalid_reason)) {
      .abort(
        "Stored cache pair failed final verification: ", invalid_reason, ".",
        class = "readjsa_cache_error"
      )
    }
    metadata <- stored
  }
  .prune_cache(cache_dir, path, max_files, max_cache_bytes, max_age)
  metadata$path <- path
  metadata$cache_hit <- valid_cache
  metadata
}

.snapshot_provenance <- function(snapshot) {
  cache_hit <- snapshot$cache_hit
  if (length(cache_hit) != 1L || !is.logical(cache_hit) || is.na(cache_hit)) {
    .abort(
      "Release snapshot did not retain one observed raw-cache outcome.",
      class = "readjsa_cache_error"
    )
  }
  provenance <- snapshot[intersect(names(snapshot), c(
    "provider_id", "dataset_id", "family_id", "release_id",
    "reference_period", "release_date", "vintage_date", "page_modified_date",
    "retrieved_at", "source_url", "final_url", "source_filename", "page_url",
    "raw_bytes", "raw_sha256", "parser_version", "schema_version",
    "terms_url", "licence", "attribution"
  ))]
  attr(provenance, "readjsa_raw_cache_hit") <- cache_hit
  provenance
}

.attach_raw_cache_audit <- function(data, provenances) {
  audit_enabled <- identical(
    tolower(trimws(Sys.getenv(
      "READJSA_MEASUREMENT_CACHE_AUDIT",
      unset = "false"
    ))),
    "true"
  )
  if (!audit_enabled) return(data)
  hits <- vapply(provenances, function(provenance) {
    hit <- attr(provenance, "readjsa_raw_cache_hit", exact = TRUE)
    if (length(hit) != 1L || !is.logical(hit) || is.na(hit)) {
      .abort(
        "Release provenance did not retain one observed raw-cache outcome.",
        class = "readjsa_cache_error"
      )
    }
    hit
  }, logical(1))
  attr(data, "readjsa_raw_cache_hits") <- unname(hits)
  data
}
