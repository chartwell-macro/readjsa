fixture <- function(name) {
  testthat::test_path("fixtures", name)
}

release_row <- function(
    dataset, family, filename, url = NULL,
    reference_period = as.Date("2026-06-01")) {
  if (is.null(url)) {
    host <- if (dataset == "salm") "www.dewr.gov.au" else
      "www.jobsandskills.gov.au"
    extension <- tools::file_ext(filename)
    tail <- if (nzchar(extension)) filename else paste0(filename, "/csv")
    url <- paste0("https://", host, "/downloads/", tail)
  }
  data.frame(
    provider_id = if (dataset == "salm") "dewr" else
      "jobs_skills_australia",
    dataset_id = dataset,
    family_id = family,
    release_id = paste(dataset, family, "2026-06", sep = "/"),
    reference_period = as.Date(reference_period),
    release_date = as.Date("2026-07-22"),
    vintage_date = as.Date("2026-07-22"),
    page_modified_date = as.Date("2026-07-22"),
    source_url = url,
    source_filename = filename,
    page_url = if (dataset == "salm") {
      "https://www.dewr.gov.au/employment-research/small-area-labour-markets"
    } else {
      "https://www.jobsandskills.gov.au/data/internet-vacancy-index"
    },
    terms_url = if (dataset == "salm") {
      "https://www.dewr.gov.au/copyright"
    } else {
      "https://www.jobsandskills.gov.au/copyright-and-disclaimer"
    },
    licence = "CC BY 4.0 subject to stated exclusions",
    attribution = "© Commonwealth of Australia",
    stringsAsFactors = FALSE
  )
}

copy_transport <- function(source) {
  force(source)
  function(url, destination, timeout, max_bytes) {
    if (!file.copy(source, destination, overwrite = TRUE)) {
      stop("fixture copy failed")
    }
    invisible(destination)
  }
}
