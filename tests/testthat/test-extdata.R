readjsa_extdata_names <- c(
  "ivi_skill_states.xlsx",
  "reos_current.xlsx",
  "salm_sa2.csv",
  "ivi.html",
  "reos.html",
  "salm.html",
  "salm_sa2_resource.html",
  "salm_lga_resource.html"
)

readjsa_extdata_path <- function(name) {
  installed <- system.file("extdata", name, package = "readjsa")
  if (nzchar(installed) && file.exists(installed)) {
    return(installed)
  }
  source <- testthat::test_path("..", "..", "inst", "extdata", name)
  if (!file.exists(source)) {
    stop("Could not resolve packaged fixture: ", name, call. = FALSE)
  }
  source
}

readjsa_official_ivi_name <- "ivi_skill_states_2026-06.xlsx"

readjsa_official_ivi_path <- function() {
  installed <- system.file(
    "extdata", "official", readjsa_official_ivi_name, package = "readjsa"
  )
  if (nzchar(installed) && file.exists(installed)) {
    return(installed)
  }
  source <- testthat::test_path(
    "..", "..", "inst", "extdata", "official", readjsa_official_ivi_name
  )
  if (!file.exists(source)) {
    stop("Could not resolve packaged official snapshot: ",
      readjsa_official_ivi_name, call. = FALSE
    )
  }
  source
}

test_that("packaged extdata resolves from an install or the source tree", {
  installed_paths <- system.file(
    "extdata", readjsa_extdata_names, package = "readjsa"
  )
  expect_true(all(nzchar(installed_paths)))
  expect_true(all(file.exists(installed_paths)))

  resolved <- vapply(
    readjsa_extdata_names,
    function(name) file.exists(readjsa_extdata_path(name)),
    logical(1)
  )
  expect_true(all(resolved))
})

test_that("packaged extdata preserves source-fixture bytes", {
  source_paths <- file.path(
    testthat::test_path("fixtures"),
    readjsa_extdata_names
  )
  expected <- vapply(
    source_paths,
    digest::digest,
    character(1),
    file = TRUE,
    algo = "sha256",
    serialize = FALSE
  )
  names(expected) <- readjsa_extdata_names
  actual <- vapply(
    readjsa_extdata_names,
    function(name) digest::digest(
      readjsa_extdata_path(name),
      file = TRUE,
      algo = "sha256",
      serialize = FALSE
    ),
    character(1)
  )
  expect_identical(actual, expected)
})

test_that("packaged extdata runs offline parser and discovery workflows", {
  ivi <- read_ivi_file(
    readjsa_extdata_path("ivi_skill_states.xlsx"),
    "skill_states"
  )
  reos <- read_reos_file(readjsa_extdata_path("reos_current.xlsx"))
  salm <- read_salm_file(
    readjsa_extdata_path("salm_sa2.csv"),
    "sa2"
  )
  expect_gt(nrow(ivi), 0L)
  expect_gt(nrow(reos), 0L)
  expect_gt(nrow(salm), 0L)

  ivi_releases <- discover_jsa_releases(
    "ivi",
    html = readjsa_extdata_path("ivi.html")
  )
  reos_releases <- discover_jsa_releases(
    "reos",
    html = readjsa_extdata_path("reos.html")
  )
  salm_releases <- discover_jsa_releases(
    "salm",
    html = readjsa_extdata_path("salm.html"),
    resource_html = list(
      salm_sa2 = readjsa_extdata_path("salm_sa2_resource.html"),
      salm_lga = readjsa_extdata_path("salm_lga_resource.html")
    )
  )
  expect_equal(nrow(ivi_releases), 6L)
  expect_equal(unique(reos_releases$family_id), "reos_tables")
  expect_setequal(salm_releases$family_id, c("salm_sa2", "salm_lga"))
})

test_that("packaged official IVI snapshot retains a verified parsed observation", {
  snapshot <- readjsa_official_ivi_path()

  expect_equal(unname(file.info(snapshot)$size), 440452)
  expect_identical(
    digest::digest(
      snapshot,
      file = TRUE,
      algo = "sha256",
      serialize = FALSE
    ),
    "5bacad1cf2fbef22b1f802e3fc3d8c5051038c31ff5839a2af7e8db10e8185fd"
  )

  parsed <- read_ivi_file(snapshot, "skill_states")
  expect_equal(dim(parsed), c(53136L, 41L))
  expect_true(all(c(
    "date", "value", "observation_status", "series_type", "measure",
    "skill_level", "state", "geography_type", "geography_code",
    "geography_name"
  ) %in% names(parsed)))
  expect_s3_class(parsed$date, "Date")
  expect_true(is.numeric(parsed$value))

  target <- parsed[
    parsed$date == as.Date("2026-06-01") &
      parsed$state == "AUST" &
      parsed$series_type == "trend" &
      parsed$skill_level == "0",
    ,
    drop = FALSE
  ]
  expect_equal(nrow(target), 1L)
  expect_identical(target$date[[1L]], as.Date("2026-06-01"))
  expect_identical(target$state[[1L]], "AUST")
  expect_identical(target$skill_level[[1L]], "0")
  expect_identical(target$series_type[[1L]], "trend")
  expect_identical(target$observation_status[[1L]], "published")
  expect_identical(target$geography_type[[1L]], "state_or_territory")
  expect_identical(target$geography_code[[1L]], "AUST")
  expect_equal(target$value[[1L]], 205057.541, tolerance = 1e-12)
})
