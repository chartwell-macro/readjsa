test_that("the exact pinned upstream MIT notice is preserved", {
  licence_path <- system.file("UPSTREAM_LICENSE.md", package = "readjsa")
  licence <- readLines(licence_path, warn = FALSE)
  upstream <- readLines(
    system.file("UPSTREAM.md", package = "readjsa"),
    warn = FALSE
  )

  expect_true("Copyright (c) 2024 readjsa authors" %in% licence)
  expect_equal(
    digest::digest(
      licence_path, file = TRUE, algo = "sha256", serialize = FALSE
    ),
    "0df6b8b14adafc47bd12f1b19effb217fb0ef236bda41a7dedd40d145362ecad"
  )
  expect_match(
    paste(upstream, collapse = "\n"),
    "94f063c92c7634c35e09e58269900064ce09f186",
    fixed = TRUE
  )
  expect_match(
    paste(upstream, collapse = "\n"),
    "version 0.0.1",
    fixed = TRUE
  )
})

test_that("current package ownership is distinct from upstream rights", {
  configured_root <- Sys.getenv("READJSA_SOURCE_ROOT", unset = "")
  local_root <- normalizePath(
    testthat::test_path("..", ".."),
    mustWork = FALSE
  )
  candidates <- c(
    if (nzchar(configured_root)) configured_root,
    local_root,
    file.path(getwd(), "..", "00_pkg_src", "readjsa"),
    file.path(getwd(), "00_pkg_src", "readjsa")
  )
  matches <- candidates[
    file.exists(file.path(candidates, "DESCRIPTION")) &
      file.exists(file.path(candidates, "LICENSE")) &
      file.exists(file.path(candidates, "LICENSE.md")) &
      file.exists(file.path(candidates, "inst", "CITATION"))
  ]
  source_layout <- length(matches) >= 1L
  if (source_layout) {
    package_root <- normalizePath(matches[[1L]])
    description <- read.dcf(file.path(package_root, "DESCRIPTION"))
    authors <- eval(parse(text = description[[1L, "Authors@R"]]))
    current_licence <- readLines(
      file.path(package_root, "LICENSE"),
      warn = FALSE
    )
    citation <- eval(parse(file = file.path(
      package_root,
      "inst",
      "CITATION"
    )))
  } else {
    description <- utils::packageDescription("readjsa")
    authors <- eval(parse(text = description[["Authors@R"]]))
    current_licence <- readLines(
      system.file("LICENSE", package = "readjsa"),
      warn = FALSE
    )
    citation <- utils::citation("readjsa")
  }
  copyright_holders <- authors[vapply(
    authors,
    function(author) "cph" %in% author$role,
    logical(1)
  )]

  expect_equal(length(copyright_holders), 1L)
  expect_identical(copyright_holders[[1]]$given, "Chartwell")
  expect_identical(copyright_holders[[1]]$family, "Macro")
  expect_identical(copyright_holders[[1]]$role, c("aut", "cre", "cph"))

  expect_identical(
    current_licence,
    c("YEAR: 2026", "COPYRIGHT HOLDER: Chartwell Macro")
  )
  expect_false(any(grepl(
    "readjsa authors|readjsa contributors",
    current_licence
  )))

  if (source_layout) {
    full_licence <- readLines(
      file.path(package_root, "LICENSE.md"),
      warn = FALSE
    )
    expect_true("Copyright (c) 2024 readjsa authors" %in% full_licence)
    expect_true("Copyright (c) 2026 Chartwell Macro" %in% full_licence)
  }

  expect_identical(format(citation$author[[1]]), "Chartwell Macro")
  expect_identical(format(citation$author[[2]]), "Matt Cowgill")
})
