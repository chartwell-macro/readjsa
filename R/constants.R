.readjsa_version <- "0.1.0"
.readjsa_schema_version <- "1"

.jsa_pages <- c(
  ivi = "https://www.jobsandskills.gov.au/data/internet-vacancy-index",
  reos = paste0(
    "https://www.jobsandskills.gov.au/data/",
    "recruitment-experiences-and-outlook-survey"
  ),
  salm = paste0(
    "https://www.dewr.gov.au/employment-research/",
    "small-area-labour-markets"
  )
)

.jsa_terms <- c(
  jobs_skills_australia =
    "https://www.jobsandskills.gov.au/copyright-and-disclaimer",
  dewr = "https://www.dewr.gov.au/copyright"
)

.ivi_families <- data.frame(
  family_id = c(
    "skill_states", "anzsco4_states", "anzsco2_states",
    "anzsco2_ivi_regions", "anzsco2_gccsa_sa4",
    "anzsco4_remoteness_northern"
  ),
  upstream_alias = c(
    "skill", "4dig", "2dig_states", "2dig_regions",
    NA_character_, NA_character_
  ),
  pattern = c(
    "skill.level.*states|skill_level.*states",
    "anzsco4.*states|anzsco4.*states_and_territories",
    "anzsco2.*states|anzsco2.*states_and_territories",
    "anzsco2.*ivi.regions|anzsco2.*ivi_regions",
    "anzsco2.*gccsa.*sa4|anzsco2.*gccsa_and_sa4",
    "anzsco4.*remoteness.*northern|anzsco4.*jsa_remoteness"
  ),
  classification_version = c(
    "ANZSCO version not specified; Australian states and territories",
    "ANZSCO version not specified; Australian states and territories",
    "ANZSCO version not specified; Australian states and territories",
    "ANZSCO version not specified; JSA IVI Regions",
    "ANZSCO version not specified; ASGS GCCSA/SA4 version not specified",
    "ANZSCO version not specified; JSA Remoteness/Northern Australia"
  ),
  stringsAsFactors = FALSE
)

#' Return the supported JSA and DEWR release families
#'
#' `jsa_release_manifest()` is the package's source-of-truth map of supported
#' publication families.  It is useful when choosing a `family` for a file
#' parser or checking the family IDs required by a release catalogue.  The
#' manifest describes supported coverage; it does not perform discovery or a
#' network request.
#'
#' @return A base [`data.frame`][base::data.frame] with nine rows and these
#'   columns, in order: `dataset_id` (`"ivi"`, `"reos"`, or `"salm"`),
#'   `family_id`, `upstream_alias`, and `classification_version`.  The six
#'   IVI rows are `skill_states`, `anzsco4_states`, `anzsco2_states`,
#'   `anzsco2_ivi_regions`, `anzsco2_gccsa_sa4`, and
#'   `anzsco4_remoteness_northern`; the remaining rows are `reos_tables`,
#'   `salm_sa2`, and `salm_lga`.  Character columns use ordinary character
#'   vectors and the data frame has no custom attributes.
#'
#' @examples
#' manifest <- jsa_release_manifest()
#' manifest[manifest$dataset_id == "salm", ]
#'
#' @seealso [discover_jsa_releases()], [read_ivi_file()], [read_salm_file()]
#' @family release-metadata
#' @export
jsa_release_manifest <- function() {
  ivi <- .ivi_families
  ivi$dataset_id <- "ivi"
  other <- data.frame(
    family_id = c("reos_tables", "salm_sa2", "salm_lga"),
    upstream_alias = NA_character_,
    pattern = c("recruitment.*data", "salm.*sa2", "salm.*lga"),
    classification_version = c(
      "Workbook metadata",
      "ASGS 2021 SA2",
      "ASGS vintage stated by release"
    ),
    dataset_id = c("reos", "salm", "salm"),
    stringsAsFactors = FALSE
  )
  rbind(ivi, other)[, c(
    "dataset_id", "family_id", "upstream_alias",
    "classification_version"
  )]
}
