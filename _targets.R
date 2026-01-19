library(targets)
library(tarchetypes)

# Pipeline settings ------------------------------------------------------------

set.seed(847322)  # From random.org

## Bayesian stuff ----
suppressPackageStartupMessages(library(brms))
options(
  mc.cores = 4,
  brms.backend = "cmdstanr"
)

## General pipeline options ----
tar_option_set(
  # Packages loaded in every pipeline run
  packages = c("tibble"),
  format = "qs"  # Use qs as the default storage format
)

## Set absolute location of targets store ----

# This hardcodes the absolute path in _targets.yaml, so to make this more
# portable, we rewrite it every time this pipeline is run (and we don't track
# _targets.yaml with git)
tar_config_set(
  store = here::here("_targets"),
  script = here::here("_targets.R")
)

## Relative here() ----

# here::here() returns an absolute path, which then gets stored in tar_meta and
# becomes computer-specific (i.e. /Users/andrew/Research/blah/thing.Rmd).
# There's no way to get a relative path directly out of here::here(), but
# fs::path_rel() works fine with it (see
# https://github.com/r-lib/here/issues/36#issuecomment-530894167)
here_rel <- function(...) {fs::path_rel(here::here(...))}

## Run the R scripts in R/ ----
tar_source()

## Set some conditional flags ----
should_deploy <- identical(Sys.getenv("UPLOAD_WEBSITES"), "TRUE")
is_docker <- identical(Sys.getenv("IS_DOCKER"), "TRUE")


# Pipeline ---------------------------------------------------------------------

list(
  ## Render the README ----
  tar_quarto(readme, here_rel("README.qmd"))
)
