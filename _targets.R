library(targets)
library(tarchetypes)

# Pipeline settings ------------------------------------------------------------

set.seed(847322)  # From random.org

options(
  tidyverse.quiet = TRUE,
  dplyr.summarise.inform = FALSE
)

## Bayesian stuff ----
suppressPackageStartupMessages(library(brms))
options(
  mc.cores = 4,
  brms.backend = "cmdstanr"
)

## General pipeline options ----
tar_option_set(
  # Packages loaded in every pipeline run
  packages = c("tidyverse"),
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
  ## Raw data files ----
  tar_target(
    oecd_raw_file,
    here_rel("data", "raw_data", "OECD", "CRS.parquet"),
    format = "file"
  ),
  tar_files_input(
    iati_files,
    fs::dir_ls("data/raw_data/IATI/", glob = "*.csv")
  ),
  tar_target(
    chaudhry_raw_file,
    here_rel(
      "data",
      "raw_data",
      "Chaudhry restrictions",
      "Updated NGO data.xlsx"
    ),
    format = "file"
  ),
  tar_target(
    vdem_raw_file,
    here_rel(
      "data",
      "raw_data",
      "V-Dem-CY-FullOthers-v15_rds",
      "V-Dem-CY-Full+Others-v15.rds"
    ),
    format = "file"
  ),
  tar_target(
    un_pop_raw_file,
    here_rel(
      "data",
      "raw_data",
      "UN data",
      "WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx"
    ),
    format = "file"
  ),
  tar_target(
    un_gdp_constant_raw_file,
    here_rel(
      "data",
      "raw_data",
      "UN data",
      "UNdata_Export_20260120_063531011.csv"
    ),
    format = "file"
  ),
  tar_target(
    un_gdp_current_raw_file,
    here_rel(
      "data",
      "raw_data",
      "UN data",
      "UNdata_Export_20260120_063559235.csv"
    ),
    format = "file"
  ),
  tar_target(
    ucdp_raw_file,
    here_rel(
      "data",
      "raw_data",
      "UCDP PRIO",
      "UcdpPrioConflict_v25_1.csv"
    ),
    format = "file"
  ),
  tar_target(
    disasters_raw_file,
    here_rel(
      "data",
      "raw_data",
      "Disasters",
      "public_emdat_custom_request_2026-01-20_7545664b-0e1b-4634-a6d8-e852c266534e.xlsx"
    ),
    format = "file"
  ),
  tar_target(
    naturalearth_raw_file,
    here_rel(
      "data",
      "raw_data",
      "ne_110m_admin_0_countries",
      "ne_110m_admin_0_countries.shp"
    ),
    format = "file"
  ),

  ## Process and clean data ----
  ### Skeletons and lookups ----
  tar_target(chaudhry_raw, load_chaudhry_raw(chaudhry_raw_file)),
  tar_target(regulations, create_regulation_lookup()),
  tar_target(democracies, create_consolidated_democracies()),
  tar_target(skeleton, create_panel_skeleton(democracies, chaudhry_raw)),
  tar_target(iati, load_iati_codes(iati_files)),

  ### Aid stuff ----
  tar_target(oecd_tidy, build_oecd_tidy(oecd_raw_file)),

  ### NGO restrictions ----
  tar_target(chaudhry_clean, load_clean_chaudhry(chaudhry_raw, regulations)),

  ### Other data sources ----
  tar_target(vdem_clean, load_clean_vdem(vdem_raw_file)),
  tar_target(autocracies, build_autocracies(vdem_clean, skeleton)),
  tar_target(wdi_clean, load_clean_wdi(skeleton)),
  tar_target(un_pop, load_clean_un_pop(un_pop_raw_file, skeleton, wdi_clean)),
  tar_target(
    un_gdp,
    load_clean_un_gdp(
      un_gdp_constant_raw_file,
      un_gdp_current_raw_file,
      skeleton
    )
  ),
  tar_target(ucdp_prio_clean, load_clean_ucdp(ucdp_raw_file)),
  tar_target(
    disasters_clean,
    load_clean_disasters(disasters_raw_file, skeleton)
  ),

  ### Combine and lag data ----
  tar_target(
    aid_panel,
    build_aid_panel(
      skeleton,
      chaudhry_clean,
      vdem_clean,
      ucdp_prio_clean,
      disasters_clean,
      un_gdp,
      un_pop
    )
  ),

  ### Map and Civicus ----
  tar_target(world_map, load_world_map(naturalearth_raw_file)),
  tar_target(civicus_clean, load_clean_civicus()),
  tar_target(
    civicus_map_data,
    create_civicus_map_data(civicus_clean, world_map)
  ),

  ## Variable details ----

  ## Models ----

  ## Manuscript and analysis notebook ----
  # tar_quarto(manuscript, path = "manuscript", quiet = FALSE),

  # tar_quarto(website, path = ".", quiet = FALSE),
  # tar_target(deploy_script, here_rel("deploy.sh"), format = "file", cue = tar_cue_skip(!should_deploy)),
  # tar_target(deploy, {
  #   # Force a dependency
  #   website
  #   # Run the deploy script
  #   if (should_deploy) processx::run(paste0("./", deploy_script))
  # }, cue = tar_cue_skip(!should_deploy)),

  ## Render the README ----
  tar_quarto(readme, here_rel("README.qmd"))
)
