
# Lookup tables -----------------------------------------------------------

create_consolidated_democracies <- function() {
  library(countrycode)

  consolidated_democracies <-
    tibble(
      country_name = c(
        "Andorra", "Australia", "Austria", "Bahamas", "Barbados", "Belgium",
        "Canada", "Denmark", "Finland", "France", "Germany", "Greece", 
        "Grenada", "Iceland", "Ireland", "Italy", "Japan", "Liechtenstein",
        "Luxembourg", "Malta", "Monaco", "Netherlands", "New Zealand",
        "Norway", "San Marino", "Spain", "Sweden", "Switzerland",
        "United Kingdom", "United States of America"
      )
    ) |>
    # Ignore these 5 microstates, since they're not in the panel skeleton
    filter(
      !(country_name %in%
        c("Andorra", "Grenada", "Liechtenstein", "Monaco", "San Marino"))
    ) |>
    mutate(
      iso3 = countrycode(country_name, "country.name", "iso3c"),
      gwcode = countrycode(country_name, "country.name", "gwn")
    )

  return(consolidated_democracies)
}

create_regulation_lookup <- function() {
  regulations <- tribble(
    ~question, ~barrier,       ~question_clean,                  ~ignore_in_index, ~question_display,
    "q1a",     "association",  "const_assoc",                    TRUE,             "Constitutional associational rights",
    "q1b",     "association",  "political_parties",              TRUE,             "Citizens form political parties",
    "q2a",     "entry",        "ngo_register",                   TRUE,             "NGO registration required",
    "q2b",     "entry",        "ngo_register_burden",            FALSE,            "NGO registration burdensome",
    "q2c",     "entry",        "ngo_register_appeal",            FALSE,            "NGO registration appealXXXnot allowed",
    "q2d",     "entry",        "ngo_barrier_foreign_funds",      FALSE,            "Registration barriers differentXXXif foreign funds involved",
    "q3a",     "funding",      "ngo_disclose_funds",             TRUE,             "Funds must be disclosed",
    "q3b",     "funding",      "ngo_foreign_fund_approval",      FALSE,            "Prior approval requiredXXXfor foreign funds",
    "q3c",     "funding",      "ngo_foreign_fund_channel",       FALSE,            "Foreign funds channeledXXXthrough government",
    "q3d",     "funding",      "ngo_foreign_fund_restrict",      FALSE,            "Foreign funds restricted",
    "q3e",     "funding",      "ngo_foreign_fund_prohibit",      FALSE,            "Foreign funds prohibited",
    "q3f",     "funding",      "ngo_type_foreign_fund_prohibit", FALSE,            "Foreign funds prohibitedXXXfor some types of NGOs",
    "q4a",     "advocacy",     "ngo_politics",                   FALSE,            "NGOs restricted from politics",
    "q4b",     "advocacy",     "ngo_politics_intimidation",      TRUE,             "NGOs intimidated from politics",
    "q4c",     "advocacy",     "ngo_politics_foreign_fund",      FALSE,            "Political barriers differentXXXif foreign funds involved"
  )

  return(regulations)
}

load_iati_codes <- function(files) {
  set_names(files, fs::path_ext_remove(basename(files))) |>
    map(\(x) read_csv(x, col_types = cols()))
}


# Panel skeleton ----------------------------------------------------------

load_chaudhry_raw <- function(path) {
  library(readxl)
  library(countrycode)

  # In this data Sudan (625) splits into North Sudan (626) and South Sudan (525)
  # in 2011, but in the other datasets regular Sudan stays 625 and South Sudan
  # becomes 626, so adjust the numbers here
  #
  # Also, Chad is in the dataset, but most values are missing, so we drop it
  chaudhry_raw <- suppressMessages(read_excel(path)) |>
    select(1:15) |>
    filter(ccode != 483) |> # Remove Chad
    # Fix Sudan
    mutate(
      ccode = case_when(
        country == "South Sudan" ~ 626,
        country == "Sudan" ~ 625,
        TRUE ~ ccode
      )
    ) |>
    # Make GW code column
    mutate(
      gwcode = countrycode(
        ccode,
        origin = "cown",
        destination = "gwn",
        custom_match = c(
          "679" = 678L, # Yemen
          "818" = 816L, # Vietnam
          "816" = 816L, # Vietnam
          "342" = 345L, # Serbia
          "341" = 347L, # Kosovo
          "348" = 341L, # Montenegro
          "315" = 316L  # Czechia
        )
      )
    )

  return(chaudhry_raw)
}

create_panel_skeleton <- function(consolidated_democracies, chaudhry_raw) {
  library(states)
  library(countrycode)

  microstates <- gwstates |>
    filter(microstate) |> 
    distinct(gwcode) |> 
    as_tibble()

  chaudhry_countries <- chaudhry_raw |> distinct(gwcode)

  # In both COW and GW codes, modern Vietnam is 816, but countrycode() thinks the
  # COW code is 817, which is old South Vietnam (see issue
  # https://github.com/vincentarelbundock/countrycode/issues/16), so we use
  # custom_match to force 816 to recode to 816
  #
  # Also, following Gleditsch and Ward, we treat Serbia after 2006 dissolution of
  # Serbia & Montenegro as 345 in COW codes (see
  # https://www.andybeger.com/states/articles/differences-gw-cow.html)
  #
  # Following V-Dem, we treat Czechoslovakia (GW/COW 315) and Czech Republic
  # (GW/COW 316) as the same continuous country (V-Dem has both use ID 157).
  #
  # Also, because the World Bank doesn't include it in the WDI, we omit
  # Taiwan (713). We also omit East Germany (265) and South Yemen (680).
  panel_skeleton_all <- state_panel(1980, 2024, partial = "any") |>
    # Remove microstates
    filter(!(gwcode %in% microstates$gwcode)) |>
    # Remove East Germany, South Yemen, Taiwan, the Bahamas, Belize, and Brunei
    filter(!(gwcode %in% c(265, 680, 713, 31, 80, 835))) |>
    # Deal with Czechia
    mutate(gwcode = recode(gwcode, `315` = 316L)) |>
    mutate(
      cowcode = countrycode(
        gwcode,
        origin = "gwn",
        destination = "cown",
        custom_match = c("816" = 816L, "340" = 345L)
      ),
      country = countrycode(
        cowcode,
        origin = "cown",
        destination = "country.name",
        custom_match = c("678" = "Yemen")
      ),
      iso2 = countrycode(
        cowcode,
        origin = "cown",
        destination = "iso2c",
        custom_match = c("345" = "RS", "347" = "XK", "678" = "YE")
      ),
      iso3 = countrycode(
        cowcode,
        origin = "cown",
        destination = "iso3c",
        custom_match = c("345" = "SRB", "347" = "XKK", "678" = "YEM")
      ),
      # Use 999 as the UN country code for Kosovo
      un = countrycode(
        cowcode,
        origin = "cown",
        destination = "un",
        custom_match = c("345" = 688, "347" = 999, "678" = 887)
      ),
      region = countrycode(cowcode, origin = "cown", destination = "region"),
      un_region = countrycode(
        cowcode,
        origin = "cown",
        destination = "un.region.name",
        custom_match = c("345" = "Europe", "347" = "Europe", "678" = "Asia")
      ),
      un_subregion = countrycode(
        cowcode,
        origin = "cown",
        destination = "un.regionsub.name",
        custom_match = c(
          "345" = "Eastern Europe",
          "347" = "Eastern Europe",
          "678" = "Western Asia"
        )
      )
    ) |>
    # There are two entries for "Yugoslavia" in 2006 after recoding 340 as 345;
    # get rid of one
    filter(!(gwcode == 340 & cowcode == 345 & year == 2006)) |>
    # Make Serbia 345 in GW codes too, for joining with other datasets
    mutate(gwcode = recode(gwcode, `340` = 345L)) |>
    mutate(country = recode(country, `Yugoslavia` = "Serbia")) |>
    arrange(gwcode, year)

  panel_skeleton <- panel_skeleton_all |> 
    filter(gwcode %in% chaudhry_countries$gwcode) |> 
    filter(!(gwcode %in% consolidated_democracies$gwcode)) |> 
    as_tibble()

  skeleton_lookup <- panel_skeleton |> 
    group_by(
      gwcode, cowcode, country, iso2, iso3, un, 
      region, un_region, un_subregion
    ) |> 
    summarize(years_included = n()) |> 
    ungroup() |> 
    arrange(country)

  return(lst(panel_skeleton, panel_skeleton_all, microstates, skeleton_lookup))
}


# Aid stuff ---------------------------------------------------------------

build_oecd_tidy <- function(path) {
  library(arrow)
  library(countrycode)
  library(states)

  # https://one.oecd.org/document/DCD/DAC(99)20/en/pdf
  # https://www.rcc.int/seedad/files/user/docs/2016_CRS_purpose_codes.pdf
  sector_lookup <- tribble(
    ~lower , ~sector_grouping , ~sector_grouping_nice                           ,
       100 , "social"         , "Social infrastructure and services"            ,
       200 , "economic"       , "Economic infrastructure and services"          ,
       300 , "production"     , "Production sectors"                            ,
       400 , "multisector"    , "Multisector/Cross-cutting"                     ,
       500 , "commodity"      , "Commodity aid and general programme assitance" ,
       600 , "debt"           , "Action relating to debt"                       ,
       700 , "humanitarian"   , "Humanitarian aid"                              ,
       910 , "administrative" , "Administrative"                                ,
       930 , "refugees"       , "Refugees in donor countries"                   ,
       998 , "unspecified"    , "Unallocated/unspecified"
  )

  crs_bilateral_raw <- read_parquet("data/raw_data/OECD/CRS.parquet")

  # In the past, when working with AidData data, we had to filter this to only
  # look at projects in DAC-eligible countries (I think becuase AidData used
  # other non-OECD sources?). Now that this is OECD only, we're looking only at
  # bilateral (bi_multi == 1) ODA (category == 10) to non-regional recipients,
  # so by definition, it only includes DAC-eligible countries. This is
  # confirmable by looking at countries that lost DAC eligibility like
  # Oman---they drop out of the data.
  #
  # For additional reference, see
  # https://www.oecd.org/en/topics/sub-issues/oda-eligibility-and-conditions/dac-list-of-oda-recipients.html#oda-recipients-list
  # for the currenet list and links to historical lists
  crs_bilateral <- crs_bilateral_raw |>
    # Lots of filtering!
    filter(
      # ODA only
      category == 10,
      # Bilateral aid only
      bi_multi == 1,
      # Remove regional/unspecified recipients
      !str_ends(de_recipientcode, "_X"),
      !str_detect(str_to_lower(recipient_name), "unspecified"),
      !str_detect(str_to_lower(recipient_name), "regional"),
      # Remove provisional data and net disbursements
      is.na(initial_report) | !initial_report %in% c(5, 9),
      # Remove projects based in donor countries:
      # - E: Scholarships and student costs in donor countries
      # - G: Administrative costs not included elsewhere
      # - H: Other in-donor expenditures
      is.na(aid_t) | !str_starts(aid_t, "[EGH]"),
      # Remove other projects based in donor countries:
      # - 91010: Administrative costs (non-sector allocable)
      # - 93010:93018: Refugees/asylum seekers in donor countries
      # - 99820: Promotion of development awareness (non-sector allocable)
      !(purpose_code %in% c(91010, 93010:93018, 99820))
    ) |>
    # Add specific categories of aid
    mutate(
      # Indicate whether spending goes to/through CSOs
      # aid_t:
      # - B01: Core support to NGOs, other private bodies, PPPs and 
      #   research institutes
      # channel_code:
      # - 20000: NGO/CSO unspecified
      # - 21xxx: International NGOs
      # - 22xxx: Donor country-based NGOs
      # - 23xxx: Developing country-based NGOs
      cso_flow = coalesce(
        aid_t %in% "B01" | (channel_code >= 20000 & channel_code < 30000),
        FALSE
      ),
      # Indicate what kind of CSO is used
      cso_channels = case_when(
        channel_code == 20000 | aid_t %in% "B01" ~ "cso_unspecified",
        channel_code >= 21000 & channel_code < 22000 ~ "cso_international",
        channel_code >= 22000 & channel_code < 23000 ~ "cso_donor",
        channel_code >= 23000 & channel_code < 24000 ~ "cso_developing",
        .default = NA_character_
      ),
      # Indicate whether the aid is contentious or tame
      contentious = ifelse(
        sector_code %in%
          c(
            151, # Government & Civil Society-general
            152 # Conflict, Peace & Security
          ),
        "high",
        "low"
      )
    ) |>
    left_join(
      sector_lookup,
      join_by(closest(sector_code >= lower))
    ) |>
    # Use constant/deflated 2023 values and scale up to full amount instead of millions
    mutate(commitment = usd_commitment_defl * 1e6)

  # There are a bunch of microstates that receive aid, and we don't track most
  # of them in the NGO data, and lots of them don't even have GW codes (like
  # Gibraltar, Netherlands Antilles, Turks and Caicos, etc.) but for the sake of
  # completeness, we create GW codes for as many as possible

  # Get microstate GW codes
  microstates <- gwstates |>
    filter(microstate) |>
    distinct(gwcode_micro = gwcode, country_name) |>
    as_tibble()

  # Get all the recipient countries, match as many as possible by name with
  # countrycode(), then join in microstate data and keep whichever is not NA
  crs_recipient_countries <- crs_bilateral |>
    distinct(recipient_name, de_recipientcode) |>
    mutate(
      gwcode_from_name = countrycode(
        recipient_name,
        "country.name",
        "gwn",
        custom_match = c("Yemen" = 678),
        warn = FALSE
      )
    ) |>
    left_join(microstates, by = join_by(recipient_name == country_name)) |>
    mutate(gwcode = coalesce(gwcode_from_name, gwcode_micro)) |>
    select(de_recipientcode, recipient_gwcode = gwcode)

  crs_tidy <- crs_bilateral |>
    left_join(crs_recipient_countries, by = join_by(de_recipientcode)) |> 
    select(
      year,
      donor_iso3 = de_donorcode,
      donor_name,
      recipient_iso3 = de_recipientcode,
      recipient_gwcode,
      recipient_name,
      commitment,
      contentious,
      sector_grouping,
      sector_code,
      cso_flow,
      cso_channels,
      channel_code,
      aid_t
    )

  return(crs_tidy)
}


# NGO restrictions --------------------------------------------------------

load_clean_chaudhry <- function(chaudhry_raw, regulations) {
  suppressPackageStartupMessages(library(scales))

  chaudhry_2024 <- expand_grid(
    gwcode = unique(chaudhry_raw$gwcode),
    year = 2024
  )

  chaudhry_long <- chaudhry_raw |>
    # Bring in 2024 rows
    bind_rows(chaudhry_2024) |>
    # Ethiopia and Czech Republic have duplicate rows in 1993 and 1994 respectively,
    # but the values are identical, so just keep the first of the two
    group_by(gwcode, year) |>
    slice(1) |>
    ungroup() |>
    arrange(gwcode, year) |>
    # Bring country details forward
    group_by(gwcode) |>
    fill(ccode, country) |>
    ungroup() |>
    # Reverse values for q2c
    mutate(q2c = 1 - q2c) |>
    # Rescale 2-point questions to 0-1 scale
    mutate(across(c(q3e, q3f, q4a), \(x) {
      rescale(x, to = c(0, 1), from = c(0, 2))
    })) |>
    # q2d and q4c use -1 to indicate less restriction/burdensomeness. Since we're
    # concerned with an index of restriction, we make the negative values zero
    mutate(across(c(q2d, q4c), \(x) ifelse(x == -1, 0, x))) |>
    pivot_longer(cols = starts_with("q"), names_to = "question") |>
    left_join(regulations, by = join_by("question")) |>
    group_by(gwcode) |>
    mutate(all_missing = all(is.na(value))) |>
    group_by(gwcode, question) |>
    # Bring most recent legislation forward in time
    fill(value) |>
    # For older NA legislation that can't be brought forward, set sensible
    # defaults. Leave countries that are 100% 0 as NA.
    mutate(value = ifelse(!all_missing & is.na(value), 0, value)) |>
    ungroup()

  chaudhry_registration <- chaudhry_long |>
    select(gwcode, year, question_clean, value) |>
    pivot_wider(names_from = "question_clean", values_from = "value")

  chaudhry_summed <- chaudhry_long |>
    filter(!ignore_in_index) |>
    group_by(gwcode, year, barrier) |>
    summarize(total = sum(value)) |>
    ungroup()

  chaudhry_clean <- chaudhry_summed |>
    pivot_wider(names_from = barrier, values_from = total) |>
    mutate(across(
      c(entry, funding, advocacy),
      list(std = \(x) x / max(x, na.rm = TRUE))
    )) |>
    mutate(
      barriers_total = advocacy + entry + funding,
      barriers_total_std = advocacy_std + entry_std + funding_std
    ) |>
    left_join(chaudhry_registration, by = c("gwcode", "year"))

  # In Suparna's clean data, due to post-Cold War chaos, Russia (365) is missing
  # for 1990-1991 and Serbia/Serbia and Montenegro/Yugoslavia (345) is missing
  # every thing pre-2006. DCJW don't include any data for Serbia, so we're out
  # of luck there—we're limited to Serbia itself and not past versions of it.
  # DCJW *do* include data for Russia, though, so we use that in our clean final
  # NGO laws data. Fortunately this is easy, since Russia's values are all 0 for
  # those two years. We just add two rows for Russia in 1990 and 1991 from DCJW
  #
  # Additionally, Suparna's final data is missing Ivory Coast (437) in 1990, but
  # it existed in previous versions, so it's just a typo. It's also all 0s.
  final_fixes <- tibble(
    gwcode = c(365, 365, 437),
    year = c(1990, 1991, 1990)
  ) |>
    mutate(
      advocacy = 0,
      entry = 0,
      funding = 0,
      entry_std = 0,
      funding_std = 0,
      advocacy_std = 0,
      barriers_total = 0,
      barriers_total_std = 0
    )

  chaudhry_clean <- chaudhry_clean |> 
    bind_rows(final_fixes) |> 
    arrange(gwcode, year)

  return(chaudhry_clean)
}


# V-Dem -------------------------------------------------------------------

load_clean_vdem <- function(path) {
  library(countrycode)

  vdem_raw <- read_rds(path) |> as_tibble()

  vdem_clean <- vdem_raw |>
    filter(year >= 1980) |>
    select(
      country_name, year, cowcode = COWcode,

      # Civil society stuff
      v2cseeorgs,  # CSO entry and exit
      v2csreprss,  # CSO repression
      v2cscnsult,  # CSO consultation
      v2csprtcpt,  # CSO participatory environment
      v2csgender,  # CSO women's participation
      v2csantimv,  # CSO anti-system movements
      v2xcs_ccsi,  # Core civil society index (entry/exit, repression, participatory env)

      # Human rights and politics
      # Political corruption index (less to more, 0-1) (public sector +
      # executive + legislative + judicial corruption)
      v2x_corr,
      v2x_rule,   # Rule of law index
      # Rights indexes
      v2x_civlib, # Civil liberties index
      v2x_clphy,  # Physical violence index
      v2x_clpriv, # Private civil liberties index
      v2x_clpol,  # Political civil liberties index
      # Democracy
      e_polity2, v2x_polyarchy, v2x_regime_amb,
      # Economics and development
      v2peedueq,  # Educational equality
      v2pehealth # Health equality
    ) |>
    # Get rid of East Germany
    filter(cowcode != 265) |>
    mutate(
      gwcode = countrycode(
        cowcode,
        origin = "cown",
        destination = "gwn",
        custom_match = c(
          "403" = 403L,
          "591" = 591L,
          "679" = 678L,
          "935" = 935L,
          "816" = 816L,
          "260" = 260L,
          "315" = 316L
        )
      )
    ) |>
    # Get rid of Hong Kong, Palestine (West Bank and Gaza), and Somaliland
    filter(!is.na(cowcode)) |>
    select(-country_name, -cowcode)

  return(vdem_clean)
}

build_autocracies <- function(vdem, skeleton) {
  autocracies <- vdem |> 
    group_by(gwcode) |> 
    summarize(avg_row = mean(v2x_regime_amb, na.rm = TRUE)) |> 
    ungroup()

  autocracies_final <- skeleton$skeleton_lookup |> 
    left_join(autocracies, by = "gwcode") |> 
    mutate(autocracy = round(avg_row, 0) <= 4)
}


# WDI ---------------------------------------------------------------------

load_clean_wdi <- function(skeleton) {
  library(WDI)
  library(countrycode)

  # World Bank World Development Indicators (WDI)
  # https://datacatalog.worldbank.org/
  # https://datacatalog.worldbank.org/search/dataset/0037712/world-development-indicators
  wdi_indicators <- c(
    gdp_percap = "NY.GDP.PCAP.PP.KD", # GDP per capita, ppp (constant 2021 international $)
    gdp = "NY.GDP.MKTP.PP.KD", # GDP, ppp (constant 2021 international $)
    trade_pct_gdp = "NE.TRD.GNFS.ZS", # Trade (% of GDP)
    population = "SP.POP.TOTL", # Population, total
    infant_mortality = "SP.DYN.IMRT.IN", # Infant moratlity rate
    life_expectancy = "SP.DYN.LE00.IN" # Life expectancy at birth
  )

  wdi_raw <- WDI(
    country = "all",
    wdi_indicators,
    extra = TRUE,
    start = 1980,
    end = 2025
  )

  wdi_clean <- wdi_raw |>
    filter(iso2c %in% unique(skeleton$panel_skeleton$iso2)) |>
    mutate(
      gwcode = countrycode(
        iso2c,
        origin = "iso2c",
        destination = "gwn",
        custom_match = c("YE" = 678L, "XK" = 347L, "VN" = 816L, "RS" = 345L)
      )
    ) |>
    mutate(
      region = ifelse(gwcode == 343, "Europe & Central Asia", region),
      income = ifelse(gwcode == 343, "Upper middle income", income)
    ) |>
    select(
      country,
      gwcode,
      year,
      region,
      income,
      population,
      infant_mortality,
      life_expectancy
    )

  return(wdi_clean)
}


# UN data -----------------------------------------------------------------

# Population
# Demographic Indicators Compact (most used: estimates and medium projections)
# https://population.un.org/wpp/downloads
load_clean_un_pop <- function(path, skeleton, wdi) {
  library(countrycode)
  library(readxl)

  # The UN doesn't have population data for Kosovo, so we use WDI data for that
  kosovo_population <- wdi |>
    select(gwcode, year, population, infant_mortality, life_expectancy) |>
    filter(gwcode == 347, year >= 2008) |> 
    arrange(year)

  # WDI data is missing for 2024 infant mortality and life expectancy, so
  # we impute it with two different models:
  #
  # - For infant mortality, the trend is basically perfectly polynomial,
  #   so we use mortality ~ year + year^2
  # - For life expectancy, the trend is fairly linear except for a huge 
  #   decline in 2020 and 2021 due to COVID, so we ignore those years and 
  #   predict 2024 based on a hypothetical pandemic-free world
  infant_model <- lm(
    infant_mortality ~ year + I(year^2),
    data = kosovo_population
  )

  life_exp_model <- lm(
    life_expectancy ~ year,
    # Ignore COVID dip
    data = kosovo_population |> filter(!year %in% c(2020, 2021))
  )

  kosovo_population <- kosovo_population |>
    mutate(
      infant_mortality = ifelse(
        year == 2024,
        predict(infant_model, newdata = tibble(year = 2024)),
        infant_mortality
      ),
      life_expectancy = ifelse(
        year == 2024,
        predict(life_exp_model, newdata = tibble(year = 2024)),
        life_expectancy
      )
    )

  un_pop_raw <- read_excel(
    path,
    range = "Estimates!A17:AV22000",
    guess_max = 3000
  )

  un_pop <- un_pop_raw |>
    filter((`Location code` %in% unique(skeleton$panel_skeleton$un))) |>
    select(
      country = `Region, subregion, country or area *`,
      un_code = `Location code`,
      year = Year,
      population = `Total Population, as of 1 July (thousands)`,
      infant_mortality = `Infant Mortality Rate (infant deaths per 1,000 live births)`,
      life_expectancy = `Life Expectancy at Birth, both sexes (years)`
    ) |>
    mutate(
      gwcode = countrycode(
        un_code,
        "un",
        "gwn",
        custom_match = c("887" = 678)
      )
    ) |>
    mutate(population = as.numeric(population) * 1000) |> # Values are in 1000s
    mutate(across(c(infant_mortality, life_expectancy), as.numeric)) |> 
    select(gwcode, year, population, infant_mortality, life_expectancy) |>
    bind_rows(kosovo_population) |>
    arrange(gwcode, year)

  return(un_pop)
}

load_clean_un_gdp <- function(path_constant, path_current, skeleton) {
  library(countrycode)

  # GDP by Type of Expenditure at constant (2015) prices - US dollars
  # https://data.un.org/Data.aspx?q=gdp&d=SNAAMA&f=grID%3a102%3bcurrID%3aUSD%3bpcFlag%3a0
  un_gdp_raw <- read_csv(path_constant, col_types = cols()) |>
    rename(country = `Country or Area`) |>
    mutate(value_type = "Constant")

  # GDP by Type of Expenditure at current prices - US dollars
  # https://data.un.org/Data.aspx?q=gdp&d=SNAAMA&f=grID%3a101%3bcurrID%3aUSD%3bpcFlag%3a0
  un_gdp_current_raw <- read_csv(path_current, col_types = cols()) |>
    rename(country = `Country or Area`) |>
    mutate(value_type = "Current")

  un_gdp <- bind_rows(un_gdp_raw, un_gdp_current_raw) |>
    filter(
      Item %in%
        c(
          "Gross Domestic Product (GDP)",
          "Exports of goods and services",
          "Imports of goods and services"
        )
    ) |>
    filter(
      !(country %in%
        c(
          "Former USSR",
          "Former Netherlands Antilles",
          "Yemen: Former Democratic Yemen",
          "United Republic of Tanzania: Zanzibar"
        ))
    ) |>
    filter(!(country == "Yemen: Former Yemen Arab Republic" & Year >= 1989)) |>
    filter(!(country == "Former Czechoslovakia" & Year >= 1990)) |>
    filter(!(country == "Former Yugoslavia" & Year >= 1990)) |>
    filter(!(country == "Former Ethiopia" & Year >= 1990)) |>
    mutate(
      country = recode(
        country,
        "Former Sudan" = "Sudan",
        "Yemen: Former Yemen Arab Republic" = "Yemen",
        "Former Czechoslovakia" = "Czechia",
        "Former Yugoslavia" = "Serbia"
      )
    ) |>
    mutate(
      iso3 = countrycode(
        country,
        "country.name",
        "iso3c",
        custom_match = c("Kosovo" = "XKK")
      )
    ) |>
    left_join(select(skeleton$skeleton_lookup, iso3, gwcode), by = "iso3") |>
    filter(!is.na(gwcode))

  un_gdp_wide <- un_gdp |>
    select(gwcode, year = Year, Item, Value, value_type) |>
    pivot_wider(names_from = c(value_type, Item), values_from = Value) |>
    rename(
      exports_constant_2015 = `Constant_Exports of goods and services`,
      imports_constant_2015 = `Constant_Imports of goods and services`,
      gdp_constant_2015 = `Constant_Gross Domestic Product (GDP)`,
      exports_current = `Current_Exports of goods and services`,
      imports_current = `Current_Imports of goods and services`,
      gdp_current = `Current_Gross Domestic Product (GDP)`
    ) |>
    mutate(gdp_deflator_2015 = gdp_current / gdp_constant_2015 * 100) |>
    mutate(un_trade_pct_gdp = (imports_current + exports_current) / gdp_current)

  # Rescale the 2015 data to 2023 to match the OECD data
  #
  # Deflator = current GDP / constant GDP * 100
  # Current GDP in year_t * (deflator in year_target / deflator in year_t)
  un_gdp_rescaled <- un_gdp_wide |>
    left_join(
      select(
        filter(un_gdp_wide, year == 2023),
        gwcode,
        deflator_target_year = gdp_deflator_2015
      ),
      by = "gwcode"
    ) |>
    mutate(
      gdp_deflator_2023 = gdp_deflator_2015 / deflator_target_year * 100,
      un_gdp_2023 = gdp_current / (gdp_deflator_2023 / 100),
      un_trade_pct_gdp = (imports_current + exports_current) / gdp_current
    )

  un_gdp_final <- un_gdp_rescaled |>
    select(gwcode, year, un_trade_pct_gdp, un_gdp = un_gdp_2023)

  return(un_gdp_final)
}


# UCDP --------------------------------------------------------------------

load_clean_ucdp <- function(path) {
  ucdp_prio_raw <- read_csv(path, col_types = cols())

  ucdp_prio_clean <- ucdp_prio_raw |>
    filter(type_of_conflict == 3) |> # Intrastate conflict
    mutate(gwcode_raw = str_split(gwno_a, pattern = ", ")) |>
    unnest(gwcode_raw) |>
    mutate(gwcode = as.integer(gwcode_raw)) |>
    group_by(gwcode, year) |>
    summarize(internal_conflict = n() > 0) |>
    ungroup()

  return(ucdp_prio_clean)
}


# EM-DAT disasters --------------------------------------------------------

load_clean_disasters <- function(path, skeleton) {
  library(readxl)
  library(countrycode)

  disasters_raw <- read_excel(path, guess_max = 10000)

  disasters <- disasters_raw |>
    # Only look at countries in the main panel
    filter(ISO %in% unique(skeleton$panel_skeleton$iso3)) |>
    mutate(
      gwcode = countrycode(
        ISO,
        origin = "iso3c",
        destination = "gwn",
        custom_match = c("YEM" = "678")
      ),
      gwcode = as.numeric(gwcode),
      year = as.numeric(str_sub(`DisNo.`, 1, 4))
    ) |>
    select(
      country = Country,
      year,
      iso3 = ISO,
      gwcode,
      type = `Disaster Type`,
      group = `Disaster Group`,
      subgroup = `Disaster Subgroup`,
      dis_deaths = `Total Deaths`,
      dis_injured = `No. Injured`,
      dis_affected = `No. Affected`,
      dis_homeless = `No. Homeless`,
      dis_total_affected = `Total Affected`,
      dis_total_damage = `Total Damage ('000 US$)`
    )

  disasters_summarized <- disasters |>
    group_by(gwcode, year, group) |>
    summarize(
      across(starts_with("dis_"), \(x) sum(x, na.rm = TRUE)),
      dis_count = n()
    ) |>
    ungroup() |>
    filter(group == "Natural") |>
    pivot_longer(names_to = "name", values_to = "value", starts_with("dis_")) |>
    mutate(group = str_to_lower(group)) |>
    unite(name, group, name) |>
    pivot_wider(names_from = "name", values_from = "value") |>
    mutate(year = as.numeric(year)) |>
    filter(year > 1980)

  return(disasters_summarized)
}


# World map ---------------------------------------------------------------

load_world_map <- function(path) {
  suppressPackageStartupMessages(library(sf))

  world_map <- read_sf(path) |>
    filter(ISO_A3 != "ATA") |> 
    rename(iso3 = ISO_A3_EH) |> 
    mutate(iso3 = case_when(
      iso3 == "GRL" ~ "DNK",
      NAME == "Kosovo" ~ "XKK",
      .default = iso3
    ))

  return(world_map)
}


# Civicus Monitor ---------------------------------------------------------

load_clean_civicus <- function() {
  library(countrycode)
  library(rvest)

  civicus <- read_html(
    str_glue(
      "http://web.archive.org/web/20260120021008/",
      "https://monitor.civicus.org/widgets/world/"
    )
  ) |>
    html_element(xpath = '//*[@id="countries-list-container"]/div[2]/table') |>
    html_table() |>
    filter(Country != "Greenland") |>
    rename(Rating = `Current rating`) |>
    mutate(
      iso3 = countrycode(
        Country,
        "country.name",
        "iso3c",
        custom_match = c(
          "Kosovo" = "XXK",
          "Micronesia" = "FSM",
          "Somaliland" = "RSL"
        )
      ),
      Score = as.numeric(str_extract(Score, "^(\\d+)/", group = 1)),
      Rating = factor(
        Rating,
        levels = c("Closed", "Repressed", "Obstructed", "Narrowed", "Open"),
        ordered = TRUE
      )
    )

  return(civicus)
}

create_civicus_map_data <- function(civicus, world_map) {
  suppressPackageStartupMessages(library(sf))

  map_with_civicus <- world_map |>
    left_join(civicus, by = join_by(iso3))

  return(map_with_civicus)
}


# Combine, clean, center, and lag everything ------------------------------

build_aid_panel <- function(
  skeleton,
  chaudhry_clean,
  vdem_clean,
  ucdp_prio_clean,
  disasters_clean,
  un_gdp,
  un_pop,
  oecd_tidy
) {
  # Calculate different versions of the aid variables
  aid_by_country_total <- oecd_tidy |> 
    group_by(recipient_gwcode, year) |> 
    summarize(total_oda = sum(commitment, na.rm = TRUE)) |> 
    ungroup()

  aid_by_country_purpose <- oecd_tidy |>
    group_by(recipient_gwcode, year, contentious) |>
    summarize(total_oda = sum(commitment, na.rm = TRUE)) |>
    ungroup() |>
    pivot_wider(
      names_from = contentious,
      names_prefix = "oda_contentious_",
      values_from = total_oda,
      values_fill = 0
    )
  
  aid_by_country_cso <- oecd_tidy |> 
    group_by(recipient_gwcode, year, cso_flow) |> 
    summarize(oda_cso_total = sum(commitment, na.rm = TRUE)) |>
    ungroup() |>
    filter(!cso_flow) |> 
    select(-cso_flow)

  aid_by_country_cso_type <- oecd_tidy |> 
    group_by(recipient_gwcode, year, cso_channels) |> 
    summarize(total = sum(commitment, na.rm = TRUE)) |>
    ungroup() |>
    pivot_wider(
      names_from = cso_channels,
      names_prefix = "oda_",
      values_from = total,
      values_fill = 0
    ) |> 
    select(-oda_NA)

  # Build full panel and merge everything together!
  country_level_data <- skeleton$panel_skeleton |>
    # filter(!(gwcode %in% democracies$gwcode)) |>
    left_join(un_gdp, by = join_by(gwcode, year)) |>
    left_join(un_pop, by = join_by(gwcode, year)) |>
    mutate(
      gdpcap = un_gdp / population,
      gdpcap_log = log(gdpcap),
      population_log = log(population)
    ) |>
    left_join(chaudhry_clean, by = join_by(gwcode, year)) |>
    # Indicator for Chaudhry data coverage
    # Chaudhry's Serbia data starts with 2006 and doesn't include pre-2006 stuff,
    # so we mark those as false. Also, Chaudhry starts in 1992 for Russia and 1993
    # for Czechia, so we mark those as false too
    mutate(laws = year %in% 1990:2026) |>
    mutate(
      laws = case_when(
        # Serbia, Czechia, and Russia
        gwcode == 345 & year <= 2005 ~ FALSE,
        gwcode == 316 & year <= 1992 ~ FALSE,
        gwcode == 365 & year <= 1991 ~ FALSE,
        .default = laws # Otherwise, use FALSE
      )
    ) |>
    left_join(
      aid_by_country_total,
      by = join_by(gwcode == recipient_gwcode, year)
    ) |>
    left_join(
      aid_by_country_purpose,
      by = join_by(gwcode == recipient_gwcode, year)
    ) |>
    left_join(
      aid_by_country_cso,
      by = join_by(gwcode == recipient_gwcode, year)
    ) |>
    left_join(
      aid_by_country_cso_type,
      by = join_by(gwcode == recipient_gwcode, year)
    ) |>
    # Treat NAs in aid as 0
    mutate(across(contains("oda"), \(x) ifelse(is.na(x), 0, x))) |>
    # Calculate aid proportions
    mutate(
      prop_contentious = oda_contentious_high / total_oda,
      prop_noncontentious = oda_contentious_low / total_oda,
      prop_cso = oda_cso_total / total_oda,
      prop_cso_int = oda_cso_international / total_oda,
      prop_cso_dom = oda_cso_donor / total_oda,
      prop_cso_foreign = (oda_cso_international + oda_cso_donor) / total_oda
    ) |>
    mutate(across(starts_with("prop_"), \(x) ifelse(is.nan(x), 0, x))) |>
    mutate(oda_per_capita = total_oda / population) |>
    mutate(across(
      c(total_oda, starts_with("oda_")),
      list(log = \(x) log1p(x))
    )) |>
    # Other datasets
    left_join(vdem_clean, by = join_by(gwcode, year)) |>
    left_join(ucdp_prio_clean, by = join_by(gwcode, year)) |>
    # Treat NAs in conflicts as FALSE
    mutate(
      internal_conflict = ifelse(
        is.na(internal_conflict),
        FALSE,
        internal_conflict
      )
    ) |>
    left_join(disasters_clean, by = join_by(gwcode, year)) |>
    # NAs in disasters are really 0, especially when occurrence is 0
    mutate(across(starts_with("natural_"), \(x) ifelse(is.na(x), 0, x))) |>
    # Add indicator for post-Cold War, since all the former Soviet republics
    # have no GDP data before 1990
    mutate(post_1989 = year >= 1990)

  # Make sure no extra rows were added through all that joining
  testthat::expect_equal(
    nrow(country_level_data),
    nrow(skeleton$panel_skeleton)
  )
}
