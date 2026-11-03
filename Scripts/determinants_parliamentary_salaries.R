# ==============================================================================
# EMPIRICAL STUDY OF THE DETERMINANTS OF PARLIAMENTARY SALARIES
# ==============================================================================

# Clear environment
rm(list = ls())
options(scipen = 999)

# Load custom functions from the Scripts folder
source("Scripts/functions.R")

# Load packages used in this script
packages <- c(
  "data.table", "plm", "lmtest", "car", "ggplot2", "readxl",
  "scales", "patchwork", "ggrepel", "fixest", "forcats",
  "modelsummary", "sandwich", "writexl", "countrycode", "WDI", "gt"
)

cargar_paquetes(packages)

# 1. LOAD DATA ####
mps_salary <- read_excel(
  "Data/DB/final_dataset_parlamentary_remunerations.xlsx",
  sheet = "balanced db",
  na = "#N/A",
  .name_repair = function(x) tolower(gsub(" |\\.", "_", x))
)

setDT(mps_salary)

# 1.1. Inspect data structure
str(mps_salary)

# 2. DATABASE CHECKS ####

# 2.1. Check for duplicated country-year observations
# Expected result: empty table
mps_salary[, .N, by = .(country, year)][N > 1]

# 3. DATA PREPARATION ####

# 3.1. Rename variables ####
setnames(
  mps_salary,
  old = c(
    "nominal_annual_gross_basic_salary",
    "nominal_annual_gross_basic_salary_usd",
    "annual_gross_basic_salary_ppp",
    "gdp_pc_ppp"
  ),
  new = c(
    "salary_nom_lcu",
    "salary_nom_usd_fmi",
    "salary_ppp_current_fmi",
    "gdp_pc_ppp_current_fmi"
  )
)

# Create ISO3 country codes in the parliamentary salary database
mps_salary[, iso3c := countrycode(country, origin = "country.name", destination = "iso3c")]

# Check unmatched countries
mps_salary[is.na(iso3c), unique(country)]

# 3.2. Download World Bank data ####

# Define the period required by the parliamentary salary dataset
start_year <- min(mps_salary$year, na.rm = TRUE)
end_year <- max(mps_salary$year, na.rm = TRUE)

# Define World Bank indicators
wb_indicators <- c(
  
  # GDP-based PPP conversion factor
  ppp_gdp_current_wb = "PA.NUS.PPP",
  
  # Household consumption PPP conversion factor
  ppp_consumption_current_wb = "PA.NUS.PRVT.PP",
  
  # GDP PPP
  gdp_ppp_current_wb = "NY.GDP.MKTP.PP.CD",
  
  gdp_ppp_constant_2021_wb = "NY.GDP.MKTP.PP.KD",
  
  # GDP per capita PPP
  gdp_pc_ppp_current_wb = "NY.GDP.PCAP.PP.CD",
  
  # Consumer Price Index
  cpi_wb = "FP.CPI.TOTL")

# Download World Development Indicators data
wb_data <- WDI(
  country = "all",
  indicator = wb_indicators,
  start = start_year,
  end = end_year,
  extra = TRUE
)

setDT(wb_data)

# Remove World Bank regional and income-group aggregates
wb_data <- wb_data[region != "Aggregates" &  !is.na(iso3c)]

# Ensure year is stored as integer
wb_data[ , year := as.integer(year)]

# 3.3. Construct derived World Bank variables ####

# Implied population
#
# GDP PPP in current international dollars divided by GDP per capita PPP
# in current international dollars gives the implied population.

wb_data[, population_implied_wb := gdp_ppp_current_wb/gdp_pc_ppp_current_wb]

# GDP per capita PPP in constant 2021 international dollars
wb_data[, gdp_pc_ppp_constant_2021_wb := gdp_ppp_constant_2021_wb/population_implied_wb]

# 3.4. Restrict World Bank data to the study sample ####

# Define countries included in the study
study_countries <- c(
  "Argentina",
  "Austria",
  "Australia",
  "Belgium",
  "Bolivia",
  "Brazil",
  "Canada",
  "Switzerland",
  "Chile",
  "Colombia",
  "Costa Rica",
  "Cyprus",
  "Czech Republic",
  "Germany",
  "Denmark",
  "Ecuador",
  "Estonia",
  "Spain",
  "Finland",
  "France",
  "United Kingdom",
  "Greece",
  "Guatemala",
  "Hungary",
  "Ireland",
  "Iceland",
  "Italy",
  "Japan",
  "Lithuania",
  "Latvia",
  "Mexico",
  "Netherlands",
  "Norway",
  "New Zealand",
  "Peru",
  "Poland",
  "Portugal",
  "Paraguay",
  "Sweden",
  "United States",
  "Uruguay",
  "Slovakia"
)

# Convert country names to ISO3 codes
study_iso3 <- countrycode(
  study_countries,
  origin = "country.name",
  destination = "iso3c"
)


# Keep only countries and years included in the parliamentary salary study
wb_study <- wb_data[
  iso3c %in% study_iso3 &
    year >= start_year &
    year <= end_year
]


# Check number of countries
uniqueN(wb_study$iso3c)

# Expected result: 42

# 3.5. Check World Bank data coverage ####

# Define key World Bank variables
key_wb_variables <- c(
  "gdp_ppp_current_wb",
  "ppp_consumption_current_wb",
  "gdp_ppp_current_wb",
  "gdp_ppp_constant_2021_wb",
  "gdp_pc_ppp_current_wb",
  "gdp_pc_ppp_constant_2021_wb",
  "cpi_wb"
)

# Count missing observations by variable
missing_summary_wb <- data.table(
  variable = key_wb_variables,
  
  missing = sapply(
    wb_study[, ..key_wb_variables],
    function(x) sum(is.na(x))
  ),
  
  total = nrow(wb_study)
)

missing_summary_wb[
  ,
  missing_pct :=
    100 * missing / total
]

missing_summary_wb

# Identify country-year observations with at least one missing key variable
missing_country_year_wb <- wb_study[
  !complete.cases(
    wb_study[, ..key_wb_variables]
  ),
  c(
    "country",
    "iso3c",
    "year",
    key_wb_variables
  ),
  with = FALSE
][
  order(
    iso3c,
    year
  )
]
missing_country_year_wb

# Detailed missing observations by country, year and variable
missing_detail_wb <- melt(
  wb_study,
  id.vars = c(
    "country",
    "iso3c",
    "year"
  ),
  measure.vars = key_wb_variables,
  variable.name = "variable",
  value.name = "value"
)[
  is.na(value)
][
  order(
    variable,
    iso3c,
    year
  )
]
missing_detail_wb

# Check whether all expected country-year rows are present
expected_wb_panel <- CJ(
  iso3c = study_iso3,
  year = seq(
    start_year,
    end_year
  )
)

missing_country_year_rows_wb <- fsetdiff(
  expected_wb_panel,
  wb_study[
    ,
    .(
      iso3c,
      year
    )
  ]
)

missing_country_year_rows_wb

# Expected result: empty table

# 3.6. Complete Argentina CPI series using CEPALSTAT ####

# Download monthly Consumer Price Index from CEPALSTAT
# CEPALSTAT indicator 365:
# Consumer Price Index (monthly)

cepal_cpi_file <- tempfile(
  fileext = ".xlsx"
)

download.file(
  url = paste0(
    "https://api-cepalstat.cepal.org/cepalstat/api/v1/",
    "indicator/365/data?format=excel&in=1&lang=en"
  ),
  destfile = cepal_cpi_file,
  mode = "wb"
)

# Read CEPALSTAT CPI data
cpi_cepal <- read_excel(
  cepal_cpi_file,
  .name_repair = function(x) {
    tolower(
      gsub(
        "[^[:alnum:]]+",
        "_",
        x
      )
    )
  }
)

setDT(cpi_cepal)

# Keep Argentina and the study period
cpi_argentina_cepal <- cpi_cepal[
  country_estandar == "Argentina" &
    as.integer(years_estandar) >= start_year &
    as.integer(years_estandar) <= end_year
]

# Create numeric year variable
cpi_argentina_cepal[
  ,
  year :=
    as.integer(
      years_estandar
    )
]

# Construct annual average CPI
#
# The World Bank annual CPI is reported as a period average.
# Therefore, the CEPALSTAT monthly CPI for Argentina is converted
# to an annual index using the arithmetic mean of the 12 monthly values.

cpi_argentina_annual_cepal <- cpi_argentina_cepal[
  ,
  .(
    cpi_argentina_cepal =
      mean(
        value,
        na.rm = FALSE
      ),
    
    months_available =
      sum(
        !is.na(value)
      )
  ),
  by = year
][
  order(year)
]

# Check that every annual CPI is based on 12 monthly observations
cpi_argentina_annual_cepal[
  months_available != 12
]

# Expected result: empty table


# Add Argentina ISO3 code
cpi_argentina_annual_cepal[
  ,
  iso3c := "ARG"
]


# Create finalized macroeconomic database
#
# At this point the database is no longer exclusively World Bank data
# because Argentina's CPI is sourced from CEPALSTAT.

macro_data <- copy(
  wb_study
)

# Add Argentina CPI from CEPALSTAT
macro_data[
  cpi_argentina_annual_cepal,
  on = .(
    iso3c,
    year
  ),
  cpi_argentina_cepal :=
    i.cpi_argentina_cepal
]

# Create final CPI variable
#
# World Bank CPI is used for all countries except Argentina.
# CEPALSTAT CPI is used for Argentina because WDI does not provide
# a complete CPI series for the study period.
macro_data[
  ,
  cpi_final :=
    fifelse(
      iso3c == "ARG",
      cpi_argentina_cepal,
      cpi_wb
    )
]

# Record CPI source
macro_data[
  ,
  cpi_source :=
    fifelse(
      iso3c == "ARG",
      "CEPALSTAT",
      "World Bank WDI"
    )
]

# Check final CPI coverage
macro_data[
  is.na(cpi_final),
  .(
    country,
    iso3c,
    year
  )
]

# Expected result: empty table

# 3.7. Construct PPP conversion factor at constant 2021 prices ####

# Obtain the 2021 household consumption PPP for each country
#
# The 2021 PPP is used as the spatial price-level anchor.

macro_data[
  ,
  ppp_consumption_2021_wb :=
    ppp_consumption_current_wb[
      match(
        2021L,
        year
      )
    ],
  by = iso3c
]


# Obtain the 2021 CPI for each country
#
# The CPI ratio is used to express each year's local currency values
# in 2021 domestic prices.

macro_data[
  ,
  cpi_2021 :=
    cpi_final[
      match(
        2021L,
        year
      )
    ],
  by = iso3c
]

# Check that base-year values are available for all countries
macro_data[
  is.na(ppp_consumption_2021_wb) |
    is.na(cpi_2021),
  unique(
    .(
      country,
      iso3c,
      ppp_consumption_2021_wb,
      cpi_2021
    )
  )
]

# Expected result: empty table

# Construct household consumption PPP conversion factor
# at constant 2021 prices
#
# Units:
# local currency units per constant 2021 international dollar
#
# Formula:
#
# PPP_constant_it =
# PPP_current_i,2021 * (CPI_it / CPI_i,2021)

macro_data[
  ,
  ppp_consumption_constant_2021_wb :=
    ppp_consumption_2021_wb *
    (
      cpi_final /
        cpi_2021
    )
]


# Validate the construction in the 2021 base year
#
# In 2021, the current PPP and the constant-2021 PPP conversion factor
# must be equal by construction.
ppp_base_year_check <- macro_data[
  year == 2021,
  .(
    country,
    iso3c,
    ppp_consumption_current_wb,
    ppp_consumption_constant_2021_wb,
    
    difference =
      ppp_consumption_constant_2021_wb -
      ppp_consumption_current_wb
  )
][
  order(country)
]

ppp_base_year_check


# Numerical equality check
all.equal(
  macro_data[
    year == 2021,
    ppp_consumption_constant_2021_wb
  ],
  macro_data[
    year == 2021,
    ppp_consumption_current_wb
  ]
)

# 3.8. Merge external variables into the parliamentary salary database ####

# Select macroeconomic variables required for the empirical analysis
macro_merge <- macro_data[
  ,
  .(
    iso3c,
    year,
    
    # PPP conversion factors
    gdp_ppp_current_wb,
    ppp_consumption_current_wb,
    ppp_consumption_2021_wb,
    ppp_consumption_constant_2021_wb,
    
    # CPI
    cpi_final,
    cpi_source,
    
    # GDP PPP
    gdp_ppp_current_wb,
    gdp_ppp_constant_2021_wb,
    
    # GDP per capita PPP
    gdp_pc_ppp_current_wb,
    gdp_pc_ppp_constant_2021_wb,
    
    # Population
    population_implied_wb
  )
]

# Merge external macroeconomic variables into parliamentary salary database
#
# The update join preserves the original number of rows in mps_salary.
mps_salary[
  macro_merge,
  on = .(
    iso3c,
    year
  ),
  `:=`(
    
    ppp_consumption_current_wb =
      i.ppp_consumption_current_wb,
    
    ppp_consumption_2021_wb =
      i.ppp_consumption_2021_wb,
    
    ppp_consumption_constant_2021_wb =
      i.ppp_consumption_constant_2021_wb,
    
    cpi_final =
      i.cpi_final,
    
    cpi_source =
      i.cpi_source,
    
    gdp_ppp_current_wb =
      i.gdp_ppp_current_wb,
    
    gdp_ppp_constant_2021_wb =
      i.gdp_ppp_constant_2021_wb,
    
    gdp_pc_ppp_current_wb =
      i.gdp_pc_ppp_current_wb,
    
    gdp_pc_ppp_constant_2021_wb =
      i.gdp_pc_ppp_constant_2021_wb,
    
    population_implied_wb =
      i.population_implied_wb
  )
]


# 3.9. Construct parliamentary salary variables ####

# Parliamentary salary in current international dollars
# using World Bank household consumption PPP
#
# This variable may contain missing observations where the World Bank
# current household-consumption PPP series is unavailable.

mps_salary[
  ,
  salary_ppp_current_wb :=
    salary_nom_lcu /
    ppp_consumption_current_wb
]

# Parliamentary salary in constant 2021 international dollars
# using the household-consumption PPP conversion factor constructed above.

mps_salary[
  ,
  salary_ppp_constant_2021_wb :=
    salary_nom_lcu /
    ppp_consumption_constant_2021_wb
]


# 3.10. Final data quality checks ####

# Check missing observations in the main constant-price variables

mps_salary[
  is.na(
    salary_ppp_constant_2021_wb
  ) |
    is.na(
      gdp_pc_ppp_constant_2021_wb
    ),
  .(
    country,
    iso3c,
    year,
    salary_nom_lcu,
    salary_ppp_constant_2021_wb,
    gdp_pc_ppp_constant_2021_wb
  )
]


# Summary statistics for parliamentary salary at constant 2021 prices

summary(
  mps_salary$salary_ppp_constant_2021_wb
)


# Summary statistics for GDP per capita PPP at constant 2021 prices

summary(
  mps_salary$gdp_pc_ppp_constant_2021_wb
)


# Inspect lowest and highest parliamentary salaries
# in constant 2021 international dollars

salary_extremes <- mps_salary[
  !is.na(
    salary_ppp_constant_2021_wb
  )
][
  order(
    salary_ppp_constant_2021_wb
  ),
  .(
    country,
    year,
    salary_nom_lcu,
    salary_ppp_constant_2021_wb
  )
]

salary_extremes[
  c(
    1:min(10, .N),
    max(1, .N - 9):.N
  )
]


# Check whether current and constant PPP salary measures coincide in 2021
#
# They should coincide when the World Bank current consumption PPP
# is available for that country.

mps_salary[
  year == 2021 &
    !is.na(
      salary_ppp_current_wb
    ),
  .(
    country,
    salary_ppp_current_wb,
    salary_ppp_constant_2021_wb,
    
    difference =
      salary_ppp_constant_2021_wb -
      salary_ppp_current_wb
  )
][
  order(country)
]


# Compare IMF and World Bank GDP-based pc current PPP conversion factors
mps_salary[
  !is.na(
    gdp_pc_ppp_current_fmi
  ) &
    !is.na(
      gdp_pc_ppp_current_wb
    ),
  .(
    country,
    year,
    gdp_pc_ppp_current_fmi,
    gdp_pc_ppp_current_wb,
    
    ratio_fmi_wb =
      gdp_pc_ppp_current_fmi /
      gdp_pc_ppp_current_wb
  )
][
  order(
    country,
    year
  )
]

# 3.11. Create final analysis dataset ####

# Keep only variables required for descriptive statistics, figures,
# econometric analyses, robustness checks, and sample definitions.

analysis_variables <- c(
  
  # ---
  # Panel identifiers
  # ---
  "country",
  "iso3c",
  "year",
  
  # ---
  # Sample and institutional characteristics
  # ---
  "chamber_type",
  "government_type",
  "structure",
  "ocde",
  
  
  # ---
  # Parliamentary salary
  # ---
  
  # Nominal salary in local currency
  "salary_nom_lcu",
  
  # Nominal salary converted to USD
  "salary_nom_usd_fmi",
  
  # Salary in current international dollars - IMF GDP PPP
  "salary_ppp_current_fmi",
  
  # Salary in current international dollars - World Bank consumption PPP
  "salary_ppp_current_wb",
  
  # Salary in constant 2021 international dollars - World Bank consumption PPP
  "salary_ppp_constant_2021_wb",
  
  # ---
  # Economic development
  # ---
  
  # GDP per capita PPP - IMF, current international dollars
  "gdp_pc_ppp_current_fmi",
  
  # GDP per capita PPP - World Bank, current international dollars
  "gdp_pc_ppp_current_wb",
  
  # GDP per capita PPP - World Bank, constant 2021 international dollars
  "gdp_pc_ppp_constant_2021_wb",
  
  # GDP per capita in current USD
  "gdp_pc_current_usd",
  
  # ---
  # Demographic and parliamentary controls
  # ---
  
  "population",
  "sits_number",
  
  # ---
  # Salary-setting mechanism
  # ---
  
  # Dickinson classification
  "mecanismo_fijacion"
  
)

# Check whether all selected variables exist

setdiff(
  analysis_variables,
  names(mps_salary)
)

# Create final analysis dataset
analysis_data <- mps_salary[
  ,
  ..analysis_variables
]

# 3.12. Define categorical variables and reference categories ####

# Rename the salary-setting mechanism variable to English

setnames(
  analysis_data,
  old = "mecanismo_fijacion",
  new = "salary_setting_mechanism"
)

# Salary-setting mechanism
#
# Reference category: Self-determined
analysis_data[
  ,
  salary_setting_mechanism := factor(
    salary_setting_mechanism,
    levels = c(
      "Autodeterminado",
      "Asistido",
      "Referenciado",
      "Independiente"
    ),
    labels = c(
      "Self-determined",
      "Assisted",
      "Referenced",
      "Independent"
    ),
    ordered = FALSE
  )
]

# Government system
#
# Reference category: Parliamentary system
analysis_data[
  ,
  government_type := factor(
    government_type,
    levels = c(
      "Parliamentary system",
      "Presidential system",
      "Presidential-Parliamentary"
    ),
    ordered = FALSE
  )
]

# Parliamentary structure
#
# Reference category: Unicameral
analysis_data[
  ,
  structure := factor(
    structure,
    levels = c(
      "Unicameral",
      "Bicameral"
    ),
    ordered = FALSE
  )
]

# 3.13. Construct logarithmic variables ####

# Log transformations are used for monetary variables and continuous controls.
log_variables <- c(
  
  # Parliamentary salary
  "salary_nom_usd_fmi",
  "salary_ppp_current_fmi",
  "salary_ppp_current_wb",
  "salary_ppp_constant_2021_wb",
  
  # GDP per capita
  "gdp_pc_current_usd",
  "gdp_pc_ppp_current_fmi",
  "gdp_pc_ppp_current_wb",
  "gdp_pc_ppp_constant_2021_wb",
  
  # Controls
  "population",
  "sits_number"
)

# Construct natural logarithms
# Non-positive values are converted to NA because their logarithm
# is undefined.
analysis_data[
  ,
  (paste0("log_", log_variables)) :=
    lapply(
      .SD,
      function(x) {
        fifelse(
          !is.na(x) & x > 0,
          log(x),
          NA_real_
        )
      }
    ),
  .SDcols = log_variables
]

# 3.14. Construct country means for Mundlak CRE models ####

# The Mundlak correlated random-effects specification includes country means
# of the time-varying continuous covariates.

mundlak_variables <- c("log_gdp_pc_ppp_constant_2021_wb",
                       "log_gdp_pc_ppp_current_fmi",
                       "log_gdp_pc_ppp_current_wb")

analysis_data[
  ,
  (paste0("mean_", mundlak_variables)) :=
    lapply(
      .SD,
      function(x) {
        
        if (all(is.na(x))) {
          NA_real_
        } else {
          mean(x, na.rm = TRUE)
        }
        
      }
    ),
  by = country,
  .SDcols = mundlak_variables
]


# Check that Mundlak means are constant within country

analysis_data[
  ,
  .(
    n_mean_gdp =
      uniqueN(mean_log_gdp_pc_ppp_constant_2021_wb)
    
  ),
  by = country
][
  n_mean_gdp != 1
]

# Expected result: empty table

# 3.15. Classify countries by region ####

# Regional groups are mutually exclusive.
#
# Mexico is classified as Latin America, consistent with the regional
# classification effectively used in the previous version of the analysis.


latin_america <- c(
  "Argentina",
  "Bolivia",
  "Brazil",
  "Chile",
  "Colombia",
  "Costa Rica",
  "Ecuador",
  "Guatemala",
  "Mexico",
  "Paraguay",
  "Peru",
  "Uruguay"
)

north_america <- c(
  "Canada",
  "United States"
)

western_europe <- c(
  "Austria",
  "Belgium",
  "Switzerland",
  "Germany",
  "France",
  "Finland",
  "Ireland",
  "Italy",
  "Portugal",
  "Spain",
  "United Kingdom",
  "Denmark",
  "Netherlands",
  "Norway",
  "Sweden",
  "Iceland",
  "Greece",
  "Cyprus"
)

eastern_europe <- c(
  "Czech Republic",
  "Estonia",
  "Hungary",
  "Lithuania",
  "Latvia",
  "Poland",
  "Slovakia"
)

asia_oceania <- c(
  "Australia",
  "New Zealand",
  "Japan"
)

# Create country-region lookup table

region_lookup <- rbindlist(
  list(
    
    data.table(
      country = western_europe,
      region = "Western Europe"
    ),
    
    data.table(
      country = eastern_europe,
      region = "Eastern Europe"
    ),
    
    data.table(
      country = north_america,
      region = "North America"
    ),
    
    data.table(
      country = asia_oceania,
      region = "Asia and Oceania"
    ),
    
    data.table(
      country = latin_america,
      region = "Latin America"
    )
  )
)

# Add region to the analysis dataset

analysis_data[
  region_lookup,
  on = "country",
  region := i.region
]

# Convert region to factor
#
# Reference category: Western Europe
analysis_data[
  ,
  region := factor(
    region,
    levels = c(
      "Western Europe",
      "Eastern Europe",
      "North America",
      "Asia and Oceania",
      "Latin America"
    ),
    ordered = FALSE
  )
]

# Check regional distribution
analysis_data[
  ,
  .(
    countries = uniqueN(country),
    observations = .N
  ),
  by = region
][
  order(region)
]


# 3.16. Final preparation checks ####

# Sort the final analysis dataset by country and year
setorder(
  analysis_data,
  country,
  year
)

# Check for duplicated country-year observations
analysis_data[
  ,
  .N,
  by = .(
    country,
    year
  )
][
  N > 1
]
# Expected result: empty table

# Check number of countries
uniqueN(
  analysis_data$country
)
# Expected result: 42

# Check number of country-year observations
nrow(
  analysis_data
)
# Expected result for 42 countries over 2001-2024: 1008


# Check the main categorical variables
table(
  analysis_data$salary_setting_mechanism,
  useNA = "ifany"
)

# 3.17. Save final analysis dataset ####

# Save the analysis-ready dataset used for descriptive statistics,
# figures and econometric models.

write.csv2(analysis_data, "Datos/BD/parliamentary_salary_analysis_data.csv", row.names = F)
analysis_data <- read.csv2("Datos/DB/parliamentary_salary_analysis_data.csv") 
setDT(analysis_data)

# 4. DESCRIPTIVE STATISTICS ####

# 4.1. Distribution of salary-setting mechanisms over time ####

# Number of countries in each salary-setting mechanism by year
mechanism_by_year <- analysis_data[
  , .(countries = uniqueN(country)),
  by = .(salary_setting_mechanism, year)
]

mechanism_by_year_wide <- dcast(
  mechanism_by_year,
  salary_setting_mechanism ~ year,
  value.var = "countries",
  fill = 0
)

mechanism_by_year_wide

# 4.2. Distribution of salary-setting mechanisms in 2024 ####

# Number and percentage of countries in each mechanism in the final study year
mechanism_2024 <- analysis_data[
  year == 2024,
  .(countries = uniqueN(country)),
  by = salary_setting_mechanism
][
  , share_pct := 100 * countries / sum(countries)
][
  order(salary_setting_mechanism)
]

mechanism_2024

# 4.3. Parliamentary salary descriptive statistics by year ####

# Function to calculate annual descriptive statistics for alternative
# parliamentary salary measures
descriptive_stats_by_year <- function(data, variable) {
  
  data[, {
    x <- get(variable)
    x <- x[!is.na(x)]
    
    .(
      observations = length(x),
      mean = if (length(x) > 0) mean(x) else NA_real_,
      median = if (length(x) > 0) median(x) else NA_real_,
      sd = if (length(x) > 1) sd(x) else NA_real_,
      min = if (length(x) > 0) min(x) else NA_real_,
      max = if (length(x) > 0) max(x) else NA_real_
    )
  }, by = year][order(year)]
}

# 4.3.1. Current international dollars - IMF GDP PPP
salary_ppp_current_fmi_year <- descriptive_stats_by_year(
  analysis_data, "salary_ppp_current_fmi"
)
salary_ppp_current_fmi_year

# 4.3.2. Current international dollars - World Bank consumption PPP
salary_ppp_current_wb_year <- descriptive_stats_by_year(
  analysis_data, "salary_ppp_current_wb"
)
salary_ppp_current_wb_year

# 4.3.3. Constant 2021 international dollars - World Bank consumption PPP
salary_ppp_constant_2021_wb_year <- descriptive_stats_by_year(
  analysis_data, "salary_ppp_constant_2021_wb"
)
salary_ppp_constant_2021_wb_year

# 4.4. Save annual descriptive statistics ####
write.csv2(
  salary_ppp_current_fmi_year,
  "Outputs/salary_ppp_current_fmi_by_year.csv",
  row.names = FALSE
)

write.csv2(
  salary_ppp_current_wb_year,
  "Outputs/salary_ppp_current_wb_by_year.csv",
  row.names = FALSE
)

write.csv2(
  salary_ppp_constant_2021_wb_year,
  "Outputs/salary_ppp_constant_2021_wb_by_year.csv",
  row.names = FALSE
)

# 4.5. Countries with minimum and maximum parliamentary salaries by year ####

# Function to identify countries with minimum and maximum parliamentary
# salaries in each year. Ties are retained.
salary_extremes_by_year <- function(data, variable) {
  
  data[!is.na(get(variable)), {
    x <- get(variable)
    min_value <- min(x)
    max_value <- max(x)
    
    rbind(
      data.table(
        extreme = "Minimum",
        country = country[x == min_value],
        salary = x[x == min_value]
      ),
      data.table(
        extreme = "Maximum",
        country = country[x == max_value],
        salary = x[x == max_value]
      )
    )
  }, by = year][order(year, extreme, country)]
}

# 4.5.1. Current international dollars - IMF GDP PPP
salary_extremes_current_fmi <- salary_extremes_by_year(
  analysis_data, "salary_ppp_current_fmi"
)
salary_extremes_current_fmi

# 4.5.2. Current international dollars - World Bank consumption PPP
salary_extremes_current_wb <- salary_extremes_by_year(
  analysis_data, "salary_ppp_current_wb"
)
salary_extremes_current_wb

# 4.5.3. Constant 2021 international dollars - World Bank consumption PPP
salary_extremes_constant_2021_wb <- salary_extremes_by_year(
  analysis_data, "salary_ppp_constant_2021_wb"
)
salary_extremes_constant_2021_wb

# 4.6. Compare alternative PPP salary measures ####

# 4.6.1. Overall comparison
salary_measure_comparison <- analysis_data[, .(
  current_fmi_observations = sum(!is.na(salary_ppp_current_fmi)),
  current_fmi_mean = mean(salary_ppp_current_fmi, na.rm = TRUE),
  
  current_wb_observations = sum(!is.na(salary_ppp_current_wb)),
  current_wb_mean = mean(salary_ppp_current_wb, na.rm = TRUE),
  
  constant_2021_wb_observations = sum(!is.na(salary_ppp_constant_2021_wb)),
  constant_2021_wb_mean = mean(salary_ppp_constant_2021_wb, na.rm = TRUE)
)]
salary_measure_comparison

# 4.6.2. Compare annual mean parliamentary salaries
salary_means_comparison <- merge(
  salary_ppp_current_fmi_year[
    , .(year, mean_salary_ppp_current_fmi = mean)
  ],
  salary_ppp_current_wb_year[
    , .(year, mean_salary_ppp_current_wb = mean)
  ],
  by = "year",
  all = TRUE
)

salary_means_comparison <- merge(
  salary_means_comparison,
  salary_ppp_constant_2021_wb_year[
    , .(year, mean_salary_ppp_constant_2021_wb = mean)
  ],
  by = "year",
  all = TRUE
)

setorder(salary_means_comparison, year)

salary_means_comparison

# 4.7. GDP per capita descriptive statistics by year ####

# Use the same function to compare alternative GDP per capita PPP measures
gdp_pc_ppp_current_fmi_year <- descriptive_stats_by_year(
  analysis_data, "gdp_pc_ppp_current_fmi"
)

gdp_pc_ppp_current_wb_year <- descriptive_stats_by_year(
  analysis_data, "gdp_pc_ppp_current_wb"
)

gdp_pc_ppp_constant_2021_wb_year <- descriptive_stats_by_year(
  analysis_data, "gdp_pc_ppp_constant_2021_wb"
)
gdp_pc_ppp_current_fmi_year
gdp_pc_ppp_current_wb_year
gdp_pc_ppp_constant_2021_wb_year

# 4.8. Descriptive statistics for the main continuous variables ####

# Overall statistics for variables potentially used in the econometric analysis
main_continuous_variables <- c(
  "salary_ppp_current_fmi",
  "salary_ppp_current_wb",
  "salary_ppp_constant_2021_wb",
  "gdp_pc_ppp_current_fmi",
  "gdp_pc_ppp_current_wb",
  "gdp_pc_ppp_constant_2021_wb"
)

descriptive_statistics <- rbindlist(
  lapply(main_continuous_variables, function(variable) {
    
    x <- analysis_data[[variable]]
    x <- x[!is.na(x)]
    
    data.table(
      variable = variable,
      observations = length(x),
      mean = mean(x),
      median = median(x),
      sd = sd(x),
      min = min(x),
      max = max(x)
    )
  })
)

descriptive_statistics

# 4.9. Descriptive statistics by salary-setting mechanism ####

# Compare parliamentary salary levels across salary-setting mechanisms.
# Both current and constant PPP measures are retained at this stage.
salary_by_mechanism <- analysis_data[, .(
  observations = .N,
  
  mean_salary_current_fmi =
    mean(salary_ppp_current_fmi, na.rm = TRUE),
  
  median_salary_current_fmi =
    median(salary_ppp_current_fmi, na.rm = TRUE),
  
  mean_salary_current_wb =
    mean(salary_ppp_current_wb, na.rm = TRUE),
  
  median_salary_current_wb =
    median(salary_ppp_current_wb, na.rm = TRUE),
  
  mean_salary_constant_2021_wb =
    mean(salary_ppp_constant_2021_wb, na.rm = TRUE),
  
  median_salary_constant_2021_wb =
    median(salary_ppp_constant_2021_wb, na.rm = TRUE)
  
), by = salary_setting_mechanism][
  order(salary_setting_mechanism)
]

salary_by_mechanism

# 4.10. Save main descriptive statistics tables ####

write.csv2(
  mechanism_by_year_wide,
  "Outputs/salary_setting_mechanism_by_year.csv",
  row.names = FALSE
)

write.csv2(
  mechanism_2024,
  "Outputs/salary_setting_mechanism_2024.csv",
  row.names = FALSE
)

write.csv2(
  salary_means_comparison,
  "Outputs/salary_ppp_measures_comparison_by_year.csv",
  row.names = FALSE
)

write.csv2(
  descriptive_statistics,
  "Outputs/descriptive_statistics.csv",
  row.names = FALSE
)

write.csv2(
  salary_by_mechanism,
  "Outputs/salary_by_setting_mechanism.csv",
  row.names = FALSE
)

# 5. VISUALISATIONS ####

# Years shown in distribution figures
# Selected benchmark years are used instead of plotting all 24 annual
# distributions simultaneously, improving readability.

figure_years <- c(2001, 2006, 2012, 2018, 2024)

figure_data <- analysis_data[year %in% figure_years]

# 5.1. Distribution of nominal parliamentary salaries ####

# Parliamentary salaries converted to current USD using market exchange rates.

fig_5_1 <- ggplot(
  figure_data,
  aes(
    x = salary_ppp_current_fmi,
    colour = factor(year),
    fill = factor(year)
  )
) +
  geom_density(linewidth = 0.8, alpha = 0.10, na.rm = TRUE) +
  scale_x_continuous(
    labels = label_dollar(
      prefix = "$",
      accuracy = 1,
      scale_cut = cut_short_scale()
    ),
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  labs(
    x = "Annual gross basic salary (current USD PPP)",
    y = "Density",
    colour = "Year",
    fill = "Year"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

fig_5_1

# 5.2. Distribution of parliamentary salaries in PPP terms ####

# Parliamentary salaries expressed in constant 2021 international dollars
# using the World Bank household-consumption PPP conversion.
fig_5_2 <- ggplot(
  figure_data,
  aes(
    x = salary_ppp_constant_2021_wb,
    colour = factor(year),
    fill = factor(year)
  )
) +
  geom_density(linewidth = 0.8, alpha = 0.10, na.rm = TRUE) +
  scale_x_continuous(
    labels = label_dollar(
      prefix = "$",
      accuracy = 1,
      scale_cut = cut_short_scale()
    ),
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  labs(
    x = "Annual gross basic salary (constant 2021 international dollars)",
    y = "Density",
    colour = "Year",
    fill = "Year"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

fig_5_2

# 5.3. Parliamentary pay and GDP per capita in 2024 ####

# Cross-sectional relationship between parliamentary pay and GDP per capita
# in market-exchange-rate USD and PPP-adjusted international dollars.
d24 <- analysis_data[year == 2024]

# Countries highlighted in the discussion of the figure
highlight_countries <- c("COL", "IRL")
d24[, highlight := iso3c %in% highlight_countries]

# Correlations displayed in each panel
corr_current <- cor(
  d24$salary_nom_usd_fmi,
  d24$gdp_pc_current_usd,
  use = "complete.obs"
)

corr_ppp <- cor(
  d24$salary_ppp_current_fmi,
  d24$gdp_pc_ppp_current_fmi,
  use = "complete.obs"
)

# Shared axis limits across panels to facilitate visual comparison
x_lo <- min(c(
  d24$gdp_pc_current_usd,
  d24$gdp_pc_ppp_current_fmi
), na.rm = TRUE) * 0.80

x_hi <- max(c(
  d24$gdp_pc_current_usd,
  d24$gdp_pc_ppp_current_fmi
), na.rm = TRUE) * 1.04

y_lo <- min(c(
  d24$salary_nom_usd_fmi,
  d24$salary_ppp_current_fmi
), na.rm = TRUE) * 0.60

y_hi <- max(c(
  d24$salary_nom_usd_fmi,
  d24$salary_ppp_current_fmi
), na.rm = TRUE) * 1.05

# Axis formatting
usd <- dollar_format(
  prefix = "$",
  big.mark = ",",
  decimal.mark = ".",
  accuracy = 1
)

# Figure colours
col_point  <- "#3E6D8E"   # Other countries
col_hilite <- "#C6603C"   # Colombia and Ireland
col_line   <- "#1F3B52"   # Linear fit and confidence interval

# Common theme
theme_fig <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 13.5, face = "bold", colour = "#222222"),
    axis.title = element_text(size = 11.5),
    axis.text = element_text(size = 9.5, colour = "#444444"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "#E6E6E6", linewidth = 0.4),
    axis.line = element_line(colour = "#9A9A9A", linewidth = 0.4),
    plot.margin = margin(8, 10, 8, 8),
    legend.position = "none"
  )

# Function to construct each panel
make_panel <- function(xvar, yvar, r, title, xlab, ylab) {
  
  ggplot(d24, aes(x = .data[[xvar]], y = .data[[yvar]])) +
    
    # Linear association
    geom_smooth(
      method = "lm", formula = y ~ x, se = TRUE,
      colour = col_line, fill = col_line,
      alpha = 0.10, linewidth = 0.8
    ) +
    
    # Countries not highlighted
    geom_point(
      data = d24[highlight == FALSE],
      colour = col_point, alpha = 0.85, size = 2.4
    ) +
    
    # Highlighted countries
    geom_point(
      data = d24[highlight == TRUE],
      colour = col_hilite, size = 3.6
    ) +
    
    # Country labels
    geom_text_repel(
      data = d24[highlight == FALSE],
      aes(label = iso3c),
      size = 2.5, colour = "#858585",
      max.overlaps = Inf,
      box.padding = 0.25,
      point.padding = 0.15,
      segment.colour = "#CCCCCC",
      segment.size = 0.2
    ) +
    
    # Highlighted country labels
    geom_text_repel(
      data = d24[highlight == TRUE],
      aes(label = iso3c),
      size = 3.2, colour = col_hilite,
      fontface = "bold",
      box.padding = 0.35,
      point.padding = 0.25,
      max.overlaps = Inf,
      segment.colour = col_hilite,
      segment.size = 0.3
    ) +
    
    # Pearson correlation
    annotate(
      "label",
      x = x_hi, y = y_lo,
      hjust = 1, vjust = 0,
      label = paste0("r = ", sprintf("%.2f", r)),
      fontface = "italic",
      size = 3.8,
      fill = "white"
    ) +
    
    coord_cartesian(
      xlim = c(x_lo, x_hi),
      ylim = c(y_lo, y_hi),
      clip = "off"
    ) +
    
    scale_x_continuous(labels = usd) +
    scale_y_continuous(labels = usd) +
    
    labs(
      title = title,
      x = xlab,
      y = ylab
    ) +
    
    theme_fig
}

# Panel (a): Market exchange rates
g_current <- make_panel(
  xvar = "gdp_pc_current_usd",
  yvar = "salary_nom_usd_fmi",
  r = corr_current,
  title = "(a) Current USD",
  xlab = "GDP per capita (current USD)",
  ylab = "Annual gross parliamentary pay (current USD)"
)

# Panel (b): Purchasing power parity
g_ppp <- make_panel(
  xvar = "gdp_pc_ppp_current_fmi",
  yvar = "salary_ppp_current_fmi",
  r = corr_ppp,
  title = "(b) Current international dollars (PPP)",
  xlab = "GDP per capita (current international dollars)",
  ylab = "Annual gross parliamentary pay (current international dollars)"
)

# Combine panels
graph1 <- (g_current | g_ppp) +
  plot_annotation(
    title = "Graph 1. Relationship between parliamentary pay and GDP per capita (2024)",
    caption = paste0(
      "Source: Prepared by the authors. Parliamentary pay data collected by the authors; ",
      "GDP per capita and PPP conversion factors from the IMF World Economic Outlook ",
      "(WEO), April 2025."
    ),
    theme = theme(
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      plot.caption = element_text(size = 9.5, hjust = 0, colour = "#555555",
                                  margin = margin(t = 8))
    )
  )
graph1

# Save figure
ggsave(
  "Outputs/graph1_pay_vs_gdp_2024.svg",
  plot = graph1,
  device = svglite::svglite,
  width = 11,
  height = 5,
  units = "in"
)

# 5.4. Type of government and structure of the legislative branch ####

# 5.4.1. Prepare institutional data and common settings 
# Government type and legislative structure are time-invariant during
# the 2001-2024 study period. Create one observation per country.
institutional_data <- unique(
  analysis_data[, .(country, government_type, structure)]
)

# Define a common country order:
# government type, legislative structure, and country name
institutional_data[, government_rank := match(
  government_type,
  c("Parliamentary system", "Presidential system", "Presidential-Parliamentary")
)]

institutional_data[, structure_rank := match(
  structure,
  c("Bicameral", "Unicameral")
)]

setorder(institutional_data, government_rank, structure_rank, country)

# Reverse levels so parliamentary systems appear at the top
country_levels <- rev(institutional_data$country)

# Country counts by government type
government_counts <- institutional_data[
  , .(countries = .N),
  by = government_type
]

government_labels_short <- c(
  "Parliamentary system"       = "Parliamentary",
  "Presidential system"        = "Presidential",
  "Presidential-Parliamentary" = "Presidential-parliamentary"
)

government_counts[, label := paste0(
  government_labels_short[as.character(government_type)],
  " (", countries, ")"
)]

labels_government <- setNames(
  government_counts$label,
  as.character(government_counts$government_type)
)

# Country counts by legislative structure
structure_counts <- institutional_data[
  , .(countries = .N),
  by = structure
]

structure_counts[, label := paste0(
  as.character(structure),
  " (", countries, ")"
)]

labels_structure <- setNames(
  structure_counts$label,
  as.character(structure_counts$structure)
)


# Colourblind-safe palettes
government_colours <- c(
  "Parliamentary system"       = "#4E79A7",
  "Presidential system"        = "#F28E2B",
  "Presidential-Parliamentary" = "#B07AA1"
)

structure_colours <- c(
  "Bicameral"  = "#9C755F",
  "Unicameral" = "#76B7B2"
)

# 5.4.2. Alternative 1: Institutional heatmap 
x_breaks <- c(2001, 2006, 2012, 2018, 2024)

# Common heatmap theme
theme_heatmap <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 13, hjust = 0.5, colour = "#222222"),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 10, colour = "#444444"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  )

# Panel (a): Type of government
g_government_heatmap <- ggplot(
  analysis_data,
  aes(
    x = year,
    y = factor(country, levels = country_levels),
    fill = government_type
  )
) +
  geom_tile(width = 1, height = 1, colour = "white", linewidth = 0.15) +
  scale_x_continuous(
    breaks = x_breaks,
    limits = c(2000.5, 2024.5),
    expand = c(0, 0)
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_manual(
    values = government_colours,
    labels = labels_government,
    breaks = names(government_colours),
    drop = FALSE
  ) +
  labs(title = "(a) Type of government") +
  guides(fill = guide_legend(nrow = 1)) +
  theme_heatmap +
  theme(
    axis.text.y = element_text(
      size = 7.5,
      colour = "#333333",
      margin = margin(r = 2)
    )
  )

g_government_heatmap

# Panel (b): Structure of the legislative branch
g_structure_heatmap <- ggplot(
  analysis_data,
  aes(
    x = year,
    y = factor(country, levels = country_levels),
    fill = structure
  )
) +
  geom_tile(width = 1, height = 1, colour = "white", linewidth = 0.15) +
  scale_x_continuous(
    breaks = x_breaks,
    limits = c(2000.5, 2024.5),
    expand = c(0, 0)
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_manual(
    values = structure_colours,
    labels = labels_structure,
    breaks = names(structure_colours),
    drop = FALSE
  ) +
  labs(title = "(b) Structure of the legislative branch") +
  guides(fill = guide_legend(nrow = 1)) +
  theme_heatmap +
  theme(axis.text.y = element_blank())

g_structure_heatmap


# Combine panels
graph2_heatmap <- (g_government_heatmap | g_structure_heatmap) +
  plot_annotation(
    title = "Graph 2. Type of government and structure of the legislative branch (2001–2024)",
    caption = paste0(
      "Source: Prepared by the authors based on data from IPU Parline.\n",
      "Note: Institutional classifications do not change during the study period. ",
      "Figures in parentheses indicate the number of countries in each category."
    ),
    theme = theme(
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      plot.caption = element_text(
        size = 10, hjust = 0, colour = "#555555",
        margin = margin(t = 8)
      )
    )
  )

graph2_heatmap

# Save figure
ggsave(
  "Outputs/graph2_government_structure_heatmap_2001_2024.svg",
  plot = graph2_heatmap,
  device = svglite::svglite,
  width = 11,
  height = 8,
  units = "in"
)

# 5.4.3. Alternative 2: Country-level institutional classification 

# Common theme
theme_institutional <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 13, hjust = 0.5, colour = "#222222"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  )

# Panel (a): Type of government
g_government_compact <- ggplot(
  institutional_data,
  aes(
    x = 1,
    y = factor(country, levels = country_levels),
    fill = government_type
  )
) +
  geom_tile(width = 0.85, height = 0.85) +
  scale_fill_manual(
    values = government_colours,
    labels = labels_government,
    breaks = names(government_colours),
    drop = FALSE
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(add = 0.25)) +
  labs(title = "(a) Type of government") +
  guides(fill = guide_legend(nrow = 1)) +
  theme_institutional +
  theme(
    axis.text.y = element_text(
      size = 8,
      colour = "#333333",
      margin = margin(r = 4)
    )
  )
g_government_compact

# Panel (b): Structure of the legislative branch
g_structure_compact <- ggplot(
  institutional_data,
  aes(
    x = 1,
    y = factor(country, levels = country_levels),
    fill = structure
  )
) +
  geom_tile(width = 0.85, height = 0.85) +
  scale_fill_manual(
    values = structure_colours,
    labels = labels_structure,
    breaks = names(structure_colours),
    drop = FALSE
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(add = 0.25)) +
  labs(title = "(b) Structure of the legislative branch") +
  guides(fill = guide_legend(nrow = 1)) +
  theme_institutional +
  theme(axis.text.y = element_blank())

g_structure_compact


# Combine panels
graph2_compact <- (g_government_compact | g_structure_compact) +
  plot_annotation(
    title = "Graph 2. Type of government and structure of the legislative branch",
    caption = paste0(
      "Source: Prepared by the authors based on data from IPU Parline.\n",
      "Note: Institutional classifications are constant over the 2001–2024 study period. ",
      "Figures in parentheses indicate the number of countries in each category."
    ),
    theme = theme(
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      plot.caption = element_text(
        size = 10, hjust = 0, colour = "#555555",
        margin = margin(t = 8)
      )
    )
  )

graph2_compact

# Save figure
ggsave(
  "Outputs/graph2_government_structure_compact.svg",
  plot = graph2_compact,
  device = svglite::svglite,
  width = 8,
  height = 8,
  units = "in"
)

# 5.5. Parliamentary pay and GDP per capita by type of government ####

# 5.5.1. Prepare 2024 data 
data_2024 <- analysis_data[year == 2024]

# Correlation and number of countries by type of government
cor_by_government <- data_2024[
  !is.na(gdp_pc_ppp_current_fmi) &
    !is.na(salary_ppp_current_fmi),
  .(
    correlation = cor(gdp_pc_ppp_current_fmi, salary_ppp_current_fmi),
    n = .N
  ),
  by = government_type
]

cor_by_government[, label := paste0(
  "r = ", sprintf("%.2f", correlation),
  "\nn = ", n
)]

# Axis formatting
usd <- dollar_format(
  prefix = "$",
  big.mark = ",",
  decimal.mark = ".",
  accuracy = 1
)

# 5.5.2. Plot 
graph3 <- ggplot(
  data_2024,
  aes(
    x = gdp_pc_ppp_current_fmi,
    y = salary_ppp_current_fmi,
    colour = structure
  )
) +
  
  # One linear trend per government-type panel
  # The fit is estimated across all countries in the panel, irrespective
  # of legislative structure.
  
  geom_smooth(
    aes(group = 1),
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    colour = "#555555",
    fill = "#999999",
    alpha = 0.12,
    linewidth = 0.7
  ) +
  
  geom_point(alpha = 0.85, size = 2.5) +
  
  geom_text_repel(
    aes(label = iso3c),
    size = 2.5,
    max.overlaps = Inf,
    show.legend = FALSE,
    segment.colour = "#CCCCCC",
    segment.size = 0.2
  ) +
  
  # Correlation and sample size in each panel
  
  geom_label(
    data = cor_by_government,
    aes(x = Inf, y = -Inf, label = label),
    inherit.aes = FALSE,
    hjust = 1.05,
    vjust = -0.25,
    size = 3.7,
    label.size = 0.25,
    fill = "white"
  ) +
  
  facet_wrap(
    ~ government_type,
    labeller = as_labeller(government_labels_short)
  ) +
  
  scale_colour_manual(
    values = structure_colours,
    breaks = names(structure_colours)
  ) +
  
  scale_x_continuous(labels = usd) +
  scale_y_continuous(labels = usd) +
  
  labs(
    title = "Graph 3. Relationship between parliamentary pay and GDP per capita by type of government (2024)",
    x = "GDP per capita (current international dollars, PPP)",
    y = "Annual gross parliamentary pay (current international dollars, PPP)",
    colour = NULL,
    caption = paste0(
      "Source: Prepared by the authors. Parliamentary pay data collected by the authors; ",
      "GDP per capita and PPP conversion factors from the IMF World Economic \nOutlook (WEO), April 2025; ",
      "type of government and legislative structure from IPU Parline."
    )
  ) +
  
  theme_classic(base_size = 12) +
  
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10, colour = "#444444"),
    legend.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank(),
    
    strip.background = element_blank(),
    strip.text = element_text(size = 11.5, face = "bold", colour = "#222222"),
    
    panel.border = element_rect(
      colour = "#BBBBBB",
      fill = NA,
      linewidth = 0.5
    ),
    
    panel.grid.major = element_line(
      colour = "#ECECEC",
      linewidth = 0.4
    ),
    
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    
    plot.caption = element_text(
      size = 9,
      hjust = 0,
      colour = "#555555",
      margin = margin(t = 8)
    ),
    
    plot.margin = margin(t = 5, r = 10, b = 10, l = 10)
  )

graph3

# 5.5.3. Save figure 
ggsave(
  "Outputs/graph3_pay_vs_gdp_by_government_2024.svg",
  plot = graph3,
  device = svglite::svglite,
  width = 10,
  height = 5.5,
  units = "in"
)

# 5.6. Evolution of salary-setting mechanisms ####

# 5.6.1. Count of countries by mechanism and year 
mech_cols <- c(
  "Self-determined" = "#D55E00",   # vermillion
  "Assisted"        = "#CC79A7",   # reddish purple
  "Referenced"      = "#0072B2",   # blue
  "Independent"     = "#009E73"    # bluish green
)

regime_evolution <- analysis_data[
  , .N,
  by = .(year, salary_setting_mechanism)
]

regime_evolution[, mechanism := factor(
  salary_setting_mechanism,
  levels = c("Self-determined", "Assisted", "Referenced", "Independent")
)]

# 5.6.2. Years to label and endpoints for direct labels 
etiquetas <- regime_evolution[year %in% c(2004, 2014, 2024)]
ends <- regime_evolution[year == max(year)]

# 5.6.3. Plot 
graph4 <- ggplot(
  regime_evolution,
  aes(x = year, y = N, colour = mechanism)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.4) +
  
  geom_text(
    data = etiquetas,
    aes(label = N),
    vjust = -0.9,
    size = 3,
    show.legend = FALSE
  ) +
  
  # Direct end labels replacing the legend
  geom_text(
    data = ends,
    aes(label = mechanism),
    hjust = 0,
    nudge_x = 0.4,
    size = 3.6,
    fontface = "bold",
    show.legend = FALSE
  ) +
  
  scale_colour_manual(values = mech_cols) +
  
  scale_x_continuous(
    breaks = c(2001, 2006, 2012, 2018, 2024),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 24, by = 4)
  ) +
  
  coord_cartesian(clip = "off") +
  
  labs(
    title = "Graph 4. Evolution of the number of countries by salary-setting mechanism (2001–2024)",
    y = "Number of countries",
    caption = paste0(
      "Source: Prepared by the authors based on parliamentary pay data collected by the authors.\n",
      "Note: Numbers shown for selected years indicate the number of countries in each salary-setting mechanism."
    )
  ) +
  
  theme_classic(base_size = 12) +
  
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 11),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 10, colour = "#444444"),
    plot.caption = element_text(size = 9, hjust = 0, colour = "#555555"),
    panel.grid.major.y = element_line(colour = "#ECECEC", linewidth = 0.4),
    axis.line = element_line(colour = "#9A9A9A", linewidth = 0.4),
    legend.position = "none",
    plot.margin = margin(t = 5, r = 95, b = 10, l = 10)
  )
graph4

# 5.6.4. Save figure 
ggsave(
  "Outputs/graph4_mechanism_counts_2001_2024.svg",
  plot = graph4,
  device = svglite::svglite,
  width = 10,
  height = 5.5,
  units = "in"
)

# 5.7. Parliamentary remuneration by salary-setting mechanism ####

# 5.7.1. Prepare selected years 
selected_years <- c(2001, 2009, 2017, 2024)

plot_data <- analysis_data[
  year %in% selected_years &
    !is.na(salary_ppp_current_fmi)
]


# 5.7.2. Identify outliers within each year and mechanism 

# Outliers follow the conventional boxplot rule:
# observations below Q1 - 1.5*IQR or above Q3 + 1.5*IQR.
plot_data[, `:=`(
  Q1 = quantile(salary_ppp_current_fmi, 0.25, na.rm = TRUE),
  Q3 = quantile(salary_ppp_current_fmi, 0.75, na.rm = TRUE)
), by = .(year, salary_setting_mechanism)]

plot_data[, IQR := Q3 - Q1]

plot_data[, outlier_flag :=
            salary_ppp_current_fmi < (Q1 - 1.5 * IQR) |
            salary_ppp_current_fmi > (Q3 + 1.5 * IQR)
]


# 5.7.3. Plot 
graph5 <- ggplot(
  plot_data,
  aes(
    x = salary_setting_mechanism,
    y = salary_ppp_current_fmi
  )
) +
  
  # Distribution by salary-setting mechanism
  geom_boxplot(
    aes(fill = salary_setting_mechanism),
    alpha = 0.80,
    width = 0.70,
    colour = "#555555",
    outlier.shape = NA
  ) +
  
  # Raw non-outlier observations
  geom_jitter(
    data = plot_data[outlier_flag == FALSE],
    aes(colour = salary_setting_mechanism),
    position = position_jitter(
      width = 0.12,
      height = 0,
      seed = 123
    ),
    size = 1.2,
    alpha = 0.5,
    show.legend = FALSE
  ) +
  
  # Outliers
  geom_point(
    data = plot_data[outlier_flag == TRUE],
    shape = 21,
    colour = "black",
    fill = "white",
    size = 2
  ) +
  
  # Country labels for outliers
  geom_text_repel(
    data = plot_data[outlier_flag == TRUE],
    aes(label = iso3c),
    size = 3,
    colour = "#333333",
    min.segment.length = 0,
    seed = 123
  ) +
  
  facet_wrap(~ year, nrow = 1) +
  
  scale_fill_manual(values = mechanism_colours) +
  scale_colour_manual(values = mechanism_colours, guide = "none") +
  
  scale_y_continuous(
    labels = dollar_format(
      prefix = "$",
      big.mark = ",",
      decimal.mark = "."
    )
  ) +
  
  labs(
    title = "Graph 5. Distribution of parliamentary pay by salary-setting mechanism",
    y = "Annual gross parliamentary pay (current international dollars, PPP)",
    x = NULL,
    fill = NULL,
    caption = paste0(
      "Source: Prepared by the authors based on parliamentary pay data collected by the authors."
    )
  ) +
  
  theme_classic(base_size = 12) +
  
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 11),
    axis.text.y = element_text(size = 10, colour = "#444444"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank(),
    
    plot.caption = element_text(
      size = 9,
      hjust = 0,
      colour = "#555555"
    ),
    
    panel.border = element_rect(
      colour = "#BBBBBB",
      fill = NA,
      linewidth = 0.5
    ),
    
    panel.grid.major.y = element_line(
      colour = "#ECECEC",
      linewidth = 0.4
    ),
    
    axis.line = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(
      size = 12,
      face = "bold",
      colour = "#222222"
    )
  )

graph5

# 5.7.4. Save figure 
ggsave(
  "Outputs/graph5_pay_distribution_by_mechanism.svg",
  plot = graph5,
  device = svglite::svglite,
  width = 10,
  height = 5.5,
  units = "in"
)

# 5.8.1 Análisis variables geográficas
table(mps_salary[year == 2024]$is_latam, mps_salary[year == 2024]$government_type)
table(mps_salary[year == 2024]$mecanismo_fijacion, mps_salary[year == 2024]$government_type)

# 5.8.2 Filtrar el año de interés y resumir
plot_dt <- mps_salary[year == 2024, .N, by = .(region, tipo_gobierno)]

# 5.8.3 Ordenar regiones (opcional: por tamaño descendente)
plot_dt[, region := factor(region, 
                           levels = plot_dt[order(-N), unique(region)])]

# 5.8.4 Graficar
ggplot(plot_dt,
       aes(x = region, y = N, fill = tipo_gobierno)) +
  geom_col(position = position_dodge2(width = 0.75, preserve = "single"),
           width     = 0.75) +
  geom_text(aes(label = N),
            position = position_dodge(width = 0.75),
            vjust = -0.3, size = 3.1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.07))) +
  labs(title    = "Gráfico X5: Distribución de países por región y tipo de gobierno (2024)",
       x        = NULL,                       # sin nombre explícito en eje x
       y        = "Número de países",
       fill     = NULL,
       caption = "Fuente: elaboración propia en base a la base de datos IPU Parline. \nNota: Se considera a México en Latinoamérica.") +
  theme_classic(base_size = 11) +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        legend.position = "top",
        plot.caption = element_text(size = 9, hjust = 0),
        axis.text.x     = element_text(angle = 25, hjust = 1))

# 5.9 Relación Dieta parlamentaria y PIB por tipo de mecanismo
ggplot(mps_salary[year == 2024],
       aes(x = gdp_pc_ppp, y = salary_ppp, label = iso_alpha3)) +
  geom_point(alpha = 0.85, size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, color = "gray40", linetype = "dashed") +
  geom_text_repel(aes(label = iso_alpha3), size = 2.5, max.overlaps = Inf, show.legend = FALSE, inherit.aes = TRUE) + 
  facet_wrap(~mecanismo_fijacion) +
  scale_x_continuous(labels = dollar_format()) +
  scale_y_continuous(labels = dollar_format()) +
  labs(title = "Gráfico 6: Relación entre dieta parlamentaria y PIB per cápita (USD PPP, 2024)",
       x = "PIB per cápita (USD PPP)",
       y = "Dieta parlamentaria bruta (USD PPP)",
       caption = "Fuente: elaboración propia en base a datos de dieta parlamentaria recolectada por los autores.\nPIB per cápita y PPP obtenidos desde el WEO abril 2025 del FMI.") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 11),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    legend.text = element_text(size = 10),
    plot.caption = element_text(size = 9, hjust = 0),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    strip.background = element_blank(),
    legend.position = "top",
    legend.title = element_blank(),
    legend.margin   = margin(r = 0, b = -5, l = -5),  # reduce márgenes de la leyenda
    plot.margin     = margin(t = 5,  r = 10, b = 20, l = 10) 
  )

# 6. ECONOMETRIC ANALYSIS ####

# 6.1. Construct within-between variables ####

# Following Bell & Jones (2015), each time-varying explanatory variable is
# decomposed into:
#
#   Within component  = X_it - mean(X_i)
#   Between component = mean(X_i)
#
# This parameterization allows the within-country and between-country
# associations to be estimated and interpreted directly.
#
# Self-Determined is the reference category for the pay-setting mechanism.
#
# Following Wooldridge (2021), the inclusion of year fixed effects absorbs
# variables that vary only across time. Therefore, period-specific
# cross-sectional averages do not need to be included separately.

# 6.1.1. Salary-setting mechanism dummies
# Self-Determined is the omitted/reference category

analysis_data[, ":="(
  mech_assisted =
    as.integer(salary_setting_mechanism == "Assisted"),
  
  mech_referenced =
    as.integer(salary_setting_mechanism == "Referenced"),
  
  mech_independent =
    as.integer(salary_setting_mechanism == "Independent")
)]


# 6.1.2. Between-country components of the salary-setting mechanism
# These variables measure the proportion of sample years that each
# country spends under each salary-setting arrangement.

analysis_data[, ":="(
  
  between_assisted =
    mean(mech_assisted, na.rm = TRUE),
  
  between_referenced =
    mean(mech_referenced, na.rm = TRUE),
  
  between_independent =
    mean(mech_independent, na.rm = TRUE)
  
), by = country]


# 6.1.3. Within-country components of the salary-setting mechanism

analysis_data[, ":="(
  
  within_assisted =
    mech_assisted - between_assisted,
  
  within_referenced =
    mech_referenced - between_referenced,
  
  within_independent =
    mech_independent - between_independent
  
)]


# 6.1.4. GDP per capita: between-country component
#
# This variable already exists in analysis_variable.
# Rename it here only to make the econometric specification easier to read.
analysis_data[
  ,
  between_log_gdp_pc_ppp :=
    mean_log_gdp_pc_ppp_current_fmi
]


# 6.1.5. GDP per capita: within-country component
analysis_data[
  ,
  within_log_gdp_pc_ppp :=
    log_gdp_pc_ppp_current_fmi -
    between_log_gdp_pc_ppp
]


# 6.1.6. Relative parliamentary remuneration
#
# ln(Pay / GDPpc) = ln(Pay) - ln(GDPpc)

analysis_data[
  ,
  log_relative_pay :=
    log_salary_ppp_current_fmi -
    log_gdp_pc_ppp_current_fmi
]

# 6.2. Validate the within-between decomposition ####

decomposition_check <- analysis_data[
  ,
  .(
    mean_within_gdp =
      mean(within_log_gdp_pc_ppp, na.rm = TRUE),
    
    mean_within_assisted =
      mean(within_assisted, na.rm = TRUE),
    
    mean_within_referenced =
      mean(within_referenced, na.rm = TRUE),
    
    mean_within_independent =
      mean(within_independent, na.rm = TRUE)
  ),
  by = country
]

decomposition_check[
  abs(mean_within_gdp) > 1e-10 |
    abs(mean_within_assisted) > 1e-10 |
    abs(mean_within_referenced) > 1e-10 |
    abs(mean_within_independent) > 1e-10
]

# Expected result: empty data.table

gdp_mean_check <- analysis_data[
  ,
  .(
    stored_mean =
      unique(mean_log_gdp_pc_ppp_current_wb),
    
    calculated_mean =
      mean(log_gdp_pc_ppp_current_wb, na.rm = TRUE)
  ),
  by = country
]

gdp_mean_check[
  abs(stored_mean - calculated_mean) > 1e-10
]

# Expected result: empty data.table

# 6.3. Specification (1): Mundlak within-between POLS ####
#
# Dependent variable:
# ln gross parliamentary salary in current international dollars PPP
#
# Estimator:
# Pooled OLS with year fixed effects
#
# Inference:
# Standard errors clustered by country
model_1_mundlak <- feols(
  
  log_salary_ppp_current_fmi ~
    
    # Within-country economic development
    within_log_gdp_pc_ppp +
    
    # Between-country economic development
    between_log_gdp_pc_ppp +
    
    # Within-country salary-setting mechanism
    within_assisted +
    within_referenced +
    within_independent +
    
    # Between-country salary-setting mechanism
    between_assisted +
    between_referenced +
    between_independent +
    
    # Time-invariant institutional characteristics
    government_type +
    structure |
    
    # Year fixed effects
    year,
  
  data = analysis_data,
  
  # Country-clustered standard errors
  vcov = ~ country
)

summary(model_1_mundlak)

# 6.4. Specification (2): Two-way fixed effects benchmark ####

model_2_fe <- feols(
  
  log_salary_ppp_current_fmi ~
    
    log_gdp_pc_ppp_current_fmi +
    
    i(
      salary_setting_mechanism,
      ref = "Self-determined"
    ) |
    
    country + year,
  
  data = analysis_data,
  vcov = ~ country
)

summary(model_2_fe)

names(coef(model_2_fe))

# 6.5. Specification (3): Relative parliamentary remuneration ####
#
# Dependent variable:
# ln(Pay) - ln(GDP per capita)
#
# GDP per capita is not included again as an explanatory variable.
model_3_relative <- feols(
  
  log_relative_pay ~
    
    # Within-country salary-setting mechanism
    within_assisted +
    within_referenced +
    within_independent +
    
    # Between-country salary-setting mechanism
    between_assisted +
    between_referenced +
    between_independent +
    
    # Time-invariant institutional characteristics
    government_type +
    structure |
    
    # Year fixed effects
    year,
  
  data = analysis_data,
  vcov = ~ country
)

summary(model_3_relative)

# 6.6. Check Mundlak-FE equivalence ####
mundlak_fe_check <- data.table(
  
  variable = c(
    "GDP per capita PPP (ln)",
    "Assisted",
    "Referenced",
    "Independent"
  ),
  
  Mundlak_within = c(
    coef(model_1_mundlak)["within_log_gdp_pc_ppp"],
    coef(model_1_mundlak)["within_assisted"],
    coef(model_1_mundlak)["within_referenced"],
    coef(model_1_mundlak)["within_independent"]
  ),
  
  Fixed_effects = c(
    coef(model_2_fe)["log_gdp_pc_ppp_current_wb"],
    coef(model_2_fe)["salary_setting_mechanism::Assisted"],
    coef(model_2_fe)["salary_setting_mechanism::Referenced"],
    coef(model_2_fe)["salary_setting_mechanism::Independent"]
  )
)

mundlak_fe_check[
  ,
  difference := Mundlak_within - Fixed_effects
]

mundlak_fe_check #The difference column should be close to zero

# 6.7. Functions to construct Table 3 ####

format_result <- function(model, term) {
  
  ct <- coeftable(model)
  
  if (!term %in% rownames(ct)) {
    return("")
  }
  
  estimate  <- ct[term, 1]
  std_error <- ct[term, 2]
  p_value   <- ct[term, 4]
  
  stars <- fifelse(
    p_value < 0.01, "***",
    fifelse(
      p_value < 0.05, "**",
      fifelse(
        p_value < 0.10, "*",
        ""
      )
    )
  )
  
  paste0(
    sprintf("%.3f", estimate),
    stars,
    " (",
    sprintf("%.3f", std_error),
    ")"
  )
}

names(coef(model_1_mundlak))
names(coef(model_2_fe))
names(coef(model_3_relative))

# 6.8. Table with the results ####
table3_results <- data.frame(
  
  section = c(
    rep("Within-country effects", 4),
    rep("Between-country effects", 4),
    rep("Time-invariant institutional characteristics", 3),
    rep("Model information", 5)
  ),
  
  variable = c(
    
    # Within-country effects
    "GDP per capita PPP (ln)",
    "Assisted mechanism",
    "Referenced mechanism",
    "Independent mechanism",
    
    # Between-country effects
    "GDP per capita PPP (ln)",
    "Assisted mechanism",
    "Referenced mechanism",
    "Independent mechanism",
    
    # Time-invariant institutional characteristics
    "Presidential system",
    "Presidential-Parliamentary system",
    "Bicameral structure",
    
    # Model information
    "Observations",
    "Adjusted R²",
    "Country fixed effects",
    "Year fixed effects",
    "No. of countries"
  ),
  
  
  # ============================================================
  # Specification (1)
  # ============================================================
  
  model_1 = c(
    
    # Within-country effects
    format_result(
      model_1_mundlak,
      "within_log_gdp_pc_ppp"
    ),
    
    format_result(
      model_1_mundlak,
      "within_assisted"
    ),
    
    format_result(
      model_1_mundlak,
      "within_referenced"
    ),
    
    format_result(
      model_1_mundlak,
      "within_independent"
    ),
    
    
    # Between-country effects
    format_result(
      model_1_mundlak,
      "between_log_gdp_pc_ppp"
    ),
    
    format_result(
      model_1_mundlak,
      "between_assisted"
    ),
    
    format_result(
      model_1_mundlak,
      "between_referenced"
    ),
    
    format_result(
      model_1_mundlak,
      "between_independent"
    ),
    
    
    # Time-invariant institutional characteristics
    format_result(
      model_1_mundlak,
      "government_typePresidential system"
    ),
    
    format_result(
      model_1_mundlak,
      "government_typePresidential-Parliamentary"
    ),
    
    format_result(
      model_1_mundlak,
      "structureBicameral"
    ),
    
    
    # Model information
    as.character(nobs(model_1_mundlak)),
    
    sprintf(
      "%.3f",
      as.numeric(fitstat(model_1_mundlak, "ar2"))
    ),
    
    "No",
    "Yes",
    as.character(uniqueN(analysis_data$country))
  ),
  
  
  # ============================================================
  # Specification (2)
  # ============================================================
  
  model_2 = c(
    
    # Within-country effects
    format_result(
      model_2_fe,
      "log_gdp_pc_ppp_current_fmi"
    ),
    
    format_result(
      model_2_fe,
      "salary_setting_mechanism::Assisted"
    ),
    
    format_result(
      model_2_fe,
      "salary_setting_mechanism::Referenced"
    ),
    
    format_result(
      model_2_fe,
      "salary_setting_mechanism::Independent"
    ),
    
    
    # Between-country effects
    "", "", "", "",
    
    
    # Time-invariant institutional characteristics
    "", "", "",
    
    
    # Model information
    as.character(nobs(model_2_fe)),
    
    sprintf(
      "%.3f",
      as.numeric(fitstat(model_2_fe, "ar2"))
    ),
    
    "Yes",
    "Yes",
    as.character(uniqueN(analysis_data$country))
  ),
  
  
  # ============================================================
  # Specification (3)
  # ============================================================
  
  model_3 = c(
    
    # Within-country effects
    "",
    
    format_result(
      model_3_relative,
      "within_assisted"
    ),
    
    format_result(
      model_3_relative,
      "within_referenced"
    ),
    
    format_result(
      model_3_relative,
      "within_independent"
    ),
    
    
    # Between-country effects
    "",
    
    format_result(
      model_3_relative,
      "between_assisted"
    ),
    
    format_result(
      model_3_relative,
      "between_referenced"
    ),
    
    format_result(
      model_3_relative,
      "between_independent"
    ),
    
    
    # Time-invariant institutional characteristics
    format_result(
      model_3_relative,
      "government_typePresidential system"
    ),
    
    format_result(
      model_3_relative,
      "government_typePresidential-Parliamentary"
    ),
    
    format_result(
      model_3_relative,
      "structureBicameral"
    ),
    
    
    # Model information
    as.character(nobs(model_3_relative)),
    
    sprintf(
      "%.3f",
      as.numeric(fitstat(model_3_relative, "ar2"))
    ),
    
    "No",
    "Yes",
    as.character(uniqueN(analysis_data$country))
  ),
  
  stringsAsFactors = FALSE
)

# 6.9. Publication-ready Table 3 in R ####
table3_gt <- table3_results |>
  
  gt(
    rowname_col = "variable",
    groupname_col = "section"
  ) |>
  
  # Dependent-variable headings
  tab_spanner(
    label = gt::html("Annual gross pay (ln)<br>(current international dollars, PPP)"),
    columns = c(model_1, model_2)
  ) |>
  
  tab_spanner(
    label = gt::html("Relative pay (ln)<br>(current international dollars, PPP)"),
    columns = model_3
  ) |>
  
  # Model headings
  cols_label(
    model_1 = "Mundlak-CRE (1)",
    model_2 = "Fixed Effects (2)",
    model_3 = "Mundlak-CRE (3)"
  ) |>
  
  # Column alignment
  cols_align(
    align = "center",
    columns = c(model_1, model_2, model_3)
  ) |>
  
  cols_align(
    align = "left",
    columns = stub()
  ) |>
  
  # Section formatting
  tab_style(
    style = list(
      cell_fill(color = "#F2F2F2"),
      cell_text(
        weight = "bold",
        style = "italic"
      )
    ),
    locations = cells_row_groups()
  ) |>
  
  # Overall formatting
  tab_options(
    table.font.names = "Times New Roman",
    table.font.size = px(18),
    column_labels.font.weight = "bold",
    data_row.padding = px(4),
    table.border.top.style = "solid",
    table.border.bottom.style = "solid",
    column_labels.border.bottom.style = "solid"
  ) |>
  
  # Table note
  tab_source_note(
    source_note = md(
      paste0(
        "**Notes:** Specifications (1) and (2) use the natural logarithm ",
        "of gross parliamentary pay expressed in current international ",
        "dollars at purchasing power parity (PPP) as the dependent variable. ",
        "Specification (3) uses the difference between the natural logarithm ",
        "of parliamentary pay and the natural logarithm of GDP per capita, ",
        "both expressed in current international dollars at PPP. ",
        "The Mundlak specifications decompose GDP per capita and the ",
        "salary-setting mechanism into within- and between-country components. ",
        "Standard errors clustered by country are reported in parentheses ",
        "to the right of each coefficient. ",
        "Significance: **p*<0.1; ***p*<0.05; ****p*<0.01."
      )
    )
  )

table3_gt

# Save GT table as HTML
gt::gtsave(
  table3_gt,
  filename = "Table3_main_results.png",
  path = "Outputs/"
)

# ================================================================
# 6. ECONOMETRIC ANALYSIS
# Independent salary-setting mechanism as reference category
# ================================================================

# 6.1. Define factor reference categories ####
analysis_data[, salary_setting_mechanism := factor(
  salary_setting_mechanism,
  levels = c(
    "Independent",
    "Self-determined",
    "Assisted",
    "Referenced"
  )
)]

analysis_data[, government_type := factor(
  government_type,
  levels = c(
    "Parliamentary system",
    "Presidential system",
    "Presidential-Parliamentary"
  )
)]

analysis_data[, structure := factor(
  structure,
  levels = c(
    "Unicameral",
    "Bicameral"
  )
)]

# 6.2. Salary-setting mechanism dummies ####
# Independent is the omitted/reference category.
analysis_data[, ":="(
  mech_self_determined =
    as.integer(salary_setting_mechanism == "Self-determined"),
  
  mech_assisted =
    as.integer(salary_setting_mechanism == "Assisted"),
  
  mech_referenced =
    as.integer(salary_setting_mechanism == "Referenced")
)]


# 6.3. Between-country components ####
analysis_data[, ":="(
  between_self_determined =
    mean(mech_self_determined, na.rm = TRUE),
  
  between_assisted =
    mean(mech_assisted, na.rm = TRUE),
  
  between_referenced =
    mean(mech_referenced, na.rm = TRUE)
), by = country]

analysis_data[, between_log_gdp_pc_ppp :=
                mean_log_gdp_pc_ppp_current_fmi]


# 6.4. Within-country components ####
analysis_data[, ":="(
  within_self_determined =
    mech_self_determined - between_self_determined,
  
  within_assisted =
    mech_assisted - between_assisted,
  
  within_referenced =
    mech_referenced - between_referenced,
  
  within_log_gdp_pc_ppp =
    log_gdp_pc_ppp_current_fmi - between_log_gdp_pc_ppp
)]

# Relative parliamentary remuneration
analysis_data[, log_relative_pay :=
                log_salary_ppp_current_fmi -
                log_gdp_pc_ppp_current_fmi]

# 6.5. Specification (1): Mundlak within-between ####
model_1_indref <- feols(
  log_salary_ppp_current_fmi ~
    within_log_gdp_pc_ppp +
    within_self_determined +
    within_assisted +
    within_referenced +
    between_log_gdp_pc_ppp +
    between_self_determined +
    between_assisted +
    between_referenced +
    government_type +
    structure |
    year,
  
  data = analysis_data,
  vcov = ~ country
)

# 6.6. Specification (2): Country and year fixed effects ####
model_2_indref <- feols(
  log_salary_ppp_current_fmi ~
    log_gdp_pc_ppp_current_fmi +
    mech_self_determined +
    mech_assisted +
    mech_referenced |
    country + year,
  
  data = analysis_data,
  vcov = ~ country
)

# 6.7. Specification (3): Relative parliamentary remuneration ####
model_3_indref <- feols(
  log_relative_pay ~
    within_self_determined +
    within_assisted +
    within_referenced +
    between_self_determined +
    between_assisted +
    between_referenced +
    government_type +
    structure |
    year,
  
  data = analysis_data,
  vcov = ~ country
)


latam_countries <- c(
  "Argentina",
  "Bolivia",
  "Brazil",
  "Chile",
  "Colombia",
  "Costa Rica",
  "Ecuador",
  "Guatemala",
  "Mexico",
  "Paraguay",
  "Peru",
  "Uruguay"
)

analysis_data[, is_latam :=
                as.integer(country %in% latam_countries)]

robust_colombia <- feols(
  log_salary_ppp_current_fmi ~
    within_log_gdp_pc_ppp +
    within_self_determined +
    within_assisted +
    within_referenced +
    between_log_gdp_pc_ppp +
    between_self_determined +
    between_assisted +
    between_referenced +
    government_type +
    structure |
    year,
  
  data = analysis_data[country != "Colombia"],
  vcov = ~ country
)

robust_ireland <- feols(
  log_salary_ppp_current_fmi ~
    within_log_gdp_pc_ppp +
    within_self_determined +
    within_assisted +
    within_referenced +
    between_log_gdp_pc_ppp +
    between_self_determined +
    between_assisted +
    between_referenced +
    government_type +
    structure |
    year,
  
  data = analysis_data[country != "Ireland"],
  vcov = ~ country
)

robust_both <- feols(
  log_salary_ppp_current_fmi ~
    within_log_gdp_pc_ppp +
    within_self_determined +
    within_assisted +
    within_referenced +
    between_log_gdp_pc_ppp +
    between_self_determined +
    between_assisted +
    between_referenced +
    government_type +
    structure |
    year,
  
  data = analysis_data[
    !country %in% c("Colombia", "Ireland")
  ],
  
  vcov = ~ country
)

robust_latam <- feols(
  log_salary_ppp_current_fmi ~
    within_log_gdp_pc_ppp +
    within_self_determined +
    within_assisted +
    within_referenced +
    between_log_gdp_pc_ppp +
    between_self_determined +
    between_assisted +
    between_referenced +
    government_type +
    structure +
    is_latam |
    year,
  
  data = analysis_data,
  vcov = ~ country
)

switchers <- c(
  "Australia",
  "Chile",
  "Finland",
  "Iceland",
  "New Zealand",
  "United Kingdom"
)

fit_leave_one_out <- function(drop_country = NULL) {
  
  if (is.null(drop_country)) {
    dt <- analysis_data
  } else {
    dt <- analysis_data[country != drop_country]
  }
  
  feols(
    log_salary_ppp_current_fmi ~
      within_log_gdp_pc_ppp +
      within_self_determined +
      within_assisted +
      within_referenced +
      between_log_gdp_pc_ppp +
      between_self_determined +
      between_assisted +
      between_referenced +
      government_type +
      structure |
      year,
    
    data = dt,
    vcov = ~ country
  )
}

loo_models <- c(
  list(
    "Full sample" = fit_leave_one_out()
  ),
  
  setNames(
    lapply(
      switchers,
      fit_leave_one_out
    ),
    paste0("Drop ", switchers)
  )
)

chile_identification_check <- analysis_data[
  country != "Chile",
  .(
    sd_within_self_determined =
      sd(within_self_determined),
    
    max_abs_within_self_determined =
      max(abs(within_self_determined)),
    
    nonzero_observations =
      sum(abs(within_self_determined) > 1e-12)
  )
]

chile_identification_check

# ================================================================
# TABLE 3
# Independent mechanism as reference category
# ================================================================


# 1. Function to format coefficient + significance + SE ####
format_result <- function(model, term) {
  
  ct <- coeftable(model)
  
  if (!term %in% rownames(ct)) {
    return("")
  }
  
  estimate  <- ct[term, 1]
  std_error <- ct[term, 2]
  p_value   <- ct[term, 4]
  
  stars <- fifelse(
    p_value < 0.01, "***",
    fifelse(
      p_value < 0.05, "**",
      fifelse(p_value < 0.10, "*", "")
    )
  )
  
  paste0(
    sprintf("%.3f", estimate),
    stars,
    " (",
    sprintf("%.3f", std_error),
    ")"
  )
}


# 2. Construct Table 3 data ####
table3_indref <- data.frame(
  
  section = c(
    rep("Within-country effects", 4),
    rep("Between-country effects", 4),
    rep("Time-invariant institutional characteristics", 3),
    rep("Model information", 5)
  ),
  
  variable = c(
    
    # Within-country effects
    "GDP per capita PPP (ln)",
    "Self-Determined mechanism",
    "Assisted mechanism",
    "Referenced mechanism",
    
    # Between-country effects
    "GDP per capita PPP (ln)",
    "Self-Determined mechanism",
    "Assisted mechanism",
    "Referenced mechanism",
    
    # Time-invariant institutional characteristics
    "Presidential system",
    "Presidential-Parliamentary system",
    "Bicameral structure",
    
    # Model information
    "Observations",
    "Adjusted R²",
    "Country fixed effects",
    "Year fixed effects",
    "No. of countries"
  ),
  
  
  # --------------------------------------------------------------
  # Model (1): Mundlak-CRE
  # --------------------------------------------------------------
  
  model_1 = c(
    
    # Within
    format_result(
      model_1_indref,
      "within_log_gdp_pc_ppp"
    ),
    
    format_result(
      model_1_indref,
      "within_self_determined"
    ),
    
    format_result(
      model_1_indref,
      "within_assisted"
    ),
    
    format_result(
      model_1_indref,
      "within_referenced"
    ),
    
    
    # Between
    format_result(
      model_1_indref,
      "between_log_gdp_pc_ppp"
    ),
    
    format_result(
      model_1_indref,
      "between_self_determined"
    ),
    
    format_result(
      model_1_indref,
      "between_assisted"
    ),
    
    format_result(
      model_1_indref,
      "between_referenced"
    ),
    
    
    # Time-invariant institutional characteristics
    format_result(
      model_1_indref,
      "government_typePresidential system"
    ),
    
    format_result(
      model_1_indref,
      "government_typePresidential-Parliamentary"
    ),
    
    format_result(
      model_1_indref,
      "structureBicameral"
    ),
    
    
    # Model information
    as.character(nobs(model_1_indref)),
    
    sprintf(
      "%.3f",
      as.numeric(fitstat(model_1_indref, "ar2"))
    ),
    
    "No",
    "Yes",
    as.character(uniqueN(analysis_data$country))
  ),
  
  
  # --------------------------------------------------------------
  # Model (2): Fixed Effects
  # --------------------------------------------------------------
  
  model_2 = c(
    
    # Within
    format_result(
      model_2_indref,
      "log_gdp_pc_ppp_current_fmi"
    ),
    
    format_result(
      model_2_indref,
      "mech_self_determined"
    ),
    
    format_result(
      model_2_indref,
      "mech_assisted"
    ),
    
    format_result(
      model_2_indref,
      "mech_referenced"
    ),
    
    
    # Between
    "", "", "", "",
    
    
    # Time-invariant institutional characteristics
    "", "", "",
    
    
    # Model information
    as.character(nobs(model_2_indref)),
    
    sprintf(
      "%.3f",
      as.numeric(fitstat(model_2_indref, "ar2"))
    ),
    
    "Yes",
    "Yes",
    as.character(uniqueN(analysis_data$country))
  ),
  
  
  # --------------------------------------------------------------
  # Model (3): Relative pay
  # --------------------------------------------------------------
  
  model_3 = c(
    
    # Within
    "",
    
    format_result(
      model_3_indref,
      "within_self_determined"
    ),
    
    format_result(
      model_3_indref,
      "within_assisted"
    ),
    
    format_result(
      model_3_indref,
      "within_referenced"
    ),
    
    
    # Between
    "",
    
    format_result(
      model_3_indref,
      "between_self_determined"
    ),
    
    format_result(
      model_3_indref,
      "between_assisted"
    ),
    
    format_result(
      model_3_indref,
      "between_referenced"
    ),
    
    
    # Time-invariant institutional characteristics
    format_result(
      model_3_indref,
      "government_typePresidential system"
    ),
    
    format_result(
      model_3_indref,
      "government_typePresidential-Parliamentary"
    ),
    
    format_result(
      model_3_indref,
      "structureBicameral"
    ),
    
    
    # Model information
    as.character(nobs(model_3_indref)),
    
    sprintf(
      "%.3f",
      as.numeric(fitstat(model_3_indref, "ar2"))
    ),
    
    "No",
    "Yes",
    as.character(uniqueN(analysis_data$country))
  ),
  
  stringsAsFactors = FALSE
)

# 3. Publication-ready GT Table 3 ####
table3_indref_gt <- table3_indref |>
  
  gt::gt(
    rowname_col = "variable",
    groupname_col = "section"
  ) |>
  
  
  # --------------------------------------------------------------
# Dependent-variable headings
# --------------------------------------------------------------

gt::tab_spanner(
  label = gt::html(
    "Annual gross pay (ln)<br>
       (current international dollars, PPP)"
  ),
  columns = c(model_1, model_2)
) |>
  
  gt::tab_spanner(
    label = gt::html(
      "Relative pay (ln)<br>
       (current international dollars, PPP)"
    ),
    columns = model_3
  ) |>
  
  
  # --------------------------------------------------------------
# Model headings
# --------------------------------------------------------------

gt::cols_label(
  model_1 = "Mundlak-CRE (1)",
  model_2 = "Fixed Effects (2)",
  model_3 = "Mundlak-CRE (3)"
) |>
  
  
  # --------------------------------------------------------------
# Alignment
# --------------------------------------------------------------

gt::cols_align(
  align = "center",
  columns = c(model_1, model_2, model_3)
) |>
  
  
  # --------------------------------------------------------------
# Section headings
# --------------------------------------------------------------

gt::tab_style(
  style = list(
    gt::cell_fill(color = "#F2F2F2"),
    gt::cell_text(
      weight = "bold",
      style = "italic",
      size = gt::px(18)
    )
  ),
  locations = gt::cells_row_groups()
) |>
  
  
  # --------------------------------------------------------------
# Dependent-variable spanners
# --------------------------------------------------------------

gt::tab_style(
  style = gt::cell_text(
    weight = "bold",
    size = gt::px(18)
  ),
  locations = gt::cells_column_spanners()
) |>
  
  
  # --------------------------------------------------------------
# Model headings
# --------------------------------------------------------------

gt::tab_style(
  style = gt::cell_text(
    weight = "bold",
    size = gt::px(18)
  ),
  locations = gt::cells_column_labels()
) |>
  
  
  # --------------------------------------------------------------
# General table formatting
# --------------------------------------------------------------

gt::tab_options(
  
  table.font.names = "Times New Roman",
  table.font.size = gt::px(18),
  
  data_row.padding = gt::px(4),
  
  table.border.top.style = "solid",
  table.border.top.width = gt::px(1),
  
  table.border.bottom.style = "solid",
  table.border.bottom.width = gt::px(1),
  
  column_labels.border.bottom.style = "solid",
  column_labels.border.bottom.width = gt::px(1),
  
  row_group.border.top.style = "solid",
  row_group.border.top.width = gt::px(1),
  
  row_group.border.bottom.style = "solid",
  row_group.border.bottom.width = gt::px(1)
) |>
  
  
  # --------------------------------------------------------------
# Notes
# --------------------------------------------------------------

gt::tab_source_note(
  source_note = gt::md(
    paste0(
      "**Notes:** Specifications (1) and (2) use the natural logarithm ",
      "of gross parliamentary pay expressed in current international ",
      "dollars at purchasing power parity (PPP) as the dependent variable. ",
      "Specification (3) uses the difference between the natural logarithm ",
      "of parliamentary pay and the natural logarithm of GDP per capita, ",
      "both expressed in current international dollars at PPP. ",
      "The Mundlak specifications decompose GDP per capita and the ",
      "salary-setting mechanism into within- and between-country components. ",
      "Independent is the reference category for the salary-setting mechanism. ",
      "Standard errors clustered by country are reported in parentheses ",
      "to the right of each coefficient. ",
      "Significance: **p*<0.1; ***p*<0.05; ****p*<0.01."
    )
  )
)


# Display table
table3_indref_gt

# 4. Save Table 3 ####
# PNG
gt::gtsave(
  table3_indref_gt,
  filename = "Table3_independent_reference.png",
  path = "Outputs/",
  zoom = 2
)

# ================================================================
# ROBUSTNESS CHECKS TABLE
# Independent mechanism as reference category
# ================================================================


# 1. Function to format coefficient + stars + standard error ####
format_result <- function(model, term) {
  
  ct <- coeftable(model)
  
  if (!term %in% rownames(ct)) {
    return("")
  }
  
  estimate  <- ct[term, 1]
  std_error <- ct[term, 2]
  p_value   <- ct[term, 4]
  
  stars <- fifelse(
    p_value < 0.01, "***",
    fifelse(
      p_value < 0.05, "**",
      fifelse(p_value < 0.10, "*", "")
    )
  )
  
  paste0(
    sprintf("%.3f", estimate),
    stars,
    " (",
    sprintf("%.3f", std_error),
    ")"
  )
}

# 2. Build robustness-check table ####

robustness_table <- data.frame(
  
  section = c(
    rep("Within-country effects", 4),
    rep("Between-country effects", 4),
    rep("Time-invariant institutional characteristics", 3),
    rep("Additional control", 1),
    rep("Model information", 5)
  ),
  
  variable = c(
    
    # Within-country effects
    "GDP per capita PPP (ln)",
    "Self-Determined mechanism",
    "Assisted mechanism",
    "Referenced mechanism",
    
    # Between-country effects
    "GDP per capita PPP (ln)",
    "Self-Determined mechanism",
    "Assisted mechanism",
    "Referenced mechanism",
    
    # Time-invariant institutional characteristics
    "Presidential system",
    "Presidential-Parliamentary system",
    "Bicameral structure",
    
    # Additional control
    "Latin America",
    
    # Model information
    "Observations",
    "Adjusted R²",
    "Country fixed effects",
    "Year fixed effects",
    "No. of countries"
  ),
  
  
  # ============================================================
  # (1) Excluding Colombia
  # ============================================================
  
  model_1 = c(
    
    # Within
    format_result(
      robust_colombia,
      "within_log_gdp_pc_ppp"
    ),
    
    format_result(
      robust_colombia,
      "within_self_determined"
    ),
    
    format_result(
      robust_colombia,
      "within_assisted"
    ),
    
    format_result(
      robust_colombia,
      "within_referenced"
    ),
    
    
    # Between
    format_result(
      robust_colombia,
      "between_log_gdp_pc_ppp"
    ),
    
    format_result(
      robust_colombia,
      "between_self_determined"
    ),
    
    format_result(
      robust_colombia,
      "between_assisted"
    ),
    
    format_result(
      robust_colombia,
      "between_referenced"
    ),
    
    
    # Time-invariant institutional characteristics
    format_result(
      robust_colombia,
      "government_typePresidential system"
    ),
    
    format_result(
      robust_colombia,
      "government_typePresidential-Parliamentary"
    ),
    
    format_result(
      robust_colombia,
      "structureBicameral"
    ),
    
    
    # LATAM control
    "",
    
    
    # Model information
    as.character(nobs(robust_colombia)),
    
    sprintf(
      "%.3f",
      as.numeric(fitstat(robust_colombia, "ar2"))
    ),
    
    "No",
    "Yes",
    as.character(
      uniqueN(
        analysis_data[
          country != "Colombia",
          country
        ]
      )
    )
  ),
  
  
  # ============================================================
  # (2) Excluding Ireland
  # ============================================================
  
  model_2 = c(
    
    # Within
    format_result(
      robust_ireland,
      "within_log_gdp_pc_ppp"
    ),
    
    format_result(
      robust_ireland,
      "within_self_determined"
    ),
    
    format_result(
      robust_ireland,
      "within_assisted"
    ),
    
    format_result(
      robust_ireland,
      "within_referenced"
    ),
    
    
    # Between
    format_result(
      robust_ireland,
      "between_log_gdp_pc_ppp"
    ),
    
    format_result(
      robust_ireland,
      "between_self_determined"
    ),
    
    format_result(
      robust_ireland,
      "between_assisted"
    ),
    
    format_result(
      robust_ireland,
      "between_referenced"
    ),
    
    
    # Time-invariant institutional characteristics
    format_result(
      robust_ireland,
      "government_typePresidential system"
    ),
    
    format_result(
      robust_ireland,
      "government_typePresidential-Parliamentary"
    ),
    
    format_result(
      robust_ireland,
      "structureBicameral"
    ),
    
    
    # LATAM control
    "",
    
    
    # Model information
    as.character(nobs(robust_ireland)),
    
    sprintf(
      "%.3f",
      as.numeric(fitstat(robust_ireland, "ar2"))
    ),
    
    "No",
    "Yes",
    as.character(
      uniqueN(
        analysis_data[
          country != "Ireland",
          country
        ]
      )
    )
  ),
  
  
  # ============================================================
  # (3) Excluding Colombia and Ireland
  # ============================================================
  
  model_3 = c(
    
    # Within
    format_result(
      robust_both,
      "within_log_gdp_pc_ppp"
    ),
    
    format_result(
      robust_both,
      "within_self_determined"
    ),
    
    format_result(
      robust_both,
      "within_assisted"
    ),
    
    format_result(
      robust_both,
      "within_referenced"
    ),
    
    
    # Between
    format_result(
      robust_both,
      "between_log_gdp_pc_ppp"
    ),
    
    format_result(
      robust_both,
      "between_self_determined"
    ),
    
    format_result(
      robust_both,
      "between_assisted"
    ),
    
    format_result(
      robust_both,
      "between_referenced"
    ),
    
    
    # Time-invariant institutional characteristics
    format_result(
      robust_both,
      "government_typePresidential system"
    ),
    
    format_result(
      robust_both,
      "government_typePresidential-Parliamentary"
    ),
    
    format_result(
      robust_both,
      "structureBicameral"
    ),
    
    
    # LATAM control
    "",
    
    
    # Model information
    as.character(nobs(robust_both)),
    
    sprintf(
      "%.3f",
      as.numeric(fitstat(robust_both, "ar2"))
    ),
    
    "No",
    "Yes",
    as.character(
      uniqueN(
        analysis_data[
          !country %in% c("Colombia", "Ireland"),
          country
        ]
      )
    )
  ),
  
  
  # ============================================================
  # (4) Latin America dummy
  # ============================================================
  
  model_4 = c(
    
    # Within
    format_result(
      robust_latam,
      "within_log_gdp_pc_ppp"
    ),
    
    format_result(
      robust_latam,
      "within_self_determined"
    ),
    
    format_result(
      robust_latam,
      "within_assisted"
    ),
    
    format_result(
      robust_latam,
      "within_referenced"
    ),
    
    
    # Between
    format_result(
      robust_latam,
      "between_log_gdp_pc_ppp"
    ),
    
    format_result(
      robust_latam,
      "between_self_determined"
    ),
    
    format_result(
      robust_latam,
      "between_assisted"
    ),
    
    format_result(
      robust_latam,
      "between_referenced"
    ),
    
    
    # Time-invariant institutional characteristics
    format_result(
      robust_latam,
      "government_typePresidential system"
    ),
    
    format_result(
      robust_latam,
      "government_typePresidential-Parliamentary"
    ),
    
    format_result(
      robust_latam,
      "structureBicameral"
    ),
    
    
    # LATAM control
    format_result(
      robust_latam,
      "is_latam"
    ),
    
    
    # Model information
    as.character(nobs(robust_latam)),
    
    sprintf(
      "%.3f",
      as.numeric(fitstat(robust_latam, "ar2"))
    ),
    
    "No",
    "Yes",
    as.character(uniqueN(analysis_data$country))
  ),
  
  stringsAsFactors = FALSE
)

# 3. Publication-ready robustness table ####

robustness_table_gt <- robustness_table |>
  
  gt::gt(
    rowname_col = "variable",
    groupname_col = "section"
  ) |>
  
  
  # --------------------------------------------------------------
# Dependent-variable heading
# --------------------------------------------------------------

gt::tab_spanner(
  label = gt::html(
    "Annual gross pay (ln)<br>
       (current international dollars, PPP)"
  ),
  columns = c(
    model_1,
    model_2,
    model_3,
    model_4
  )
) |>
  
  
  # --------------------------------------------------------------
# Model headings
# --------------------------------------------------------------

gt::cols_label(
  model_1 = "Excluding Colombia (1)",
  model_2 = "Excluding Ireland (2)",
  model_3 = "Excluding both (3)",
  model_4 = "LATAM dummy (4)"
) |>
  
  
  # --------------------------------------------------------------
# Alignment
# --------------------------------------------------------------

gt::cols_align(
  align = "center",
  columns = c(
    model_1,
    model_2,
    model_3,
    model_4
  )
) |>
  
  
  # --------------------------------------------------------------
# Section headings
# --------------------------------------------------------------

gt::tab_style(
  style = list(
    gt::cell_fill(color = "#F2F2F2"),
    gt::cell_text(
      weight = "bold",
      style = "italic",
      size = gt::px(18)
    )
  ),
  locations = gt::cells_row_groups()
) |>
  
  
  # --------------------------------------------------------------
# Dependent-variable heading
# --------------------------------------------------------------

gt::tab_style(
  style = gt::cell_text(
    weight = "bold",
    size = gt::px(18)
  ),
  locations = gt::cells_column_spanners()
) |>
  
  
  # --------------------------------------------------------------
# Column headings
# --------------------------------------------------------------

gt::tab_style(
  style = gt::cell_text(
    weight = "bold",
    size = gt::px(18)
  ),
  locations = gt::cells_column_labels()
) |>
  
  
  # --------------------------------------------------------------
# General formatting
# --------------------------------------------------------------

gt::tab_options(
  
  table.font.names = "Times New Roman",
  table.font.size = gt::px(18),
  
  data_row.padding = gt::px(4),
  
  table.border.top.style = "solid",
  table.border.top.width = gt::px(1),
  
  table.border.bottom.style = "solid",
  table.border.bottom.width = gt::px(1),
  
  column_labels.border.bottom.style = "solid",
  column_labels.border.bottom.width = gt::px(1),
  
  row_group.border.top.style = "solid",
  row_group.border.top.width = gt::px(1),
  
  row_group.border.bottom.style = "solid",
  row_group.border.bottom.width = gt::px(1)
) |>
  
  
  # --------------------------------------------------------------
# Notes
# --------------------------------------------------------------

gt::tab_source_note(
  source_note = gt::md(
    paste0(
      "**Notes:** The dependent variable is the natural logarithm of ",
      "annual gross parliamentary pay expressed in current international ",
      "dollars at purchasing power parity (PPP). ",
      "All specifications use the Mundlak within-between specification ",
      "and include year fixed effects. ",
      "Independent is the reference category for the salary-setting mechanism. ",
      "Columns (1) and (2) exclude Colombia and Ireland, respectively; ",
      "column (3) excludes both countries; and column (4) includes a ",
      "Latin America dummy. Standard errors clustered by country are ",
      "reported in parentheses to the right of each coefficient. ",
      "Significance: **p*<0.1; ***p*<0.05; ****p*<0.01."
    )
  )
)


# Display table
robustness_table_gt

# 4. Save robustness table ####
gt::gtsave(
  robustness_table_gt,
  filename = "TableA2_robustness_checks.png",
  path = "Outputs/"
)

# ================================================================
# LEAVE-ONE-SWITCHER-OUT ROBUSTNESS CHECK
# Independent mechanism as reference category
# ================================================================
switchers <- c(
  "Australia",
  "Chile",
  "Finland",
  "Iceland",
  "New Zealand",
  "United Kingdom"
)

# Function to estimate Specification (1)
# excluding one switcher at a time
fit_leave_one_switcher_out <- function(drop_country = NULL) {
  
  if (is.null(drop_country)) {
    dt <- analysis_data
  } else {
    dt <- analysis_data[country != drop_country]
  }
  
  feols(
    log_salary_ppp_current_fmi ~
      
      # Within-country effects
      within_log_gdp_pc_ppp +
      within_self_determined +
      within_assisted +
      within_referenced +
      
      # Between-country effects
      between_log_gdp_pc_ppp +
      between_self_determined +
      between_assisted +
      between_referenced +
      
      # Time-invariant institutional characteristics
      government_type +
      structure +
      
      # Regional control
      is_latam |
      
      # Year fixed effects
      year,
    
    data = dt,
    vcov = ~ country
  )
}

# Full-sample model
loo_full <- fit_leave_one_switcher_out()

# Leave-one-switcher-out models
loo_australia <- fit_leave_one_switcher_out("Australia")
loo_chile     <- fit_leave_one_switcher_out("Chile")
loo_finland   <- fit_leave_one_switcher_out("Finland")
loo_iceland   <- fit_leave_one_switcher_out("Iceland")
loo_nz        <- fit_leave_one_switcher_out("New Zealand")
loo_uk        <- fit_leave_one_switcher_out("United Kingdom")

# Format coefficient, significance stars and clustered SE
format_loo_result <- function(model, term) {
  
  ct <- coeftable(model)
  
  # Coefficient cannot be estimated
  if (!term %in% rownames(ct)) {
    return("Not identified")
  }
  
  estimate  <- ct[term, 1]
  std_error <- ct[term, 2]
  p_value   <- ct[term, 4]
  
  stars <- fifelse(
    p_value < 0.01, "***",
    fifelse(
      p_value < 0.05, "**",
      fifelse(p_value < 0.10, "*", "")
    )
  )
  
  paste0(
    sprintf("%.3f", estimate),
    stars,
    " (",
    sprintf("%.3f", std_error),
    ")"
  )
}

# ================================================================
# Build leave-one-switcher-out table
# ================================================================

loo_table <- data.frame(
  
  section = c(
    rep("Within-country effects", 4),
    rep("Between-country effects", 4),
    rep("Time-invariant institutional characteristics", 3),
    rep("Additional control", 1),
    rep("Model information", 4)
  ),
  
  variable = c(
    
    # Within-country effects
    "GDP per capita PPP (ln)",
    "Self-Determined mechanism",
    "Assisted mechanism",
    "Referenced mechanism",
    
    # Between-country effects
    "GDP per capita PPP (ln)",
    "Self-Determined mechanism",
    "Assisted mechanism",
    "Referenced mechanism",
    
    # Time-invariant institutional characteristics
    "Presidential system",
    "Presidential-Parliamentary system",
    "Bicameral structure",
    
    # Additional control
    "Latin America",
    
    # Model information
    "Observations",
    "Adjusted R²",
    "Year fixed effects",
    "No. of countries"
  ),
  
  stringsAsFactors = FALSE
)

extract_loo_column <- function(model, n_countries) {
  
  c(
    
    # Within-country effects
    format_loo_result(model, "within_log_gdp_pc_ppp"),
    format_loo_result(model, "within_self_determined"),
    format_loo_result(model, "within_assisted"),
    format_loo_result(model, "within_referenced"),
    
    # Between-country effects
    format_loo_result(model, "between_log_gdp_pc_ppp"),
    format_loo_result(model, "between_self_determined"),
    format_loo_result(model, "between_assisted"),
    format_loo_result(model, "between_referenced"),
    
    # Time-invariant institutional characteristics
    format_loo_result(
      model,
      "government_typePresidential system"
    ),
    
    format_loo_result(
      model,
      "government_typePresidential-Parliamentary"
    ),
    
    format_loo_result(
      model,
      "structureBicameral"
    ),
    
    # Additional control
    format_loo_result(
      model,
      "is_latam"
    ),
    
    # Model information
    as.character(nobs(model)),
    
    sprintf(
      "%.3f",
      as.numeric(fitstat(model, "ar2"))
    ),
    
    "Yes",
    as.character(n_countries)
  )
}

loo_table$full_sample <-
  extract_loo_column(
    loo_full,
    42
  )

loo_table$drop_australia <-
  extract_loo_column(
    loo_australia,
    41
  )

loo_table$drop_chile <-
  extract_loo_column(
    loo_chile,
    41
  )

loo_table$drop_finland <-
  extract_loo_column(
    loo_finland,
    41
  )

loo_table$drop_iceland <-
  extract_loo_column(
    loo_iceland,
    41
  )

loo_table$drop_nz <-
  extract_loo_column(
    loo_nz,
    41
  )

loo_table$drop_uk <-
  extract_loo_column(
    loo_uk,
    41
  )

loo_table

# ================================================================
# Publication-ready leave-one-switcher-out table
# ================================================================
loo_table_gt <- loo_table |>
  
  gt::gt(
    rowname_col = "variable",
    groupname_col = "section"
  ) |>
  
  
# --------------------------------------------------------------
# Dependent-variable heading
# --------------------------------------------------------------

gt::tab_spanner(
  label = gt::html(
    "Annual gross pay (ln)<br>
       (current international dollars, PPP)"
  ),
  columns = c(
    full_sample,
    drop_australia,
    drop_chile,
    drop_finland,
    drop_iceland,
    drop_nz,
    drop_uk
  )
) |>
  
  
  # --------------------------------------------------------------
# Column labels
# --------------------------------------------------------------

gt::cols_label(
  full_sample     = "Full sample",
  drop_australia  = "Excl. Australia",
  drop_chile      = "Excl. Chile",
  drop_finland    = "Excl. Finland",
  drop_iceland    = "Excl. Iceland",
  drop_nz         = "Excl. New Zealand",
  drop_uk         = "Excl. United Kingdom"
) |>
  
# --------------------------------------------------------------
# Alignment
# --------------------------------------------------------------
gt::cols_align(
  align = "center",
  columns = c(
    full_sample,
    drop_australia,
    drop_chile,
    drop_finland,
    drop_iceland,
    drop_nz,
    drop_uk
  )
) |>
  
  
# --------------------------------------------------------------
# Section headings
# --------------------------------------------------------------

gt::tab_style(
  style = list(
    gt::cell_fill(color = "#F2F2F2"),
    gt::cell_text(
      weight = "bold",
      style = "italic",
      size = gt::px(16)
    )
  ),
  locations = gt::cells_row_groups()
) |>
  
  
# --------------------------------------------------------------
# Dependent-variable heading
# --------------------------------------------------------------
gt::tab_style(
  style = gt::cell_text(
    weight = "bold",
    size = gt::px(16)
  ),
  locations = gt::cells_column_spanners()
) |>
  
  
# --------------------------------------------------------------
# Column headings
# --------------------------------------------------------------
gt::tab_style(
  style = gt::cell_text(
    weight = "bold",
    size = gt::px(16)
  ),
  locations = gt::cells_column_labels()
) |>

# --------------------------------------------------------------
# General table formatting
# --------------------------------------------------------------
gt::tab_options(
  
  table.font.names = "Times New Roman",
  table.font.size = gt::px(16),
  
  data_row.padding = gt::px(3),
  
  table.border.top.style = "solid",
  table.border.top.width = gt::px(1),
  
  table.border.bottom.style = "solid",
  table.border.bottom.width = gt::px(1),
  
  column_labels.border.bottom.style = "solid",
  column_labels.border.bottom.width = gt::px(1),
  
  row_group.border.top.style = "solid",
  row_group.border.top.width = gt::px(1),
  
  row_group.border.bottom.style = "solid",
  row_group.border.bottom.width = gt::px(1)
) |>
  
  
# --------------------------------------------------------------
# Notes
# --------------------------------------------------------------
gt::tab_source_note(
  source_note = gt::md(
    paste0(
      "**Notes:** Each specification re-estimates the main Mundlak ",
      "within-between model after excluding one country that changed ",
      "salary-setting mechanism during 2001–2024. ",
      "Independent is the reference category for the salary-setting mechanism. ",
      "All specifications include a Latin America dummy and year fixed effects. ",
      "Country-clustered standard errors are reported in parentheses. ",
      "\"Not identified\" indicates that the corresponding within-country ",
      "contrast cannot be estimated after excluding that country. ",
      "Significance: **p*<0.1; ***p*<0.05; ****p*<0.01."
    )
  )
)

loo_table_gt

gt::gtsave(
  loo_table_gt,
  filename = "Table_leave_one_switcher_out.png",
  path = "Outputs/",
  zoom = 2
)
