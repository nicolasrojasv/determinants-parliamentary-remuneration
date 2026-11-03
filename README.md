# Determinants of Parliamentary Remuneration

Research data and R scripts accompanying the study by Felipe Larraín and Nicolás Rojas.

Corresponding author: Nicolás Rojas, <nrojasv@fen.uchile.cl>.

## Contents and status

This initial deposit contains the supplied Excel workbook, two original R scripts and instructions for running the analysis. The workbook and scripts have not been modified in this deposit.

The workbook contains a balanced country-year panel: **42 countries, 2001–2024, 1,008 observations and 25 columns**, in the worksheet `balanced db`. This describes the supplied workbook; the estimation sample of an individual model can differ if variables are missing or observations are excluded.

**End-to-end execution in a clean R environment has not yet been verified for this deposit.** The package does not yet include a package-version lockfile, a record of the authors' R session or frozen copies of the external downloads. These instructions document the workflow implemented in the supplied code; they do not establish that every output exactly matches the submitted manuscript.

## Repository structure

Preserve the paths and capitalisation below. Run all commands from the repository root.

| Path | Purpose |
| --- | --- |
| `README.md` | Project description and execution instructions. |
| `Scripts/determinants_parliamentary_salaries.R` | Main data-processing, descriptive, graphical and econometric analysis. |
| `Scripts/functions.R` | Defines `cargar_paquetes()`, which installs missing packages and loads them. |
| `Data/DB/final_dataset_parlamentary_remunerations.xlsx` | Local input workbook; the main script reads `balanced db`. |

Two additional directories are created by the preparation commands below:

| Path | Purpose |
| --- | --- |
| `Outputs/` | Descriptive tables, figures and regression-table images exported by the script. |

## Software requirements

- R. The script uses the native pipe `|>`, which requires R 4.1.0 or later. This is a syntax requirement, not a verified minimum for every package; use a current supported R installation.
- RStudio is optional and provides a convenient interface.
- Internet access for package installation and for the external data downloads described below.
- Chrome or another supported Chromium-based browser for exporting regression tables to PNG through `gt` and `webshot2`.

The main script loads the following packages through `cargar_paquetes()`:

```r
c(
  "data.table", "plm", "lmtest", "car", "ggplot2", "readxl",
  "scales", "patchwork", "ggrepel", "fixest", "forcats",
  "modelsummary", "sandwich", "writexl", "countrycode", "WDI", "gt"
)
```

The script also calls `svglite::svglite` for SVG exports. PNG exports through `gt::gtsave()` require `webshot2`. These two packages are not included in the main script's installation list and are installed separately in the instructions below. Additional system libraries or build tools may be required by R packages on some operating systems.

## External data downloaded during execution

The Excel workbook is not the only data input. The script also retrieves:

1. **World Bank World Development Indicators**, using the `WDI` package. The indicator codes are `PA.NUS.PPP`, `PA.NUS.PRVT.PP`, `NY.GDP.MKTP.PP.CD`, `NY.GDP.MKTP.PP.KD`, `NY.GDP.PCAP.PP.CD` and `FP.CPI.TOTL`.
2. **CEPALSTAT monthly consumer price index**, indicator `365`. The code downloads an Excel file, selects Argentina and constructs annual averages for the study period.

The CEPALSTAT endpoint used in the script is:

<https://api-cepalstat.cepal.org/cepalstat/api/v1/indicator/365/data?format=excel&in=1&lang=en>

These downloads occur again when the script is rerun. Revisions to external data or changes to the APIs may affect results or execution. Exact replication requires retaining the external data versions used for the manuscript, together with their retrieval dates.

## Running the analysis

### 1. Download the repository

On GitHub, select **Code > Download ZIP**, extract the archive and keep the folder structure intact. Alternatively, clone the repository using Git.

### 2. Set the working directory

Open a fresh R session. Set the working directory to the extracted repository root, which contains `README.md`, `Scripts/` and `Data/`.

In RStudio, use **Session > Set Working Directory > Choose Directory**. Alternatively:

```r
# Replace this example with the location of your extracted repository.
setwd("C:/path/to/determinants-parliamentary-remuneration")
```

Do not set the working directory to `Scripts/`.

### 3. Prepare dependencies and output directories

Run the following in the R console:

```r
options(repos = c(CRAN = "https://cloud.r-project.org"))

extra_packages <- c("svglite", "webshot2")
missing_packages <- setdiff(extra_packages, rownames(installed.packages()))
if (length(missing_packages) > 0L) {
  install.packages(missing_packages)
}

dir.create("Outputs", recursive = TRUE, showWarnings = FALSE)

stopifnot(
  file.exists("Scripts/functions.R"),
  file.exists("Scripts/determinants_parliamentary_salaries.R"),
  file.exists("Data/DB/final_dataset_parlamentary_remunerations.xlsx")
)
```

The main script installs the remaining missing packages when it starts. Make sure a compatible browser is installed before running the PNG export steps.

### 4. Execute the main script

```r
source("Scripts/determinants_parliamentary_salaries.R", encoding = "UTF-8")
```

The main script sources `Scripts/functions.R` automatically. It also clears the R global environment at the start, which is why a fresh session is recommended.

If the script stops, inspect the first error and resolve it before claiming successful reproduction. Do not skip a failed step if subsequent steps depend on its objects or files.

### 5. Record the execution environment

After a successful complete run:

```r
writeLines(capture.output(sessionInfo()), "Outputs/sessionInfo.txt")
```

This records the R and loaded package versions for that run. It is not a substitute for a dependency lockfile.

## Analysis workflow and outputs

The main script performs the following tasks:

1. Imports and checks the country-year panel.
2. Downloads macroeconomic data, constructs alternative PPP measures and merges them into the panel.
3. Creates the analysis variables, logarithms and country means.
4. Computes descriptive statistics and produces figures.
5. Estimates within-between/Mundlak and two-way fixed-effects models, relative-remuneration specifications, and robustness checks.

The intermediate dataset is written to `Data/BD/parliamentary_salary_analysis_data.csv`. The script uses `write.csv2()` and `read.csv2()`, which use semicolon-separated fields and decimal commas.

Descriptive CSV files and SVG figures are exported to `Outputs/`. Some additional figures are created as R objects rather than exported as standalone files. Regression-table exports include:

- `Outputs/Table3_main_results.png`
- `Outputs/Table3_independent_reference.png`
- `Outputs/TableA2_robustness_checks.png`
- `Outputs/Table_leave_one_switcher_out.png`

The script includes analyses using different reference categories for the salary-setting mechanism. Preserve both blocks when reproducing the supplied workflow.

## Troubleshooting

| Issue | Check |
| --- | --- |
| `Scripts/functions.R` or the Excel workbook cannot be found | Confirm the working directory is the repository root and the paths match the structure above. |
| The analysis CSV cannot be written | Create `Data/DB/` before execution. |
| An export to `Outputs/` fails because the directory does not exist | Create `Outputs/` before execution. |
| SVG export reports that `svglite` is unavailable | Install `svglite`. |
| PNG export fails | Check `webshot2` and the Chrome/Chromium installation. |
| An external download fails or expected columns are missing | Check connectivity and whether the World Bank or CEPALSTAT service or response format has changed. |

## Citation and reuse

Please cite the accompanying paper when using this material. A version-specific archival DOI and a final dataset citation can be added once a verified release has been deposited in an archival repository.

No reuse licence is assigned by this initial deposit. Code and dataset licensing should be specified by the authors, taking account of the terms of the underlying data sources.

For questions about the files or analysis, contact Nicolás Rojas at <nrojasv@fen.uchile.cl>.

## Software documentation

- [GitHub: downloading repository source archives](https://docs.github.com/en/repositories/working-with-files/using-files/downloading-source-code-archives)
- [gt: saving tables and PNG export requirements](https://gt.rstudio.com/reference/gtsave.html)
- [webshot2: browser requirements](https://rstudio.github.io/webshot2/)
