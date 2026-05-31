# AquaValidator

AquaValidator is an R Shiny application for reviewing and validating water-quality laboratory results. It presents lab measurements in a clear interface and gives analysts the tools to judge whether results are accurate and realistic before approving them.

It was built for Aqualysis, a Dutch water-quality laboratory, and is shared publicly with their permission. Aqualysis is a semi-governmental institution and its water-quality measurement data is public.

## What it does

The app loads an Excel export of laboratory results and helps an analyst work through a validation list ("fiatteerlijst"):

- Flags results that fall outside expected ranges and need review.
- Shows each measurement in context: a table of the most recent samples per measurement point, and interactive plots of results and calculated ratios over time.
- Lets the analyst select samples or individual results, add comments, and approve, reject, or mark duplicate results.
- Exports the validated samples and results, with comments, to CSV.

## Data flow

```text
Excel export of lab results
  -> load and split into validation list + results
  -> flag out-of-range results, compute ratios
  -> analyst reviews in tables and interactive plots
  -> approve / reject / comment
  -> export validated samples and results to CSV
```

The SQL in `SQL Queries/` documents how the source result lists are produced from the laboratory information system. The app itself reads the exported Excel file rather than connecting to that database directly.

## Running it

AquaValidator is an R package. With R and the dependencies in `DESCRIPTION` installed:

```r
# from the repository root
source("run.R")
# or
aquavalidator::startValidator()
```

Both forms must be run from the repository root: they call `pkgload::load_all("./")` and `shiny::runApp("R")` with paths relative to the project root. `run.R` loads the package and starts the Shiny app in the browser. Set the `AQUAVALIDATOR_EXPORT_PATH` environment variable to choose where validated CSV exports are written (it defaults to a local `validated_output` folder).

## Repository layout

- `R/app.R`: the Shiny UI and server, plus helper functions.
- `R/` (other files): package startup and utilities.
- `SQL Queries/`: SQL used to produce the source result lists.
- `Documentatie/`: a Dutch user manual with screenshots of the interface.
- `man/`: generated R documentation.
- `tests/`: testthat tests.

## License

The R source code is released under the MIT License (see `LICENSE` and `LICENSE.md`). The license covers the code only. The SQL queries, the user manual, and any water-quality measurement data are not relicensed and remain under their original terms.
