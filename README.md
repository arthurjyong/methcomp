# methcomp

R tooling for comparing two methods — or two raters — of measurement across multiple variables. For each variable it computes agreement and method-comparison statistics and renders them into a single PDF report.

## What it does

Given two Excel files with identical column structure (one per method/rater), `my_methcomp_v2.1.R` computes, for every column:

- **Intraclass correlation coefficient (ICC)** — two-way, agreement, single-rater
- **Pearson product–moment correlation** with R²
- **Passing–Bablok regression**
- **Bland–Altman analysis** — differences in absolute units and as percentages, with confidence-interval ribbons
- **Normality testing** of the differences — Shapiro–Wilk and D'Agostino–Pearson
- **Paired and unpaired t-tests**
- **Polynomial fits** — quadratic and cubic, each with R² and an F-test

Results are rendered to PDF via `report.Rmd`.

## Usage

1. Put exactly two `.xlsx` files in the project directory — one per method/rater — with identical column headers.
2. Run `my_methcomp_v2.1.R` in R. It auto-detects the two Excel files, runs the per-column analysis, and renders `report.Rmd` to PDF.

Required R packages: `readxl`, `irr`, `rmarkdown`, `lpSolve`, `ggplot2`, `MethComp`, `moments`, `rlang`, `knitr`.

## Sample data

`method_1.xlsx` and `method_2.xlsx` are **synthetic example data** — made-up weight / height / BMI / blood-pressure / pulse values, included only so the script runs out of the box. They are not real measurements and contain no personal data. Replace them with your own.
