# How Big Are Prague's Cafés? (Café Size topic)

Data Analytics semestral project, Anthony George. Presentation together with Sawyer Gatdula.

## Structure

```
analysis/       raw_data.csv (class data, not edited by hand)
                01_cleaning.R, 02_descriptive.R, 03_analysis.R
                dataset.csv (written by 01_cleaning.R)
report/         report.tex, report.pdf, cafestyle.sty
                tables/ and figures/ (written by 02 and 03)
presentation/   slides.tex, slides.pdf (Beamer)
```

## How to reproduce

1. Open `Cafe project.Rproj` in RStudio (so the working directory is the repository root).
2. Run the scripts in order:
   1. `analysis/01_cleaning.R`: raw_data.csv → dataset.csv (base R only)
   2. `analysis/02_descriptive.R`: decision table, descriptive statistics and plots (`D_*`)
   3. `analysis/03_analysis.R`: A1–A10 tables and figures, diagnostics and the summary table
3. Compile `report/report.tex` and `presentation/slides.tex` with pdfLaTeX.

Scripts 02 and 03 install any missing packages themselves
(xtable, rstatix, lmtest, marginaleffects).
