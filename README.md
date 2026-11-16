# Prague Cafés — Café Size (Appendix C)

Run in RStudio after opening `Cafe project.Rproj`, in this order:

1. `analysis/01_cleaning.R` — raw_data.csv → dataset.csv
2. `analysis/02_descriptive.R` — decision table D_table.tex + D_*.pdf
3. `analysis/03_analysis.R` — A1–A10 tables and figures + summary table

Then compile `report/report.tex` and `presentation/slides.tex` (pdfLaTeX).
`01_cleaning.R` uses base R only. `02_descriptive.R` needs dplyr, ggplot2 and xtable
(`install.packages(c("dplyr", "ggplot2", "xtable"))`); `03_analysis.R` installs any
missing packages itself on its first run.

Author: Anthony George
