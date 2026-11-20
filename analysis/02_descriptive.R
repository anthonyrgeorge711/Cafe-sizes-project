
# WHAT: loads xtable, the package that turns an R table into a LaTeX table
#       for the report.
# HOW:  library(). (It's installed already; the real 03 script installed it.)
library(xtable)

# WHAT: makes the folders where the report looks for tables and figures.
# HOW:  dir.create(); recursive = TRUE also makes the parent folder 'report';
#       showWarnings = FALSE stays quiet if they already exist.
dir.create("report/tables",  recursive = TRUE, showWarnings = FALSE)
dir.create("report/figures", recursive = TRUE, showWarnings = FALSE)


df <- read.csv("analysis/dataset.csv")

hist(df$seats_inside, breaks =30, main= "Seats inside", xlab = "Seats inside")
# cafes with or without seat count
sum(!is.na(df$seats_inside))
sum(is.na(df$seats_inside))

#average and middle

mean(df$seats_inside, na.rm = TRUE)
median(df$seats_inside, na.rm = TRUE)

sd(df$seats_inside, na.rm = TRUE)

x <- na.omit(df$seats_inside)

mean((x - mean(x))^3) / sd(x)^3

#-------     

qqnorm(x, main= "Q-Q plot: seats inside")

qqline(x, col="red")

#----
length(x)

set.seed(2026)

boot_means <- replicate(5000, mean(sample(x, replace = TRUE)))
hist(boot_means, breaks = 40, main = "5000 bootsrapped means")

mean((boot_means - mean(boot_means))^3) / sd(boot_means)^3


#------

#checks

# WHAT: our check function, now also saving 4 plots as a PDF and returning the
#       descriptive statistics. 'file' is the short name for the PDF.
# HOW:  function(x, file) takes two inputs: the values, and a file name.
check <- function(x, file) {
  # WHAT: drop missing values.  HOW: na.omit().
  x <- na.omit(x)
  # WHAT: skewness of the data itself.  HOW: the same formula as before.
  skew <- mean((x - mean(x))^3) / sd(x)^3
  # WHAT: the bootstrap of the mean.  HOW: set.seed + replicate, as before.
  set.seed(2026)
  boot_means <- replicate(5000, mean(sample(x, replace = TRUE)))
  # WHAT: skewness of the 5,000 averages.  HOW: same formula.
  boot_skew <- mean((boot_means - mean(boot_means))^3) / sd(boot_means)^3
  # WHAT: rule 1: normal if |skewness| < 1.  HOW: abs() drops the minus sign.
  normal <- abs(skew) < 1
  # WHAT: rule 2: CLT applies if 30+ cafes AND the averages look normal.  HOW: '&' = AND.
  clt <- length(x) >= 30 & abs(boot_skew) < 0.5
  # WHAT: the verdict.  HOW: '|' = OR; ifelse() picks the word.
  verdict <- ifelse(normal | clt, "Parametric", "Non-parametric")
  
  # WHAT: opens a PDF file, e.g. report/figures/D_seats_inside.pdf.
  # HOW:  paste0() glues the pieces of the name together with no spaces;
  #       pdf() starts drawing into that file (10 x 2.8 inches).
  pdf(paste0("report/figures/D_", file, ".pdf"), width = 10, height = 2.8)
  # WHAT: puts 4 plots side by side in one row.  HOW: par(mfrow = c(rows, columns)).
  par(mfrow = c(1, 4))
  # WHAT: plot 1, histogram.  HOW: hist().
  hist(x, breaks = 20, main = "Histogram", xlab = file)
  # WHAT: plot 2, boxplot.  HOW: boxplot(), drawn sideways.
  boxplot(x, horizontal = TRUE, main = "Boxplot")
  # WHAT: plot 3, normal Q-Q plot with its red reference line.  HOW: qqnorm(), qqline().
  qqnorm(x, main = "Normal Q-Q")
  qqline(x, col = "red")
  # WHAT: plot 4, the 5,000 bootstrapped means.  HOW: hist().
  hist(boot_means, breaks = 40, main = "Bootstrapped means", xlab = "Mean")
  # WHAT: closes and saves the PDF.  HOW: dev.off() = "device off".
  dev.off()
  
  # WHAT: returns one row: the statistics (rounded) plus the verdict.
  # HOW:  data.frame(); IQR() = interquartile range; round(x, 1) = 1 decimal.
  data.frame(n = length(x), mean = round(mean(x), 1), sd = round(sd(x), 1),
             median = median(x), iqr = IQR(x), min = min(x), max = max(x),
             skew = round(skew, 2), normal, clt, verdict)
}

# ---- Step 6: The decision table ----

# WHAT: runs check() on every variable and group, saving a PDF for each, and
#       stacks the results into one table D (one row per group).
# HOW:  each check(values, "file name") returns one row; cbind() adds the label;
#       rbind() stacks the rows.
D <- rbind(
  cbind(group = "A1: seats inside, all cafes",
        check(df$seats_inside, "seats_inside")),
  cbind(group = "A2: seats inside, WITH terrace",
        check(df$seats_inside[df$has_outdoor %in% TRUE], "seats_inside_outdoor_TRUE")),
  cbind(group = "A2: seats inside, WITHOUT terrace",
        check(df$seats_inside[df$has_outdoor %in% FALSE], "seats_inside_outdoor_FALSE")),
  cbind(group = "A3: inside minus outside",
        check(df$seats_inside - df$seats_outside, "diff_in_out")),
  cbind(group = "A4: seats outside, Prague 1",
        check(df$seats_outside[df$district_A4 %in% "Prague 1"], "seats_outside_Prague1")),
  cbind(group = "A4: seats outside, Prague 2",
        check(df$seats_outside[df$district_A4 %in% "Prague 2"], "seats_outside_Prague2")),
  cbind(group = "A4: seats outside, Prague 3",
        check(df$seats_outside[df$district_A4 %in% "Prague 3"], "seats_outside_Prague3")),
  cbind(group = "A4: seats outside, Prague 5",
        check(df$seats_outside[df$district_A4 %in% "Prague 5"], "seats_outside_Prague5")),
  cbind(group = "A4: seats outside, Prague 7",
        check(df$seats_outside[df$district_A4 %in% "Prague 7"], "seats_outside_Prague7")),
  cbind(group = "A4: seats outside, Prague 8",
        check(df$seats_outside[df$district_A4 %in% "Prague 8"], "seats_outside_Prague8")),
  cbind(group = "A6-A10: Price Level (CZK)",
        check(df$price_level, "price_level")),
  cbind(group = "A8: distance from Orloj (km)",
        check(df$dist_orloj, "dist_orloj"))
)

# WHAT: shows the table.  HOW: typing its name prints it.
D

# WHAT: saves the DECISION columns as report/tables/D_table.tex (main text).
# HOW:  D[, c(...)] picks the columns; xtable() + print(file = ) writes LaTeX;
#       no row numbers; booktabs = clean lines; floating = FALSE (caption is in report.tex).
print(xtable(D[, c("group", "n", "skew", "normal", "clt", "verdict")]),
      file = "report/tables/D_table.tex",
      include.rownames = FALSE, booktabs = TRUE, floating = FALSE)

# WHAT: saves the STATISTICS columns as report/tables/D_stats.tex (appendix).
# HOW:  same as above, other columns.
print(xtable(D[, c("group", "n", "mean", "sd", "median", "iqr", "min", "max")]),
      file = "report/tables/D_stats.tex",
      include.rownames = FALSE, booktabs = TRUE, floating = FALSE)

# ---- Step 7: Yes/no variables (counts and percentages) ----

# WHAT: a table with the Yes and No counts of the four yes/no variables.
# HOW:  sum(x, na.rm = TRUE) counts the TRUEs (Yes); sum(!x, ...) counts the
#       FALSEs (No), because '!' flips FALSE to TRUE.
cat_tab <- data.frame(
  variable = c("Small cafe (15 or fewer seats)", "Top-rated (4.7 or higher)",
               "Outdoor seating", "Above-median Price Level"),
  yes = c(sum(df$small_cafe, na.rm = TRUE), sum(df$top_rated, na.rm = TRUE),
          sum(df$has_outdoor, na.rm = TRUE), sum(df$above_median_price, na.rm = TRUE)),
  no  = c(sum(!df$small_cafe, na.rm = TRUE), sum(!df$top_rated, na.rm = TRUE),
          sum(!df$has_outdoor, na.rm = TRUE), sum(!df$above_median_price, na.rm = TRUE)))

# WHAT: the percentage of Yes for each variable.
# HOW:  yes / (yes + no) x 100, rounded to 1 decimal.
cat_tab$percent_yes <- round(100 * cat_tab$yes / (cat_tab$yes + cat_tab$no), 1)

# WHAT: shows it, and saves it as report/tables/D_categorical.tex.
# HOW:  print, then xtable() + print(file = ).
cat_tab
print(xtable(cat_tab), file = "report/tables/D_categorical.tex",
      include.rownames = FALSE, booktabs = TRUE, floating = FALSE)