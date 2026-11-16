# =============================================================================
# 02_descriptive.R
# Describes every variable used in A1-A10 (and every group that is compared),
# and decides for each one: PARAMETRIC or NON-PARAMETRIC test.
# Reads only analysis/dataset.csv (made by 01_cleaning.R).
# Exports: report/tables/D_table.tex   (the decision table)
#          report/tables/D_stats.tex   (full descriptive statistics)
#          report/tables/D_categorical.tex (counts and percentages)
#          report/figures/D_*.pdf      (plots of every variable and group)
#
# Reading guide: each line of code has a comment.
#   WHAT = what the line achieves
#   HOW  = which function or trick does it
# =============================================================================


# ---- 0. Setup ---------------------------------------------------------------

# WHAT: if the script is started from inside the analysis folder, move up one
#       level to the project folder, so the paths below work either way.
# HOW:  getwd() gives the current folder; basename() takes its last part;
#       setwd("..") moves to the folder above.
if (basename(getwd()) == "analysis") setwd("..")

# WHAT: loads three add-on packages: dplyr (data handling), ggplot2 (plots),
#       xtable (turns R tables into LaTeX tables for the report).
# HOW:  library() makes a package's functions available.
library(dplyr)
library(ggplot2)
library(xtable)

# WHAT: makes sure the output folders exist.
# HOW:  dir.create() makes a folder; recursive = TRUE also makes missing parent
#       folders; showWarnings = FALSE keeps quiet if the folder already exists.
dir.create("report/tables",  recursive = TRUE, showWarnings = FALSE)
dir.create("report/figures", recursive = TRUE, showWarnings = FALSE)

# WHAT: loads the clean data from 01_cleaning.R and calls it 'df'.
# HOW:  read.csv(). This time R may guess the types, because the data is clean:
#       seats become numbers and TRUE/FALSE columns become logical.
df <- read.csv("analysis/dataset.csv")

# WHAT: the random seed, so the bootstrap gives the same result on every run.
#       (The report states this number.)
# HOW:  a plain number stored under a name.
SEED <- 2026

# WHAT: how many bootstrap resamples to draw (the brief asks for at least 5,000).
# HOW:  a plain number stored under a name.
N_BOOT <- 5000


# ---- 1. A helper to measure skewness ----------------------------------------
# Skewness measures how lopsided a distribution is: 0 = symmetric,
# positive = a long tail to the right (a few very big cafes).
# Our rule of thumb: |skewness| < 1 counts as roughly symmetric.

# WHAT: defines our own function skewness(x).
# HOW:  'function(x) { ... }' creates a function; the lines inside { } run
#       each time it is used, and the last line is the answer it returns.
skewness <- function(x) {
  # WHAT: drops the missing values first.
  # HOW:  !is.na(x) is TRUE for the values that are present; x[ ] keeps those.
  x <- x[!is.na(x)]
  # WHAT: the skewness formula: the average cubed distance from the mean,
  #       divided by the standard deviation cubed.
  # HOW:  mean() = average, sd() = standard deviation, ^3 = to the power of 3.
  mean((x - mean(x))^3) / sd(x)^3
}


# ---- 2. describe(): numbers + 4 plots for one variable (or one group) ---------
# The 4 plots: histogram | boxplot | normal Q-Q plot | bootstrapped means.
# Decision rules (fixed before looking at the data):
#   * Normal      : |skewness| < 1
#   * CLT applies : n >= 30 AND the bootstrapped means look normal
#                   (|skewness of the means| < 0.5)
#   * Parametric if Normal OR CLT applies; otherwise non-parametric.
# The automatic rule summarises the plots. LOOK at each D_*.pdf too, and if you
# disagree with a verdict, change it below with a comment saying why.

# WHAT: defines the function describe(), which takes a variable x, its name,
#       the group it belongs to, and a short name for the plot file.
# HOW:  'group = "All cafes"' is a default, used when no group is given.
describe <- function(x, variable, group = "All cafes", file_stub) {

  # WHAT: counts the missing values (they are reported in the table).
  # HOW:  is.na() + sum().
  n_miss <- sum(is.na(x))

  # WHAT: drops the missing values from here on.
  # HOW:  keeps only the values where !is.na(x) is TRUE.
  x <- x[!is.na(x)]

  # WHAT: the number of observations that are left.
  # HOW:  length() counts the values.
  n <- length(x)

  # WHAT: the skewness of the variable.
  # HOW:  our helper function from section 1.
  sk <- skewness(x)

  # WHAT: the Shapiro-Wilk normality test p-value (optional, for the appendix;
  #       the plots matter more, as the brief says).
  # HOW:  shapiro.test() only works with 3 to 5000 values that are not all
  #       equal, so we run it only then; otherwise NA. $p.value takes the p-value.
  sw_p <- if (n >= 3 && n <= 5000 && sd(x) > 0) shapiro.test(x)$p.value else NA

  # WHAT: fixes the random numbers, so the bootstrap is reproducible.
  # HOW:  set.seed() with our SEED.
  set.seed(SEED)

  # WHAT: the bootstrap: 5,000 times, draw a new sample of n cafes from our own
  #       data (with replacement) and take its mean. This shows how the MEAN
  #       would vary from sample to sample.
  # HOW:  sample(x, n, replace = TRUE) draws n values, each can be drawn twice;
  #       mean() averages them; replicate() repeats this N_BOOT times.
  boot_means <- replicate(N_BOOT, mean(sample(x, n, replace = TRUE)))

  # WHAT: how lopsided the 5,000 bootstrapped means are.
  # HOW:  our skewness() helper again. Close to 0 = the mean is close to normal.
  boot_sk <- skewness(boot_means)

  # WHAT: rule 1: is the variable itself roughly normal?
  # HOW:  abs() = absolute value (drops the minus sign); '<' gives TRUE/FALSE.
  normal <- abs(sk) < 1

  # WHAT: rule 2: does the central limit theorem help? (enough cafes AND the
  #       bootstrapped means look normal)
  # HOW:  '&&' = AND for single TRUE/FALSE values.
  clt <- n >= 30 && abs(boot_sk) < 0.5

  # WHAT: the conclusion: parametric if rule 1 OR rule 2 holds.
  # HOW:  '||' = OR; if/else picks one of the two texts.
  verdict <- if (normal || clt) "Parametric" else "Non-parametric"

  # WHAT: opens a PDF file for the 4 plots, e.g. report/figures/D_seats_inside.pdf.
  # HOW:  paste0() glues text with no spaces; file.path() joins folder and file
  #       name; pdf() starts drawing into that file (10 x 2.8 inches).
  pdf(file.path("report/figures", paste0("D_", file_stub, ".pdf")), width = 10, height = 2.8)

  # WHAT: puts the 4 plots side by side in one row, with small margins.
  # HOW:  par(mfrow = c(1, 4)) = 1 row, 4 columns; mar = margins
  #       (bottom, left, top, right).
  par(mfrow = c(1, 4), mar = c(4, 4, 2.5, 1))

  # WHAT: plot 1, the histogram: how the values are distributed.
  # HOW:  hist() with 20 bars; main = title ("\n" starts a new line);
  #       cex.main = 0.9 makes the title slightly smaller.
  hist(x, breaks = 20, col = "grey80", border = "white",
       main = paste0(variable, "\n", group), xlab = variable, cex.main = 0.9)

  # WHAT: plot 2, the boxplot: median, quartiles and outliers at a glance.
  # HOW:  boxplot(), drawn sideways with horizontal = TRUE.
  boxplot(x, horizontal = TRUE, col = "grey80", main = "Boxplot", xlab = variable)

  # WHAT: plot 3, the normal Q-Q plot: if the data were normal, the points
  #       would lie on a straight line.
  # HOW:  qqnorm() draws the points (pch = 16 = filled dots, cex = dot size).
  qqnorm(x, main = "Normal Q-Q", pch = 16, cex = 0.6)

  # WHAT: adds the reference line to the Q-Q plot.
  # HOW:  qqline() draws it in red.
  qqline(x, col = "red")

  # WHAT: plot 4, histogram of the 5,000 bootstrapped means: if it looks like a
  #       bell curve, the central limit theorem works for this variable.
  # HOW:  hist() with 40 bars.
  hist(boot_means, breaks = 40, col = "grey80", border = "white",
       main = paste0("Bootstrap means (", N_BOOT, ")"), xlab = "Mean")

  # WHAT: closes and saves the PDF file.
  # HOW:  dev.off() = "device off", i.e. stop drawing into the file.
  dev.off()

  # WHAT: returns one row of results for this variable or group.
  # HOW:  data.frame() builds a one-row table; the last expression of a function
  #       is its answer. mean, sd, median, IQR (interquartile range), min, max
  #       are the summary numbers the brief asks for; ifelse() turns TRUE/FALSE
  #       into "Yes"/"No"; stringsAsFactors = FALSE keeps text as plain text.
  data.frame(Variable = variable, Group = group, n = n, Missing = n_miss,
             Mean = mean(x), SD = sd(x), Median = median(x), IQR = IQR(x),
             Min = min(x), Max = max(x), Skewness = sk, Shapiro_p = sw_p,
             Boot_skew = boot_sk,
             Normal = ifelse(normal, "Yes", "No"),
             CLT = ifelse(clt, "Yes", ifelse(n < 30, "No (n < 30)", "No")),
             Conclusion = verdict, stringsAsFactors = FALSE)
}


# ---- 3. Describe every variable and group the questions use ------------------

# WHAT: an empty list that will collect one result row per variable/group.
# HOW:  list() makes an empty container.
rows <- list()

# A1, A6, A7, A8, A10: seats inside, all cafes together.
# WHAT: describes seats inside and adds the result to the list.
# HOW:  rows[[length(rows) + 1]] means "the next free place in the list".
rows[[length(rows) + 1]] <- describe(df$seats_inside, "Seats inside", file_stub = "seats_inside")

# A2: seats inside, separately for cafes WITH and WITHOUT outdoor seating
#     (the groups being compared must each be checked).
# WHAT: repeats the next lines once for TRUE (with outdoor) and once for FALSE.
# HOW:  a for-loop over the two values.
for (g in c(TRUE, FALSE)) {
  # WHAT: a readable label for the group.
  # HOW:  ifelse() picks the text depending on g.
  lab <- ifelse(g, "Outdoor seating", "No outdoor seating")
  # WHAT: describes seats inside for this group only, and adds it to the list.
  # HOW:  df$has_outdoor %in% g picks the cafes in this group (NA is left out);
  #       paste0() builds the file name, e.g. "seats_inside_outdoor_TRUE".
  rows[[length(rows) + 1]] <- describe(df$seats_inside[df$has_outdoor %in% g],
                                       "Seats inside", lab,
                                       paste0("seats_inside_outdoor_", g))
}

# A3: a paired comparison, so what matters is the DIFFERENCE within each cafe.
# WHAT: seats inside minus seats outside, for every cafe.
# HOW:  subtracting two columns works cafe by cafe; NA if either is missing.
df$diff_in_out <- df$seats_inside - df$seats_outside

# WHAT: describes that difference and adds it to the list.
# HOW:  describe() as above.
rows[[length(rows) + 1]] <- describe(df$diff_in_out, "Inside minus outside",
                                     "Paired difference", "diff_in_out")

# A4: seats outside, separately for each of the six districts compared.
# WHAT: repeats the next line for "Prague 1", "Prague 2", ... "Prague 8".
# HOW:  paste("Prague", c(1, 2, 3, 5, 7, 8)) builds the six labels.
for (d in paste("Prague", c(1, 2, 3, 5, 7, 8))) {
  # WHAT: describes seats outside for this district, and adds it to the list.
  # HOW:  %in% d picks the cafes in district d; gsub(" ", "", d) removes the
  #       space for the file name ("Prague1").
  rows[[length(rows) + 1]] <- describe(df$seats_outside[df$district_A4 %in% d],
                                       "Seats outside", d,
                                       paste0("seats_outside_", gsub(" ", "", d)))
}

# A6-A10: the Price Level (the outcome) and the distance from the Orloj
#         (the extra predictor in A8).
# WHAT: describes both and adds them to the list.
# HOW:  describe() as above.
rows[[length(rows) + 1]] <- describe(df$price_level, "Price Level (CZK)", file_stub = "price_level")
rows[[length(rows) + 1]] <- describe(df$dist_orloj, "Distance from Orloj (km)", file_stub = "dist_orloj")

# WHAT: stacks all the result rows into one table, D.
# HOW:  rbind() stacks tables on top of each other; do.call() hands it every
#       element of the list at once.
D <- do.call(rbind, rows)

# WHAT: prints the table to the Console, with 3 significant digits.
# HOW:  print() with digits = 3.
print(D, digits = 3)


# ---- 4. Group plots (groups side by side) ------------------------------------

# WHAT: plot for A2: seats inside, with vs without outdoor seating, side by side.
# HOW:  ggplot() builds a plot in layers joined with '+'.
#       filter() keeps cafes where both values are known.
#       aes() says what goes on the axes: x = the group label, y = seats inside.
g1 <- ggplot(filter(df, !is.na(has_outdoor), !is.na(seats_inside)),
             aes(x = ifelse(has_outdoor, "Outdoor seating", "No outdoor seating"),
                 y = seats_inside)) +
  # WHAT: a boxplot per group, without separate outlier dots (the points
  #       below already show every cafe).
  # HOW:  geom_boxplot(); outlier.shape = NA hides the outlier dots.
  geom_boxplot(outlier.shape = NA, fill = "grey90") +
  # WHAT: one dot per cafe, spread out sideways so they do not overlap.
  # HOW:  geom_jitter(); width = sideways spread; alpha = 0.5 = half transparent.
  geom_jitter(width = 0.15, alpha = 0.5, size = 1.2) +
  # WHAT: axis labels and a clean look.
  # HOW:  labs() sets labels (x = NULL = no label); theme_minimal() = plain style.
  labs(x = NULL, y = "Seats inside") + theme_minimal()

# WHAT: saves the plot as a PDF for the report.
# HOW:  ggsave(file, plot, size in inches).
ggsave("report/figures/D_groups_A2.pdf", g1, width = 5, height = 4)

# WHAT: plot for A4: seats outside in each of the six districts, side by side.
# HOW:  the same layers as above, with district on the x-axis.
g2 <- ggplot(filter(df, !is.na(district_A4), !is.na(seats_outside)),
             aes(x = district_A4, y = seats_outside)) +
  geom_boxplot(outlier.shape = NA, fill = "grey90") +    # WHAT: a box per district. HOW: geom_boxplot().
  geom_jitter(width = 0.15, alpha = 0.5, size = 1.2) +  # WHAT: one dot per cafe. HOW: geom_jitter().
  labs(x = NULL, y = "Seats outside") + theme_minimal()  # WHAT: labels and style. HOW: labs(), theme_minimal().

# WHAT: saves it as a PDF.
# HOW:  ggsave().
ggsave("report/figures/D_groups_A4.pdf", g2, width = 6, height = 4)


# ---- 5. Categorical variables (yes/no variables, used in A5, A2, A9, A10) ----
# These need no normality check: we just count them.

# WHAT: a table with the Yes and No counts of four yes/no variables.
# HOW:  each data.frame() makes two rows (Yes, No) for one variable;
#       sum(x, na.rm = TRUE) counts the TRUEs (= Yes); sum(!x, ...) counts the
#       FALSEs (= No), because '!' turns FALSE into TRUE; rbind() stacks them.
cat_tab <- rbind(
  data.frame(Variable = "Small cafe (<= 15 seats inside)",
             Level = c("Yes", "No"),
             Count = c(sum(df$small_cafe, na.rm = TRUE), sum(!df$small_cafe, na.rm = TRUE))),
  data.frame(Variable = "Top-rated (rating >= 4.7)",
             Level = c("Yes", "No"),
             Count = c(sum(df$top_rated, na.rm = TRUE), sum(!df$top_rated, na.rm = TRUE))),
  data.frame(Variable = "Outdoor seating",
             Level = c("Yes", "No"),
             Count = c(sum(df$has_outdoor, na.rm = TRUE), sum(!df$has_outdoor, na.rm = TRUE))),
  data.frame(Variable = "Above-median Price Level",
             Level = c("Yes", "No"),
             Count = c(sum(df$above_median_price, na.rm = TRUE),
                       sum(!df$above_median_price, na.rm = TRUE)))
)

# WHAT: adds the percentage of Yes and No within each variable.
# HOW:  %>% passes the table on to the next step; group_by(Variable) makes
#       sum(Count) the total of each variable; mutate() adds the new column;
#       ungroup() and as.data.frame() turn it back into a plain table.
cat_tab <- cat_tab %>% group_by(Variable) %>%
  mutate(Percent = 100 * Count / sum(Count)) %>% ungroup() %>% as.data.frame()

# WHAT: shows the counts and percentages in the Console.
# HOW:  print().
print(cat_tab)


# ---- 6. Export the tables for the report --------------------------------------

# WHAT: picks the columns of the decision table, which the report's main text
#       refers to (Table "D").
# HOW:  D[, c(...)] keeps only the listed columns.
dec <- D[, c("Variable", "Group", "n", "Skewness", "Normal", "CLT", "Conclusion")]

# WHAT: writes it as a LaTeX table to report/tables/D_table.tex.
# HOW:  xtable() converts it; digits = decimals per column (the first number is
#       for row names); include.rownames = FALSE drops R's row numbers;
#       booktabs = TRUE uses the nice table lines the brief asks for;
#       floating = FALSE, because the caption is added in the .tex report.
print(xtable(dec, digits = c(0, 0, 0, 0, 2, 0, 0, 0)),
      file = "report/tables/D_table.tex", include.rownames = FALSE,
      booktabs = TRUE, floating = FALSE)

# WHAT: picks the columns for the full statistics table (for the appendix).
# HOW:  same as above.
st <- D[, c("Variable", "Group", "n", "Missing", "Mean", "SD", "Median", "IQR",
            "Min", "Max", "Shapiro_p", "Boot_skew")]

# WHAT: gives two columns readable headings.
# HOW:  names(st) == "Shapiro_p" finds that column; '<-' gives it a new name.
names(st)[names(st) == "Shapiro_p"] <- "Shapiro p"
names(st)[names(st) == "Boot_skew"] <- "Skew of boot. means"

# WHAT: writes it to report/tables/D_stats.tex, in a smaller font.
# HOW:  xtable() + print() as above; size = "\\footnotesize" shrinks the font.
print(xtable(st, digits = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 3, 2)),
      file = "report/tables/D_stats.tex", include.rownames = FALSE,
      booktabs = TRUE, floating = FALSE, size = "\\footnotesize")

# WHAT: writes the yes/no counts to report/tables/D_categorical.tex.
# HOW:  xtable() + print() as above; percentages with 1 decimal.
print(xtable(cat_tab, digits = c(0, 0, 0, 0, 1)),
      file = "report/tables/D_categorical.tex", include.rownames = FALSE,
      booktabs = TRUE, floating = FALSE)

# WHAT: saves the decisions as a file, so 03_analysis.R uses exactly what was
#       decided here (parametric or not) instead of deciding again.
# HOW:  write.csv() without row numbers.
write.csv(D, "analysis/descriptive_decisions.csv", row.names = FALSE)

# WHAT: a message that the script has finished.
# HOW:  cat() prints text; "\n" ends the line.
cat("Descriptive tables and figures exported.\n")
