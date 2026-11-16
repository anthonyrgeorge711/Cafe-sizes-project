# =============================================================================
# 03_analysis.R  -  Cafe Size topic, analyses A1-A10
# Reads analysis/dataset.csv (from 01_cleaning.R) and
#       analysis/descriptive_decisions.csv (from 02_descriptive.R).
# Every analysis block follows the same order:
#   hypotheses -> method -> assumption checks -> test
#   -> table (report/tables/Ak_table.tex) -> figure (report/figures/Ak_figure.pdf)
# alpha = .05 throughout. Every bootstrap: seed 2026, 5,000 resamples.
#
# Reading guide: each line of code has a comment.
#   WHAT = what the line achieves
#   HOW  = which function or trick does it
# =============================================================================


# ---- 0. Setup ---------------------------------------------------------------

# WHAT: if the script is started from inside the analysis folder, move up to the
#       project folder, so the paths below work either way.
# HOW:  basename(getwd()) = name of the current folder; setwd("..") goes up one.
if (basename(getwd()) == "analysis") setwd("..")

# WHAT: a helper that installs a package if it is missing, then loads it.
# HOW:  for every package name: requireNamespace() checks whether it is
#       installed (quietly = no messages); if not ('!'), install.packages()
#       installs it; library() loads it (character.only = TRUE because the name
#       is stored in a variable, not typed directly).
load_packages <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# WHAT: loads every package this script needs:
#   dplyr (data handling), ggplot2 (plots), xtable (LaTeX tables),
#   car (Levene's test, VIF), sandwich + lmtest (robust standard errors and
#   regression tests), rstatix (Dunn and Games-Howell post hoc tests),
#   marginaleffects (marginal effects for the logistic regression in A10).
# HOW:  our helper above, with c() listing the names.
load_packages(c("dplyr", "ggplot2", "xtable", "car", "sandwich", "lmtest",
                "rstatix", "marginaleffects"))

# WHAT: makes sure the output folders exist.
# HOW:  dir.create(); recursive = TRUE also makes parent folders;
#       showWarnings = FALSE keeps quiet if they already exist.
dir.create("report/tables",  recursive = TRUE, showWarnings = FALSE)
dir.create("report/figures", recursive = TRUE, showWarnings = FALSE)

# WHAT: loads the clean data.
# HOW:  read.csv().
df <- read.csv("analysis/dataset.csv")

# WHAT: loads the parametric / non-parametric decisions made in 02_descriptive.R,
#       so this script uses exactly what was decided there.
# HOW:  read.csv().
dec <- read.csv("analysis/descriptive_decisions.csv")

# WHAT: seats inside measured in units of 10 seats, because A7-A10 ask about
#       the change "per 10 seats".
# HOW:  dividing the column by 10 (e.g. 35 seats -> 3.5).
df$seats10 <- df$seats_inside / 10

# WHAT: fixed settings: random seed, number of bootstrap resamples, significance level.
# HOW:  plain numbers stored under names.
SEED   <- 2026
N_BOOT <- 5000
ALPHA  <- 0.05

# WHAT: makes every ggplot in this script use the same plain style.
# HOW:  theme_set() sets the default theme; base_size = font size.
theme_set(theme_minimal(base_size = 11))


# ---- 1. Helper functions ---------------------------------------------------
# Small functions used again and again below, so every number is formatted the
# same way, as the brief's reporting rules ask (B.4).

# WHAT: formats a number with 2 decimals (estimates, test statistics).
# HOW:  formatC(x, format = "f", digits = 2): "f" = fixed decimals.
f2 <- function(x) formatC(x, format = "f", digits = 2)

# WHAT: formats a number with 1 decimal (prices, seats).
# HOW:  as above, with digits = 1.
f1 <- function(x) formatC(x, format = "f", digits = 1)

# WHAT: formats a p-value the way the brief asks: 3 decimals, no leading zero
#       (".034"), and "< .001" for very small values.
# HOW:  ifelse(): if p < .001 write "< .001"; otherwise format with 3 decimals
#       and delete the leading "0" with sub("^0", "", ...).
fp <- function(p) ifelse(p < .001, "< .001", sub("^0", "", formatC(p, format = "f", digits = 3)))

# WHAT: writes a confidence interval as "[lower, upper]".
# HOW:  paste0() glues the pieces; f = which formatter to use (2 decimals by default).
ci <- function(lo, hi, f = f2) paste0("[", f(lo), ", ", f(hi), "]")

# WHAT: looks up in the decision table whether a variable (and every compared
#       group of it) may be analysed with a parametric test.
# HOW:  dec[dec$Variable == variable, ] keeps that variable's rows; if groups are
#       given, %in% keeps only those groups; all() is TRUE only if EVERY
#       remaining row says "Parametric".
is_param <- function(variable, groups = NULL) {
  d <- dec[dec$Variable == variable, ]
  if (!is.null(groups)) d <- d[d$Group %in% groups, ]
  all(d$Conclusion == "Parametric")
}

# WHAT: saves a results table as LaTeX, e.g. report/tables/A1_table.tex.
# HOW:  xtable() converts the table; print(..., file = ) writes it; no row
#       numbers; booktabs = TRUE for nice lines; floating = FALSE because the
#       caption is added in report.tex; size = optional smaller font.
save_tab <- function(tab, name, size = NULL) {
  print(xtable(tab), file = file.path("report/tables", paste0(name, "_table.tex")),
        include.rownames = FALSE, booktabs = TRUE, floating = FALSE, size = size)
}

# WHAT: saves a ggplot as PDF, e.g. report/figures/A1_figure.pdf.
# HOW:  ggsave(file, plot, width, height in inches).
save_fig <- function(p, name, w = 6, h = 4)
  ggsave(file.path("report/figures", paste0(name, "_figure.pdf")), p, width = w, height = h)

# WHAT: bootstrap 95% confidence interval for a mean (used when the variable is
#       skewed and the usual t-based interval is doubtful).
# HOW:  set.seed() makes it reproducible; replicate() draws 5,000 resamples
#       (sample(..., replace = TRUE)) and takes each one's mean; quantile() takes
#       the 2.5% and 97.5% points of those means = the percentile interval.
boot_ci_mean <- function(x) {
  set.seed(SEED)
  quantile(replicate(N_BOOT, mean(sample(x, length(x), replace = TRUE))), c(.025, .975))
}

# WHAT: an empty list that collects one summary line per analysis (for the
#       summary table at the end).
# HOW:  list().
summary_rows <- list()

# WHAT: adds one line to that summary list.
# HOW:  data.frame() builds the line; '<<-' (instead of '<-') changes the list
#       that lives OUTSIDE this function, so the line is kept after it ends.
add_summary <- function(a, question, method, estimate, stat, p)
  summary_rows[[length(summary_rows) + 1]] <<-
    data.frame(Analysis = a, Question = question, Method = method,
               Estimate = estimate, Statistic = stat, p = p)

# WHAT: lists the influential cafes in a regression: those with Cook's distance
#       above 4/n (a common rule of thumb).
# HOW:  cooks.distance() measures how much each cafe moves the fitted line;
#       names(cd) are the row numbers, which tell us which cafe each value
#       belongs to; we keep those above 4 / number of cafes.
influential <- function(m, data) {
  cd  <- cooks.distance(m)
  idx <- as.integer(names(cd)[cd > 4 / length(cd)])
  data.frame(cafe = data$cafe_name[idx], cooks_d = round(cd[cd > 4 / length(cd)], 3))
}

# WHAT: regression diagnostics: saves the plots for the appendix and tests
#       whether the residual variance is constant. Answers TRUE if it is NOT
#       constant, which means robust standard errors are needed.
# HOW:  pdf() opens the file; par(mfrow = c(1, 3)) puts 3 plots in a row;
#       plot(m, which = c(1, 2, 4)) draws residuals vs fitted, the Q-Q plot of
#       residuals and Cook's distance; dev.off() saves the file.
#       bptest() is the Breusch-Pagan test (H0: constant variance); its p-value
#       is printed; the last line returns TRUE if p < .05.
diagnostics <- function(m, name) {
  pdf(file.path("report/figures", paste0(name, "_diagnostics.pdf")), width = 9, height = 3.5)
  par(mfrow = c(1, 3))
  plot(m, which = c(1, 2, 4))
  dev.off()
  bp <- bptest(m)
  cat(name, "Breusch-Pagan p =", fp(bp$p.value), "\n")
  bp$p.value < ALPHA
}

# WHAT: builds the regression table: coefficient (B), standard error, 95% CI,
#       t and p for each term; with robust (HC3) standard errors if needed.
# HOW:  vcovHC(type = "HC3") = robust variance, vcov() = ordinary variance;
#       coeftest() gives B, SE, t, p using that variance; coefci() gives the CIs.
#       check.names = FALSE keeps the column name "95% CI" as written.
reg_table <- function(m, robust) {
  V   <- if (robust) vcovHC(m, type = "HC3") else vcov(m)
  ct  <- coeftest(m, vcov. = V)
  cfi <- coefci(m, vcov. = V)
  data.frame(Term = rownames(ct), B = f2(ct[, 1]), SE = f2(ct[, 2]),
             `95% CI` = ci(cfi[, 1], cfi[, 2]), t = f2(ct[, 3]), p = fp(ct[, 4]),
             check.names = FALSE)
}


# =============================================================================
# A1  One sample: Is the average number of seats inside different from 30?
#   H0: mean seats inside = 30      H1: mean != 30 (two-tailed)
# =============================================================================

# WHAT: seats inside without the missing values.
# HOW:  na.omit() drops the NAs.
x <- na.omit(df$seats_inside)

# WHAT: how many cafes are used.
# HOW:  length() counts the values.
n <- length(x)

# WHAT: prints n and how many cafes were lost to missing values (the brief asks
#       to report this for every analysis).
# HOW:  cat() prints; sum(is.na()) counts the missing values.
cat("\nA1: n =", n, "| lost to NA:", sum(is.na(df$seats_inside)), "\n")

# WHAT: picks the method according to the decision table from 02_descriptive.R.
# HOW:  is_param() reads the table; if TRUE we run the t-test, else the
#       Wilcoxon test.
if (is_param("Seats inside", "All cafes")) {

  # WHAT: the one-sample t-test against 30.
  # HOW:  t.test(x, mu = 30); mu = the benchmark under H0.
  a1 <- t.test(x, mu = 30)

  # WHAT: a bootstrap CI for the mean, because seats inside are very skewed.
  # HOW:  our helper boot_ci_mean().
  bci <- boot_ci_mean(x)

  # WHAT: Cohen's d for one sample = how many standard deviations the mean is
  #       away from 30.
  # HOW:  (mean - 30) / standard deviation.
  d1 <- (mean(x) - 30) / sd(x)

  # WHAT: the results table: n, mean, both CIs, median, IQR, d, t and p.
  # HOW:  data.frame(); a1$conf.int = the t-based CI; a1$parameter = degrees of
  #       freedom; a1$statistic = t; a1$p.value = p; the f1/f2/fp/ci helpers
  #       format the numbers.
  A1 <- data.frame(n = n, Mean = f1(mean(x)),
                   `t-based 95% CI` = ci(a1$conf.int[1], a1$conf.int[2], f1),
                   `Bootstrap 95% CI` = ci(bci[1], bci[2], f1),
                   Median = f1(median(x)), IQR = f1(IQR(x)), d = f2(d1),
                   t = paste0("t(", a1$parameter, ") = ", f2(a1$statistic)),
                   p = fp(a1$p.value), check.names = FALSE)

  # WHAT: the A1 line for the summary table at the end.
  # HOW:  our helper add_summary(), with text built by paste0().
  add_summary("A1", "Mean seats inside vs 30", "One-sample t-test",
              paste0("M = ", f1(mean(x)), " seats, 95% CI ", ci(bci[1], bci[2], f1)),
              paste0("t(", a1$parameter, ") = ", f2(a1$statistic), ", d = ", f2(d1)),
              fp(a1$p.value))

} else {

  # WHAT: the non-parametric alternative: Wilcoxon signed-rank test against 30.
  # HOW:  wilcox.test(x, mu = 30); conf.int = TRUE also gives a CI.
  a1 <- wilcox.test(x, mu = 30, conf.int = TRUE)

  # WHAT: its results table: n, median, CI, test statistic V and p.
  # HOW:  data.frame() with the formatting helpers.
  A1 <- data.frame(n = n, Median = f1(median(x)),
                   `95% CI (pseudo-median)` = ci(a1$conf.int[1], a1$conf.int[2], f1),
                   V = f2(a1$statistic), p = fp(a1$p.value), check.names = FALSE)

  # WHAT: its summary line.
  # HOW:  add_summary().
  add_summary("A1", "Seats inside vs 30", "Wilcoxon signed-rank",
              paste0("Mdn = ", f1(median(x))), paste0("V = ", f2(a1$statistic)),
              fp(a1$p.value))
}

# WHAT: shows the table in the Console and saves it as A1_table.tex.
# HOW:  print() and our save_tab() helper.
print(A1)
save_tab(A1, "A1")

# WHAT: the A1 figure: every cafe as a dot, the mean with its bootstrap CI in
#       blue, and the benchmark of 30 as a red dashed line.
# HOW:  ggplot() in layers:
#       geom_jitter() = the dots, spread sideways so they do not overlap;
#       geom_hline() = a horizontal line at 30;
#       annotate("pointrange") = one point (the mean) with a vertical bar (the CI);
#       coord_cartesian(ylim) zooms in to 0-80 seats without dropping data;
#       labs() = axis labels and a subtitle.
p <- ggplot(data.frame(x = x), aes(x = "", y = x)) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1.3) +
  geom_hline(yintercept = 30, linetype = "dashed", colour = "firebrick") +
  annotate("pointrange", x = 1, y = mean(x), ymin = bci[1], ymax = bci[2],
           colour = "steelblue", size = 0.8, linewidth = 1.2) +
  coord_cartesian(ylim = c(0, 80)) +
  labs(x = NULL, y = "Seats inside",
       subtitle = "Blue: mean with bootstrap 95% CI; red dashed: benchmark of 30 (y-axis cut at 80)")

# WHAT: saves it as A1_figure.pdf.
# HOW:  our save_fig() helper.
save_fig(p, "A1", w = 6)


# =============================================================================
# A2  Two groups: Do cafes WITH outdoor seating have MORE seats inside?
#   H0: mean(outdoor) = mean(no outdoor)
#   H1: mean(outdoor) > mean(no outdoor)   (one-tailed: the question says "more")
#   Difference = outdoor minus no outdoor.
# =============================================================================

# WHAT: the cafes where both seats inside and outdoor seating are known.
# HOW:  filter() keeps the rows where both are not missing; %>% passes df into it.
d2 <- df %>% filter(!is.na(seats_inside), !is.na(has_outdoor))

# WHAT: seats inside of the cafes WITH outdoor seating.
# HOW:  has_outdoor is TRUE/FALSE, so it can pick rows directly.
x_out <- d2$seats_inside[d2$has_outdoor]

# WHAT: seats inside of the cafes WITHOUT outdoor seating.
# HOW:  '!' turns TRUE into FALSE and back.
x_no <- d2$seats_inside[!d2$has_outdoor]

# WHAT: prints n and the number of cafes lost.
# HOW:  nrow() counts rows.
cat("\nA2: n =", nrow(d2), "| lost:", nrow(df) - nrow(d2), "\n")

# WHAT: assumption check: do the two groups have equal variances? (Levene's test)
# HOW:  leveneTest(outcome ~ group); factor() treats TRUE/FALSE as two groups.
#       We use Welch's t-test anyway, which does not assume equal variances.
print(leveneTest(seats_inside ~ factor(has_outdoor), data = d2))

# WHAT: picks the method: both groups must be "Parametric" in the decision table.
# HOW:  is_param() with the two group names.
if (is_param("Seats inside", c("Outdoor seating", "No outdoor seating"))) {

  # WHAT: Welch's t-test, one-tailed (outdoor > no outdoor).
  # HOW:  t.test(first group, second group); R subtracts second from first, so
  #       alternative = "greater" tests "outdoor is greater". Welch is R's default.
  a2_one <- t.test(x_out, x_no, alternative = "greater")

  # WHAT: the same test two-tailed, only to get the usual two-sided 95% CI,
  #       which the brief asks us to report even for one-tailed tests.
  # HOW:  t.test() with the default alternative ("two.sided").
  a2_two <- t.test(x_out, x_no)

  # WHAT: the pooled standard deviation of the two groups (needed for Cohen's d).
  # HOW:  the textbook formula: each group's variance (var()) weighted by its
  #       size minus 1, divided by the total size minus 2, then the square root.
  sp <- sqrt(((length(x_out) - 1) * var(x_out) + (length(x_no) - 1) * var(x_no)) /
               (length(x_out) + length(x_no) - 2))

  # WHAT: Cohen's d = the difference in means in standard-deviation units.
  # HOW:  (mean outdoor - mean no outdoor) / pooled SD.
  d_2 <- (mean(x_out) - mean(x_no)) / sp

  # WHAT: the difference in means, in seats.
  # HOW:  subtraction.
  diff2 <- mean(x_out) - mean(x_no)

  # WHAT: the results table: one row per group plus a row for the difference.
  # HOW:  data.frame() with 3 rows; t.test(x_out)$conf.int gives each group's
  #       own CI; "" leaves a cell empty.
  A2 <- data.frame(Group = c("Outdoor seating", "No outdoor seating", "Difference (outdoor - none)"),
                   n = c(length(x_out), length(x_no), ""),
                   Mean = f1(c(mean(x_out), mean(x_no), diff2)),
                   `95% CI` = c(ci(t.test(x_out)$conf.int[1], t.test(x_out)$conf.int[2], f1),
                                ci(t.test(x_no)$conf.int[1],  t.test(x_no)$conf.int[2],  f1),
                                ci(a2_two$conf.int[1], a2_two$conf.int[2], f1)),
                   d = c("", "", f2(d_2)),
                   `Welch t` = c("", "", paste0("t(", f2(a2_one$parameter), ") = ", f2(a2_one$statistic))),
                   `p (one-tailed)` = c("", "", fp(a2_one$p.value)), check.names = FALSE)

  # WHAT: the A2 summary line.
  # HOW:  add_summary().
  add_summary("A2", "Seats inside: outdoor vs none", "Welch t-test (one-tailed)",
              paste0("Diff = ", f1(diff2), " seats, 95% CI ", ci(a2_two$conf.int[1], a2_two$conf.int[2], f1)),
              paste0("t(", f2(a2_one$parameter), ") = ", f2(a2_one$statistic), ", d = ", f2(d_2)),
              fp(a2_one$p.value))

} else {

  # WHAT: the non-parametric alternative: Wilcoxon rank-sum test, one-tailed.
  # HOW:  wilcox.test() with the same group order and alternative = "greater".
  a2_one <- wilcox.test(x_out, x_no, alternative = "greater", conf.int = TRUE)

  # WHAT: its results table (W and p).
  # HOW:  data.frame().
  A2 <- data.frame(W = f2(a2_one$statistic), p = fp(a2_one$p.value))

  # WHAT: its summary line.
  # HOW:  add_summary().
  add_summary("A2", "Seats inside: outdoor vs none", "Wilcoxon rank-sum (one-tailed)",
              "", paste0("W = ", a2_one$statistic), fp(a2_one$p.value))
}

# WHAT: shows and saves the table.
# HOW:  print() and save_tab().
print(A2)
save_tab(A2, "A2")

# WHAT: each group's mean and 95% CI, for the figure.
# HOW:  mutate() adds a readable group label; group_by() splits by group;
#       summarise() computes the mean and the t-based CI of each group.
m2 <- d2 %>% mutate(grp = ifelse(has_outdoor, "Outdoor seating", "No outdoor seating")) %>%
  group_by(grp) %>% summarise(m = mean(seats_inside), lo = t.test(seats_inside)$conf.int[1],
                              hi = t.test(seats_inside)$conf.int[2])

# WHAT: the A2 figure: dots for every cafe in the two groups, plus each group's
#       mean with its CI in blue.
# HOW:  geom_jitter() = dots; geom_pointrange() = mean + CI bar from the m2
#       table; coord_cartesian() zooms to 0-100 seats.
p <- ggplot(d2, aes(x = ifelse(has_outdoor, "Outdoor seating", "No outdoor seating"), y = seats_inside)) +
  geom_jitter(width = 0.15, alpha = 0.35, size = 1.2) +
  geom_pointrange(data = m2, aes(x = grp, y = m, ymin = lo, ymax = hi),
                  colour = "steelblue", size = 0.7, linewidth = 1.2) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = NULL, y = "Seats inside", subtitle = "Blue: group mean with 95% CI (y-axis cut at 100)")

# WHAT: saves it as A2_figure.pdf.
# HOW:  save_fig().
save_fig(p, "A2", w = 5)


# =============================================================================
# A3  Paired: How many MORE seats does a cafe have inside than outside?
#   Difference = inside minus outside, within the same cafe.
#   H0: mean difference = 0   H1: mean difference > 0 (one-tailed, fixed in advance)
# =============================================================================

# WHAT: the cafes where both seat counts are known (a pair needs both).
# HOW:  filter() on both columns.
d3 <- df %>% filter(!is.na(seats_inside), !is.na(seats_outside))

# WHAT: the difference for each cafe.
# HOW:  column minus column, cafe by cafe.
diff3 <- d3$seats_inside - d3$seats_outside

# WHAT: prints n and the number of cafes lost.
# HOW:  cat().
cat("\nA3: n =", nrow(d3), "| lost:", nrow(df) - nrow(d3), "\n")

# WHAT: picks the method from the decision table (for paired data it is the
#       DIFFERENCE that has to qualify).
# HOW:  is_param("Inside minus outside").
if (is_param("Inside minus outside")) {

  # WHAT: paired t-test, one-tailed (inside > outside).
  # HOW:  t.test(inside, outside, paired = TRUE) tests the mean of the differences.
  a3_one <- t.test(d3$seats_inside, d3$seats_outside, paired = TRUE, alternative = "greater")

  # WHAT: the same test two-tailed, for the two-sided 95% CI.
  # HOW:  default alternative.
  a3_two <- t.test(d3$seats_inside, d3$seats_outside, paired = TRUE)

  # WHAT: Cohen's dz = mean difference / standard deviation of the differences
  #       (the effect size for paired data, as the brief defines it).
  # HOW:  mean() / sd().
  dz <- mean(diff3) / sd(diff3)

  # WHAT: the results table.
  # HOW:  data.frame() with the formatting helpers.
  A3 <- data.frame(n = nrow(d3), `Mean inside` = f1(mean(d3$seats_inside)),
                   `Mean outside` = f1(mean(d3$seats_outside)),
                   `Mean difference` = f1(mean(diff3)),
                   `95% CI` = ci(a3_two$conf.int[1], a3_two$conf.int[2], f1),
                   dz = f2(dz), t = paste0("t(", a3_one$parameter, ") = ", f2(a3_one$statistic)),
                   `p (one-tailed)` = fp(a3_one$p.value), check.names = FALSE)

  # WHAT: the A3 summary line.
  # HOW:  add_summary().
  add_summary("A3", "Seats inside minus outside", "Paired t-test (one-tailed)",
              paste0("Diff = ", f1(mean(diff3)), " seats, 95% CI ", ci(a3_two$conf.int[1], a3_two$conf.int[2], f1)),
              paste0("t(", a3_one$parameter, ") = ", f2(a3_one$statistic), ", dz = ", f2(dz)),
              fp(a3_one$p.value))

} else {

  # WHAT: the non-parametric alternative: Wilcoxon signed-rank test, paired.
  # HOW:  wilcox.test(..., paired = TRUE, alternative = "greater").
  a3_one <- wilcox.test(d3$seats_inside, d3$seats_outside, paired = TRUE, alternative = "greater")

  # WHAT: its results table.
  # HOW:  data.frame().
  A3 <- data.frame(n = nrow(d3), `Median diff` = f1(median(diff3)), V = a3_one$statistic,
                   p = fp(a3_one$p.value), check.names = FALSE)

  # WHAT: its summary line.
  # HOW:  add_summary().
  add_summary("A3", "Seats inside minus outside", "Wilcoxon signed-rank",
              paste0("Mdn diff = ", f1(median(diff3))), paste0("V = ", a3_one$statistic),
              fp(a3_one$p.value))
}

# WHAT: shows and saves the table.
# HOW:  print() and save_tab().
print(A3)
save_tab(A3, "A3")

# WHAT: the A3 figure: a histogram of the differences, a red line at 0 (no
#       difference), and the mean difference with its CI in blue under the bars.
# HOW:  geom_histogram() with bars 5 seats wide; geom_vline() = vertical line;
#       annotate("errorbarh") = horizontal CI bar; annotate("point") = the mean;
#       coord_cartesian(xlim) zooms to -60..120.
p <- ggplot(data.frame(d = diff3), aes(x = d)) +
  geom_histogram(binwidth = 5, fill = "grey75", colour = "white") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "firebrick") +
  annotate("errorbarh", y = -2, xmin = a3_two$conf.int[1], xmax = a3_two$conf.int[2],
           height = 3, colour = "steelblue", linewidth = 1.2) +
  annotate("point", x = mean(diff3), y = -2, colour = "steelblue", size = 3) +
  coord_cartesian(xlim = c(-60, 120)) +
  labs(x = "Seats inside minus seats outside (same cafe)", y = "Number of cafes",
       subtitle = "Blue: mean difference with 95% CI; red dashed: no difference (x-axis cut at 120)")

# WHAT: saves it as A3_figure.pdf.
# HOW:  save_fig().
save_fig(p, "A3")


# =============================================================================
# A4  More groups: Does the number of OUTDOOR seats differ between districts
#     (Prague 1, 2, 3, 5, 7, 8)?
#   H0: outdoor seats are distributed the same way in all six districts
#   H1: at least one district differs (two-tailed)
# =============================================================================

# WHAT: cafes in the six districts with a known number of outdoor seats, with
#       the districts in a fixed order (Prague 1 first).
# HOW:  filter() drops the missing ones; factor(levels = ...) sets the order.
d4 <- df %>% filter(!is.na(district_A4), !is.na(seats_outside)) %>%
  mutate(district_A4 = factor(district_A4, levels = paste("Prague", c(1, 2, 3, 5, 7, 8))))

# WHAT: prints n and how many cafes are left out (other districts or missing).
# HOW:  cat().
cat("\nA4: n =", nrow(d4), "| outside the six districts or NA:", nrow(df) - nrow(d4), "\n")

# WHAT: the six district names, used to look up the decision table.
# HOW:  levels() returns the categories of a factor.
groups4 <- levels(d4$district_A4)

# WHAT: n, mean, 95% CI and median of outdoor seats in each district.
# HOW:  group_by() splits by district; summarise() computes per district;
#       n() counts rows; t.test()$conf.int gives the CI.
grp_stats <- d4 %>% group_by(district_A4) %>%
  summarise(n = n(), Mean = mean(seats_outside), lo = t.test(seats_outside)$conf.int[1],
            hi = t.test(seats_outside)$conf.int[2], Median = median(seats_outside))

# WHAT: shows them.
# HOW:  print().
print(grp_stats)

# WHAT: picks the method: ALL six districts must be "Parametric".
#       (Prague 2, 3 and 5 have fewer than 30 cafes and are skewed, so in our
#       data this is FALSE and the Kruskal-Wallis branch runs.)
# HOW:  is_param() with the six district names.
if (is_param("Seats outside", groups4)) {

  # WHAT: assumption check: equal variances across districts?
  # HOW:  leveneTest().
  print(leveneTest(seats_outside ~ district_A4, data = d4))

  # WHAT: Welch's ANOVA (does not assume equal variances).
  # HOW:  oneway.test(outcome ~ group, var.equal = FALSE).
  a4 <- oneway.test(seats_outside ~ district_A4, data = d4, var.equal = FALSE)

  # WHAT: the test statistic written as F(df1, df2) = value.
  # HOW:  a4$parameter holds the two degrees of freedom; paste0() builds the text.
  stat4 <- paste0("F(", f2(a4$parameter[1]), ", ", f2(a4$parameter[2]), ") = ", f2(a4$statistic))

  # WHAT: pairwise comparisons that suit Welch's ANOVA: Games-Howell.
  # HOW:  games_howell_test() from rstatix.
  post4 <- games_howell_test(d4, seats_outside ~ district_A4)

  # WHAT: the post hoc table: each pair, mean difference, CI, adjusted p.
  # HOW:  data.frame() from the columns games_howell_test() returns.
  post_tab <- data.frame(Pair = paste(post4$group2, "-", post4$group1),
                         `Mean diff` = f1(post4$estimate), `95% CI` = ci(post4$conf.low, post4$conf.high, f1),
                         `p (adj.)` = fp(post4$p.adj), check.names = FALSE)

  # WHAT: the method's name for the tables.
  # HOW:  plain text.
  method4 <- "Welch ANOVA + Games-Howell"

} else {

  # WHAT: the non-parametric test for several groups: Kruskal-Wallis.
  # HOW:  kruskal.test(outcome ~ group). It compares the ranks of the values.
  a4 <- kruskal.test(seats_outside ~ district_A4, data = d4)

  # WHAT: the test statistic written as H(df) = value.
  # HOW:  paste0().
  stat4 <- paste0("H(", a4$parameter, ") = ", f2(a4$statistic))

  # WHAT: pairwise comparisons for Kruskal-Wallis: Dunn's test, with Holm's
  #       adjustment for testing 15 pairs at once.
  # HOW:  dunn_test() from rstatix, p.adjust.method = "holm".
  post4 <- dunn_test(d4, seats_outside ~ district_A4, p.adjust.method = "holm")

  # WHAT: the mean difference of each pair with a bootstrap 95% CI (Dunn's test
  #       gives no CI, but the brief asks for one per pair).
  # HOW:  for each pair i (sapply over the row numbers): take the outdoor seats
  #       of both districts (a and b); 5,000 times resample both and take the
  #       difference in means; keep the real difference and the 2.5% and 97.5%
  #       points. t() turns the result so each pair is one row.
  pair_ci <- t(sapply(seq_len(nrow(post4)), function(i) {
    a <- d4$seats_outside[d4$district_A4 == post4$group2[i]]
    b <- d4$seats_outside[d4$district_A4 == post4$group1[i]]
    set.seed(SEED)
    bd <- replicate(N_BOOT, mean(sample(a, replace = TRUE)) - mean(sample(b, replace = TRUE)))
    c(mean(a) - mean(b), quantile(bd, c(.025, .975)))
  }))

  # WHAT: the post hoc table: pair, mean difference, bootstrap CI, z, Holm p.
  # HOW:  data.frame() with the formatting helpers.
  post_tab <- data.frame(Pair = paste(post4$group2, "minus", post4$group1),
                         `Mean diff` = f1(pair_ci[, 1]),
                         `Bootstrap 95% CI` = ci(pair_ci[, 2], pair_ci[, 3], f1),
                         z = f2(post4$statistic), `p (Holm)` = fp(post4$p.adj), check.names = FALSE)

  # WHAT: the method's name for the tables.
  # HOW:  plain text.
  method4 <- "Kruskal-Wallis + Dunn (Holm)"
}

# WHAT: shows the overall test and the pairwise table.
# HOW:  print().
print(a4)
print(post_tab)

# WHAT: the main A4 table: one row per district (n, mean, CI, median).
# HOW:  data.frame() from grp_stats.
A4 <- data.frame(District = grp_stats$district_A4, n = grp_stats$n, Mean = f1(grp_stats$Mean),
                 `95% CI` = ci(grp_stats$lo, grp_stats$hi, f1), Median = f1(grp_stats$Median),
                 check.names = FALSE)

# WHAT: adds a last row with the overall test result.
# HOW:  rbind() stacks one more row under the table.
A4 <- rbind(A4, data.frame(District = method4, n = "", Mean = "", `95% CI` = stat4,
                           Median = paste("p =", fp(a4$p.value)), check.names = FALSE))

# WHAT: saves the main table and the post hoc table (the latter goes to the appendix).
# HOW:  save_tab() twice.
save_tab(A4, "A4")
save_tab(post_tab, "A4_posthoc")

# WHAT: the A4 summary line (the range of district means).
# HOW:  min() and max() of the district means.
add_summary("A4", "Outdoor seats by district", method4,
            paste0("Means ", f1(min(grp_stats$Mean)), "-", f1(max(grp_stats$Mean)), " seats"),
            stat4, fp(a4$p.value))

# WHAT: the A4 figure: dots for every cafe per district, plus each district's
#       mean with its CI in blue.
# HOW:  geom_jitter() + geom_pointrange() from grp_stats; zoom to 0-70 seats.
p <- ggplot(d4, aes(x = district_A4, y = seats_outside)) +
  geom_jitter(width = 0.15, alpha = 0.35, size = 1.2) +
  geom_pointrange(data = grp_stats, aes(y = Mean, ymin = lo, ymax = hi),
                  colour = "steelblue", size = 0.6, linewidth = 1.1) +
  coord_cartesian(ylim = c(0, 70)) +
  labs(x = NULL, y = "Seats outside", subtitle = "Blue: district mean with 95% CI (y-axis cut at 70)")

# WHAT: saves it as A4_figure.pdf.
# HOW:  save_fig().
save_fig(p, "A4")


# =============================================================================
# A5  Two categorical: Is a SMALL cafe (15 or fewer seats inside) MORE likely to
#     be top-rated (4.7 or higher) than other cafes? By how many percentage points?
#   H0: P(top | small) = P(top | other)   H1: P(top | small) > P(top | other)
#   Difference = small minus other, in percentage points (pp).
# =============================================================================

# WHAT: cafes where both yes/no variables are known.
# HOW:  filter().
d5 <- df %>% filter(!is.na(small_cafe), !is.na(top_rated))

# WHAT: prints n and the number of cafes lost.
# HOW:  cat().
cat("\nA5: n =", nrow(d5), "| lost:", nrow(df) - nrow(d5), "\n")

# WHAT: the 2 x 2 table of counts: small/other by top-rated/not.
# HOW:  table() counts combinations; factor(x, levels, labels) puts TRUE first
#       and gives readable labels.
tab5 <- table(Small = factor(d5$small_cafe, c(TRUE, FALSE), c("Small (<= 15)", "Other")),
              Top = factor(d5$top_rated, c(TRUE, FALSE), c("Top-rated", "Not top-rated")))

# WHAT: shows the counts, and the row percentages (% top-rated within each group).
# HOW:  prop.table(tab5, 1) divides each row by its total; x 100; round to 1 decimal.
print(tab5)
print(round(100 * prop.table(tab5, 1), 1))

# WHAT: the chi-square test of independence.
# HOW:  chisq.test(); correct = FALSE switches off Yates' continuity correction,
#       so the statistic matches the standard formula.
chi5 <- chisq.test(tab5, correct = FALSE)

# WHAT: assumption check: the expected counts (all should be 5 or more).
# HOW:  chi5$expected holds them; min() finds the smallest.
print(chi5$expected)
cat("Smallest expected count:", min(chi5$expected), "\n")

# WHAT: the standardised residuals: cells above 2 (in absolute value) are the
#       ones driving the association.
# HOW:  chi5$stdres, rounded to 2 decimals.
print(round(chi5$stdres, 2))

# WHAT: Cramer's V, the effect size for a contingency table.
# HOW:  for a 2 x 2 table it is sqrt(chi-square / N); sum(tab5) = N.
cramer_v <- sqrt(chi5$statistic / sum(tab5))

# WHAT: the one-tailed test of "small cafes have a higher share of top-rated".
# HOW:  prop.test(successes, totals): tab5[, 1] = top-rated counts per group,
#       rowSums() = group sizes; alternative = "greater" (first group = small).
pt5_one <- prop.test(tab5[, 1], rowSums(tab5), alternative = "greater", correct = FALSE)

# WHAT: the same two-tailed, for the two-sided 95% CI of the difference.
# HOW:  prop.test() with the default alternative.
pt5_two <- prop.test(tab5[, 1], rowSums(tab5), correct = FALSE)

# WHAT: the difference in percentage points (small minus other).
# HOW:  $estimate holds the two proportions; their difference x 100.
pp_diff <- 100 * (pt5_two$estimate[1] - pt5_two$estimate[2])

# WHAT: picks the test: Fisher's exact test if any expected count is below 5,
#       otherwise the chi-square result.
# HOW:  if / else on min(chi5$expected).
if (min(chi5$expected) < 5) {

  # WHAT: Fisher's exact test, one-tailed.
  # HOW:  fisher.test(..., alternative = "greater").
  fi5 <- fisher.test(tab5, alternative = "greater")

  # WHAT: keeps its p-value and the method name.
  # HOW:  $p.value; plain text.
  p5 <- fi5$p.value
  test5 <- "Fisher's exact"
  stat5 <- "Fisher's exact test"

} else {

  # WHAT: keeps the one-tailed p-value and the method name.
  # HOW:  $p.value; plain text.
  p5 <- pt5_one$p.value
  test5 <- "Chi-square (one-tailed)"

  # WHAT: the statistic written as chi2(df, N) = value.
  # HOW:  paste0().
  stat5 <- paste0("chi2(1, N = ", sum(tab5), ") = ", f2(chi5$statistic))
}

# WHAT: the A5 table: counts and % top-rated per group, plus a row with the
#       difference, its CI, the test, Cramer's V and p.
# HOW:  data.frame() with 3 rows; "" leaves cells empty.
A5 <- data.frame(Group = c("Small (<= 15 seats)", "Other", "Difference (pp)"),
                 `Top-rated` = c(tab5[, 1], ""), `Not top-rated` = c(tab5[, 2], ""),
                 `% top-rated` = c(f1(100 * prop.table(tab5, 1)[, 1]), f1(pp_diff)),
                 `95% CI (pp)` = c("", "", ci(100 * pt5_two$conf.int[1], 100 * pt5_two$conf.int[2], f1)),
                 Test = c("", "", stat5), `Cramer's V` = c("", "", f2(cramer_v)),
                 `p (one-tailed)` = c("", "", fp(p5)), check.names = FALSE)

# WHAT: shows and saves it (in a smaller font, because it is wide).
# HOW:  print() and save_tab(size = "\\small").
print(A5)
save_tab(A5, "A5", size = "\\small")

# WHAT: the A5 summary line.
# HOW:  add_summary().
add_summary("A5", "Small cafe -> top-rated", test5,
            paste0(f1(pp_diff), " pp, 95% CI ", ci(100 * pt5_two$conf.int[1], 100 * pt5_two$conf.int[2], f1)),
            paste0(stat5, ", V = ", f2(cramer_v)), fp(p5))

# WHAT: a small table for the figure: % top-rated and n in each group.
# HOW:  data.frame().
pr5 <- data.frame(grp = c("Small (<= 15 seats)", "Other"),
                  p = 100 * prop.table(tab5, 1)[, 1], n = rowSums(tab5))

# WHAT: the lower and upper 95% CI of each group's percentage.
# HOW:  sapply() runs prop.test() once per group (i = 1, 2) and takes $conf.int.
pr5$lo <- sapply(1:2, function(i) 100 * prop.test(tab5[i, 1], pr5$n[i])$conf.int[1])
pr5$hi <- sapply(1:2, function(i) 100 * prop.test(tab5[i, 1], pr5$n[i])$conf.int[2])

# WHAT: keeps "Small" as the first bar in the figure.
# HOW:  factor() with the current order as levels.
pr5$grp <- factor(pr5$grp, levels = pr5$grp)

# WHAT: the A5 figure: a bar per group (% top-rated) with its 95% CI, and n.
# HOW:  geom_col() = bars; geom_errorbar() = CI; geom_text() = the n labels.
p <- ggplot(pr5, aes(x = grp, y = p)) +
  geom_col(fill = "grey80", width = 0.6) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.15, colour = "steelblue", linewidth = 1) +
  geom_text(aes(label = paste0("n = ", n)), y = 3, size = 3.3) +
  labs(x = NULL, y = "% of cafes rated 4.7 or higher", subtitle = "Bars: share top-rated, with 95% CI")

# WHAT: saves it as A5_figure.pdf.
# HOW:  save_fig().
save_fig(p, "A5", w = 4.5)


# =============================================================================
# A6  Correlation: Is the Price Level correlated with the number of seats inside?
#   H0: correlation = 0   H1: correlation != 0 (two-tailed)
#   Method: Pearson's r needs both variables roughly normal. The central limit
#   theorem is about MEANS and does not help here. Seats inside is far from
#   normal (skewness 4.4), so Spearman's rho with a bootstrap CI is used.
# =============================================================================

# WHAT: cafes where both variables are known.
# HOW:  filter().
d6 <- df %>% filter(!is.na(price_level), !is.na(seats_inside))

# WHAT: prints n and the number of cafes lost.
# HOW:  cat().
cat("\nA6: n =", nrow(d6), "| lost:", nrow(df) - nrow(d6), "\n")

# WHAT: are BOTH variables normal according to the decision table?
# HOW:  picks the "All cafes" rows of both variables from 'dec'; all() is TRUE
#       only if both say Normal = "Yes".
both_normal <- all(dec$Normal[dec$Variable %in% c("Seats inside", "Price Level (CZK)") &
                                dec$Group == "All cafes"] == "Yes")

# WHAT: picks Pearson (both normal) or Spearman (otherwise).
# HOW:  if / else.
if (both_normal) {

  # WHAT: Pearson's correlation test.
  # HOW:  cor.test(x, y, method = "pearson").
  a6 <- cor.test(d6$seats_inside, d6$price_level, method = "pearson")

  # WHAT: keeps the coefficient, its CI and the method name.
  # HOW:  $estimate and $conf.int.
  r6  <- a6$estimate
  lo6 <- a6$conf.int[1]
  hi6 <- a6$conf.int[2]
  m6  <- "Pearson's r"

} else {

  # WHAT: Spearman's rank correlation test.
  # HOW:  cor.test(..., method = "spearman"); exact = FALSE because there are
  #       ties (cafes with the same value).
  a6 <- cor.test(d6$seats_inside, d6$price_level, method = "spearman", exact = FALSE)

  # WHAT: keeps the coefficient (rho).
  # HOW:  $estimate.
  r6 <- a6$estimate

  # WHAT: fixes the random numbers for the bootstrap.
  # HOW:  set.seed().
  set.seed(SEED)

  # WHAT: bootstrap of rho: 5,000 times, resample whole cafes (keeping each
  #       cafe's seats and price together) and recompute rho.
  # HOW:  sample(nrow(d6), replace = TRUE) draws row numbers; cor(...,
  #       method = "spearman") computes rho on those rows; replicate() repeats.
  bs <- replicate(N_BOOT, {
    i <- sample(nrow(d6), replace = TRUE)
    cor(d6$seats_inside[i], d6$price_level[i], method = "spearman")
  })

  # WHAT: the bootstrap 95% CI and the method name.
  # HOW:  quantile() at 2.5% and 97.5%.
  lo6 <- quantile(bs, .025)
  hi6 <- quantile(bs, .975)
  m6  <- "Spearman's rho"
}

# WHAT: the A6 table: method, n, coefficient, CI, shared variance and p.
# HOW:  data.frame(); shared variance = coefficient squared x 100 (%).
A6 <- data.frame(Method = m6, n = nrow(d6), Coefficient = f2(r6),
                 `95% CI` = ci(lo6, hi6), `Shared variance (%)` = f1(100 * r6^2),
                 p = fp(a6$p.value), check.names = FALSE)

# WHAT: shows and saves it.
# HOW:  print() and save_tab().
print(A6)
save_tab(A6, "A6")

# WHAT: the A6 summary line.
# HOW:  add_summary().
add_summary("A6", "Price Level ~ seats inside", m6,
            paste0(f2(r6), ", 95% CI ", ci(lo6, hi6)), paste0("n = ", nrow(d6)), fp(a6$p.value))

# WHAT: the A6 figure: scatterplot of seats inside against Price Level, with
#       the coefficient and its CI in the subtitle.
# HOW:  geom_point() = one dot per cafe (alpha = half transparent).
p <- ggplot(d6, aes(seats_inside, price_level)) +
  geom_point(alpha = 0.5) +
  labs(x = "Seats inside", y = "Price Level (CZK)",
       subtitle = paste0(m6, " = ", f2(r6), ", 95% CI ", ci(lo6, hi6), ", n = ", nrow(d6)))

# WHAT: saves it as A6_figure.pdf.
# HOW:  save_fig().
save_fig(p, "A6")


# =============================================================================
# A7  Simple regression: By how many CZK does the Price Level change per
#     10 additional seats inside?
#   H0: slope = 0   H1: slope != 0
# =============================================================================

# WHAT: cafes with both a Price Level and a seat count.
# HOW:  filter().
d7 <- df %>% filter(!is.na(price_level), !is.na(seats10))

# WHAT: the linear regression: Price Level explained by seats (per 10).
# HOW:  lm(outcome ~ predictor) fits a straight line by least squares (OLS).
m7 <- lm(price_level ~ seats10, data = d7)

# WHAT: checks the assumptions (plots + Breusch-Pagan) and remembers whether
#       robust standard errors are needed.
# HOW:  our diagnostics() helper; TRUE = use robust SEs.
rob7 <- diagnostics(m7, "A7")

# WHAT: lists the influential cafes.
# HOW:  our influential() helper.
print(influential(m7, d7))

# WHAT: the regression table (B, SE, CI, t, p).
# HOW:  our reg_table() helper.
A7 <- reg_table(m7, rob7)

# WHAT: readable names for the two terms.
# HOW:  replaces the Term column.
A7$Term <- c("Intercept", "Seats inside (per 10)")

# WHAT: adds a last row with n and R-squared (and a note if robust SEs were used).
# HOW:  nobs() = number of cafes used; summary(m7)$r.squared = R-squared;
#       rbind() adds the row.
A7 <- rbind(A7, data.frame(Term = paste0("n = ", nobs(m7), ", R2 = ", f2(summary(m7)$r.squared),
                                         ifelse(rob7, ", robust (HC3) SE", "")),
                           B = "", SE = "", `95% CI` = "", t = "", p = "", check.names = FALSE))

# WHAT: shows and saves it.
# HOW:  print() and save_tab().
print(A7)
save_tab(A7, "A7")

# WHAT: the A7 summary line (the slope is in row 2 of the table).
# HOW:  A7$B[2] etc. take row 2 of each column.
add_summary("A7", "Price Level per 10 seats", paste0("OLS", ifelse(rob7, " (HC3 SE)", "")),
            paste0(A7$B[2], " CZK, 95% CI ", A7$`95% CI`[2]),
            paste0("t = ", A7$t[2], ", R2 = ", f2(summary(m7)$r.squared)), A7$p[2])

# WHAT: the A7 figure: scatterplot with the fitted line and its 95% band.
# HOW:  geom_point() = dots; geom_smooth(method = "lm") = the OLS line with its
#       confidence band.
p <- ggplot(d7, aes(seats_inside, price_level)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "steelblue") +
  labs(x = "Seats inside", y = "Price Level (CZK)", subtitle = "OLS line with 95% confidence band")

# WHAT: saves it as A7_figure.pdf.
# HOW:  save_fig().
save_fig(p, "A7")


# =============================================================================
# A8  Multiple regression: Does the seats-Price Level link hold once we account
#     for distance from the Orloj? How much of it remains?
#   H0: slope of seats (holding distance constant) = 0   H1: != 0
# =============================================================================

# WHAT: cafes with Price Level, seats and distance all known (the same sample
#       for both models, so they can be compared fairly).
# HOW:  filter() on all three.
d8 <- df %>% filter(!is.na(price_level), !is.na(seats10), !is.na(dist_orloj))

# WHAT: the model WITHOUT distance, on this same sample (the "before").
# HOW:  lm().
m8a <- lm(price_level ~ seats10, data = d8)

# WHAT: the model WITH distance (the "after").
# HOW:  lm(outcome ~ predictor1 + predictor2).
m8 <- lm(price_level ~ seats10 + dist_orloj, data = d8)

# WHAT: assumption checks; robust SEs if needed.
# HOW:  diagnostics().
rob8 <- diagnostics(m8, "A8")

# WHAT: influential cafes.
# HOW:  influential().
print(influential(m8, d8))

# WHAT: checks that the two predictors are not too strongly related to each
#       other (VIF near 1 = fine; above about 5 = a problem).
# HOW:  vif() from the car package.
print(vif(m8))

# WHAT: the regression table with readable term names.
# HOW:  reg_table(), then replace the Term column.
A8 <- reg_table(m8, rob8)
A8$Term <- c("Intercept", "Seats inside (per 10)", "Distance from Orloj (km)")

# WHAT: the slope of seats before and after adding distance.
# HOW:  coef() returns the coefficients; ["seats10"] picks the seats one.
b_before <- coef(m8a)["seats10"]
b_after  <- coef(m8)["seats10"]

# WHAT: adds a row with n, R-squared, the slope before, and how much remains (%).
# HOW:  rbind(); % remains = after / before x 100.
A8 <- rbind(A8, data.frame(Term = paste0("n = ", nobs(m8), ", R2 = ", f2(summary(m8)$r.squared),
                                         "; seats slope without distance = ", f2(b_before),
                                         " (", f1(100 * b_after / b_before), "% remains)"),
                           B = "", SE = "", `95% CI` = "", t = "", p = "", check.names = FALSE))

# WHAT: shows and saves it.
# HOW:  print() and save_tab().
print(A8)
save_tab(A8, "A8", size = "\\small")

# WHAT: the A8 summary line.
# HOW:  add_summary().
add_summary("A8", "Seats slope controlling distance", paste0("OLS", ifelse(rob8, " (HC3 SE)", "")),
            paste0(A8$B[2], " CZK, 95% CI ", A8$`95% CI`[2]),
            paste0("was ", f2(b_before), "; dist B = ", A8$B[3]), A8$p[2])

# WHAT: the 95% CI of the seats slope in each model (for the figure).
# HOW:  coefci() with robust or ordinary variance; ["seats10", ] picks that row.
ci_a <- coefci(m8a, vcov. = if (rob8) vcovHC(m8a, type = "HC3") else vcov(m8a))["seats10", ]
ci_b <- coefci(m8,  vcov. = if (rob8) vcovHC(m8,  type = "HC3") else vcov(m8))["seats10", ]

# WHAT: a small table with both slopes and their CIs.
# HOW:  data.frame().
cmp <- data.frame(model = c("Seats only", "Seats + distance"), b = c(b_before, b_after),
                  lo = c(ci_a[1], ci_b[1]), hi = c(ci_a[2], ci_b[2]))

# WHAT: the A8 figure: the seats slope with its CI, before vs after adding
#       distance, with a red line at 0 (no effect).
# HOW:  geom_hline() at 0; geom_pointrange() = slope + CI.
p <- ggplot(cmp, aes(x = model, y = b)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "firebrick") +
  geom_pointrange(aes(ymin = lo, ymax = hi), colour = "steelblue", linewidth = 1.2) +
  labs(x = NULL, y = "CZK change in Price Level per 10 seats inside",
       subtitle = "Slope of seats inside with 95% CI, before and after controlling for distance")

# WHAT: saves it as A8_figure.pdf.
# HOW:  save_fig().
save_fig(p, "A8", w = 5)


# =============================================================================
# A9  Interaction: Does the seats-Price Level relationship differ between cafes
#     WITH and WITHOUT outdoor seating?
#   H0: interaction coefficient = 0 (same slope)   H1: != 0
# =============================================================================

# WHAT: cafes with all three known, and a readable outdoor variable with
#       "No outdoor seating" as the reference group.
# HOW:  filter(); mutate() + factor(levels = ...) puts "No outdoor" first,
#       so lm() compares outdoor cafes against it.
d9 <- df %>% filter(!is.na(price_level), !is.na(seats10), !is.na(has_outdoor)) %>%
  mutate(outdoor = factor(ifelse(has_outdoor, "Outdoor seating", "No outdoor seating"),
                          levels = c("No outdoor seating", "Outdoor seating")))

# WHAT: the regression with an interaction: a separate slope for each group.
# HOW:  'seats10 * outdoor' = seats + outdoor + seats x outdoor; the last term
#       is the DIFFERENCE in slopes between the groups.
m9 <- lm(price_level ~ seats10 * outdoor, data = d9)

# WHAT: assumption checks; robust SEs if needed.
# HOW:  diagnostics().
rob9 <- diagnostics(m9, "A9")

# WHAT: influential cafes.
# HOW:  influential().
print(influential(m9, d9))

# WHAT: the regression table with readable term names.
# HOW:  reg_table(), then replace the Term column.
A9 <- reg_table(m9, rob9)
A9$Term <- c("Intercept (no outdoor)", "Seats inside (per 10), no outdoor",
             "Outdoor seating", "Seats x outdoor (difference in slope)")

# WHAT: adds a row with n and R-squared.
# HOW:  rbind().
A9 <- rbind(A9, data.frame(Term = paste0("n = ", nobs(m9), ", R2 = ", f2(summary(m9)$r.squared)),
                           B = "", SE = "", `95% CI` = "", t = "", p = "", check.names = FALSE))

# WHAT: shows and saves it.
# HOW:  print() and save_tab().
print(A9)
save_tab(A9, "A9", size = "\\small")

# WHAT: the A9 summary line (the interaction is row 4).
# HOW:  add_summary().
add_summary("A9", "Seats slope: outdoor vs none", paste0("OLS interaction", ifelse(rob9, " (HC3 SE)", "")),
            paste0("Slope diff ", A9$B[4], " CZK, 95% CI ", A9$`95% CI`[4]),
            paste0("t = ", A9$t[4]), A9$p[4])

# WHAT: the A9 figure: dots coloured by group, with a separate fitted line and
#       95% band for each group.
# HOW:  colour = outdoor in aes() splits by group; geom_smooth(method = "lm")
#       then fits one line per colour; scale_colour_manual() picks the colours.
p <- ggplot(d9, aes(seats_inside, price_level, colour = outdoor)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x) +
  scale_colour_manual(values = c("darkorange", "steelblue")) +
  labs(x = "Seats inside", y = "Price Level (CZK)", colour = NULL,
       subtitle = "OLS lines with 95% confidence bands by outdoor seating") +
  theme(legend.position = "bottom")

# WHAT: saves it as A9_figure.pdf.
# HOW:  save_fig().
save_fig(p, "A9")


# =============================================================================
# A10 Logistic regression: Does the probability of an ABOVE-MEDIAN Price Level
#     change with each additional 10 seats inside? By how many pp?
#   H0: slope = 0 (average marginal effect = 0)   H1: != 0
# =============================================================================

# WHAT: cafes with both variables known.
# HOW:  filter().
d10 <- df %>% filter(!is.na(above_median_price), !is.na(seats10))

# WHAT: the logistic regression: the chance of being above the median, explained
#       by seats (per 10).
# HOW:  glm(..., family = binomial) fits a logistic model for a yes/no outcome.
m10 <- glm(above_median_price ~ seats10, data = d10, family = binomial)

# WHAT: shows the model's coefficients (in log-odds).
# HOW:  summary().
print(summary(m10))

# WHAT: the average marginal effect (AME): by how much the PROBABILITY changes
#       per 10 seats, averaged over all cafes. This turns log-odds into
#       something readable.
# HOW:  avg_slopes() from the marginaleffects package.
ame <- avg_slopes(m10, variables = "seats10")

# WHAT: shows it.
# HOW:  print().
print(ame)

# WHAT: the coefficients table and their 95% CIs (Wald).
# HOW:  summary(m10)$coefficients; confint.default() = estimate +/- 1.96 x SE.
cf10 <- summary(m10)$coefficients
ci10 <- confint.default(m10)

# WHAT: the A10 table: coefficients, CIs, odds ratios, z and p, plus the AME in
#       percentage points.
# HOW:  data.frame(); exp() turns log-odds into odds ratios; x 100 turns the
#       AME into percentage points.
A10 <- data.frame(Term = c("Intercept", "Seats inside (per 10)", "AME: change in P(above median), pp"),
                  Estimate = c(f2(cf10[, 1]), f1(100 * ame$estimate)),
                  `95% CI` = c(ci(ci10[, 1], ci10[, 2]), ci(100 * ame$conf.low, 100 * ame$conf.high, f1)),
                  `Odds ratio` = c(f2(exp(cf10[, 1])), ""),
                  z = c(f2(cf10[, 3]), f2(ame$statistic)),
                  p = c(fp(cf10[, 4]), fp(ame$p.value)), check.names = FALSE)

# WHAT: adds a row with n.
# HOW:  rbind(); nobs() = number of cafes used.
A10 <- rbind(A10, data.frame(Term = paste0("n = ", nobs(m10)), Estimate = "", `95% CI` = "",
                             `Odds ratio` = "", z = "", p = "", check.names = FALSE))

# WHAT: shows and saves it.
# HOW:  print() and save_tab().
print(A10)
save_tab(A10, "A10", size = "\\small")

# WHAT: the A10 summary line.
# HOW:  add_summary().
add_summary("A10", "P(above-median price) per 10 seats", "Logistic regression, AME",
            paste0(f1(100 * ame$estimate), " pp, 95% CI ", ci(100 * ame$conf.low, 100 * ame$conf.high, f1)),
            paste0("z = ", f2(ame$statistic)), fp(ame$p.value))

# WHAT: 200 evenly spaced seat values from 0 to the largest cafe, to draw a
#       smooth prediction line.
# HOW:  seq(from, to, length.out = 200).
grid <- data.frame(seats10 = seq(0, max(d10$seats10), length.out = 200))

# WHAT: the model's prediction (in log-odds) and its standard error at each point.
# HOW:  predict(type = "link", se.fit = TRUE).
pr <- predict(m10, grid, type = "link", se.fit = TRUE)

# WHAT: turns the predictions and their 95% CI into probabilities.
# HOW:  plogis() converts log-odds to a probability between 0 and 1;
#       the CI is prediction +/- 1.96 x SE, converted the same way.
grid$p  <- plogis(pr$fit)
grid$lo <- plogis(pr$fit - 1.96 * pr$se.fit)
grid$hi <- plogis(pr$fit + 1.96 * pr$se.fit)

# WHAT: the A10 figure: each cafe as a dot at 1 (above median) or 0 (not), and
#       the predicted probability with its 95% band.
# HOW:  geom_jitter() (small vertical spread so dots do not overlap);
#       geom_ribbon() = the band; geom_line() = the predicted probability;
#       seats10 * 10 turns the x-axis back into seats.
p <- ggplot() +
  geom_jitter(data = d10, aes(seats_inside, as.numeric(above_median_price)),
              height = 0.03, width = 0, alpha = 0.4) +
  geom_ribbon(data = grid, aes(x = seats10 * 10, ymin = lo, ymax = hi), fill = "steelblue", alpha = 0.2) +
  geom_line(data = grid, aes(seats10 * 10, p), colour = "steelblue", linewidth = 1) +
  labs(x = "Seats inside", y = "P(Price Level above median)",
       subtitle = "Points: cafes (1 = above median); line: predicted probability with 95% CI")

# WHAT: saves it as A10_figure.pdf.
# HOW:  save_fig().
save_fig(p, "A10")


# =============================================================================
# Summary table of all ten analyses (for the main text of the report)
# =============================================================================

# WHAT: stacks the ten summary lines into one table.
# HOW:  do.call(rbind, list) stacks every element of the list.
S <- do.call(rbind, summary_rows)

# WHAT: shows it.
# HOW:  print().
print(S)

# WHAT: saves it as report/tables/S_summary_table.tex, in a small font.
# HOW:  xtable() + print(file = ...).
print(xtable(S), file = "report/tables/S_summary_table.tex", include.rownames = FALSE,
      booktabs = TRUE, floating = FALSE, size = "\\scriptsize")

# WHAT: a message that everything is done.
# HOW:  cat().
cat("\nAll analyses done. Tables in report/tables, figures in report/figures.\n")
