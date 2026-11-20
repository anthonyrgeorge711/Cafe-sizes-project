
library(xtable)
library(rstatix)

dir.create("report/tables",  recursive = TRUE, showWarnings = FALSE)
dir.create("report/figures", recursive = TRUE, showWarnings = FALSE)

# WHAT: a helper that writes a p-value the way the brief wants:
#       3 decimals, no leading zero (".031"), and "< .001" for tiny values.
# HOW:  sprintf("%.3f", p) gives 3 decimals ("0.031"); sub("^0", "", ...) deletes
#       the leading 0; ifelse() picks "< .001" when p is smaller than .001.
fp <- function(p) ifelse(p < .001, "< .001", sub("^0", "", sprintf("%.3f", p)))

# WHAT: a helper that saves a results table as report/tables/<name>_table.tex.
# HOW:  xtable() converts it; print(file = ) writes it; no row numbers;
#       booktabs = clean lines; floating = FALSE (caption is in report.tex).
save_tab <- function(tab, name) {
  print(xtable(tab), file = paste0("report/tables/", name, "_table.tex"),
        include.rownames = FALSE, booktabs = TRUE, floating = FALSE)
}



df <- read.csv("analysis/dataset.csv")


# A1: Is the average number of seats inside different from 30?
#   H0: the mean number of seats inside = 30
#   H1: the mean is different from 30 (two-tailed: the question says
#       "different", not "more" or "less")
#   Method: parametric (decision table: n = 193, CLT applies)
#           -> one-sample t-test

x <- na.omit(df$seats_inside)

a1 <- t.test(x, mu = 30)

a1

(mean(x)-30) / sd(x)

median(x)


A1 <- data.frame(n = length(x), mean = round(mean(x), 1),
                 ci_low = round(a1$conf.int[1], 1), ci_high = round(a1$conf.int[2], 1),
                 median = median(x), t = round(a1$statistic, 2), df = a1$parameter,
                 p = fp(a1$p.value), d = round((mean(x) - 30) / sd(x), 2))
A1
save_tab(A1, "A1")

# WHAT: the A1 figure: every cafe as a grey dot, the mean with its 95% CI in blue,
#       and the benchmark of 30 as a red dashed line.
# HOW:  pdf() opens the file; stripchart() draws one dot per cafe (method =
#       "jitter" spreads them sideways; ylim zooms to 0-80 seats);
#       abline(h = 30) draws the line; points() draws the mean;
#       arrows(..., code = 3, angle = 90) draws the CI as a bar with flat ends;
#       dev.off() saves the file.
pdf("report/figures/A1_figure.pdf", width = 5, height = 4)
stripchart(x, method = "jitter", vertical = TRUE, pch = 16, col = "grey60",
           ylim = c(0, 80), ylab = "Seats inside", main = "A1: seats inside vs 30")
abline(h = 30, lty = 2, col = "red")
points(1, mean(x), pch = 19, col = "blue", cex = 1.5)
arrows(1, a1$conf.int[1], 1, a1$conf.int[2], angle = 90, code = 3,
       length = 0.1, col = "blue", lwd = 2)
dev.off()




#A2: Do cafes WITH outdoor seating have MORE seats inside than cafes without?
  #   H0: the mean seats inside is the same in both groups
  #   H1: it is HIGHER in cafes with outdoor seating (one-tailed: "more")
  #   Method: parametric (both groups n >= 30, CLT applies) -> Welch's t-test
  #   Difference = with terrace minus without
  
x_with <- na.omit(df$seats_inside[df$has_outdoor %in% TRUE])
x_without <- na.omit(df$seats_inside[df$has_outdoor %in% FALSE])

mean(x_with)

mean(x_without)

a2 <- t.test(x_with, x_without, alternative = "greater")
a2

t.test(x_with,x_without)$conf.int

sp <- sqrt(((length(x_with) - 1) * var(x_with) + (length(x_without) - 1) * var(x_without)) /
             (length(x_with) + length(x_without) - 2))
(mean(x_with) - mean(x_without)) / sp



#-
# WHAT: the two-sided 95% CI of the difference, and Cohen's d, stored for the table.
# HOW:  $conf.int from the two-tailed test; d = difference / pooled sd.
ci2 <- t.test(x_with, x_without)$conf.int
d2  <- (mean(x_with) - mean(x_without)) / sp

# WHAT: the A2 table: both groups' n and means, the difference, its CI,
#       Welch's t and df, the one-tailed p, and d.
# HOW:  data.frame() with one row; round() for decimals; fp() formats p.
A2 <- data.frame(n_with = length(x_with), n_without = length(x_without),
                 mean_with = round(mean(x_with), 1), mean_without = round(mean(x_without), 1),
                 difference = round(mean(x_with) - mean(x_without), 1),
                 ci_low = round(ci2[1], 1), ci_high = round(ci2[2], 1),
                 t = round(a2$statistic, 2), df = round(a2$parameter, 2),
                 p_one_tailed = fp(a2$p.value), d = round(d2, 2))
A2
save_tab(A2, "A2")

# WHAT: the A2 figure: dots for each cafe in both groups, plus each group's
#       mean with its 95% CI in blue.
# HOW:  stripchart(list(...)) draws one column of dots per group;
#       t.test(group)$conf.int gives each group's own CI;
#       points() and arrows() draw the means and CIs at positions 1 and 2.
pdf("report/figures/A2_figure.pdf", width = 5, height = 4)
stripchart(list("Without terrace" = x_without, "With terrace" = x_with),
           method = "jitter", vertical = TRUE, pch = 16, col = "grey60",
           ylim = c(0, 100), ylab = "Seats inside", main = "A2: seats inside by terrace")
ci_without <- t.test(x_without)$conf.int
ci_with    <- t.test(x_with)$conf.int
points(c(1, 2), c(mean(x_without), mean(x_with)), pch = 19, col = "blue", cex = 1.5)
arrows(c(1, 2), c(ci_without[1], ci_with[1]), c(1, 2), c(ci_without[2], ci_with[2]),
       angle = 90, code = 3, length = 0.1, col = "blue", lwd = 2)
dev.off()




# A3: How many MORE seats does a cafe have inside than outside?
#   H0: the mean difference (inside minus outside) = 0
#   H1: the mean difference is greater than 0 (one-tailed: "more")
#   Method: parametric (n = 191, CLT applies) -> paired t-test

both <- !is.na(df$seats_inside) & !is.na(df$seats_outside
                                        )
inside <- df$seats_inside[both]
outside <- df$seats_outside[both]

d3 <- inside - outside

mean(d3)

a3 <- t.test(inside, outside, paired = TRUE, alternative = "greater")
a3

t.test(inside, outside, paired = TRUE)$conf.int

mean(d3) / sd(d3)

#-

# WHAT: the two-sided 95% CI of the mean difference, stored for the table.
# HOW:  $conf.int from the two-tailed paired test.
ci3 <- t.test(inside, outside, paired = TRUE)$conf.int

# WHAT: the A3 table: n, mean inside, mean outside, mean difference, CI, t, df,
#       one-tailed p, and dz.
# HOW:  data.frame() with one row.
A3 <- data.frame(n = length(d3), mean_inside = round(mean(inside), 1),
                 mean_outside = round(mean(outside), 1),
                 difference = round(mean(d3), 1),
                 ci_low = round(ci3[1], 1), ci_high = round(ci3[2], 1),
                 t = round(a3$statistic, 2), df = a3$parameter,
                 p_one_tailed = fp(a3$p.value), dz = round(mean(d3) / sd(d3), 2))
A3
save_tab(A3, "A3")

# WHAT: the A3 figure: a histogram of the per-cafe differences, a red line at 0
#       (no difference), and the mean difference with its CI in blue.
# HOW:  hist(); abline(v = 0) draws a vertical line; abline(v = c(...)) draws the CI
#       ends as dashed blue lines; xlim zooms to -60..120.
pdf("report/figures/A3_figure.pdf", width = 6, height = 4)
hist(d3, breaks = 40, col = "grey80", border = "white", xlim = c(-60, 120),
     xlab = "Seats inside minus seats outside (same cafe)", main = "A3: inside minus outside")
abline(v = 0, lty = 2, col = "red")
abline(v = mean(d3), col = "blue", lwd = 2)
abline(v = ci3, col = "blue", lty = 3, lwd = 2)
dev.off()

#A4: Does the number of outdoor seats differ between districts?
  #   H0: outdoor seats are distributed the same way in all six districts
  #   H1: at least one district differs
  #   Method: NON-parametric (Prague 2, 3, 5 have n < 30 and skewed data)
  #           -> Kruskal-Wallis test, then Dunn's pairwise test (Holm-adjusted)
  # =============================================================================

d4 <- df[!is.na(df$district_A4) & !is.na(df$seats_outside), ]

tapply(d4$seats_outside, d4$district_A4, length)
tapply(d4$seats_outside, d4$district_A4, mean)
tapply(d4$seats_outside, d4$district_A4, median)

a4 <- kruskal.test(seats_outside ~ district_A4, data = d4)

a4

library(rstatix)

dunn_test(d4, seats_outside ~ district_A4, p.adjust.method = "holm")

#-
# WHAT: the Dunn results stored (so they can be saved), and the post hoc table:
#       each pair, z, raw p and Holm-adjusted p.
# HOW:  '<-' stores the result; data.frame() picks the columns; fp() formats p.
post4 <- dunn_test(d4, seats_outside ~ district_A4, p.adjust.method = "holm")
A4_post <- data.frame(pair = paste(post4$group2, "vs", post4$group1),
                      z = round(post4$statistic, 2),
                      p = fp(post4$p), p_holm = fp(post4$p.adj))
save_tab(A4_post, "A4_posthoc")

# WHAT: each district's n, mean, 95% CI and median, plus the Kruskal-Wallis result.
# HOW:  tapply() per district; t.test()$conf.int gives each district's CI;
#       sapply() runs it for every district.
districts <- sort(unique(d4$district_A4))
ci4 <- sapply(districts, function(d) t.test(d4$seats_outside[d4$district_A4 == d])$conf.int)
A4 <- data.frame(district = districts,
                 n = as.vector(tapply(d4$seats_outside, d4$district_A4, length)),
                 mean = round(as.vector(tapply(d4$seats_outside, d4$district_A4, mean)), 1),
                 ci_low = round(ci4[1, ], 1), ci_high = round(ci4[2, ], 1),
                 median = as.vector(tapply(d4$seats_outside, d4$district_A4, median)))
A4$test <- c(paste0("Kruskal-Wallis H(", a4$parameter, ") = ", round(a4$statistic, 2),
                    ", p = ", fp(a4$p.value)), rep("", length(districts) - 1))
A4
save_tab(A4, "A4")

# WHAT: the A4 figure: dots for every cafe per district, plus each district's
#       mean with its 95% CI in blue.
# HOW:  stripchart(values ~ group) draws one column per district; points() and
#       arrows() add the means and CIs at positions 1 to 6.
pdf("report/figures/A4_figure.pdf", width = 7, height = 4)
stripchart(seats_outside ~ district_A4, data = d4, method = "jitter", vertical = TRUE,
           pch = 16, col = "grey60", ylim = c(0, 70), ylab = "Seats outside",
           main = "A4: outdoor seats by district")
points(1:6, A4$mean, pch = 19, col = "blue", cex = 1.5)
arrows(1:6, ci4[1, ], 1:6, ci4[2, ], angle = 90, code = 3, length = 0.08, col = "blue", lwd = 2)
dev.off()
# =============================================================================
# A5: Is a SMALL cafe (15 or fewer seats inside) MORE likely to be top-rated
#     (4.7 or higher) than other cafes? By how many percentage points?
#   H0: the share of top-rated cafes is the same for small and other cafes
#   H1: it is HIGHER for small cafes (one-tailed: "more likely")
#   Method: two yes/no variables -> 2x2 table and chi-square test
# =============================================================================


d5 <- df[!is.na(df$small_cafe) & !is.na(df$top_rated), ]

tab5 <- table(small = d5$small_cafe, top_rated = d5$top_rated)

tab5

round(100* prop.table(tab5, 1), 1)

a5 <- chisq.test(tab5, correct = FALSE)
a5
a5$expected

sqrt(a5$statistic / sum(tab5))



#-

# WHAT: the one-tailed test of "small cafes are MORE often top-rated", and the
#       95% CI of the difference in percentage points.
# HOW:  prop.test(successes, group sizes): the small cafes come FIRST, so
#       alternative = "greater" means "small is higher". tab5["TRUE", "TRUE"]
#       = small AND top-rated; sum(tab5["TRUE", ]) = all small cafes.
top   <- c(tab5["TRUE", "TRUE"], tab5["FALSE", "TRUE"])
sizes <- c(sum(tab5["TRUE", ]), sum(tab5["FALSE", ]))
p5_one <- prop.test(top, sizes, alternative = "greater", correct = FALSE)
p5_two <- prop.test(top, sizes, correct = FALSE)

# WHAT: the A5 table: % top-rated in each group, the difference in percentage
#       points with its CI, chi-square, one-tailed p, Cramer's V.
# HOW:  100 x proportions; $conf.int from the two-tailed test.
A5 <- data.frame(pct_small = round(100 * top[1] / sizes[1], 1),
                 pct_other = round(100 * top[2] / sizes[2], 1),
                 difference_pp = round(100 * (top[1] / sizes[1] - top[2] / sizes[2]), 1),
                 ci_low = round(100 * p5_two$conf.int[1], 1),
                 ci_high = round(100 * p5_two$conf.int[2], 1),
                 chi2 = round(a5$statistic, 2), N = sum(tab5),
                 p_one_tailed = fp(p5_one$p.value),
                 V = round(sqrt(a5$statistic / sum(tab5)), 2),
                 min_expected = round(min(a5$expected), 1))
A5
save_tab(A5, "A5")

# WHAT: the A5 figure: a bar per group (% top-rated) with its 95% CI.
# HOW:  barplot() returns the bar positions (stored in 'bars'); each group's own
#       CI comes from prop.test(); arrows() draws them on the bars.
ci_small <- 100 * prop.test(top[1], sizes[1])$conf.int
ci_other <- 100 * prop.test(top[2], sizes[2])$conf.int
pdf("report/figures/A5_figure.pdf", width = 4.5, height = 4)
bars <- barplot(c("Small (15 or fewer)" = A5$pct_small, "Other" = A5$pct_other),
                ylim = c(0, 100), col = "grey80", ylab = "% top-rated (4.7 or higher)",
                main = "A5: top-rated by size")
arrows(bars, c(ci_small[1], ci_other[1]), bars, c(ci_small[2], ci_other[2]),
       angle = 90, code = 3, length = 0.1, col = "blue", lwd = 2)
dev.off()
