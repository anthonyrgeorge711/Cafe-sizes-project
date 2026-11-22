# WHAT: a helper that installs a package if it is missing, then loads it,
#       so the script also runs on a computer that doesn't have the package yet.
# HOW:  requireNamespace() checks whether it is installed; if not ('!'),
#       install.packages() installs it from CRAN; library() then loads it
#       (character.only = TRUE because the name is stored in a variable).
load_packages <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, repos = "https://cloud.r-project.org")
    }
    library(pkg, character.only = TRUE)
  }
}

# WHAT: loads (and if needed installs) every package this script uses:
#       xtable (LaTeX tables), rstatix (Dunn's test, A4), lmtest (Breusch-Pagan,
#       A7-A9) and marginaleffects (average marginal effect, A10).
# HOW:  our load_packages() helper.
load_packages(c("xtable", "rstatix", "lmtest", "marginaleffects"))

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




#A6


d6 <- df[!is.na(df$price_level) & !is.na(df$seats_inside),]

a6 <- cor.test(d6$seats_inside, d6$price_level, method = "spearman", exact = FALSE)

a6

set.seed(2026)
boot_rho <- replicate (5000, {
  i <- sample(nrow(d6), replace = TRUE)
  cor(d6$seats_inside[i], d6$price_level[i], method = "spearman")
})

ci6 <- quantile(boot_rho, c(0.025, 0.975))
                
ci6

A6 <- data.frame(n = nrow(d6), rho = round(a6$estimate, 2),
                 ci_low = round(ci6[1], 2), ci_high = round(ci6[2], 2),
                 shared_variance = round(100 * a6$estimate^2, 1),
                 p = fp(a6$p.value))

A6

save_tab(A6, "A6")


pdf("report/figures/A6_figure.pdf", width = 6, height = 4)
plot(d6$seats_inside, d6$price_level, pch = 16, col = "grey50",
     xlab = "Seats inside", ylab = "Price Level (CZK)", main = "A6: Price Level vs seats",
     sub = paste0("Spearman's rho = ", A6$rho, ", 95% CI [", A6$ci_low, ", ", A6$ci_high, "]"))
dev.off()


# A7

library(lmtest)

d6$seats10 <- d6$seats_inside /10

m7 <- lm(price_level ~ seats10,data = d6)

summary(m7)

reg_tab <- function(m) {
  co <- summary(m)$coefficients
  ci <- confint(m)
  data.frame(term = rownames(co), B = round(co[, 1], 2), SE = round(co[, 2], 2),
             ci_low = round(ci[, 1], 2), ci_high = round(ci[, 2], 2),
             t = round(co[, 3], 2), p = fp(co[, 4]))
}

bptest(m7)

pdf("report/figures/A7_diagnostics.pdf", width = 9, height = 3.5)
par(mfrow = c(1, 3))
plot(m7, which = c(1, 2, 4))
dev.off()

cd7 <- cooks.distance(m7)
inf17 <- data.frame(cafe = d6$cafe_name, cooks_d = round(cd7, 3))[cd7 > 4 / nrow(d6), ]
inf17[order(-inf17$cooks_d), ]

A7 <- reg_tab(m7)
A7$n  <- nobs(m7)
A7$R2 <- round(summary(m7)$r.squared, 2)
A7
save_tab(A7, "A7")

grid7 <- data.frame(seats10 = seq(0, max(d6$seats10), length.out = 100))
pred7 <- predict(m7, grid7, interval = "confidence")
pdf("report/figures/A7_figure.pdf", width = 6, height = 4)
plot(d6$seats_inside, d6$price_level, pch = 16, col = "grey50",
     xlab = "Seats inside", ylab = "Price Level (CZK)", main = "A7: Price Level per 10 seats")
lines(grid7$seats10 * 10, pred7[, "fit"], col = "blue", lwd = 2)
lines(grid7$seats10 * 10, pred7[, "lwr"], col = "blue", lty = 2)
lines(grid7$seats10 * 10, pred7[, "upr"], col = "blue", lty = 2)
dev.off()




#----A8

d8 <- d6[!is.na(d6$dist_orloj), ]

m8a <- lm(price_level ~ seats10, data = d8)

m8 <- lm(price_level ~ seats10 + dist_orloj, data =d8)

summary(m8)

b_before <- coef(m8a)["seats10"]
b_after <- coef(m8)["seats10"]
b_before
b_after
round(100* b_after / b_before, 1)

bptest(m8)
pdf("report/figures/A8_diagnostics.pdf", width = 9, height = 3.5)
par(mfrow = c(1, 3))
plot(m8, which = c(1, 2, 4))
dev.off()

cd8 <- cooks.distance(m8)
infl8 <- data.frame(cafe = d8$cafe_name, cooks_d = round(cd8, 3))[cd8 > 4/nrow(d8), ]
infl8[order(-infl8$cooks_d), ]

A8 <- reg_tab(m8)
A8$n <- nobs(m8)
A8$R2 <- round(summary(m8)$r.squared, 2)
A8$seats_sloped_before <- round(b_before, 2)

A8
save_tab(A8, "A8")

ci_before <- confint(m8a)["seats10", ]
ci_after  <- confint(m8)["seats10", ]
pdf("report/figures/A8_figure.pdf", width = 5, height = 4)
plot(c(1, 2), c(b_before, b_after), xlim = c(0.5, 2.5),
     ylim = range(c(0, ci_before, ci_after)), pch = 19, col = "blue", xaxt = "n",
     xlab = "", ylab = "CZK per 10 seats inside", main = "A8: seats slope before and after")
axis(1, at = c(1, 2), labels = c("Seats only", "Seats + distance"))
arrows(c(1, 2), c(ci_before[1], ci_after[1]), c(1, 2), c(ci_before[2], ci_after[2]),
       angle = 90, code = 3, length = 0.1, col = "blue", lwd = 2)
abline(h = 0, lty = 2, col = "red")
dev.off()


#--------

#A9

d9 <- d6[!is.na(d6$has_outdoor), ]
d9$outdoor <- factor(ifelse(d9$has_outdoor, "Outdoor", "No outdoor"),
                     levels = c("No outdoor", "Outdoor"))

m9 <- lm(price_level ~ seats10 * outdoor, data = d9)

summary(m9)

bptest(m9)

pdf("report/figures/A9_diagnostics.pdf", width = 9, height = 3.5)
par(mfrow = c(1, 3))
plot(m9, which = c(1, 2, 4))
dev.off()

cd9 <- cooks.distance(m9)
infl9 <- data.frame(cafe = d9$cafe_name, cooks_d = round(cd9, 3))[cd9 >4 / nrow(d9), ]
infl9[order(-infl9$cooks_d), ]

A9 <- reg_tab(m9)
A9$n <- nobs(m9)
A9$R2 <- round(summary(m9)$r.squared, 2)

A9

save_tab(A9, "A9")


pdf("report/figures/A9_figure.pdf", width = 6, height = 4)
cols <- c("No outdoor" = "darkorange", "Outdoor" = "steelblue")
plot(d9$seats_inside, d9$price_level, pch = 16, col = cols[as.character(d9$outdoor)],
     xlab = "Seats inside", ylab = "Price Level (CZK)", main = "A9: slope by outdoor seating")
for (g in levels(d9$outdoor)) {
  grid9 <- data.frame(seats10 = seq(0, max(d9$seats10), length.out = 100),
                      outdoor = factor(g, levels = levels(d9$outdoor)))
  pred9 <- predict(m9, grid9, interval = "confidence")
  lines(grid9$seats10 * 10, pred9[, "fit"], col = cols[g], lwd = 2)
  lines(grid9$seats10 * 10, pred9[, "lwr"], col = cols[g], lty = 2)
  lines(grid9$seats10 * 10, pred9[, "upr"], col = cols[g], lty = 2)
}
legend("topright", legend = names(cols), col = cols, pch = 16, lwd = 2, bty = "n")
dev.off()



#----

#A10


library(marginaleffects)

d10 <- d6[!is.na(d6$above_median_price), ]

m10 <- glm(above_median_price ~ seats10, data = d10, family = binomial)

summary(m10)

ame10 <- avg_slopes(m10, variables = "seats10")
ame10

A10 <- data.frame(n= nobs(m10),
                  ame_pp = round(100* ame10$estimate, 1),
                  ci_low = round(100 * ame10$conf.low, 1),
                  ci_high = round(100 * ame10$conf.high, 1),
                  z = round(ame10$statistic, 2), p = fp(ame10$p.value),
                  odds_ratio = round(exp(coef(m10)["seats10"]), 2))

A10

save_tab(A10, "A10")

grid10 <- data.frame(seats10 = seq(0, max(d10$seats10), length.out = 100))
pr10 <- predict(m10, grid10, type = "link", se.fit = TRUE)
pdf("report/figures/A10_figure.pdf", width = 6, height = 4)
plot(d10$seats_inside, jitter(as.numeric(d10$above_median_price), amount = 0.03),
     pch = 16, col = "grey50", xlab = "Seats inside",
     ylab = "P(Price Level above median)", main = "A10: chance of an above-median price")
lines(grid10$seats10 * 10, plogis(pr10$fit), col = "blue", lwd = 2)
lines(grid10$seats10 * 10, plogis(pr10$fit - 1.96 * pr10$se.fit), col = "blue", lty = 2)
lines(grid10$seats10 * 10, plogis(pr10$fit + 1.96 * pr10$se.fit), col = "blue", lty = 2)
dev.off()



#----

#summary

ci_txt <- function(lo, hi) paste0("[", lo, ", ", hi, "]")


row_of <- function(tab, name) tab[tab$term == name, ]
r7 <- row_of(A7, "seats10")
r8 <- row_of(A8, "seats10")
r9 <- row_of(A9, "seats10:outdoorOutdoor")


S <- data.frame(
  analysis = paste0("A", 1:10),
  question = c("Mean seats inside vs 30", "Seats inside: terrace vs none",
               "Seats inside minus outside", "Outdoor seats by district",
               "Small cafe -> top-rated", "Price Level ~ seats inside",
               "Price Level per 10 seats", "Seats slope, controlling distance",
               "Seats slope: terrace vs none", "P(above-median price) per 10 seats"),
  method = c("One-sample t-test", "Welch t-test (one-tailed)", "Paired t-test (one-tailed)",
             "Kruskal-Wallis + Dunn (Holm)", "Chi-square (one-tailed)", "Spearman's rho",
             "OLS regression", "Multiple regression", "Regression with interaction",
             "Logistic regression (AME)"),
  estimate = c(
    paste0("M = ", A1$mean, " seats ", ci_txt(A1$ci_low, A1$ci_high)),
    paste0("Diff = ", A2$difference, " seats ", ci_txt(A2$ci_low, A2$ci_high)),
    paste0("Diff = ", A3$difference, " seats ", ci_txt(A3$ci_low, A3$ci_high)),
    paste0("Means ", min(A4$mean), "-", max(A4$mean), " seats"),
    paste0(A5$difference_pp, " pp ", ci_txt(A5$ci_low, A5$ci_high)),
    paste0("rho = ", A6$rho, " ", ci_txt(A6$ci_low, A6$ci_high)),
    paste0(r7$B, " CZK ", ci_txt(r7$ci_low, r7$ci_high)),
    paste0(r8$B, " CZK ", ci_txt(r8$ci_low, r8$ci_high), " (was ", r7$B, ")"),
    paste0("Slope diff ", r9$B, " CZK ", ci_txt(r9$ci_low, r9$ci_high)),
    paste0(A10$ame_pp, " pp ", ci_txt(A10$ci_low, A10$ci_high))),
  statistic = c(
    paste0("t(", A1$df, ") = ", A1$t, ", d = ", A1$d),
    paste0("t(", A2$df, ") = ", A2$t, ", d = ", A2$d),
    paste0("t(", A3$df, ") = ", A3$t, ", dz = ", A3$dz),
    paste0("H(", a4$parameter, ") = ", round(a4$statistic, 2)),
    paste0("chi2(1, N = ", A5$N, ") = ", A5$chi2, ", V = ", A5$V),
    paste0("n = ", A6$n),
    paste0("t = ", r7$t, ", R2 = ", A7$R2[1]),
    paste0("t = ", r8$t, ", R2 = ", A8$R2[1]),
    paste0("t = ", r9$t, ", R2 = ", A9$R2[1]),
    paste0("z = ", A10$z)),
  p = c(A1$p, A2$p_one_tailed, A3$p_one_tailed, fp(a4$p.value), A5$p_one_tailed,
        A6$p, r7$p, r8$p, r9$p, A10$p))

S
save_tab(S, "S_summary")
