
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
