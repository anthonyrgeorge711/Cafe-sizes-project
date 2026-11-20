

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


#checks 

check <- function(x) {
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
  # WHAT: rule 2: CLT applies if 30+ cafes AND the averages look normal.
  # HOW:  '&' = AND.
  clt <- length(x) >= 30 & abs(boot_skew) < 0.5
  # WHAT: the verdict: parametric if rule 1 OR rule 2 holds.
  # HOW:  '|' = OR; ifelse() picks the word.
  verdict <- ifelse(normal | clt, "Parametric", "Non-parametric")
  # WHAT: returns one row of results.  HOW: data.frame(); round() to 2 decimals.
  data.frame(n = length(x), skew = round(skew, 2), normal, clt, verdict)
}

# WHAT: test the function on A1 (should say Parametric, n = 193).
# HOW:  call it with the seats-inside column.
check(df$seats_inside)
#---

# ---- Step 6: The decision table ----

# WHAT: runs check() on every variable and group the questions compare,
#       and stacks the results into one table, one row per group.
# HOW:  each check(...) returns one row; cbind() adds a label column in front;
#       rbind() stacks all rows on top of each other.
#       df$has_outdoor %in% TRUE picks only cafes WITH a terrace (and skips NA);
#       %in% FALSE picks the cafes without one.
D <- rbind(
  cbind(group = "A1: seats inside, all cafes",
        check(df$seats_inside)),
  cbind(group = "A2: seats inside, WITH terrace",
        check(df$seats_inside[df$has_outdoor %in% TRUE])),
  cbind(group = "A2: seats inside, WITHOUT terrace",
        check(df$seats_inside[df$has_outdoor %in% FALSE])),
  cbind(group = "A3: inside minus outside",
        check(df$seats_inside - df$seats_outside)),
  cbind(group = "A4: seats outside, Prague 1",
        check(df$seats_outside[df$district_A4 %in% "Prague 1"])),
  cbind(group = "A4: seats outside, Prague 2",
        check(df$seats_outside[df$district_A4 %in% "Prague 2"])),
  cbind(group = "A4: seats outside, Prague 3",
        check(df$seats_outside[df$district_A4 %in% "Prague 3"])),
  cbind(group = "A4: seats outside, Prague 5",
        check(df$seats_outside[df$district_A4 %in% "Prague 5"])),
  cbind(group = "A4: seats outside, Prague 7",
        check(df$seats_outside[df$district_A4 %in% "Prague 7"])),
  cbind(group = "A4: seats outside, Prague 8",
        check(df$seats_outside[df$district_A4 %in% "Prague 8"]))
)

# WHAT: shows the decision table.
# HOW:  typing the name of an object prints it.
D
