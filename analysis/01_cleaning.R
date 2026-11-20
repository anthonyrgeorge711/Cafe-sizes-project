raw <- read.csv("analysis/raw_data.csv",
                colClasses = "character",
                na.strings = c("","NA","na","#VALUE!"))



dim(raw)

str(raw)

colSums(is.na(raw))

#remove empty rows
empty_rows <- is.na(raw$Cafe.name)& is.na(raw$Cafe.address..Google.Maps.)

sum(empty_rows)

clean <- raw[!empty_rows,]

dim(clean)

#clean cafes entered twice

clean[] <- lapply(clean, trimws)
clean$name_key <- tolower(gsub("[^[:alnum:]]", "", clean$Cafe.name))
repeat_name <- duplicated(clean$name_key)
sum(repeat_name)
clean[clean$name_key %in% clean$name_key[repeat_name], c("Name","Cafe.name","Cafe.address..Google.Maps.","Seating.Places.Inside")]

#new 

clean$lat <- as.numeric(clean$Latitude)
clean$lon <- as.numeric(clean$Longitude)

first <- match(clean$name_key, clean$name_key)

km_apart <- sqrt(((clean$lat - clean$lat[first]) * 111)^2 +
                   ((clean$lon - clean$lon[first]) * 71)^2)
duplicate <- first != seq_len(nrow(clean)) & km_apart <0.1

sum(duplicate)
clean$Cafe.name[duplicate]

clean <- clean[!duplicate, ]

nrow(clean)

#fix spelling
table(clean$District..number., useNA = "ifany")

table(clean$Cafe.Type, useNA ="ifany")

## ---- Step 5: Fix spelling of categories ----

# WHAT: lists every district spelling and how often it appears.
# HOW:  table() counts each different value; useNA = "ifany" also shows missing ones.
table(clean$District..number., useNA = "ifany")
# WHAT: the same for the cafe type.
# HOW:  table() again.
table(clean$Cafe.Type, useNA = "ifany")

# WHAT: makes every district label lower case.
# HOW:  tolower().
district_fix <- tolower(clean$District..number.)

# WHAT: fixes the typos "prauge" and "prage".
# HOW:  gsub(); '|' means OR.
district_fix <- gsub("prauge|prage", "prague", district_fix)

# WHAT: puts the capital P back: "Prague 1".
# HOW:  sub() replaces "prague" with "Prague"; stored in a NEW column 'district'.
clean$district <- sub("prague", "Prague", district_fix)

# WHAT: checks the fix: original spelling vs cleaned label.
# HOW:  a cross-table with table().
table(original = clean$District..number., cleaned = clean$district)

# WHAT: cafe type with "Chain" written as "chain"; the missing one stays NA.
# HOW:  sub(); stored in a NEW column 'cafe_type'.
clean$cafe_type <- sub("Chain", "chain", clean$Cafe.Type)

# WHAT: checks the cafe types.
# HOW:  table() with useNA = "ifany".
table(clean$cafe_type, useNA = "ifany")


#turn text to numbers

# WHAT: a small helper that turns a price cell into one number, keeping the
#       FIRST price: "85, 95, 105" -> 85, "55/30" -> 55, "70" -> 70, NA -> NA.
# HOW:  function(x) defines our own function. sub() keeps only the first
#       number at the start: "^([0-9.]+).*$" means "digits at the start (kept,
#       because of the brackets), then anything (.*) up to the end"; "\\1"
#       means "replace everything with just the bracketed part".
#       as.numeric() then turns that text into a number.
first_price <- function(x) as.numeric(sub("^([0-9.]+).*$", "\\1", x))

# WHAT: the four cleaned prices, as numbers, in NEW columns.
# HOW:  first_price() applied to each raw price column.
clean$espresso   <- first_price(clean$Espresso.Price.in.CZK..dine.in.)
clean$cappuccino <- first_price(clean$Cappuccino.Price.in.CZK..normal.milk..dine.in.)
clean$flat_white <- first_price(clean$Flat.White.Price.in.CZK..normal.milk..dine.in.)
clean$filter     <- first_price(clean$Filter.Batch..in.CZK..dine.in.)

# WHAT: checks the conversion: original text next to the number we kept,
#       for the odd espresso cells only.
# HOW:  'odd' marks those rows (recomputed here for espresso); the two
#       columns after the comma are shown side by side.
odd <- !grepl("^[0-9.]+$", clean$Espresso.Price.in.CZK..dine.in.) &
  !is.na(clean$Espresso.Price.in.CZK..dine.in.)
clean[odd, c("Espresso.Price.in.CZK..dine.in.", "espresso")]

# WHAT: a quick look at the converted prices: min, median, mean, max, missing.
# HOW:  summary() on the four new columns.
summary(clean[, c("espresso", "cappuccino", "flat_white", "filter")])


#clean seatd

clean$seats_inside  <- as.integer(clean$Seating.Places.Inside)
clean$seats_outside <- as.integer(clean$Seating.Places.Outside)

clean$rating <- as.numeric(clean$Rating..Google.)

clean$n_ratings <- as.numeric(gsub(",", "", clean$Number.of.ratings..Google.))

# WHAT: finds the distance column by searching for "Orloj" in the column names,
#       so we never have to type the letter ž (which breaks on some computers).
# HOW:  grep(word, names) gives the position of the column whose name contains it.
orloj_col <- grep("Orloj", names(clean))

# WHAT: the distance from the Orloj in km, as a number.
# HOW:  clean[[position]] picks a column by its number instead of its name.
clean$dist_orloj <- as.numeric(clean[[orloj_col]])
summary(clean[, c("seats_inside", "seats_outside", "rating", "n_ratings", "dist_orloj")])
#-----------
#new variables

has_all3 <- !is.na(clean$espresso) & !is.na(clean$cappuccino) & !is.na(clean$flat_white)

clean$price_level <- ifelse(has_all3,
                            (clean$espresso + clean$cappuccino + clean$flat_white) / 3,
                            NA)

clean$has_outdoor <- clean$seats_outside>0

clean$small_cafe <- clean$seats_inside<=15

clean$top_rated <-clean$rating >=4.7

clean$district_A4 <- ifelse(clean$district %in% c("Prague 1", "Prague 2", "Prague 3",
                                                  "Prague 5", "Prague 7", "Prague 8"),
                            clean$district, NA)
median_price <- median(clean$price_level, na.rm = TRUE)
median_price

clean$above_median_price <- clean$price_level > median_price

table(clean$has_outdoor, useNA ="ifany")
table(clean$small_cafe,  useNA = "ifany")
table(clean$top_rated,   useNA = "ifany")
#save

clean$cafe_name   <- clean$Cafe.name
clean$student     <- clean$Name
clean$is_roastery <- clean$Is.Roastery..0.1.

keep <- c("cafe_name", "student", "district", "district_A4", "cafe_type",
          "is_roastery", "espresso", "cappuccino", "flat_white", "filter",
          "price_level", "above_median_price", "seats_inside", "seats_outside",
          "has_outdoor", "small_cafe", "rating", "n_ratings", "top_rated",
          "dist_orloj", "lat", "lon")

dataset <- clean[, keep]
dim(dataset)
colSums(is.na(dataset))

write.csv(dataset, "analysis/dataset.csv", row.names = FALSE)
