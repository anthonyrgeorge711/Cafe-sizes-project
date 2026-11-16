# =============================================================================
# 01_cleaning.R
# Reads the raw class data, cleans it, and writes analysis/dataset.csv.
# Run it from the project folder (open "Cafe project.Rproj" first).
# The raw file is never edited by hand: every change happens here, in code.
#
# Reading guide: each line of code has a comment above or next to it.
#   WHAT = what the line achieves
#   HOW  = which function or trick does it
# =============================================================================


# ---- 1. Read the raw data ---------------------------------------------------

# WHAT: loads the class spreadsheet (saved as CSV) into R and calls it 'raw'.
# HOW:  read.csv() reads a comma-separated file into a table (a "data frame").
#       '<-' stores the result under the name on its left.
raw <- read.csv("analysis/raw_data.csv",
                # WHAT: read every column as text, not as numbers.
                # HOW:  "character" is R's word for text. Without this, R would
                #       guess each column's type and quietly turn a cell like
                #       "89, 99" into NA, and we would lose data without noticing.
                colClasses = "character",
                # WHAT: tells R which cell contents mean "missing".
                # HOW:  c() makes a list of values; any cell equal to one of them
                #       becomes NA: an empty cell, "NA", "na", or the spreadsheet
                #       error "#VALUE!".
                na.strings = c("", "NA", "na", "#VALUE!"),
                # WHAT: reads Czech letters (ř, ž, é) correctly.
                # HOW:  UTF-8 is the text encoding the file was saved in.
                encoding = "UTF-8")


# ---- 2. Inspect before changing anything ------------------------------------

# WHAT: prints the number of rows and columns (299 and 26).
# HOW:  dim() = "dimensions" of a table.
dim(raw)

# WHAT: prints every column with its type and its first few values.
# HOW:  str() = "structure". Every column should show 'chr' (text).
str(raw)

# WHAT: counts the missing values in each column.
# HOW:  is.na() turns every cell into TRUE (missing) or FALSE; colSums() adds up
#       each column, and TRUE counts as 1. The number 99 repeating across almost
#       every column is the clue that there are 99 empty rows.
colSums(is.na(raw))

# WHAT: prints the original column names, so we can check their order.
# HOW:  names() returns the column names of a table.
names(raw)

# WHAT: replaces the long column names with short ones, so the code stays readable.
# HOW:  assigning to names(raw) renames the columns in order: the 1st new name
#       goes to the 1st column, and so on. The "_raw" ending marks the original
#       text version of a column that we will clean into a new column later.
names(raw) <- c("student", "visit_time", "district_raw", "district_name",
                "cafe_name", "address", "coords", "gmaps_link",
                "espresso_raw", "cappuccino_raw", "flat_white_raw", "filter_raw",
                "payment", "seats_inside_raw", "seats_outside_raw",
                "rating_raw", "n_ratings_raw", "cafe_type_raw", "is_roastery",
                "photo_link", "note", "automatic", "dist_orloj_raw",
                "lat_sheet", "lon_sheet", "adj_rating_sheet")


# ---- 3. Remove empty template rows ------------------------------------------
# The sheet has 99 pre-formatted rows where only the Payment dropdown is filled.

# WHAT: marks each row as empty (TRUE) or not (FALSE).
# HOW:  raw$cafe_name picks one column; is.na() asks "is it missing?";
#       '&' means AND, so a row counts as empty only if it has no cafe name
#       AND no address.
empty_rows <- is.na(raw$cafe_name) & is.na(raw$address)

# WHAT: counts the empty rows (99).
# HOW:  sum() adds up the TRUEs, because TRUE counts as 1.
sum(empty_rows)

# WHAT: shows the first six empty rows, so we can look before deleting.
# HOW:  raw[rows, ] picks rows (before the comma) and all columns (nothing after
#       the comma); head() shows only the first six.
head(raw[empty_rows, ])

# WHAT: keeps only the rows that are NOT empty, in a new table called 'clean'.
# HOW:  '!' means NOT, so !empty_rows is TRUE for the real cafes. 'raw' itself
#       stays untouched ("never overwrite the original").
clean <- raw[!empty_rows, ]

# WHAT: checks the result: 200 rows = 10 students x 20 cafes.
# HOW:  dim() again.
dim(clean)


# ---- 4. Duplicates -----------------------------------------------------------

# (a) Exact duplicate rows

# WHAT: counts rows that are identical copies of an earlier row (result: 0).
# HOW:  duplicated() gives TRUE for each row that already appeared above it;
#       sum() counts them.
sum(duplicated(clean))

# (b) The same cafe entered by two students (rows differ, so (a) misses them)

# WHAT: removes stray spaces at the start and end of every cell
#       ("Joseph Muchnick " becomes "Joseph Muchnick").
# HOW:  trimws() = "trim white space"; lapply() applies it to every column;
#       'clean[] <-' puts the cleaned columns back into the same table.
clean[] <- lapply(clean, trimws)

# WHAT: makes a simplified version of each cafe name, so "Cafe-Cafe",
#       "cafe cafe" and "Cafe Cafe" all become "cafecafe".
# HOW:  gsub() replaces text: the pattern [^[:alnum:]] means "anything that is
#       NOT a letter or digit", and it is replaced by "" (nothing). tolower()
#       then makes everything lower case. The result goes into a new column.
clean$name_key <- tolower(gsub("[^[:alnum:]]", "", clean$cafe_name))

# WHAT: takes the latitude out of the coordinates cell "50.08, 14.42".
# HOW:  sub() replaces the first match of a pattern: ",.*$" means "the comma
#       and everything after it", which is deleted, leaving "50.08".
#       as.numeric() turns that text into a number.
clean$lat <- as.numeric(sub(",.*$", "", clean$coords))

# WHAT: takes the longitude out of the same cell.
# HOW:  "^.*," means "everything from the start up to and including the comma",
#       which is deleted, leaving " 14.42"; as.numeric() makes it a number.
clean$lon <- as.numeric(sub("^.*,", "", clean$coords))

# The rule: two rows are the same cafe only if they have the same name key AND
# are less than 100 m apart. A name alone is not enough, because EMA Espresso
# Bar has three different branches.

# WHAT: creates a TRUE/FALSE marker per row, starting with "not repeated" for all.
# HOW:  rep(FALSE, n) repeats FALSE n times; nrow() gives the number of rows.
repeated_row <- rep(FALSE, nrow(clean))

# WHAT: goes through every row i (from the 2nd to the last) ...
# HOW:  a for-loop runs the code inside { } once for each value of i.
for (i in 2:nrow(clean)) {

  # WHAT: ... and compares it with every EARLIER row j.
  # HOW:  1:(i - 1) is the list of row numbers above row i.
  for (j in 1:(i - 1)) {

    # WHAT: only continue if the two rows have the same simplified name.
    # HOW:  '==' tests whether two values are equal; if() runs the code inside
    #       { } only when the test is TRUE.
    if (clean$name_key[i] == clean$name_key[j]) {

      # WHAT: distance between the two cafes in km.
      # HOW:  difference in latitude x 111 km and in longitude x 71 km (the
      #       values from the brief), then Pythagoras: sqrt(a^2 + b^2).
      km_apart <- sqrt(((clean$lat[i] - clean$lat[j]) * 111)^2 +
                       ((clean$lon[i] - clean$lon[j]) * 71)^2)

      # WHAT: prints each same-name pair and how far apart they are, to inspect.
      # HOW:  cat() prints its pieces one after another; round(x, 3) keeps 3
      #       decimals; "\n" starts a new line.
      cat(clean$cafe_name[j], "/", clean$cafe_name[i], ":",
          round(km_apart, 3), "km apart\n")

      # WHAT: if they are under 100 m apart, mark the LATER row as a repeat.
      # HOW:  0.1 km = 100 m; we set position i of the marker to TRUE.
      if (km_apart < 0.1) repeated_row[i] <- TRUE
    }
  }
}

# WHAT: finds every row belonging to a repeated cafe, first entries included.
# HOW:  clean$name_key[repeated_row] lists the names of the repeated rows;
#       %in% asks for each row "is my name in that list?".
same_cafe <- clean$name_key %in% clean$name_key[repeated_row]

# WHAT: shows those rows side by side, to compare them before deleting anything.
# HOW:  rows chosen by same_cafe (before the comma), and only these five columns
#       (the c(...) after the comma).
clean[same_cafe, c("student", "cafe_name", "address", "seats_inside_raw", "espresso_raw")]

# DECISION: keep the FIRST entry of each repeated cafe (the earlier row in the
# shared sheet) and drop the later one. This rule is fixed before looking at
# the values, so we do not pick whichever seat count we like better.

# WHAT: removes the 5 later entries.
# HOW:  !repeated_row is TRUE for every row we keep.
clean <- clean[!repeated_row, ]

# WHAT: checks: 5 repeats removed, 195 cafes left.
# HOW:  sum() counts the TRUEs; nrow() counts the rows.
sum(repeated_row)
nrow(clean)


# ---- 5. Categories: district and cafe type -----------------------------------

# WHAT: lists every spelling used for the district, with how often it appears.
# HOW:  table() counts each different value; useNA = "ifany" also shows the
#       missing ones. Here we see typos like "Prage 7" and "prauge 1".
table(clean$district_raw, useNA = "ifany")

# WHAT: the same for the cafe type.
# HOW:  table() again.
table(clean$cafe_type_raw, useNA = "ifany")

# WHAT: makes the district lower case and fixes the two typos.
# HOW:  tolower() first ("Prague 1" -> "prague 1"); then gsub() replaces
#       "prauge" or "prage" (the '|' means OR) with "prague".
district_fix <- gsub("prauge|prage", "prague", tolower(clean$district_raw))

# WHAT: keeps only the district number ("prague 1" -> 1).
# HOW:  sub() deletes "prague" plus any spaces at the start ('^' = start,
#       '\\s*' = any number of spaces); as.integer() makes the rest a whole number.
district_num <- as.integer(sub("^prague\\s*", "", district_fix))

# WHAT: rebuilds one clean label per district, e.g. "Prague 1".
# HOW:  paste() glues "Prague" and the number with a space; ifelse() keeps NA
#       when the number is missing (ifelse(test, if TRUE, if FALSE)).
clean$district <- ifelse(is.na(district_num), NA, paste("Prague", district_num))

# WHAT: checks the fix: every original spelling next to its cleaned label.
# HOW:  table() with two columns makes a cross-table.
table(original = clean$district_raw, cleaned = clean$district, useNA = "ifany")

# WHAT: lower-case version of the cafe type, so spelling differences in
#       capitals ("Local Chain" vs "local chain") do not matter.
# HOW:  tolower().
type_lower <- tolower(clean$cafe_type_raw)

# WHAT: one consistent label per cafe type; anything else (the one blank cell)
#       stays missing (NA), as the brief asks.
# HOW:  nested ifelse(): if "independent" -> "Independent", else if
#       "local chain" -> "Local chain", else if "international chain" ->
#       "International chain", else NA.
clean$cafe_type <- ifelse(type_lower == "independent", "Independent",
                   ifelse(type_lower == "local chain", "Local chain",
                   ifelse(type_lower == "international chain", "International chain", NA)))

# WHAT: checks the result: original spelling next to the cleaned label.
# HOW:  a cross-table with table().
table(original = clean$cafe_type_raw, cleaned = clean$cafe_type, useNA = "ifany")


# ---- 6. Prices: cells with several values ------------------------------------

# WHAT: a small helper that answers "is this cell something other than a plain
#       number?" (for example "85, 95, 105" or "65/40").
# HOW:  function(x) defines our own function. grepl() tests a pattern:
#       "^[0-9]+(\\.[0-9]+)?$" means "only digits, optionally a decimal part".
#       '!' turns the answer around, and !is.na(x) makes missing cells FALSE.
not_plain <- function(x) !is.na(x) & !grepl("^[0-9]+(\\.[0-9]+)?$", x)

# WHAT: lists the unusual values in each price column BEFORE converting them.
# HOW:  not_plain() marks the odd cells; [ ] picks them; unique() removes
#       repeats so every odd value is shown once.
unique(clean$espresso_raw[not_plain(clean$espresso_raw)])
unique(clean$cappuccino_raw[not_plain(clean$cappuccino_raw)])
unique(clean$flat_white_raw[not_plain(clean$flat_white_raw)])
unique(clean$filter_raw[not_plain(clean$filter_raw)])

# DECISION (class default): keep the FIRST listed price, usually the standard
# size. A missing price stays NA: the cafe does not serve that drink (never 0).

# WHAT: a helper that turns "85, 95, 105" into the number 85.
# HOW:  sub() keeps only the first number at the start of the text: the part in
#       ( ) is the number, and "\\1" means "replace everything with that part".
#       as.numeric() turns it into a number; NA stays NA.
first_price <- function(x) as.numeric(sub("^([0-9]+(\\.[0-9]+)?).*$", "\\1", x))

# WHAT: the cleaned price of each drink, as a number, in new columns.
# HOW:  first_price() applied to each raw price column. The _raw columns stay
#       as they were, so we can compare.
clean$espresso   <- first_price(clean$espresso_raw)
clean$cappuccino <- first_price(clean$cappuccino_raw)
clean$flat_white <- first_price(clean$flat_white_raw)
clean$filter     <- first_price(clean$filter_raw)

# WHAT: marks the rows where any of the three main prices had several values.
# HOW:  '|' means OR: TRUE if the espresso OR cappuccino OR flat white cell was odd.
multi <- not_plain(clean$espresso_raw) | not_plain(clean$cappuccino_raw) |
         not_plain(clean$flat_white_raw)

# WHAT: shows those rows: the original text next to the number we kept, to check
#       by eye that the first price was taken.
# HOW:  rows chosen by 'multi', and six columns listed in c(...).
clean[multi, c("espresso_raw", "espresso", "cappuccino_raw", "cappuccino",
               "flat_white_raw", "flat_white")]


# ---- 7. Seats, ratings and distance: text to numbers -------------------------

# WHAT: lists anything in these columns that is not a plain number, before
#       converting. Only the number of ratings has problems: "1,020".
# HOW:  the not_plain() helper from step 6, as above.
unique(clean$seats_inside_raw[not_plain(clean$seats_inside_raw)])
unique(clean$seats_outside_raw[not_plain(clean$seats_outside_raw)])
unique(clean$rating_raw[not_plain(clean$rating_raw)])
unique(clean$n_ratings_raw[not_plain(clean$n_ratings_raw)])

# WHAT: seats inside and outside as whole numbers. 0 is a real value
#       (a grab-and-go cafe has no seats), so it is kept.
# HOW:  as.integer() turns text into a whole number.
clean$seats_inside  <- as.integer(clean$seats_inside_raw)
clean$seats_outside <- as.integer(clean$seats_outside_raw)

# WHAT: the Google rating as a number (e.g. 4.6).
# HOW:  as.numeric() turns text into a number with decimals.
clean$rating <- as.numeric(clean$rating_raw)

# WHAT: the number of ratings as a number: "1,020" becomes 1020.
# HOW:  gsub(",", "", x) deletes every comma (the thousands separator) first;
#       then as.numeric().
clean$n_ratings <- as.numeric(gsub(",", "", clean$n_ratings_raw))

# WHAT: the distance from the Orloj in km (already computed in the sheet).
# HOW:  as.numeric().
clean$dist_orloj <- as.numeric(clean$dist_orloj_raw)

# WHAT: checks the rows that had a comma: text next to the converted number.
# HOW:  grepl(",", x) is TRUE where the text contains a comma.
clean[grepl(",", clean$n_ratings_raw), c("n_ratings_raw", "n_ratings")]


# ---- 8. New variables the Cafe Size questions need ---------------------------

# WHAT: marks the cafes that have all three prices (espresso, cappuccino, flat white).
# HOW:  !is.na() = "is not missing", combined with '&' (AND) three times.
has_all3 <- !is.na(clean$espresso) & !is.na(clean$cappuccino) & !is.na(clean$flat_white)

# WHAT: the Price Level (questions A6-A10) = the average of the three prices,
#       only for cafes that have all three; the others get NA.
# HOW:  ifelse(test, value if TRUE, value if FALSE); the average is the sum / 3.
clean$price_level <- ifelse(has_all3,
                            (clean$espresso + clean$cappuccino + clean$flat_white) / 3,
                            NA)

# WHAT: does the cafe have outdoor seating? (questions A2 and A9)
# HOW:  '>' compares each value with 0 and gives TRUE/FALSE.
#       A missing seat count gives NA, so missing stays missing.
clean$has_outdoor <- clean$seats_outside > 0

# WHAT: is it a small cafe, with 15 or fewer seats inside? (question A5)
# HOW:  '<=' means "less than or equal to".
clean$small_cafe <- clean$seats_inside <= 15

# WHAT: is the cafe top-rated, with a rating of 4.7 or higher? (question A5)
# HOW:  '>=' means "greater than or equal to".
clean$top_rated <- clean$rating >= 4.7

# WHAT: the district, but only for the six districts that question A4 compares
#       (Prague 1, 2, 3, 5, 7, 8); every other district becomes NA.
# HOW:  paste("Prague", c(1, 2, ...)) builds the six labels; %in% checks
#       whether each cafe's district is one of them; ifelse() keeps it or sets NA.
clean$district_A4 <- ifelse(clean$district %in% paste("Prague", c(1, 2, 3, 5, 7, 8)),
                            clean$district, NA)

# WHAT: the median Price Level, the threshold for question A10.
#       It is computed here and never typed in by hand.
# HOW:  median() finds the middle value; na.rm = TRUE skips the missing ones.
median_price_level <- median(clean$price_level, na.rm = TRUE)

# WHAT: prints the median, so we can see it (79.7 CZK).
# HOW:  typing an object's name prints it.
median_price_level

# WHAT: is the cafe's Price Level above the median? (question A10)
# HOW:  '>' compares each cafe's Price Level with the median.
clean$above_median_price <- clean$price_level > median_price_level


# ---- 9. Check the result and save --------------------------------------------

# WHAT: the list of columns the later scripts need.
# HOW:  c() makes a list of column names.
keep <- c("cafe_name", "student", "district", "district_A4", "cafe_type",
          "is_roastery", "espresso", "cappuccino", "flat_white", "filter",
          "price_level", "above_median_price", "seats_inside", "seats_outside",
          "has_outdoor", "small_cafe", "rating", "n_ratings", "top_rated",
          "dist_orloj", "lat", "lon")

# WHAT: the final clean table with only those columns.
# HOW:  all rows (nothing before the comma), the 'keep' columns after it.
dataset <- clean[, keep]

# WHAT: rows before and after cleaning (299 -> 195).
# HOW:  nrow() counts rows; ';' lets two commands share one line.
nrow(raw); nrow(dataset)

# WHAT: missing values per variable in the final data.
# HOW:  is.na() + colSums(), as in step 2.
colSums(is.na(dataset))

# WHAT: min, quartiles, median, mean and max of the main numeric variables,
#       to check that the ranges are plausible.
# HOW:  summary() gives these six numbers for each column.
summary(dataset[, c("seats_inside", "seats_outside", "price_level",
                    "rating", "n_ratings", "dist_orloj")])

# WHAT: plausibility checks. If any one fails, the script stops with an error
#       instead of saving wrong data.
# HOW:  stopifnot() stops if any condition is FALSE; all(..., na.rm = TRUE)
#       asks "is this true for every cafe?", ignoring missing values.
#       Ratings between 1 and 5, no negative seats, prices between 20 and 300 CZK.
stopifnot(all(dataset$rating >= 1 & dataset$rating <= 5, na.rm = TRUE),
          all(dataset$seats_inside  >= 0, na.rm = TRUE),
          all(dataset$seats_outside >= 0, na.rm = TRUE),
          all(dataset$price_level > 20 & dataset$price_level < 300, na.rm = TRUE))

# WHAT: saves the clean data as analysis/dataset.csv, the ONLY file that
#       02_descriptive.R and 03_analysis.R read.
# HOW:  write.csv() writes a table to a CSV file; row.names = FALSE leaves out
#       R's row numbers, as the brief asks.
write.csv(dataset, "analysis/dataset.csv", row.names = FALSE)
