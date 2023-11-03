

oil <- read_csv("01_data/01_raw/oil.csv")
# Skip the first 4 rows
oil <- oil[-(1:4), ]

# Separate the column into two using the comma as a separator
# This step assumes that you have a column named 'V1' that contains the data to split.
oil <- separate(oil, col = names(oil), into = c("date", "oil_price"), sep = ",")

# Convert 'date' to an actual Date type if necessary
oil$date <- as.Date(oil$date, format = "%m/%d/%Y")

# Convert 'oil_price' to numeric if necessary
oil$oil_price <- as.numeric(oil$oil_price)

oil$log_oil_price <- log(oil$oil_price)

# Define the start and end dates
start_date <- as.Date("2014-01-01")
end_date <- as.Date("2016-12-31")

# Generate the sequence of dates
date_sequence <- seq(from = start_date, to = end_date, by = "day")

oil <- data.frame("date" = date_sequence)  %>% left_join(oil, by = c("date"))

# Find the index of the first non-NA value
first_non_na <- which(!is.na(oil$oil_price))[1]

# Find the index of the last non-NA value
last_non_na <- which(!is.na(oil$oil_price))[length(which(!is.na(oil$oil_price)))]

# Subset the dataframe to keep only the rows between the first and last non-NA values
oil <- oil[first_non_na:last_non_na, ]

oil$oil_price <- na.approx(oil$oil_price)
oil$log_oil_price <- na.approx(oil$log_oil_price)

oil <- oil %>% mutate(diff_oil_price = oil_price - lag(oil_price),
                      diff_log_oil_price = log_oil_price - lag(log_oil_price))

saveRDS(oil, file = "01_data/02_processed/cleaned_oil.rds")
