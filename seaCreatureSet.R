install.packages("zoo")
install.packages("data.table")
require(zoo)
require(data.table)

#read in raw data (data.table)
df <- fread("C:/Users/Me/Desktop/ArcticCreatureAnalysis/seabird_mammal_raw_data.csv")
df
is.data.table(df)

#ensure date column exists
df[, date := as.Date(paste(YEAR, MONTH, "01", sep = "-"))]

#aggregate ABUNDANCE by GENUS_SPECIES and date
agg_data <- df[, .(ABUNDANCE = sum(ABUNDANCE)), by = .(date, GENUS_SPECIES)]

#reshape data to wide format (GENUS_SPECIES as columns)
wide_data <- dcast(agg_data, date ~ GENUS_SPECIES, value.var = "ABUNDANCE", 
                   fill = NA)

#generate a full sequence of monthly dates
full_dates <- seq(from = as.Date("1993-01-01"), to = as.Date("2011-12-01"), 
                  by = "month")

# Merge full date range with dataset to fill missing months
wide_data <- merge(data.table(date = full_dates), wide_data, by = "date", 
                   all.x = TRUE)

# Handle missing values (Choose one)
wide_data[is.na(wide_data)] <- 0  # Option 1: Fill missing months with 0
# wide_data <- na.locf(wide_data)  # Option 2: Forward-fill 
                                   #(last observation carried forward)
# wide_data <- na.approx(wide_data)  # Option 3: Linear interpolation

# Convert to zoo object (Now it is regular)
zoo_obj <- zoo(wide_data[, -1, with = FALSE], order.by = wide_data$date)

# Convert to ts (If fully regular, i.e., all months filled)
mts_obj <- ts(coredata(zoo_obj), start = c(1993, 1), frequency = 12)
mts_obj

# Ensure date column exists (First day of the month)
df[, date := as.Date(paste(YEAR, MONTH, "01", sep = "-"))]  

# Aggregate total ABUNDANCE by GENUS_SPECIES and date
agg_data <- df[, .(ABUNDANCE = sum(ABUNDANCE)), by = .(date, GENUS_SPECIES)]

# Reshape data to wide format (GENUS_SPECIES as columns)
wide_data <- dcast(agg_data, date ~ GENUS_SPECIES, value.var = "ABUNDANCE", fill = 0)

# Generate full sequence of monthly dates
full_dates <- seq(from = as.Date("1993-01-01"), to = as.Date("2011-12-01"), by = "month")

# Merge full date range with dataset to fill missing months
wide_data <- merge(data.table(date = full_dates), wide_data, by = "date", all.x = TRUE)

# Convert to zoo object
zoo_obj <- zoo(wide_data[, -1, with = FALSE], order.by = wide_data$date)

# Convert to mts (Ensure regular time steps: monthly frequency)
mts_obj <- ts(coredata(zoo_obj), start = c(1993, 1), frequency = 12)

plot(mts_obj[, "Adelie penguin"])

# Convert mts to zoo for interpolation
zoo_obj <- zoo(mts_obj, order.by = time(mts_obj))

# Apply linear interpolation to each column
zoo_interpolated <- na.approx(zoo_obj)

#rounded mts
zoo_interpolated <- round(zoo_interpolated)

# Convert back to mts after interpolation
mts_interpolated <- ts(coredata(zoo_interpolated), start = c(1993, 1), frequency = 12)

class(mts_interpolated)
mts_interpolated[, "Antarctic petrel"]

plot(mts_interpolated[, "Antarctic fur-seal"])

#histogram
hist(mts_interpolated[, "Adelie penguin"], 30)

#differenced histogram
hist(diff(mts_interpolated[, "Adelie penguin"]), 30)

#Plot data stricly from 1993
yearSection <- window(mts_interpolated, start = c(1995, 1), end = c(1995, 12))
yearSection[, "Antarctic petrel"]
plot(yearSection[, "Antarctic petrel"])


#pearson correlation1
correlation <- cor(mts_interpolated[, "Antarctic fur-seal"], 
                   mts_interpolated[, "Adelie penguin"], method = 'pearson')

#pearson correlation2
correlation <- cor(mts_interpolated[, "Antarctic petrel"], 
                   mts_interpolated[, "Antarctic tern"], method = 'pearson')
correlation