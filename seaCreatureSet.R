install.packages("zoo")
install.packages("data.table")
install.packages("tseries")
install.packages("ggplot2")
install.packages("arrow")
require(arrow)
require(tseries)
require(zoo)
require(data.table)
require(ggplot2)

#read in raw data (data.table)
df <- fread("C:/Users/Me/Desktop/ArcticDataAnalysis/seabird_mammal_raw_data.csv")
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
wide_data[is.na(wide_data)] <- 0

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
#full_dates <- seq(from = as.Date("1993-01-01"), to = as.Date("2011-12-01"), by = "month")
full_dates <- seq(from = as.Date("1993-01-01"), to = as.Date("2011-12-01"), by = "year")


# Merge full date range with dataset to fill missing months
wide_data <- merge(data.table(date = full_dates), wide_data, by = "date", all.x = TRUE)

# Convert to zoo object
zoo_obj <- zoo(wide_data[, -1, with = FALSE], order.by = wide_data$date)

# Convert to mts (Ensure regular time steps)
mts_obj <- ts(coredata(zoo_obj), start = c(1993, 1), frequency = 1)

# Convert mts to zoo for interpolation
zoo_obj <- zoo(mts_obj, order.by = time(mts_obj))

# Apply linear interpolation to each column
#zoo_interpolated <- na.approx(zoo_obj)

#round interpolated abundances to whole numbers
#zoo_interpolated <- round(zoo_interpolated)

# Convert back to mts after interpolation
#mts_interpolated <- ts(coredata(zoo_interpolated), start = c(1993, 1), frequency = 12)


#----------RAW DATA PLOTS----------
plot(mts_obj[, "Adelie penguin"], ylab = "Abundance", 
     main = "Adelie Penguin Sightings (1993-2011)", type = "p")
plot(mts_obj[, "Antarctic petrel"], ylab = "Abundance", 
     main = "Antarctic Petrel Sightings (1993-2011)",type = "p")
plot(mts_obj[, "Antarctic prion"], ylab = "Abundance", 
     main = "Antarctic Prion Sightings (1993-2011)", type = "p")
plot(mts_obj[, "Antarctic tern"], ylab = "Abundance", 
     main = "Antarctic Tern Sightings (1993-2011)",type = "p")
plot(mts_obj[, "Black-Browed albatross"], ylab = "Abundance", 
     main = "Black-Browed Albatross Sightings (1993-2011)",type = "p")
plot(mts_obj[, "Black-Belled storm-petrel"], ylab = "Abundance", 
     main = "Black-Belled Storm-Petrel Sightings (1993-2011)",type = "p")


#----------STATIONARITY TESTING----------
#Augmented Dickey-Fuller Stationarity tests on RAW data
adf.test(mts_obj[, "Adelie penguin"])
adf.test(mts_obj[, "Antarctic petrel"])
adf.test(mts_obj[, "Antarctic prion"])
adf.test(mts_obj[, "Antarctic tern"])
adf.test(mts_obj[, "Black-Browed albatross"])
adf.test(mts_obj[, "Black-Belled storm-petrel"])

#Augmented Dickey-Fuller Stationarity tests on Differenced data
adf.test(diff(mts_obj[, "Adelie penguin"]))
adf.test(diff(mts_obj[, "Antarctic petrel"]))
adf.test(diff(mts_obj[, "Antarctic prion"]))
adf.test(diff(mts_obj[, "Antarctic tern"]))
adf.test(diff(mts_obj[, "Black-Browed albatross"]))
adf.test(diff(mts_obj[, "Black-Belled storm-petrel"]))


#----------AUTOCORRELATION----------
acf(mts_obj[, "Adelie penguin"])
acf(mts_obj[, "Antarctic petrel"])
acf(mts_obj[, "Antarctic prion"])
acf(mts_obj[, "Antarctic tern"])
acf(mts_obj[, "Black-Browed albatross"])
acf(mts_obj[, "Black-Belled storm-petrel"])


#----------PEARSON CORRELATION----------
#Pearson correlation calculations (raw)
penvpet <- cor(mts_obj[, "Adelie penguin"], mts_obj[, "Antarctic petrel"], 
               method = 'pearson')
penvprion <- cor(mts_obj[, "Adelie penguin"], mts_obj[, "Antarctic prion"], 
               method = 'pearson')
penvtern <- cor(mts_obj[, "Adelie penguin"], mts_obj[, "Antarctic tern"], 
               method = 'pearson')
penvalbatross <- cor(mts_obj[, "Adelie penguin"], 
                     mts_obj[, "Black-Browed albatross"], method = 'pearson')
penvstormpet <- cor(mts_obj[, "Adelie penguin"], 
                    mts_obj[, "Black-Belled storm-petrel"], method = 'pearson')

petvprion <- cor(mts_obj[, "Antarctic petrel"], mts_obj[, "Antarctic prion"], 
               method = 'pearson')
petvtern <- cor(mts_obj[, "Antarctic petrel"], mts_obj[, "Antarctic tern"], 
                 method = 'pearson')
petvalbatross <- cor(mts_obj[, "Antarctic petrel"], 
                     mts_obj[, "Black-Browed albatross"], method = 'pearson')
petvstormpet <- cor(mts_obj[, "Antarctic petrel"], 
                    mts_obj[, "Black-Belled storm-petrel"], method = 'pearson')

prionvtern <- cor(mts_obj[, "Antarctic prion"], 
                  mts_obj[, "Antarctic tern"], method = 'pearson')
prionvalbatross <- cor(mts_obj[, "Antarctic prion"], 
                  mts_obj[, "Black-Browed albatross"], method = 'pearson')
prionvstormpet <- cor(mts_obj[, "Antarctic prion"], 
                  mts_obj[, "Black-Belled storm-petrel"], method = 'pearson')

ternvalbatross <- cor(mts_obj[, "Antarctic tern"], 
                      mts_obj[, "Black-Browed albatross"], method = 'pearson')
ternvstormpet <- cor(mts_obj[, "Antarctic tern"], 
                      mts_obj[, "Black-Belled storm-petrel"], method = 'pearson')

albatrossvstormpet <- cor(mts_obj[, "Black-Browed albatross"], 
                          mts_obj[, "Black-Belled storm-petrel"], 
                          method = 'pearson')

#Pearson correlation matrix (raw)
corrMatrix <- matrix(c(0,penvpet,penvprion,penvtern,penvalbatross,penvstormpet,
                       penvpet, 0, petvprion, petvtern, petvalbatross, 
                       petvstormpet, penvprion,petvprion,0,prionvtern,
                       prionvalbatross,prionvstormpet,penvtern,petvtern,
                       prionvtern,0,ternvalbatross,ternvstormpet,penvalbatross,
                       petvalbatross,prionvalbatross,ternvalbatross,0,
                       albatrossvstormpet,penvstormpet,petvstormpet,
                       prionvstormpet,ternvstormpet,albatrossvstormpet,0), 
                     nrow = 6, ncol = 6)

colnames(corrMatrix) <- c("Adelie Penguin", "Antarctic Petrel", 
                          "Antarctic Prion", "Antarctic Tern", 
                          "Black-Browed Albatross", "Black-Belled Storm-Petrel")
rownames(corrMatrix) <- c("Adelie Penguin", "Antarctic Petrel", 
                          "Antarctic Prion", "Antarctic Tern", 
                          "Black-Browed Albatross", "Black-Belled Storm-Petrel")


#Pearson correlation calculations (differenced)
penvpetD <- cor(diff(mts_obj[, "Adelie penguin"]), diff(mts_obj[, "Antarctic petrel"]), 
               method = 'pearson')
penvprionD <- cor(diff(mts_obj[, "Adelie penguin"]), diff(mts_obj[, "Antarctic prion"]), 
                 method = 'pearson')
penvternD <- cor(diff(mts_obj[, "Adelie penguin"]), diff(mts_obj[, "Antarctic tern"]), 
                method = 'pearson')
penvalbatrossD <- cor(diff(mts_obj[, "Adelie penguin"]), 
                     diff(mts_obj[, "Black-Browed albatross"]), method = 'pearson')
penvstormpetD <- cor(diff(mts_obj[, "Adelie penguin"]), 
                    diff(mts_obj[, "Black-Belled storm-petrel"]), method = 'pearson')

petvprionD <- cor(diff(mts_obj[, "Antarctic petrel"]), diff(mts_obj[, "Antarctic prion"]), 
                 method = 'pearson')
petvternD <- cor(diff(mts_obj[, "Antarctic petrel"]), diff(mts_obj[, "Antarctic tern"]), 
                method = 'pearson')
petvalbatrossD <- cor(diff(mts_obj[, "Antarctic petrel"]), 
                     diff(mts_obj[, "Black-Browed albatross"]), method = 'pearson')
petvstormpetD <- cor(diff(mts_obj[, "Antarctic petrel"]), 
                    diff(mts_obj[, "Black-Belled storm-petrel"]), method = 'pearson')

prionvternD <- cor(diff(mts_obj[, "Antarctic prion"]), 
                  diff(mts_obj[, "Antarctic tern"]), method = 'pearson')
prionvalbatrossD <- cor(diff(mts_obj[, "Antarctic prion"]), 
                       diff(mts_obj[, "Black-Browed albatross"]), method = 'pearson')
prionvstormpetD <- cor(diff(mts_obj[, "Antarctic prion"]), 
                      diff(mts_obj[, "Black-Belled storm-petrel"]), method = 'pearson')

ternvalbatrossD <- cor(diff(mts_obj[, "Antarctic tern"]), 
                      diff(mts_obj[, "Black-Browed albatross"]), method = 'pearson')
ternvstormpetD <- cor(diff(mts_obj[, "Antarctic tern"]), 
                     diff(mts_obj[, "Black-Belled storm-petrel"]), method = 'pearson')

albatrossvstormpetD <- cor(diff(mts_obj[, "Black-Browed albatross"]), 
                          diff(mts_obj[, "Black-Belled storm-petrel"]), 
                          method = 'pearson')

#Pearson correlation matrix (differenced)
corrMatrixD <- matrix(c(0,penvpetD,penvprionD,penvternD,penvalbatrossD,penvstormpetD,
                       penvpetD, 0, petvprion, petvternD, petvalbatrossD, 
                       petvstormpetD, penvprionD,petvprionD,0,prionvternD,
                       prionvalbatrossD,prionvstormpetD,penvternD,petvternD,
                       prionvternD,0,ternvalbatrossD,ternvstormpetD,penvalbatrossD,
                       petvalbatrossD,prionvalbatrossD,ternvalbatrossD,0,
                       albatrossvstormpetD,penvstormpetD,petvstormpetD,
                       prionvstormpetD,ternvstormpetD,albatrossvstormpetD,0), 
                       nrow = 6, ncol = 6)

colnames(corrMatrixD) <- c("Adelie Penguin", "Antarctic Petrel", 
                          "Antarctic Prion", "Antarctic Tern", 
                          "Black-Browed Albatross", "Black-Belled Storm-Petrel")
rownames(corrMatrixD) <- c("Adelie Penguin", "Antarctic Petrel", 
                          "Antarctic Prion", "Antarctic Tern", 
                          "Black-Browed Albatross", "Black-Belled Storm-Petrel")


#----------RANKED FREQUENCY PLOT----------
#group desired species
species <- c("Adelie penguin", "Antarctic petrel", "Antarctic prion",
             "Antarctic tern", "Black-Browed albatross", 
             "Black-Belled storm-petrel")

#create mts object for a specific year
dataSubset <- mts_obj[, species]
yearData <- window(dataSubset, c(1996), end = c(1996))
end(yearData)
totalAbundances <- colSums(yearData, na.rm = TRUE)
rankedData <- data.frame(Species = names(totalAbundances), 
                         Abundance = totalAbundances)

#order species by greatest to least abundance
rankedData <- rankedData[order(-rankedData$Abundance), ]

#display ranked frequency plot
barplot(rankedData$Abundance, 
       names.arg = rankedData$Species, 
       las = 1,            # Rotate labels
       col = "brown", 
       main = paste("Ranked Frequency Plot for 1996"),
       xlab = "Species", 
       ylab = "Abundance")

plot(diff(mts_obj[, "Adelie penguin"]), type = "p")
adf.test(diff(diff(mts_obj[, "Adelie penguin"])))



#----------AR MODEL FITTING----------
mts_frame <- data.frame(mts_obj)

plot(mts_frame[["Adelie penguin"]], type = "l")


fcFrame <- fread("C:/Users/Me/Desktop/ArcticDataAnalysis/Daily_Demand_Forecasting_Orders.csv")

plot(fcFrame[["Fiscal sector orders"]], type = "l")
plot(pacf(fcFrame[["Banking orders (2)"]])) #banking orders

fit <- ar(fcFrame[["Banking orders (2)"]], method = "mle")
fit

#Autoregressive Integrated Moving Average of order 3 (ARIMA(3))
est <- arima(x = fcFrame[["Banking orders (2)"]], order = c(3, 0, 0))
plot(fcFrame[["Banking orders (2)"]], type = "l") #banking orders

