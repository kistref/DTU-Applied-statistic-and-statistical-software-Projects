#author: Zuzanna Czelusniak,
 #       Thomas Aamand Witting,
  #       Zoltán György Varga

#### Import necessary libraries ####
library(car)
library(data.table)
library(dplyr)

library(stargazer)

#### Prepare WUnderground data ####

## (a) Read the RData file
load("WUndergroundHourly.RData")

# See summary and str to get an overview of the data
summary(WG)
str(WG)

# Replace empty strings with NAs
WG$dir[WG$dir==""] <- NA
WG$cond[WG$cond==""] <- NA

# Convert categorical columns to R factors
WG$dir <- as.factor(WG$dir)
WG$cond <- as.factor(WG$cond)
WG$fog <- as.factor(WG$fog)
WG$rain <- as.factor(WG$rain)
WG$snow <- as.factor(WG$snow)


## (b) Exclude columns with pure NAs or fixed values
excludes <- c("wind_gust", "wind_chill", "heat_index", "precip", "precip_rate", "precip_total", "hail", "thunder", "tornado")
WG <- WG[ , -which(names(WG) %in% excludes)]

## (c) For each day calculate:
#     1. The mean value for continuous variables
WG.agg.cont <- aggregate(Filter(is.numeric, WG), list(date=as.Date(WG$date)), mean, na.rm =TRUE, na.action = na.pass)

#     2. The mode of the factor variables
# Create mode function for factor variables. Ignores NAs
fact.mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

# Compute mode
WG.agg.fact <- aggregate(Filter(is.factor, WG), list(date=as.Date(WG$date)), fact.mode)

# (d) Aggregate your results into a single dataframe and set all times to 00:00:00
# Merge
WG.clean <- merge(WG.agg.cont, WG.agg.fact, by = "date")
WG.clean$date <- as.POSIXct(WG.clean$date, format="%d-%m-%Y %H:%M:%S")

# Set time to 00:00:00
times <- strptime(WG.clean$date, "%Y-%m-%d %H:%M:%S")
h.str <- as.numeric(format(times, "%H")) +
  as.numeric(format(times, "%M"))/60

WG.clean$date <- WG.clean$date - h.str * 60 * 60

#### Prepare meter data ####
# (a) Get an overview of the file structure in 'meterdata.zip'
# Prefix      = "HTK_Dag_"
# Suffix      = "_40.txt"
# Complete    = Prefix + ddmmyyyy + Suffix
# No files in a different format than above.
# Regex       = "*.txt"

# (b) Read all data into a single dataframe
# (c) Keep only columns 1, 2 and 4 ("ID", "Time" and "Reading") and convert to proper class
files <- dir('meterdata/', pattern = "*.txt", full.names = TRUE)
HTK <- data.table(do.call("rbind", lapply(files, fread, sep=";", select=c(1,2,4))))
setnames(HTK, c("id", "time", "reading"))
HTK$id <- as.factor(HTK$id)
HTK$time <- as.POSIXct(HTK$time, format="%d-%m-%Y %H.%M")
HTK$reading <- as.numeric(sub(",", ".", HTK$reading, fixed = TRUE))

# (d) Exclude meters with less than 121 records to avoid long gaps
HTK <- HTK %>% group_by(id) %>% filter(n() >= 121)

# (e) Calculate consumption for each building
# Interpolate a daily reading at 11:59PM
times.inter <- c(WG.clean$date + (24*60*60 - 60))

HTK <- data.table(HTK)
HTK.itp <- HTK[, approx(time, reading, xout=times.inter, ties=min), by=id]
setnames(HTK.itp, "x", "time")
setnames(HTK.itp, "y", "reading")

# Compute consumption
HTK.itp[ , consumption := reading - shift(reading), by = id] 

# Remove rows with na
HTK.itp <- na.exclude(HTK.itp)
# Remove reading, it is no longer relevant
HTK.itp <- subset(HTK.itp, select=-c(reading))

# (f) Merge the meter and WUnderground dataframes
# Create "date" column in HTK.itp that matches with WG.clean$date, used as the merge column
HTK.itp$date <- as.POSIXct(as.Date(HTK.itp$time), format="%d-%m-%Y")
times <- strptime(HTK.itp$date, "%Y-%m-%d %H:%M:%S")
h.str <- as.numeric(format(times, "%H")) + as.numeric(format(times, "%M"))/60
HTK.itp$date <- HTK.itp$date - h.str * 60 * 60

# Perform merge, key column = "date". Outer join
HTK.WG <- merge(HTK.itp, WG.clean, by = "date", all = TRUE)
HTK.WG <- na.exclude(HTK.WG)
# Remove "snow" as it is only 0s. Remove "time" as it is no longer relevant
HTK.WG <- subset(HTK.WG, select=-c(snow, time))

# (g) Include summary() of your merged dataframe in your report and compare with the version that is handed out
summary(HTK.WG)

# Reorder columns to match
HTK.WG <- HTK.WG[,c(1:7,10,8,9,11:13)]

HTK.WG.handout <- read.csv("merged_data.csv", header = TRUE)
HTK.WG.handout$date <- as.POSIXct(HTK.WG.handout$date)
HTK.WG.handout$ID <- as.factor(HTK.WG.handout$ID)
HTK.WG.handout$dir <- as.factor(HTK.WG.handout$dir)
HTK.WG.handout$cond <- as.factor(HTK.WG.handout$cond)
HTK.WG.handout$fog <- as.factor(HTK.WG.handout$fog)
HTK.WG.handout$rain <- as.factor(HTK.WG.handout$rain)

summary(HTK.WG.handout)

# (h) Include the number of rows in your data.frame and the number of remaining meters in your report
nrow(HTK.WG)
nlevels(factor(HTK.WG$id))
nlevels(factor(HTK.WG.handout$ID))

# (i) Include your code for merging the data as an appendix

#### Statistical Analysis ####






X <- diag(1, 10, 10)


X * 



