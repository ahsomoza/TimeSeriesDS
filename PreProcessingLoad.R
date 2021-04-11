library(forecast)
library(tseries)
library(ggplot2)
library(dplyr)


#setwd("/Users/teresa/UPM/MASTER/TIME SERIES/R/")
#data<-read.csv("/Users/pablodonisebri/Clase/Master/SegundoCuatri/TimeSeries/Proyecto/time_series_60min.csv")
data <- read.csv("time_series_60min.csv", header=TRUE) #, as.is=TRUE, na.strings="-1")


################## FILTERS ####################
##### filter columns ##########
#We are interested in the columns that contain load_actual in their names and the timestamp (utc_timestamp)
#As there are some countries with more than one column: "DE_50hertz_load_actual_entsoe_transparency", "DE_LU_load_actual_entsoe_transparency",
#"DE_amprion_load_actual_entsoe_transparency", "DE_tennet_load_actual_entsoe_transparency", "DE_transnetbw_load_actual_entsoe_transparency"
# We are only interested in column names like: letter letter + "_load_actual_entsoe_transparency"
filtered <- dplyr::select(data, c("utc_timestamp",dplyr::matches("\\b[A-Z][A-Z]_load_actual_entsoe_transparency")))
#We check that the regex pattern worked
colnames(filtered)

#### filter rows ############
#We dropped the first row that contains all null values, and allow us to start in 2015
filtered<-filtered[-1,]
#We will delete all the entries from the year 2020 as it has been an atypical year.
filtered <- subset(filtered, startsWith(filtered$utc_timestamp, "201"))
#We check NA's for all the countries
summary(filtered)


#####
#IDEAS FOR NULL TREATMENT:
#- Countries with many NA's --> deleted (e.g. UA, CY)
#- Countries with problematic NA's --> deleted (LT,LU,ME,RS)
#As we want to calculate the average per day:
#- If a day has less and equal than 5 NA's, we do not care about these NA's, we compute the average with the rest of the values
#- If a day has more than 6 NA's: impute those values BUT take into account 
#   - that if the NA's are from the beggining of the inputs, how it should be done (e.g. 2015-01-02 or sth alike)
#   - if the TS has trend it can not be replace by the values of the past year
####


###### filter columns #########

#We will drop Cyprus and Ukraine because they have too many NA's (17469 and 24765 respectively)
#filtered <- subset(filtered, -c("UA_load_actual_entsoe_transparency", "CY_load_actual_entsoe_transparency"))
filtered$UA_load_actual_entsoe_transparency <- NULL
filtered$CY_load_actual_entsoe_transparency <- NULL
filtered$LT_load_actual_entsoe_transparency <- NULL
filtered$LU_load_actual_entsoe_transparency <- NULL
filtered$ME_load_actual_entsoe_transparency <- NULL
filtered$RS_load_actual_entsoe_transparency <- NULL

#checking for missing data
#sum(is.na(filtered))
summary(filtered)

#To see the dates that each country has as null values
for (row in 1:nrow(filtered)){
  if(is.na(filtered$BG_load_actual_entsoe_transparency[row])){
    print(filtered$utc_timestamp[row])
  }
}

################### NULL VALUES ################
############# imputing null values #########
filtered$utc_timestamp <- as.Date(filtered$utc_timestamp) # type of the column changed from character to
#- If a day has more than 6 NA's: impute those values BUT take into account 
# that if the NA's are from the beggining of the inputs, how it should be done (e.g. 2015-01-02 or sth alike)

imputingNulls <- function(df){
  # Select the days with more than 5 null values
  print(strsplit(colnames(df)[2],"_")[[1]][1])
  subset(aggregate(x = df[,2], by = list(df$utc_timestamp), function(x) {sum(is.na(x))}), subset=x>5)
  }

for (col in 2:length(filtered)){
  print(imputingNulls(filtered[c(1,col)]))
  print("")
}


for (row in 1:nrow(filtered)){
  if(is.na(filtered$BG_load_actual_entsoe_transparency[row])){
    print(filtered$utc_timestamp[row])
  }
}
summary(filtered)

################### HOURLY TO DAILY DATA FRAME ################
hourlyToDailyAverage <- function(df){
  setNames(aggregate(x = df[,2],by = list(df$utc_timestamp), FUN = mean, na.rm=TRUE), c("date",strsplit(colnames(df)[2],"_")[[1]][1]))
}

hourlyDF <- hourlyToDailyAverage(filtered[c("utc_timestamp","AT_load_actual_entsoe_transparency")])

for (col in 3:length(filtered)){
  hourlyDF <- merge(hourlyDF, hourlyToDailyAverage(filtered[c(1,col)]), by="date")
}
summary(hourlyDF)

hourlyDF[, "month"] <- format(hourlyDF[,"date"], "%Y%m")
countries <- names(hourlyDF)[2:26]
for (country in countries){
  meanMonthDF <- aggregate(x = hourlyDF[country],by = list(hourlyDF$month), FUN = mean, na.rm=TRUE)
  for (row in 1:nrow(hourlyDF)){
    if(is.na(hourlyDF[row, country])){
      date_toreplace <- format(hourlyDF[row, "date"], "%Y%m")
      hourlyDF[row, country] = meanMonthDF[meanMonthDF[,1] == date_toreplace,country]
    }
  }
}
hourlyDF$month <- NULL
summary(hourlyDF)

write.csv(hourlyDF, "hourlyDF.csv")
