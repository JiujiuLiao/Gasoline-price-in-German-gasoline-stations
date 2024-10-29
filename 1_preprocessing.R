setwd("D:/TU/DataProjectGasStations/testfolder/")
Sys.setenv(LANG="en")

library(tidyr)
library(zoo)

# QGIS information added to raw data
stations <- read.csv("SA_gas_stations.csv")

# raw price data
prices_full <- rbind(
  read.csv("2024-02-12-prices.csv"), read.csv("2024-02-13-prices.csv"),
  read.csv("2024-02-14-prices.csv"), read.csv("2024-02-15-prices.csv"))

# selecting only the Saxon gas stations
prices_full <- prices_full[prices_full$station_uuid %in% stations$uuid,]

prices_full[,"date"] <- as.POSIXct(prices_full[,"date"])


###### cleaning stations data ##################################################

# changing names and adapting character formatting
stations$krs_name_s <- gsub(pattern = "['", replacement = "", 
                                stations$krs_name_s, fixed = TRUE)
stations$krs_name_s <- gsub(pattern = "']", replacement = "", 
                                stations$krs_name_s, fixed = TRUE)

stations$settlement[stations$settlement == "Landkreis"] <- "Rural"
stations$settlement[stations$settlement == "Stadtkreis"] <- "Urban"


###### cleaning the brands column ##############################################

replace_station_name <- function(current_name, new_name){
  A$brand[which(A$brand %in% current_name)] <- new_name
  return(A$brand)
}

A <- stations
brand_table_before <- as.data.frame(table(A$brand))

# all the variants of free gas stations in the bft club 
# (which is not a company, but for a model we can treat as one)
A$brand <- replace_station_name(unique(grep(pattern = "bft", x = A$brand, 
                                            ignore.case = TRUE, value = TRUE)),
                                "bft")

# variants of AVIA
A$brand <- replace_station_name(unique(grep(pattern = "avia", x = A$brand, 
                                            ignore.case = TRUE, value = TRUE)),
                                "AVIA")

#ARAL
A$brand <- replace_station_name(unique(grep(pattern = "aral", x = A$brand, 
                                            ignore.case = TRUE, value = TRUE)),
                                "ARAL")

#Shell
A$brand <- replace_station_name(unique(grep(pattern = "shell", x = A$brand, 
                                            ignore.case = TRUE, value = TRUE)),
                                "Shell")

#ESSO - in this case grep includes other brands and there is only one variant
A$brand <- replace_station_name("Esso","ESSO")

#TotalEnergies
A$brand <- replace_station_name(unique(grep(pattern = "total", x = A$brand, 
                                            ignore.case = TRUE, value = TRUE)),
                                "TotalEnergies")

# AGIP ENI
A$brand <- replace_station_name(unique(grep(pattern = "agip", x = A$brand, 
                                            ignore.case = TRUE, value = TRUE)),
                                "AGIP ENI")

# Raiffeisen
A$brand <- replace_station_name(unique(grep(pattern = "raiffeisen", x = A$brand, 
                                            ignore.case = TRUE, value = TRUE)),
                                "Raiffeisen")
A[A$name %in% grep(pattern = "raiffeisen", x = A$name, 
                   ignore.case = TRUE, value = TRUE), "brand"] <- "Raiffeisen"

# orlen is part of STAR
A$brand <- replace_station_name(unique(grep(pattern = "orlen", x = A$brand, 
                                            ignore.case = TRUE, value = TRUE)),
                                "STAR")

# tamoil and hem are the same company
A$brand <- replace_station_name(unique(grep(pattern = "tamoil", x = A$brand, 
                                            ignore.case = TRUE, value = TRUE)),
                                "HEM")

# OIL!
A$brand <- replace_station_name(unique(grep(pattern = "oil!", x = A$brand, 
                                            ignore.case = TRUE, value = TRUE)),
                                "OIL!")

# variations on free gas station names
A$brand <- replace_station_name(unique(grep(pattern = "frei", x = A$brand, 
                                            ignore.case = TRUE, value = TRUE)),
                                "Freie Tankstelle")
A[A$name %in% grep(pattern = "freie", x = A$name, ignore.case = TRUE, 
                   value = TRUE), "brand"] <- "Freie Tankstelle"
A[A$brand == "", "brand"] <- "Freie Tankstelle"

# Westfalen
A$brand <- replace_station_name(unique(grep(pattern = "westfalen", x = A$brand, 
                                            ignore.case = TRUE, value = TRUE)),
                                "Westfalen")

# Classic
A$brand <- replace_station_name(unique(grep(pattern = "classic", x = A$brand, 
                                            ignore.case = TRUE, value = TRUE)),
                                "CLASSIC")

# varaiations on supermarket gas stations
A$brand <- replace_station_name(unique(grep(pattern = "supermarkt", x = A$brand, 
                                            ignore.case = TRUE, value = TRUE)),
                                "Supermarkt-Tankstelle")

# Hoyer
A$brand <- replace_station_name(unique(grep(pattern = "hoyer", x = A$brand, 
                                            ignore.case = TRUE, value = TRUE)),
                                "Hoyer")

# EDEKA
A$brand <- replace_station_name(unique(grep(pattern = "edeka", x = A$brand, 
                                            ignore.case = TRUE, value = TRUE)),
                                "EDEKA")

# Q1
A$brand <- replace_station_name(unique(grep(pattern = "q1", x = A$brand, 
                                            ignore.case = TRUE, value = TRUE)),
                                "Q1")

# ELAN
A$brand <- replace_station_name(unique(grep(pattern = "elan", x = A$brand, 
                                            ignore.case = TRUE, value = TRUE)),
                                "ELAN")

# Orlen
A$brand <- replace_station_name(unique(grep(pattern = "orlen", x = A$brand, 
                                            ignore.case = TRUE, value = TRUE)),
                                "ORLEN")

# PM
A$brand <- replace_station_name(unique(grep(pattern = "pm", x = A$brand, 
                                            ignore.case = TRUE, value = TRUE)),
                                "PM")

# markant
A$brand <- replace_station_name(unique(grep(pattern = "markant", x = A$brand, 
                                            ignore.case = TRUE, value = TRUE)),
                                "Markant")

# NORDOEL
A$brand <- replace_station_name(unique(grep(pattern = "nordoel", x = A$brand, 
                                            ignore.case = TRUE, value = TRUE)),
                                "NORDOEL")

# Globus
A$brand <- replace_station_name(unique(grep(pattern = "globus", x = A$brand, 
                                            ignore.case = TRUE, value = TRUE)),
                                "Globus")


brand_table_after <- as.data.frame(table(A$brand))

###### merging smallest brands to reduce number of factors #####################

brand_combined <- brand_table_after[brand_table_after$Freq > 20,]
A$brand_combined[A$brand %in% brand_combined$Var1] <- A$brand[A$brand %in% brand_combined$Var1]
A$brand_combined[!A$brand %in% brand_combined$Var1] <- "11-20 station brand"
brand_combined <- brand_table_after[brand_table_after$Freq > 10,]
A$brand_combined[!A$brand %in% brand_combined$Var1] <- "1-10 station brand"

brand_table_comb <- as.data.frame(table(A$brand_combined))

A$brand <- A$brand_combined
A <- A[,-which(colnames(A) == "brand_combined")]

stations <- A

rm(A,brand_table_before, brand_combined, brand_table_after, brand_table_comb)


###### calculating average prices per hour #####################################

as.numeric(min(prices_full[,"date"]))
strftime(1707692400)

average_price <- function(id, fueltype){
  #initializing variables to be used later
  averages <- NA
  last_price <- NA
  testing <- NA
  
  #creating short df with only the specified station for quicker runtime
  prices <- prices_full[prices_full$station_uuid == id,]
  
  #iterating over 96 steps one for each hour over four days
  for (i in 1:96){
    averages[i] <- NA
    # We add 3600 sec (1 hour) times the iterated step to get a start and end 
    # time for the period we look at
    starter <- 1707692400 + (i-1)*3600
    end <- starter + 3600
    # cutting the dataframe to only show price changes within the selected hour 
    within_price_changes <- prices[prices$station_uuid == id &
                                     prices$date <= end &
                                     prices$date >= starter,]
    
    # very first price to be set, before this price we have no information for 
    # the prices of the given gas station
    if (nrow(within_price_changes) != 0 & is.na(last_price)){
      last_price <- within_price_changes[within_price_changes[,"date"] == 
                                           max(within_price_changes[,"date"]),
                                         fueltype]
    }
    
    # after a first price has been set there are different cases for what can 
    # happen
    else if (is.na(last_price) == FALSE){
      # case 1: the entire hour goes by without another change in price. The 
      # average is the last price that was set.
      if (nrow(within_price_changes) == 0){
        averages[i] <- last_price
      }
      # case 2: one price has been set, old and new price are averaged according
      # to their fraction of the hour
      else if (nrow(within_price_changes) == 1){
        averages[i] <- as.numeric(((
          as.numeric(within_price_changes[1,"date"]) - starter)) * 
            last_price + (end - as.numeric(within_price_changes[1,"date"])) * 
            within_price_changes[1,fueltype]) / 3600
      }
      # case 3: more than one price has been set. They are all averaged 
      # according to their fraction of the hour
      else{
        
        # fraction of the price that was set before the start of the hour
        first_part <-  ((as.numeric(within_price_changes[1,"date"]) - starter) *
                          last_price ) / 3600
        
        # fractions of the prices set within the hour
        other_parts <- NA
        for (j in 1:(nrow(within_price_changes))){
          
          # last price to be set
          if (j == nrow(within_price_changes)){
            other_parts[j] <- ((end - 
                                  as.numeric(within_price_changes[j,"date"])) * 
                                 within_price_changes[j,fueltype]) / 3600
          }
          # all the other prices. in general they follow the formula 
          # ((t3 - t2)*p2)/3600, where p2 is the second price set, t2 is the 
          # time its set and t3 is the time the third price is set
          else {
            other_parts[j] <- ((as.numeric(within_price_changes[(j+1),"date"]) -
                                  as.numeric(within_price_changes[j,"date"])) * 
                                 within_price_changes[j,fueltype] ) / 3600
          }
        }
        #summing up the fractions
        averages[i] <- sum(c(first_part, other_parts))
      }
    }
  }
  return(averages)
}

###### Diesel ##################################################################
# here we calculate the average prices for diesel, this same process is used 
# again for E5 and E10

hourly_averages <- data.frame(unique(prices_full$station_uuid))
colnames(hourly_averages)[1] <- "uuid"

for (i in 1:length(unique(prices_full$station_uuid))) {
  hourly_averages[i, 2:97] <- average_price(prices_full$station_uuid[i],"diesel") 
  
  if (i %% 150 == 0) {print(paste(
    "1/3 is ",
    round((1 - (length(unique(prices_full$station_uuid))-i)/
             length(unique(prices_full$station_uuid))) * 100
          , digits = 0)
    , "% done", sep = "" ))}
}

A <- NA
for (i in 1:96){
  A[i] <- 1707692400 + (i-1)*3600
  }
colnames(hourly_averages)[2:97] <- A
rm(A)

hourly_averages_long_diesel <- pivot_longer(hourly_averages, !uuid, 
                                            names_to = "time", values_to = "price")
colnames(hourly_averages_long_diesel)[3] <- "diesel_price"


###### E5 ######################################################################
rm(hourly_averages)
hourly_averages <- data.frame(unique(prices_full$station_uuid))
colnames(hourly_averages)[1] <- "uuid"


for (i in 1:length(unique(prices_full$station_uuid))) {
  hourly_averages[i, 2:97] <- average_price(prices_full$station_uuid[i],"e5") 
  
  if (i %% 150 == 0) {print(paste(
    "2/3 is ",
    round((1 - (length(unique(prices_full$station_uuid))-i)/
          length(unique(prices_full$station_uuid))) * 100
          , digits = 0)
    , "% done", sep = "" ))}
}


A <- NA
for (i in 1:96){
  A[i] <- 1707692400 + (i-1)*3600
}
colnames(hourly_averages)[2:97] <- A
rm(A)

hourly_averages_long_e5 <- pivot_longer(hourly_averages, !uuid, 
                                        names_to = "time", values_to = "price")
colnames(hourly_averages_long_e5)[3] <- "e5_price"

###### E10 #####################################################################
rm(hourly_averages)
hourly_averages <- data.frame(unique(prices_full$station_uuid))
colnames(hourly_averages)[1] <- "uuid"


for (i in 1:length(unique(prices_full$station_uuid))) {
  hourly_averages[i, 2:97] <- average_price(prices_full$station_uuid[i],"e10") 
  
  if (i %% 150 == 0) {print(paste(
    "3/3 is ",
    round((1 - (length(unique(prices_full$station_uuid))-i)/
             length(unique(prices_full$station_uuid))) * 100
          , digits = 0)
    , "% done", sep = "" ))}
}


A <- NA
for (i in 1:96){
  A[i] <- 1707692400 + (i-1)*3600
}
colnames(hourly_averages)[2:97] <- A
rm(A)

hourly_averages_long_e10 <- pivot_longer(hourly_averages, !uuid, 
                                         names_to = "time", values_to = "price")
colnames(hourly_averages_long_e10)[3] <- "e10_price"


###### merge the three #########################################################

hourly_averages_long <- merge(x = hourly_averages_long_diesel,
                              y = hourly_averages_long_e5 ,by=c("uuid","time"))
hourly_averages_long <- merge(x = hourly_averages_long,
                              y = hourly_averages_long_e10 ,by=c("uuid","time"))

# cut off only the three days that are used, the other was included for the
# prices in the night between the 12th and 13th
hourly_averages_long$time <- as.numeric(hourly_averages_long$time)
strftime(min(hourly_averages_long$time))

hourly_averages_long <- hourly_averages_long[hourly_averages_long$time>=1707778800,]

strftime(min(hourly_averages_long$time))

rm(hourly_averages, hourly_averages_long_diesel, hourly_averages_long_e10, 
   hourly_averages_long_e5, prices_full)

# replacing 0 at stations that only sell diesel with NAs
hourly_averages_long$e5_price[na.fill(hourly_averages_long$e5_price == 0, FALSE)] <- NA
hourly_averages_long$e10_price[na.fill(hourly_averages_long$e10_price == 0, FALSE)] <- NA


###### adding crude oil prices #################################################

# taken from https://www.boerse-frankfurt.de/commodity/wti-rohoel

# day     open    close  
#15/02/24	76.416	78.1692
#14/02/24	77.8563	76.6142
#13/02/24	77.4165	77.8119
#12/02/24	76.7113	77.0451
#09/02/24	76.3255	76.5625
#08/02/24	74.3028	76.4498

hourly_averages_long$time <- as.POSIXct(hourly_averages_long$time)
hourly_averages_long$crude_oil_price <- 76.4498
hourly_averages_long$crude_oil_price[hourly_averages_long$time > as.POSIXct("2024-02-09 08:00:00")] <- 76.3255 
hourly_averages_long$crude_oil_price[hourly_averages_long$time > as.POSIXct("2024-02-09 22:00:00")] <- 76.5625
hourly_averages_long$crude_oil_price[hourly_averages_long$time > as.POSIXct("2024-02-12 08:00:00")] <- 76.7113
hourly_averages_long$crude_oil_price[hourly_averages_long$time > as.POSIXct("2024-02-12 22:00:00")] <- 77.0451
hourly_averages_long$crude_oil_price[hourly_averages_long$time > as.POSIXct("2024-02-13 08:00:00")] <- 77.4165
hourly_averages_long$crude_oil_price[hourly_averages_long$time > as.POSIXct("2024-02-13 22:00:00")] <- 77.8119
hourly_averages_long$crude_oil_price[hourly_averages_long$time > as.POSIXct("2024-02-14 08:00:00")] <- 77.8563
hourly_averages_long$crude_oil_price[hourly_averages_long$time > as.POSIXct("2024-02-14 22:00:00")] <- 76.6142
hourly_averages_long$crude_oil_price[hourly_averages_long$time > as.POSIXct("2024-02-15 08:00:00")] <- 76.416
hourly_averages_long$crude_oil_price[hourly_averages_long$time > as.POSIXct("2024-02-15 22:00:00")] <- 78.1692

hourly_averages_long$time <- as.numeric(hourly_averages_long$time)


###### adding the frequency measure ############################################

prices <- rbind(
  read.csv("2024-02-13-prices.csv"), read.csv("2024-02-14-prices.csv"), 
  read.csv("2024-02-15-prices.csv")
)

A <- table(prices$station_uuid,prices$dieselchange)
B <- as.data.frame(A[,2])
A <- table(prices$station_uuid,prices$e5change)
B <- cbind(B,A[,2])
A <- table(prices$station_uuid,prices$e10change)
B <- cbind(B,A[,2])

colnames(B) <- c("diesel", "e5", "e10")

B$freq <- B$diesel + B$e5 + B$e10
B$uuid <- rownames(B)

B <- B[,c(4,5)]
stations <- merge(stations, B, by = "uuid")

rm(A, B, prices)


###### merging averages and stations ###########################################

aggregated_data <- merge(hourly_averages_long, stations, by = "uuid")
rm(stations, hourly_averages_long)

# drop rows with any missing values
aggregated_data <- aggregated_data[complete.cases(aggregated_data), ]      


Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME","English")

aggregated_data[, "time"] <- as.POSIXct(aggregated_data[, "time"])

aggregated_data$day <- strftime(aggregated_data[, "time"], format = "%a")
unique(aggregated_data$day)

aggregated_data$hour <- strftime(aggregated_data[, "time"], format = "%H")

aggregated_data <- aggregated_data[,
                                   -which(colnames(aggregated_data) %in% 
                                            c("uuid","time","region",
                                              "n_city"))]

# selecting the four biggest brands only 
aggregated_data <- aggregated_data[aggregated_data$brand %in% 
                                     c("ARAL","TotalEnergies","Shell","STAR"),]

write.csv(x = aggregated_data, file = "aggregated_data_Saxony_13-15_big_brands.csv")
