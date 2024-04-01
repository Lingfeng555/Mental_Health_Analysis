#Check if there is any global variables loaded, if not if call the script to load them
setwd(as.character(getwd()))

if( length(ls()) == 0 ){
  source(paste(as.character(getwd()), "/LoadDatasets.R", sep = ""))
}

Load_Libraries <- function(packages){
  newpack  = packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(newpack)) install.packages(newpack)
  lapply(packages, library, character.only=TRUE)
}

Load_Libraries(c("forecast", "dplyr", "data.table"))

normalize <- function(data) ((data - min(data)) / (max(data) - min(data)))

process_Hapiness <- function(rawHappiness){
  # Remove unnecesary columns (Rank)
  rawHappiness <- cbind(rawHappiness[,1:2], rawHappiness[,4:10])
  
  # Normalize data columns (All columns except country and year)
  rawHappiness[,3:9] <- apply(rawHappiness[,3:9], 2, normalize)
  
  rawHappiness
}

process_Iq <- function(rawIq){
  
  rawIq$Pisa2022Math <- ifelse(Iq_Per_Country$Pisa2022Math <= 390, "Very low",
                                        ifelse(Iq_Per_Country$Pisa2022Math <= 440, "Low",
                                        ifelse(Iq_Per_Country$Pisa2022Math <= 484, "High", 
                                        ifelse(Iq_Per_Country$Pisa2022Math <= 576, "Very High", 0))))
  
  rawIq$Pisa2022Read <- ifelse(Iq_Per_Country$Pisa2022Read <= 385, "Very low",
                                        ifelse(Iq_Per_Country$Pisa2022Read <= 438, "Low",
                                        ifelse(Iq_Per_Country$Pisa2022Read <= 481, "High", 
                                        ifelse(Iq_Per_Country$Pisa2022Read <= 544, "Very High", 0))))
  
  rawIq$Pisa2022Science <- ifelse(Iq_Per_Country$Pisa2022Science <= 405, "Very low",
                                        ifelse(Iq_Per_Country$Pisa2022Science <= 448, "Low",
                                        ifelse(Iq_Per_Country$Pisa2022Science <= 495, "High", 
                                        ifelse(Iq_Per_Country$Pisa2022Science <= 562, "Very High", 0))))

  rawIq[is.na(Iq_Per_Country)] <- "Unknown"
  # Normalize data columns
  rawIq[,2] <- apply(Iq_Per_Country[, "Iq_byLynnBecker", drop = FALSE], 2, normalize)
  
  rawIq
}

process_Disorders <- function(rawDisorders){
  
  # Normalize data columns (All columns except country and year)
  rawDisorders[,3:9] <- apply(rawDisorders[,3:9], 2, normalize)
  
  rawDisorders
}

NA_Rows_Replace <- function (rawGDP){
  na_rows <- which(rowSums(is.na(rawGDP)) == (ncol(rawGDP)-1)) #The column of the country names does not count
  if (length(na_rows) > 0) {
    rawGDP[na_rows, 2:ncol(rawGDP)] <- 0
  }
  rawGDP
}

Traspose_DataFrame <- function(dataframe){
  t_dataframe <- t(dataframe)
  t_dataframe <- as.data.frame(dataframe)
  return(t_dataframe)
}

Build_Model <- function(vector){
  temp <- data.frame(
    year = 2013:2021,
    GDP_Per_Capita = vector
  )
  temp <- temp[!is.na(temp$GDP_Per_Capita),]
  lm(GDP_Per_Capita~year,temp)
}

Interpolate_Na <- function (rawGDP){
  na_index <- which(is.na(rawGDP$Y2021))
  na_missing <- rawGDP[na_index,] # All missing values has at least a NA at 2021
  years <- 2013:2021
  for(i in 1:nrow(na_missing)){
    GDP_vector <- na_missing[i, 2:ncol(na_missing)] #Get the vector of GDB per capita
    model <- as.numeric(GDP_vector) %>% Build_Model #Build a lineal regression of each country
    missing_year <- years[is.na(as.numeric(GDP_vector))] #Get the missing years
    rawGDP[na_index[i], missing_year - 2013 + 2] <- predict(model, data.frame(year = missing_year))  #Fill with approximations
  }
  rawGDP
} 

process_GDP <- function(rawGDP){
  rawGDP <- rawGDP %>% NA_Rows_Replace %>% Interpolate_Na
  rawGDP[,2:ncol(rawGDP)] <- apply(rawGDP[,2:ncol(rawGDP)], 2, normalize)
  rawGDP 
}

process_Suicide <- function(rawSuicide){
  rawSuicide 
}

happiness <- process_Hapiness(happiness)
Iq_Per_Country <- process_Iq(Iq_Per_Country)
Mental_Disorders <- process_Disorders(Mental_Disorders)
GDP_Per_Capita <- process_GDP(GDP_Per_Capita)
Suicide_Per_Country <- process_Suicide(Suicide_Per_Country)
