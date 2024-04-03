#Check if there is any global variables loaded, if not if call the script to load them
if( length(ls()) == 0 ){
  source("Init/LoadDatasets.R")
}

Load_Libraries <- function(packages){
  newpack  = packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(newpack)) install.packages(newpack)
  lapply(packages, library, character.only=TRUE)
}

Load_Libraries(c("forecast", "dplyr", "data.table"))

process_Hapiness <- function(rawHappiness){
  # Remove unnecesary columns (Rank)
  rawHappiness <- cbind(rawHappiness[,1:2], rawHappiness[,4:10])
  
  rawHappiness
}

process_Iq <- function(rawIq){
  
  rawIq <- rawIq[order(rawIq$Country),]
  
  rawIq$Pisa2022Math <- ifelse(rawIq$Pisa2022Math <= 390, "Very low",
                                        ifelse(rawIq$Pisa2022Math <= 440, "Low",
                                        ifelse(rawIq$Pisa2022Math <= 484, "High", 
                                        ifelse(rawIq$Pisa2022Math <= 576, "Very High", 0))))
  
  rawIq$Pisa2022Read <- ifelse(rawIq$Pisa2022Read <= 385, "Very low",
                                        ifelse(rawIq$Pisa2022Read <= 438, "Low",
                                        ifelse(rawIq$Pisa2022Read <= 481, "High", 
                                        ifelse(rawIq$Pisa2022Read <= 544, "Very High", 0))))
  
  rawIq$Pisa2022Science <- ifelse(rawIq$Pisa2022Science <= 405, "Very low",
                                        ifelse(rawIq$Pisa2022Science <= 448, "Low",
                                        ifelse(rawIq$Pisa2022Science <= 495, "High", 
                                        ifelse(rawIq$Pisa2022Science <= 562, "Very High", 0))))
  
  rawIq <- rawIq[!is.na(rawIq$Iq_byLynnBecker),]
  
  rawIq[is.na(rawIq)] <- "Unknown"
  # Normalize data columns
  
  rawIq$Iq_byLynnBecker <- c(rawIq$Iq_byLynnBecker %>% as.numeric)
  
  rawIq
}

process_Disorders <- function(rawDisorders){
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
  rawGDP 
}

process_Suicide <- function(rawSuicide){
  rawSuicide <- rawSuicide[rawSuicide$Gender != "Total" & rawSuicide$Year == 2017 & rawSuicide$Age_Range == "Total years",]
  rawSuicide <- rawSuicide[order(rawSuicide$Country), ]
  
  Suicide_Male <- rawSuicide[rawSuicide$Gender == "Male", "Number"]
  Suicide_Female <- rawSuicide[rawSuicide$Gender == "Female", "Number"]
  
  rawSuicide <- rawSuicide[rawSuicide$Gender != "Male",]
  
  rawSuicide$Age_Range <- NULL
  rawSuicide$Upper <- NULL
  rawSuicide$Lower <- NULL
  rawSuicide$Year <- NULL
  rawSuicide$Gender <- NULL
  rawSuicide$Number <- NULL
  
  rawSuicide$Suicide_Male <- c(Suicide_Male)
  rawSuicide$Suicide_Female <- c(Suicide_Female)
  
  rawSuicide
}

#Process all datasets
happiness <- process_Hapiness(happiness)
Iq_Per_Country <- process_Iq(Iq_Per_Country)
Mental_Disorders <- process_Disorders(Mental_Disorders)
GDP_Per_Capita <- process_GDP(GDP_Per_Capita)
Suicide_Per_Country <- process_Suicide(Suicide_Per_Country)
