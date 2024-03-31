#Check if there is any global variables loaded, if not if call the script to load them
if( length(ls()) == 0 ){
  source(paste(as.character(getwd()), "/LoadDatasets.R", sep = ""))
}

Load_Libraries <- function(packages){
  newpack  = packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(newpack)) install.packages(newpack)
  lapply(packages, library, character.only=TRUE)
}

c("forecast") %>% Load_Libraries

normalize <- function(data) ((data - min(data)) / (max(data) - min(data)))

process_Hapiness <- function(rawHappiness){
  # Remove unnecesary columns (Rank)
  rawHappiness <- cbind(rawHappiness[,1:2], rawHappiness[,4:10])
  
  # Normalize data columns (All columns except country and year)
  rawHappiness[,3:9] <- apply(rawHappiness[,3:9], 2, normalize)
  
  rawHappiness
}

process_Iq <- function(rawIq){
  rawIq
}

process_Disorders <- function(rawDisorders){
  
  # Normalize data columns (All columns except country and year)
  rawDisorders[,3:9] <- apply(rawDisorders[,3:9], 2, normalize)
  
  rawDisorders
}

Interpolate_Na <- function (rawGDP){
  na_missing <- which(is.na(rawGDP$Y2021))
  rawGDP
} 

NA_Rows_Replace <- function (rawGDP){
  na_rows <- which(rowSums(is.na(rawGDP)) == (ncol(rawGDP)-1)) #The column of the country names does not count
  if (length(na_rows) > 0) {
    rawGDP[na_rows, 2:ncol(rawGDP)] <- 0
  }
  rawGDP
}

process_GDP <- function(rawGDP){
  rawGDP <- rawGDP %>% NA_Rows_Replace %>% Interpolate_Na
  rawGDP 
}

process_Suicide <- function(rawSuicide){
  rawSuicide 
}

clean_Datasets <- function(){
  happiness <- process_Hapiness(happiness)
  Iq_Per_Country <- process_Iq(Iq_Per_Country)
  Mental_Disorders <- process_Disorders(Mental_Disorders)
  GDP_Per_Capita <- process_GDP(GDP_Per_Capita)
  Suicide_Per_Country <- process_Suicide(Suicide_Per_Country)
}

clean_Datasets()
