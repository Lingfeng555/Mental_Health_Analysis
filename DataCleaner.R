#Check if there is any global variables loaded, if not if call the script to load them
if( length(ls()) == 0 ){
  source(paste(as.character(getwd()), "/LoadDatasets.R", sep = ""))
}

normalize <- function(data){
  (data - min(data)) / (max(data) - min(data))
}

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

process_Pib <- function(rawPib){
  rawPib
}

process_Suicide <- function(rawSuicide){
  rawSuicide
}

clean_Datasets <- function(){
  happiness <- process_Hapiness(happiness)
  Iq_Per_Country <- process_Iq(Iq_Per_Country)
  Mental_Disorders <- process_Disorders(Mental_Disorders)
  Pib_Per_Country <- process_Pib(Pib_Per_Country)
  Suicide_Per_Country <- process_Suicide(Suicide_Per_Country)
}

clean_Datasets()