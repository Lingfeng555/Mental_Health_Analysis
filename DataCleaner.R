#Check if there is any global variables loaded, if not if call the script to load them
if( length(ls()) == 0 ){
  source(paste(as.character(getwd()), "/LoadDatasets.R", sep = ""))
}

process_Hapiness <- function(rawHappiness){
  rawHappiness
}

process_Iq <- function(rawIq){
  rawIq
}

process_Disorders <- function(rawDisorders){
  rawDisorders
}

process_Pib <- function(rawPib){
  rawPib
}

process_Suicide <- function(rawSuicide){
  rawSuicide
}

Clean <- function(){
  happiness <- process_Hapiness(happiness)
  Iq_Per_Country <- process_Iq(Iq_Per_Country)
  Mental_Disorders <- process_Disorders(Mental_Disorders)
  Pib_Per_Country <- process_Pib(Pib_Per_Country)
  Suicide_Per_Country <- process-Suicide(Suicide_Per_Country)
}

Clean()