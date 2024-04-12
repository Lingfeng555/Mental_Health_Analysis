if( length(ls()) == 0 ){
  source("init/DataCleaner.R")
}

Load_Libraries <- function(packages){
  newpack  = packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(newpack)) install.packages(newpack)
  lapply(packages, library, character.only=TRUE)
}

Load_Libraries(c("dplyr", "faraway"))

normalize <- function(data) {
  if(is.character(data)){
    return(data)
  }else{
    return((data - min(data)) / (max(data) - min(data)))
  }
}

MERGE <- function(){
  #We will only consider 2017
  happiness <- happiness[happiness$Year == 2017,]
  happiness$Year <- NULL
  happiness$GDP_Capita <- NULL
  colnames(happiness)[2] <- "Happiness"
  
  GDP_Per_Capita <- data.frame(
    Country = GDP_Per_Capita$Country,
    GDP_Per_Capita = GDP_Per_Capita$Y2017
  )
  
  Iq_Per_Country <- data.frame(
    Country = Iq_Per_Country$Country,
    IQ = as.numeric(Iq_Per_Country$Iq_byLynnBecker),
    Math = Iq_Per_Country$Pisa2022Math,
    Science = Iq_Per_Country$Pisa2022Science,
    Read = Iq_Per_Country$Pisa2022Read
  )
  
  Mental_Disorders <- Mental_Disorders[Mental_Disorders$Year==2017,]
  Mental_Disorders$Year <- NULL
  
  ret <- merge(happiness, GDP_Per_Capita, by = "Country")
  ret <- merge(ret, Mental_Disorders, by = "Country")
  ret <- merge(ret, Suicide_Per_Country, by ="Country")
  ret <- merge(ret, Iq_Per_Country, by = "Country")
  
  ret[,2:(ncol(ret)-3)] <- apply(ret[,2:(ncol(ret)-3)], 2, normalize)
  ret$Bipolar <- c(ret$Bipolar > mean(ret$Bipolar))
  return(ret)
}

MENTAL_HEALTH <- MERGE()
#rm(GDP_Per_Capita,happiness,Iq_Per_Country, Mental_Disorders, Suicide_Per_Country)

rm(list = ls()[which(ls()!="MENTAL_HEALTH")])

DEPRESSION_MODEL <- lm(Depression ~ Life_Expectancy + 
                                    Freedom + 
                                    Gov_Corruption + 
                                    Generosity + 
                                    GDP_Per_Capita + 
                                    IQ + 
                                    Math + Science + Read, MENTAL_HEALTH)
HAPPINESS_MODEL <- lm(Happiness ~ Life_Expectancy + 
                                  Freedom + 
                                  Gov_Corruption + 
                                  Generosity + 
                                  GDP_Per_Capita + 
                                  IQ + 
                                  Math + Science + Read, MENTAL_HEALTH)

ANXIETY_MODEL <- lm(Anxiety ~ Life_Expectancy + 
                              Freedom + 
                              Gov_Corruption + 
                              Generosity + 
                              GDP_Per_Capita + 
                              IQ + 
                              Math + Science + Read, MENTAL_HEALTH)

FEMALE_SUICIDE_MODEL <- lm(Suicide_Female ~ Life_Expectancy + 
                                      Freedom + 
                                      Gov_Corruption + 
                                      Generosity + 
                                      GDP_Per_Capita + 
                                      IQ + 
                                      Math + Science + Read, MENTAL_HEALTH)

MALE_SUICIDE_MODEL <- lm(Suicide_Male ~ Life_Expectancy + 
                             Freedom + 
                             Gov_Corruption + 
                             Generosity + 
                             GDP_Per_Capita + 
                             IQ + 
                             Math + Science + Read, MENTAL_HEALTH)

anova(DEPRESSION_MODEL)
anova(HAPPINESS_MODEL)
anova(ANXIETY_MODEL)
anova(FEMALE_SUICIDE_MODEL)
anova(MALE_SUICIDE_MODEL)
