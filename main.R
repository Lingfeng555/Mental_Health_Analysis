rm(list = ls())
source("Init/BuildModel.R")

Load_Libraries <- function(packages){
  newpack  = packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(newpack)) install.packages(newpack)
  lapply(packages, library, character.only=TRUE)
}

Load_Libraries(c("dplyr", "faraway"))

# Default Model Building

# Y_MODEL <- lm(Y ~ Life_Expectancy + 
#                  Freedom + 
#                  Gov_Corruption + 
#                  Generosity + 
#                  GDP_Per_Capita + 
#                  IQ + 
#                  Math + Science + Read, MENTAL_HEALTH) Categories...

# Model Selection

#Removed Life_Expectancy
#Removed Read
#Removed Science
ANXIETY_MODEL <- lm(Anxiety ~ Freedom + 
                      Gov_Corruption + 
                      Generosity + 
                      GDP_Per_Capita + 
                      IQ + 
                      Math , MENTAL_HEALTH)

# Plot