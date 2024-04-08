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

#Removed Science
#Removed Read
#Removed Freedom
#DEPRESSION_MODEL <- lm(Depression ~ Life_Expectancy + 
#                         Freedom + 
#                         Gov_Corruption + 
#                         Generosity + 
#                         GDP_Per_Capita + 
#                         IQ + 
#                         Math + Science + Read, MENTAL_HEALTH)
print("BEFORE DEPRESSION_MODEL")
anova(DEPRESSION_MODEL)
DEPRESSION_MODEL <- lm(Depression ~ Life_Expectancy + 
                 Gov_Corruption + 
                 Generosity + 
                 GDP_Per_Capita + 
                 Math, MENTAL_HEALTH) 
print("AFTER DEPRESSION_MODEL")
anova(DEPRESSION_MODEL)

# We can notice that there is a Low F- value of Science and Read, which means that there is a low relation between these variables and Depression
# Suprinsingly there is a very low F value between IQ and Depression which means there may be no relationship between these variables

print("BEFORE DEPRESSION_MODEL")
anova(DEPRESSION_MODEL)
DEPRESSION_MODEL <- lm(Depression ~ Life_Expectancy + 
                         Gov_Corruption + 
                         Generosity + 
                         GDP_Per_Capita + 
                         Math, MENTAL_HEALTH) 
print("AFTER DEPRESSION_MODEL")
anova(DEPRESSION_MODEL)
