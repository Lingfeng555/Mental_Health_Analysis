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
# ----------------------------------------------------------------------------------------------------------------------------
 print("BEFORE DEPRESSION_MODEL")
 anova(DEPRESSION_MODEL)
 DEPRESSION_MODEL <- lm(Depression ~ Life_Expectancy +
                  Gov_Corruption +
                  Generosity +
                  GDP_Per_Capita +
                  Math, MENTAL_HEALTH)
 print("AFTER DEPRESSION_MODEL")
# anova(DEPRESSION_MODEL)
# 
# We can notice that there is a Low F- value of Science and Read, which means that there is a low relation between these variables and Depression
# Suprinsingly there is a very low F value between IQ and Depression which means there may be no relationship between these variables

# ----------------------------------------------------------------------------------------------------------------------------
# Removed Generosity
HAPPINESS_MODEL <- lm(Happiness ~ Life_Expectancy + 
                  Freedom + 
                  Gov_Corruption + 
                  GDP_Per_Capita , MENTAL_HEALTH) 
anova(HAPPINESS_MODEL)

# 1. By looking at coefficients at first the only coefficient that seems significant is the Life_Expectancy
# 2. After looking at the anova test with all the variables, we can see that the F-value is very low for Generosity as well as having a confidence interval that is out of expectations.
# 3. Another variable that we will be removing is Science due to similar reasons as Generosity.
# 4. Finally the only other variable that will be removed will be IQ due to similar reasons as Science and Generosity.
# 5. All variables are the perception of the citizens of the country (ej. High Freedom = Satisfied with the freedom in the country, 
# High Gov_Corruption = Many people perceives that the government is corrupt)

# ----------------------------------------------------------------------------------------------------------------------------
# ANXIETY_MODEL <- lm(Anxiety ~ Life_Expectancy + 
#                       Freedom + 
#                       Gov_Corruption + 
#                       Generosity + 
#                       GDP_Per_Capita + 
#                       IQ + 
#                       Math + Science + Read, MENTAL_HEALTH)
# We remove all the variables which P-values are above 0.15

# Removed Science
# Removed Read
# Removed Math
# Removed Generosity
# Removed IQ
# Removed Freedom

ANXIETY_MODEL <- lm(Anxiety ~ Life_Expectancy + 
                      Gov_Corruption + 
                      GDP_Per_Capita, MENTAL_HEALTH)

anova(ANXIETY_MODEL)

# ----------------------------------------------------------------------------------------------------------------------------
FEMALE_SUICIDE_MODEL <- lm(Suicide_Female ~ Gov_Corruption + 
                             Generosity + 
                             GDP_Per_Capita + 
                             IQ + 
                             Math, MENTAL_HEALTH)
anova(FEMALE_SUICIDE_MODEL)

# ----------------------------------------------------------------------------------------------------------------------------
# MALE_SUICIDE_MODEL <- lm(Suicide_Male ~ Life_Expectancy + 
#                       Freedom + 
#                       Gov_Corruption + 
#                       Generosity + 
#                       GDP_Per_Capita + 
#                       IQ + 
#                       Math + Science + Read, MENTAL_HEALTH)
# We remove all the variables which P-values are above 0.15

# Removed Science
# Removed Read
# Removed Gov_Corruption
# Removed Generosity
# Removed GDP_Per_Capita
# Removed Freedom
# Removed Life_Expectancy

MALE_SUICIDE_MODEL <- lm(Suicide_Male ~ IQ +
                           Math, MENTAL_HEALTH)

anova(MALE_SUICIDE_MODEL)
