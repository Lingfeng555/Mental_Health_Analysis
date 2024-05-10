rm(list = ls())
source("Init/BuildModel.R")

Load_Libraries <- function(packages){
  newpack  = packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(newpack)) install.packages(newpack)
  lapply(packages, library, character.only=TRUE)
}

Load_Libraries(c("dplyr", "faraway", "FactoMineR", "plotrix"))

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
                        GDP_Per_Capita 
                        , MENTAL_HEALTH)
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
# Removed Life_Spectancy
# Removed Read
# Removed Science
# Removed Freedom
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

MALE_SUICIDE_MODEL <- lm(Suicide_Male ~ Generosity +
                      GDP_Per_Capita +
                      IQ +
                      Math, MENTAL_HEALTH)

anova(MALE_SUICIDE_MODEL)


#Standarize the main dataset
std_mental_health <- MENTAL_HEALTH[, 2:(ncol(MENTAL_HEALTH)-3)] %>% scale
std_mental_health %>% summary

pca0 <- PCA(X = std_mental_health, scale.unit = TRUE)
pca0

summary(pca0)


pca0$eig

# individual (coordinates)
pca0$ind$coord

# variables (correlations)
pca0$var$coord

plot(pca0,choix="ind")
plot(pca0,choix="var")

# view each individual's contribution to each principle component:
pca0$ind$contrib
# verify each principle component's contributions sum up to 100%:
colSums(pca0$ind$contrib)


# cos2 is the squared cosine for each principle component, calculated as (Dim.X/Dist)^2. 
# The closer it is to 1 for a given principle component, 
# the better that principle component is at capturing all the characteristics 
# of that individual.
(pca0$ind$coord/pca0$ind$dist)^2  #=cos2 for individuals
#For a variable cos2 is the square of the correlation:
(pca0$var$cor)^2 #=cos2 for variables





# Create factors with specified levels for Math, Science, and Read variables
MENTAL_HEALTH$Math <- factor(MENTAL_HEALTH$Math)
MENTAL_HEALTH$Science <- factor(MENTAL_HEALTH$Science)
MENTAL_HEALTH$Read <- factor(MENTAL_HEALTH$Read)

# Create a copy of the original dataset
MENTAL_HEALTH_Categorical <- MENTAL_HEALTH[, c( "Math", "Science", "Read", "Schizophrenia", "Bipolar", "Anxiety", "Eating_Disorders", "Depression", "Alcohol_Related_Disorders","Drug_Related_Disorders", "Suicide_Male", "Suicide_Female" )]

# Calculate the average value for each disorder column
avg_disorders <- colMeans(MENTAL_HEALTH[, c("Schizophrenia", "Bipolar", "Anxiety", "Eating_Disorders", 
                                            "Depression", "Alcohol_Related_Disorders", "Drug_Related_Disorders", "Suicide_Male", "Suicide_Female")], na.rm = TRUE)

# Convert the original values to "yes" or "no" based on whether they are above or below the average
MENTAL_HEALTH_Categorical$Schizophrenia <- factor(ifelse(MENTAL_HEALTH$Schizophrenia > avg_disorders["Schizophrenia"], "yes", "no"))
MENTAL_HEALTH_Categorical$Bipolar <- factor(ifelse(MENTAL_HEALTH$Bipolar > avg_disorders["Bipolar"], "yes", "no"))
MENTAL_HEALTH_Categorical$Anxiety <- factor(ifelse(MENTAL_HEALTH$Anxiety > avg_disorders["Anxiety"], "yes", "no"))
MENTAL_HEALTH_Categorical$Eating_Disorders <- factor(ifelse(MENTAL_HEALTH$Eating_Disorders > avg_disorders["Eating_Disorders"], "yes", "no"))
MENTAL_HEALTH_Categorical$Depression <- factor(ifelse(MENTAL_HEALTH$Depression > avg_disorders["Depression"], "yes", "no"))
MENTAL_HEALTH_Categorical$Drug_Related_Disorders <- factor(ifelse(MENTAL_HEALTH$Drug_Related_Disorders > avg_disorders["Drug_Related_Disorders"], "yes", "no"))
MENTAL_HEALTH_Categorical$Alcohol_Related_Disorders <- factor(ifelse(MENTAL_HEALTH$Alcohol_Related_Disorders > avg_disorders["Alcohol_Related_Disorders"], "yes", "no"))
MENTAL_HEALTH_Categorical$Suicide_Male <- factor(ifelse(MENTAL_HEALTH$Suicide_Male > avg_disorders["Suicide_Male"], "yes", "no"))
MENTAL_HEALTH_Categorical$Suicide_Female <- factor(ifelse(MENTAL_HEALTH$Suicide_Female > avg_disorders["Suicide_Female"], "yes", "no"))


# Create vectors for row names and column names
row_names <- c("Math_VeryLow", "Math_Low", "Math_High", "Math_VeryHigh", 
               "Read_VeryLow", "Read_Low", "Read_High", "Read_VeryHigh",
               "Science_VeryLow", "Science_Low", "Science_High", "Science_VeryHigh")

column_names <- c("Depression-yes", "Depression-no", 
                  "Anxiety-yes", "Anxiety-no", 
                  "Bipolar-yes", "Bipolar-no", 
                  "Eating_Disorders-yes", "Eating_Disorders-no", 
                  "Drug_Related_Disorders-yes", "Drug_Related_Disorders-no", 
                  "Alcohol_Related_Disorders-yes", "Alcohol_Related_Disorders-no", 
                  "Schizophrenia-yes", "Schizophrenia-no", 
                  "Suicide_Male-yes", "Suicide_Male-no", 
                  "Suicide_Female-yes", "Suicide_Female-no")

# Create an empty contingency table
contingency_table <- matrix(0, nrow = length(row_names), ncol = length(column_names), dimnames = list(row_names, column_names))

# Loop through each row in the dataframe
for (i in 1:length(row_names)) {
  stringVariable <- row_names[i]
  split_strings <- strsplit(stringVariable, "_")[[1]]
  variable <- split_strings[1]
  level <- split_strings[2]
  for (j in 1:length(column_names)){
    stringDisorder <- column_names[j]
    split_strings <- strsplit(stringDisorder, "-")[[1]]
    disorder <- split_strings[1]
    yesNo <- split_strings[2]
    quantity <- sum(MENTAL_HEALTH_Categorical[, variable] == level & MENTAL_HEALTH_Categorical[, disorder] == yesNo)
    contingency_table[i, j] <- quantity
  }
}

# Print the contingency table
print(contingency_table)







