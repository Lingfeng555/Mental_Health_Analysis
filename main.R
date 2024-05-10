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




# CORRESPONDENCE ANALYSIS

# Create factors with specified levels for Math variable
MENTAL_HEALTH$Math <- factor(MENTAL_HEALTH$Math)

# Create a copy of the original dataset with relevant columns
MENTAL_HEALTH_Categorical <- MENTAL_HEALTH[, c( "Math","Depression")]

# Define breaks for categorizing Depression variable into 5 levels
breaks <- quantile(MENTAL_HEALTH$Depression, probs = seq(0, 1, by = 0.2), na.rm = TRUE)

# Convert numerical values to categorical strings
MENTAL_HEALTH_Categorical$Depression <- cut(MENTAL_HEALTH$Depression, breaks = breaks, labels = c("VeryLow", "Low", "Medium", "High", "VeryHigh"), include.lowest = TRUE)

# Convert the categorical strings to factors
MENTAL_HEALTH_Categorical$Depression <- factor(MENTAL_HEALTH_Categorical$Depression, levels = c("VeryLow", "Low", "Medium", "High", "VeryHigh"))

# Create vectors for row names and column names
row_names <- c("Math_Unknown","Math_VeryLow", "Math_Low", "Math_High", "Math_VeryHigh")

column_names <- c("Depression-VeryLow", "Depression-Low","Depression-Medium","Depression-High", "Depression-VeryHigh")

# Create a contingency table
contingency_table <- table(MENTAL_HEALTH_Categorical$Math, MENTAL_HEALTH_Categorical$Depression)

# Create observed contingency table
observed_table <- contingency_table

# Calculate expected contingency table under independence
expected_table <- outer(rowSums(observed_table), colSums(observed_table)) / sum(observed_table)

# Perform chi-square test for independence
chi_square_test <- chisq.test(observed_table) # Gives warning due to low sample size

# Print observed contingency table
print("Observed Contingency Table:")
print(observed_table)

# Print expected contingency table under independence
print("Expected Contingency Table under Independence:")
print(expected_table)

# Print chi-square test results
print("Chi-Square Test for Independence:")
print(chi_square_test)

# X-squared = 34.285 with 16 X2((5-1)·(5-1)) degrees of freedom, p-value = 0.004972 meaning 
# that there is no significant relationship between the variables thus making them independent.

# Print the contingency table
print(contingency_table)

CA_contingency <- CA(contingency_table)

# Math in blue and Depression in red
plot(CA_contingency)

summary(CA_contingency)





