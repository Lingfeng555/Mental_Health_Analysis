# Imports
Load_Libraries <- function(packages){
  newpack  = packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(newpack)) install.packages(newpack)
}
c("readxl") %>% Load_Libraries

# In general we want to keep the information of each year so that we can check that if variables are correlated in a some way
# Also change the names of the columns so it is easier to understand
# There is some key points for some dataset that is documented so any of us can understand all the dataframe by only consult this script

# This csv gives a raw dataset
# Remove few column and just keep the 2022 stats as a reference, and remove useless columns
Iq_Per_Country <- read.csv("RawDatasets/IQ_Per_Country.csv")
Iq_Per_Country <- data.frame(
  Country = Iq_Per_Country$country,
  Iq_byLynnBecker = Iq_Per_Country$averageIqByCountry_iqLynnBecker2019,
  Pisa2022Math = Iq_Per_Country$AverageIQPISA2022MeanScoreMathematics,
  Pisa2022Read = Iq_Per_Country$AverageIQPISA2022MeanScoreReading,
  Pisa2022Science = Iq_Per_Country$AverageIQPISA2022MeanScoreScience
)

# Read the CSV of mental disorder, gives us a raw data
Mental_Disorders <- read.csv(
  file = "RawDatasets/Mental_Disorders_For_Coutries_Per_Year.csv",
  dec = "."
)

# Let's keep the columns that main be useful and remove the others
Mental_Disorders <- data.frame(
  # This dataframe is composed by many different datasets concatenated, we only want to keep the first one, so there is a filter by rows
  Country = Mental_Disorders$Entity[1:6468],
  # All the columns are readed as strings so call as.numeric() to parse it to numbers
  # We keep all disorders columns because in case of being possible we want to extender the model to other disorders that is not depression
  Year = as.numeric(Mental_Disorders$Year[1:6468]),
  Schizophrenia = as.numeric(Mental_Disorders$Schizophrenia....[1:6468]),
  Bipolar = as.numeric(Mental_Disorders$Bipolar.disorder....[1:6468]),
  Anxiety = as.numeric(Mental_Disorders$Anxiety.disorders....[1:6468]),
  Eating_Disorders = as.numeric(Mental_Disorders$Eating.disorders....[1:6468]),
  Depression = as.numeric(Mental_Disorders$Depression....[1:6468]),
  Drug_Related_Disorders = as.numeric(Mental_Disorders$Drug.use.disorders....[1:6468]),
  Alcohol_Related_Disorders = as.numeric(Mental_Disorders$Alcohol.use.disorders....[1:6468])
)

# Read the CSV of PIB, gives us a raw data
GDP_Per_Capita <- read.csv("RawDatasets/GDP_Per_Capita.csv")

# In this case we only keep the information of years and remove useless information such as the code of each country
GDP_Per_Capita <- data.frame(
  Country = GDP_Per_Capita$Country.Name,
  Y2013 = as.numeric(gsub(",", ".", GDP_Per_Capita$X2013)),
  Y2014 = as.numeric(gsub(",", ".", GDP_Per_Capita$X2014)),
  Y2015 = as.numeric(gsub(",", ".", GDP_Per_Capita$X2015)),
  Y2016 = as.numeric(gsub(",", ".", GDP_Per_Capita$X2016)),
  Y2017 = as.numeric(gsub(",", ".", GDP_Per_Capita$X2017)),
  Y2018 = as.numeric(gsub(",", ".", GDP_Per_Capita$X2018)),
  Y2019 = as.numeric(gsub(",", ".", GDP_Per_Capita$X2019)),
  Y2020 = as.numeric(gsub(",", ".", GDP_Per_Capita$X2020)),
  Y2021 = as.numeric(gsub(",", ".", GDP_Per_Capita$X2021))
)

# Read raw data of the suicide of each country
Suicide_Per_Country <- read.csv("RawDatasets/Suicide_Per_Country.csv")

# Keep column that can be useful and remove others
Suicide_Per_Country <- data.frame(
  Country = Suicide_Per_Country$GEO_NAME_SHORT,
  Year = Suicide_Per_Country$DIM_TIME,
  Gender = Suicide_Per_Country$DIM_SEX,
  Age_Range = Suicide_Per_Country$DIM_AGE,
  Number = Suicide_Per_Country$VALUE_NUMERIC,
  Upper = Suicide_Per_Country$VALUE_NUMERIC_UPPER,
  Lower = Suicide_Per_Country$VALUE_NUMERIC_LOWER
)

# Happiness dataframe
happiness <- readxl::read_xlsx("RawDatasets/Hapiness/all_years.xlsx")
# Remove useless columns
happiness <- happiness[,1:10]
happiness$Gov_Corruption <- as.numeric(happiness$Gov_Corruption) # cast a string column to numeric

# This dataframe shows indicators, for example France has 1.05 in life expectancy, that doesn't means that the average life expectancy is 1.05 years. 
# The score column is the happiness rating based on the other variables in the data frame
# Column such as Gov_Corruption and Generosity is a percentage 1=100% and 0=0%
summary(happiness)
boxplot(happiness$Score) #No outliers

# Nothing special, just show the educational level of each country in 2022
summary(Iq_Per_Country)
boxplot(Iq_Per_Country$Pisa2022Math) #No outliers
boxplot(Iq_Per_Country$Pisa2022Read) #No outliers
boxplot(Iq_Per_Country$Pisa2022Science) #No outliers

# All columns are percentages over 100 -> 2.05 = 2.05%
summary(Mental_Disorders)

# This is a dataframe that contains a absolute number of PIB, it has to be normalized
summary(GDP_Per_Capita)

# This dataframe contains the number of suicide deaths in a year, divided by the population and multiplied by 100 000, with a lower and upper bound
summary(Suicide_Per_Country)
boxplot(Suicide_Per_Country$Number) #The is a lot of outliers but, the majoity is because the data is splited in diferent age range. But there is also a lot of real outliers too
