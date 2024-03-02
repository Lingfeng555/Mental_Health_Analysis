Iq_Per_Country <- read.csv("RawDatasets/IQ_Per_Country.csv")

Mental_Disorders <- read.csv(
  file = "RawDatasets/Mental_Disorders_For_Coutries_Per_Year.csv",
  dec = "."
)
Mental_Disorders <- data.frame(
  Country = Mental_Disorders$Entity[1:6468],
  Year = as.numeric(Mental_Disorders$Year[1:6468]),
  Schizophrenia = as.numeric(Mental_Disorders$Schizophrenia....[1:6468]),
  Bipolar = as.numeric(Mental_Disorders$Bipolar.disorder....[1:6468]),
  Anxiety = as.numeric(Mental_Disorders$Anxiety.disorders....[1:6468]),
  Eating_Disorders = as.numeric(Mental_Disorders$Eating.disorders....[1:6468]),
  Depression = as.numeric(Mental_Disorders$Depression....[1:6468]),
  Drug_Related_Disorders = as.numeric(Mental_Disorders$Drug.use.disorders....[1:6468]),
  Alcohol_Related_Disorders = as.numeric(Mental_Disorders$Alcohol.use.disorders....[1:6468])
)

Pib_Per_Country <- read.csv(
  file = "RawDatasets/PIB_Per_Country_Per_Year.csv",
  na.strings = "..",
  dec = "."
)
  
Pib_Per_Country <- data.frame(
  Currency = Pib_Per_Country$Series.Name,
  Country = Pib_Per_Country$Country.Name,
  Y2013 = Pib_Per_Country$X2013..YR2013.,
  Y2014 = Pib_Per_Country$X2014..YR2014.,
  Y2015 = Pib_Per_Country$X2015..YR2015.,
  Y2016 = Pib_Per_Country$X2016..YR2016.,
  Y2017 = Pib_Per_Country$X2017..YR2017.,
  Y2018 = Pib_Per_Country$X2018..YR2018.,
  Y2019 = Pib_Per_Country$X2019..YR2019.,
  Y2020 = Pib_Per_Country$X2020..YR2020.,
  Y2021 = Pib_Per_Country$X2021..YR2021.,
   Y2022 = Pib_Per_Country$X2022..YR2022.
)

Suicide_Per_Country <- read.csv("RawDatasets/Suicide_Per_Country.csv")

Suicide_Per_Country <- data.frame(
  Country = Suicide_Per_Country$GEO_NAME_SHORT,
  Year = Suicide_Per_Country$DIM_TIME,
  Gender = Suicide_Per_Country$DIM_SEX,
  Age_Range = Suicide_Per_Country$DIM_AGE,
  Number = Suicide_Per_Country$VALUE_NUMERIC,
  Upper = Suicide_Per_Country$VALUE_NUMERIC_UPPER,
  Lower = Suicide_Per_Country$VALUE_NUMERIC_LOWER
)

Happiness15_19 <- list(Happiness2015 = read.csv("RawDatasets/Hapiness/2015.csv"), 
                       Happiness2016 = read.csv("RawDatasets/Hapiness/2016.csv"), 
                       Happiness2017 = read.csv("RawDatasets/Hapiness/2017.csv"), 
                       Happiness2018 = read.csv("RawDatasets/Hapiness/2018.csv"), 
                       Happiness2019 = read.csv("RawDatasets/Hapiness/2019.csv")) 
