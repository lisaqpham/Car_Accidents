#------------------------------------------------------------
# Load data and packages
#------------------------------------------------------------
options(scipen = 10)
set.seed(1818)

# please install these if running the first time 
# using install.packages()
library('here')
library(partykit)
library(tidyverse)
library(PerformanceAnalytics)
library(rpart)       
library(rpart.plot)
library(forcats)
library(rsample)
library(ggplot2)
library(ggmap)
library(dplyr)
library(lubridate)
library('randomForest')

#-------------------------------------------------------------------------------
# Read in US Accidents Dataset https://www.kaggle.com/sobhanmoosavi/us-accidents
#-------------------------------------------------------------------------------

crashes <- read.csv(here("datasets", "US_Accidents_Dec20_updated 2.csv"))
summary(crashes)

# Clean and Factor crashes dataset
crashes_clean <-
  crashes %>% 
  mutate(Start_Time_Clean = ymd_hms(Start_Time),
         End_Time_Clean = ymd_hms(End_Time),
         Weather_Condition_Clean = as.factor(Weather_Condition),
         Wind_Direction_Clean = as.factor(Wind_Direction),
         Weather_Timestamp_Clean = ymd_hms(Weather_Timestamp),
         State_Clean = as.factor(State),
         County_Clean = as.factor(County),
         City_Clean = as.factor(City),
         Country_Clean = as.factor(Country),
         Side_Clean = as.factor(Side),
         Severity_Clean = as.factor(Severity),
         Amenity_Clean = as.factor(Amenity),
         Bump_Clean = as.factor(Bump),
         Crossing_Clean = as.factor(Crossing),
         Give_Way_Clean = as.factor(Give_Way),
         Junction_Clean = as.factor(Junction),
         No_Exit_Clean = as.factor(No_Exit),
         Railway_Clean = as.factor(Railway),
         Roundabout_Clean = as.factor(Roundabout),
         Station_Clean = as.factor(Station),
         Stop_Clean = as.factor(Stop),
         Traffic_Calming_Clean = as.factor(Traffic_Calming),
         Traffic_Signal_Clean = as.factor(Traffic_Signal),
         Turning_Loop_Clean = as.factor(Turning_Loop),
         Sunrise_Sunset_Clean = as.factor(Sunrise_Sunset))

crashes_clean = subset(crashes_clean, select = -c(ID,Start_Time,End_Time,Weather_Condition,Wind_Direction,Weather_Timestamp,State,County,City,Country,Side,Amenity,Bump,Crossing,Give_Way,Junction,No_Exit,Railway,Roundabout,Station,Stop,Traffic_Calming,Traffic_Signal,Turning_Loop,Sunrise_Sunset))
crashes_clean = subset(crashes_clean, select = -c(Description,Number,Timezone,Airport_Code,Country_Clean,Civil_Twilight,Nautical_Twilight,Astronomical_Twilight) )

summary(crashes_clean)

# Plot Histograms/Bar Charts
data(crashes_clean)
ggplot(data=crashes, aes(x = Severity)) + geom_histogram()
ggplot(data=crashes_clean, aes(x = Start_Time_Clean)) + geom_histogram(binwidth = 86400) + labs(x="Crash Date",y="# of Crashes")

table(crashes$State)
crashes_clean <- crashes_clean %>% mutate(State_Clean2 = fct_infreq(State_Clean))
ggplot(data=crashes_clean, aes(x = State_Clean2)) + geom_bar() + labs(x="State",y="# of Crashes")

# Map Plot with Severity
qmplot(Start_Lng, Start_Lat, data = crashes, maptype = "toner-lite", color = factor(Severity))

# Individual Severity Map Plots
crashes_severity_1 = subset(crashes, Severity == 1, select = c(Severity,Start_Lng, Start_Lat))
crashes_severity_2 = subset(crashes, Severity == 2, select = c(Severity,Start_Lng, Start_Lat))
crashes_severity_3 = subset(crashes, Severity == 3, select = c(Severity,Start_Lng, Start_Lat))
crashes_severity_4 = subset(crashes, Severity == 4, select = c(Severity,Start_Lng, Start_Lat))
qmplot(Start_Lng, Start_Lat, data = crashes_severity_1, maptype = "toner-lite") 
qmplot(Start_Lng, Start_Lat, data = crashes_severity_2, maptype = "toner-lite") 
qmplot(Start_Lng, Start_Lat, data = crashes_severity_3, maptype = "toner-lite") 
qmplot(Start_Lng, Start_Lat, data = crashes_severity_4, maptype = "toner-lite") 

table(crashes$Severity)

#------------------------------------------------------------------------------------------
# Read in NYC Accidents Dataset https://www.kaggle.com/mysarahmadbhat/nyc-traffic-accidents
#------------------------------------------------------------------------------------------

nyc_crashes <- read.csv(here("datasets", "NYC Accidents 2020.csv"))
nyc_crashes <- subset(nyc_crashes, LATITUDE != 0.00 & LONGITUDE != 0.00) #eliminate outlier (0,0)
summary(nyc_crashes)

# Map NYC Crashes
qmplot(LONGITUDE, LATITUDE, data = nyc_crashes, maptype = "toner-lite", color="red")

# Clean and Factor NYC crashes dataset
nyc_crashes_clean <- 
  nyc_crashes %>% 
  mutate(CONTRIBUTING.FACTOR.VEHICLE.1_Clean = as.factor(CONTRIBUTING.FACTOR.VEHICLE.1),
         CONTRIBUTING.FACTOR.VEHICLE.2_Clean = as.factor(CONTRIBUTING.FACTOR.VEHICLE.2),
         CONTRIBUTING.FACTOR.VEHICLE.3_Clean = as.factor(CONTRIBUTING.FACTOR.VEHICLE.3),
         CONTRIBUTING.FACTOR.VEHICLE.4_Clean = as.factor(CONTRIBUTING.FACTOR.VEHICLE.4),
         CONTRIBUTING.FACTOR.VEHICLE.5_Clean = as.factor(CONTRIBUTING.FACTOR.VEHICLE.5),
         NUMBER.OF.PERSONS.INJURED_Clean = as.factor(NUMBER.OF.PERSONS.INJURED),
         ZIP.CODE_Clean = as.factor(ZIP.CODE),
         VEHICLE.TYPE.CODE.1_Clean = as.factor(VEHICLE.TYPE.CODE.1),
         VEHICLE.TYPE.CODE.2_Clean = as.factor(VEHICLE.TYPE.CODE.2),
         VEHICLE.TYPE.CODE.3_Clean = as.factor(VEHICLE.TYPE.CODE.3),
         VEHICLE.TYPE.CODE.4_Clean = as.factor(VEHICLE.TYPE.CODE.4),
         VEHICLE.TYPE.CODE.5_Clean = as.factor(VEHICLE.TYPE.CODE.5))
summary(nyc_crashes_clean)

# Unordered Vehicle 1 Contributing Factor Bar Chart
ggplot(data=nyc_crashes_clean, aes(x = CONTRIBUTING.FACTOR.VEHICLE.1_Clean)) + geom_bar() + labs(x="Vehicle 1 Contributing Factor",y="Count")

# Ordered Vehicle 1 Contributing Factor Bar Chart
ggplot(data=nyc_crashes_clean, aes(x = CONTRIBUTING.FACTOR.VEHICLE.1_Clean2)) + geom_bar() + labs(x="Vehicle 1 Contributing Factor",y="Count")

# Vehicle 1 Contributing Factor Bar Chart - Cleaned and Top 10
nyc_crashes_clean <- nyc_crashes_clean %>% mutate(factor1_simple = fct_lump_n(CONTRIBUTING.FACTOR.VEHICLE.1_Clean, n = 10))
levels(nyc_crashes_clean$factor1_simple)
nyc_crashes_clean <- nyc_crashes_clean %>% mutate(factor1_simple2 = fct_infreq(factor1_simple))
nyc_crashes_clean_factor <- subset(nyc_crashes_clean, factor1_simple2 != "Unspecified" & factor1_simple2 != "Other")
ggplot(data=nyc_crashes_clean_factor, aes(x = factor1_simple2)) + geom_histogram(stat="count") + labs(x="Vehicle 1 Contributing Factor",y="Count")
summary(nyc_crashes_clean$factor1_simple2)

# Vehicle 2 Contributing Factor Bar Chart - Cleaned and Top 10
nyc_crashes_clean <- nyc_crashes_clean %>% mutate(factor2_simple = fct_lump_n(CONTRIBUTING.FACTOR.VEHICLE.2_Clean, n = 10))
levels(nyc_crashes_clean$factor2_simple)
nyc_crashes_clean <- nyc_crashes_clean %>% mutate(factor2_simple2 = fct_infreq(factor2_simple))
nyc_crashes_clean_factor <- subset(nyc_crashes_clean, factor2_simple2 != "Unspecified" & factor2_simple2 != "Other")
ggplot(data=nyc_crashes_clean_factor, aes(x = factor2_simple2)) + geom_histogram(stat="count") + labs(x="Vehicle 2 Contributing Factor",y="Count")
summary(nyc_crashes_clean$factor2_simple2)

# Vehicle 3 Contributing Factor Bar Chart - Cleaned and Top 10
nyc_crashes_clean <- nyc_crashes_clean %>% mutate(factor3_simple = fct_lump_n(CONTRIBUTING.FACTOR.VEHICLE.3_Clean, n = 10))
levels(nyc_crashes_clean$factor3_simple)
nyc_crashes_clean <- nyc_crashes_clean %>% mutate(factor3_simple2 = fct_infreq(factor3_simple))
nyc_crashes_clean_factor <- subset(nyc_crashes_clean, factor3_simple2 != "Unspecified" & factor3_simple2 != "Other" & factor3_simple2 != "")
ggplot(data=nyc_crashes_clean_factor, aes(x = factor3_simple2)) + geom_histogram(stat="count") + labs(x="Vehicle 3 Contributing Factor",y="Count")
summary(nyc_crashes_clean$factor3_simple2)

# Vehicle 4 Contributing Factor Bar Chart - Cleaned and Top 10
nyc_crashes_clean <- nyc_crashes_clean %>% mutate(factor4_simple = fct_lump_n(CONTRIBUTING.FACTOR.VEHICLE.4_Clean, n = 10))
levels(nyc_crashes_clean$factor4_simple)
nyc_crashes_clean <- nyc_crashes_clean %>% mutate(factor4_simple2 = fct_infreq(factor4_simple))
nyc_crashes_clean_factor <- subset(nyc_crashes_clean, factor4_simple2 != "Unspecified" & factor4_simple2 != "Other")
ggplot(data=nyc_crashes_clean_factor, aes(x = factor4_simple2)) + geom_histogram(stat="count") + labs(x="Vehicle 4 Contributing Factor",y="Count")
summary(nyc_crashes_clean$factor4_simple2)

# Vehicle 5 Contributing Factor Bar Chart - Cleaned and Top 10
nyc_crashes_clean <- nyc_crashes_clean %>% mutate(factor5_simple = fct_lump_n(CONTRIBUTING.FACTOR.VEHICLE.5_Clean, n = 10))
levels(nyc_crashes_clean$factor5_simple)
nyc_crashes_clean <- nyc_crashes_clean %>% mutate(factor5_simple2 = fct_infreq(factor5_simple))
nyc_crashes_clean_factor <- subset(nyc_crashes_clean, factor5_simple2 != "Unspecified" & factor5_simple2 != "Other")
ggplot(data=nyc_crashes_clean_factor, aes(x = factor5_simple2)) + geom_histogram(stat="count") + labs(x="Vehicle 5 Contributing Factor",y="Count")
summary(nyc_crashes_clean$factor5_simple2)

# Histogram of Persons Injured
ggplot(data=nyc_crashes_clean, aes(x=NUMBER.OF.PERSONS.INJURED)) + geom_histogram(stat="count") + labs(x="Number of Persons Injured",y="Count")
summary(nyc_crashes_clean$NUMBER.OF.PERSONS.INJURED)

# Histogram of Persons Injured
ggplot(data=nyc_crashes_clean, aes(x=NUMBER.OF.PERSONS.KILLED)) + geom_histogram(stat="count") + labs(x="Number of Persons Injured",y="Count")
summary(nyc_crashes_clean$NUMBER.OF.PERSONS.KILLED)

# Histogram of Pedestrians Injured
ggplot(data=nyc_crashes_clean, aes(x=NUMBER.OF.PEDESTRIANS.INJURED)) + geom_histogram(stat="count") + labs(x="Number of Persons Injured",y="Count")
summary(nyc_crashes_clean$NUMBER.OF.PEDESTRIANS.INJURED)

# Histogram of Cyclists Injured
ggplot(data=nyc_crashes_clean, aes(x=NUMBER.OF.CYCLIST.INJURED)) + geom_histogram(stat="count") + labs(x="Number of Persons Injured",y="Count")
summary(nyc_crashes_clean$NUMBER.OF.CYCLIST.INJURED)

# Histogram of Motorists Injured
ggplot(data=nyc_crashes_clean, aes(x=NUMBER.OF.MOTORIST.INJURED)) + geom_histogram(stat="count") + labs(x="Number of Persons Injured",y="Count")
summary(nyc_crashes_clean$NUMBER.OF.MOTORIST.INJURED)

# Histogram of Top 10 Zip Codes
nyc_crashes_clean <- nyc_crashes_clean %>% mutate(zipcode = fct_lump_n(ZIP.CODE_Clean, n = 10))
levels(nyc_crashes_clean$zipcode)
nyc_crashes_clean <- nyc_crashes_clean %>% mutate(zipcode2 = fct_infreq(zipcode))
nyc_crashes_clean_factor <- subset(nyc_crashes_clean, zipcode2 != "Other")
ggplot(data=nyc_crashes_clean_factor, aes(x = zipcode2)) + geom_histogram(stat="count") + labs(x="Zip Code",y="Count")
summary(nyc_crashes_clean_factor$zipcode2)

# Map of Crashes at Top 10 Zip Codes
nyc_crashes_clean_coord <- subset(nyc_crashes_clean, ZIP.CODE == 11207 | ZIP.CODE == 11236 | ZIP.CODE == 11212 | ZIP.CODE == 11203 | ZIP.CODE == 11385 | ZIP.CODE == 11208 | ZIP.CODE == 11434 | ZIP.CODE == 11234 | ZIP.CODE == 11226 | ZIP.CODE == 11233)
qmplot(LONGITUDE, LATITUDE, data = nyc_crashes_clean_coord, maptype = "toner-lite", color="red")

#--------------------------------------------------------
# Regressions
#--------------------------------------------------------

# Split Training and Testing Data - US crashes
crashes_split <- initial_split(crashes_clean, prop = 0.75)
crashes_train <- training(crashes_split)
crashes_test <- testing(crashes_split)
crashes_train <- na.omit(crashes_train)
crashes_test <- na.omit(crashes_test)

# Split Training and Testing Data - NYC crashes
nyc_crashes_split <- initial_split(nyc_crashes_clean, prop = 0.75)
nyc_crashes_train <- training(nyc_crashes_split)
nyc_crashes_test <- testing(nyc_crashes_split)
nyc_crashes_train <- na.omit(nyc_crashes_train)
nyc_crashes_test <- na.omit(nyc_crashes_test)

# Dimensions of Training and Testing Data - US & NYC
dim(crashes_train)
dim(crashes_test)

dim(nyc_crashes_train)
dim(nyc_crashes_test)

# Regression Models

mod1 <- lm(NUMBER.OF.PERSONS.INJURED ~ LATITUDE + LONGITUDE + factor(CONTRIBUTING.FACTOR.VEHICLE.1_Clean)+ factor(CONTRIBUTING.FACTOR.VEHICLE.2_Clean) +factor(CONTRIBUTING.FACTOR.VEHICLE.3_Clean) + factor(CONTRIBUTING.FACTOR.VEHICLE.4_Clean)
           + factor(CONTRIBUTING.FACTOR.VEHICLE.5_Clean), data = nyc_crashes_train)
summary(mod1)

mod2 <- lm(Severity ~ Visibility.mi. + Pressure.in. + Humidity... + Wind_Chill.F.
           + Precipitation.in. + factor(Weather_Condition_Clean) + factor(Wind_Direction_Clean),  data = crashes_train)
summary(mod2)

# ----------------------------------------
# Correlation Plot
# ----------------------------------------

install.packages('ggcorrplot')

library('ggcorrplot')

ggcorrplot(round(cor(nyc_crashes_clean),2), 
          type = "lower", insig = "blank",
          show.diag = TRUE, lab = TRUE, 
          colors = c("red","white","blue"))

nyc_crashes_contri = subset(nyc_crashes_clean, select = c(CONTRIBUTING.FACTOR.VEHICLE.1_Clean, CONTRIBUTING.FACTOR.VEHICLE.2_Clean, CONTRIBUTING.FACTOR.VEHICLE.3_Clean, zipcode, NUMBER.OF.PERSONS.INJURED))
nyc_crashes_contri = subset(nyc_crashes_clean, select = c(factor1_simple,zipcode, NUMBER.OF.PERSONS.INJURED))

model.matrix(~0+., data=nyc_crashes_contri) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)

# ----------------------------------------
# Regression Trees
# ----------------------------------------

?ctree

# Tree based on location
nyc_tree_mod <- ctree(NUMBER.OF.PERSONS.INJURED ~ ZIP.CODE + LATITUDE + LONGITUDE, data = nyc_crashes_train)
print(nyc_tree_mod)
plot(nyc_tree_mod, gp = gpar(fontsize = 6))

# Tree based on Contributing Factors
nyc_tree_mod2 <- ctree(NUMBER.OF.PERSONS.INJURED ~ factor1_simple, data = nyc_crashes_clean)
print(nyc_tree_mod2)
plot(nyc_tree_mod2, gp = gpar(fontsize = 6))

# Trees based on Vehicle Type
nyc_crashes_clean <- nyc_crashes_clean %>% mutate(main_vehicletype = fct_lump_n(VEHICLE.TYPE.CODE.1_Clean, n = 10))
nyc_crashes_clean <- nyc_crashes_clean %>% mutate(secondary_vehicletype = fct_lump_n(VEHICLE.TYPE.CODE.2_Clean, n = 10))

nyc_tree_mod3 <- ctree(NUMBER.OF.PERSONS.INJURED ~ main_vehicletype, data = nyc_crashes_clean)
print(nyc_tree_mod3)
plot(nyc_tree_mod3, gp = gpar(fontsize = 6))

nyc_tree_mod4 <- ctree(NUMBER.OF.PERSONS.INJURED ~ main_vehicletype + secondary_vehicletype, data = nyc_crashes_clean)
print(nyc_tree_mod4)
plot(nyc_tree_mod4, gp = gpar(fontsize = 6))

# ----------------------------------------
# Maps of Contributing Factors by Location
# ----------------------------------------

qmplot(LONGITUDE, LATITUDE, data = nyc_crashes_clean, maptype = "toner-lite", color=factor1_simple)
nyc_crashes_clean_coord <- nyc_crashes_clean_coord %>% mutate(factor1_simple = fct_lump_n(CONTRIBUTING.FACTOR.VEHICLE.1_Clean, n = 10))
qmplot(LONGITUDE, LATITUDE, data = nyc_crashes_clean_coord, maptype = "toner-lite", color=factor1_simple)
