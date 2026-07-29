# This script gets census data and runs regressions post peer-review

# Loading libraries

library(AER)
library(dplyr)
library(lmtest)
library(tigris)
library(ggplot2)
library(leaflet)
library(sandwich)
library(stargazer)
library(tidycensus)
library(modelsummary)

# Project directory

direc <- 'D:/weak_sustainability/'

# Reading in the county level land cover proportions panel data set

pd <- read.csv(paste0(direc, 'data/county_level_proportions_2001_2011_2021.csv'))

# Updating the FIPS codes in pd to make them all five digies

pd$County <- as.character(pd$County)
pd$County <- ifelse(nchar(pd$County) < 5, paste0('0', pd$County), pd$County)

# Get the counties land area from tigris

us_counties <- counties(cb = TRUE)

# Getting county level ACS data

states <- c('01', '04', '05', '06', '08', '09', '10', '11', '12', '13', '16', '17', '18', '19', '20', '21', '22',
            '23', '24', '25', '26', '27', '28', '29', '30', '31', '32', '33', '34', '35', '36', '37', '38',
            '39', '40', '41', '42', '44', '45', '46', '47', '48', '49', '50', '51', '53', '54', '55', '56')

c21 <- as.data.frame(NULL)
c11 <- as.data.frame(NULL)

for (s in states) {
  
  print(paste0('Collecting ACS data for state FIPS ', s, '.......'))
  
  tmp21 <- get_acs(state = s, geography = 'county', year = 2021, variables = c('DP05_0001', 'DP03_0062', 'DP02_0067P', 'DP02_0068P', 'DP03_0009P', 'DP02_0084P', 'DP03_0019',
                                                                               'DP03_0021', 'DP04_0001', 'DP03_0033', 'DP03_0034', 'DP03_0035', 'DP03_0036', 'DP03_0037',
                                                                               'DP03_0038', 'DP03_0039', 'DP03_0040', 'DP03_0041', 'DP03_0042', 'DP03_0043', 'DP03_0044',
                                                                               'DP03_0045', 'DP05_0002', 'DP05_0008', 'DP05_0009', 'DP05_0010', 'DP05_0011', 'DP05_0012',
                                                                               'DP05_0013', 'DP05_0014', 'DP05_0015', 'DP05_0016', 'DP05_0017', 'DP02_0060', 'DP02_0061'))
  
  tmp11 <- get_acs(state = s, geography = 'county', year = 2011, variables = c('DP05_0001', 'DP03_0062', 'DP02_0066P', 'DP02_0067P', 'DP03_0009P', 'DP02_0082P', 'DP03_0019',
                                                                               'DP03_0021', 'DP04_0001', 'DP03_0033', 'DP03_0034', 'DP03_0035', 'DP03_0036', 'DP03_0037',
                                                                               'DP03_0038', 'DP03_0039', 'DP03_0040', 'DP03_0041', 'DP03_0042', 'DP03_0043', 'DP03_0044',
                                                                               'DP03_0045', 'DP05_0002', 'DP05_0008', 'DP05_0009', 'DP05_0010', 'DP05_0011', 'DP05_0012',
                                                                               'DP05_0013', 'DP05_0014', 'DP05_0015', 'DP05_0016', 'DP05_0017', 'DP02_0059', 'DP02_0060'))
  
  c21 <- rbind(c21, tmp21)
  c11 <- rbind(c11, tmp11)
  
}

# Merging the ACS data and the NLCD data

data <- pd %>% filter(Year >= 2010)

pop <- c()
inc <- c()
ed <- c()
ed2 <- c()
emp <- c()
hunits <- c()
movers <- c()
comms <- c()
pt <- c()
men <- c()
age.15.34 <- c()
age.35.64 <- c()
age.65 <- c()
nohs <- c()
outdo <- c()
cons <- c()
manu <- c()
whole <- c()
retail <- c()
trans <- c()
fin <- c()
info <- c()
pro <- c()
sw <- c()
amen <- c()
other <- c()
pad <- c()

for (i in 1:(nrow(data)/2)) {
  
  print(paste0('Creating 2021 ACS data for county ', i, ' of 3,108.......'))
  
  tmp21 <- c21 %>% filter(GEOID == data$County[i])
  
  pop <- c(pop, log(tmp21[which(tmp21$variable == 'DP05_0001'),]$estimate[1]))
  inc <- c(inc, log(tmp21[which(tmp21$variable == 'DP03_0062'),]$estimate[1]))
  ed <- c(ed, tmp21[which(tmp21$variable == 'DP02_0068P'),]$estimate[1])
  ed2 <- c(ed2, tmp21[which(tmp21$variable == 'DP02_0067P'),]$estimate[1])
  emp <- c(emp, tmp21[which(tmp21$variable == 'DP03_0009P'),]$estimate[1])
  movers <- c(movers, tmp21[which(tmp21$variable == 'DP02_0084P'),]$estimate[1])
  comms <- c(comms, log(tmp21[which(tmp21$variable == 'DP03_0019'),]$estimate[1]))
  pt <- c(pt, max(0, log(tmp21[which(tmp21$variable == 'DP03_0021'),]$estimate[1])))
  hunits <- c(hunits, log(tmp21[which(tmp21$variable == 'DP04_0001'),]$estimate[1]))
  outdo <- c(outdo, tmp21[which(tmp21$variable == 'DP03_0033'),]$estimate[1])
  cons <- c(cons, tmp21[which(tmp21$variable == 'DP03_0034'),]$estimate[1])
  manu <- c(manu, tmp21[which(tmp21$variable == 'DP03_0035'),]$estimate[1])
  whole <- c(whole, tmp21[which(tmp21$variable == 'DP03_0036'),]$estimate[1])
  retail <- c(retail, tmp21[which(tmp21$variable == 'DP03_0037'),]$estimate[1])
  trans <- c(trans, tmp21[which(tmp21$variable == 'DP03_0038'),]$estimate[1])
  fin <- c(fin, tmp21[which(tmp21$variable == 'DP03_0039'),]$estimate[1])
  info <- c(info, tmp21[which(tmp21$variable == 'DP03_0040'),]$estimate[1])
  pro <- c(pro, tmp21[which(tmp21$variable == 'DP03_0041'),]$estimate[1])
  sw <- c(sw, tmp21[which(tmp21$variable == 'DP03_0042'),]$estimate[1])
  amen <- c(amen, tmp21[which(tmp21$variable == 'DP03_0043'),]$estimate[1])
  other <- c(other, tmp21[which(tmp21$variable == 'DP03_0044'),]$estimate[1])
  pad <- c(pad, tmp21[which(tmp21$variable == 'DP03_0045'),]$estimate[1])
  men <- c(men, tmp21[which(tmp21$variable == 'DP05_0002'),]$estimate[1])
  age.15.34 <- c(age.15.34, tmp21[which(tmp21$variable == 'DP05_0008'),]$estimate[1] + tmp21[which(tmp21$variable == 'DP05_0009'),]$estimate[1] + tmp21[which(tmp21$variable == 'DP05_0010'),]$estimate[1])
  age.35.64 <- c(age.35.64, tmp21[which(tmp21$variable == 'DP05_0011'),]$estimate[1] + tmp21[which(tmp21$variable == 'DP05_0012'),]$estimate[1] + tmp21[which(tmp21$variable == 'DP05_0013'),]$estimate[1] + tmp21[which(tmp21$variable == 'DP05_0014'),]$estimate[1])
  age.65 <- c(age.65, tmp21[which(tmp21$variable == 'DP05_0015'),]$estimate[1] + tmp21[which(tmp21$variable == 'DP05_0016'),]$estimate[1] + tmp21[which(tmp21$variable == 'DP05_0017'),]$estimate[1])
  nohs <- c(nohs, tmp21[which(tmp21$variable == 'DP02_0060P'),]$estimate[1] + tmp21[which(tmp21$variable == 'DP02_0061P'),]$estimate[1])
  
}

for (i in 3109:6216) {
  
  print(paste0('Creating 2011 ACS data for county ', i-3108, ' of 3,108.......'))
  
  tmp11 <- c11 %>% filter(GEOID == data$County[i])
  
  pop <- c(pop, log(tmp11[which(tmp11$variable == 'DP05_0001'),]$estimate[1]))
  inc <- c(inc, log(tmp11[which(tmp11$variable == 'DP03_0062'),]$estimate[1]))
  ed <- c(ed, tmp11[which(tmp11$variable == 'DP02_0067P'),]$estimate[1])
  ed2 <- c(ed2, tmp11[which(tmp11$variable == 'DP02_0066P'),]$estimate[1])
  emp <- c(emp, tmp11[which(tmp11$variable == 'DP03_0009P'),]$estimate[1])
  movers <- c(movers, tmp11[which(tmp11$variable == 'DP02_0082P'),]$estimate[1])
  comms <- c(comms, log(tmp11[which(tmp11$variable == 'DP03_0019'),]$estimate[1]))
  pt <- c(pt, max(0, log(tmp11[which(tmp11$variable == 'DP03_0021'),]$estimate[1])))
  hunits <- c(hunits, log(tmp11[which(tmp11$variable == 'DP04_0001'),]$estimate[1]))
  outdo <- c(outdo, tmp11[which(tmp11$variable == 'DP03_0033'),]$estimate[1])
  cons <- c(cons, tmp11[which(tmp11$variable == 'DP03_0034'),]$estimate[1])
  manu <- c(manu, tmp11[which(tmp11$variable == 'DP03_0035'),]$estimate[1])
  whole <- c(whole, tmp11[which(tmp11$variable == 'DP03_0036'),]$estimate[1])
  retail <- c(retail, tmp11[which(tmp11$variable == 'DP03_0037'),]$estimate[1])
  trans <- c(trans, tmp11[which(tmp11$variable == 'DP03_0038'),]$estimate[1])
  fin <- c(fin, tmp11[which(tmp11$variable == 'DP03_0039'),]$estimate[1])
  info <- c(info, tmp11[which(tmp11$variable == 'DP03_0040'),]$estimate[1])
  pro <- c(pro, tmp11[which(tmp11$variable == 'DP03_0041'),]$estimate[1])
  sw <- c(sw, tmp11[which(tmp11$variable == 'DP03_0042'),]$estimate[1])
  amen <- c(amen, tmp11[which(tmp11$variable == 'DP03_0043'),]$estimate[1])
  other <- c(other, tmp11[which(tmp11$variable == 'DP03_0044'),]$estimate[1])
  pad <- c(pad, tmp11[which(tmp11$variable == 'DP03_0045'),]$estimate[1])
  men <- c(men, tmp11[which(tmp11$variable == 'DP05_0002'),]$estimate[1])
  age.15.34 <- c(age.15.34, tmp11[which(tmp11$variable == 'DP05_0008'),]$estimate[1] + tmp11[which(tmp11$variable == 'DP05_0009'),]$estimate[1] + tmp11[which(tmp11$variable == 'DP05_0010'),]$estimate[1])
  age.35.64 <- c(age.35.64, tmp11[which(tmp11$variable == 'DP05_0011'),]$estimate[1] + tmp11[which(tmp11$variable == 'DP05_0012'),]$estimate[1] + tmp11[which(tmp11$variable == 'DP05_0013'),]$estimate[1] + tmp11[which(tmp11$variable == 'DP05_0014'),]$estimate[1])
  age.65 <- c(age.65, tmp11[which(tmp11$variable == 'DP05_0015'),]$estimate[1] + tmp11[which(tmp11$variable == 'DP05_0016'),]$estimate[1] + tmp11[which(tmp11$variable == 'DP05_0017'),]$estimate[1])
  nohs <- c(nohs, tmp11[which(tmp11$variable == 'DP02_0059P'),]$estimate[1] + tmp11[which(tmp11$variable == 'DP02_0060P'),]$estimate[1])
  
}

data <- cbind(data, pop, inc, ed, ed2, emp, movers, comms, pt, hunits, men, age.15.34, age.35.64, age.65, nohs, outdo, cons, manu, whole, retail, trans, fin, info, pro, sw, amen, other, pad)

colnames(data) <- c('County', 'Year', 'Water', 'Development', 'Barren', 'Forests', 'Shrublands', 'Grasslands', 'Agriculture',
                    'Wetlands', 'Population', 'Income', 'Education_BS', 'Education_HS', 'Unemployment', 'New_Residents',
                    'Commute_Solo_By_Car', 'Public_Transit', 'Housing_Units', 'Men', 'Age_15_34', 'Age_35_64', 'Age_65', 'No_HS', 
                    'Outdoors', 'Construction', 'Manufacturing', 'Wholesale', 'Retial', 'Transportation', 'Finance', 'Information',
                    'Professional', 'Social', 'Amenities', 'Other_Jobs', 'Public_Administration')

# Adding a total jobs column to data

data$Jobs <- data$Outdoors + data$Construction + data$Manufacturing + data$Wholesale + data$Retial + data$Transportation + data$Finance + data$Information + data$Professional + data$Social + data$Amenities + data$Other_Jobs + data$Public_Administration

# Creating a differenced data set for areas

land.area <- c()

for (i in 1:nrow(data)) {
  
  print(i)
  tmp <- us_counties[which(us_counties$GEOID == data$County[i]),]
  land.area <- c(land.area, tmp$ALAND/1000000)
  
}

data$AREA <- land.area

data$Water_Area <- data$Water * data$AREA
data$Development_Area <- data$Development * data$AREA
data$Barren_Area <- data$Barren * data$AREA
data$Forests_Area <- data$Forests * data$AREA
data$Shrublands_Area <- data$Shrublands * data$AREA
data$Grasslands_Area <- data$Grasslands * data$AREA
data$Agriculture_Area <- data$Agriculture * data$AREA
data$Wetlands_Area <- data$Wetlands * data$AREA

counties <- data$County[1:3108]
water <- data$Water_Area[1:3108] - data$Water_Area[3109:6216]
development <- data$Development_Area[1:3108] - data$Development_Area[3109:6216]
barren <- data$Barren_Area[1:3108] - data$Barren_Area[3109:6216]
forests <- data$Forests_Area[1:3108] - data$Forests_Area[3109:6216]
shrublands <- data$Shrublands_Area[1:3108] - data$Shrublands_Area[3109:6216]
grasslands <- data$Grasslands_Area[1:3108] - data$Grasslands_Area[3109:6216]
agriculture <- data$Agriculture_Area[1:3108] - data$Agriculture_Area[3109:6216]
wetlands <- data$Wetlands_Area[1:3108] - data$Wetlands_Area[3109:6216]
population <- data$Population[1:3108] - data$Population[3109:6216]
income <- 1.36*data$Income[1:3108] - data$Income[3109:6216]
education <- data$Education_BS[1:3108] - data$Education_BS[3109:6216]
education2 <- data$Education_HS[1:3108] - data$Education_HS[3109:6216]
unemployment <- data$Unemployment[1:3108] - data$Unemployment[3109:6216]
turnover <- data$New_Residents[1:3108] - data$New_Residents[3109:6216]
commy_car <- data$Commute_Solo_By_Car[1:3108] - data$Commute_Solo_By_Car[3109:6216]
public_commy <- data$Public_Transit[1:3108] - data$Public_Transit[3109:6216]
housing_units <- data$Housing_Units[1:3108] - data$Housing_Units[3109:6216]
outdoors <- data$Outdoors[1:3108] - data$Outdoors[3109:6216]
construction <- data$Construction[1:3108] - data$Construction[3109:6216]
manufacturing <- data$Manufacturing[1:3108] - data$Manufacturing[3109:6216]
wholesale <- data$Wholesale[1:3108] - data$Wholesale[3109:6216]
retail <- data$Retial[1:3108] - data$Retial[3109:6216]
transportation <- data$Transportation[1:3108] - data$Transportation[3109:6216]
finance <- data$Finance[1:3108] - data$Finance[3109:6216]
information <- data$Information[1:3108] - data$Information[3109:6216]
professional <- data$Professional[1:3108] - data$Professional[3109:6216]
social <- data$Social[1:3108] - data$Social[3109:6216]
amenities <- data$Amenities[1:3108] - data$Amenities[3109:6216]
other_jobs <- data$Other_Jobs[1:3108] - data$Other_Jobs[3109:6216]
public_administration <- data$Public_Administration[1:3108] - data$Public_Administration[3109:6216]
jobs <- data$Jobs[1:3108] - data$Jobs[3109:6216]

df <- as.data.frame(cbind(water, development, barren, forests, shrublands, grasslands, agriculture, wetlands, population, income,
                          education, education2, unemployment, turnover, commy_car, public_commy, housing_units, outdoors,
                          construction, manufacturing, wholesale, retail, transportation, finance, information, professional,
                          social, amenities, other_jobs, public_administration, jobs))

colnames(df) <- c('Water', 'Development', 'Barren', 'Forests', 'Shrublands', 'Grasslands', 'Agriculture', 'Wetlands', 'Population',
                  'Income', 'Education_BS', 'Education_HS', 'Unemployment', 'New_Residents', 'Commute_Solo_By_Car', 'Public_Transit',
                  'Housing_Units', 'Outdoors', 'Construction', 'Manufacturing', 'Wholesale', 'Retial', 'Transportation', 'Finance',
                  'Information', 'Professional', 'Social', 'Amenities', 'Other_Jobs', 'Public_Administration', 'Jobs')

df$County <- counties

# Adding an aggregate employment growth rate to df

egr <- c()

for (i in 1:nrow(df)) {
  
  print(paste0('Creating employment growth rate data for county ', i, ' of 3,108.......'))
  
  tmp <- data %>% filter(County == df$County[i])
  
  tmp_rates <- c((tmp$Outdoors[1] / tmp$Outdoors[2])^(1/10) - 1, 
                 (tmp$Construction[1] / tmp$Construction[2])^(1/10) - 1, 
                 (tmp$Manufacturing[1] / tmp$Manufacturing[2])^(1/10) - 1, 
                 (tmp$Wholesale[1] / tmp$Wholesale[2])^(1/10) - 1, 
                 (tmp$Retial[1] / tmp$Retial[2])^(1/10) - 1, 
                 (tmp$Transportation[1] / tmp$Transportation[2])^(1/10) - 1, 
                 (tmp$Finance[1] / tmp$Finance[2])^(1/10) - 1, 
                 (tmp$Information[1] / tmp$Information[2])^(1/10) - 1, 
                 (tmp$Professional[1] / tmp$Professional[2])^(1/10) - 1, 
                 (tmp$Social[1] / tmp$Social[2])^(1/10) - 1, 
                 (tmp$Amenities[1] / tmp$Amenities[2])^(1/10) - 1, 
                 (tmp$Other_Jobs[1] / tmp$Other_Jobs[2])^(1/10) - 1, 
                 (tmp$Public_Administration[1] / tmp$Public_Administration[2])^(1/10) - 1)
  
  tmp_rates[is.infinite(tmp_rates)] <- 0
  tmp_rates[is.na(tmp_rates)] <- 0
  val <- tmp$Outdoors[2]/tmp$Jobs[2]*tmp_rates[1] + tmp$Construction[2]/tmp$Jobs[2]*tmp_rates[2] + tmp$Manufacturing[2]/tmp$Jobs[2]*tmp_rates[3] + tmp$Wholesale[2]/tmp$Jobs[2]*tmp_rates[4] + tmp$Retial[2]/tmp$Jobs[2]*tmp_rates[5] + tmp$Transportation[2]/tmp$Jobs[2]*tmp_rates[6] + tmp$Finance[2]/tmp$Jobs[2]*tmp_rates[7] + tmp$Information[2]/tmp$Jobs[2]*tmp_rates[8] + tmp$Professional[2]/tmp$Jobs[2]*tmp_rates[9] + tmp$Social[2]/tmp$Jobs[2]*tmp_rates[10] + tmp$Amenities[2]/tmp$Jobs[2]*tmp_rates[11] + tmp$Other_Jobs[2]/tmp$Jobs[2]*tmp_rates[12] + tmp$Public_Administration[2]/tmp$Jobs[2]*tmp_rates[13]
  egr <- c(egr, val)
  
}

df <- cbind(df, egr)
colnames(df)[ncol(df)] <- 'Employment_Growth_Rate'

# Creating the Bartik instrument

baseline <- data %>% filter(Year == 2011)
update <- data %>% filter(Year == 2021)

out.rate <- (sum(update$Outdoors, na.rm = TRUE) / sum(baseline$Outdoors, na.rm = TRUE))^(1/10) - 1
con.rate <- (sum(update$Construction, na.rm = TRUE) / sum(baseline$Construction, na.rm = TRUE))^(1/10) - 1
man.rate <- (sum(update$Manufacturing, na.rm = TRUE) / sum(baseline$Manufacturing, na.rm = TRUE))^(1/10) - 1
who.rate <- (sum(update$Wholesale, na.rm = TRUE) / sum(baseline$Wholesale, na.rm = TRUE))^(1/10) - 1
ret.rate <- (sum(update$Retial, na.rm = TRUE) / sum(baseline$Retial, na.rm = TRUE))^(1/10) - 1
tra.rate <- (sum(update$Transportation, na.rm = TRUE) / sum(baseline$Transportation, na.rm = TRUE))^(1/10) - 1
fin.rate <- (sum(update$Finance, na.rm = TRUE) / sum(baseline$Finance, na.rm = TRUE))^(1/10) - 1
inf.rate <- (sum(update$Information, na.rm = TRUE) / sum(baseline$Information, na.rm = TRUE))^(1/10) - 1
pro.rate <- (sum(update$Professional, na.rm = TRUE) / sum(baseline$Professional, na.rm = TRUE))^(1/10) - 1
soc.rate <- (sum(update$Social, na.rm = TRUE) / sum(baseline$Social, na.rm = TRUE))^(1/10) - 1
ame.rate <- (sum(update$Amenities, na.rm = TRUE) / sum(baseline$Amenities, na.rm = TRUE))^(1/10) - 1
oth.rate <- (sum(update$Other_Jobs, na.rm = TRUE) / sum(baseline$Other_Jobs, na.rm = TRUE))^(1/10) - 1
pub.rate <- (sum(update$Public_Administration, na.rm = TRUE) / sum(baseline$Public_Administration, na.rm = TRUE))^(1/10) - 1

national_rates <- c(out.rate, con.rate, man.rate, who.rate, ret.rate, tra.rate, fin.rate, inf.rate, pro.rate, soc.rate, ame.rate, oth.rate, pub.rate)

bartik <- c()

for (i in 1:nrow(df)) {
  
  print(paste0('Creating Bartik instrument for county ', i, ' of 3,108.......'))
  
  tmp <- data %>% filter(County == df$County[i])
  
  tmp_rates[is.infinite(tmp_rates)] <- 0
  tmp_rates[is.na(tmp_rates)] <- 0
  val <- tmp$Outdoors[2]/tmp$Jobs[2]*national_rates[1] + tmp$Construction[2]/tmp$Jobs[2]*national_rates[2] + tmp$Manufacturing[2]/tmp$Jobs[2]*national_rates[3] + tmp$Wholesale[2]/tmp$Jobs[2]*national_rates[4] + tmp$Retial[2]/tmp$Jobs[2]*national_rates[5] + tmp$Transportation[2]/tmp$Jobs[2]*national_rates[6] + tmp$Finance[2]/tmp$Jobs[2]*national_rates[7] + tmp$Information[2]/tmp$Jobs[2]*national_rates[8] + tmp$Professional[2]/tmp$Jobs[2]*national_rates[9] + tmp$Social[2]/tmp$Jobs[2]*national_rates[10] + tmp$Amenities[2]/tmp$Jobs[2]*national_rates[11] + tmp$Other_Jobs[2]/tmp$Jobs[2]*national_rates[12] + tmp$Public_Administration[2]/tmp$Jobs[2]*national_rates[13]
  bartik <- c(bartik, val)
  
}

df <- cbind(df, bartik)
colnames(df)[ncol(df)] <- 'Bartik'

# Adding a state fixed effect

df$State <- substr(df$County, 1, 2)

# Adding historical values of land cover

df$Water.2011 <- data$Water_Area[3109:6216]
df$Development.2011 <- data$Development_Area[3109:6216]
df$Barren.2011 <- data$Barren_Area[3109:6216]
df$Forests.2011 <- data$Forests_Area[3109:6216]
df$Shrublands.2011 <- data$Shrublands_Area[3109:6216]
df$Grasslands.2011 <- data$Grasslands_Area[3109:6216]
df$Agriculture.2011 <- data$Agriculture_Area[3109:6216]
df$Wetlands.2011 <- data$Wetlands_Area[3109:6216]

# Adding urban-rural continuum codes

ur <- read.csv(paste0(direc, 'data/ur_codes.csv'))

ur <- ur %>% filter(Attribute == 'RUCC_2023')

ur$FIPS <- ifelse(ur$FIPS < 10000, paste0('0', as.character(ur$FIPS)), as.character(ur$FIPS))
ur$Large <- ifelse(ur$Value <= 3, 1, 0)
ur$Small <- ifelse(ur$Value %in% c(4,5), 1, 0)
ur$Rural <- ifelse(ur$Value >= 6, 1, 0)

large <- c()
small <- c()
rural <- c()

for (i in 1:nrow(df)) {
  
  print(paste0('Designating rural-urban status for county ', i, ' of 3,108.......'))
  
  tmp <- ur %>% filter(FIPS == df$County[i])
  
  large <- c(large, tmp$Large[1])
  small <- c(small, tmp$Small[1])
  rural <- c(rural, tmp$Rural[1])
  
}

df$Large <- large
df$Small <- small
df$Rural <- rural

# Adding historical values of land cover

df$Water.2011 <- data$Water_Area[3109:6216]
df$Development.2011 <- data$Development_Area[3109:6216]
df$Barren.2011 <- data$Barren_Area[3109:6216]
df$Forests.2011 <- data$Forests_Area[3109:6216]
df$Shrublands.2011 <- data$Shrublands_Area[3109:6216]
df$Grasslands.2011 <- data$Grasslands_Area[3109:6216]
df$Agriculture.2011 <- data$Agriculture_Area[3109:6216]
df$Wetlands.2011 <- data$Wetlands_Area[3109:6216]

# Adding historical values of land cover

df$Water.2011P <- data$Water[3109:6216]
df$Development.2011P <- data$Development[3109:6216]
df$Barren.2011P <- data$Barren[3109:6216]
df$Forests.2011P <- data$Forests[3109:6216]
df$Shrublands.2011P <- data$Shrublands[3109:6216]
df$Grasslands.2011P <- data$Grasslands[3109:6216]
df$Agriculture.2011P <- data$Agriculture[3109:6216]
df$Wetlands.2011P <- data$Wetlands[3109:6216]

# Running regressions for all counties

water.mod <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                   + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                   + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                   + Wetlands.2011P + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

development.mod <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                         + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                         + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                         + Wetlands.2011P + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

barren.mod <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                    + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                    + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                    + Wetlands.2011P + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

forests.mod <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                     + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                     + Wetlands.2011P + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

shrublands.mod <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                        + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                        + Wetlands.2011P + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

grasslands.mod <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                        + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                        + Wetlands.2011P + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

agriculture.mod <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                         + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                         + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                         + Wetlands.2011P + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

wetlands.mod <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                      + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                      + Wetlands.2011P + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

water.modx <- coeftest(water.mod, vcov = vcovCL(water.mod, type = 'HC1'))
development.modx <- coeftest(development.mod, vcov = vcovCL(development.mod, type = 'HC1'))
barren.modx <- coeftest(barren.mod, vcov = vcovCL(barren.mod, type = 'HC1'))
forests.modx <- coeftest(forests.mod, vcov = vcovCL(forests.mod, type = 'HC1'))
shrublands.modx <- coeftest(shrublands.mod, vcov = vcovCL(shrublands.mod, type = 'HC1'))
grasslands.modx <- coeftest(grasslands.mod, vcov = vcovCL(grasslands.mod, type = 'HC1'))
agriculture.modx <- coeftest(agriculture.mod, vcov = vcovCL(agriculture.mod, type = 'HC1'))
wetlands.modx <- coeftest(wetlands.mod, vcov = vcovCL(wetlands.mod, type = 'HC1'))

stargazer(water.mod, development.mod, barren.mod, forests.mod, shrublands.mod, grasslands.mod, agriculture.mod, wetlands.mod,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(water.modx, development.modx, barren.modx, forests.modx, shrublands.modx, grasslands.modx, agriculture.modx, wetlands.modx,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Running regressions for rural counties

water.rural <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                     + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                     + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 1),])

development.rural <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                           + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                           + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 1),])

barren.rural <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                      + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                      + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 1),])

forests.rural <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                       + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                       + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                       + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 1),])

shrublands.rural <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                          + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                          + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 1),])

grasslands.rural <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                          + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                          + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 1),])

agriculture.rural <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                           + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                           + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 1),])

wetlands.rural <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                        + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                        + Wetlands.2011P + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                        + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 1),])

water.ruralx <- coeftest(water.rural, vcov = vcovCL(water.rural, type = 'HC1'))
development.ruralx <- coeftest(development.rural, vcov = vcovCL(development.rural, type = 'HC1'))
barren.ruralx <- coeftest(barren.rural, vcov = vcovCL(barren.rural, type = 'HC1'))
forests.ruralx <- coeftest(forests.rural, vcov = vcovCL(forests.rural, type = 'HC1'))
shrublands.ruralx <- coeftest(shrublands.rural, vcov = vcovCL(shrublands.rural, type = 'HC1'))
grasslands.ruralx <- coeftest(grasslands.rural, vcov = vcovCL(grasslands.rural, type = 'HC1'))
agriculture.ruralx <- coeftest(agriculture.rural, vcov = vcovCL(agriculture.rural, type = 'HC1'))
wetlands.ruralx <- coeftest(wetlands.rural, vcov = vcovCL(wetlands.rural, type = 'HC1'))

stargazer(water.rural, development.rural, barren.rural, forests.rural, shrublands.rural, grasslands.rural, agriculture.rural, wetlands.rural,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(water.ruralx, development.ruralx, barren.ruralx, forests.ruralx, shrublands.ruralx, grasslands.ruralx, agriculture.ruralx, wetlands.ruralx,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Running regressions for all non-rural counties

water.urban <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                     + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                     + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 0),])

development.urban <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                           + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                           + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 0),])

barren.urban <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                      + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                      + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 0),])

forests.urban <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                       + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                       + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                       + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 0),])

shrublands.urban <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                          + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                          + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 0),])

grasslands.urban <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                          + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                          + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 0),])

agriculture.urban <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                           + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                           + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 0),])

wetlands.urban <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                        + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                        + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rural == 0),])

water.urbanx <- coeftest(water.urban, vcov = vcovCL(water.urban, type = 'HC1'))
development.urbanx <- coeftest(development.urban, vcov = vcovCL(development.urban, type = 'HC1'))
barren.urbanx <- coeftest(barren.urban, vcov = vcovCL(barren.urban, type = 'HC1'))
forests.urbanx <- coeftest(forests.urban, vcov = vcovCL(forests.urban, type = 'HC1'))
shrublands.urbanx <- coeftest(shrublands.urban, vcov = vcovCL(shrublands.urban, type = 'HC1'))
grasslands.urbanx <- coeftest(grasslands.urban, vcov = vcovCL(grasslands.urban, type = 'HC1'))
agriculture.urbanx <- coeftest(agriculture.urban, vcov = vcovCL(agriculture.urban, type = 'HC1'))
wetlands.urbanx <- coeftest(wetlands.urban, vcov = vcovCL(wetlands.urban, type = 'HC1'))

stargazer(water.urban, development.urban, barren.urban, forests.urban, shrublands.urban, grasslands.urban, agriculture.urban, wetlands.urban,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(water.urbanx, development.urbanx, barren.urbanx, forests.urbanx, shrublands.urbanx, grasslands.urbanx, agriculture.urbanx, wetlands.urbanx,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Running regressions for all large metropolitan counties (UR codes 1-3)

water.large <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                     + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                     + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Large == 1),])

development.large <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                           + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                           + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Large == 1),])

barren.large <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                      + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                      + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Large == 1),])

forests.large <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                       + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                       + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                       + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Large == 1),])

shrublands.large <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                          + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                          + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Large == 1),])

grasslands.large <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                          + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                          + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Large == 1),])

agriculture.large <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                           + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                           + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Large == 1),])

wetlands.large <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                        + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                        + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Large == 1),])

water.largex <- coeftest(water.large, vcov = vcovCL(water.large, type = 'HC1'))
development.largex <- coeftest(development.large, vcov = vcovCL(development.large, type = 'HC1'))
barren.largex <- coeftest(barren.large, vcov = vcovCL(barren.large, type = 'HC1'))
forests.largex <- coeftest(forests.large, vcov = vcovCL(forests.large, type = 'HC1'))
shrublands.largex <- coeftest(shrublands.large, vcov = vcovCL(shrublands.large, type = 'HC1'))
grasslands.largex <- coeftest(grasslands.large, vcov = vcovCL(grasslands.large, type = 'HC1'))
agriculture.largex <- coeftest(agriculture.large, vcov = vcovCL(agriculture.large, type = 'HC1'))
wetlands.largex <- coeftest(wetlands.large, vcov = vcovCL(wetlands.large, type = 'HC1'))

stargazer(water.large, development.large, barren.large, forests.large, shrublands.large, grasslands.large, agriculture.large, wetlands.large,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(water.largex, development.largex, barren.largex, forests.largex, shrublands.largex, grasslands.largex, agriculture.largex, wetlands.largex,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Running regressions for all small metropolitan counties (UR codes 4 and 5)

water.small <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                     + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                     + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Small == 1),])

development.small <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                           + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                           + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Small == 1),])

barren.small <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                      + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                      + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Small == 1),])

forests.small <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                       + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                       + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                       + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Small == 1),])

shrublands.small <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                          + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                          + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Small == 1),])

grasslands.small <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                          + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                          + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Small == 1),])

agriculture.small <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                           + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                           + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                           + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Small == 1),])

wetlands.small <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                        + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                        + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Small == 1),])

water.smallx <- coeftest(water.small, vcov = vcovCL(water.small, type = 'HC1'))
development.smallx <- coeftest(development.small, vcov = vcovCL(development.small, type = 'HC1'))
barren.smallx <- coeftest(barren.small, vcov = vcovCL(barren.small, type = 'HC1'))
forests.smallx <- coeftest(forests.small, vcov = vcovCL(forests.small, type = 'HC1'))
shrublands.smallx <- coeftest(shrublands.small, vcov = vcovCL(shrublands.small, type = 'HC1'))
grasslands.smallx <- coeftest(grasslands.small, vcov = vcovCL(grasslands.small, type = 'HC1'))
agriculture.smallx <- coeftest(agriculture.small, vcov = vcovCL(agriculture.small, type = 'HC1'))
wetlands.smallx <- coeftest(wetlands.small, vcov = vcovCL(wetlands.small, type = 'HC1'))

stargazer(water.small, development.small, barren.small, forests.small, shrublands.small, grasslands.small, agriculture.small, wetlands.small,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(water.smallx, development.smallx, barren.smallx, forests.smallx, shrublands.smallx, grasslands.smallx, agriculture.smallx, wetlands.smallx,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Determining halves for income

df$Rich <- as.integer(df$Income > median(df$Income, na.rm = T))

# Running regressions for higher half of incomes

water.rich <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                    + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                    + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                    + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rich == 1),])

development.rich <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                          + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                          + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rich == 1),])

barren.rich <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                     + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                     + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rich == 1),])

forests.rich <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                      + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                      + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rich == 1),])

shrublands.rich <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                         + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                         + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                         + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rich == 1),])

grasslands.rich <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                         + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                         + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                         + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rich == 1),])

agriculture.rich <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                          + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                          + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rich == 1),])

wetlands.rich <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                       + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                       + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                       + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rich == 1),])

water.richx <- coeftest(water.rich, vcov = vcovCL(water.rich, type = 'HC1'))
development.richx <- coeftest(development.rich, vcov = vcovCL(development.rich, type = 'HC1'))
barren.richx <- coeftest(barren.rich, vcov = vcovCL(barren.rich, type = 'HC1'))
forests.richx <- coeftest(forests.rich, vcov = vcovCL(forests.rich, type = 'HC1'))
shrublands.richx <- coeftest(shrublands.rich, vcov = vcovCL(shrublands.rich, type = 'HC1'))
grasslands.richx <- coeftest(grasslands.rich, vcov = vcovCL(grasslands.rich, type = 'HC1'))
agriculture.richx <- coeftest(agriculture.rich, vcov = vcovCL(agriculture.rich, type = 'HC1'))
wetlands.richx <- coeftest(wetlands.rich, vcov = vcovCL(wetlands.rich, type = 'HC1'))

stargazer(water.rich, development.rich, barren.rich, forests.rich, shrublands.rich, grasslands.rich, agriculture.rich, wetlands.rich,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(water.richx, development.richx, barren.richx, forests.richx, shrublands.richx, grasslands.richx, agriculture.richx, wetlands.richx,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Running regressions for lower half of incomes

water.poor <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                    + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                    + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                    + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rich == 0),])

development.poor <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                          + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                          + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rich == 0),])

barren.poor <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                     + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                     + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rich == 0),])

forests.poor <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                      + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                      + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rich == 0),])

shrublands.poor <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                         + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                         + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                         + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rich == 0),])

grasslands.poor <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                         + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                         + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                         + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rich == 0),])

agriculture.poor <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                          + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                          + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rich == 0),])

wetlands.poor <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                       + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                       + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                       + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Rich == 0),])

water.poorx <- coeftest(water.poor, vcov = vcovCL(water.poor, type = 'HC1'))
development.poorx <- coeftest(development.poor, vcov = vcovCL(development.poor, type = 'HC1'))
barren.poorx <- coeftest(barren.poor, vcov = vcovCL(barren.poor, type = 'HC1'))
forests.poorx <- coeftest(forests.poor, vcov = vcovCL(forests.poor, type = 'HC1'))
shrublands.poorx <- coeftest(shrublands.poor, vcov = vcovCL(shrublands.poor, type = 'HC1'))
grasslands.poorx <- coeftest(grasslands.poor, vcov = vcovCL(grasslands.poor, type = 'HC1'))
agriculture.poorx <- coeftest(agriculture.poor, vcov = vcovCL(agriculture.poor, type = 'HC1'))
wetlands.poorx <- coeftest(wetlands.poor, vcov = vcovCL(wetlands.poor, type = 'HC1'))

stargazer(water.poor, development.poor, barren.poor, forests.poor, shrublands.poor, grasslands.poor, agriculture.poor, wetlands.poor,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(water.poorx, development.poorx, barren.poorx, forests.poorx, shrublands.poorx, grasslands.poorx, agriculture.poorx, wetlands.poorx,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Determining halves for growth rates

poop.mod <- lm(Employment_Growth_Rate ~ Bartik, data = df)
df$Exogenous_Growth <- c(poop.mod$fitted.values[1:(which(is.na(df$Employment_Growth_Rate))-1)], NA, poop.mod$fitted.values[which(is.na(df$Employment_Growth_Rate)):length(poop.mod$fitted.values)])
df$Exo_Fast <- as.integer(df$Exogenous_Growth > median(df$Exogenous_Growth, na.rm = T))
df$Fast <- as.integer(df$Employment_Growth_Rate > median(df$Employment_Growth_Rate, na.rm = T))

# Running regressions for higher half of growth rates

water.fast <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                    + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                    + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                    + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Fast == 1),])

development.fast <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                          + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                          + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Fast == 1),])

barren.fast <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                     + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                     + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Fast == 1),])

forests.fast <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                      + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                      + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Fast == 1),])

shrublands.fast <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                         + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                         + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                         + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Fast == 1),])

grasslands.fast <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                         + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                         + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                         + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Fast == 1),])

agriculture.fast <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                          + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                          + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Fast == 1),])

wetlands.fast <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                       + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                       + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                       + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Fast == 1),])

water.fastx <- coeftest(water.fast, vcov = vcovCL(water.fast, type = 'HC1'))
development.fastx <- coeftest(development.fast, vcov = vcovCL(development.fast, type = 'HC1'))
barren.fastx <- coeftest(barren.fast, vcov = vcovCL(barren.fast, type = 'HC1'))
forests.fastx <- coeftest(forests.fast, vcov = vcovCL(forests.fast, type = 'HC1'))
shrublands.fastx <- coeftest(shrublands.fast, vcov = vcovCL(shrublands.fast, type = 'HC1'))
grasslands.fastx <- coeftest(grasslands.fast, vcov = vcovCL(grasslands.fast, type = 'HC1'))
agriculture.fastx <- coeftest(agriculture.fast, vcov = vcovCL(agriculture.fast, type = 'HC1'))
wetlands.fastx <- coeftest(wetlands.fast, vcov = vcovCL(wetlands.fast, type = 'HC1'))

stargazer(water.fast, development.fast, barren.fast, forests.fast, shrublands.fast, grasslands.fast, agriculture.fast, wetlands.fast,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(water.fastx, development.fastx, barren.fastx, forests.fastx, shrublands.fastx, grasslands.fastx, agriculture.fastx, wetlands.fastx,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Running regressions for lower half of growth rates

water.slow <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                    + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                    + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                    + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Fast == 0),])

development.slow <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                          + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                          + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Fast == 0),])

barren.slow <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                     + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                     + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Fast == 0),])

forests.slow <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                      + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                      + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Fast == 0),])

shrublands.slow <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                         + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                         + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                         + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Fast == 0),])

grasslands.slow <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                         + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                         + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                         + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Fast == 0),])

agriculture.slow <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                          + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                          + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Fast == 0),])

wetlands.slow <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                       + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                       + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                       + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Fast == 0),])

water.slowx <- coeftest(water.slow, vcov = vcovCL(water.slow, type = 'HC1'))
development.slowx <- coeftest(development.slow, vcov = vcovCL(development.slow, type = 'HC1'))
barren.slowx <- coeftest(barren.slow, vcov = vcovCL(barren.slow, type = 'HC1'))
forests.slowx <- coeftest(forests.slow, vcov = vcovCL(forests.slow, type = 'HC1'))
shrublands.slowx <- coeftest(shrublands.slow, vcov = vcovCL(shrublands.slow, type = 'HC1'))
grasslands.slowx <- coeftest(grasslands.slow, vcov = vcovCL(grasslands.slow, type = 'HC1'))
agriculture.slowx <- coeftest(agriculture.slow, vcov = vcovCL(agriculture.slow, type = 'HC1'))
wetlands.slowx <- coeftest(wetlands.slow, vcov = vcovCL(wetlands.slow, type = 'HC1'))

stargazer(water.slow, development.slow, barren.slow, forests.slow, shrublands.slow, grasslands.slow, agriculture.slow, wetlands.slow,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(water.slowx, development.slowx, barren.slowx, forests.slowx, shrublands.slowx, grasslands.slowx, agriculture.slowx, wetlands.slowx,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Dividing states into east / west

east <- c('01', '05', '10', '11', '12', '13', '21', '22', '24', '28', '37', '45', '47', '51', '54', '09',
          '23', '25', '33', '34', '36', '42', '44', '50', '17', '18', '19', '26', '27', '29', '39', '55')

regions <- c()

for (i in 1:nrow(df)) {
  
  if (df$State[i] %in% east) {
    
    regions <- c(regions, 'East')
    
  } else {
    
    regions <- c(regions, 'West')
    
  }
  
}

df$Region <- regions

# Running regressions for the east

water.east <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                    + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                    + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                    + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Region == 'East'),])

development.east <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                          + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                          + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Region == 'East'),])

barren.east <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                     + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                     + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Region == 'East'),])

forests.east <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                      + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                      + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Region == 'East'),])

shrublands.east <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                         + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                         + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                         + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Region == 'East'),])

grasslands.east <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                         + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                         + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                         + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Region == 'East'),])

agriculture.east <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                          + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                          + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Region == 'East'),])

wetlands.east <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                       + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                       + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                       + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Region == 'East'),])

water.eastx <- coeftest(water.east, vcov = vcovCL(water.east, type = 'HC1'))
development.eastx <- coeftest(development.east, vcov = vcovCL(development.east, type = 'HC1'))
barren.eastx <- coeftest(barren.east, vcov = vcovCL(barren.east, type = 'HC1'))
forests.eastx <- coeftest(forests.east, vcov = vcovCL(forests.east, type = 'HC1'))
shrublands.eastx <- coeftest(shrublands.east, vcov = vcovCL(shrublands.east, type = 'HC1'))
grasslands.eastx <- coeftest(grasslands.east, vcov = vcovCL(grasslands.east, type = 'HC1'))
agriculture.eastx <- coeftest(agriculture.east, vcov = vcovCL(agriculture.east, type = 'HC1'))
wetlands.eastx <- coeftest(wetlands.east, vcov = vcovCL(wetlands.east, type = 'HC1'))

stargazer(water.east, development.east, barren.east, forests.east, shrublands.east, grasslands.east, agriculture.east, wetlands.east,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(water.eastx, development.eastx, barren.eastx, forests.eastx, shrublands.eastx, grasslands.eastx, agriculture.eastx, wetlands.eastx,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Running regressions for the west

water.west <- ivreg(Water ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                    + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                    + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                    + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Region == 'West'),])

development.west <- ivreg(Development ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                          + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                          + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Region == 'West'),])

barren.west <- ivreg(Barren ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                     + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                     + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Region == 'West'),])

forests.west <- ivreg(Forests ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                      + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                      + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Region == 'West'),])

shrublands.west <- ivreg(Shrublands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                         + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                         + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                         + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Region == 'West'),])

grasslands.west <- ivreg(Grasslands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                         + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                         + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                         + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Region == 'West'),])

agriculture.west <- ivreg(Agriculture ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                          + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                          + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Region == 'West'),])

wetlands.west <- ivreg(Wetlands ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                       + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                       + Wetlands.2011 + Development.2011P + Barren.2011P + Forests.2011P + Shrublands.2011P + Grasslands.2011P + Agriculture.2011P
                       + Wetlands.2011P + factor(State) | . - Employment_Growth_Rate + Bartik, data = df[which(df$Region == 'West'),])

water.westx <- coeftest(water.west, vcov = vcovCL(water.west, type = 'HC1'))
development.westx <- coeftest(development.west, vcov = vcovCL(development.west, type = 'HC1'))
barren.westx <- coeftest(barren.west, vcov = vcovCL(barren.west, type = 'HC1'))
forests.westx <- coeftest(forests.west, vcov = vcovCL(forests.west, type = 'HC1'))
shrublands.westx <- coeftest(shrublands.west, vcov = vcovCL(shrublands.west, type = 'HC1'))
grasslands.westx <- coeftest(grasslands.west, vcov = vcovCL(grasslands.west, type = 'HC1'))
agriculture.westx <- coeftest(agriculture.west, vcov = vcovCL(agriculture.west, type = 'HC1'))
wetlands.westx <- coeftest(wetlands.west, vcov = vcovCL(wetlands.west, type = 'HC1'))

stargazer(water.west, development.west, barren.west, forests.west, shrublands.west, grasslands.west, agriculture.west, wetlands.west,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

stargazer(water.westx, development.westx, barren.westx, forests.westx, shrublands.westx, grasslands.westx, agriculture.westx, wetlands.westx,
          type = 'text', omit.stat = c('f', 'ser'), omit = c('State'))

# Saving results

write.csv(stargazer(water.modx, development.modx, barren.modx, forests.modx, shrublands.modx, grasslands.modx, agriculture.modx, wetlands.modx, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/review_main.txt'), row.names = FALSE)
write.csv(stargazer(water.ruralx, development.ruralx, barren.ruralx, forests.ruralx, shrublands.ruralx, grasslands.ruralx, agriculture.ruralx, wetlands.ruralx, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/review_rural.txt'), row.names = FALSE)
write.csv(stargazer(water.urbanx, development.urbanx, barren.urbanx, forests.urbanx, shrublands.urbanx, grasslands.urbanx, agriculture.urbanx, wetlands.urbanx, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/review_non_rural.txt'), row.names = FALSE)
write.csv(stargazer(water.largex, development.largex, barren.largex, forests.largex, shrublands.largex, grasslands.largex, agriculture.largex, wetlands.largex, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/review_large_urban.txt'), row.names = FALSE)
write.csv(stargazer(water.smallx, development.smallx, barren.smallx, forests.smallx, shrublands.smallx, grasslands.smallx, agriculture.smallx, wetlands.smallx, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/review_small_urban.txt'), row.names = FALSE)
write.csv(stargazer(water.richx, development.richx, barren.richx, forests.richx, shrublands.richx, grasslands.richx, agriculture.richx, wetlands.richx, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/review_rich.txt'), row.names = FALSE)
write.csv(stargazer(water.poorx, development.poorx, barren.poorx, forests.poorx, shrublands.poorx, grasslands.poorx, agriculture.poorx, wetlands.poorx, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/review_poor.txt'), row.names = FALSE)
write.csv(stargazer(water.fastx, development.fastx, barren.fastx, forests.fastx, shrublands.fastx, grasslands.fastx, agriculture.fastx, wetlands.fastx, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/review_fast.txt'), row.names = FALSE)
write.csv(stargazer(water.slow, development.slowx, barren.slowx, forests.slowx, shrublands.slowx, grasslands.slowx, agriculture.slowx, wetlands.slowx, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/review_slow.txt'), row.names = FALSE)
write.csv(stargazer(water.eastx, development.eastx, barren.eastx, forests.eastx, shrublands.eastx, grasslands.eastx, agriculture.eastx, wetlands.eastx, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/review_east.txt'), row.names = FALSE)
write.csv(stargazer(water.westx, development.westx, barren.westx, forests.westx, shrublands.westx, grasslands.westx, agriculture.westx, wetlands.westx, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/review_west.txt'), row.names = FALSE)

f.stats.main <- rep(69.97, 8)
f.stats.rural <- rep(40.59, 8)
f.stats.urban <- rep(15.83, 8)
f.stats.large <- rep(9.37, 8)
f.stats.small <- rep(0.35, 8)
f.stats.rich <- rep(52.69, 8)
f.stats.poor <- rep(22.67, 8)
f.stats.fast <- rep(35.93, 8)
f.stats.slow <- rep(26.35, 8)
f.stats.east <- rep(22.55, 8)
f.stats.west <- rep(47.24, 8)

nobs.main <- rep(3106, 8)
nobs.rural <- rep(1309, 8)
nobs.urban <- rep(1797, 8)
nobs.large <- rep(805, 8)
nobs.small <- rep(992, 8)
nobs.rich <- rep(1553, 8)
nobs.poor <- rep(1553, 8)
nobs.fast <- rep(1553, 8)
nobs.slow <- rep(1553, 8)
nobs.east <- rep(2046, 8)
nobs.west <- rep(1060, 8)

additional.stats <- as.data.frame(rbind(f.stats.main, f.stats.rural, f.stats.urban, f.stats.large, f.stats.small, f.stats.rich, f.stats.poor, f.stats.fast, f.stats.slow, f.stats.east, f.stats.west,
                                        nobs.main, nobs.rural, nobs.urban, nobs.large, nobs.small, nobs.rich, nobs.poor, nobs.fast, nobs.slow, nobs.east, nobs.west))

write.csv(additional.stats, paste0(direc, 'results/review_additional_stats.txt'), row.names = TRUE)

# Additional statistics requested by a reviewer

d11 <- data %>% filter(Year == 2011)
d21 <- data %>% filter(Year == 2021)

areas_11 <- c(sum(d11$Water_Area), sum(d11$Development_Area), sum(d11$Barren_Area), sum(d11$Forests_Area),
              sum(d11$Shrublands_Area), sum(d11$Grasslands_Area), sum(d11$Agriculture_Area), sum(d11$Wetlands_Area))

areas_21 <- c(sum(d21$Water_Area), sum(d21$Development_Area), sum(d21$Barren_Area), sum(d21$Forests_Area),
              sum(d21$Shrublands_Area), sum(d21$Grasslands_Area), sum(d21$Agriculture_Area), sum(d21$Wetlands_Area))

areas_11 <- c(areas_11, sum(areas_11))
areas_21 <- c(areas_21, sum(areas_21))

props_11 <- c(mean(d11$Water), mean(d11$Development), mean(d11$Barren), mean(d11$Forests),
              mean(d11$Shrublands), mean(d11$Grasslands), mean(d11$Agriculture), mean(d11$Wetlands))

props_21 <- c(mean(d21$Water), mean(d21$Development), mean(d21$Barren), mean(d21$Forests),
              mean(d21$Shrublands), mean(d21$Grasslands), mean(d21$Agriculture), mean(d21$Wetlands))

props_11 <- c(props_11, sum(props_11))
props_21 <- c(props_21, sum(props_21))

apdf <- as.data.frame(rbind(areas_11, areas_21, props_11, props_21))

write.csv(apdf, paste0(direc, 'results/areas_and_proportions.txt'), row.names = FALSE)

