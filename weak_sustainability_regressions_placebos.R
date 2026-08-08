# This script gets census data and runs regressions

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

us_counties <- counties(year = 2020)

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

# Creating a differenced data set

counties <- data$County[1:3108]
water <- data$Water[1:3108] - data$Water[3109:6216]
development <- data$Development[1:3108] - data$Development[3109:6216]
barren <- data$Barren[1:3108] - data$Barren[3109:6216]
forests <- data$Forests[1:3108] - data$Forests[3109:6216]
shrublands <- data$Shrublands[1:3108] - data$Shrublands[3109:6216]
grasslands <- data$Grasslands[1:3108] - data$Grasslands[3109:6216]
agriculture <- data$Agriculture[1:3108] - data$Agriculture[3109:6216]
wetlands <- data$Wetlands[1:3108] - data$Wetlands[3109:6216]
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

df$Water.2011 <- data$Water[3109:6216]
df$Development.2011 <- data$Development[3109:6216]
df$Barren.2011 <- data$Barren[3109:6216]
df$Forests.2011 <- data$Forests[3109:6216]
df$Shrublands.2011 <- data$Shrublands[3109:6216]
df$Grasslands.2011 <- data$Grasslands[3109:6216]
df$Agriculture.2011 <- data$Agriculture[3109:6216]
df$Wetlands.2011 <- data$Wetlands[3109:6216]

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

ct <- c('09001', '09003', '09005', '09007', '09009', '09011', '09013', '09015')
ct2 <- c(1,1,0,1,1,1,1,1)
ct3 <- c(0,0,1,0,0,0,0,0)

for (i in 1:nrow(df)) {
  
  print(paste0('Designating rural-urban status for county ', i, ' of 3,108.......'))
  
  if (df$State[i] != '09') {
    
    tmp <- ur %>% filter(FIPS == df$County[i])
    
    large <- c(large, tmp$Large[1])
    small <- c(small, tmp$Small[1])
    rural <- c(rural, tmp$Rural[1])
    
  } else {
    
    large <- c(large, ct2[which(ct == df$County[i])])
    small <- c(small, ct3[which(ct == df$County[i])])
    rural <- c(rural, 0)
    
  }
  
}

df$Large <- large
df$Small <- small
df$Rural <- rural

# Adding land area data

land.area <- c()

for (i in 1:nrow(df)) {
  
  print(paste0('Getting land area for county ', i, ' of 3,108.......'))
  
  tmp <- us_counties %>% filter(GEOID == df$County[i])
  land.area <- c(land.area, tmp$ALAND[1])
  
}

df$Land_Area <- land.area

df$Development_Area <- df$Land_Area * df$Development.2011
df$Barren_Area <- df$Land_Area * df$Barren.2011
df$Forests_Area <- df$Land_Area * df$Forests.2011
df$Shrublands_Area <- df$Land_Area * df$Shrublands.2011
df$Grasslands_Area <- df$Land_Area * df$Grasslands.2011
df$Agriculture_Area <- df$Land_Area * df$Agriculture.2011
df$Wetlands_Area <- df$Land_Area * df$Wetlands.2011

# Placebo test prep

set.seed(420)

xxx <- sample(nrow(df))

df$Water.Placebo <- df$Water[xxx]
df$Development.Placebo <- df$Development[xxx]
df$Barren.Placebo <- df$Barren[xxx]
df$Forests.Placebo <- df$Forests[xxx]
df$Shrublands.Placebo <- df$Shrublands[xxx]
df$Grasslands.Placebo <- df$Grasslands[xxx]
df$Agriculture.Placebo <- df$Agriculture[xxx]
df$Wetlands.Placebo <- df$Wetlands[xxx]
df$Employment_Growth_Rate.Placebo <- df$Employment_Growth_Rate[xxx]
df$Bartik.Placebo <- df$Bartik[xxx]

# Running placebos with randomized EGR and Bartik

water.mod <- ivreg(Water ~ Employment_Growth_Rate.Placebo + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                   + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                   + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate.Placebo + Bartik.Placebo, data = df)

development.mod <- ivreg(Development ~ Employment_Growth_Rate.Placebo + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                         + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                         + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate.Placebo + Bartik.Placebo, data = df)

barren.mod <- ivreg(Barren ~ Employment_Growth_Rate.Placebo + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                    + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                    + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate.Placebo + Bartik.Placebo, data = df)

forests.mod <- ivreg(Forests ~ Employment_Growth_Rate.Placebo + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                     + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate.Placebo + Bartik.Placebo, data = df)

shrublands.mod <- ivreg(Shrublands ~ Employment_Growth_Rate.Placebo + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                        + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate.Placebo + Bartik.Placebo, data = df)

grasslands.mod <- ivreg(Grasslands ~ Employment_Growth_Rate.Placebo + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                        + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                        + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate.Placebo + Bartik.Placebo, data = df)

agriculture.mod <- ivreg(Agriculture ~ Employment_Growth_Rate.Placebo + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                         + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                         + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate.Placebo + Bartik.Placebo, data = df)

wetlands.mod <- ivreg(Wetlands ~ Employment_Growth_Rate.Placebo + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                      + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate.Placebo + Bartik.Placebo, data = df)

water.modx <- coeftest(water.mod, vcov = vcovCL(water.mod, type = 'HC1'))
development.modx <- coeftest(development.mod, vcov = vcovCL(development.mod, type = 'HC1'))
barren.modx <- coeftest(barren.mod, vcov = vcovCL(barren.mod, type = 'HC1'))
forests.modx <- coeftest(forests.mod, vcov = vcovCL(forests.mod, type = 'HC1'))
shrublands.modx <- coeftest(shrublands.mod, vcov = vcovCL(shrublands.mod, type = 'HC1'))
grasslands.modx <- coeftest(grasslands.mod, vcov = vcovCL(grasslands.mod, type = 'HC1'))
agriculture.modx <- coeftest(agriculture.mod, vcov = vcovCL(agriculture.mod, type = 'HC1'))
wetlands.modx <- coeftest(wetlands.mod, vcov = vcovCL(wetlands.mod, type = 'HC1'))

# Running placebos with randomized outcomes

water.mod2 <- ivreg(Water.Placebo ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                    + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                    + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

development.mod2 <- ivreg(Development.Placebo ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                          + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

barren.mod2 <- ivreg(Barren.Placebo ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                     + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                     + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

forests.mod2 <- ivreg(Forests.Placebo ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                      + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                      + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

shrublands.mod2 <- ivreg(Shrublands.Placebo ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                         + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                         + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

grasslands.mod2 <- ivreg(Grasslands.Placebo ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                         + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                         + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

agriculture.mod2 <- ivreg(Agriculture.Placebo ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                          + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                          + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

wetlands.mod2 <- ivreg(Wetlands.Placebo ~ Employment_Growth_Rate + Population + Income + Education_BS + Unemployment + Commute_Solo_By_Car + Public_Transit + New_Residents
                       + Housing_Units + Development.2011 + Barren.2011 + Forests.2011 + Shrublands.2011 + Grasslands.2011 + Agriculture.2011
                       + Wetlands.2011 + Rural + Small + factor(State) | . - Employment_Growth_Rate + Bartik, data = df)

water.mod2x <- coeftest(water.mod2, vcov = vcovCL(water.mod2, type = 'HC1'))
development.mod2x <- coeftest(development.mod2, vcov = vcovCL(development.mod2, type = 'HC1'))
barren.mod2x <- coeftest(barren.mod2, vcov = vcovCL(barren.mod2, type = 'HC1'))
forests.mod2x <- coeftest(forests.mod2, vcov = vcovCL(forests.mod2, type = 'HC1'))
shrublands.mod2x <- coeftest(shrublands.mod2, vcov = vcovCL(shrublands.mod2, type = 'HC1'))
grasslands.mod2x <- coeftest(grasslands.mod2, vcov = vcovCL(grasslands.mod2, type = 'HC1'))
agriculture.mod2x <- coeftest(agriculture.mod2, vcov = vcovCL(agriculture.mod2, type = 'HC1'))
wetlands.mod2x <- coeftest(wetlands.mod2, vcov = vcovCL(wetlands.mod2, type = 'HC1'))

# Results

stargazer(water.mod, development.mod, barren.mod, forests.mod, shrublands.mod, grasslands.mod,
          agriculture.mod, wetlands.mod, type = 'text', omit = c('State'))

stargazer(water.mod2, development.mod2, barren.mod2, forests.mod2, shrublands.mod2, grasslands.mod2,
          agriculture.mod2, wetlands.mod2, type = 'text', omit = c('State'))

stargazer(water.modx, development.modx, barren.modx, forests.modx, shrublands.modx, grasslands.modx,
          agriculture.modx, wetlands.modx, type = 'text', omit = c('State'))

stargazer(water.mod2x, development.mod2x, barren.mod2x, forests.mod2x, shrublands.mod2x, grasslands.mod2x,
          agriculture.mod2x, wetlands.mod2x, type = 'text', omit = c('State'))

# Saving results

write.csv(stargazer(water.modx, development.modx, barren.modx, forests.modx, shrublands.modx, grasslands.modx, agriculture.modx, wetlands.modx, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/placebo.txt'), row.names = FALSE)
write.csv(stargazer(water.mod2x, development.mod2x, barren.mod2x, forests.mod2x, shrublands.mod2x, grasslands.mod2x, agriculture.mod2x, wetlands.mod2x, omit.stat = c('f', 'ser'), omit = c('State')), paste0(direc, 'results/placebo2.txt'), row.names = FALSE)

# Saving additional regression stats

f.stats.1 <- rep(393, 8)
f.stats.2 <- rep(70, 8)

nobs.1 <- rep(3106, 8)
nobs.2 <- rep(3106, 8)

additional.stats <- as.data.frame(rbind(f.stats.1, f.stats.2, nobs.1, nobs.2))

write.csv(additional.stats, paste0(direc, 'results/placebo_stats.txt'), row.names = TRUE)

