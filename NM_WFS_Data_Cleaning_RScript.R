###############################################################################
######################### SET WORKING AREA ####################################
###############################################################################

#LIBRARIES
library(tidyverse)
library(lubridate)
library(data.table)
library(eeptools)
library(dplyr)
library(readxl)
#library(knitr)
#library(kableExtra)
#library(magick)
#library(webshot)

#Set working directory
setwd("D://Colorado State University//WFS")

###############################################################################
############################# READ AND CLEAN DATA #############################
###############################################################################

#Read in New Mexico Department of Health Hospitalization Data
NMDOH_hospitalization <- read.csv("Data//NMDOH_2016-2022//NMDOH_2016-2022_v2.csv") %>%
  select("Admission_Date", "Patient_Resi_County_FIPS_Cd", 4,
         "resp_prim", "asthma_prim", "copd_prim", "pneu_prim", "bron_prim",
         "cardio_prim", "cardiac_prim", "arrythmia_prim","heartfail_prim",
         "ischemic_prim", "mi_prim", "cerebro_prim") %>%
  rename("Date" = "Admission_Date",
         "County" = "Patient_Resi_County_FIPS_Cd",
         "Total_Cases" = 3,
         "respiratory_dx" = "resp_prim",
         "asthma_dx" = "asthma_prim", 
         "copd_dx" = "copd_prim", 
         "pneumonia_dx" ="pneu_prim", 
         "bronchitis_dx" = "bron_prim",
         "cardiovascular_dx" = "cardio_prim", 
         "cardiacarrest_dx" = "cardiac_prim", 
         "arrythmia_dx" = "arrythmia_prim",
         "heartfailure_dx" = "heartfail_prim",
         "ischemic_dx" = "ischemic_prim", 
         "myocardial_dx" = "mi_prim", 
         "cerebrovascular_dx" = "cerebro_prim") %>%
  mutate(County = recode(County, 
                         '1' = 'Bernalillo',
                         '3' = 'Catron',
                         '5' = 'Chaves',
                         '6' = 'Cibola',
                         '7' = 'Colfax',
                         '9' = 'Curry',
                         '11' = 'De Baca',
                         '13' = 'Doña Ana',
                         '15' = 'Eddy',
                         '17' = 'Grant',
                         '19' = 'Guadalupe',
                         '21' = 'Harding',
                         '23' = 'Hidalgo',
                         '25' = 'Lea',
                         '27' = 'Lincoln',
                         '28' = 'Los Alamos',
                         '29' = 'Luna',
                         '31' = 'McKinley',
                         '33' = 'Mora',
                         '35' = 'Otero',
                         '37' = 'Quay',
                         '39' = 'Rio Arriba',
                         '41' = 'Roosevelt',
                         '43' = 'Sandoval',
                         '45' = 'San Juan',
                         '47' = 'San Miguel',
                         '49' = 'Santa Fe',
                         '51' = 'Sierra',
                         '53' = 'Socorro',
                         '55' = 'Taos',
                         '57' = 'Torrance',
                         '59' = 'Union',
                         '61' = 'Valencia'))

#Convert Date from "01JAN2016" format to "2016-01-01"
NMDOH_hospitalization$Date <- as.Date(dmy(NMDOH_hospitalization$Date))

###############################################################################
################## CREATE PATIENT IDs & MONTH/YEAR COLUMNS #####################
###############################################################################

#Pivot data frame to long format
NMDOH_long <- NMDOH_hospitalization %>%
  pivot_longer(cols = 4:15,
               names_to = "Primary_dx",
               values_to = "Count")

#Create single observation for each count of Primary_dx, creating unique identifier
NMDOH_long_bi <- NMDOH_long %>%
  uncount(., Count) %>% #spreads count for each dx and makes multiple individual observations
  mutate(Patient_DX = substr(Primary_dx, 1,4)) %>%
  mutate(ID = row_number())#grabs each row number and puts it into the column

NMDOH_long_bi <- NMDOH_long_bi %>% #substr(Primary_dx, 1,4))
  mutate(Patient_ID = paste(NMDOH_long_bi$Patient_DX, NMDOH_long_bi$ID)) %>% #combines Patient_Dx and ID to create the unique ID
  mutate(Episode = 1) %>% #adds back each case occurring as 1
  select("Date", "County", "Primary_dx", "Episode", "Patient_ID")

#Pivot to wide format for later analyses and fill in NA values
NMDOH_wide <- NMDOH_long_bi %>%
  pivot_wider(names_from = Primary_dx,
              values_from = Episode) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% #fills in NA values after pivoting dataset
  distinct(Patient_ID,.keep_all=TRUE) #keeps distinctly different observations

#Create Year and Month columns
NMDOH_data <- NMDOH_wide %>%
  mutate(Year = format(NMDOH_wide$Date, "%Y"),
         Month = format(NMDOH_wide$Date, "%m")) %>%
  select(1,16,17,2:15)

###############################################################################
############################ CASE-CROSSOVER SETUP #############################
###############################################################################

#Filter out study period of interest
NMWFS_data <- NMDOH_data %>%
  filter(grepl("-04-|-05-|-06-|-07-|-08-|-09-", Date)) %>% #Fire months (Apr-Aug)
  #filter(grepl("-01-|-02-|-03-|-10-|-11-|-12-", Date)) %>% #Non-fire
  filter(Date > "2016-01-01")

#Creating Case-Crossover Dataset
dates <- NMWFS_data[,c(1:5)] #Date, Year, Month, County, Patient_ID

dates <- dates[rep(seq_len(nrow(dates)), each = 53), ]
dates$index <- rep(-26:26,nrow(dates)/53)
ref_dates <- dates %>%
  mutate(Date=Date+(index*7)) %>%
  filter(grepl("-04-|-05-|-06-|-07-|-08-|-09-", Date)) %>% #fire months (Apr.-Aug.)
  #filter(grepl("-01-|-02-|-03-|-10-|-11-|-12-", Date)) %>%
  filter(Date>"2016-01-01") 
ref_dates <- ref_dates %>%
  filter(index!=0) %>%
  select(-index)

replaceNA <- function(x) (ifelse(is.na(x),0,x))

NMWFS_data <- bind_rows(NMWFS_data, ref_dates) %>%
  arrange(Date, County) %>%
  mutate_at(c("respiratory_dx","asthma_dx","copd_dx","bronchitis_dx",
              "pneumonia_dx","cardiovascular_dx","arrythmia_dx",
              "cardiacarrest_dx","cerebrovascular_dx","heartfailure_dx",
              "ischemic_dx","myocardial_dx"), replaceNA)

################################################################################
######################### READ IN EXPOSURE DATA ################################
################################################################################

#PM2.5 Data

#Read in population weighted PM2.5 exposure data
PM_data <- read.csv("Data//AllCountySmoke_Total//AllCountySmoke_KO_2016-2022.csv") %>%
  select("Date", "County", "smokepm25", "totalpm25")


#Convert dates from character to Date class
PM_data$Date <- ymd(PM_data$Date) 

#PM_data <- PM_data %>%
#  mutate(Year = format(PM_data$Date, "%Y"),
#         Month = format(PM_data$Date, "%m"))


#PM_data %>%
#  group_by(Date) %>%
#  ggplot(aes(x = Date, y = smokepm25)) +
#  geom_point() +
#  geom_smooth(color = "red", linewidth = 0.7) +
#  facet_wrap(~Year, scales = "free") +
#  ylab("Smoke PM2.5") +
#  xlab("Date")
  

#Heat Index Data

#Read in populaiton weighted Heat Index exposure data
HI_data <- read_csv("D://Colorado State University//WFS//Data//HI_PopWeighted//max_HI_county_pop_weighted.csv")

#Clean HI data
HI_data <- HI_data %>%
  select(2:2559) %>%
  mutate(county = recode(county, 
                         '35001' = 'Bernalillo',
                         '35003' = 'Catron',
                         '35005' = 'Chaves',
                         '35006' = 'Cibola',
                         '35007' = 'Colfax',
                         '35009' = 'Curry',
                         '35011' = 'De Baca',
                         '35013' = 'Doña Ana',
                         '35015' = 'Eddy',
                         '35017' = 'Grant',
                         '35019' = 'Guadalupe',
                         '35021' = 'Harding',
                         '35023' = 'Hidalgo',
                         '35025' = 'Lea',
                         '35027' = 'Lincoln',
                         '35028' = 'Los Alamos',
                         '35029' = 'Luna',
                         '35031' = 'McKinley',
                         '35033' = 'Mora',
                         '35035' = 'Otero',
                         '35037' = 'Quay',
                         '35039' = 'Rio Arriba',
                         '35041' = 'Roosevelt',
                         '35043' = 'Sandoval',
                         '35045' = 'San Juan',
                         '35047' = 'San Miguel',
                         '35049' = 'Santa Fe',
                         '35051' = 'Sierra',
                         '35053' = 'Socorro',
                         '35055' = 'Taos',
                         '35057' = 'Torrance',
                         '35059' = 'Union',
                         '35061' = 'Valencia')) %>%
  rename("County" = "county")

#Pivot to workable format, and joining data to hospitalization
HI_data <- HI_data %>%
  pivot_longer(cols = 2:2558,
               names_to = "Date",
               values_to = "HeatIndex")

#convert column from character to numeric
HI_data$Date <- as.numeric(HI_data$Date)

#Convert date from unix format to date YYYY-MM-DD
HI_data$Date <- as_date(HI_data$Date)

###############################################################################
###################### CREATING LAG FUNCTION FOR EXPOSURES ####################
###############################################################################

#Defining the lag
funlag <- function(var, n=6){
  var <- enquo(var)
  indices <- seq_len(n)
  map(indices, ~quo(lag(!!var, !!.x)) ) %>% 
    set_names(sprintf("%s_lag%d", rlang::quo_text(var), indices))
}

exp_data <- left_join(PM_data, HI_data,
                      by = c("Date", "County"))

#Run 5-day lag function on exposure data
exp_data <- exp_data %>%
  group_by(County) %>%
  mutate(., !!!funlag(smokepm25,5),!!!funlag(totalpm25,5),!!!funlag(HeatIndex,5))

###############################################################################
###################### JOIN EXPOSURE AND OUTCOME DATASETS #####################
###############################################################################

#Join Exposure and Outcome data sets
full_dataset <- left_join(NMWFS_data, exp_data,
                              by = c("Date", "County"))

################################################################################
##################### SAVE CLEANED DATASETS AS .CSV FILES ######################
################################################################################

#Save and write combined fire season dataset to .csv file
write_csv(full_dataset,"Data//Clean_Data//full_dataset.csv")

################################################################################
######################## CREATE CASE-CROSSOVER LISTS ###########################
################################################################################

#Create list of casecrossover dataframes
asthma_full <- full_dataset %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(asthma_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=asthma_dx) %>%
  mutate(out_name="asthma")
copd_full <- full_dataset %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(copd_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=copd_dx) %>%
  mutate(out_name="copd")
pneumonia_full <- full_dataset %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(pneumonia_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=pneumonia_dx) %>%
  mutate(out_name="pneumonia")
bronchitis_full <- full_dataset %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(bronchitis_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=bronchitis_dx) %>%
  mutate(out_name="bronchitis")
respiratory_full <- full_dataset %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(respiratory_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=respiratory_dx) %>%
  mutate(out_name="respiratory")
arrythmia_full <- full_dataset %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(arrythmia_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=arrythmia_dx) %>%
  mutate(out_name="arrythmia")
cardiacarrest_full <- full_dataset %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(cardiacarrest_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=cardiacarrest_dx) %>%
  mutate(out_name="cardiacarrest")
cerebrovascular_full <- full_dataset %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(cerebrovascular_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=cerebrovascular_dx) %>%
  mutate(out_name="cerebrovascular")
heartfailure_full <- full_dataset %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(heartfailure_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=heartfailure_dx) %>%
  mutate(out_name="heartfailure")
ischemic_full <- full_dataset %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(ischemic_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=ischemic_dx) %>%
  mutate(out_name="ischemic")
myocardial_full <- full_dataset %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(myocardial_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=myocardial_dx) %>%
  mutate(out_name="myocardial")
cardiovascular_full <- full_dataset %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(cardiovascular_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=cardiovascular_dx) %>%
  mutate(out_name="cardiovascular")

casecross_list <- list(asthma_full,bronchitis_full,copd_full,pneumonia_full,
                            respiratory_full,arrythmia_full,cardiacarrest_full,
                            cerebrovascular_full,heartfailure_full,
                            ischemic_full,myocardial_full,
                            cardiovascular_full)

#Save the crasscross_list object as a .rds file
saveRDS(casecross_list,"Data//Clean_Data//casecross_list.rds")
