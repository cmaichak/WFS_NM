###############################################################################
######################### SET WORKING AREA ####################################
###############################################################################

#LIBRARIES
library(tidyverse)
library(data.table)
library(eeptools)
library(dplyr)
library(readxl)
library(knitr)
library(kableExtra)
library(magick)
library(webshot)

#Set working directory
setwd("C://Users//court//Documents//Colorado State University//WFS")

###############################################################################
############################# READ AND CLEAN DATA #############################
###############################################################################

#Read in New Mexico Department of Health Hospitalization Data
hospitalization <- read.csv("Data//NMDOH_2016-2022//NMDOH_2016-2022.csv") %>%
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
hospitalization$Date <- as.Date(dmy(hospitalization$Date))

#Read in population weighted PM2.5 exposure data
exp_data <- read.csv("Data//AllCountySmoke_Total//AllCountySmoke_Total.csv")

#Convert dates from character to Date class
exp_data$Date <- mdy(exp_data$Date)  

###############################################################################
###################### JOIN EXPOSURE AND OUTCOME DATASETS #####################
###############################################################################

#Join Exposure and Outcome data sets
combined_dataset <- left_join(hospitalization, exp_data,
                          by = c("Date", "County")) %>%
  select(1:15,17,18)

###############################################################################
####################### CREATE PATIENT IDs ####################################
###############################################################################

#Pivot data frame to long format
NMDOH_long <- combined_dataset %>%
  pivot_longer(cols = 4:15,
               names_to = "Primary_dx",
               values_to = "Count")

#Create single observation for each count of Primary_dx, creating unique identifier
#runs line by line, does not like mutate "Patient_ID"              
NMDOH_long_bi <- NMDOH_long %>%
  uncount(., Count) %>% #spreads count for each dx and makes multiple individual observations
  mutate(Patient_DX = substr(Primary_dx, 1,4)) %>%
  mutate(ID = row_number())#grabs each row number and puts it into the column

NMDOH_long_bi <- NMDOH_long_bi %>% #substr(Primary_dx, 1,4))
  mutate(Patient_ID = paste(NMDOH_long_bi$Patient_DX, NMDOH_long_bi$ID)) %>% #combines Patient_Dx and ID to create the unique ID
  mutate(Episode = 1) %>% #adds back each case occurring as 1
  select("Date", "County", "Primary_dx", "Episode", "Patient_ID", "smokepm25", "totalpm25")

#Pivot to wide format for later analyses and fill in NA values
NMWFS_data <- NMDOH_long_bi %>%
  pivot_wider(names_from = Primary_dx,
              values_from = Episode) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% #fills in NA values after pivoting dataset
  distinct(Patient_ID,.keep_all=TRUE) #keeps distinctly different observations

###############################################################################
############################ CASE-CROSSOVER SETUP #############################
###############################################################################

#Dataset too large for RAM, Breaking into Fire months and Non-fire months

#Fire
NMWFS_data_fire <- NMWFS_data %>%
  filter(grepl("-04-|-05-|-06-|-07-|-08-", Date)) %>% #fire months (Apr.-Aug.)
  filter(Date > "2016-01-01")

#Creating Case-CrossOver Dataset
dates <- NMWFS_data_fire[,c("Date","County","Patient_ID", "smokepm25", "totalpm25")]

dates <- dates[rep(seq_len(nrow(dates)), each = 53), ]
dates$index <- rep(-26:26,nrow(dates)/53)
ref_dates <- dates %>%
  mutate(Date=Date+(index*7)) %>%
  filter(grepl("-04-|-05-|-06-|-07-|-08-", Date)) %>% #fire months (Apr.-Aug.)
  filter(Date>"2016-01-01")
ref_dates <- ref_dates %>%
  filter(index!=0) %>%
  select(-index)

replaceNA <- function(x) (ifelse(is.na(x),0,x))
NMWFS_fire <- bind_rows(NMWFS_data_fire, ref_dates) %>%
  arrange(Date, County) %>%
  select(1:17) %>%
  mutate_at(c("respiratory_dx","asthma_dx","copd_dx","bronchitis_dx",
              "pneumonia_dx","cardiovascular_dx","arrythmia_dx",
              "cardiacarrest_dx","cerebrovascular_dx","heartfailure_dx",
              "ischemic_dx","myocardial_dx"), replaceNA)

###############################################################################

#Non-fire
NMWFS_data_nonfire <- NMWFS_data %>%
  filter(grepl("-01-|-02-|-03-|-09-|-10-|-11-|-12-", Date)) %>% #non-fire months (Jan.- Mar., Sep.-Dec.)
  filter(Date > "2016-01-01")

#Creating Case-CrossOver Dataset
dates <- NMWFS_data_nonfire[,c("Date","County","Patient_ID", "smokepm25", "totalpm25")]

dates <- dates[rep(seq_len(nrow(dates)), each = 53), ]
dates$index <- rep(-26:26,nrow(dates)/53)
ref_dates <- dates %>%
  mutate(Date=Date+(index*7)) %>%
  filter(grepl("-01-|-02-|-03-|-09-|-10-|-11-|-12-", Date)) %>% #non-fire months (Jan.- Mar., Sep.-Dec.)
  filter(Date>"2016-01-01")
ref_dates <- ref_dates %>%
  filter(index!=0) %>%
  select(-index)

replaceNA <- function(x) (ifelse(is.na(x),0,x))
NMWFS_nonfire <- bind_rows(NMWFS_data_nonfire, ref_dates) %>%
  arrange(Date, County) %>%
  select(1:17) %>%
  mutate_at(c("respiratory_dx","asthma_dx","copd_dx","bronchitis_dx",
              "pneumonia_dx","cardiovascular_dx","arrythmia_dx",
              "cardiacarrest_dx","cerebrovascular_dx","heartfailure_dx",
              "ischemic_dx","myocardial_dx"), replaceNA)

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

#Run lag function for fire dataset on exposure data
NMWFS_full_fire <- NMWFS_fire %>%
  group_by(County) %>%
  mutate(., !!!funlag(smokepm25,5),!!!funlag(totalpm25,5))
#,!!!funlag(max_tmpf,5),!!!funlag(ARITHMETIC.MEAN,5))

#Save and write combined dataset to .csv file
write_csv(NMWFS_full_fire,"Data//Clean_Data//NMWFS_full_fire.csv")

################################################################################

#Run lag function for non-fire dataset on exposure data
NMWFS_full_nonfire <- NMWFS_nonfire %>%
  group_by(County) %>%
  mutate(., !!!funlag(smokepm25,5),!!!funlag(totalpm25,5))
#,!!!funlag(max_tmpf,5),!!!funlag(ARITHMETIC.MEAN,5))

#Save and write combined dataset to .csv file
write_csv(NMWFS_full_nonfire,"Data//Clean_Data//NMWFS_full_nonfire.csv")

################################################################################
######################## CREATE CASE-CROSSOVER LISTS ###########################
################################################################################

#Fire months

#Create list of casecrossover dataframes
asthma_full <- NMWFS_full_fire %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(asthma_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=asthma_dx) %>%
  mutate(out_name="asthma")
copd_full <- NMWFS_full_fire %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(copd_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=copd_dx) %>%
  mutate(out_name="copd")
pneumonia_full <- NMWFS_full_fire %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(pneumonia_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=pneumonia_dx) %>%
  mutate(out_name="pneumonia")
bronchitis_full <- NMWFS_full_fire %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(bronchitis_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=bronchitis_dx) %>%
  mutate(out_name="bronchitis")
respiratory_full <- NMWFS_full_fire %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(respiratory_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=respiratory_dx) %>%
  mutate(out_name="respiratory")
arrythmia_full <- NMWFS_full_fire %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(arrythmia_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=arrythmia_dx) %>%
  mutate(out_name="arrythmia")
cardiacarrest_full <- NMWFS_full_fire %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(cardiacarrest_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=cardiacarrest_dx) %>%
  mutate(out_name="cardiacarrest")
cerebrovascular_full <- NMWFS_full_fire %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(cerebrovascular_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=cerebrovascular_dx) %>%
  mutate(out_name="cerebrovascular")
heartfailure_full <- NMWFS_full_fire %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(heartfailure_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=heartfailure_dx) %>%
  mutate(out_name="heartfailure")
ischemic_full <- NMWFS_full_fire %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(ischemic_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=ischemic_dx) %>%
  mutate(out_name="ischemic")
myocardial_full <- NMWFS_full_fire %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(myocardial_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=myocardial_dx) %>%
  mutate(out_name="myocardial")
cardiovascular_full <- NMWFS_full_fire %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(cardiovascular_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=cardiovascular_dx) %>%
  mutate(out_name="cardiovascular")

fire_casecross_list <- list(asthma_full,bronchitis_full,copd_full,pneumonia_full,
                       respiratory_full,arrythmia_full,cardiacarrest_full,
                       cerebrovascular_full,heartfailure_full,
                       ischemic_full,myocardial_full,
                       cardiovascular_full)

#Save the crasscross_list object as a .rds file
saveRDS(fire_casecross_list,"Data//Clean_Data//fire_casecross_list.rds")

################################################################################

#Non-fire

#Create list of casecrossover dataframes
asthma_full <- NMWFS_full_nonfire %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(asthma_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=asthma_dx) %>%
  mutate(out_name="asthma")
copd_full <- NMWFS_full_nonfire %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(copd_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=copd_dx) %>%
  mutate(out_name="copd")
pneumonia_full <- NMWFS_full_nonfire %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(pneumonia_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=pneumonia_dx) %>%
  mutate(out_name="pneumonia")
bronchitis_full <- NMWFS_full_nonfire %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(bronchitis_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=bronchitis_dx) %>%
  mutate(out_name="bronchitis")
respiratory_full <- NMWFS_full_nonfire %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(respiratory_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=respiratory_dx) %>%
  mutate(out_name="respiratory")
arrythmia_full <- NMWFS_full_nonfire %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(arrythmia_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=arrythmia_dx) %>%
  mutate(out_name="arrythmia")
cardiacarrest_full <- NMWFS_full_nonfire %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(cardiacarrest_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=cardiacarrest_dx) %>%
  mutate(out_name="cardiacarrest")
cerebrovascular_full <- NMWFS_full_nonfire %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(cerebrovascular_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=cerebrovascular_dx) %>%
  mutate(out_name="cerebrovascular")
heartfailure_full <- NMWFS_full_nonfire %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(heartfailure_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=heartfailure_dx) %>%
  mutate(out_name="heartfailure")
ischemic_full <- NMWFS_full_nonfire %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(ischemic_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=ischemic_dx) %>%
  mutate(out_name="ischemic")
myocardial_full <- NMWFS_full_nonfire %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(myocardial_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=myocardial_dx) %>%
  mutate(out_name="myocardial")
cardiovascular_full <- NMWFS_full_nonfire %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(cardiovascular_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=cardiovascular_dx) %>%
  mutate(out_name="cardiovascular")

nonfire_casecross_list <- list(asthma_full,bronchitis_full,copd_full,pneumonia_full,
                       respiratory_full,arrythmia_full,cardiacarrest_full,
                       cerebrovascular_full,heartfailure_full,
                       ischemic_full,myocardial_full,
                       cardiovascular_full)

#Save the crasscross_list object as a .rds file
saveRDS(nonfire_casecross_list,"Data//Clean_Data//nonfire_casecross_list.rds")

