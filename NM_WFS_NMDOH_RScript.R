#New Mexico Department of Health Data Cleaning and Setting up Case-Crossover Study Design
#title: "WFS_NM"
#author: "Courtney Maichak"
#date: "2024-02-17"

#NEW MEXICO CALF CANYON/HERMITS PEAK WILDFIRE SMOKE ANALYSIS (2022) CASE-CROSSOVER STUDY, 2016-2022
##About Info
#Date Burned: April 6, 2022 - August 21, 2022
#Area Burned: 341,471 acres
#Cause: Escaped prescribed burn (Hermits Peak Fire) & leftover burn piles (Calf Canyon Fire)

#LIBRARIES
library(tidyverse)
library(data.table)
library(lubridate)
library(eeptools)
library(dplyr)
library(readxl)

#CLEANING NEW MEXICO DEPARTMENT OF HEALTH HOSPITALIZATION DATA

#Read in NM DOH Hospitalization Data
hospitalization <- read.csv(
  "C://Users//court//OneDrive//Documents//Colorado State University//WFS//Data//NMDOH_2016-2022//NMDOH_2016-2022.csv") %>%
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
                         '1' = 'Bernilillo',
                         '3' = 'Catron',
                         '5' = 'Chaves',
                         '6' = 'Cibola',
                         '7' = 'Colfax',
                         '9' = 'Curry',
                         '11' = 'De Baca',
                         '13' = 'Dona Ana',
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

hospitalization$Date <- dmy(hospitalization$Date) #converts date to correct format 
#hospitalization$Date <- as.Date(hospitalization$Date)

hospitalization <- hospitalization %>%
  mutate(Year = format(hospitalization$Date, "%Y"))

#create annual summary table
NMDOH_table1 <- hospitalization %>%
  group_by(Year) %>%
  summarise(respiratory_dx = sum(respiratory_dx), 
            asthma_dx = sum(asthma_dx),
            bronchitis_dx = sum(bronchitis_dx),
            copd_dx = sum(copd_dx),
            pneumonia_dx = sum(pneumonia_dx),
            cardiovascular_dx = sum(cardiovascular_dx),
            arrythmia_dx = sum(arrythmia_dx),
            cardiacarrest_dx = sum(cardiacarrest_dx),
            cerebrovascular_dx = sum(cerebrovascular_dx),
            heartfailure_dx = sum(heartfailure_dx),
            ischemic_dx = sum(ischemic_dx),
            myocardial_dx = sum(myocardial_dx))

#kable(NMDOH_table1)

#pivot data frame to long format
NMDOH_long <- hospitalization %>%
  pivot_longer(cols = 4:15,
               names_to = "Primary_dx",
               values_to = "Count")

#create graphs showing daily change for 2016-2020
(NMDOH_figure1 <- ggplot(NMDOH_long) +
    geom_line(aes(x = Date, y = Count)) +
    facet_wrap(~factor(Primary_dx, levels = c('respiratory_dx', 'asthma_dx',
                                              'bronchitis_dx','copd_dx',
                                              'pneumonia_dx',   
                                              'cardiovascular_dx','arrythmia_dx',
                                              'cardiacarrest_dx',
                                              'cerebrovascular_dx',
                                              'heartfailure_dx', 'ischemic_dx',
                                              'myocardial_dx')), ncol = 3))

#Create single observation for each count of Primary_dx 
#runs line by line, does not like mutate "Patient_ID"              
NMDOH_long_bi <- NMDOH_long %>%
  uncount(., Count) %>% #spreads count for each dx and makes multiple individual observations
  #creating unique identifier for checking code later
  mutate(Patient_DX = substr(Primary_dx, 1,4)) %>% #substr(Primary_dx, 1,4))
  mutate(ID = row_number()) %>% #grabs each row number and puts it into the column 
  mutate(Patient_ID = paste(NMDOH_long_bi$Patient_DX, NMDOH_long_bi$ID)) %>% #combines Patient_Dx and ID to create the unique ID
  mutate(Episode = 1) %>% #adds back each case occurring as 1
  select("Date", "Year", "County", "Primary_dx", "Episode", "Patient_ID")


NMDOH_data <- NMDOH_long_bi %>%
  filter(grepl("-04-|-05-|-06-|-07-|-08-", Date)) %>% #fire months (Apr.-Aug.)
  filter(Date > "2016-01-01")

NMDOH_data <- NMDOH_data %>%
  mutate(Prim_dx=substr(Primary_dx,1,4)) %>% #want to keep this info after pivoting
  pivot_wider(names_from = Primary_dx,
              values_from = Episode)

NMDOH_data <- NMDOH_data %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% #fills in NA values after pivoting dataset
  distinct(Patient_ID,.keep_all=TRUE) #keeps distinctly different observations

#cardiorespiratory_dx <- c(NMDOH$respiratory_dx, NMDOH$cardiovascular_dx)

###############################################################################
#Setting up Study

#Defining the lag
funlag <- function(var, n=6){
  var <- enquo(var)
  indices <- seq_len(n)
  map(indices, ~quo(lag(!!var, !!.x)) ) %>% 
    set_names(sprintf("%s_lag%d", rlang::quo_text(var), indices))
}

#exp_data <- exp_data %>%
  #group_by(County) %>%
  #mutate(., !!!funlag(mean_relh,5),!!!funlag(mean_tmpf,5), 
         #!!!funlag(max_tmpf,5),!!!funlag(ARITHMETIC.MEAN,5))

###############################################################################
#Creating Case-CrossOver Dataset
dates <- NMDOH_data[,c("Date","County","Patient_ID")]

dates <- dates[rep(seq_len(nrow(dates)), each = 53), ]
dates$index <- rep(-26:26,nrow(dates)/53)
ref_dates <- dates %>%
  mutate(Date=Date+(index*7)) %>%
  filter(grepl("-04-|-05-|-06-|-07-|-08-",Date)) %>%
  filter(Date>"2016-01-01")
ref_dates <- ref_dates %>%
  filter(index!=0) %>%
  select(-index)

replaceNA <- function(x) (ifelse(is.na(x),0,x))
NMDOH_full <- bind_rows(NMDOH_data,ref_dates) %>%
  arrange(County,Date) %>%
  mutate_at(c("asthma_dx","copd_dx","pneumonia_dx","bronchitis_dx",
              "respiratory_dx","arrythmia_dx","cerebrovascular_dx",
              "heartfailure_dx","ischemic_dx","myocardial_dx",
              "cardiovascular_dx"),replaceNA)

#Join Exposure and Outcome data sets
full_dataset <- left_join(NMDOH_full,exp_data,by=c("County","Date","quarter"))

#Save and write combined dataset to .csv file
write_csv(NMDOH_full_dataset,"../clean_data/NMDOH_full_dataset.csv")

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
arrhythmia_full <- full_dataset %>%
  group_by(Patient_ID) %>%
  mutate(n=length(unique(arrythmia_dx))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=arrythmia_dx) %>%
  mutate(out_name="arrhythmia")
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

casecross_list <- list(asthma_full,copd_full,pneumonia_full,bronchitis_full,
                       respiratory_full,arrhythmia_full,cerebrovascular_full,
                       ischemic_full,myocardial_full,heartfailure_full,
                       cardiovascular_full,cardiorespiratory_full)

#Save the crasscross_list object as a .rds file
saveRDS(casecross_list,"../Clean_Data/casecross_list.rds")