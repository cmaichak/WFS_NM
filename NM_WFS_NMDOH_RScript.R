#New Mexico Department of Health Data Cleaning and Setting up Case-Crossover Study Design
#title: "WFS_NM"
#author: "Courtney Maichak"
#date: "2024-02-17"

#NEW MEXICO CALF CANYON/HERMIT PEAK WILDFIRE SMOKE ANALYSIS (2022) cASE-CROSSOVER STUDY, 2016-2022
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

#Read in NM DOH Hospitalization Data
hospitalization <- read.csv(
  "C://Users//court//OneDrive//Documents//Colorado State University//WFS//Data//NMDOH_Data_for_CSU//NMDOH_Data_for_CSU.csv") %>%
  select("Admission_Date", "Patient_Resi_County_FIPS_Cd", 4,
         "resp_prim", "asthma_prim", "copd_prim", "pneu_prim", "bron_prim",
         "cardio_prim", "cardiac_prim", "arrythmia_prim","heartfail_prim",
         "ischemic_prim", "mi_prim", "cerebro_prim") %>%
  rename("Date" = "Admission_Date",
         "County_FIPS" = "Patient_Resi_County_FIPS_Cd",
         "Total_Cases" = 4,
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
         "cerebrovascular_dx" = "cerebro_prim")

#converts date to correct format
hospitalization$Date <- dmy(hospitalization$Date) 

NMDOH <- hospitalization %>%
  filter(grepl("-04-|-05-|-06-|-07-|-08-", Date)) %>% #fire months (Apr.-Aug.)
  filter(Date > "2016-01-01")

#cardiorespiratory_dx <- c(NMDOH$respiratory_dx, NMDOH$cardiovascular_dx)

NMDOH <- NMDOH %>%
  mutate(prindx_sub=substr(prindx,1,3)) %>%
  mutate(asthma=ifelse(prindx_sub %in% asthma_dx,1,0),
         copd=ifelse(prindx_sub %in% copd_dx,1,0),
         pneumonia=ifelse(prindx_sub %in% pneumonia_dx,1,0),
         bronchitis=ifelse(prindx_sub %in% bronchitis_dx,1,0),
         respiratory=ifelse(prindx_sub %in% respiratory_dx,1,0),
         arrythmia=ifelse(prindx_sub %in% arrythmia_dx,1,0),
         cerebrovascular=ifelse(prindx_sub %in% cerebrovascular_dx,1,0),
         heartfailure=ifelse(prindx_sub %in% heartfailure_dx,1,0),
         ischemic=ifelse(prindx_sub %in% ischemic_dx,1,0),
         myocardial=ifelse(prindx_sub %in% myocardial_dx,1,0),
         cardiovascular=ifelse(prindx_sub %in% cardiovascular_dx,1,0),
         heatillness=ifelse(prindx_sub %in% heatillness_dx,1,0),
         forearmfracture=ifelse(prindx_sub %in% forearmfracture_dx,1,0),
         cardiorespiratory=ifelse(prindx_sub %in% cardiorespiratory_dx,1,0)) %>%
  filter(enctype==2) %>%
  filter(placesvc==1) %>%
  distinct(hfdrepisode,.keep_all=TRUE)

NMDOH_cardioresp <- NMDOH %>%
  filter(cardiorespiratory==1) 

###############################################################################
#Setting up Study

#Defining the lag
funlag <- function(var, n=6){
  var <- enquo(var)
  indices <- seq_len(n)
  map(indices, ~quo(lag(!!var, !!.x)) ) %>% 
    set_names(sprintf("%s_lag%d", rlang::quo_text(var), indices))
}

exp_data <- exp_data %>%
  group_by(county) %>%
  mutate(., !!!funlag(mean_relh,5),!!!funlag(mean_tmpf,5), 
         !!!funlag(max_tmpf,5),!!!funlag(ARITHMETIC.MEAN,5))

###############################################################################
#Creating Case-CrossOver Dataset
dates <- HFDR_data[,c("Date","county","hfdrepisode","race","age","sex","ethnicity")]

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
HFDR_full <- bind_rows(HFDR_data,ref_dates) %>%
  arrange(county,Date) %>%
  mutate_at(c("asthma","copd","pneumonia","bronchitis","respiratory","arrythmia",
              "cerebrovascular","heartfailure","ischemic","myocardial","cardiovascular",
              "heatillness","forearmfracture","cardiorespiratory"),replaceNA)

full_dataset <- left_join(HFDR_full,exp_data,by=c("county","Date","quarter"))
write_csv(full_dataset,"../clean_data/full_HFDR_dataset.csv")

#'Create list of casecrossover dataframes
asthma_full <- full_dataset %>%
  group_by(hfdrepisode) %>%
  mutate(n=length(unique(asthma))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=asthma) %>%
  mutate(out_name="asthma")
copd_full <- full_dataset %>%
  group_by(hfdrepisode) %>%
  mutate(n=length(unique(copd))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=copd) %>%
  mutate(out_name="copd")
pneumonia_full <- full_dataset %>%
  group_by(hfdrepisode) %>%
  mutate(n=length(unique(pneumonia))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=pneumonia) %>%
  mutate(out_name="pneumonia")
bronchitis_full <- full_dataset %>%
  group_by(hfdrepisode) %>%
  mutate(n=length(unique(bronchitis))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=bronchitis) %>%
  mutate(out_name="bronchitis")
respiratory_full <- full_dataset %>%
  group_by(hfdrepisode) %>%
  mutate(n=length(unique(respiratory))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=respiratory) %>%
  mutate(out_name="respiratory")
arrhythmia_full <- full_dataset %>%
  group_by(hfdrepisode) %>%
  mutate(n=length(unique(arrythmia))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=arrythmia) %>%
  mutate(out_name="arrhythmia")
cerebrovascular_full <- full_dataset %>%
  group_by(hfdrepisode) %>%
  mutate(n=length(unique(cerebrovascular))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=cerebrovascular) %>%
  mutate(out_name="cerebrovascular")
heartfailure_full <- full_dataset %>%
  group_by(hfdrepisode) %>%
  mutate(n=length(unique(heartfailure))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=heartfailure) %>%
  mutate(out_name="heartfailure")
ischemic_full <- full_dataset %>%
  group_by(hfdrepisode) %>%
  mutate(n=length(unique(ischemic))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=ischemic) %>%
  mutate(out_name="ischemic")
myocardial_full <- full_dataset %>%
  group_by(hfdrepisode) %>%
  mutate(n=length(unique(myocardial))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=myocardial) %>%
  mutate(out_name="myocardial")
cardiovascular_full <- full_dataset %>%
  group_by(hfdrepisode) %>%
  mutate(n=length(unique(cardiovascular))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=cardiovascular) %>%
  mutate(out_name="cardiovascular")
cardiorespiratory_full <- full_dataset %>%
  group_by(hfdrepisode) %>%
  mutate(n=length(unique(cardiorespiratory))) %>%
  filter(n==2) %>%
  select(-n) %>%
  rename(outcome=cardiorespiratory) %>%
  mutate(out_name="cardiorespiratory")

casecross_list <- list(asthma_full,copd_full,pneumonia_full,bronchitis_full,
                       respiratory_full,arrhythmia_full,cerebrovascular_full,
                       ischemic_full,myocardial_full,heartfailure_full,
                       cardiovascular_full,cardiorespiratory_full)

saveRDS(casecross_list,"../clean_data/casecross_list.rds")