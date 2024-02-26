#PACKAGES
library(tidyverse)
library(data.table)
library(lubridate)
library(dplyr)
library(readxl)
library(rnoaa) #NOAA ASOS network, relative humidity and temperature
library(riem) #NOAA ASOS network, relative humidity and temperature
library(foreach)
library(doParallel)
library(RAQSAPI)
library(sf)
library(leaflet)
library(maps)
library(mapview)
library(gifski)

#source("helper_functions/riem_helper_functions.R")

#Load Daily Smoke Files from NOAA
urlBase <- 'https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile'
destfile <- "temp.zip" #make a folder and add something into empty folder to zip it
dates <- seq.Date(from=as.Date("2022-01-01"),to=as.Date("2022-12-31"),by="day") 

# loop takes 2.5 minutes on my computer, downloads about 10 MB of data
system.time({
  smokepolys <- foreach(date = dates, .packages=c("sf","tidyverse"), .combine="rbind")%do%{ #file input/output usually can't be parallelized
    yyyy <- str_sub(date,1,4)
    mm <- str_sub(date,6,7)
    #dd <- str_sub(date,9,10)
    dt <- str_replace_all(date,pattern="-",replacement="")
    fileURL <- str_c(urlBase, yyyy, mm, str_c("hms_smoke",dt,".zip"), sep ="/")
    destfile <- str_c("temp.zip")
    try_error <- try(
      download.file(url=fileURL, destfile=destfile)
    )
    if( !(class(try_error) == "try-error") ){
      unzip(zipfile=destfile, 
            exdir=str_c("temp.zip",dt))
      file.remove(destfile)
      shape <- st_read(str_c("temp.zip",dt))
      if(identical(colnames(shape),c("Satellite","Start","End","Density","geometry"))){
        return(shape %>% 
                 mutate(Date = date))
      }else{
        return(NULL)
      }
    }else{
      return(NULL)
    }
  }
})#end system.time

save(x=smokepolys,file="US_HMSSmokePolygons_2022.RData")


#Loading 2022 PM2.5 data for New Mexico using RAQSAPI
#Load RAQSAPI credentials
RAQSAPI::aqs_credentials(username="your username",
                         key="your key")

#Load 2022 PM2.5 data for New Mexico(35)
PM25 <- aqs_dailysummary_by_state(parameter="88101",#PM2.5 in ug/m3
                                  bdate=as.Date("2022-01-01"),
                                  edate=as.Date("2022-12-31"),
                                  stateFIPS= c(35)) %>%  
  filter(pollutant_standard=="PM25 Annual 2012") %>% #There are duplicate concentrations (rows) because the form of the standard didn't change from 2006 to 2012
  st_as_sf(coords=c("longitude","latitude")) %>%
  st_set_crs("EPSG:4326") %>% #WGS 84
  mutate(Date = as.Date(date_local),
         Pollutant = "PM2.5") %>%
  rename("Concentration"="arithmetic_mean") %>%
  select(geometry,state_code,county_code,site_number,Date,Pollutant,Concentration) #retains the 24hr avg concentration


#Organize smoke plumes and PM2.5 monitors by date

#Split PM2.5 Monitor by date
PM_datesplit <- PM25 %>%
  group_by(Date) %>%
  group_split()

#Split smoke plume by date
smoke_datesplit <- smokepolys %>%
  group_by(Date) %>%
  group_split


#Mapping smoke plumes during the Calf Canyon and Hermits Peak Wildfires

#Read in fire polygons
cchp_fire <- st_read("hermits_peak//nm3571810539920220406_20210612_20220615_burn_bndy.shp")

#Set color palette
pal <- colorNumeric(palette="Spectral",seq(-3.0, 71.2,by=10),reverse=TRUE)

#Get unique days for both datasets
days_smoke <- smokepolys$Date %>% 
  unique() %>% 
  sort()

days_pm <- PM25$Date %>% 
  unique() %>% 
  sort()

#Fires occured April 6, 2022 - August 21, 2022
#daynumber 96 - 233 (Julian Day Range)

#Create base layer map
mapStates <- map("state", fill = TRUE, plot = TRUE)

#Create sequences 
datenumber <- as.vector(format(seq(as.Date("2022-04-06"), as.Date("2022-08-21"), by = "days"), format = "%m-%d-%Y"))
daynumber <- as.vector(c(96:233))

#Function to create timeseries of smoke polygon leaflet maps
timeseries <- function(daynumber, datenumber) {
  print(daynumber)
  #leaflet
  leaflet(mapStates) %>%
    addTiles() %>%
    setView(lat = 34.3, lng = -106, zoom = 7) %>%
    addControl(datenumber, position = "topleft") %>%
    addPolygons(data = st_transform(st_as_sf(cchp_fire), 4326),
                color = "darkred",
                weight = 1,
                fillOpacity = 1) %>%
    addPolygons(data = smokepolys %>%
                  filter(Date==days_smoke[daynumber]),
                color = "grey",
                opacity = 0) %>%
    addCircleMarkers(data = PM25 %>% 
                       filter(Date==days_pm[daynumber]), 
                     weight = 1,                           #thickness of the outer ring
                     color="black",                        #color of outer ring
                     fillOpacity = 1,
                     fillColor = ~pal(Concentration)) %>% #parameter to set the color scheme
    addLegend(pal = pal, 
              values = 1:55,
              title = "Average Daily PM2.5 </br>Concentration") %>%
    mapshot(file = paste0(daynumber,"_map.png")) #Save each image
}

map2(daynumber, datenumber, timeseries, .progress = FALSE) #Iterates through each day ###Order of argument matters!!!

png_files <- list.files("timeseries_smoke/", pattern = ".png", full.names = TRUE)
gifski(png_files, gif_file = "GIFS//WFSanimation.gif", delay = 0.5)
