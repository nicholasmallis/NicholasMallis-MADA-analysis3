

###############################
# processing script

library(dplyr) #for data processing
library(here) #to set paths

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","raw_data","SympAct_Any_Pos.Rda")

#load data. 
rawdata <- readRDS(data_location)

#take a look at the data
dplyr::glimpse(rawdata)

#printing
print(rawdata)

#Remove all variables that have Score or Total or FluA or FluB or 
#Dxname or Activity in their name. Also remove the variable 
#Unique.Visit

#Here I remove columns by number
processeddata <- rawdata[, -c(1:8, 41:63)]

#Here I use na.omit to remove NA observations
processeddata <- na.omit(processeddata) 

#checking
glimpse(processeddata)

# location to save file
save_data_location <- here::here("data","processed_data","processeddata.rds")
saveRDS(processeddata, file = save_data_location)


