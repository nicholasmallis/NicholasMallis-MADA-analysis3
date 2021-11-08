

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



# For those symptoms where you have both multiple levels and yes/no, 
# remove all the yes/no versions. That should remove 4 variables.


processeddata <- select(processeddata, -c(WeaknessYN, CoughYN, CoughYN2, MyalgiaYN))

glimpse(processeddata)

# We also want to code the 3 ordinal/multi-level factors as ordered, 
# so make sure they are coded as ordered factors. 
# The order should of course be None/Mild/Moderate/Severe.

#as.factor(data$Weakness)
#table(data$Weakness)

#as.factor(data$CoughIntensity)
#table(data$CoughIntensity)

#as.factor(data$Myalgia)
#table(data$Myalgia)


processeddata <- mutate(processeddata, Weakness = factor(Weakness, levels = c("None", "Mild",
                                                            "Moderate","Severe"),ordered = TRUE))
processeddata <- mutate(processeddata, CoughIntensity = factor(CoughIntensity, levels = c("None", "Mild",
                                                                        "Moderate","Severe"),ordered = TRUE))
processeddata <- mutate(processeddata, Myalgia = factor(Myalgia, levels = c("None", "Mild",
                                                          "Moderate","Severe"),ordered = TRUE))

# But it’s often better to decide manually for each variable based on your scientific 
# expertise if you want to remove it or not. We’ll take that approach here.
# After looking at the data, we decide to remove those binary predictors that 
# have <50 entries in one category (there are 2). Write code to remove them.

library(table1) #loading the table 1 package

table1 <- table1(~ . , data=processeddata, overall="Total")
table1

# Looks like hearing and vision both don't have more than 50 yes
processeddata <- select(processeddata, -c(Hearing, Vision))


# location to save file
save_data_location <- here::here("data","processed_data","processeddata.rds")
saveRDS(processeddata, file = save_data_location)


