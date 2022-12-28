# Zeynep Gunes Ozkan 2022, Dissertation Preprocess of Raw Data
# Bournemouth University

# This script uses raw data from Qualtrics

# Required libraries and functions

library(readxl)
library(dplyr)
library(stringr)
source("functions.R")

#### TR ####

data <- read_excel("TR_Data.xlsx")

# Cleaning the Raw Data

dataTR <- subset(data, data$Nationality == 1)

dataTR <- dataTR[ , -which(names(dataTR) %in% c("StartDate","EndDate",
                                                "Status","IPAddress","Duration (in seconds)",
                                                "Finished","RecordedDate","ResponseId",
                                                "RecipientLastName","RecipientFirstName",
                                                "RecipientEmail","ExternalReference",
                                                "LocationLatitude", "LocationLongitude",
                                                "DistributionChannel", "UserLanguage","email",
                                                "inform consent","Q15"))]
dataTR <- dataTR[ , -which(names(dataTR) %in% c("Gender_4_TEXT","Ethnicity_5_TEXT","Education_7_TEXT",
                                                "Marital_5_TEXT", "Employment_8_TEXT","Purpose_10_TEXT"))]

dataTR <- transform(dataTR[grep("\\d+", dataTR$Age),,drop=F], Age= as.numeric(as.character(Age)))

dataTR$Age <- as.numeric(dataTR$Age)

dataTR <- subset(dataTR, dataTR$Age < 100)

dataTR <- subset(dataTR, dataTR$Age >= 18)

dataTR <- subset(dataTR, dataTR$Progress == 100)

# UCLA & OCI 

dataTR$OCI_socsup <- NA
dataTR$OCI_dep <- NA
dataTR$OCI_imp <- NA
dataTR$OCI_att <- NA

revTR <- dataTR %>% uclaRev() %>% UclaScore() %>% OCIrev() %>% OCItotal() %>% OCIatt() %>%
  OCIdep() %>% OCIimp() %>% OCIsocsup()

save(revTR, file= "dataTR.Rda")
write.csv(revTR,"dataTR.csv")

#### UK ####

data2 <- read_excel("UK_Data.xlsx")

# Cleaning the Raw Data

dataUK <- subset(data2, data2$Nationality == 1)

for( i in 1:nrow(dataUK)){
  if(dataUK$Nationality[i] == 1){
    dataUK$Nationality[i] <- 2
  }
}

dataUK <- dataUK[ , -which(names(dataUK) %in% c("StartDate","EndDate",
                                                "Status","IPAddress","Duration (in seconds)",
                                                "Finished","RecordedDate","ResponseId",
                                                "RecipientLastName","RecipientFirstName",
                                                "RecipientEmail","ExternalReference",
                                                "LocationLatitude", "LocationLongitude",
                                                "DistributionChannel", "UserLanguage","email",
                                                "inform consent","Q15"))]

dataUK <- dataUK[ , -which(names(dataUK) %in% c("Gender_4_TEXT","Ethnicity_5_TEXT",
                                                "Marital_5_TEXT", "Employment_8_TEXT","Purpose_10_TEXT"))]


dataUK <- transform(dataUK[grep("\\d+", dataUK$Age),,drop=F], Age= as.numeric(as.character(Age)))

dataUK$Age <- as.numeric(dataUK$Age)

dataUK <- subset(dataUK, dataUK$Age < 100)

dataUK <- subset(dataUK, dataUK$Age >= 18)

dataUK <- subset(dataUK, dataUK$Progress == 100)

# UCLA & OCI

dataUK$OCI_socsup <- NA
dataUK$OCI_dep <- NA
dataUK$OCI_imp <- NA
dataUK$OCI_att <- NA

revUK <- dataUK %>% uclaRev() %>% UclaScore() %>% OCIrev() %>% OCItotal() %>% OCIatt() %>%
  OCIdep() %>% OCIimp() %>% OCIsocsup()


save(revUK, file= "dataUK.Rda")
write.csv(revUK,"dataUK.csv")

#### MIXED ####

dataMix <- rbind(revTR, revUK)

### Grouping Purpose ###

# work/educ <- 1,2,3
# free time <- 5,6,7
# social media/news/communication <- 4,8,9

# 1= only work/educ
# 2= only free time
# 3= only social media/news/communication
# 4= work/educ & free time
# 5= work/educ & social media/news/communication
# 6= free time & social media/news/communication
# 7= all 

dataMix <- dataMix[ , -which(names(dataMix) %in% c("ucla_1","ucla_2","ucla_3","ucla_4","ucla_5",
                                                   "ucla_6","ucla_7","ucla_8","ucla_9","ucla_10","ucla_11","ucla_12",
                                                   "ucla_13", "ucla_14","ucla_15", "ucla_16","ucla_17","ucla_18","ucla_19","ucla_20",
                                                   "OCI_1","OCI_2","OCI_3","OCI_4","OCI_5","OCI_6","OCI_7","OCI_8","OCI_9","OCI_10",
                                                   "OCI_11","OCI_12","OCI_13","OCI_14","OCI_15","OCI_16","OCI_17","OCI_18","OCI_19","OCI_20",
                                                   "OCI_21","OCI_22","OCI_23","OCI_24","OCI_25","OCI_26","OCI_27","OCI_28","OCI_29","OCI_30",
                                                   "OCI_31","OCI_32","OCI_33","OCI_34","OCI_35","OCI_36"))]

dataMix$Purpose <- as.character(dataMix$Purpose)
dataMix$Purpose_G <- NA

for(i in 1:nrow(dataMix)){
  if(grepl('[1 2 3]',dataMix$Purpose[i]) == TRUE && grepl('[4 5 6 7 8 9]',dataMix$Purpose[i]) == FALSE){
    dataMix$Purpose_G[i] <- 1
  }else{
    if(grepl('[5 6 7]',dataMix$Purpose[i]) == TRUE && grepl('[1 2 3 4 8 9]',dataMix$Purpose[i]) == FALSE){
      dataMix$Purpose_G[i] <- 2
    }else{
      if(grepl('[4 8 9]',dataMix$Purpose[i]) == TRUE && grepl('[1 2 3 5 6 7]',dataMix$Purpose[i]) == FALSE){
        dataMix$Purpose_G[i] <- 3
      }else{
        if(grepl('[1 2 3 5 6 7]',dataMix$Purpose[i]) == TRUE && grepl('[4 8 9]',dataMix$Purpose[i]) == FALSE){
          dataMix$Purpose_G[i] <- 4
        }else{
          if(grepl('[1 2 3 4 8 9]',dataMix$Purpose[i]) == TRUE && grepl('[5 6 7]',dataMix$Purpose[i]) == FALSE){
            dataMix$Purpose_G[i] <- 5
          }else{
            if(grepl('[4 5 6 7 8 9]',dataMix$Purpose[i]) == TRUE && grepl('[1 2 3]',dataMix$Purpose[i]) == FALSE){
              dataMix$Purpose_G[i] <- 6
            }else{
              if(grepl('[1 2 3]',dataMix$Purpose[i]) == TRUE && grepl('[5 6 7]',dataMix$Purpose[i]) == TRUE && grepl('[4 8 9]',dataMix$Purpose[i]) == TRUE){
                dataMix$Purpose_G[i] <- 7
              }
            }
          }
        }
      }
    }
  }
}

# dataMix <- dataMix[ , -which(names(dataMix) %in% "Purpose")]

save(dataMix, file= "dataMix.Rda")
write.csv(dataMix,"dataMix.csv")

