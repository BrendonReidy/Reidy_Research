# A new script with some combined collections analyses/graphs for the end of the year report
library(ggplot2)
library(readr)
library(lubridate)
library(tidyverse)
library(gganimate)
library(dplyr)
library(transformr)


# 1. Pathing -----------------------------------------------------------------
path.google <- "~/Google Drive/My Drive" # Mac
path.dat <- file.path(path.google,"/LivingCollections_Phenology/Data_Observations")
#path.figs <- file.path(path.google, "LivingCollections_Phenology/Reports/2023_02_EndOfYear_Report/figures_2023_end")
# this is for google -># 
path.figs <- "~/Google Drive/My Drive/LivingCollections_Phenology/Reports/2024_02_End_Of_Year_Report/figures_2024_end"
if(!dir.exists("../data")) dir.create("../data/")
if(!dir.exists("../figures/")) dir.create("../figures/")


# 2. Arb Data------


##* Reading in the historic data for the previous years----

dat.22 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_All_2022_FINAL.csv"))
dat.21 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_All_2021_FINAL.csv"))
dat.20<- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_All_2020_FINAL.csv"))
dat.19 <- read.csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_All_2019_FINAL.csv"))
dat.18 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_All_2018_FINAL.csv"))
#reading in 2023 data
acer23<- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_Acer_2023_FINAL.csv"))
quercus23 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_Quercus_2023_FINAL.csv"))
ulmus23 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_Ulmus_2023_FINAL.csv"))
##binding 
dat.23<- rbind(ulmus23, quercus23, acer23)

#reading in 2024 data
acer24<- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_Acer_2024_FINAL.csv"))
quercus24 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_Quercus_2024_FINAL.csv"))
ulmus24 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_Ulmus_2024_FINAL.csv"))
##binding 
dat.24<- rbind(ulmus24, quercus24, acer24)
##one line to bind them all
dat.all <- rbind(dat.24,dat.23, dat.22, dat.21, dat.20, dat.19, dat.18)


##* Formatting date and checks----
#getting the correct date format
dat.all$yday <- lubridate::yday(dat.all$Date.Observed)
dat.all$Date <- as.Date(paste0("2018-", dat.all$yday), format="%Y-%j")

##werid 2027 value
summary(dat.all)
#removing the "Q.macrocarpa" collection because that is not necessary for the end of year pheology report 
dat.all <- dat.all[!(dat.all$Collection %in% c("Q. macrocarpa", "Tilia")), ]

#Checking what is present in the collection column to see if it's been removed
unique(dat.all$Collection)

##* Setting a spring only data frame---- 
#because we did not make observations in spring 2020
dat.spring <- dat.all[dat.all$Year != "2020", ]
#further excluding all elm observations for 2021 since we did not observe phenology that spring
dat.spring <- dat.spring[!(dat.spring$Collection == "Ulmus" & dat.spring$Year == "2021"), ]
##checking unique values of the Year and Collection column in dat.spring
unique(dat.spring$Year)
unique(dat.spring$Collection)
##somewhere we have a year listed as 2027..come back to this. 
dat.huh <- dat.all[dat.all$Year == "2027", ]
head(dat.huh)
#it's dale in 12/11/ 2022

dat.oak<-dat.spring[dat.spring$Collection=="Quercus",]

head(dat.oak)
dat.oaks<-dat.oak[dat.oak$Species %in% c("Quercus acerifolia", "Quercus coccinea","Quercus gambelii", "Quercus ilicifolia", 
                  "Quercus marilandica", "Quercus palustris", "Quercus phellos", "Quercus shumardii", "Quercus velutina"),]

#Leaf buds spec----
dat.lb <- dat.oaks[dat.oaks$leaf.breaking.buds.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.breaking.buds.observed","Date", "Collection")]
dat.lb <- dat.lb[!is.na(dat.lb$PlantNumber),]
summary(dat.lb)
head(dat.lb)

#finding the minimimum and maximum range and mean of the dates breaking leaf buds were observed on our trees.
#Note the na.rm=T which is removing N/A values
min(dat.lb$Date.Observed)
max(dat.lb$Date.Observed)
range(dat.lb$Date.Observed)
mean(dat.lb$Date.Observed,na.rm=T)

#Now make my Yday
dat.lb$yday <- lubridate::yday(dat.lb$Date.Observed)
summary(dat.lb)


#only looking at trees that showed breaking leaf buds in the first half of the year
dat.lb <- dat.lb [dat.lb$yday<=180 &dat.lb$yday>=61,]
summary(dat.lb)

#aggregating quercus.lf so it shows me the date of first breaking leaf buds for  every plant number and species 
breaking.buds <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.lb, FUN=min, na.rm=T)
summary(breaking.buds)
head(breaking.buds)

breaking.buds$Date <- as.Date(paste0("2024-", breaking.buds$yday), format="%Y-%j")

##* Graphing----

ggplot(data=breaking.buds) +
  facet_grid(Species~ ., scales="free_y") + 
  geom_histogram(alpha=0.25, aes(x=Date, fill=as.factor(Year), color=as.factor(Year))) +
  scale_x_date(date_labels="%b %d", date_breaks="1 week") +  
  scale_fill_manual(name="Year", values=c("2018"="#CC79A7", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"= "#F0E442", "2023"="purple3","2024"="orange" )) +
  scale_color_manual(name="Year", values=c("2018"="#CC79A7", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"="#F0E442", "2023"= "purple3","2024"="orange")) +
  theme_bw() +
  labs(title="Mean Day of First Breaking Leaf Buds", x="Date", fill="Year")+
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 90, hjust = 1, size= 6),
        strip.text.y = element_text(angle = 0, hjust = 0, size = 8), # Adjusts facet text
        strip.text.x = element_text(size = 8),                       # Adjusts year labels
        strip.text = element_text(margin = margin(5,5,5,5)))         # Adds padding around facet text



# Open flowers----
dat.fo <- dat.oaks[dat.oaks$flower.open.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.open.observed","Date", "Collection")]
dat.fo <- dat.fo[!is.na(dat.fo$PlantNumber),]
summary(dat.fo)
head(dat.fo)

#finding the minimimum and maximum range and mean of the dates open flowers were observed on our trees.
#Note the na.rm=T which is removing N/A values
min(dat.fo$Date.Observed)
max(dat.fo$Date.Observed)
range(dat.fo$Date.Observed)
mean(dat.fo$Date.Observed,na.rm=T)

#Now make my Yday
dat.fo$yday <- lubridate::yday(dat.fo$Date.Observed)
summary(dat.fo)


#only looking at trees that showed open flowers in the first half of the year
#dat.fo <- dat.fo [dat.fo$yday<=180,]
summary(dat.fo)

#aggregating quercus.lf so it shows me the date of open flowers for  every plant number and species 
flower.open <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.fo, FUN=min, na.rm=T)
summary(flower.open)
head(flower.open)

flower.open$Date <- as.Date(paste0("2024-", flower.open$yday), format="%Y-%j")


##* Graphing----

ggplot(data=flower.open) +
  facet_grid(Species~., scales="free_y") + 
  geom_histogram(alpha=0.25, aes(x=Date, fill=as.factor(Year), color=as.factor(Year))) +
  scale_x_date(date_labels="%b %d", date_breaks="1 week") +  
  scale_fill_manual(name="Year", values=c("2018"="#CC79A7", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"= "#F0E442", "2023"="purple3","2024"="orange" )) +
  scale_color_manual(name="Year", values=c("2018"="#CC79A7", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"="#F0E442", "2023"= "purple3","2024"="orange")) +
  theme_bw() +
  labs(title="Mean Day of First Flowers", x="Date", fill="Year")+
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 75, hjust = 1),
        strip.text.y = element_text(angle = 0, hjust = 0, size = 8), # Adjusts facet text
        strip.text.x = element_text(size = 8),                       # Adjusts year labels
        strip.text = element_text(margin = margin(5,5,5,5)))         # Adds padding around facet text

# leaves present spec----
dat.lp <- dat.oaks[dat.oaks$leaf.present.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.present.observed","Date", "Collection")]
dat.lp <- dat.lp[!is.na(dat.lp$PlantNumber),]
summary(dat.lp)
head(dat.lp)

     #finding the minimimum and maximum range and mean of the dates leaf present was observed on our trees.
     #Note the na.rm=T which is removing N/A values
     min(dat.lp$Date.Observed)
     max(dat.lp$Date.Observed)
     range(dat.lp$Date.Observed)
     mean(dat.lp$Date.Observed,na.rm=T)
     
     #Now make my Yday
     dat.lp$yday <- lubridate::yday(dat.lp$Date.Observed)
     summary(dat.lp)
     
     
     #only looking at trees that showed leaf present in the first 200 days of the year
     dat.lp <- dat.lp [dat.lp$yday >= 65 &dat.lp$yday<=200,]
     #summary(dat.lp)
     
     #aggregating quercus.lf so it shows me the date of first leaf present for  every plant number and species 
     leaves.present <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.lp, FUN=min, na.rm=T)
     summary(leaves.present)
     head(leaves.present)
     
     leaves.present$Date <- as.Date(paste0("2024-", leaves.present$yday), format="%Y-%j")

##* Graphing----
ggplot(data=leaves.present) +
       facet_grid(Species~., scales="free_y") + 
       geom_histogram(alpha=0.25, aes(x=Date, fill=as.factor(Year), color=as.factor(Year))) +
       scale_x_date(date_labels="%b %d", date_breaks="1 week") +  
       scale_fill_manual(name="Year", values=c("2018"="#CC79A7", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"= "#F0E442", "2023"="purple3","2024"="orange" )) +
       scale_color_manual(name="Year", values=c("2018"="#CC79A7", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"="#F0E442", "2023"= "purple3","2024"="orange")) +
       theme_bw() +
       labs(title="Mean Day of First Leaf", x="Date", fill="Year")+
       theme(legend.position = "right",
             axis.text.x = element_text(angle = 75, hjust = 1),
             strip.text.y = element_text(angle = 0, hjust = 0, size = 8), # Adjusts facet text
             strip.text.x = element_text(size = 8),                       # Adjusts year labels
             strip.text = element_text(margin = margin(5,5,5,5)))         # Adds padding around facet text
     
     
     # part 2----
dat.oaks2<-dat.oak[dat.oak$PlantNumber %in% c("1173-2004*5", "145-75*8","260-93*1", "362-96*7", 
                                         "467-85*1", "10-2013*1", "540-96*2", "185-92*55", "18-2013*1"),]

     #Leaf buds ----
     dat.lb <- dat.oaks2[dat.oaks2$leaf.breaking.buds.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.breaking.buds.observed","Date", "Collection")]
     dat.lb <- dat.lb[!is.na(dat.lb$PlantNumber),]
     summary(dat.lb)
     head(dat.lb)
     
     #finding the minimimum and maximum range and mean of the dates breaking leaf buds were observed on our trees.
     #Note the na.rm=T which is removing N/A values
     min(dat.lb$Date.Observed)
     max(dat.lb$Date.Observed)
     range(dat.lb$Date.Observed)
     mean(dat.lb$Date.Observed,na.rm=T)
     
     #Now make my Yday
     dat.lb$yday <- lubridate::yday(dat.lb$Date.Observed)
     summary(dat.lb)
     
     
     #only looking at trees that showed breaking leaf buds in the first half of the year
     dat.lb <- dat.lb [dat.lb$yday<=180 &dat.lb$yday>=61,]
     summary(dat.lb)
     
     #aggregating quercus.lf so it shows me the date of first breaking leaf buds for  every plant number and species 
     breaking.buds <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.lb, FUN=min, na.rm=T)
     summary(breaking.buds)
     head(breaking.buds)
     
     breaking.buds$Date <- as.Date(paste0("2024-", breaking.buds$yday), format="%Y-%j")

##* Graphing----

ggplot(data=breaking.buds) +
  facet_grid(PlantNumber~., scales="free_y") + 
  geom_histogram(alpha=0.25, aes(x=Date, fill=as.factor(Year), color=as.factor(Year))) +
  scale_x_date(date_labels="%b %d", date_breaks="1 weeks") +  
  scale_fill_manual(name="Year", values=c("2018"="#CC79A7", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"= "#F0E442", "2023"="purple3","2024"="orange" )) +
  scale_color_manual(name="Year", values=c("2018"="#CC79A7", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"="#F0E442", "2023"= "purple3","2024"="orange")) +
  theme_bw() +
  labs(title="Mean Day of First Breaking Leaf Buds", x="Date", fill="Year")+
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.y = element_text(angle = 0, hjust = 0, size = 8), # Adjusts facet text
        strip.text.x = element_text(size = 8),                       # Adjusts year labels
        strip.text = element_text(margin = margin(5,5,5,5)))         # Adds padding around facet text



#open flowers---- 
dat.fo <- dat.oaks2[dat.oaks2$flower.open.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.open.observed","Date", "Collection")]
dat.fo <- dat.fo[!is.na(dat.fo$PlantNumber),]
summary(dat.fo)
head(dat.fo)

#finding the minimimum and maximum range and mean of the dates open flowers were observed on our trees.
#Note the na.rm=T which is removing N/A values
min(dat.fo$Date.Observed)
max(dat.fo$Date.Observed)
range(dat.fo$Date.Observed)
mean(dat.fo$Date.Observed,na.rm=T)

#Now make my Yday
dat.fo$yday <- lubridate::yday(dat.fo$Date.Observed)
summary(dat.fo)


#only looking at trees that showed open flowers in the first half of the year
dat.fo <- dat.fo [dat.fo$yday<=180,]
summary(dat.fo)


#aggregating quercus.lf so it shows me the date of open flowers for  every plant number and species 
flower.open <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.fo, FUN=min, na.rm=T)
summary(flower.open)
head(flower.open)

flower.open$Date <- as.Date(paste0("2024-", flower.open$yday), format="%Y-%j")

##* Graphing----

ggplot(data=flower.open) +
  facet_grid(PlantNumber~., scales="free_y") + 
  geom_histogram(alpha=0.25, aes(x=Date, fill=as.factor(Year), color=as.factor(Year))) +
  scale_x_date(date_labels="%b %d", date_breaks="1 week") +  
  scale_fill_manual(name="Year", values=c("2018"="#CC79A7", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"= "#F0E442", "2023"="purple3","2024"="orange" )) +
  scale_color_manual(name="Year", values=c("2018"="#CC79A7", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"="#F0E442", "2023"= "purple3","2024"="orange")) +
  theme_bw() +
  labs(title="Mean Day of First Flowers", x="Date", fill="Year")+
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 65, hjust = 1),
        strip.text.y = element_text(angle = 0, hjust = 0, size = 8), # Adjusts facet text
        strip.text.x = element_text(size = 8),                       # Adjusts year labels
        strip.text = element_text(margin = margin(5,5,5,5)))         # Adds padding around facet text

# leaves present----
dat.lp <- dat.oaks2[dat.oaks2$leaf.present.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.present.observed","Date", "Collection")]
dat.lp <- dat.lp[!is.na(dat.lp$PlantNumber),]
summary(dat.lp)
head(dat.lp)

#finding the minimimum and maximum range and mean of the dates leaf present was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(dat.lp$Date.Observed)
max(dat.lp$Date.Observed)
range(dat.lp$Date.Observed)
mean(dat.lp$Date.Observed,na.rm=T)

#Now make my Yday
dat.lp$yday <- lubridate::yday(dat.lp$Date.Observed)
summary(dat.lp)


#only looking at trees that showed leaf present in the first 200 days of the year
dat.lp <- dat.lp [dat.lp$yday >= 75 &dat.lp$yday<=200,]
#summary(dat.lp)

#aggregating quercus.lf so it shows me the date of first leaf present for  every plant number and species 
leaves.present <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.lp, FUN=min, na.rm=T)
summary(leaves.present)
head(leaves.present)

leaves.present$Date <- as.Date(paste0("2024-", leaves.present$yday), format="%Y-%j")


##* Graphing----

ggplot(data=leaves.present) +
  facet_grid(PlantNumber~., scales="free_y") + 
  geom_histogram(alpha=0.25, aes(x=Date, fill=as.factor(Year), color=as.factor(Year))) +
  scale_x_date(date_labels="%b %d", date_breaks="1 week") +  
  scale_fill_manual(name="Year", values=c("2018"="#CC79A7", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"= "#F0E442", "2023"="purple3","2024"="orange" )) +
  scale_color_manual(name="Year", values=c("2018"="#CC79A7", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"="#F0E442", "2023"= "purple3","2024"="orange")) +
  theme_bw() +
  labs(title="Mean Day First Leaves", x="Date", fill="Year")+
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 85, hjust = 1, size = 6.5),
        strip.text.y = element_text(angle = 0, hjust = 0, size = 8), # Adjusts facet text
        strip.text.x = element_text(size = 8),                       # Adjusts year labels
        strip.text = element_text(margin = margin(6,6,6,6)))         # Adds padding around facet text
