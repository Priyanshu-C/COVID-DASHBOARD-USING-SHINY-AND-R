library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
library(plotly)
library(DT)
library(readxl)
library(dplyr)
library(ggplot2)
library(maptools)
library(rgeos)
library(ggmap)
library(scales)
library(RColorBrewer)
library(lubridate)
library(tibbletime)
library(sjmisc)
set.seed(8000)

#Importing Global RawData
globaldata <- read_xlsx("Covid-19 Dataset.xlsx")
globaldata2 <- read.csv("patients_data.csv")
globalTSD <- read.csv("date_wise_data.csv")


#For PLotting Heatmap of India
#Cleaning for the Map 
StateData <- subset(globaldata2, select = detected_state)
StateData <- as.data.frame(table(StateData))

StateRecovered <- filter(globaldata2,current_status == 'Recovered')
StateRecovered <- subset(StateRecovered, select = detected_state)
StateRecovered <- as.data.frame(table(StateRecovered))

StateHospitalized <- filter(globaldata2,current_status == 'Hospitalized')
StateHospitalized <- subset(StateHospitalized, select = detected_state)
StateHospitalized <- as.data.frame(table(StateHospitalized))

StateDeceased <- filter(globaldata2,current_status == 'Deceased')
StateDeceased <- subset(StateDeceased, select = detected_state)
StateDeceased <- as.data.frame(table(StateDeceased))

FinalMAPData<- merge(StateData,StateRecovered,by.x = "StateData",by.y = "StateRecovered")
FinalMAPData<- merge(FinalMAPData,StateHospitalized,by.x = "StateData",by.y = "StateHospitalized")
FinalMAPData <- rename(FinalMAPData,"Active" = "Freq.x")
FinalMAPData <- rename(FinalMAPData,"Hospitalized" = "Freq")
FinalMAPData <- rename(FinalMAPData,"Recovered" = "Freq.y")
FinalMAPData<- merge(FinalMAPData,StateDeceased,by.x = "StateData",by.y = "StateDeceased")
FinalMAPData <- rename(FinalMAPData,"Deceased" = "Freq")
FinalMAPData <- rename(FinalMAPData,"State" = "StateData")

          MAP_DATA <- FinalMAPData
          shp <- readShapeSpatial('Admin2.shp')
          shp.f <- fortify(shp, region = "ST_NM")
          mapve <- MAP_DATA %>% rename(id = State)
          merge.shp.coef<-merge(shp.f,mapve, by="id", all.x=TRUE)
          final.plot<-merge.shp.coef[order(merge.shp.coef$order), ]

#DATA CLEANING FOR GENDER CHART

gender <- subset(globaldata2,select = gender)
gender <- as.data.frame(table(gender))
gender[1,1] <- NA

#DATA CLEANING FOR AGE PLOT

age <- read_xlsx("Covid-19 Dataset.xlsx")
age <- subset(age, select = `Age Bracket`)
age <- filter(age, `Age Bracket` !=  ' ') 
age <- age %>% rename(age = `Age Bracket`)

age_18 <- filter(age, age < 18)
age_30 <- filter(age, age >= 18 & age <30)
age_70 <- filter(age, age >= 30 & age <70)
age_100 <- filter(age, age > 70)

#DATA CLEANING FOR Nationality PLOT

Nationality <- subset(globaldata, select = Nationality)
Nationality <- as.data.frame(table(Nationality))
Nationality <- filter(Nationality,Freq > 1)


#DATA CLEANING FOR HEADERDATA
HEADERDATA <- subset(globaldata2, select = current_status)
HEADERDATA <- as.data.frame(table(HEADERDATA))
HEADERDATAR <- filter(HEADERDATA, HEADERDATA == "Recovered" )
HEADERDATAA <- filter(HEADERDATA, HEADERDATA == "Hospitalized" )
HEADERDATAD <- filter(HEADERDATA, HEADERDATA == "Deceased" )
 
age_plot <- data.frame(AgeRange = c("Below 18","Between 18 and 30","Between 30 and 70","Above 70"),age =c(nrow(age_18),nrow(age_30),nrow(age_70),nrow(age_100)))

#Color Schemes
confirmed_color <- "purple"
active_color <- "#1f77b4"
recovered_color <- "forestgreen"
death_color <- "red"

excel_sheets("Covid-19 Dataset.xlsx")


