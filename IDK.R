library(dplyr)
library(tibbletime)
library(lubridate)

a2 <- read_xlsx("Covid-19 Dataset.xlsx", sheet = "Cases_Time-Series")
a3 <- as_tbl_time(a2, index = Date)
finddate <- as.Date(Sys.Date()) %m-% months(1)
a3 <- filter_time(a3, finddate~ 'end')


