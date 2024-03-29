## ----setup---------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

library(sqldf)
library(RODBC)
library(dplyr)
library(forcats)
library(tidyr)
library(knitr)
library(lubridate)



## ----initializing--------------------------------------------------------
# connecting to SQL server
myconn<-odbcConnect("dartmouth", "sgoli", "sgoli@qbs181")

# reading in table with read.csv
IC_BP <- read.csv(file="~/IC_BP_v2.csv", header = TRUE, sep = ',')


## ----#1a and b: Using sqlQuery (SQL Commands) to create a new column called BPStatus that has a dichotomous outcome----

## 1 a and b: converting to dichotomous outcomes (0, 1)

# change name of BPAlerts to BPStatus

colnames(IC_BP)[colnames(IC_BP)=="BPAlerts"] <- "BPStatus"

sample_n(IC_BP, 10) # printing 10 random rows

# convert to dichotomous outcomes
IC_BP <- IC_BP %>% mutate(BPStatus = ifelse(BPStatus == "Hypo1" | BPStatus == "Normal", 1, 0))

sample_n(IC_BP, 10) # printing 10 random rows



## ----#1c: Merge / join with Demographics table to get EnrollmentDate-----

Demographics <- sqlQuery(myconn, "select * from Demographics")
IC_BP <- sqldf("select a.*, b.tri_imaginecareenrollmentemailsentdate as EnrollmentDate from IC_BP a
INNER JOIN Demographics b
ON b.contactid = a.ID")

# cleaning up results: convert EnrollmentDate comment to date type using lubridate
IC_BP$EnrollmentDate <- mdy(IC_BP$EnrollmentDate)

sample_n(IC_BP, 10) # printing 10 random rows



## ----#1d: 12-week interval based on EnrollmentDate-----------------------


# Finding 12-week interval for each patient using Lubridate
IC_BP$Twelve_End_Date <- IC_BP$EnrollmentDate
week(IC_BP$Twelve_End_Date) <- week(IC_BP$Twelve_End_Date) + 12

# converting Observed time to date format (5 digit serial conversion -- dates from 1900)
IC_BP$ObservedTime_Date <- as.Date("1/1/1900", format = "%m/%d/%Y")
day(IC_BP$ObservedTime_Date) <- day(IC_BP$ObservedTime_Date) + IC_BP$ObservedTime

# finding only the measurements taken that fall between the patient's 12 week interval
IC_BP_12Week <- subset(IC_BP, IC_BP$ObservedTime_Date >= EnrollmentDate & IC_BP$ObservedTime_Date < IC_BP$Twelve_End_Date)

# computing the average status for each by summing BPStatus and dividing by total number of measurements
Average_Status <- sqldf("SELECT ID, avg(BPStatus) Mean 
      FROM IC_BP_12Week 
      GROUP BY ID")

# Joining the Average status column with bigger table so that you can see for each measurement
IC_BP_12Week <- sqldf("SELECT a.*, b.Mean Average_Status from IC_BP_12Week a
  INNER JOIN
    Average_Status b on a.ID = b.ID")

# Rounding up or down according to get a dichotomous Average BP Status
IC_BP_12Week$Average_Status <- ifelse(IC_BP_12Week$Average_Status >= 0.5, 1, 0)

sample_n(IC_BP_12Week, 10) # printing 10 random rows



## ----#1e: Compare the scores from baseline (first week) to follow-up scores (12 weeks)----

# order the elements for each ID by ObservedTime. Assumption: ObservedTime likely refers to some sort of timestamp/date format that the BP measurement was sent to the system. Therefore, I thought it would be logical that in order to find the baseline score and the score at the end of the interval, I would order the elements for each ID by ObservedTime and compare the first and last element. 
IC_BP_12Week <- IC_BP_12Week[ order( IC_BP_12Week$ID, IC_BP_12Week$ObservedTime_Date ), ]

sample_n(IC_BP_12Week, 10) # printing random 10 rows that are ordered by contactid and ObservedTime

# Get the first row for each ID group
IC_BP_FirstElement <- IC_BP_12Week %>%
    group_by(ID) %>%
    slice(c(1)) %>%
    ungroup()

# Get the last row for each ID group
IC_BP_LastElement <- IC_BP_12Week %>%
    group_by(ID) %>%
    slice(c(n())) %>%
    ungroup()

# Join the two tables to get side by side comparison of beginning and end status for each ID
IC_BP_FirstLast <- sqldf("SELECT a.ID, a.BPStatus as BPStatus1, b.BPStatus as BPStatus2 
  from IC_BP_FirstElement a
  INNER JOIN IC_BP_LastElement b on a.ID = b.ID
")

# Create a succint conclusion based on comparison of statuses
IC_BP_FirstLast$Conclusion <- ifelse(IC_BP_FirstLast$BPStatus1 == 1 & IC_BP_FirstLast$BPStatus2 == 1, 'Always Controlled', NA)
IC_BP_FirstLast$Conclusion <- ifelse(IC_BP_FirstLast$BPStatus1 == 0 & IC_BP_FirstLast$BPStatus2 == 0 & is.na(IC_BP_FirstLast$Conclusion), 'Always Uncontrolled', IC_BP_FirstLast$Conclusion)
IC_BP_FirstLast$Conclusion <- ifelse(IC_BP_FirstLast$BPStatus1 == 1 & IC_BP_FirstLast$BPStatus2 == 0 & is.na(IC_BP_FirstLast$Conclusion), 'Controlled to Uncontrolled', IC_BP_FirstLast$Conclusion)
IC_BP_FirstLast$Conclusion <- ifelse(IC_BP_FirstLast$BPStatus1 == 0 & IC_BP_FirstLast$BPStatus2 == 1 & is.na(IC_BP_FirstLast$Conclusion), 'Uncontrolled to Controlled', IC_BP_FirstLast$Conclusion)

sample_n(IC_BP_FirstLast, 10) # printing 10 rows with the tables with the conclusions



## ----#1f: How many customers were brought from uncontrolled regime to controlled regime after 12 weeks of intervention----
# Counting the number that have changed from Uncontrolled to Controlled from the beginning and end of care based on conclusions created in part 2
count(subset(IC_BP_FirstLast, Conclusion == 'Uncontrolled to Controlled'))



## ----#2: Merge Demographics, Conditions, TextMessages based on MaxDate with SQL----

# attaching screenshots of SQL Code and output
myimages<-list.files("images/", pattern = ".png", full.names = TRUE)
include_graphics(myimages)




## ----#3: Merge Demographics, Conditions, TextMessages--------------------

## Creating three tables in R from SQL Tables
Demographics <- sqlQuery(myconn, "select * from Demographics")
TextMessages <- sqlQuery(myconn, "select * from TextMessages")
Conditions <- sqlQuery(myconn, "select * from Conditions")

## Merging three tables with inner join
Dem_Con_Text <- merge(Demographics, Conditions, by.x = c("contactid"), by.y = c("tri_patientid"))
Dem_Con_Text <- merge(Dem_Con_Text, TextMessages, by.x = c("contactid"), by.y = c("tri_contactId"))

## Cleaning: converting TextSentDate to date format using Lubridate mdy
Dem_Con_Text$TextSentDate <- mdy(Dem_Con_Text$TextSentDate)

## Ordering the dates based on the contactid group
Dem_Con_Text <- Dem_Con_Text[ order( Dem_Con_Text$contactid, Dem_Con_Text$TextSentDate ), ]

## makes Dem_Con_Text a long table that has multiple rows for same contactid depending on if there are multiple SenderName and Tri_Name entries based on latest TextSentDate
Dem_Con_Text <- Dem_Con_Text %>% 
  group_by(contactid) %>%
  filter(TextSentDate == max(TextSentDate))

# converting to data frame in order perform following functions
Dem_Con_Text <- as.data.frame(Dem_Con_Text)

## creates a wide table that contains only unique patientids; sometimes the latest text sent date will have been sent by two different sender names, so a list is created under SenderName; if there are multiple senders, there will be multiple tri_names from the Conditions, which are all stored in the same column

Dem_Con_Text_OneRow <- Dem_Con_Text %>% group_by(contactid, TextSentDate, Gender_Name = fct_explicit_na(Gender_Name, na_level = NA), gendercode, tri_age, parentcustomeridname, tri_imaginecareenrollmentstatus, address1_stateorprovince, tri_imaginecareenrollmentemailsentdate, tri_enrollmentcompletedate, gender) %>% summarise(SenderNames = paste(SenderName, collapse =","), tri_names = paste(tri_name, collapse =","))


Dem_Con_Text_OneRow <- as.data.frame(Dem_Con_Text_OneRow)
sample_n(Dem_Con_Text_OneRow, 10)


