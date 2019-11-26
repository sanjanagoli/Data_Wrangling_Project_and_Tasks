## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----Setup---------------------------------------------------------------
library(RODBC)
library(sqldf)
library(dplyr)
library(ggplot2)
myconn<-odbcConnect("dartmouth", "sgoli", "sgoli@qbs181")


## ----Problem 1-----------------------------------------------------------
IC_PhoneCall<-sqlQuery(myconn, "select * from PhoneCall")
sqlQuery(myconn, "ALTER TABLE [sgoli].PhoneCall ADD EnrollmentGroup VARCHAR(255)")
sqlQuery(myconn, "UPDATE [sgoli].PhoneCall
SET
    EnrollmentGroup = 
    CASE
        WHEN PhoneCall_Encounter.EncounterCode = '125060000' THEN 'Clinical Alert'
        WHEN PhoneCall_Encounter.EncounterCode = '125060001' THEN 'Health Coaching'
        WHEN PhoneCall_Encounter.EncounterCode = '125060002' THEN 'Technical Question'
        WHEN PhoneCall_Encounter.EncounterCode = '125060003' THEN 'Administrative'
        WHEN PhoneCall_Encounter.EncounterCode = '125060004' THEN 'Other'
        WHEN PhoneCall_Encounter.EncounterCode = '125060005' THEN 'Lack of Engagement'
    END
FROM
    [sgoli].PhoneCall
Inner JOIN
    PhoneCall_Encounter
ON
    [sgoli].PhoneCall.tri_CustomerIDEntityReference = PhoneCall_Encounter.CustomerId
")

# saving the changes to the table
IC_PhoneCall<-sqlQuery(myconn, "select * from [sgoli].PhoneCall")

# outputting 10 random rows
sample_n(IC_PhoneCall, 10)


## ----Problem 2-----------------------------------------------------------
# creating a table of counts based on Enrollment Group
IC_CountEnrollmentGroup<-data.frame(table("EnrollmentGroup" = IC_PhoneCall$EnrollmentGroup))

# sample_n for 10 rows will not work because table has few than 10 rows
head(IC_CountEnrollmentGroup, 10)



## ----Problem 3-----------------------------------------------------------
# Merge the Phone call encounter table with Call duration table.
IC_PCEncounterCallDuration<-sqlQuery(myconn, "select distinct * from CallDuration A INNER JOIN PhoneCall_Encounter B on A.tri_CustomerIDEntityReference = B.CustomerId")

# outputting 10 random rows
sample_n(IC_PCEncounterCallDuration, 10)


## ----Problem 4-----------------------------------------------------------
IC_CallDuration<-sqlQuery(myconn, "SELECT * FROM CallDuration")

# Obtaining Counts for CallType and CallOutcome
IC_CallTypeCount<-data.frame(table("CallType" = IC_CallDuration$CallType))
IC_CallOutcomeCount<-data.frame(table("CallOutcome" = IC_CallDuration$CallOutcome))

# Using SQL Query to find the average callduration based on each EnrollmentGroup / EncounterCode
IC_AvgCallDuration<-sqlQuery(myconn, "SELECT EncounterCode, avg(Cast(CallDuration as int)) as 'Average Call Duration' from CallDuration A INNER JOIN PhoneCall_Encounter B on A.tri_CustomerIDEntityReference = B.CustomerId
                             GROUP BY EncounterCode
                             ORDER BY EncounterCode")

# outputting 10 random rows
head(IC_CallTypeCount, 10)
head(IC_CallOutcomeCount, 10)
head(IC_AvgCallDuration, 10)


## ----Problem 5-----------------------------------------------------------
# Merging the tables
IC_DemConditionsText<-sqlQuery(myconn, "SELECT * from Demographics A INNER JOIN Conditions B on A.contactid = B.tri_patientid INNER JOIN TextMessages C on B.tri_patientid = C.tri_contactId")

# Converting the date column into Date data type
IC_DemConditionsText$TextSentDate<-as.Date(IC_DemConditionsText$TextSentDate, "%m/%d/%y")
IC_DemConditionsText$TextSentDate<-IC_DemConditionsText$TextSentDate[ order(IC_DemConditionsText$TextSentDate , decreasing = FALSE )]

# Creating a column that assigns a week to the date
IC_DemConditionsText<-IC_DemConditionsText%>% 
    mutate(week = cut.Date(IC_DemConditionsText$TextSentDate, breaks = "1 week", labels = FALSE)) %>% 
    arrange(IC_DemConditionsText$TextSentDate)

# Creates a table that organizes into week, senderType, and frequency
IC_TextPerWeekBySender<-data.frame(table(IC_DemConditionsText$week, IC_DemConditionsText$SenderName))
names(IC_TextPerWeekBySender) <- c("Week", "SenderType", "Frequency")

# plotting data
ggplot(IC_TextPerWeekBySender, aes(Week, Frequency, colour = SenderType, group=SenderType)) + geom_line()

# outputting rows of table
sample_n(IC_TextPerWeekBySender, 10)



## ----Problem 6-----------------------------------------------------------
# Computing counts of Texts based on week and condition
IC_TextPerWeekByCondition<-data.frame(table(IC_DemConditionsText$week, IC_DemConditionsText$tri_name))
names(IC_TextPerWeekByCondition) <- c("Week", "Condition", "Frequency")
    
# Plotting data
ggplot(IC_TextPerWeekByCondition, aes(Week, Frequency, colour = Condition, group=Condition)) + geom_line()

# outputting rows of table
sample_n(IC_TextPerWeekByCondition, 10)


