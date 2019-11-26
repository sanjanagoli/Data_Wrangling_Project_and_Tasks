## ----setup---------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(haven)
library(tidyverse)
library(sqldf)
library(data.table)
library(mltools)
library(tidyr)
library(dplyr)

# the haven library has a method read_xpt that allows R to read in SAS data in the form of .xpt
input<-read_xpt("~/Downloads/DIQ_I.XPT")
enhancedData<-input

# Need to convert to data.table in order to perform one hot encoding with one_hot later in the set
enhancedData_dataTable<-data.table(enhancedData, keep.rownames = TRUE)
enhancedData_dataTable<-select(enhancedData_dataTable, -rn)


## ----DID040--------------------------------------------------------------
# replace data that can affect mean (666, 999, 777)
enhancedData_dataTable$DID040<-replace(enhancedData_dataTable$DID040, enhancedData_dataTable$DID040==666, NA)
enhancedData_dataTable$DID040<-replace(enhancedData_dataTable$DID040, enhancedData_dataTable$DID040==777, NA)
enhancedData_dataTable$DID040<-replace(enhancedData_dataTable$DID040, enhancedData_dataTable$DID040==999, NA)

# Replacing missing values with mean of the existing data
enhancedData_dataTable$DID040[is.na(enhancedData_dataTable$DID040)] <- mean(enhancedData_dataTable$DID040, na.rm = TRUE)


## ------------------------------------------------------------------------
# replace data that can affect mean (666, 999, 777)
enhancedData_dataTable$DID060<-replace(enhancedData$DID060, enhancedData_dataTable$DID060==666, NA)
enhancedData_dataTable$DID060<-replace(enhancedData_dataTable$DID060, enhancedData_dataTable$DID060==777, NA)
enhancedData_dataTable$DID060<-replace(enhancedData_dataTable$DID060, enhancedData_dataTable$DID060==999, NA)

# Replace missing values with imputed mean
enhancedData_dataTable$DID060[is.na(enhancedData_dataTable$DID060)] <- mean(enhancedData_dataTable$DID060, na.rm = TRUE)


## ----DID250--------------------------------------------------------------
# replace data that can affect mean (9999, 7777)
enhancedData_dataTable$DID250<-replace(enhancedData_dataTable$DID250, enhancedData_dataTable$DID250==9999, NA)
enhancedData_dataTable$DID250<-replace(enhancedData_dataTable$DID250, enhancedData_dataTable$DID250==7777, NA)

# Replace missing values with imputed mean
enhancedData_dataTable$DID250[is.na(enhancedData_dataTable$DID250)] <- mean(enhancedData_dataTable$DID250, na.rm = TRUE)


## ----DIQ280--------------------------------------------------------------
# replace data for which the code it represents can affect mean (999 [don't know], 777 [refused -- only 2 refused]) --> because there is not real value recorded for these, these are as useful as missing values, so should be replaced with NA in order to faciliate mean imputation
enhancedData_dataTable$DIQ280<-replace(enhancedData_dataTable$DIQ280, enhancedData_dataTable$DIQ280==999, NA)
enhancedData_dataTable$DIQ280<-replace(enhancedData_dataTable$DIQ280, enhancedData_dataTable$DIQ280==777, NA)

enhancedData_dataTable$DIQ280[is.na(enhancedData_dataTable$DIQ280)] <- mean(enhancedData_dataTable$DIQ280, na.rm = TRUE)


## ----DIQ300S-------------------------------------------------------------
# replace data for which the code it represents can affect mean (9999 [don't know], 7777 [refused]) --> because there is not real value recorded for these, these are as useful as missing values, so should be replaced with NA in order to faciliate mean imputation
enhancedData_dataTable$DIQ300S<-replace(enhancedData_dataTable$DIQ300S, enhancedData_dataTable$DIQ300S==9999, NA)
enhancedData_dataTable$DIQ300S<-replace(enhancedData_dataTable$DIQ300S, enhancedData_dataTable$DIQ300S==7777, NA)

enhancedData_dataTable$DIQ300S[is.na(enhancedData_dataTable$DIQ300S)] <- mean(enhancedData_dataTable$DIQ300S, na.rm = TRUE)


## ----DIQ300D-------------------------------------------------------------
# replace data for which the code it represents can affect mean (9999 [don't know], 7777 [refused]) --> because there is not real value recorded for these, these are as useful as missing values, so should be replaced with NA in order to faciliate mean imputation
enhancedData_dataTable$DIQ300D<-replace(enhancedData_dataTable$DIQ300D, enhancedData_dataTable$DIQ300D==9999, NA)
enhancedData_dataTable$DIQ300D<-replace(enhancedData_dataTable$DIQ300D, enhancedData_dataTable$DIQ300D==7777, NA)

enhancedData_dataTable$DIQ300D[is.na(enhancedData_dataTable$DIQ300D)] <- mean(enhancedData_dataTable$DIQ300D, na.rm = TRUE)


## ----DID310S-------------------------------------------------------------
# replace data for which the code it represents can affect mean (9999 [don't know], 6666 [provider did not specify goal], 7777 [refused -- only 1 refused]) --> because there is not real value recorded for these, these are as useful as missing values, so should be replaced with NA in order to faciliate mean imputation
enhancedData_dataTable$DID310S<-replace(enhancedData_dataTable$DID310S, enhancedData_dataTable$DID310S==9999, NA)
enhancedData_dataTable$DID310S<-replace(enhancedData_dataTable$DID310S, enhancedData_dataTable$DID310S==7777, NA)
enhancedData_dataTable$DID310S<-replace(enhancedData_dataTable$DID310S, enhancedData_dataTable$DID310S==6666, NA)

enhancedData_dataTable$DID310S[is.na(enhancedData_dataTable$DID310S)] <- mean(enhancedData_dataTable$DID310S, na.rm = TRUE)


## ----DID310D-------------------------------------------------------------
# replace data for which the code it represents can affect mean (9999 [don't know], 6666 [provider did not specify goal], 7777 [refused]) --> because there is not real value recorded for these, these are as useful as missing values, so should be replaced with NA in order to faciliate mean imputation
enhancedData_dataTable$DID310D<-replace(enhancedData_dataTable$DID310D, enhancedData_dataTable$DID310D==9999, NA)
enhancedData_dataTable$DID310D<-replace(enhancedData_dataTable$DID310D, enhancedData_dataTable$DID310D==7777, NA)
enhancedData_dataTable$DID310D<-replace(enhancedData_dataTable$DID310D, enhancedData_dataTable$DID310D==6666, NA)

enhancedData_dataTable$DID310D[is.na(enhancedData_dataTable$DID310D)] <- mean(enhancedData_dataTable$DID310D, na.rm = TRUE)


## ----DID320--------------------------------------------------------------
# replace data for which the code it represents can affect mean (9999 [don't know], 6666 [provider did not specify goal], 7777 [refused], 5555 [Never Heard of LDL -- 59]) --> because there is not real value recorded for these, these are as useful as missing values, so should be replaced with NA in order to faciliate mean imputation
enhancedData_dataTable$DID320<-replace(enhancedData_dataTable$DID320, enhancedData_dataTable$DID320==9999, NA)
enhancedData_dataTable$DID320<-replace(enhancedData_dataTable$DID320, enhancedData_dataTable$DID320==7777, NA)
enhancedData_dataTable$DID320<-replace(enhancedData_dataTable$DID320, enhancedData_dataTable$DID320==6666, NA)
enhancedData_dataTable$DID320<-replace(enhancedData_dataTable$DID320, enhancedData_dataTable$DID320==5555, NA)

enhancedData_dataTable$DID320[is.na(enhancedData_dataTable$DID320)] <- mean(enhancedData_dataTable$DID320, na.rm = TRUE)


## ----DID330--------------------------------------------------------------
# replace data for which the code it represents can affect mean (9999 [don't know], 6666 [provider did not specify goal], 7777 [refused]) --> because there is not real value recorded for these, these are as useful as missing values, so should be replaced with NA in order to faciliate mean imputation
enhancedData_dataTable$DID330<-replace(enhancedData_dataTable$DID330, enhancedData_dataTable$DID330==9999, NA)
enhancedData_dataTable$DID330<-replace(enhancedData_dataTable$DID330, enhancedData_dataTable$DID330==7777, NA)
enhancedData_dataTable$DID330<-replace(enhancedData_dataTable$DID330, enhancedData_dataTable$DID330==6666, NA)

enhancedData_dataTable$DID330[is.na(enhancedData_dataTable$DID330)] <- mean(enhancedData_dataTable$DID330, na.rm = TRUE)


## ----DID341--------------------------------------------------------------
# replace data for which the code it represents can affect mean (9999 [don't know], 7777 [refused]) --> because there is not real value recorded for these, these are as useful as missing values, so should be replaced with NA in order to faciliate mean imputation; in this case it doesn't make sense to replace 0 because it is a meaningful value that should have an effect on the mean
enhancedData_dataTable$DID341<-replace(enhancedData_dataTable$DID341, enhancedData_dataTable$DID341==9999, NA)
enhancedData_dataTable$DID341<-replace(enhancedData_dataTable$DID341, enhancedData_dataTable$DID341==7777, NA)

enhancedData_dataTable$DID341[is.na(enhancedData_dataTable$DID341)] <- mean(enhancedData_dataTable$DID341, na.rm = TRUE)


## ----DIQ010 OHE----------------------------------------------------------

## Need to convert to character column to facilitate turning into factor in order to do one_hot function
enhancedData_dataTable$DIQ010 <- as.character(enhancedData_dataTable$DIQ010)

## Need to convert to factor in order to perform one_hot function
enhancedData_dataTable$DIQ010<-as.factor(enhancedData_dataTable$DIQ010)
enhancedData_dataTable<-one_hot(enhancedData_dataTable, sparsifyNAs = TRUE, naCols = TRUE, dropCols = TRUE)


## ----DIQ160-175A---------------------------------------------------------
enhancedData_dataTable$DIQ160 <- as.character(enhancedData$DIQ160)
enhancedData_dataTable$DIQ160<-as.factor(enhancedData$DIQ160)
enhancedData_dataTable<-one_hot(enhancedData_dataTable, sparsifyNAs = TRUE, naCols = TRUE, dropCols = TRUE)


## ------------------------------------------------------------------------
enhancedData_dataTable$DIQ170 <- as.character(enhancedData$DIQ170)
enhancedData_dataTable$DIQ170<-as.factor(enhancedData$DIQ170)
enhancedData_dataTable<-one_hot(enhancedData_dataTable, sparsifyNAs = TRUE, naCols = TRUE, dropCols = TRUE)


## ------------------------------------------------------------------------
enhancedData_dataTable$DIQ172 <- as.character(enhancedData$DIQ172)
enhancedData_dataTable$DIQ172<-as.factor(enhancedData$DIQ172)
enhancedData_dataTable<-one_hot(enhancedData_dataTable, sparsifyNAs = TRUE, naCols = TRUE, dropCols = TRUE)


## ------------------------------------------------------------------------
enhancedData_dataTable$DIQ175A <- as.character(enhancedData$DIQ175A)
enhancedData_dataTable$DIQ175A<-as.factor(enhancedData$DIQ175A)
enhancedData_dataTable<-one_hot(enhancedData_dataTable, sparsifyNAs = TRUE, naCols = TRUE, dropCols = TRUE)


## ----DIQ175B-175X--------------------------------------------------------
enhancedData_dataTable$DIQ175B <- as.character(enhancedData$DIQ175B)
enhancedData_dataTable$DIQ175B<-as.factor(enhancedData$DIQ175B)

enhancedData_dataTable$DIQ175C <- as.character(enhancedData$DIQ175C)
enhancedData_dataTable$DIQ175C<-as.factor(enhancedData$DIQ175C)

enhancedData_dataTable$DIQ175D <- as.character(enhancedData$DIQ175D)
enhancedData_dataTable$DIQ175D<-as.factor(enhancedData$DIQ175D)

enhancedData_dataTable$DIQ175E <- as.character(enhancedData$DIQ175E)
enhancedData_dataTable$DIQ175E<-as.factor(enhancedData$DIQ175E)

enhancedData_dataTable$DIQ175F <- as.character(enhancedData$DIQ175F)
enhancedData_dataTable$DIQ175F<-as.factor(enhancedData$DIQ175F)

enhancedData_dataTable$DIQ175G <- as.character(enhancedData$DIQ175G)
enhancedData_dataTable$DIQ175G<-as.factor(enhancedData$DIQ175G)

enhancedData_dataTable$DIQ175H <- as.character(enhancedData$DIQ175H)
enhancedData_dataTable$DIQ175H<-as.factor(enhancedData$DIQ175H)

enhancedData_dataTable$DIQ175I <- as.character(enhancedData$DIQ175I)
enhancedData_dataTable$DIQ175I<-as.factor(enhancedData$DIQ175I)

enhancedData_dataTable$DIQ175J <- as.character(enhancedData$DIQ175J)
enhancedData_dataTable$DIQ175J<-as.factor(enhancedData$DIQ175J)

enhancedData_dataTable$DIQ175K <- as.character(enhancedData$DIQ175K)
enhancedData_dataTable$DIQ175K <-as.factor(enhancedData$DIQ175K)

enhancedData_dataTable$DIQ175L <- as.character(enhancedData$DIQ175L)
enhancedData_dataTable$DIQ175L <-as.factor(enhancedData$DIQ175L)

enhancedData_dataTable$DIQ175M <- as.character(enhancedData$DIQ175M)
enhancedData_dataTable$DIQ175M <-as.factor(enhancedData$DIQ175M)

enhancedData_dataTable$DIQ175N <- as.character(enhancedData$DIQ175N)
enhancedData_dataTable$DIQ175N <-as.factor(enhancedData$DIQ175N)

enhancedData_dataTable$DIQ175O <- as.character(enhancedData$DIQ175O)
enhancedData_dataTable$DIQ175O <-as.factor(enhancedData$DIQ175O)

enhancedData_dataTable$DIQ175P <- as.character(enhancedData$DIQ175P)
enhancedData_dataTable$DIQ175P <-as.factor(enhancedData$DIQ175P)

enhancedData_dataTable$DIQ175Q <- as.character(enhancedData$DIQ175Q)
enhancedData_dataTable$DIQ175Q <-as.factor(enhancedData$DIQ175Q)

enhancedData_dataTable$DIQ175R <- as.character(enhancedData$DIQ175R)
enhancedData_dataTable$DIQ175R <-as.factor(enhancedData$DIQ175R)

enhancedData_dataTable$DIQ175S <- as.character(enhancedData$DIQ175S)
enhancedData_dataTable$DIQ175S <-as.factor(enhancedData$DIQ175S)

enhancedData_dataTable$DIQ175T <- as.character(enhancedData$DIQ175T)
enhancedData_dataTable$DIQ175T <-as.factor(enhancedData$DIQ175T)

enhancedData_dataTable$DIQ175U <- as.character(enhancedData$DIQ175U)
enhancedData_dataTable$DIQ175U <-as.factor(enhancedData$DIQ175U)

enhancedData_dataTable$DIQ175V <- as.character(enhancedData$DIQ175V)
enhancedData_dataTable$DIQ175V <-as.factor(enhancedData$DIQ175V)

enhancedData_dataTable$DIQ175W <- as.character(enhancedData$DIQ175W)
enhancedData_dataTable$DIQ175W <-as.factor(enhancedData$DIQ175W)

enhancedData_dataTable$DIQ175X <- as.character(enhancedData$DIQ175X)
enhancedData_dataTable$DIQ175X <-as.factor(enhancedData$DIQ175X)

enhancedData_dataTable<-one_hot(enhancedData_dataTable, sparsifyNAs = TRUE, naCols = TRUE, dropCols = TRUE)


## ----DIQ180, DIQ050, DIQ070, DIQ240, DIQ275, DIQ360, DIQ080--------------
enhancedData_dataTable$DIQ180 <- as.character(enhancedData$DIQ180)
enhancedData_dataTable$DIQ180<-as.factor(enhancedData$DIQ180)

enhancedData_dataTable$DIQ050 <- as.character(enhancedData$DIQ050)
enhancedData_dataTable$DIQ050<-as.factor(enhancedData$DIQ050)

enhancedData_dataTable$DIQ070 <- as.character(enhancedData$DIQ070)
enhancedData_dataTable$DIQ070<-as.factor(enhancedData$DIQ070)

enhancedData_dataTable$DIQ240 <- as.character(enhancedData$DIQ240)
enhancedData_dataTable$DIQ240<-as.factor(enhancedData$DIQ240)

enhancedData_dataTable$DIQ275 <- as.character(enhancedData$DIQ275)
enhancedData_dataTable$DIQ275<-as.factor(enhancedData$DIQ275)

enhancedData_dataTable$DIQ360 <- as.character(enhancedData$DIQ360)
enhancedData_dataTable$DIQ360<-as.factor(enhancedData$DIQ360)

enhancedData_dataTable$DIQ080 <- as.character(enhancedData$DIQ080)
enhancedData_dataTable$DIQ080<-as.factor(enhancedData$DIQ080)

enhancedData_dataTable<-one_hot(enhancedData_dataTable, sparsifyNAs = TRUE, naCols = TRUE, dropCols = TRUE)


## ----DIQ230, DIQ291------------------------------------------------------
enhancedData_dataTable$DIQ230 <- as.character(enhancedData$DIQ230)
enhancedData_dataTable$DIQ230<-as.factor(enhancedData$DIQ230)

enhancedData_dataTable$DIQ291 <- as.character(enhancedData$DIQ291)
enhancedData_dataTable$DIQ291<-as.factor(enhancedData$DIQ291)

enhancedData_dataTable<-one_hot(enhancedData_dataTable, sparsifyNAs = TRUE, naCols = TRUE, dropCols = TRUE)


## ----DID260--------------------------------------------------------------

## Replacing "don't know" and "refused" with NA
enhancedData_dataTable$DID260<-replace(enhancedData$DID260, enhancedData$DID260==777, NA)

enhancedData_dataTable$DID260<-replace(enhancedData$DID260, enhancedData$DID260==999, NA)

## Replacing the values in DID260 wtih newly standardized units (per year)
enhancedData_dataTable$DID260<-ifelse(enhancedData_dataTable$DIQ260U==1,enhancedData_dataTable$DID260*365, enhancedData_dataTable$DID260*1 )
enhancedData_dataTable$DID260<-ifelse(enhancedData_dataTable$DIQ260U==2,enhancedData_dataTable$DID260*52, enhancedData_dataTable$DID260*1 )
enhancedData_dataTable$DID260<-ifelse(enhancedData_dataTable$DIQ260U==3,enhancedData_dataTable$DID260*12, enhancedData_dataTable$DID260*1 )

## Drop DIQ260 because the units column is not necessary
enhancedData_dataTable<-select(enhancedData_dataTable, -DIQ260U)

## Mean imputation
enhancedData_dataTable$DID260[is.na(enhancedData_dataTable$DID260)] <- mean(enhancedData_dataTable$DID260, na.rm = TRUE)


## ----060-----------------------------------------------------------------

## Replacing "less than a month", don't know", and "refused" with NA
enhancedData_dataTable$DID060<-replace(enhancedData$DID060, enhancedData$DID060==777, NA)
enhancedData_dataTable$DID060<-replace(enhancedData$DID060, enhancedData$DID060==666, NA)
enhancedData_dataTable$DID060<-replace(enhancedData$DID060, enhancedData$DID060==999, NA)

## Replacing values in DID060 to reflect standardized units (years)
enhancedData_dataTable$DID060<-ifelse(enhancedData_dataTable$DIQ060U==1,enhancedData_dataTable$DID060/12, enhancedData_dataTable$DID060)

## Drop DIQ260 because the units column is not necessary
enhancedData_dataTable<-select(enhancedData_dataTable, -DIQ060U)

## Imputing mean and replacing missing values with mean
enhancedData_dataTable$DID060[is.na(enhancedData_dataTable$DID060)] <- mean(enhancedData_dataTable$DID060, na.rm = TRUE)



## ----DID350--------------------------------------------------------------

## Replacing "don't know" and "refused" with NA
enhancedData_dataTable$DID350<-replace(enhancedData$DID350, enhancedData$DID350==7777, NA)

enhancedData_dataTable$DID350<-replace(enhancedData$DID350, enhancedData$DID350==9999, NA)

## Replacing the values in DID260 wtih newly standardized units (per year)
enhancedData_dataTable$DID350<-ifelse(enhancedData_dataTable$DIQ350U==1,enhancedData_dataTable$DID350*365, enhancedData_dataTable$DID350 )
enhancedData_dataTable$DID350<-ifelse(enhancedData_dataTable$DIQ350U==2,enhancedData_dataTable$DID350*52, enhancedData_dataTable$DID350 )
enhancedData_dataTable$DID350<-ifelse(enhancedData_dataTable$DIQ350U==3,enhancedData_dataTable$DID350*12, enhancedData_dataTable$DID350 )

## Drop DIQ260 because the units column is not necessary
enhancedData_dataTable<-select(enhancedData_dataTable, -DIQ350U)

## Mean imputation
enhancedData_dataTable$DID350[is.na(enhancedData_dataTable$DID350)] <- mean(enhancedData_dataTable$DID350, na.rm = TRUE)


## ----Data types----------------------------------------------------------
## Need to convert to character column to facilitate turning into factor in order to do one_hot function
enhancedData$DIQ010 <- as.character(enhancedData$DIQ010)

## Need to convert to factor in order to perform one_hot function
enhancedData$DIQ010<-as.factor(enhancedData$DIQ010)


## ------------------------------------------------------------------------
sapply(enhancedData, function(x) sum(is.na(x)))
sapply(enhancedData, function(x) round(sum(is.na(x)/9575), 4))


## ------------------------------------------------------------------------
enhancedData[, -which(colMeans(is.na(enhancedData)) > 0.9)]


## ------------------------------------------------------------------------
sqldf("select DID260, count(DID260) from enhancedData group by DID260")
sqldf("select DID260, DIQ260U from enhancedData where DID260=8")


## ------------------------------------------------------------------------
sqldf("select DID350, count(DID350) from enhancedData group by DID350")
sqldf("select DID350, DIQ350U from enhancedData where DID350=7")


## ----Counts--------------------------------------------------------------
sqldf("select DIQ175U, count(DIQ175U) from enhancedData group by DIQ175U")
sqldf("select DIQ010, count(DIQ010) from enhancedData group by DIQ010")
sqldf("select DIQ180, count(DIQ180) from enhancedData group by DIQ180")
sqldf("select DIQ240, count(DIQ240) from enhancedData group by DIQ240")
sqldf("select DIQ350U, count(DIQ350U) from enhancedData group by DIQ350U")
sqldf("select DIQ080, count(DIQ080) from enhancedData group by DIQ080")

# This is a continuous variable -- the first query looks at the counts of each element in the column
# The second query determines if there are 627 values between 1-60 as indicated in the dataset guide
sqldf("select DID250, count(DID250) from enhancedData group by DID250")
sqldf("select count(DID250) from enhancedData where DID250 >= 1 and DID250 <=60")

# How to find count of missing values
sapply(enhancedData, function(x) sum(is.na(x)))


## ------------------------------------------------------------------------
apply(enhancedData_dataTable, 2, function(x) any(is.na(x)))

