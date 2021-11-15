##### erases global environment ####
rm(list=ls())

#### SETWD####

#setwd("/aa/resadmin/PHSRP/Student Interns/Pathway Fellows/Jasmin Fernandez/BrFSS TRans Project/Datasets of all years")
setwd("C:/Users/cogps/Desktop/Thesis Analysis/R Practice/BRSFF Project")

#### read data ####
library(haven)

BRFSS_2016 <- read_sav("COPYbrfs_16nr2.sav")
BRFSS_2017 <- read_sav("COPYbrfs_17nr2.sav")
BRFSS_2018 <- read_sav("COPYbrfs_18nr1.sav")
POVERTY_DS <- read_sav("Poverty200 variable for 2016-2018.sav")

#####Subsetting & Merging ####

library(dplyr)

Subs.16 <- subset(BRFSS_2016, select =c("MENTHLTH", "PHYSHLTH", "POORHLTH", "TRNSGNDR", "SXORIEN2", "EMPLOY2", "EDUCA",
                                        "AGE", "SMKEVDA2", "YEAR", "SEQNOQ", "DATE","HAVEPLN3","DRINKNUM",
                                        "ORACE3A_1", "ORACE3A_2", "ORACE3A_3", "ORACE3A_4", "ORACE3A_5", "ORACE3A_6", "ORACE3A_8", "ORACE3A_9","HISP4", "RACEGR", "DRNKALC2"))
Subs.17 <- subset(BRFSS_2017, select =c("MENTHLTH", "PHYSHLTH", "POORHLTH", "TRNSGNDR", "SXORIEN2", "EMPLOY2", "EDUCA",
                                        "AGE", "SMKEVDA2", "YEAR", "SEQNOQ", "DATE","HAVEPLN3","DRINKNUM",
                                        "ORACE3A_1", "ORACE3A_2", "ORACE3A_3", "ORACE3A_4", "ORACE3A_5", "ORACE3A_6", "ORACE3A_8", "ORACE3A_9","HISP4", "RACEGR", "DRNKALC2"))
Subs.18 <- subset(BRFSS_2018, select =c("MENTHLTH", "PHYSHLTH", "POORHLTH", "TRNSGNDR", "SXORIEN2", "EMPLOY2", "EDUCA",
                                        "AGE", "SMKEVDA2", "YEAR", "SEQNOQ", "DATE","HAVEPLN3",
                                        "ORACE3A_1", "ORACE3A_2", "ORACE3A_3", "ORACE3A_4", "ORACE3A_5", "ORACE3A_6", "ORACE3A_8", "ORACE3A_9","HISP4", "RACEGR", "DRNKALC2"))
Pov.All <- subset(POVERTY_DS, select = c("poverty200", "SEQNOQ"))

NEWMERGED<-bind_rows(Subs.16,Subs.17,Subs.18) 

MERGED <- merge(NEWMERGED,Pov.All, by = c("SEQNOQ"), all = TRUE)


#### recoding IV for MERGED subsetted Data set ####

MERGED <- within(MERGED, {
  SXORIEN2[SXORIEN2 == 1] <- "5"
})
MERGED <- within(MERGED, {
  SXORIEN2[SXORIEN2 == 2] <- "1"
  SXORIEN2[SXORIEN2 == 3] <- "1"
  SXORIEN2[SXORIEN2 == 4] <- "1"
})

# transgender
MERGED <- within(MERGED, {
  TRNSGNDR[TRNSGNDR == 2] <- "1"
  TRNSGNDR[TRNSGNDR == 3] <- "1"
})  

#### Recd RACE ####

MERGED <- within(MERGED, {
  RACEGR[RACEGR == 1] <- "white"
  RACEGR[RACEGR == 2] <- "Black"
  RACEGR[RACEGR == 3] <- "hispanic"
  RACEGR[RACEGR == 4] <- "other"
  RACEGR[RACEGR == 9] <- NA
})  

### Coding Conditional Variables

MERGED$LGBT <- with(MERGED, ifelse(TRNSGNDR == 1 & SXORIEN2 == 1, "queer",
                                             ifelse(TRNSGNDR==1 & SXORIEN2 ==5, "queer",
                                                    ifelse(TRNSGNDR==4 & SXORIEN2 ==1, "queer",
                                                           ifelse(TRNSGNDR==4 & SXORIEN2 ==5, "not.queer", NA)))) )

MERGED$TIME <- with(MERGED, ifelse(DATE < "2016-04-25", "BfrPrim",
                                               ifelse(DATE >= "2016-04-25" & DATE < "2016-11-08", "AtAftPrimBfrFedElc",
                                                      ifelse(DATE >= "2016-11-08" & DATE < "2017-01-20", "AtAftFedElcBfrInaug",
                                                             ifelse(DATE >= "2017-01-20", "AtAftrInaug", NA)))) )

# changing the order of the levels within the variable time
summary(as.factor(MERGED$TIME))
levels(as.factor(MERGED$TIME))
MERGED$TIME <- factor(MERGED$TIME, c("BfrPrim", "AtAftPrimBfrFedElc", "AtAftFedElcBfrInaug", "AtAftrInaug"))
levels(as.factor(MERGED$TIME))
summary(as.factor(MERGED$TIME))

#### Recoding DV's #### 

#MENTHLTH
MERGED <- within(MERGED, {
  MENTHLTH[MENTHLTH == 88] <- 0  # Never
  MENTHLTH[MENTHLTH == 77] <- NA # unknown/Don't Know/ Not sure
  MENTHLTH[MENTHLTH == 99] <- NA # this one is the refused-- or the NA's
})


#PHYSHLTH
MERGED <- within(MERGED, {
  PHYSHLTH[PHYSHLTH == 88] <- 0
  PHYSHLTH[PHYSHLTH == 77] <- NA
  PHYSHLTH[PHYSHLTH == 99] <- NA
})


#POORHLTH
MERGED <- within(MERGED, {
  POORHLTH[POORHLTH == 88] <- 0
  POORHLTH[POORHLTH == 77] <- NA
  POORHLTH[POORHLTH == 99] <- NA
})



# Creating a Variable based on two Others

MERGED$POORHLTH[MERGED$MENTHLTH == 0 & MERGED$PHYSHLTH == 0] = 0


## Recodinf Emply2 variable 

MERGED <- within(MERGED, {
  EMPLOY2[EMPLOY2 == 1] <- "employed"
  EMPLOY2[EMPLOY2 == 2] <- "employed"
  EMPLOY2[EMPLOY2 == 3] <- "not.employed"
  EMPLOY2[EMPLOY2 == 4] <- "not.employed"
  EMPLOY2[EMPLOY2 == 5] <- "not.employed"
  EMPLOY2[EMPLOY2 == 6] <- "not.employed"
  EMPLOY2[EMPLOY2 == 7] <- "not.employed"
  EMPLOY2[EMPLOY2 == 8] <- "not.employed"
  EMPLOY2[EMPLOY2 == 77] <- NA
  EMPLOY2[EMPLOY2 == 99] <- NA
})

## Educa Recoding 

MERGED <- within(MERGED, {
  EDUCA[EDUCA == 1] <- "lessBA"
  EDUCA[EDUCA == 2] <- "lessBA"
  EDUCA[EDUCA == 3] <- "lessBA"
  EDUCA[EDUCA == 4] <- "lessBA"
  EDUCA[EDUCA == 5] <- "lessBA"
  EDUCA[EDUCA == 6] <- "lessBA"
  EDUCA[EDUCA == 88] <- "lessBA"
  EDUCA[EDUCA == 7] <- "BAorMore"
  EDUCA[EDUCA == 8] <- "BAorMore"
  EDUCA[EDUCA == 77] <- NA
  EDUCA[EDUCA == 99] <- NA
})


#AGEB ## this one is already recoded
MERGED <- within(MERGED, {
  AGE[AGE == 7] <- NA
  AGE[AGE == 9] <- NA
})


## Smoking Variable Recoding 

MERGED <- within(MERGED, {
  SMKEVDA2[SMKEVDA2 == 1] <- "smokes"
  SMKEVDA2[SMKEVDA2 == 2] <- "smokes"
  SMKEVDA2[SMKEVDA2 == 3] <- "d.notsmoke"
  SMKEVDA2[SMKEVDA2 == 77] <- NA
  SMKEVDA2[SMKEVDA2 == 99] <- NA
})

# poverty 200

MERGED <- within(MERGED, {
  poverty200[poverty200 == 1] <- "poor"
  poverty200[poverty200 == 0] <- "not.poor"
})


#### HAVEPLN3 recoding ####
MERGED <- within(MERGED, {
  HAVEPLN3[HAVEPLN3 == 1] <- "insurance"
  HAVEPLN3[HAVEPLN3 == 2] <- "no.insurance"
  HAVEPLN3[HAVEPLN3 == 77] <- NA
  HAVEPLN3[HAVEPLN3 == 99] <- NA
})

## Drinking Alcohol Recoding

MERGED <- within(MERGED, {
  DRNKALC2[DRNKALC2 == 101] <- 4.29
  DRNKALC2[DRNKALC2 == 102] <- 8.57
  DRNKALC2[DRNKALC2 == 103] <- 12.86
  DRNKALC2[DRNKALC2 == 104] <- 17.14
  DRNKALC2[DRNKALC2 == 105] <- 21.42
  DRNKALC2[DRNKALC2 == 106] <- 25.7
  DRNKALC2[DRNKALC2 == 107] <- 30
  DRNKALC2[DRNKALC2 == 201] <- 1
  DRNKALC2[DRNKALC2 == 202] <- 2
  DRNKALC2[DRNKALC2 == 203] <- 3
  DRNKALC2[DRNKALC2 == 204] <- 4
  DRNKALC2[DRNKALC2 == 205] <- 5
  DRNKALC2[DRNKALC2 == 206] <- 6
  DRNKALC2[DRNKALC2 == 207] <- 7
  DRNKALC2[DRNKALC2 == 208] <- 8
  DRNKALC2[DRNKALC2 == 209] <- 9
  DRNKALC2[DRNKALC2 == 210] <- 10
  DRNKALC2[DRNKALC2 == 211] <- 11
  DRNKALC2[DRNKALC2 == 212] <- 12
  DRNKALC2[DRNKALC2 == 213] <- 13
  DRNKALC2[DRNKALC2 == 214] <- 14
  DRNKALC2[DRNKALC2 == 215] <- 15
  DRNKALC2[DRNKALC2 == 216] <- 16
  DRNKALC2[DRNKALC2 == 217] <- 17
  DRNKALC2[DRNKALC2 == 218] <- 18
  DRNKALC2[DRNKALC2 == 219] <- 19
  DRNKALC2[DRNKALC2 == 220] <- 20
  DRNKALC2[DRNKALC2 == 221] <- 21
  DRNKALC2[DRNKALC2 == 222] <- 22
  DRNKALC2[DRNKALC2 == 223] <- 23
  DRNKALC2[DRNKALC2 == 224] <- 24
  DRNKALC2[DRNKALC2 == 225] <- 25
  DRNKALC2[DRNKALC2 == 226] <- 26
  DRNKALC2[DRNKALC2 == 227] <- 27
  DRNKALC2[DRNKALC2 == 228] <- 28
  DRNKALC2[DRNKALC2 == 229] <- 29
  DRNKALC2[DRNKALC2 == 230] <- 30
  DRNKALC2[DRNKALC2 == 777] <- NA
  DRNKALC2[DRNKALC2 == 888] <- 0
  DRNKALC2[DRNKALC2 == 999] <- NA
})



#### Making Variables either Categorical or Numerical
## and Changing the Order of the Levels if need be

## Education
MERGED$EDUCA = as.factor(MERGED$EDUCA)
levels(MERGED$EDUCA)
          #reference Category:BA or More

# Poverty
MERGED$poverty200 = as.factor(MERGED$poverty200)
MERGED$poverty200 <- factor(MERGED$poverty200, c("not.poor","poor"))
          # reference Category: not.poor

## Insurance Variable

MERGED$HAVEPLN3 = as.factor(MERGED$HAVEPLN3)
levels(MERGED$HAVEPLN3)
          # reference: insurance


## Race Variable 

MERGED$RACEGR = as.factor(MERGED$RACEGR)# this one has their levels at the approproate level with Black as reference
levels(MERGED$RACEGR)
MERGED$RACEGR <- factor(MERGED$RACEGR, c("white","Black","hispanic","other"))
            # reference white

## Smoking Variable
MERGED$SMKEVDA2 = as.factor(MERGED$SMKEVDA2)
          # reference do not smokes

## Making Variable Numeric 
MERGED$AGE = as.numeric(MERGED$AGE)
MERGED$DRNKALC2 = as.numeric(MERGED$DRNKALC2)
MERGED$POORHLTH = as.numeric(MERGED$POORHLTH)
MERGED$MENTHLTH = as.numeric(MERGED$MENTHLTH)
MERGED$PHYSHLTH = as.numeric(MERGED$PHYSHLTH)


#### subsetting data ####
Queer <- MERGED[MERGED$LGBT == "queer", ]
nonQueer <- MERGED[MERGED$LGBT == "not.queer", ]



### getting rid of NA ####
MERGED.Clean <- MERGED[complete.cases(MERGED$LGBT),]

Queer.Clean <- Queer[complete.cases(Queer$LGBT),]
#summary(as.factor(Queer.Clean$LGBT))
nonQueer.Clean <- nonQueer[complete.cases(nonQueer$LGBT),]
#summary(as.factor(nonQueer.Clean$LGBT))

sixteen.Clean <- MERGED.Clean[MERGED.Clean$YEAR == "2016", ]
seventeen.Clean <- MERGED.Clean[MERGED.Clean$YEAR == "2017", ]
eighteen.Clean <- MERGED.Clean[MERGED.Clean$YEAR == "2018", ]


### Cross tabs####
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")


## Write a CSV file ####
library(data.table)

# write.csv(Queer.Clean, "SubQueerClean.csv")
# write.csv(nonQueer.Clean, "SubnonQueerClean.csv")
# write.csv(MERGED.Clean, "SubMERGED.Clean.csv")


