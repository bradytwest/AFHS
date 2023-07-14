library(haven)
library(dplyr)
library(stringr)
library(pscl)
library(datetime)
library(lubridate)
library(lme4)
library(lmerTest)
library(data.table)
library(openxlsx)
library(clipr)
library(survey)
library(ggplot2)
library(ggpubr)

all_variables<- c("ScreenerDone",
                 "ScreenerDoneDateTime", 
                 "TLIFENAME",
                 "Phase_S",
                 "ScreenerMode",
                 "Phase_F",
                 "Phase_M1",
                 "Phase_M2",
                 "Phase_M3",
                 "SelectedMemberAge",
                 "SelectedMemberHisp",
                 "SelectedMemberRace1", 
                 "SelectedMemberRace2",
                 "SelectedMemberRace3", 
                 "SelectedMemberRace4", 
                 "SelectedMemberRace5",
                 "SelectedMemberSex",
                 "offeredFull",
                 "FinalAaporCategory_F",
                 "FinalAaporCategory_M1",
                 "FinalAaporCategory_M2",
                 "FinalAaporCategory_M3",
                 "offeredM1",
                 "offeredM2",
                 "offeredM3",
                 "offeredM1M2",
                 "offeredM2M3")


# HYPOTHESIS 1-----------------------------------------------------------

setwd("O:/AFHS-DATA/Analysis/Shiyu/GSRA")
datCase<- read_sas("datPlosOne.sas7bdat")


# Screener complete indicator ----
table(datCase$ScreenerDone, exclude= F)
sum(datCase$ScreenerDoneDateTime== "")

datCase<- datCase%>%
  dplyr::mutate(
    screenComplete= dplyr::case_when(
      !is.na(ScreenerDone) & ScreenerDoneDateTime!= "" ~ "respondent",
      TRUE~ "nonrespondent"
    )
  )

datCase$screenComplete<- factor(datCase$screenComplete,
                                levels= c("respondent", "nonrespondent"))

table(datCase$screenComplete, datCase$ScreenerDone, exclude= F)

# # appending TAPESTRY variables to the caselevel dataset
# table(auxiliary$TLIFENAME, exclude= F)
# 
# intersect(unique(auxiliary$GIDBG), unique(datCase$GIDBG))%>% length() #double checking GIDBG for matching 
# length(unique(datCase$GIDBG))
# 
# datCase<- datCase%>%
#   dplyr::left_join(auxiliary[c("GIDBG", "TLIFENAME")]%>% distinct(),
#                    by= "GIDBG")
# 
# dim(datCase)

# Response by phase ----
table(datCase$Phase_S, exclude= F)

datCase%>%
  dplyr::select(Phase_S, screenComplete)%>%
  table(exclude= F)

## Response rate by phase by Tapestry ----
table(datCase$TLIFENAME, exclude= F)%>% write_clip()

datCase%>%
  dplyr::filter(TLIFENAME== "Cozy Country Living")%>% #change TLIFENAME
  dplyr::select(Phase_S)%>%
  table(exclude= F)


datCase%>%
  dplyr::filter(TLIFENAME== "Cozy Country Living")%>% #change TLIFENAME
  dplyr::select(Phase_S, screenComplete)%>%
  table(exclude= F)



# Rsponse by mode ---- 
datCase$ScreenerMode<- factor(datCase$ScreenerMode,
                              levels= c("Web", "PAPI"))
table(datCase$ScreenerMode, exclude= F)
datCase%>%
  dplyr::select(screenComplete, ScreenerMode,  Phase_S)%>%
  table(exclude= F)

## Response rate by mode by Tapestry ----

table(datCase$TLIFENAME, exclude= F)%>% write_clip()


datCase%>%
  dplyr::filter(TLIFENAME== "Uptown Individuals")%>%
  dplyr::select(screenComplete, ScreenerMode,  Phase_S)%>%
  table(exclude= F)


# HYPOTHEIS 2 ------------------------------------------------------

# Reading dataset ----
setwd("O:/AFHS-DATA/Analysis/Shiyu/GSRA")
caselevel_phase<- read_sas("datPlosOne.sas7bdat")

#Rename the variables to their old names 
caselevel_phase$phase_Full<- caselevel_phase$Phase_F
caselevel_phase$phase_M1<- caselevel_phase$Phase_M1
caselevel_phase$phase_M2<- caselevel_phase$Phase_M2
caselevel_phase$phase_M3<- caselevel_phase$Phase_M3


# Create demographic categories ---- 

## age ----
summary(caselevel_phase$SelectedMemberAge)

caselevel_phase<- caselevel_phase%>%
  dplyr::mutate(agecat2= dplyr::case_when(
    SelectedMemberAge>=18 & SelectedMemberAge<=19 ~ "18-19",
    SelectedMemberAge>=20 & SelectedMemberAge<=49 ~ "20-49"
  ))
table(caselevel_phase$agecat2, exclude= F)

## race and ethnicity ----
table(caselevel_phase$SelectedMemberHisp, exclude= F)
table(caselevel_phase$SelectedMemberRace1, exclude= F)

#does the person have multiple races?
caselevel_phase$raceCount<- caselevel_phase%>%
  dplyr::select(SelectedMemberRace1, SelectedMemberRace2,
                SelectedMemberRace3, SelectedMemberRace4,
                SelectedMemberRace5)%>%
  apply(., 1, function(x) sum(!is.na(x)))

#Coding race and ethnicity into one variable `racecat3`
table(caselevel_phase$SelectedMemberHisp, exclude= F)
table(caselevel_phase$raceCount, caselevel_phase$SelectedMemberHisp,exclude= F)
caselevel_phase<- caselevel_phase%>%
  dplyr::mutate(racecat3= dplyr::case_when(
    SelectedMemberHisp== 1~ "Hispanic",
    (SelectedMemberHisp== 5| is.na(SelectedMemberHisp)) & raceCount==1 & SelectedMemberRace1== 5~ "Non-H Other",
    (SelectedMemberHisp== 5| is.na(SelectedMemberHisp)) & raceCount==1 & SelectedMemberRace1== 4~ "Non-H Black",
    (SelectedMemberHisp== 5| is.na(SelectedMemberHisp)) & raceCount==1 & SelectedMemberRace1== 2~ "Non-H Other",
    (SelectedMemberHisp== 5| is.na(SelectedMemberHisp)) & raceCount==1 & SelectedMemberRace1== 1~ "Non-H Other",
    (SelectedMemberHisp== 5| is.na(SelectedMemberHisp)) & raceCount==1 & SelectedMemberRace1== 3~ "Non-H Other",
    (SelectedMemberHisp== 5| is.na(SelectedMemberHisp)) & raceCount>1 ~ "Non-H Other" 
  ))

table(caselevel_phase$racecat3, exclude = F)

## gender ----
table(caselevel_phase$SelectedMemberSex, exclude= F)
caselevel_phase<- caselevel_phase%>%
  dplyr::mutate(male= dplyr::case_when(
    SelectedMemberSex==1 ~ "male",
    SelectedMemberSex==5 ~ "female"
  ))
table(caselevel_phase$SelectedMemberSex, caselevel_phase$male, exclude= F)


## categories ----
caselevel_phase<- caselevel_phase%>%
  dplyr::mutate(democat12= dplyr::case_when(
    male== "male"   & racecat3== "Non-H Other" & agecat2== "18-19"~ "m-other-1819",
    male== "male"   & racecat3== "Non-H Other" & agecat2== "20-49"~ "m-other-2049",
    male== "male"   & racecat3== "Non-H Black" & agecat2== "18-19"~ "m-black-1819",
    male== "male"   & racecat3== "Non-H Black" & agecat2== "20-49"~ "m-black-2049",
    male== "male"   & racecat3== "Hispanic"    & agecat2== "18-19"~ "m-hisp-1819",
    male== "male"   & racecat3== "Hispanic"    & agecat2== "20-49"~ "m-hisp-2049",
    male== "female" & racecat3== "Non-H Other" & agecat2== "18-19"~ "f-other-1819",
    male== "female" & racecat3== "Non-H Other" & agecat2== "20-49"~ "f-other-2049",
    male== "female" & racecat3== "Non-H Black" & agecat2== "18-19"~ "f-black-1819",
    male== "female" & racecat3== "Non-H Black" & agecat2== "20-49"~ "f-black-2049",
    male== "female" & racecat3== "Hispanic"    & agecat2== "18-19"~ "f-hisp-1819",
    male== "female" & racecat3== "Hispanic"    & agecat2== "20-49"~ "f-hisp-2049"
  ))

caselevel_phase$democat12<- factor(caselevel_phase$democat12,
                                   levels= c("m-other-1819", "m-other-2049", 
                                             "m-black-1819", "m-black-2049", 
                                             "m-hisp-1819", "m-hisp-2049", 
                                             "f-other-1819", "f-other-2049", 
                                             "f-black-1819", "f-black-2049", 
                                             "f-hisp-1819", "f-hisp-2049"))

# RR of FULL-design main interview ----

## Subsetting dataset (denominator of RR)----
table(caselevel_phase$offeredFull, exclude= F)
datFul<- caselevel_phase%>%
  dplyr::filter(offeredFull==1) #subset those offered Full questionnaire
dim(datFul) #766 


#RR by phase overall
table(datFul$phase_Full, datFul$FinalAaporCategory_F, exclude= F) #numerator

#RR by phase for demographic subgroups
table(datFul$democat12, exclude= F) #denominator by subgroup

table(datFul$phase_Full, datFul$FinalAaporCategory_F, datFul$democat12, exclude= F) #numerator


# RR of Module-design main interview ~ M1 ----

## Subsetting dataset (denominator of RR)----
table(caselevel_phase$offeredM1, exclude= F)
datM1<- caselevel_phase%>%
  dplyr::filter(offeredM1==1) #subset those offered M1 questionnaire
dim(datM1) #746


#RR by phase overall
table(datM1$phase_M1, datM1$FinalAaporCategory_M1, exclude= F) #numerator

#RR by phase for demographic subgroups
table(datM1$democat12, exclude= F) #denominator by subgroup

table(datM1$phase_M1, datM1$FinalAaporCategory_M1, datM1$democat12, exclude= F) #numerator


# RR of Module-design main interview ~ M2 ----
## Subsetting dataset----
caselevel_phase<- caselevel_phase%>%
  dplyr::mutate(offeredM2Ever= dplyr::case_when(
    offeredM2==1| offeredM1M2==1  ~ 1,
    TRUE~ 0
  ))
table(caselevel_phase$offeredM2Ever, exclude= F)

datM2<- caselevel_phase%>%
  dplyr::filter(offeredM2Ever==1)#subset those offered M2 questionnaire
dim(datM2) #737


#RR by phase overall
table(datM2$phase_M2, datM2$FinalAaporCategory_M2, exclude= F) #numerator

#RR by phase for demographic subgroups
table(datM2$democat12, exclude= F) #denominator

table(datM2$phase_M2, datM2$FinalAaporCategory_M2, datM2$democat12, exclude= F) #numerator


# RR of Module-design main interview ~ M3 ----
## Subsetting dataset----
caselevel_phase<- caselevel_phase%>%
  dplyr::mutate(offeredM3Ever= dplyr::case_when(
    offeredM3==1| offeredM2M3==1  ~ 1,
    TRUE~ 0
  ))
table(caselevel_phase$offeredM3Ever, exclude= F)

datM3<- caselevel_phase%>%
  dplyr::filter(offeredM3Ever==1)#subset those offered M3 questionnaire
dim(datM3) #460


#RR by phase overall
table(datM3$phase_M3, datM3$FinalAaporCategory_M3, exclude= F) #numerator

#RR by phase for demographic subgroups
table(datM3$democat12, exclude= F) #denominator

table(datM3$phase_M3, datM3$FinalAaporCategory_M3, datM3$democat12, exclude= F) #numerator




# TELEPHONE REMINDER DESCRIPTIVE ANALYSIS------------------------------------------------------

## Full ----
table(caselevel_phase$offeredFull, exclude= F)
datFul<- caselevel_phase%>%
  dplyr::filter(offeredFull==1) #subset those offered Full questionnaire
dim(datFul) #766 

table(datFul$phase_Full, datFul$FinalAaporCategory_F, exclude= F) #numerator


## M1 ----
table(caselevel_phase$offeredM1, exclude= F)
datM1<- caselevel_phase%>%
  dplyr::filter(offeredM1==1) #subset those offered M1 questionnaire
dim(datM1) #746

table(datM1$phase_M1, datM1$FinalAaporCategory_M1, exclude= F) #numerator


## M2 ----
caselevel_phase<- caselevel_phase%>%
  dplyr::mutate(offeredM2Ever= dplyr::case_when(
    offeredM2==1| offeredM1M2==1  ~ 1,
    TRUE~ 0
  ))
table(caselevel_phase$offeredM2Ever, exclude= F)

datM2<- caselevel_phase%>%
  dplyr::filter(offeredM2Ever==1)#subset those offered M2 questionnaire
dim(datM2) #737

table(datM2$phase_M2, datM2$FinalAaporCategory_M2, exclude= F) #numerator


## M3 ----
caselevel_phase<- caselevel_phase%>%
  dplyr::mutate(offeredM3Ever= dplyr::case_when(
    offeredM3==1| offeredM2M3==1  ~ 1,
    TRUE~ 0
  ))
table(caselevel_phase$offeredM3Ever, exclude= F)

datM3<- caselevel_phase%>%
  dplyr::filter(offeredM3Ever==1)#subset those offered M3 questionnaire
dim(datM3) #460

table(datM3$phase_M3, datM3$FinalAaporCategory_M3, exclude= F) #numerator













