setwd("~/Documents/01-Research/R/Harvard")
# read csv file and create an object, h
Exp_Pred <- read.csv("Expert_Prediction_cleaned.csv")
Ads_Cost <- read.csv("Ads_Cost_2012_2018.csv")
CVAP <- read.csv("cvap_district_2012-2020_clean.csv")
Vote_Inc <- read.csv("incumb_dist_1948-2020 (3).csv")

#installing required packages
install.packages("tidyverse")
# loading required packages
library(tidyverse)

Exp_Pred_2012_2020 <- Exp_Pred %>%
  select(year, state, district_num, avg_rating, Close_E_D, LLS_D, LLS_R, St_AVG_R) %>%
  group_by(year, state, district_num) %>%
  rename(DISTRICT = district_num, STATENAME = state)

Ads_Cost_2012_2018 <- Ads_Cost %>%
  select(st_cd_fips, STATENAME, DISTRICT, year, est_cost_sum) %>%
  group_by(year, STATENAME, DISTRICT) %>%
  rename(DISTRICT = DISTRICT, STATENAME = STATENAME)

CVAP_2012_2020 <- CVAP %>%
  select(cd, state, cvap, year) %>%
  group_by(year, state, cd) %>%
  rename(DISTRICT = cd, STATENAME = state)

Vote_Inc_1948_2020 <- Vote_Inc %>%
  select(year, state, district_num, RepVotes, DemVotes,RepVotesMajorPercent, DemVotesMajorPercent, Incumbent_R, Incumbent_D, Open_seat) %>%
  group_by(year, state, district_num) %>%
  rename(DISTRICT = district_num, STATENAME = state)


ExpP_LJ_CVAP_2012_2020 <- Exp_Pred_2012_2020 %>% left_join(CVAP_2012_2020, by=c("year", "DISTRICT", "STATENAME"))

Final_Dataset_2012_2020 <- ExpP_LJ_CVAP_2012_2020 %>% left_join(Vote_Inc_1948_2020, by=c("year", "DISTRICT", "STATENAME")) 

Final_2012_2020_D <- Final_Dataset_2012_2020 %>%
  select(year, STATENAME, DISTRICT, avg_rating, Close_E_D, LLS_R, LLS_D, St_AVG_R, cvap, RepVotes, DemVotes, RepVotesMajorPercent, DemVotesMajorPercent, Incumbent_R, Incumbent_D, Open_seat) %>%
  mutate(voterTR = ((RepVotes)+(DemVotes))/(cvap)) %>%
  rename(DISTRICT = DISTRICT, STATENAME = STATENAME)

## Exp_LJ_Ads_2012_2018 <- Exp_Pred_2012_2020 %>% left_join(Ads_Cost_2012_2018, by=c("year", "DISTRICT", "STATENAME"))




write.csv(Final_2012_2018_D, "Final_2012_2020_D.csv")

############
Final_2012_2020_DF <- read.csv("Final_2012_2020_DF.csv")
Final_2012_2018_DF <- Final_2012_2020_DF %>% left_join(Ads_Cost_2012_2018, by=c("year", "DISTRICT", "STATENAME"))


##Incumbent_R
###best = M1
lm_vTR_M1 <- lm (voterTR~Close_E_D+Incumbent_R+MidtermE_D+Open_seat, data=Final_2012_2020_DF)
summary(lm_vTR_M1)
#### Interpretation:  XXXXXXX

#lm_vTR_M2 <- lm (voterTR~Close_E_D+Incumbent_R+LLS_R+MidtermE_D, data=Final_2012_2020_DF)
#summary(lm_vTR_M2)

#lm_vTR_M3 <- lm (voterTR~Close_E_D+Incumbent_R+LLS_D+MidtermE_D, data=Final_2012_2020_DF)
#summary(lm_vTR_M3)


##Incumbent_D
###best = M4
lm_vTR_M4 <- lm (voterTR~Close_E_D+Incumbent_D+MidtermE_D+Open_seat, data=Final_2012_2020_DF)
summary(lm_vTR_M4)
#### Interpretation:  XXXXXXX

#lm_vTR_M5 <- lm (voterTR~Close_E_D+Incumbent_D+LLS_R+MidtermE_D, data=Final_2012_2020_DF)
#summary(lm_vTR_M5)

#lm_vTR_M6 <- lm (voterTR~Close_E_D+Incumbent_D+LLS_D+MidtermE_D, data=Final_2012_2020_DF)
#summary(lm_vTR_M6)


####### No Incumbent Var
#lm_vTR_M7 <- lm (voterTR~Close_E_D+MidtermE_D, data=Final_2012_2020_DF)
#summary(lm_vTR_M7)

#lm_vTR_M8 <- lm (voterTR~Close_E_D+LLS_R+MidtermE_D, data=Final_2012_2020_DF)
#summary(lm_vTR_M8)

#lm_vTR_M9 <- lm (voterTR~Close_E_D+LLS_D+MidtermE_D, data=Final_2012_2020_DF)
#summary(lm_vTR_M9)


##Incumbent_R with Ads Cost
#lm_vTR_M10 <- lm (voterTR~Close_E_D+Incumbent_R+MidtermE_D+est_cost_sum+Open_seat, data=Final_2012_2018_DF)
#summary(lm_vTR_M10)

#lm_vTR_M11 <- lm (voterTR~Incumbent_R+LLS_R+MidtermE_D+est_cost_sum, data=Final_2012_2018_DF)
#summary(lm_vTR_M11)

#lm_vTR_M12 <- lm (voterTR~Incumbent_R+LLS_D+MidtermE_D+est_cost_sum, data=Final_2012_2018_DF)
#summary(lm_vTR_M12)


##Incumbent_D with Ads Cost
###best = M13
lm_vTR_M13 <- lm (voterTR~Close_E_D+Incumbent_D+MidtermE_D+est_cost_sum+Open_seat, data=Final_2012_2018_DF)
summary(lm_vTR_M13)
######## intepretation: XXXXXXXX

#lm_vTR_M14 <- lm (voterTR~Close_E_D+Incumbent_D+LLS_R+MidtermE_D+est_cost_sum, data=Final_2012_2018_DF)
#summary(lm_vTR_M14)

#lm_vTR_M15 <- lm (voterTR~Close_E_D+Incumbent_D+LLS_D+MidtermE_D+est_cost_sum, data=Final_2012_2018_DF)
#summary(lm_vTR_M15)


####### No Incumbent Var
#lm_vTR_M16 <- lm (voterTR~Close_E_D+MidtermE_D+est_cost_sum, data=Final_2012_2018_DF)
#summary(lm_vTR_M16)

#lm_vTR_M17 <- lm (voterTR~Close_E_D+LLS_R+MidtermE_D+est_cost_sum, data=Final_2012_2018_DF)
#summary(lm_vTR_M17)

#lm_vTR_M18 <- lm (voterTR~Close_E_D+LLS_D+MidtermE_D+est_cost_sum, data=Final_2012_2018_DF)
#summary(lm_vTR_M18)


##==================================================
# filter for Close Election only
#F_2012_2018_DF_CE <- Final_2012_2018_DF %>%
  filter(Close_E_D %in% c(1))

##Incumbent_D with Ads Cost only for Close Elections
lm_vTR_M19 <- lm (voterTR~LLS_D+Incumbent_D+MidtermE_D+est_cost_sum, data=F_2012_2018_DF_CE)
summary(lm_vTR_M19)
##==================================================


##==================================================
# filter for Not Close Election only
#F_2012_2018_DF_NCE <- Final_2012_2018_DF %>%
  filter(Close_E_D %in% c(0))

##Incumbent_D with Ads Cost only for Close Elections
###best = M20 for sub-sample
lm_vTR_M20 <- lm (voterTR~Incumbent_D+MidtermE_D+est_cost_sum, data=F_2012_2018_DF_NCE)
summary(lm_vTR_M20)

##### interpretation: XXXXXX
##==================================================


EE_data_R1 <- read.csv("Election_Economy_R1.csv")
Final_2012_2018_DF_ME_R1 <- EE_data_R1 %>% left_join(Final_2012_2018_DF, by=c("year"))
##### interpretation: XXXXXX
##==================================================
lm_DVsh_M1 <- lm (DemVotesMajorPercent ~ voterTR+Close_E_D+LLS_D+Pres_D+GDP_Growth_Pct+US_CPI+US_Unemployment.Rate, data=Final_2012_2018_DF_ME_R1)
summary(lm_DVsh_M1)
##### interpretation: XXXXXX
##==================================================
lm_RVsh_M1 <- lm (RepVotesMajorPercent ~ voterTR+Close_E_D+LLS_R+Pres_R+GDP_Growth_Pct+US_CPI+US_Unemployment.Rate, data=Final_2012_2018_DF_ME_R1)
summary(lm_RVsh_M1)
##### interpretation: XXXXXX
##==================================================