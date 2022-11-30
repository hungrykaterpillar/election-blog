setwd("~/Documents/01-Research/R/Harvard")
# read csv file and create an object, h
Exp_Pred <- read.csv("Expert_Prediction_cleaned.csv")
Ads_Cost <- read.csv("Ads_Cost_2012_2018.csv")
CVAP <- read.csv("cvap_district_2012-2020_clean.csv")
Vote_Inc <- read.csv("incumb_dist_1948-2020 (3).csv")

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

write.csv(Final_2012_2018_D, "Final_2012_2020_D.csv")

############
Final_2012_2020_DF <- read.csv("Final_2012_2020_DF.csv")
Final_2012_2018_DF <- Final_2012_2020_DF %>% left_join(Ads_Cost_2012_2018, by=c("year", "DISTRICT", "STATENAME"))

EE_data_R1 <- read.csv("Election_Economy_R1.csv")
Final_2012_2018_DF_ME_R1 <- EE_data_R1 %>% left_join(Final_2012_2018_DF, by=c("year"))
##### interpretation: XXXXXX
##==================================================
lm_DVsh_M1 <- lm (DemVotesMajorPercent ~ Close_E_D+LLS_D+Pres_D+GDP_Growth_Pct+US_CPI+US_Unemployment.Rate, data=Final_2012_2018_DF_ME_R1)
summary(lm_DVsh_M1)
##### interpretation: XXXXXX
##==================================================

setwd("~/Documents/01-Research/R/Harvard")
Demog_0920 <- read.csv("demographic_2009_2020.csv")
Demog_0920 <- Demog_0920 %>% mutate_at(c('district'), as.numeric)

Demog_0920_R <- Demog_0920 %>%
  rename(DISTRICT = district, STATENAME = state)

Final_Dataset_2012_2020_ME_R2 <- Final_2012_2018_DF_ME_R1 %>% left_join(Demog_0920_R, by=c("year", "DISTRICT", "STATENAME")) 

##==================================================
lm_DVsh_M2 <- lm (DemVotesMajorPercent ~ Close_E_D+LLS_D+Pres_D+GDP_Growth_Pct+US_CPI+US_Unemployment.Rate+female+X20_29+white, data=Final_Dataset_2012_2020_ME_R2)
summary(lm_DVsh_M2)
### interpretation XXXXXXX
#### Adj R-square got improved to 0.7069 and F-statistic shows that the independent variables are jointly significant in estimating the DemVotesMajorPercent
###### female: 1% (0.01) increase in female population increased the democrat major vote share (DemVotesMajorPercent) by 0.4396%
###### X20_29: 1% (0.01) increase in population in 20s (20-29) increased the democrat major vote share (DemVotesMajorPercent) by 0.1975%
###### white: 1% (0.01) increase in white (not Hispanic) population decreased the democrat major vote share (DemVotesMajorPercent) by 0.02473%

lm_DVsh_M3 <- lm (DemVotesMajorPercent ~ Close_E_D+LLS_D+Pres_D+GDP_Growth_Pct+US_CPI+US_Unemployment.Rate+female+X20_29+X65.+white, data=Final_Dataset_2012_2020_ME_R2)
summary(lm_DVsh_M3)
### interpretation XXXXXXX
#### Adj R-square got improved to 0.7080 and F-statistic shows that the independent variables are jointly significant in estimating the DemVotesMajorPercent
###### female: 1% (0.01) increase in female population increased the democrat major vote share (DemVotesMajorPercent) by 0.4286%
###### X20_29: 1% (0.01) increase in population in 20s (20-29) increased the democrat major vote share (DemVotesMajorPercent) by 0.1408% (but significant only at 10%-level, not at 5%)
###### X65.: 1% (0.01) increase in elderly population 65 yrs and older (>=65) decreased the democrat major vote share (DemVotesMajorPercent) by 0.0922% (but significant only at 10%-level, not at 5%)
###### white: 1% (0.01) increase in white (not Hispanic) population decreased the democrat major vote share (DemVotesMajorPercent) by 0.02081%
##==================================================