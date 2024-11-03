
library(tidyverse)
library(readxl)
library(epitools)
library(data.table)

###########################################################################


ospi <- read.csv("C:/Users/dcpcdcfrwk/Downloads/Report_Card_Enrollment_2023-24_School_Year_20241102.csv")

# url <- "https://raw.githubusercontent.com/SPS-By-The-Numbers/sps-by-the-numbers/refs/heads/apps-only/data/2024-06-18-sps-demographic-data.json"
# 
# pathie <- "C:/Users/dcpcdcfrwk/Downloads/2024-06-18-sps-demographic-data.json"
# 
# df <- read_json(pathie, simplifyVector = TRUE)
# 
# df2 <- do.call(rbind.data.frame, df)
# 
# names(df2) <- df2[1,]
# 
# df2 <- df2[-1,]


#####

schools <- read_excel("C:/Users/dcpcdcfrwk/Downloads/School status.xlsx", 
                     sheet = "List")


two <- left_join(schools, ospi, by = "SchoolName") %>%
   select(-OrganizationLevel, -ESDName, -ESDOrganizationID, -DistrictCode, -DistrictName, -DistrictOrganizationId, 
          -SchoolCode, -SchoolOrganizationID, -County, -CurrentSchoolType, -GradeLevel ) %>%
   mutate(
         #  perc_male = Male/All.Students *100,
         #  perc_AIAN = American.Indian..Alaskan.Native/All.Students *100,
         #  perc_Asian = Asian / All.Students *100,
         #  perc_Black = Black..African.American / All.Students *100,
         #  perc_Hisp = Hispanic..Latino.of.any.race.s. / All.Students *100,
         #  perc_NHPI = Native.Hawaiian..Other.Pacific.Islander / All.Students *100,
         #  perc_multirace = Two.or.More.Races / All.Students *100,
         #  perc_White = White / All.Students *100,
         #  perc_ESL = English.Language.Learners/All.Students *100,
          perc_homeless = Homeless/All.Students *100,
         #  perc_LowSES = Low.Income/All.Students *100,
         #  perc_SPED = Students.with.Disabilities/All.Students *100,
          
          aian_nhpi = American.Indian..Alaskan.Native + Native.Hawaiian..Other.Pacific.Islander,
          n_notWhite = All.Students - White,
          
          status_3 = ifelse(is.na(Status), "Neither", Status),
          close_yn = ifelse(status_3 == "Closing", 1, 0),
          change_yn = ifelse(status_3 == "Closing" | status_3 == "Receiving", 1, 0),
          
          all = Students.with.Disabilities + Students.without.Disabilities,
          same = ifelse(all == All.Students, 1,0 )
          )

table(two$SchoolYear)

#####
SPED_m <- two %>%
   group_by(close_yn) %>%
   summarise(sum_notSPED = sum(Students.without.Disabilities),
             sum_SPED = sum(Students.with.Disabilities)) %>% 
   column_to_rownames("close_yn")
   # `rownames<-`(.[,1])
# rownames(SPED_m) = c("No", "Yes") 
SPED_m <- as.matrix(SPED_m)
oddsratio(SPED_m)

SPED_change <- two %>%
   group_by(change_yn) %>%
   summarise(sum_notSPED = sum(Students.without.Disabilities),
             sum_SPED = sum(Students.with.Disabilities)) %>% 
   column_to_rownames("change_yn")
SPED_change_m <- as.matrix(SPED_change)
SPED_change_m
oddsratio(SPED_change_m)

#####
White_RE <- two %>%
   group_by(close_yn) %>%
   summarise(sum_notwhite = sum(n_notWhite),
             sum_white = sum(White)   ) %>% 
   column_to_rownames("close_yn") 
   # `rownames<-`(.[,1])
White_RE_m <- as.matrix(White_RE)

oddsratio(White_RE_m)


#####
ESL <- two %>%
   group_by(close_yn) %>%
   summarise(sum_notESL = sum(Non.English.Language.Learners),
             sum_ESL = sum(English.Language.Learners)   ) %>% 
   column_to_rownames("close_yn") 
# `rownames<-`(.[,1])
ESL_m <- as.matrix(ESL)
ESL_m
oddsratio(ESL_m)

ESL_change <- two %>%
   group_by(change_yn) %>%
   summarise(sum_notESL = sum(Non.English.Language.Learners),
             sum_ESL = sum(English.Language.Learners)   ) %>% 
   column_to_rownames("change_yn") 
# `rownames<-`(.[,1])
ESL_change_mat <- as.matrix(ESL_change)
ESL_change_mat
oddsratio(ESL_change_mat)


#####
Income <- two %>%
   group_by(close_yn) %>%
   summarise(sum_notLow = sum(Non.Low.Income),
             sum_Low = sum(Low.Income)   ) %>% 
   column_to_rownames("close_yn") 
# `rownames<-`(.[,1])
Income_m <- as.matrix(Income)
oddsratio(Income_m)

Income_change <- two %>%
   group_by(change_yn) %>%
   summarise(sum_notLow = sum(Non.Low.Income),
             sum_Low = sum(Low.Income)   ) %>% 
   column_to_rownames("change_yn") 
# `rownames<-`(.[,1])
Income_change_m <- as.matrix(Income_change)
Income_change_m
oddsratio(Income_change_m)

#####
oddsratio(Homeless_m)

Homeless <- two %>%
   group_by(close_yn) %>%
   summarise(sum_notH= sum(Non.Homeless),
             sum_H = sum(Homeless)   ) %>% 
   column_to_rownames("close_yn") 
# `rownames<-`(.[,1])
Homeless_m <- as.matrix(Homeless)

Homeless <- two %>%
   group_by(change_yn) %>%
   summarise(sum_notH= sum(Non.Homeless),
             sum_H = sum(Homeless)   ) %>% 
   column_to_rownames("change_yn") 
# `rownames<-`(.[,1])
Homeless_m2 <- as.matrix(Homeless)
Homeless_m2
oddsratio(Homeless_m2)

###########################################################################
White_RE_region <- two %>%
   group_by(Region, close_yn) %>%
   summarise(sum_notwhite = sum(n_notWhite),
             sum_white = sum(White),
             total = sum(All.Students),
             p_white = sum_white/total *100,
             p_another = sum_notwhite / total *100) %>%
   filter(Region != "SE")


##############################################################################################
#####
SPED <- two %>%
   group_by(close_yn) %>%
   summarise(sum_notSPED = sum(Students.without.Disabilities),
             sum_SPED = sum(Students.with.Disabilities)) 

long <- melt(setDT(SPED), id.vars = c("close_yn"), variable.name = "disability")

m_sped <- glm(close_yn ~ disability , 
              data = long, 
              weights = value, 
              family = binomial("logit"))

exp(cbind(OR = coef(m_sped), confint(m_sped)))

##
SPED <- two %>%
   group_by(Region, close_yn) %>%
   summarise(sum_notSPED = sum(Students.without.Disabilities),
             sum_SPED = sum(Students.with.Disabilities)) %>%
   filter(Region !="SE")

long <- melt(setDT(SPED), id.vars = c("Region","close_yn"), variable.name = "disability")

m_sped <- glm(close_yn ~ disability + Region , 
              data = long, 
              weights = value, 
              family = binomial("logit"))

exp(cbind(OR = coef(m_sped), confint(m_sped)))

####
SPED2 <- two %>%
   group_by(change_yn) %>%
   summarise(sum_notSPED = sum(Students.without.Disabilities),
             sum_SPED = sum(Students.with.Disabilities)) 

long2 <- melt(setDT(SPED2), id.vars = c("change_yn"), variable.name = "disability")

m_sped2 <- glm(change_yn ~ disability , 
              data = long2, 
              weights = value, 
              family = binomial("logit"))

exp(cbind(OR = coef(m_sped2), confint(m_sped2)))

SPED3 <- two %>%
   group_by(Region, change_yn) %>%
   summarise(sum_notSPED = sum(Students.without.Disabilities),
             sum_SPED = sum(Students.with.Disabilities)) %>%
   filter(Region !="SE")

long3 <- melt(setDT(SPED3), id.vars = c("Region","change_yn"), variable.name = "disability")

m_sped3 <- glm(change_yn ~ disability + Region , 
              data = long3, 
              weights = value, 
              family = binomial("logit"))

exp(cbind(OR = coef(m_sped3), confint(m_sped3)))


##################################################################################
##### RE unadjusted
RE <- two %>%
   group_by( close_yn) %>%
   summarise(sWhite = sum(White),
             sAIAN_NHPI = sum(aian_nhpi),
             sAsian = sum(Asian),
             sBlack = sum(Black..African.American ),
             sHisp = sum(Hispanic..Latino.of.any.race.s.),
             smultirace = sum(Two.or.More.Races))

RE_long <- melt(setDT(RE), id.vars = c("close_yn"), variable.name = "race_eth")

m_re <- glm(close_yn ~ race_eth ,
              data = RE_long,
              weights = value,
              family = binomial("logit"))

exp(cbind(OR = coef(m_re), confint(m_re)))

#### RE close adjusted
RE <- two %>%
   group_by(Region, close_yn) %>%
   summarise(sWhite = sum(White),
             sAIAN_NHPI = sum(aian_nhpi),
             sAsian = sum(Asian),
             sBlack = sum(Black..African.American ),
             sHisp = sum(Hispanic..Latino.of.any.race.s.),
             smultirace = sum(Two.or.More.Races)
   ) %>%
   filter(Region != "SE")

RE_long <- melt(setDT(RE), id.vars = c("Region","close_yn"), variable.name = "race_eth")

m_re <- glm(close_yn ~ race_eth + Region , 
              data = RE_long, 
              weights = value, 
              family = binomial("logit"))

exp(cbind(OR = coef(m_re), confint(m_re)))

#### Re affected Unadjusted
RE2 <- two %>%
   group_by(change_yn) %>%
   summarise(sWhite = sum(White),
             sAIAN_NHPI = sum(aian_nhpi),
             sAsian = sum(Asian),
             sBlack = sum(Black..African.American ),
             sHisp = sum(Hispanic..Latino.of.any.race.s.),
             smultirace = sum(Two.or.More.Races)   )

RE_long2 <- melt(setDT(RE2), id.vars = c("change_yn"), variable.name = "race_eth")

m_re2 <- glm(change_yn ~ race_eth ,
            data = RE_long2,
            weights = value,
            family = binomial("logit"))

exp(cbind(OR = coef(m_re2), confint(m_re2)))

#### Re affected Adjusted
RE2 <- two %>%
   group_by(Region, change_yn) %>%
   summarise(sWhite = sum(White),
             sAIAN_NHPI = sum(aian_nhpi),
             sAsian = sum(Asian),
             sBlack = sum(Black..African.American ),
             sHisp = sum(Hispanic..Latino.of.any.race.s.),
             smultirace = sum(Two.or.More.Races)   ) %>%
   filter(Region !="SE")

RE_long2 <- melt(setDT(RE2), id.vars = c("Region","change_yn"), variable.name = "race_eth")

m_re3 <- glm(change_yn ~ race_eth + Region , 
            data = RE_long2, 
            weights = value, 
            family = binomial("logit"))

exp(cbind(OR = coef(m_re3), confint(m_re3)))



#################################################################################
##### income change unadjusted
income <- two %>%
   group_by(close_yn) %>%
   summarise(sum_notLow = sum(Non.Low.Income),
             sum_Low = sum(Low.Income)  )

income_long <- melt(setDT(income), id.vars = c("close_yn"), variable.name = "income")

m_i <- glm(close_yn ~ income ,
              data = income_long,
              weights = value,
              family = binomial("logit"))

exp(cbind(OR = coef(m_i), confint(m_i)))
##### income change adjusted
income <- two %>%
   group_by(Region, close_yn) %>%
   summarise(sum_notLow = sum(Non.Low.Income),
             sum_Low = sum(Low.Income)  ) %>%
   filter(Region != "SE")

income_long <- melt(setDT(income), id.vars = c("Region","close_yn"), variable.name = "income")

m_income <- glm(close_yn ~ income +Region , 
            data = income_long, 
            weights = value, 
            family = binomial("logit"))

exp(cbind(OR = coef(m_income), confint(m_income)))

### income affected unadjusted
income2 <- two %>%
   group_by(change_yn) %>%
   summarise(sum_notLow = sum(Non.Low.Income),
             sum_Low = sum(Low.Income)  ) 

income_long2 <- melt(setDT(income2), id.vars = c("change_yn"), variable.name = "income")

m_income3 <- glm(change_yn ~ income , 
                 data = income_long2, 
                 weights = value, 
                 family = binomial("logit"))

exp(cbind(OR = coef(m_income3), confint(m_income3)))

### income affected adjusted
income3 <- two %>%
   group_by(Region, change_yn) %>%
   summarise(sum_notLow = sum(Non.Low.Income),
             sum_Low = sum(Low.Income)  ) %>%
   filter(Region != "SE")

income_long3 <- melt(setDT(income3), id.vars = c("Region","change_yn"), variable.name = "income")

m_income3 <- glm(change_yn ~ income +Region , 
                data = income_long3, 
                weights = value, 
                family = binomial("logit"))

exp(cbind(OR = coef(m_income3), confint(m_income3)))

#########################################################################
### unhoused closed unadjusted
unhoused <- two %>%
   group_by( close_yn) %>%
   summarise(sum_notH= sum(Non.Homeless),
             sum_H = sum(Homeless)  ) 

unhoused_long <- melt(setDT(unhoused), id.vars = c("close_yn"), variable.name = "housing")

m_house <- glm(close_yn ~ housing ,
              data = unhoused_long,
              weights = value,
              family = binomial("logit"))

exp(cbind(OR = coef(m_house), confint(m_house)))

### unhoused closed adjusted
unhoused2 <- two %>%
   group_by(Region, close_yn) %>%
   summarise(sum_notH= sum(Non.Homeless),
             sum_H = sum(Homeless)  ) %>%
   filter(Region != "SE")

unhoused_long2 <- melt(setDT(unhoused2), id.vars = c("Region","close_yn"), variable.name = "housing")

m_house2 <- glm(close_yn ~ housing + Region , 
                data = unhoused_long2, 
                weights = value, 
                family = binomial("logit"))

exp(cbind(OR = coef(m_house2), confint(m_house2)))

### unhoused change unadjusted
unhoused3 <- two %>%
   group_by( change_yn) %>%
   summarise(sum_notH= sum(Non.Homeless),
             sum_H = sum(Homeless)  ) 
unhoused_long3 <- melt(setDT(unhoused3), id.vars = c("change_yn"), variable.name = "housing")

m_house3 <- glm(change_yn ~ housing  , 
               data = unhoused_long3, 
               weights = value, 
               family = binomial("logit"))

exp(cbind(OR = coef(m_house3), confint(m_house3)))

### unhoused change adjusted
unhoused2 <- two %>%
   group_by(Region, change_yn) %>%
   summarise(sum_notH= sum(Non.Homeless),
             sum_H = sum(Homeless)  ) %>%
   filter(Region != "SE")
unhoused_long2 <- melt(setDT(unhoused2), id.vars = c("Region","change_yn"), variable.name = "housing")

m_house3 <- glm(change_yn ~ housing + Region , 
                data = unhoused_long2, 
                weights = value, 
                family = binomial("logit"))

exp(cbind(OR = coef(m_house3), confint(m_house3)))


############################################################################
### esl change unadjusted
esl <- two %>%
   group_by(close_yn) %>%
   summarise(sum_notESL = sum(Non.English.Language.Learners),
             sum_ESL = sum(English.Language.Learners)  ) 

esl_long <- melt(setDT(esl), id.vars = c("close_yn"), variable.name = "language")

m_e <- glm(close_yn ~ language ,
              data = esl_long,
              weights = value,
              family = binomial("logit"))

exp(cbind(OR = coef(m_e), confint(m_e)))

### esl change adjusted
esl <- two %>%
   group_by(Region, close_yn) %>%
   summarise(sum_notESL = sum(Non.English.Language.Learners),
             sum_ESL = sum(English.Language.Learners)  ) %>%
   filter(Region != "SE")

esl_long <- melt(setDT(esl), id.vars = c("Region","close_yn"), variable.name = "language")

m_esl <- glm(close_yn ~ language + Region , 
               data = esl_long, 
               weights = value, 
               family = binomial("logit"))

exp(cbind(OR = coef(m_esl), confint(m_esl)))
 
### esl change unadjusetd
esl4 <- two %>%
   group_by( change_yn) %>%
   summarise(sum_notESL = sum(Non.English.Language.Learners),
             sum_ESL = sum(English.Language.Learners)  ) 

esl_long4 <- melt(setDT(esl4), id.vars = c("change_yn"), variable.name = "language")

m_esl4 <- glm(change_yn ~ language , 
             data = esl_long4, 
             weights = value, 
             family = binomial("logit"))

exp(cbind(OR = coef(m_esl4), confint(m_esl4)))



### esl change adjusetd
esl2 <- two %>%
   group_by(Region, change_yn) %>%
   summarise(sum_notESL = sum(Non.English.Language.Learners),
             sum_ESL = sum(English.Language.Learners)  ) %>%
   filter(Region != "SE")

esl_long2 <- melt(setDT(esl2), id.vars = c("Region","change_yn"), variable.name = "language")

m_esl3 <- glm(change_yn ~ language + Region , 
              data = esl_long2, 
              weights = value, 
              family = binomial("logit"))

exp(cbind(OR = coef(m_esl3), confint(m_esl3)))
###########################################################################

cycle1 <- read_excel("C:/Users/dcpcdcfrwk/Downloads/SPS P223 data from 2019-2024 for publication.xlsx", 
                   sheet = "raw data")


es <- cycle1 %>%
   #keep ES grades and Oct 2024 counts
   filter(School != "Non-Public Agencies") %>%
   filter(Grade %in% c("1.0", "2.0", "3.0", "4.0", "5.0", "K", "6.0", "Preschool")) %>%
   filter(Month ==  as.POSIXct("2024-10-01", tz="UTC")) %>%
   
   #get total counts for each school
   group_by(School) %>%
   summarise(total_count=n(), 
             Reg_sum = sum(`Regular Program`),
             Bil_sum = sum(`Bilingual Served`),
             SPED_sum = sum(`Spec. Ed. Served`),
             total_students = sum(`P223 Total Count`)
             ) %>%
   mutate(      k_8 = grepl("K-8", School, fixed = TRUE)) %>%
   #keep only k-5 ES
   filter(total_count == 6 | total_count == 7)  %>%
   filter( k_8 =="FALSE") %>%
   
   #label closing/receiving schools
   mutate(close_receiving_3 = ifelse(School %in% c("Sacajawea", "North Beach", "Stevens", "Highland Park"), "Closing", 
                                   ifelse(School %in% c("John Rogers", "Viewlands", "Montlake", "Sanislo"),"Receiving", "No change")),
          
          change_yn = ifelse(close_receiving_3 == "No change", "No change", "Change"),
          
          close_yn = ifelse(close_receiving_3 == "Closing", "Closing", "Not Closing"),
          
          #percents of students
          perc_reg = Reg_sum/total_students *100,
          perc_biling = Bil_sum/total_students *100,
          perc_SPED = SPED_sum/total_students *100,
          )

# table(es$close_receiving_3)
   

### percents by school status - change v no change
perc_groups <- es %>%
   group_by(change_yn) %>%
   mutate(regular_prog= sum(Reg_sum),
          bilingual = sum(Bil_sum),
          SPED = sum(SPED_sum),
          total = sum(total_students),
          #percents of students
          perc_reg2 = regular_prog/total *100,
          perc_biling2 = bilingual/total *100,
          perc_SPED2 = SPED/total *100) %>%
   filter(row_number()==1) %>%
   select(change_yn, perc_reg2, perc_biling2, perc_SPED2)

perc_groups


### percents by school status - close v not closing
perc_group_close <- es %>%
   group_by(close_yn) %>%
   mutate(regular_prog= sum(Reg_sum),
          bilingual = sum(Bil_sum),
          SPED = sum(SPED_sum),
          total = sum(total_students),
          #percents of students
          perc_reg2 = regular_prog/total *100,
          perc_biling2 = bilingual/total *100,
          perc_SPED2 = SPED/total *100) %>%
   filter(row_number()==1) %>%
   select(change_yn, regular_prog, SPED, total, perc_reg2, perc_biling2, perc_SPED2)

perc_group_close

