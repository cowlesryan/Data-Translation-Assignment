library(tidyverse)
library(purrr)
library(lubridate)
library(vtable)
library(car)
library(jtools)
library(fixest)
library(wooldridge)
library(stringr)
library(vtable)
library(plm)
library(rdrobust)

data <- read_csv("Data_Translation_Assignment/Data/cps_00006.csv")
ind <- read_csv('Data_Translation_Assignment/Data/indnames.csv')
ind <- ind %>% mutate(IND = ind)

data1 <- left_join(data, ind)
data1 <- data1 %>% mutate(EARNWEEK = ifelse(EARNWEEK > 9999, NA_real_, EARNWEEK))

data_part1 <- data1
data_part2 <- data_part1 %>% filter(!is.na(indname)) %>%  unite(date1, c("YEAR", "MONTH"), sep ="-") %>%
  mutate(date = ym(date1)) %>% mutate(covid = date > ymd("2020-03-01"))

data_part3 <- data_part2 %>% mutate(emp = case_when(
  EMPSTAT == 10 ~ "employed",
  EMPSTAT == 01 ~ "employed",
  EMPSTAT == 12 ~ "employed",
  EMPSTAT == 20 ~ "unemployed",
  EMPSTAT == 21 ~ "unemployed",
  EMPSTAT == 22 ~ "unemployed"
))

data_part4 <- data_part3 %>% filter(!is.na(emp)) %>% group_by(date, emp) %>% mutate(n = n())

testing1 <- data_part4 %>% mutate(employed = case_when(
  emp == "employed" ~ 1,
  emp == "unemployed" ~ 0
)) %>% 
  mutate(unemployed = case_when(
    emp == "employed" ~ 0,
    emp == "unemployed" ~ 1
  )) %>% group_by(date, emp) %>% summarize(sum_emp = sum(employed), sum_unem = sum(unemployed))

data_emp <- data_part4 %>% filter(emp == "employed") %>% mutate(totalemployed = n) %>% 
  group_by(date) %>% summarize(totalemployed = mean(totalemployed))
data_unemp <- data_part4 %>% filter(emp == "unemployed") %>% mutate(totalunemployed = n) %>% 
  group_by(date) %>% summarize(totalunemployed = mean(totalunemployed))
data_emp1 <- left_join(data_emp, data_unemp)

data_part5 <- left_join(data_part4, data_emp1)
data_part6 <- data_part5 %>% filter(RACE != 999, EDUC != c(000, 001, 999), indname != c("Military"), REGION != c(99, 97), FAMINC <= 995) %>% 
  mutate(urate = (totalunemployed/(totalemployed+totalunemployed))*100) %>%
  mutate(date_num = case_when(
    date < ymd("2020-01-01") ~ as.numeric(str_sub(date, 6, 7)), 
    date >= ymd("2020-01-01") ~ as.numeric(str_sub(date, 6, 7))+12
  )) %>% mutate(date_centered = date_num - 16) %>%
  mutate(race = case_when(
    RACE == 100 ~ "white",
    RACE == 200 ~ "black",
    RACE == 300 ~ "nativeamerican",
    RACE == 651 ~ "asian", 
    RACE == 700 ~ "other"
  )) %>% mutate(education = case_when(
    EDUC <= 071 ~ "nohighschooldegree",
    EDUC == 073 ~ "highschool",
    EDUC == 111 ~ "bachelors", 
    EDUC == 081 ~ "somecollegenodegree",
    EDUC == 091 ~ "AssocDegreeOCC",
    EDUC == 092 ~ "AssocDegreeAcademic",
    EDUC == 123 ~ "masters",
    EDUC == 125 ~ "phd"
  )) %>% mutate(familyincome = case_when(
    FAMINC > 830 ~ "highearner",
    FAMINC < 830 ~ "lowearner"
  )) %>% mutate(familyincome2 = case_when(
    FAMINC <= 720 ~ "bottom25%",
    FAMINC >= 842 ~ "top25%"
  )) %>%  mutate(region = case_when(
    REGION == 11 ~ "NewEnglandDivision",
    REGION == 12 ~ "MiddleAtlanticDivision",
    REGION == 21 ~ "EastNorthCentralDivision",
    REGION == 22 ~ "WestNorthCentralDivision",
    REGION == 31 ~ "SouthAtlanticDivision",
    REGION == 32 ~ "EastSouthCentralDivision",
    REGION == 33 ~ "WestSouthCentralDivision",
    REGION == 41 ~ "MountainDivision",
    REGION == 42 ~ "PacificDivision"
  )) %>% 
  mutate(familyincome = factor(familyincome), education = factor(education), race = factor(race), REGION = factor(REGION),
                indname = factor(indname), familyincome2 = factor(familyincome2), region = factor(region))

data_part6 <- within(data_part6, familyincome <- relevel(familyincome, ref = "lowearner"))
data_part6 <- within(data_part6, education <- relevel(education, ref = "highschool"))
data_part6 <- within(data_part6, race <- relevel(race, ref = "white"))
data_part6 <- within(data_part6, REGION <- relevel(REGION, ref = "42"))
data_part6 <- within(data_part6, indname <- relevel(indname, ref = "Retail Trade"))
data_part6 <- within(data_part6, familyincome2 <- relevel(familyincome2, ref = "bottom25%"))
data_part6 <- within(data_part6, region <- relevel(region, ref = "PacificDivision"))

write_csv(data_part6, file = "Data_Translation_Assignment/Data/FinalData.csv")

# RACE == 700 ~ "other",    RACE == 650 ~ "api",     EDUC < 071 ~ "someschoolnodiploma",

test2 <- data_part6 %>% group_by(date_num) %>% summarize(urate = mean(urate))

test <- data_part5 %>% group_by(date) %>% summarize(totalemployed = mean(totalemployed), totalunemployed = mean(totalunemployed)) %>%
  mutate(urate = (totalunemployed/(totalemployed+totalunemployed))*100) %>%
  mutate(covid = date > ymd("2020-03-01"))
sum(test$totalemployed) + sum(test$totalunemployed)

# Question 1 Regression
model1_part1 <- lm(urate ~ date_centered*covid + region, data = data_part6)
export_summs(model1_part1)
model2_part1 <- lm(urate ~ date_centered*covid + I(date_centered^2)*covid + region, data = data_part6)
export_summs(model2_part1)
linearHypothesis(model, c("I(date_centered^2)=0", "covidTRUE:I(date_centered^2)=0"))
linearHypothesis(model1_part1, matchCoefs(model1_part1, "education"))
linearHypothesis(model1_part1, matchCoefs(model1_part1, "familyincome"))
linearHypothesis(model1_part1, matchCoefs(model1_part1, "REGION"))
linearHypothesis(model2_part1, matchCoefs(model2_part1, "familyincome"))

#write_csv(data_part6, file = "Data_Translation_Assignment/Data/FinalDataRetail.csv")

#Question 2 Regression
model1_part2 <- lm(urate ~ date_centered*covid + covid*indname + region, data = data_part6)
export_summs(model1_part2)
model2_part2 <- lm(urate ~ date_centered*covid + I(date_centered^2)*covid + covid*indname + region, data = data_part6)
export_summs(model2_part2)


#Question 3 Regression
model1_part3 <- lm(urate ~ date_centered*covid + covid*region, data = data_part6)
export_summs(model1_part3)
model3_part3 <- lm(urate ~ date_centered*covid + region + covid*education, data = data_part6)
export_summs(model3_part3)
model2_part3 <- lm(urate ~ date_centered*covid + region + covid*familyincome, data = data_part6)
export_summs(model2_part3)

#write_csv(data_part6, file = "Data_Translation_Assignment/Data/FinalDataAllInd.csv")
