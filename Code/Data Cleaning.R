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

data <- read_csv("Data_Translation_Assignment/Data/cps_00004.csv")
ind <- read_csv('Data_Translation_Assignment/Data/indnames.csv')
ind <- ind %>% mutate(IND = ind)

data1 <- left_join(data, ind)

data_part1 <- data1 %>% filter(indname == "Retail Trade")
data_part2 <- data_part1 %>%  unite(date1, c("YEAR", "MONTH"), sep ="-") %>%
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

data_emp <- data_part4 %>% filter(emp == "employed") %>% mutate(totalemployed = n) %>% 
  group_by(date) %>% summarize(totalemployed = mean(totalemployed))
data_unemp <- data_part4 %>% filter(emp == "unemployed") %>% mutate(totalunemployed = n) %>% 
  group_by(date) %>% summarize(totalunemployed = mean(totalunemployed))
data_emp1 <- left_join(data_emp, data_unemp)

data_part5 <- left_join(data_part4, data_emp1)
data_part6 <- data_part5 %>% mutate(urate = (totalunemployed/(totalemployed+totalunemployed))*100) %>%
  mutate(date_num = case_when(
    date < ymd("2020-01-01") ~ as.numeric(str_sub(date, 6, 7)), 
    date >= ymd("2020-01-01") ~ as.numeric(str_sub(date, 6, 7))+12
  )) %>% mutate(date_centered = date_num - 16)

test2 <- data_part6 %>% group_by(date_num) %>% summarize(urate = mean(urate))

test <- data_part5 %>% group_by(date) %>% summarize(totalemployed = mean(totalemployed), totalunemployed = mean(totalunemployed)) %>%
  mutate(urate = (totalunemployed/(totalemployed+totalunemployed))*100) %>%
  mutate(covid = date > ymd("2020-03-01"))
sum(test$totalemployed) + sum(test$totalunemployed)

test_graph <- data_part6 %>%
  ggplot(aes(x = date, y = urate, col = covid)) +
  geom_point() + 
  geom_vline(xintercept = ymd("2020-04-01")) +
  geom_smooth(aes(group = covid), method = 'lm', formula = y ~ poly(x, 2))
test_graph

model <- lm(urate ~ date_centered*covid, data = data_part6)
export_summs(model)

model2 <- lm(urate ~ date_centered*covid + I(date_centered^2)*covid, data = data_part6)
export_summs(model2)

model1 <- rdrobust(data_part6$urate, data_part6$date_centered, c = 0)

rdplot(data_part6$urate, data_part6$date_centered)
