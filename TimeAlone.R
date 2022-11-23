library(here)
library(tidyverse)
dfrespondents <- read_csv(here("data", "atusresp_2021.dat"))
dfsummary <- read_csv(here("data", "atussum_2021.dat"))

colnames(dfsummary)

df_alone <- select(dfrespondents, TRTALONE, TUCASEID)

df_char <- select(dfsummary, TUCASEID, TEAGE, TESEX)
 
alone_time21 <- left_join(df_alone,df_char, by="TUCASEID")

alone_time <- alone_time21 |> mutate(
  gender = case_when(TESEX == 1 ~ "male", TRUE ~ "female")) |> 
  mutate(
    age_group = case_when(
      TEAGE <=20 ~ "15-20",
      TEAGE >20 & TEAGE <=25 ~ "15-25",
      TEAGE >25 & TEAGE <=30 ~ "26-30",
      TEAGE >30 & TEAGE <=35 ~ "31-35",
      TEAGE >35 & TEAGE <=40 ~ "36-40",
      TEAGE >40 & TEAGE <=45 ~ "41-45",
      TEAGE >45 & TEAGE <=50 ~ "46-50",
      TEAGE >50 & TEAGE <=55 ~ "51-55",
      TEAGE >55 & TEAGE <=60 ~ "56-60",
      TEAGE >60 & TEAGE <=65 ~ "61-65",
      TEAGE >65  ~ "66+"),
    age_group_index = case_when(
      TEAGE <=20 ~ 1,
      TEAGE >20 & TEAGE <=25 ~ 2,
      TEAGE >25 & TEAGE <=30 ~ 3,
      TEAGE >30 & TEAGE <=35 ~ 4,
      TEAGE >35 & TEAGE <=40 ~ 5,
      TEAGE >40 & TEAGE <=45 ~ 6,
      TEAGE >45 & TEAGE <=50 ~ 7,
      TEAGE >50 & TEAGE <=55 ~ 8,
      TEAGE >55 & TEAGE <=60 ~ 9,
      TEAGE >60 & TEAGE <=65 ~ 10,
      TEAGE >65  ~ 11))

alone_time_mean_single_year <- alone_time |> 
  group_by(TEAGE, gender) |> 
  summarize(mean_time_alone = mean(TRTALONE)) 

ggplot(
    data = alone_time_mean_single_year,
    mapping = aes(
      x = TEAGE, 
      y = mean_time_alone,
      color = gender)) +
  geom_line()

alone_time_mean_age_group <- alone_time |> 
  group_by(age_group, age_group_index, gender) |> 
  summarize(mean_time_alone = mean(TRTALONE), .groups = "drop") 

write_csv(alone_time_mean_age_group,"alone_time_mean_age_group.csv")

ggplot(
  data = alone_time_mean_age_group,
  mapping = aes(
    x = age_group_index, 
    y = mean_time_alone,
    color = gender)) +
  geom_line()

ggplot(
  data = alone_time_mean_age_group,
  mapping = aes(
    x = age_group, 
    y = mean_time_alone,
    fill = gender)) +
  geom_col(position = position_dodge())

## Get 2019 data to compare and calculate change

dfrespondents19 <- read_csv(here("data", "atusresp_2019.dat"))
dfsummary19 <- read_csv(here("data", "atussum_2019.dat"))

df_alone19 <- select(dfrespondents19, TRTALONE, TUCASEID)

df_char19 <- select(dfsummary19, TUCASEID, TEAGE, TESEX)

alone_time19 <- left_join(df_alone19,df_char19, by="TUCASEID")

alone_time19 <- alone_time19 |> mutate(
  gender = case_when(TESEX == 1 ~ "male", TRUE ~ "female")) |> 
  mutate(
    age_group = case_when(
      TEAGE <=20 ~ "15-20",
      TEAGE >20 & TEAGE <=25 ~ "15-25",
      TEAGE >25 & TEAGE <=30 ~ "26-30",
      TEAGE >30 & TEAGE <=35 ~ "31-35",
      TEAGE >35 & TEAGE <=40 ~ "36-40",
      TEAGE >40 & TEAGE <=45 ~ "41-45",
      TEAGE >45 & TEAGE <=50 ~ "46-50",
      TEAGE >50 & TEAGE <=55 ~ "51-55",
      TEAGE >55 & TEAGE <=60 ~ "56-60",
      TEAGE >60 & TEAGE <=65 ~ "61-65",
      TEAGE >65  ~ "66+"),
    age_group_index = case_when(
      TEAGE <=20 ~ 1,
      TEAGE >20 & TEAGE <=25 ~ 2,
      TEAGE >25 & TEAGE <=30 ~ 3,
      TEAGE >30 & TEAGE <=35 ~ 4,
      TEAGE >35 & TEAGE <=40 ~ 5,
      TEAGE >40 & TEAGE <=45 ~ 6,
      TEAGE >45 & TEAGE <=50 ~ 7,
      TEAGE >50 & TEAGE <=55 ~ 8,
      TEAGE >55 & TEAGE <=60 ~ 9,
      TEAGE >60 & TEAGE <=65 ~ 10,
      TEAGE >65  ~ 11))

alone_time_mean_single_year19 <- alone_time19 |> 
  group_by(TEAGE, gender) |> 
  summarize(mean_time_alone = mean(TRTALONE))

ggplot(
  data = alone_time_mean_single_year19,
  mapping = aes(
    x = TEAGE, 
    y = mean_time_alone,
    color = gender)) +
  geom_line()

alone_time_mean_age_group19 <- alone_time19 |> 
  group_by(age_group, age_group_index, gender) |> 
  summarize(mean_time_alone = mean(TRTALONE), .groups = "drop") 

ggplot(
  data = alone_time_mean_age_group19,
  mapping = aes(
    x = age_group, 
    y = mean_time_alone,
    fill = gender)) +
  geom_col(position = position_dodge())

write_csv(alone_time_mean_age_group19,"alone_time_mean_age_group19.csv")

mean_alone_time19 <- alone_time19 |> mutate(
  gender = case_when(TESEX == 1 ~ "male", TRUE ~ "female")) |> 
  mutate(age_group_simple = case_when(
    TEAGE >=15 & TEAGE <=40 ~ "15-40",
    TRUE ~ "other")) |> 
  group_by(age_group_simple, gender) |> 
  summarize(mean_time_alone19 = mean(TRTALONE))

mean_alone_time21 <- alone_time |> mutate(
  gender = case_when(TESEX == 1 ~ "male", TRUE ~ "female")) |> 
  mutate(age_group_simple = case_when(
    TEAGE >=15 & TEAGE <=40 ~ "15-40",
    TRUE ~ "other")) |> 
  group_by(age_group_simple, gender) |> 
  summarize(mean_time_alone21 = mean(TRTALONE))

change_in_alone_time <- left_join(
  mean_alone_time19, 
  mean_alone_time21, 
  by=c("age_group_simple", "gender")) |> 
  mutate(percent_change = ((mean_time_alone21 - mean_time_alone19) / mean_time_alone19) *100)

## Compare to 10 years ago

dfrespondents11 <- read_csv(here("data", "atusresp_2011.dat"))
dfsummary11 <- read_csv(here("data", "atussum_2011.dat"))

df_alone11 <- select(dfrespondents11, TRTALONE, TUCASEID)

df_char11 <- select(dfsummary11, tucaseid, TEAGE, TESEX) |> 
  rename(TUCASEID=tucaseid)

alone_time11 <- left_join(df_alone11,df_char11, by="TUCASEID")

alone_time11 <- alone_time11 |> mutate(
  gender = case_when(TESEX == 1 ~ "male", TRUE ~ "female")) |> 
  mutate(
    age_group = case_when(
      TEAGE <=20 ~ "15-20",
      TEAGE >20 & TEAGE <=25 ~ "21-25",
      TEAGE >25 & TEAGE <=30 ~ "26-30",
      TEAGE >30 & TEAGE <=35 ~ "31-35",
      TEAGE >35 & TEAGE <=40 ~ "36-40",
      TEAGE >40 & TEAGE <=45 ~ "41-45",
      TEAGE >45 & TEAGE <=50 ~ "46-50",
      TEAGE >50 & TEAGE <=55 ~ "51-55",
      TEAGE >55 & TEAGE <=60 ~ "56-60",
      TEAGE >60 & TEAGE <=65 ~ "61-65",
      TEAGE >65  ~ "66+"),
    age_group_index = case_when(
      TEAGE <=20 ~ 1,
      TEAGE >20 & TEAGE <=25 ~ 2,
      TEAGE >25 & TEAGE <=30 ~ 3,
      TEAGE >30 & TEAGE <=35 ~ 4,
      TEAGE >35 & TEAGE <=40 ~ 5,
      TEAGE >40 & TEAGE <=45 ~ 6,
      TEAGE >45 & TEAGE <=50 ~ 7,
      TEAGE >50 & TEAGE <=55 ~ 8,
      TEAGE >55 & TEAGE <=60 ~ 9,
      TEAGE >60 & TEAGE <=65 ~ 10,
      TEAGE >65  ~ 11))

alone_time_mean_single_year11 <- alone_time11 |> 
  group_by(TEAGE, gender) |> 
  summarize(mean_time_alone = mean(TRTALONE))

ggplot(
  data = alone_time_mean_single_year11,
  mapping = aes(
    x = TEAGE, 
    y = mean_time_alone,
    color = gender)) +
  geom_line()

alone_time_mean_age_group11 <- alone_time11 |> 
  group_by(age_group, age_group_index, gender) |> 
  summarize(mean_time_alone = mean(TRTALONE), .groups = "drop") 

ggplot(
  data = alone_time_mean_age_group11,
  mapping = aes(
    x = age_group, 
    y = mean_time_alone,
    fill = gender)) +
  geom_col(position = position_dodge())

write_csv(alone_time_mean_age_group11,"alone_time_mean_age_group11.csv")

mean_alone_time11 <- alone_time11 |> mutate(
  gender = case_when(TESEX == 1 ~ "male", TRUE ~ "female")) |> 
  mutate(age_group_simple = case_when(
    TEAGE >=15 & TEAGE <=40 ~ "15-40",
    TRUE ~ "other")) |> 
  group_by(age_group_simple, gender) |> 
  summarize(mean_time_alone11 = mean(TRTALONE))

mean_alone_time21 <- alone_time |> mutate(
  gender = case_when(TESEX == 1 ~ "male", TRUE ~ "female")) |> 
  mutate(age_group_simple = case_when(
    TEAGE >=15 & TEAGE <=40 ~ "15-40",
    TRUE ~ "other")) |> 
  group_by(age_group_simple, gender) |> 
  summarize(mean_time_alone21 = mean(TRTALONE))

change_in_alone_time1121 <- left_join(
  mean_alone_time11, 
  mean_alone_time21, 
  by=c("age_group_simple", "gender")) |> 
  mutate(percent_change = ((mean_time_alone21 - mean_time_alone11) / mean_time_alone11) *100)

## Count number of young people who spend more than 8 hours TRTAlONE (480 minutes)
alone_time21 <- alone_time21 |> mutate(
  gender = case_when(TESEX == 1 ~ "male", TRUE ~ "female")) |>
  mutate(age_group_simple = case_when(
    TEAGE >=15 & TEAGE <=40 ~ "15-40",
    TRUE ~ "other")) |> 
mutate(eight_hours=ifelse(TRTALONE>=480, "More", "Less"))

  alone_time21 |> filter(age_group_simple == "15-40") |> 
    group_by(gender, eight_hours) |> summarise(count = n()) |> 
    mutate(percent = count/sum(count)*100)
  
  ## Count number of older people who spend more than 8 hours TRTAlONE (480 minutes)
  alone_time21 <- alone_time21 |> mutate(
    gender = case_when(TESEX == 1 ~ "male", TRUE ~ "female")) |>
    mutate(age_group_simple = case_when(
      TEAGE >=65 ~ "65+",
      TRUE ~ "other")) |> 
    mutate(eight_hours=ifelse(TRTALONE>=480, "More", "Less"))
  
  alone_time21 |> filter(age_group_simple == "65+") |> 
    group_by(gender, eight_hours) |> summarise(count = n()) |> 
    mutate(percent = count/sum(count)*100)
