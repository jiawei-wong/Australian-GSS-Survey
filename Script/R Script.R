### Preamble ###
# Purpose: Clean and Set-up the General Social Survey: Summary Results, Australia data
# Author: Min Chang, Julia Wong
# Date: March 4 2022
# Contact: hyemin.chang@mail.utoronto.ca, jiawei.wong@mail.utoronto.ca
# Pre-requisites: None

### Workspace Set-up ###
install.packages("knitr")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("reshape2")
install.packages("ggplot2")
install.packages("kableExtra")
install.packages("bibtex")
install.packages("bookdown")

tinytex::install_tinytex()

library(knitr)
library(tidyverse)
library(dplyr)
library(reshape2)
library(ggplot2)
library(kableExtra)
library(bibtex)
library(bookdown)

### Saving the dataset ###
write_csv(x = GSS_Table1,
          file = "raw_data.csv")

raw_data <- read_csv("raw_data.csv",
                     show_col_types = FALSE)

### Grabbing specific rows ###
cleaned_australian_data <- raw_data |>
  slice(5, 6, 40, 41, 42, 43, 44, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 
        83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 109, 110, 111, 112, 113, 129)

### Creating a table with the number of survey collected in 2019 and 2020 ###
total_reports_2019 <- cleaned_australian_data |>
  slice(1, 2, 36) |>
  select(-1, -2, -3, -4, -5, -6, -7, -8) |>
  mutate(year = 2019,
         males = 9807,
         females = 10164.1,
         total = 20010.8) |>
  slice(1)

total_reports_2020 <- cleaned_australian_data |>
  slice(1, 2, 36) |>
  select(-1, -2, -3, -4, -5) |>
  mutate(year = 2020,
         males = 9955.5,
         females = 10325.2,
         total = 20265.2) |>
  slice(1) |>
  select(-1, -2, -3)

total_reports <- rbind(total_reports_2019, total_reports_2020) |> #merging two dataframes
  ceiling() #round decimals to whole numbers

total_reports |>
  knitr::kable(caption = "Number of Australians that Contributed to the Survey in 2019 and 2020",
               col.names = c("Year", "Male", "Female", "Total"),
               booktabs = TRUE) |>
  kable_classic(full_width = T) |>
  kable_styling(latex_options = "HOLD_position")

### Creating a graph that compares the relationship between education and employment rate ###

education_employment <- cleaned_australian_data |>
  slice(1, 2, 18, 19, 20:29)

education_2019 <- education_employment |>
  select(1:4) |> #select 2019 data
  slice(-1:-11) |> #select education data
  rename(c("education_level" = "Australian Bureau of Statistics", "males" = "...2", "females" = "...3", "total" = "...4")) |>
  mutate(year = 2019,
         males = as.numeric(males),
         females = as.numeric(females),
         total = as.numeric(total)) |>
  mutate_if(is.numeric, ~round(., 0)) #to round up numeric columns

employment_2019 <- education_employment |>
  select(1:4) |>
  slice(5:8) |>
  rename(c("labour_force_status" = "Australian Bureau of Statistics", "males" = "...2", "females" = "...3", "total" = "...4")) |>
  mutate(year = 2019,
         males = as.numeric(males),
         females = as.numeric(females),
         total = as.numeric(total)) |>
  mutate_if(is.numeric, ~round(., 0)) #to round up numeric columns

education_2020 <- education_employment |>
  select(1, 6:8) |>
  slice(12:14) |>
  rename(c("education_level" = "Australian Bureau of Statistics", "males" = "...6", "females" = "...7", "total" = "...8")) |>
  mutate(year = 2020,
         males = as.numeric(males),
         females = as.numeric(females),
         total = as.numeric(total)) |>
  mutate_if(is.numeric, ~round(., 0)) #to round up numeric columns

employment_2020 <- education_employment |>
  select(1, 6:8) |>
  slice(5:8) |>
  rename(c("labour_force_status" = "Australian Bureau of Statistics", "males" = "...6", "females" = "...7", "total" = "...8")) |>
  mutate(year = 2020,
         males = as.numeric(males),
         females = as.numeric(females),
         total = as.numeric(total)) |>
  mutate_if(is.numeric, ~round(., 0)) #to round up numeric columns

education_level <- rbind(education_2019, education_2020) |>
  select(-2, -3) |>
  filter(education_level == "Bachelor Degree or above")

employment_status <- rbind(employment_2019, employment_2020) |>
  select(-2, -3) |>
  slice(-4, -8) |>
  group_by(year)

employment_status |>
  ggplot(aes(year, total, fill = labour_force_status), las=1) +
  geom_bar(stat = "identity") +
  ylim(0, 15000) +
  geom_text(aes(label=total), size = 4, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer() +
  scale_x_continuous(breaks = employment_status$year) +
  labs(x = "Year", y = "Number of People", fill = "Employment Type",
       title = "Number of Australians in the Labour Force by Employment Type in 2019 and 2020") +
  theme_light()

education_level |>
  ggplot(aes(year, total)) +
  geom_line() +
  ylim(0, 8000) +
  scale_x_continuous(breaks = education_level$year) +
  geom_point() +
  labs(x = "Year", y = "Number of Degrees Earned (Bachelor's and Above)",
       title = "Increase of Bachelor's and Above Degreed Earned between 2019 and 2020") +
  theme_light()

### Creating graphs and tables regarding health ###
#Variables I need: Year, Count (Total), Smoker?
health_social <- cleaned_australian_data |>
  slice(1:14)

#Smoking status
smoker_status_2019 <- health_social |>
  select(1:4) |>
  slice(13, 14) |>
  rename(c("smoker_status" = "Australian Bureau of Statistics", "males" = "...2", "females" = "...3", "total" = "...4")) |>
  mutate(year = 2019,
         males = as.numeric(males),
         females = as.numeric(females),
         total = as.numeric(total)) |>
  mutate_if(is.numeric, ~round(., 0)) #to round up numeric columns

smoker_status_2020 <- health_social |>
  select(1, 6:8) |>
  slice(13, 14) |>
  rename(c("smoker_status" = "Australian Bureau of Statistics", "males" = "...6", "females" = "...7", "total" = "...8")) |>
  mutate(year = 2020,
         males = as.numeric(males),
         females = as.numeric(females),
         total = as.numeric(total)) |>
  mutate_if(is.numeric, ~round(., 0)) #to round up numeric columns

smoker_status <- rbind(smoker_status_2019, smoker_status_2020) |>
  select(-2, -3)

smoker_status |>
  ggplot(aes(x = year, y = total, fill = smoker_status)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_brewer() +
  scale_x_continuous(breaks = smoker_status$year) +
  labs(x = "Year", y = "Number of People", fill = "Smoking Status",
       title = "Number of Smokers and Non-Smokers in 2019 and 2020") +
  theme_light()

#Health status
health_status_2019 <- health_social |>
  slice(8:10) |>
  select(1:4) |>
  rename(c("health_status" = "Australian Bureau of Statistics", "males" = "...2", "females" = "...3", "total" = "...4")) |>
  mutate(year = 2019,
         males = as.numeric(males),
         females = as.numeric(females),
         total = as.numeric(total)) |>
  mutate_if(is.numeric, ~round(., 0))

health_status_2020 <- health_social |>
  slice(8:10) |>
  select(1, 6:8) |>
  rename(c("health_status" = "Australian Bureau of Statistics", "males" = "...6", "females" = "...7", "total" = "...8")) |>
  mutate(year = 2020,
         males = as.numeric(males),
         females = as.numeric(females),
         total = as.numeric(total)) |>
  mutate_if(is.numeric, ~round(., 0))

health_status <- rbind(health_status_2019, health_status_2020) |>
  select(-2, -3)

health_status |>
  ggplot(aes(x = health_status, y = total, fill = health_status)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = total), vjust = -0.2) + #include labels of values
  facet_wrap(~year) +
  scale_fill_brewer() +
  theme_light() +
  labs(x = "Health Status", y = "Number of People", fill = "Health Status",
       title = "Health Status of Australians in 2019 and 2020")

#Social life
social_life_2019 <- health_social|>
  slice(4) |>
  select(1:4) |>
  rename(c("social_life" = "Australian Bureau of Statistics", "males" = "...2", "females" = "...3", "total" = "...4")) |>
  mutate(year = 2019,
         males = as.numeric(males),
         females = as.numeric(females),
         total = as.numeric(total)) |>
  mutate_if(is.numeric, ~round(., 0))

social_life_2020 <- health_social|>
  slice(4) |>
  select(1, 6:8) |>
  rename(c("social_life" = "Australian Bureau of Statistics", "males" = "...6", "females" = "...7", "total" = "...8")) |>
  mutate(year = 2020,
         males = as.numeric(males),
         females = as.numeric(females),
         total = as.numeric(total)) |>
  mutate_if(is.numeric, ~round(., 0))

social_life <- rbind(social_life_2019, social_life_2020) |>
  select(-2, -3) |>
  group_by(year)

social_life |>
  ggplot(aes(x = year, y = total)) +
  geom_bar(position ="dodge", stat = "identity", fill = "lightblue") +
  geom_text(aes(label = total), hjust = 1.5, size = 8) +
  scale_x_continuous(breaks = social_life$year) +
  theme_light() +
  labs(x = "Year", y = "Number of People Involved in Social Groups",
       title = "Decrease of Involvement in Social Groups in 2020") +
  coord_flip()