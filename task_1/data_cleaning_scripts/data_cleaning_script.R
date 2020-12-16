# Dirty data project
# Task 1 - Decathlon Data
# This is an R script to clean the raw input data
-------------------------------------------------

# 1 Reading in the raw data

  ## Installing "here" package in the terminal: install.packages("here")
  ## Installing "readr" package in the terminal: install.packages("readr")
  ## uploading the libraries

library(here)
library(readr)
library(janitor)
library(tidyverse)

  ## Test where the top level of the project directory is

here::here()

  ## Reading in the data, changing the column header names to snake_case style

decathlon_raw_data <- read_rds(here("raw_data/decathlon.rds")) %>% clean_names()


# 2 Cleaning the data

  ## converting row names into first column

decathlon_raw_data <- tibble::rownames_to_column(decathlon_raw_data, "name")

  ## Checking there is no missing values in the data frame - all good!

decathlon_raw_data %>% 
  +   summarise(across(.fns = ~sum(is.na(.x))))

glimpse(decathlon_raw_data)

  # We see results from two decathlon competitions - Decastar and Olympic Games
  # Some of the participants competed in both competitions

  # Converting the first letter of competitor name to Uppercase and the rest of the letters to lower case (eq. 'Abcdef..')

decathlon_clean <- decathlon_raw_data %>% 
  mutate(name = str_to_title(name)) 

View(decathlon_clean)


# Saving the cleaned data as a csv file

write_csv(decathlon_clean, path = "clean_data/decathlon_clean.csv")
 



