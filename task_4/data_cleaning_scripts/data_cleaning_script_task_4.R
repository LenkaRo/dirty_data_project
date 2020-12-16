# Dirty data project
# Task 4 - Halloween Candy
# This is an R script to clean the raw input data
-------------------------------------------------
  
  # 1 Reading in the raw data
  
  ## installing packages in the terminal: e.g. install.packages("here")
  ## uploading the libraries
  
  library(here)
library(readr)
library(readxl)
library(janitor)
library(tidyverse)

## Test where the top level of the project directory is

here::here()

## Reading in the data, changing the column header names to snake_case style

bbc_2015 <- read_excel("raw_data/boing-boing-candy-2015.xlsx") %>% clean_names()
bbc_2016 <- read_excel("raw_data/boing-boing-candy-2016.xlsx") %>% clean_names()
bbc_2017 <- read_excel("raw_data/boing-boing-candy-2017.xlsx") %>% clean_names()

# Cleaning the data

## combining the three data sets together

### they have all different amount of rows and columns

## column names

length(bbc_2015)
length(bbc_2016)
length(bbc_2017)

# renaming candy heading names to match across the three data frames

## to see the differences in naming:

column_names <- tibble(names(bbc_2015),
                       c(names(bbc_2016), NA),
                       c(names(bbc_2017), NA, NA, NA, NA))

## creating vector 'year' so the observations can be identified by the year they came from 

year_vector <- c(rep(2015, times = nrow(bbc_2015)))

bbc_2015 <- bbc_2015 %>%
  rename(age = how_old_are_you,
         going_out_trick_or_treating_yourself = are_you_going_actually_going_trick_or_treating_yourself,
         bonkers_the_candy = bonkers,
         hersheys_kisses = hershey_s_kissables,
         hersheys_milk_chocolate = hershey_s_milk_chocolate,
         licorice_yes_black = licorice, 
         sweetums_a_friend_to_diabetes = sweetums,
         "100_grand_bar" = x100_grand_bar) %>% 
  mutate(timestamp = year_vector) %>% rename(year = timestamp)


year_vector <- c(rep(2016, times = nrow(bbc_2016)))

bbc_2016 <- bbc_2016 %>%
  rename(age = how_old_are_you,
         going_out_trick_or_treating_yourself = are_you_going_actually_going_trick_or_treating_yourself,
         country = which_country_do_you_live_in,
         state = which_state_province_county_do_you_live_in,
         gender = your_gender,
         box_o_raisins = boxo_raisins,
         hersheys_milk_chocolate = hershey_s_milk_chocolate,
         "100_grand_bar" = x100_grand_bar) %>%  
  mutate(timestamp = year_vector) %>% rename(year = timestamp)


# getting rid of 'q1_', 'q2_', ... prefix in column headings

colnames(bbc_2017) <- gsub(pattern = "q[0-9]*_", '', colnames(bbc_2017))
year_vector <- c(rep(2017, times = nrow(bbc_2017)))

bbc_2017 <- bbc_2017 %>%
  rename(state = state_province_county_etc,
         anonymous_brown_globs_that_come_in_black_and_orange_wrappers = anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes, 
         box_o_raisins = boxo_raisins, 
         hersheys_milk_chocolate = hershey_s_milk_chocolate) %>% 
  mutate(year = year_vector)


# joining the three data frames by rows

join_all <- bind_rows(bbc_2015,
                      bbc_2016,
                      bbc_2017)

## subsetting candy columns by keeping only those columns containing JOY, DESPAIR or MEH

candies <-  join_all %>%
  mutate_if(is.character, ~replace(., is.na(.), "MISSING")) %>% 
  mutate_all(~str_detect(. , "JOY|DESPAIR|MISSING|MEH")) %>%
  summarise_all(~sum(., na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = 'col_names', values_to = 'values') %>%
  filter(values == nrow(join_all)) %>%
  select(col_names) %>%
  pull()

# removing columns that do not sound candy to me

candies[!candies %in% c("abstained_from_m_ming", 
                        "bonkers_the_board_game", 
                        "cash_or_other_forms_of_legal_tender", 
                        "chardonnay", 
                        "dental_paraphenalia", 
                        "hugs_actual_physical_hugs", 
                        "generic_brand_acetaminophen", 
                        "person_of_interest_season_3_dvd_box_set_not_including_disc_4_with_hilarious_outtakes", 
                        "please_list_any_items_not_included_above_that_give_you_despair",
                        "real_housewives_of_orange_county_season_9_blue_ray", 
                        "vicodin")] 

join_all <- join_all %>% 
  select(year,
         age,
         gender,
         country,
         state,
         going_out_trick_or_treating_yourself,
         all_of(candies))

# cleaning the data points in columns age and country

## age

### age is data class character, also contains other then number values, eg 30's, enough, very etc.
### I am going to change it to integer and drop age outliers 

join_all <- join_all %>%
  mutate(age, age = as.integer(age)) %>% 
  mutate(
    age = case_when(
      age > 99 | age < 4 ~ "replace_with_NA",
      TRUE ~ as.character(age)
    )
  ) %>% 
  mutate(
    age = na_if(age, "replace_with_NA")
  ) %>% 
  mutate(age, age = as.integer(age))

### checking all the unique values, age range is now 4 to 99

# join_all$age %>% 
#   unique() %>% 
#   sort()


## country

### checking all distinct values in column country - so many! Here I would recommend to the survey authors to use a "choose from" type of questionnaire..

# join_all_candy$country %>% 
#   unique()

### renaming typos and gathering all the nonsense ones in value "others"

join_all <- join_all %>% 
  mutate(
    country = recode(country,
                     "usa" = "USA",
                     "US" = "USA",
                     "United States of America" = "USA",
                     "uSA" = "USA",
                     "united states" = "USA",
                     "Canada`" = "Canada",
                     "canada" = "Canada",
                     "United States" = "USA",
                     "us" = "USA",                                                                 
                     "france" = "France",                                                              
                     "USSA" = "USA",                                                                
                     "U.S.A." = "USA",                                                              
                     "A tropical island south of the equator" = "other",                            
                     "england" = "UK",                                                             
                     "uk" = "UK",                                                                  
                     "United Kingdom" = "UK",                                                     
                     "Neverland" = "other",    #!!!!!!!!!!!!!                                                      
                     "USA!" = "USA",                                                                
                     "this one" = "USA",
                     "USA (I think but it's an election year so who can really tell)" = "USA",     
                     "51.0" = "other",                                                                
                     "Usa" = "USA",                                                                 
                     "U.S." = "USA",                                                                
                     "Us" = "USA",                                                                  
                     "America" = "USA",                                                             
                     "Units States" = "USA",                                                        
                     "belgium" = "Belgium",                                                             
                     "croatia"= "Croatia",                                                           
                     "United states" = "USA",                                                    
                     "England" = "UK",                                                            
                     "USA USA USA" = "USA",                                                        
                     "the best one - usa" = "USA",                                                 
                     "USA! USA! USA!" = "USA",                                                     
                     "47.0" = "other",                                                             
                     "españa" = "Spain",
                     "u.s." = "USA",                                                              
                     "there isn't one for old men" = "other",   #!!!!!!!!!!!!!                                                       
                     "one of the best ones" = "other",                                               
                     "The Yoo Ess of Aaayyyyyy" = "USA",                                            
                     "United Kindom" = "UK",                                                            
                     "hungary" = "Hungary",                                                           
                     "united states of america" = "USA",                                                            
                     "Somewhere" = "other", 
                     "54.0" = "other",                                                             
                     "44.0" = "other",                                                             
                     "god's country"= "USA",                                                         
                     "USA!!!!!!" = "USA",
                     "EUA" = "other",                                                                
                     "USA! USA!" = "USA",
                     "45.0" = "other",                                                               
                     "sweden" = "Sweden",                                                              
                     "United Sates" = "USA",                                                       
                     "Sub-Canadian North America... 'Merica" = "USA",                              
                     "The Netherlands" = "Netherlands",
                     "Trumpistan" = "USA",     #!!!!!!!!!!!!!                                                         
                     "U.s." = "USA",
                     "Merica" = "USA",
                     "germany" = "Germany",
                     "See above" = "other",                                                          
                     "UNited States" = "USA",                                                      
                     "kenya" = "Kenya",                                                               
                     "30.0" = "other",
                     "The republic of Cascadia" = "Cascadia",                                           
                     "United Stetes" = "USA",                                                      
                     "america" = "USA",                                                            
                     "Not the USA or Canada" = "other",                                              
                     "USA USA USA USA" = "USA",                                                    
                     "United  States of America" = "USA",                                          
                     "netherlands" = "Netherlands",                                                         
                     "Denial" = "other",                                                             
                     "United State" = "USA",                                                       
                     "United staes" = "USA",
                     "u.s.a." = "USA",                                                            
                     "USAUSAUSA"= "USA",                                                           
                     "35" = "other",                                                                 
                     "finland" = "Finland",                                                            
                     "unhinged states"  = "USA",                                                   
                     "US of A"        = "USA",                                                     
                     "Unites States"   = "USA",                                                    
                     "The United States"  = "USA",                                                 
                     "North Carolina"    = "USA",                                                  
                     "Unied States"  = "USA",                                                      
                     "Europe" = "other",                                                             
                     "Earth" = "other",                                                              
                     "U S" = "USA",                                                                
                     "U.K." = "UK",
                     "The United States of America" = "USA",                                       
                     "unite states"  = "USA",                                                      
                     "46" = "other",                                                                 
                     "cascadia" = "Cascadia",                                                            
                     "insanity lately" = "other",                                                    
                     "USA? Hard to tell anymore.."     = "USA",                                    
                     "'merica"  = "USA",                                                           
                     "usas"  = "USA",                                                              
                     "Pittsburgh" = "USA",
                     "45"  = "other",                                                                
                     "32"  = "other",                                                                
                     "australia" = "Australia",                                                          
                     "A"  = "other",                                                                 
                     "Can"  = "Canada",                                                               
                     "Canae" = "Canada",                                                               
                     "New York" = "USA",                                                           
                     "California" = "USA",                                                         
                     "USa"  = "USA",                                                               
                     "South africa"  = "South Africa",                                                      
                     "I pretend to be from Canada, but I am really from the United States." = "USA",  #!!!!!!!!!!!!! 
                     "Uk" = "UK",                                                                 
                     "United Stated" = "USA",                                                      
                     "Ahem....Amerca" = "USA",                                                     
                     "UD" = "other",                                                                  
                     "New Jersey" = "USA",                                                         
                     "CANADA"    = "Canada",                                                          
                     "United ststes" = "USA",                                                      
                     "United Statss"    = "USA",                                                   
                     "endland"  = "UK",                                                       
                     "Atlantis" = "other",                                                           
                     "murrika"  = "USA",                                                           
                     "USAA" = "USA",                                                                
                     "Alaska" = "USA",
                     "united States" = "USA",                                                      
                     "soviet canuckistan" = "other",                                                 
                     "N. America" = "USA",                                                         
                     "hong kong"= "China",                                                           
                     "spain" = "Spain",                                                              
                     "Hong Kong" = "China",                                                          
                     "Narnia" = "other",      #!!!!!!!!!!!!!                                                
                     "u s a" = "USA",                                                              
                     "United Statea" = "USA",                                                      
                     "united ststes" = "USA",                                                      
                     "1"  = "other",                                                                 
                     "subscribe to dm4uz3 on youtube"  = "other",                                    
                     "United kingdom"  = "UK",                                                    
                     "USA USA USA!!!!" = "USA",                                                    
                     "I don't know anymore"  = "other",                                              
                     "Fear and Loathing" = "other",
                     "Scotland" = "UK",
                     "Korea" = "South Korea",
                     "Murica" = "USA"
    )
  )

### checking all the unique values country (sorted alphabetically),
### we have now 36 countries and 1 other category represented in the data frame

# join_all$country %>% 
#   unique() %>% 
#   sort()


# HURRAY! clean_data data frame for analysis
# just bringing the non candy columns forward and sorting the candies alphabeticaly 

candy_clean <- join_all[,order(colnames(join_all))] %>% 
  relocate(year, .before = 1) %>% 
  relocate(age, .after = 1) %>% 
  relocate(gender, .after = 2) %>% 
  relocate(country, .after = 3) %>% 
  relocate(state, .after = 4) %>% 
  relocate(going_out_trick_or_treating_yourself, .after = 5) 

View(candy_clean)

## double-checking there isn't any column of just missing values - OK! 

# nonNAs <- function(x) {
#   as.vector(apply(x, 2, function(x) length(which(!is.na(x)))))
#   }

# nonNAs(candy_clean)

# saving the new clean_data data frame as a csv file

write_csv(candy_clean, path = "clean_data/candy_clean.csv")
