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

### they have all different amount of rows and columns, 
### I will have to adjust this first by adding columns of NAs to create maximum combination of all three tables
### eg. only 2017 table has column id, I will create an id vector of nulls and cbind it to the data frame 2015 and 2016, at the right position
### this will ensure all three table structures match for the final bind_rows


## 2015 

### creating vectors of NAs
### adding these to match the table column structure and data type across the all three tables 
### id, gender, country and state_province is missing in 2015 data frame

id_vector_2015 <- rep(NA, 5630) %>% as.double()
gender_vector_2015 <- rep(NA, 5630) %>% as.character()
country_vector_2015 <- rep(NA, 5630) %>% as.character()
state_province_vector_2015 <- rep(NA, 5630) %>% as.character()

bbc_2015_id <- bbc_2015 %>% 
  add_column(id_vector_2015, .after = 0) %>% 
  add_column(gender_vector_2015, .after = 3) %>% 
  add_column(country_vector_2015, .after = 4) %>% 
  add_column(state_province_vector_2015, .after = 5)

### renaming column heading names

bbc_2015_id <- bbc_2015_id %>% 
  rename(id = id_vector_2015,
         age = how_old_are_you,
         gender = gender_vector_2015,
         country = country_vector_2015,
         state_province = state_province_vector_2015,
         going_out_trick_or_treating_yourself = are_you_going_actually_going_trick_or_treating_yourself)

### ordering column heading names alphabetically

bbc_2015_id_alphabetically <- bbc_2015_id[,order(colnames(bbc_2015_id))]

### bringing id, timestamp, ... going_out_trick_or_treating_yourself back to the front of data frame

bbc_2015_ordered <- bbc_2015_id_alphabetically %>% 
  relocate(id, .before = 1) %>% 
  relocate(timestamp, .after = 1) %>% 
  relocate(age, .after = 2) %>% 
  relocate(gender, .after = 3) %>% 
  relocate(country, .after = 4) %>% 
  relocate(state_province, .after = 5) %>% 
  relocate(going_out_trick_or_treating_yourself, .after = 6)

names(bbc_2015_ordered)



## 2016 

### creating vectors on NAs
### adding these to match the table column structure and data type across the all three tables
### id is missing in 2016 data frame

id_vector_2016 <- rep(NA, 1259) %>% as.double()

bbc_2016_id <- bbc_2016 %>% 
  add_column(id_vector_2016, .after = 0)

### renaming column heading names

bbc_2016_id <- bbc_2016_id %>% 
  rename(id = id_vector_2016,
       age = how_old_are_you,
       gender = your_gender,
       country = which_country_do_you_live_in,
       state_province = which_state_province_county_do_you_live_in,
       going_out_trick_or_treating_yourself = are_you_going_actually_going_trick_or_treating_yourself)

### ordering column heading names alphabetically

bbc_2016_id_alphabetically <- bbc_2016_id[,order(colnames(bbc_2016_id))]

### shuffling the columns to match the structure of 2015 data frame

bbc_2016_ordered <- bbc_2016_id_alphabetically %>% 
  relocate(id, .before = 1) %>% 
  relocate(timestamp, .after = 1) %>% 
  relocate(age, .after = 2) %>% 
  relocate(gender, .after = 3) %>% 
  relocate(country, .after = 4) %>% 
  relocate(state_province, .after = 5) %>% 
  relocate(going_out_trick_or_treating_yourself, .after = 6)

names(bbc_2016_ordered)



## 2017

### creating vectors on NAs
### adding these to match the table column structure and data type across the all three tables
### timestamp is missing in 2017 data frame

timestamp_vector_2017 <- rep(NA, 2460) %>% as.Date()

bbc_2017_id <- bbc_2017 %>% 
  add_column(timestamp_vector_2017, .after = 1)

### removing "q1_", "q2_", ... prefix in column headings

colnames(bbc_2017_id) <- gsub(pattern = "q[0-9]*_", '', colnames(bbc_2017_id))

### renaming column headings

bbc_2017_id <- bbc_2017_id %>% 
  rename(id = internal_id,
         timestamp = timestamp_vector_2017,
         state_province = state_province_county_etc,
         going_out_trick_or_treating_yourself = going_out,
         x100_grand_bar = "100_grand_bar")

### ordering column heading names alphabetically

bbc_2017_id_alphabetically <- bbc_2017_id[,order(colnames(bbc_2017_id))]

### shuffling the columns to match the structure of 2015 data frame

bbc_2017_ordered <- bbc_2017_id_alphabetically %>% 
  relocate(id, .before = 1) %>% 
  relocate(timestamp, .after = 1) %>% 
  relocate(age, .after = 2) %>% 
  relocate(gender, .after = 3) %>% 
  relocate(country, .after = 4) %>% 
  relocate(state_province, .after = 5) %>% 
  relocate(going_out_trick_or_treating_yourself, .after = 6)


## column 8 and onward are candies

### creating vectors from column headings

column_headings_2015 <- names(bbc_2015_ordered) 
column_headings_2016 <- names(bbc_2016_ordered)
column_headings_2017 <- names(bbc_2017_ordered)

### checking the length of vectors. The longest one sets the length the other two have to match
### to be able to create a table of size: length(longest vector) x 3

length(column_headings_2015) #128 <--- longest vector, fill the others with NAs to the same length
length(column_headings_2016) #124
length(column_headings_2017) #121

column_headings_2016_right_length <- c(column_headings_2016, NA, NA, NA, NA)
column_headings_2017_right_length <- c(column_headings_2017, NA, NA, NA, NA, NA, NA, NA)

### creating table to check matches in candy types

candies <- tibble(column_headings_2015,
                  column_headings_2016_right_length,
                  column_headings_2017_right_length)

### there were apparently questions in the survey not related to candy types, I am gonna drop those columns not containing string "JOY"
### (considering the amount of respondents, this condition should pick up all candy types, there should always be at least one respondent liking a particular candy)
### in this step, I am only considering candy columns (position 8 onward)
### id, timestamp, ... going_out_trick_or_treating_yourself remain untouched

#### 2015

bbc_2015_ordered_subset <- bbc_2015_ordered %>% 
  select(8:128)

bbc_2015_ordered_subset_candies_only <- bbc_2015_ordered_subset[, apply(bbc_2015_ordered_subset, 2, `%in%`, x = "JOY")]

#### 2016 

bbc_2016_ordered_subset <- bbc_2016_ordered %>% 
  select(8:124)

bbc_2016_ordered_subset_candies_only <- bbc_2016_ordered_subset[, apply(bbc_2016_ordered_subset, 2, `%in%`, x = "JOY")]

#### 2017

bbc_2017_ordered_subset <- bbc_2017_ordered %>% 
  select(8:121)

bbc_2017_ordered_subset_candies_only <- bbc_2017_ordered_subset[, apply(bbc_2017_ordered_subset, 2, `%in%`, x = "JOY")]



### "glueing" the data frames back together with first 8 columns

#### 2015

bbc_2015_sub_first_seven_columns <- bbc_2015_ordered %>% 
  select(1:7)

bbc_2015_candies <- cbind(bbc_2015_sub_first_seven_columns, bbc_2015_ordered_subset_candies_only)

#### 2016

bbc_2016_sub_first_seven_columns <- bbc_2016_ordered %>% 
  select(1:7)

bbc_2016_candies <- cbind(bbc_2016_sub_first_seven_columns, bbc_2016_ordered_subset_candies_only)

#### 2017

bbc_2017_sub_first_seven_columns <- bbc_2017_ordered %>% 
  select(1:7)

bbc_2017_candies <- cbind(bbc_2017_sub_first_seven_columns, bbc_2017_ordered_subset_candies_only)


### creating vectors from column headings

column_headings_2015_candies_only <- names(bbc_2015_candies) 
column_headings_2016_candies_only <- names(bbc_2016_candies) 
column_headings_2017_candies_only <- names(bbc_2017_candies) 


length(column_headings_2015_candies_only) # 103
length(column_headings_2016_candies_only) # 107
length(column_headings_2017_candies_only) # 110 <----

### again, checking the length of vectors. The longest one sets the length the other two have to match
### to be able to create a table of size: length(longest vector) x 3

column_headings_2015_candies_only_right_length <- c(column_headings_2015_candies_only, NA, NA, NA, NA, NA, NA, NA)
column_headings_2016_andies_only_right_length <- c(column_headings_2016_candies_only, NA, NA, NA)

### creating table to check matches in candy types

candies_only <- tibble(column_headings_2015_candies_only_right_length,
                       column_headings_2016_andies_only_right_length,
                       column_headings_2017_candies_only)
                       
dim(candies_only) # 110 x 3


#### as of now, there is allegedly 112 types of candies across the three data sets
#### when a particular type of candy is missing in one of the three tables, I create it and fill it with NAs
#### I created tibble candies_only to visually see the differences in the heading names across the three years
#### there is few little differences in candy naming across the years so adjusting these too 
#### (eg. "bonkers" in 2015 renamed to "bonkers_the_candy" to match naming in 2016 and 2017,
#### typo in "boxo_raisins", changed to "box_o_raisins" in 2016 and 2017,
#### "hershey_s_kissables" in 2015 renamed to "hersheys_kisses",
#### "sweetums" renames to "sweetums_a_friend_to_diabetes" 


abstained_from_m_ming_vector_2015 <- rep(NA, 5630) %>% as.character()
abstained_from_m_ming_vector_2016 <- rep(NA, 1259) %>% as.character()

blue_m_ms_vector_2015 <- rep(NA, 5630) %>% as.character()

bonkers_the_board_game_vector_2015 <- rep(NA, 5630) %>% as.character()

brach_products_not_including_candy_corn_vector_2016 <- rep(NA, 1259) %>% as.character()
brach_products_not_including_candy_corn_vector_2017 <- rep(NA, 2460) %>% as.character()

bubble_gum_vector_2016 <- rep(NA, 1259) %>% as.character()
bubble_gum_vector_2017 <- rep(NA, 2460) %>% as.character()

chardonnay_vector_2015 <- rep(NA, 5630) %>% as.character()

coffee_crisp_vector_2015 <- rep(NA, 5630) %>% as.character()

dark_chocolate_hershey_vector_2016 <- rep(NA, 1259) %>% as.character()
dark_chocolate_hershey_vector_2017 <- rep(NA, 2460) %>% as.character()

dove_bars_vector_2015 <- rep(NA, 5630) %>% as.character()

green_party_m_ms_vector_2015 <- rep(NA, 5630) %>% as.character()
green_party_m_ms_vector_2016 <- rep(NA, 1259) %>% as.character()

hersheys_dark_chocolate_vector_2015 <- rep(NA, 5630) %>% as.character()

independent_m_ms_vector_2015 <- rep(NA, 5630) %>% as.character()
independent_m_ms_vector_2016 <- rep(NA, 1259) %>% as.character()

lapel_pins_vector_vector_2016 <- rep(NA, 1259) %>% as.character()
lapel_pins_vector_vector_2017 <- rep(NA, 2460) %>% as.character()

mary_janes_vector_2017 <- rep(NA, 2460) %>% as.character()

mike_and_ike_vector_2015 <- rep(NA, 5630) %>% as.character()

mint_leaves_vector_2016 <- rep(NA, 1259) %>% as.character()
mint_leaves_vector_2017 <- rep(NA, 2460) %>% as.character()

mint_m_ms_vector_2016 <- rep(NA, 1259) %>% as.character()
mint_m_ms_vector_2017 <- rep(NA, 2460) %>% as.character()

mr_goodbar_vector_2015 <- rep(NA, 5630) %>% as.character()

peanut_butter_bars_vector_2016 <- rep(NA, 1259) %>% as.character()
peanut_butter_bars_vector_2017 <- rep(NA, 2460) %>% as.character()

peanut_butter_jars_vector_2016 <- rep(NA, 1259) %>% as.character()
peanut_butter_jars_vector_2017 <- rep(NA, 2460) %>% as.character()

peeps_vector_2015 <- rep(NA, 5630) %>% as.character()

peterson_brand_sidewalk_chalk_vector_2016 <- rep(NA, 1259) %>% as.character()
peterson_brand_sidewalk_chalk_vector_2017 <- rep(NA, 2460) %>% as.character()

person_of_interest_season_3_dvd_box_set_not_including_disc_4_with_hilarious_outtakes_vector_2015 <- rep(NA, 5630) %>% as.character()
person_of_interest_season_3_dvd_box_set_not_including_disc_4_with_hilarious_outtakes_vector_2017 <- rep(NA, 2460) %>% as.character()

please_list_any_items_not_included_above_that_give_you_despair_vector_2016 <- rep(NA, 1259) %>% as.character()
please_list_any_items_not_included_above_that_give_you_despair_vector_2017 <- rep(NA, 2460) %>% as.character()

real_housewives_of_orange_county_season_9_blue_ray_vector_2015 <- rep(NA, 5630) %>% as.character()
real_housewives_of_orange_county_season_9_blue_ray_vector_2016 <- rep(NA, 1259) %>% as.character()

red_m_ms_vector_2015 <- rep(NA, 5630) %>% as.character()

reeses_pieces_vector_2015 <- rep(NA, 5630) %>% as.character()

ribbon_candy_vector_2016 <- rep(NA, 1259) %>% as.character()
ribbon_candy_vector_2017 <- rep(NA, 2460) %>% as.character()

runts_vector_2016 <- rep(NA, 1259) %>% as.character()
runts_vector_2017 <- rep(NA, 2460) %>% as.character()

sea_salt_flavored_stuff_probably_chocolate_since_this_is_the_it_flavor_of_the_year_vector_2016 <- rep(NA, 1259) %>% as.character()
sea_salt_flavored_stuff_probably_chocolate_since_this_is_the_it_flavor_of_the_year_vector_2017 <- rep(NA, 2460) %>% as.character()

sandwich_sized_bags_filled_with_boo_berry_crunch_vector_2015 <- rep(NA, 5630) %>% as.character()
sandwich_sized_bags_filled_with_boo_berry_crunch_vector_2016 <- rep(NA, 1259) %>% as.character()

sourpatch_kids_i_e_abominations_of_nature_vector_2015 <- rep(NA, 5630) %>% as.character()

sweet_tarts_vector_2015 <- rep(NA, 5630) %>% as.character()

take_5_vector_2015 <- rep(NA, 5630) %>% as.character()
take_5_vector_2016 <- rep(NA, 1259) %>% as.character()

third_party_m_ms_vector_2015 <- rep(NA, 5630) %>% as.character()
third_party_m_ms_vector_2017 <- rep(NA, 2460) %>% as.character()

tic_tacs_vector_2015 <- rep(NA, 5630) %>% as.character()

whatchamacallit_bars_vector_2015 <- rep(NA, 5630) %>% as.character()


bbc_2015_final <- bbc_2015_candies %>% 
  add_column(abstained_from_m_ming_vector_2015, .after = 7) %>% rename(abstained_from_m_ming = abstained_from_m_ming_vector_2015) %>% 
  add_column(blue_m_ms_vector_2015, .after = "black_jacks") %>% rename(blue_m_ms = blue_m_ms_vector_2015) %>% 
  add_column(bonkers_the_board_game_vector_2015, .after = "blue_m_ms") %>% rename(bonkers_the_board_game = bonkers_the_board_game_vector_2015) %>% 
  rename(bonkers_the_candy = bonkers) %>% 
  add_column(chardonnay_vector_2015, .after = "cash_or_other_forms_of_legal_tender") %>% rename(chardonnay = chardonnay_vector_2015) %>% 
  add_column(coffee_crisp_vector_2015, .after = "chiclets") %>% rename(coffee_crisp = coffee_crisp_vector_2015) %>% 
  add_column(dove_bars_vector_2015, .after = "dots") %>% rename(dove_bars = dove_bars_vector_2015) %>% 
  add_column(green_party_m_ms_vector_2015, .after = "good_n_plenty") %>% rename(green_party_m_ms = green_party_m_ms_vector_2015) %>% 
  rename(hersheys_kisses = hershey_s_kissables) %>% 
  rename(hersheys_milk_chocolate = hershey_s_milk_chocolate) %>% 
  add_column(hersheys_dark_chocolate_vector_2015, .after = "hersheys_milk_chocolate") %>% rename(hersheys_dark_chocolate = hersheys_dark_chocolate_vector_2015) %>%
  add_column(independent_m_ms_vector_2015, .after = "hugs_actual_physical_hugs") %>% rename(independent_m_ms = independent_m_ms_vector_2015) %>% 
  rename(licorice_yes_black = licorice) %>% 
  add_column(mike_and_ike_vector_2015, .after = "maynards") %>% rename(mike_and_ike = mike_and_ike_vector_2015) %>% 
  add_column(mr_goodbar_vector_2015, .after = "mint_m_ms") %>% rename(mr_goodbar = mr_goodbar_vector_2015) %>% 
  add_column(peeps_vector_2015, .after = "peanut_m_m_s") %>% rename(peeps = peeps_vector_2015) %>% 
  add_column(person_of_interest_season_3_dvd_box_set_not_including_disc_4_with_hilarious_outtakes_vector_2015, .after = "peterson_brand_sidewalk_chalk") %>% rename(person_of_interest_season_3_dvd_box_set_not_including_disc_4_with_hilarious_outtakes = person_of_interest_season_3_dvd_box_set_not_including_disc_4_with_hilarious_outtakes_vector_2015) %>% 
  add_column(real_housewives_of_orange_county_season_9_blue_ray_vector_2015, .after = "please_list_any_items_not_included_above_that_give_you_despair") %>% rename(real_housewives_of_orange_county_season_9_blue_ray = real_housewives_of_orange_county_season_9_blue_ray_vector_2015) %>% 
  add_column(red_m_ms_vector_2015, .after = "real_housewives_of_orange_county_season_9_blue_ray") %>% rename(red_m_ms = red_m_ms_vector_2015) %>% 
  add_column(reeses_pieces_vector_2015, .after = "reese_s_peanut_butter_cups") %>% rename(reeses_pieces = reeses_pieces_vector_2015) %>% 
  add_column(sandwich_sized_bags_filled_with_boo_berry_crunch_vector_2015, .after = "sea_salt_flavored_stuff_probably_chocolate_since_this_is_the_it_flavor_of_the_year") %>% rename(sandwich_sized_bags_filled_with_boo_berry_crunch = sandwich_sized_bags_filled_with_boo_berry_crunch_vector_2015) %>% 
  add_column(sourpatch_kids_i_e_abominations_of_nature_vector_2015, .after = "snickers") %>% rename(sourpatch_kids_i_e_abominations_of_nature = sourpatch_kids_i_e_abominations_of_nature_vector_2015) %>% 
  add_column(sweet_tarts_vector_2015, .after = "swedish_fish") %>% rename(sweet_tarts = sweet_tarts_vector_2015) %>% 
  rename(sweetums_a_friend_to_diabetes = sweetums) %>% 
  add_column(take_5_vector_2015, .after = "sweetums_a_friend_to_diabetes") %>% rename(take_5 = take_5_vector_2015) %>% 
  add_column(third_party_m_ms_vector_2015, .after = "take_5") %>% rename(third_party_m_ms = third_party_m_ms_vector_2015) %>% 
  add_column(tic_tacs_vector_2015, .after = "three_musketeers") %>% rename(tic_tacs = tic_tacs_vector_2015) %>% 
  add_column(whatchamacallit_bars_vector_2015, .after = "vicodin") %>% rename(whatchamacallit_bars = whatchamacallit_bars_vector_2015)
  
# !!!!! swap columns hersheys_kisses and hersheys_milk_chocolate !!!!
# !!!!! swap columns licorice_yes_black and licorice_not_black !!!!
  

bbc_2016_final <- bbc_2016_candies %>% 
  add_column(abstained_from_m_ming_vector_2016, .after = 7) %>% rename(abstained_from_m_ming = abstained_from_m_ming_vector_2016) %>% 
  rename(box_o_raisins = boxo_raisins) %>% 
  add_column(brach_products_not_including_candy_corn_vector_2016, .after = "box_o_raisins") %>% rename(brach_products_not_including_candy_corn = brach_products_not_including_candy_corn_vector_2016) %>% 
  add_column(bubble_gum_vector_2016, .after = "broken_glow_stick") %>% rename(bubble_gum = bubble_gum_vector_2016) %>% 
  add_column(dark_chocolate_hershey_vector_2016, .after = "creepy_religious_comics_chick_tracts") %>% rename(dark_chocolate_hershey = dark_chocolate_hershey_vector_2016) %>% 
  add_column(green_party_m_ms_vector_2016, .after = "good_n_plenty") %>% rename(green_party_m_ms = green_party_m_ms_vector_2016) %>% 
  rename(hersheys_milk_chocolate = hershey_s_milk_chocolate) %>% 
  add_column(independent_m_ms_vector_2016, .after = "hugs_actual_physical_hugs") %>% rename(independent_m_ms = independent_m_ms_vector_2016) %>% 
  add_column(lapel_pins_vector_vector_2016, .after = "laffy_taffy") %>% rename(lapel_pins = lapel_pins_vector_vector_2016) %>% 
  add_column(mint_leaves_vector_2016, .after = "mint_kisses") %>% rename(mint_leaves = mint_leaves_vector_2016) %>% 
  add_column(mint_m_ms_vector_2016, .after = "mint_leaves") %>% rename(mint_m_ms = mint_m_ms_vector_2016) %>% 
  add_column(peanut_butter_bars_vector_2016, .after = "nown_laters") %>% rename(peanut_butter_bars = peanut_butter_bars_vector_2016) %>% 
  add_column(peanut_butter_jars_vector_2016, .after = "peanut_butter_bars") %>% rename(peanut_butter_jars = peanut_butter_jars_vector_2016) %>% 
  add_column(peterson_brand_sidewalk_chalk_vector_2016, .after = "pencils") %>% rename(peterson_brand_sidewalk_chalk = peterson_brand_sidewalk_chalk_vector_2016) %>% 
  add_column(please_list_any_items_not_included_above_that_give_you_despair_vector_2016, .after = "pixy_stix") %>% rename(please_list_any_items_not_included_above_that_give_you_despair = please_list_any_items_not_included_above_that_give_you_despair_vector_2016) %>% 
  add_column(real_housewives_of_orange_county_season_9_blue_ray_vector_2016, .after = "please_list_any_items_not_included_above_that_give_you_despair") %>% rename(real_housewives_of_orange_county_season_9_blue_ray = real_housewives_of_orange_county_season_9_blue_ray_vector_2016) %>% 
  add_column(ribbon_candy_vector_2016, .after = "regular_m_ms") %>% rename(ribbon_candy = ribbon_candy_vector_2016) %>% 
  add_column(runts_vector_2016, .after = "rolos") %>% rename(runts = runts_vector_2016) %>% 
  add_column(sea_salt_flavored_stuff_probably_chocolate_since_this_is_the_it_flavor_of_the_year_vector_2016, .after = "runts") %>% rename(sea_salt_flavored_stuff_probably_chocolate_since_this_is_the_it_flavor_of_the_year = sea_salt_flavored_stuff_probably_chocolate_since_this_is_the_it_flavor_of_the_year_vector_2016) %>% 
  add_column(sandwich_sized_bags_filled_with_boo_berry_crunch_vector_2016, .after = "sea_salt_flavored_stuff_probably_chocolate_since_this_is_the_it_flavor_of_the_year") %>% rename(sandwich_sized_bags_filled_with_boo_berry_crunch = sandwich_sized_bags_filled_with_boo_berry_crunch_vector_2016) %>% 
  add_column(take_5_vector_2016, .after = "sweetums_a_friend_to_diabetes") %>% rename(take_5 = take_5_vector_2016) 
  
  
bbc_2017_final <- bbc_2017_candies %>% 
  rename(anonymous_brown_globs_that_come_in_black_and_orange_wrappers = anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes) %>% 
  rename(box_o_raisins = boxo_raisins) %>% 
  add_column(brach_products_not_including_candy_corn_vector_2017, .after = "box_o_raisins") %>% rename(brach_products_not_including_candy_corn = brach_products_not_including_candy_corn_vector_2017) %>% 
  add_column(bubble_gum_vector_2017, .after = "broken_glow_stick") %>% rename(bubble_gum = bubble_gum_vector_2017) %>% 
  add_column(dark_chocolate_hershey_vector_2017, .after = "creepy_religious_comics_chick_tracts") %>% rename(dark_chocolate_hershey = dark_chocolate_hershey_vector_2017) %>% 
  rename(hersheys_milk_chocolate = hershey_s_milk_chocolate) %>% 
  add_column(lapel_pins_vector_vector_2017, .after = "laffy_taffy") %>% rename(lapel_pins = lapel_pins_vector_vector_2017) %>% 
  add_column(mary_janes_vector_2017, .after = "mars") %>% rename(mary_janes = mary_janes_vector_2017) %>% 
  add_column(mint_leaves_vector_2017, .after = "mint_kisses") %>% rename(mint_leaves = mint_leaves_vector_2017) %>% 
  add_column(mint_m_ms_vector_2017, .after = "mint_leaves") %>% rename(mint_m_ms = mint_m_ms_vector_2017) %>% 
  add_column(peanut_butter_bars_vector_2017, .after = "nown_laters") %>% rename(peanut_butter_bars = peanut_butter_bars_vector_2017) %>% 
  add_column(peanut_butter_jars_vector_2017, .after = "peanut_butter_bars") %>% rename(peanut_butter_jars = peanut_butter_jars_vector_2017) %>% 
  add_column(peterson_brand_sidewalk_chalk_vector_2017, .after = "pencils") %>% rename(peterson_brand_sidewalk_chalk = peterson_brand_sidewalk_chalk_vector_2017) %>% 
  add_column(person_of_interest_season_3_dvd_box_set_not_including_disc_4_with_hilarious_outtakes_vector_2017, .after = "peterson_brand_sidewalk_chalk") %>% rename(person_of_interest_season_3_dvd_box_set_not_including_disc_4_with_hilarious_outtakes = person_of_interest_season_3_dvd_box_set_not_including_disc_4_with_hilarious_outtakes_vector_2017) %>% 
  add_column(please_list_any_items_not_included_above_that_give_you_despair_vector_2017, .after = "pixy_stix") %>% rename(please_list_any_items_not_included_above_that_give_you_despair = please_list_any_items_not_included_above_that_give_you_despair_vector_2017) %>% 
  add_column(ribbon_candy_vector_2017, .after = "regular_m_ms") %>% rename(ribbon_candy = ribbon_candy_vector_2017) %>% 
  add_column(runts_vector_2017, .after = "rolos") %>% rename(runts = runts_vector_2017) %>% 
  add_column(sea_salt_flavored_stuff_probably_chocolate_since_this_is_the_it_flavor_of_the_year_vector_2017, .after = "runts") %>% rename(sea_salt_flavored_stuff_probably_chocolate_since_this_is_the_it_flavor_of_the_year = sea_salt_flavored_stuff_probably_chocolate_since_this_is_the_it_flavor_of_the_year_vector_2017) %>% 
  add_column(third_party_m_ms_vector_2017, .after = "take_5") %>% rename(third_party_m_ms = third_party_m_ms_vector_2017) 
  

##### due to sorting the column heading names alphabetically first and then, in the step above, finding and fixing typos in column heading names
##### I am now going to order alphabetically all three data frame again to get exactly the same column heading names across them

###### ordering column heading names alphabetically 

bbc_2015_final_alphabetically <- bbc_2015_final[,order(colnames(bbc_2015_final))]
bbc_2016_final_alphabetically <- bbc_2016_final[,order(colnames(bbc_2016_final))]
bbc_2017_final_alphabetically <- bbc_2017_final[,order(colnames(bbc_2017_final))]


###### and calling the id, timestamp, ... going_out_trick_or_treating_yourself columns back to the front again

bbc_2015_final_alphabetically_with_first_seven_columns_back <- bbc_2015_final_alphabetically %>% 
  relocate(id, .before = 1) %>% 
  relocate(timestamp, .after = 1) %>% 
  relocate(age, .after = 2) %>% 
  relocate(gender, .after = 3) %>% 
  relocate(country, .after = 4) %>% 
  relocate(state_province, .after = 5) %>% 
  relocate(going_out_trick_or_treating_yourself, .after = 6)


bbc_2016_final_alphabetically_with_first_seven_columns_back <- bbc_2016_final_alphabetically %>% 
  relocate(id, .before = 1) %>% 
  relocate(timestamp, .after = 1) %>% 
  relocate(age, .after = 2) %>% 
  relocate(gender, .after = 3) %>% 
  relocate(country, .after = 4) %>% 
  relocate(state_province, .after = 5) %>% 
  relocate(going_out_trick_or_treating_yourself, .after = 6)


bbc_2017_final_alphabetically_with_first_seven_columns_back <- bbc_2017_final_alphabetically %>% 
  relocate(id, .before = 1) %>% 
  relocate(timestamp, .after = 1) %>% 
  relocate(age, .after = 2) %>% 
  relocate(gender, .after = 3) %>% 
  relocate(country, .after = 4) %>% 
  relocate(state_province, .after = 5) %>% 
  relocate(going_out_trick_or_treating_yourself, .after = 6)



### creating vectors from column headings

column_headings_2015_final_alphabetically_with_first_seven_columns_back <- names(bbc_2015_final_alphabetically_with_first_seven_columns_back) 
column_headings_2016_final_alphabetically_with_first_seven_columns_back <- names(bbc_2016_final_alphabetically_with_first_seven_columns_back) 
column_headings_2017_final_alphabetically_with_first_seven_columns_back <- names(bbc_2017_final_alphabetically_with_first_seven_columns_back) 

### checking they are the same length - OK!

length(column_headings_2015_final_alphabetically_with_first_seven_columns_back) # 126
length(column_headings_2016_final_alphabetically_with_first_seven_columns_back) # 126
length(column_headings_2017_final_alphabetically_with_first_seven_columns_back) # 126 

### checking column heading vectors are identical - OK!

column_headings_2015_final_alphabetically_with_first_seven_columns_back == column_headings_2016_final_alphabetically_with_first_seven_columns_back
column_headings_2016_final_alphabetically_with_first_seven_columns_back == column_headings_2017_final_alphabetically_with_first_seven_columns_back



# Joining the three tables together using bind by rows

join_all <- bind_rows(
  bbc_2015_final_alphabetically_with_first_seven_columns_back,
  bbc_2016_final_alphabetically_with_first_seven_columns_back,
  bbc_2017_final_alphabetically_with_first_seven_columns_back)


## dropping columns that do not sound "candy" to me

join_all_candy <- join_all %>% 
  select(- abstained_from_m_ming,
         - bonkers_the_board_game,
         - chardonnay,
         - hugs_actual_physical_hugs,
         - person_of_interest_season_3_dvd_box_set_not_including_disc_4_with_hilarious_outtakes,
         - please_list_any_items_not_included_above_that_give_you_despair,
         - real_housewives_of_orange_county_season_9_blue_ray,
         - vicodin)

dim(join_all_candy)




# cleaning the data points

## age

### age is data class character, also contains other then number values, eg 30's, enough, very
### I am going to change it to integer and drop age outliers 

join_all_candy_age <- join_all_candy %>%
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

join_all_candy_age$age %>% 
  unique() %>% 
  sort()


## country

### checking all distinct values in column country - so many! Here I would recommend to the survey authors to use a "choose from" type of questionnaire..

join_all_candy$country %>% 
  unique()

### renaming typos and gathering all the nonsense ones in value "others"

join_all_candy_age_country <- join_all_candy_age %>% 
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

### all unique countries sorted alphabetically, we have now 36 countries and 1 other category represented in the data frame

join_all_candy_age_country$country %>% 
  unique() %>% 
  sort()


# HURRAY! clean_data data frame for analysis

## last tweaks - getting rid of timestamp column and filling id column with reasonable id numbers

id_vector <- 1:nrow(join_all_candy_age_country)
year_vector <- c(rep(2015, times = nrow(bbc_2015)), rep(2016, times = nrow(bbc_2016)), rep(2017, times = nrow(bbc_2017)))


candy_clean <- join_all_candy_age_country %>% 
  mutate(id = id_vector) %>% 
  mutate(timestamp = year_vector) %>% rename(year = timestamp)

dim(candy_clean)
glimpse(candy_clean)

View(candy_clean)

## double-checking there isn't any column of just missing values - OK! 

nonNAs <- function(x) {
  as.vector(apply(x, 2, function(x) length(which(!is.na(x)))))
}

missing_values_candies <- nonNAs(candy_clean)
missing_values_candies


# saving the new clean_data data frame as a csv file

write_csv(candy_clean, path = "clean_data/candy_clean.csv")
