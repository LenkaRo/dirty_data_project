# dirty_data_project
CodeClan's dirty data project involving cleaning and tidying (very) messy data.

## Task 4 - Halloween Candy

We are dealing with data obtained from three independent surveys.

These surveys were carried on in years 2015, 2016 and 2017.

Respondents from all over the world were ask to rank various candies based on their subjective opinion.

They were offered three options as an answer - *joy*, *despair*, or *meh*

There were also additional questions like what was the respondents' age, what was their country of origin, whether they prefer name Betty or Veronica etc. These questions had no default set of answers and it was up to the respondents to type in whatever they found appropriate.

Some candy types, as well as additional questions, did only appear in one or two surveys.

As a result, the raw data entering the analysis differed significantly and required some deep cleaning first.

More information on the data can be found 
[here](https://www.scq.ubc.ca/so-much-candy-data-seriously/).

### The project structure:

#### Data cleaning

* *[data\_cleaning\_script\_task\_4.R](https://github.com/LenkaRo/dirty_data_project/blob/master/task_4/data_cleaning_scripts/data_cleaning_script_task_4.R)* - combining the three data frames together and saving the cleaned data into a *candy\_clean.csv* file
 
#### Data analysis

* *[candy\_analysis.Rmd](https://github.com/LenkaRo/dirty_data_project/blob/master/task_4/analysis_and_documentation/candy_analysis.Rmd)* - reading in the *candy\_clean.csv* file and carrying out an analysis

Both files are thoroughly commented on what steps have been taken.
