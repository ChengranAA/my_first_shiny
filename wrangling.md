# <u>**Data Wrangling**</u>

The following is the library used in the data cleaning process

```{R}
library(tidyverse)
library(lubridate)
```



Two datasets was stored in the Data folder and was read by the method recommended by the data.world webseit. 

```{R}
covid_activity_all <- read.csv("Data/COVID-19_Activity.csv", header=TRUE, stringsAsFactors = FALSE)
covid_data_loca_pop <- read.csv("Data/Master_Location_Pop_Table.csv", header=TRUE, stringsAsFactors=FALSE)
```



For the preprocessing, the *covid_activity_all* dataset has some irrelvant information for this analysis, `select()` was used to deselect those variables. The `filter()` was used to clean up the empty entries in the *CONTINENT_NAME* and *COUNTRY_ALPHA_3_CODE* column. Finally, the total number of new case and death of each day for each country was calculated. 

```{R}
covid_activity_all <- covid_activity_all %>%
  select(-DATA_SOURCE_NAME, -COUNTY_NAME, -PROVINCE_STATE_NAME, -COUNTY_FIPS_NUMBER, COUNTRY_ALPHA_2_CODE) %>%
  filter(!is.na(COUNTRY_ALPHA_3_CODE) & !is.na(CONTINENT_NAME)) %>%
  filter(COUNTRY_ALPHA_3_CODE != "" & CONTINENT_NAME != "") %>%
  group_by(COUNTRY_SHORT_NAME, REPORT_DATE, COUNTRY_ALPHA_3_CODE, CONTINENT_NAME) %>%
  summarise(total_new_positive_by_day = sum(PEOPLE_POSITIVE_NEW_CASES_COUNT), 
            total_new_death_day = sum(PEOPLE_DEATH_NEW_COUNT), .groups = 'drop')
```



For preprocessing the *covid_data_loca_pop* data set, first, irrelvant information was removed. Secondly, Total population will be caculated for each country, ISO3 code was used to identified each unique country, the data set was grounped by the *COUNTRY_ALPHA_3_CODE* variable. Finally, only population greater than zero will be selected. 

```{R}
covid_data_loca_pop <- covid_data_loca_pop %>%
  select(-DATA_SOURCE_NAME, -GEO_LATITUDE, -GEO_LONGITUDE) %>%
  group_by(COUNTRY_ALPHA_3_CODE) %>%
  summarise(total_population = sum(GEO_REGION_POPULATION_COUNT), .groups = "drop") %>%
  filter(total_population > 0)
```



Two datasets was joined by *COUNTRY_ALPHA_3_CODE* variable. A new data set was created. 

```{R}
covid_full_by_country_pop <- left_join(covid_data_loca_pop, covid_full_by_country, by = c("COUNTRY_ALPHA_3_CODE"))
```



Percentage of total infected and death was calculated for each country. Population (millions of) was added for better visualization. 

```{R}
covid_full_by_country_pop <- covid_full_by_country_pop %>%
  mutate(percent_infected = round(total_infected / total_population * 100, 2), 
         percent_death = round(total_death / total_population * 100, 2), 
         population_million_of = total_population / 1000000)
```



For plotting total number of infected and death per day, the *covid_activity_all_year* dataframe was created by summariszing each observation by day and added a new variable *year* for plot grouping. Also, the type of the *REPORT_DATE* and *year* were changed. 

```{R}
covid_activity_all_year <- covid_activity_all %>%
  group_by(REPORT_DATE) %>%
  summarise(total_new_death_day = sum(total_new_death_day), 
            total_new_positive_by_day = sum(total_new_positive_by_day)) %>%
  mutate(year = year(REPORT_DATE))

covid_activity_all_year$REPORT_DATE <- as_date(covid_activity_all_year$REPORT_DATE)
covid_activity_all_year$year <- as.factor(covid_activity_all_year$year)
```

















