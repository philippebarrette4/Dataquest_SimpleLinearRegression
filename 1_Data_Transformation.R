library(readxl) # Load Excel files
library(dplyr) # Data wrangling and manipulation
library(magrittr) # Make all colnames lower case with no space
library(stringr) # String formatting and replacement
library(tidyr) # Nesting and unnesting dataframes

#Load excel files for each borough
bronx <- read_excel("./datasetProject/rollingsales_bronx.xls", skip = 4)
brooklyn <- read_excel("./datasetProject/rollingsales_brooklyn.xls", skip = 4)
manhattan <- read_excel("./datasetProject/rollingsales_manhattan.xls", skip = 4)
queens <- read_excel("./datasetProject/rollingsales_queens.xls", skip = 4)
statenisland <- read_excel("./datasetProject/rollingsales_statenisland.xls", skip = 4)

#See the number attributed to the boroughs
head(bronx) #2
head(brooklyn) #3
head(manhattan) #1
head(queens) #4
head(statenisland) #5

#Binding the five boroughs
NYC_property_sales <- bind_rows(bronx, brooklyn, manhattan, queens, statenisland)

#Removing the boroughs objects
rm(bronx, brooklyn, manhattan, queens, statenisland)

#Changing number of boroughs for their names
NYC_property_sales <- NYC_property_sales %>%
  mutate(BOROUGH =
  case_when(
    NYC_property_sales$BOROUGH == 1 ~ "Manhattan",
    NYC_property_sales$BOROUGH == 2 ~ "Bronx",
    NYC_property_sales$BOROUGH == 3 ~ "Brooklyn",
    NYC_property_sales$BOROUGH == 4 ~ "Queens",
    NYC_property_sales$BOROUGH == 5 ~ "Statenisland"))

#Conversion of all column names to lower case with no spaces
colnames(NYC_property_sales) %<>% str_replace_all("\\s", "_") %>% tolower()
#See if the column names were changed
str(NYC_property_sales)

#Converison of Capitalized columns to Title Case
NYC_property_sales <- NYC_property_sales %>%
  mutate(neighborhood = str_to_title(neighborhood)) %>%
  mutate(building_class_category = str_to_title(building_class_category)) %>%
  mutate(address = str_to_title(address))
#See if columns were changed
str(NYC_property_sales)

#Drop the columns that are empty (Here only ease-ment) and select only distinct observations
NYC_property_sales <- NYC_property_sales %>%
  select(-`ease-ment`) %>%
  distinct()
#See if it worked
str(NYC_property_sales)

#Filtering of the data
NYC_property_sales <- NYC_property_sales %>%
  filter(sale_price > 10000) %>%
  filter(gross_square_feet > 0) %>%
  drop_na(c(gross_square_feet, sale_price))
#Verification of the filtering
summary(NYC_property_sales %>% select(sale_price, gross_square_feet))
is.na(NYC_property_sales %>% select(gross_square_feet, sale_price))

#Arranging boroughs by alphebetic order
NYC_property_sales <- NYC_property_sales %>%
  arrange(borough)

#Writing a csv file with the modified dataset
write_csv(NYC_property_sales, "NYC_property_sales.csv")








