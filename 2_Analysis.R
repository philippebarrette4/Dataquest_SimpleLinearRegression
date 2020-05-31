library(readr) # Load and write csv files
library(ggplot2) # Data vizualisation
library(dplyr) # Data wrangling and manipulation
library(tidyr) # Nesting and unnesting dataframes 
library(purrr) # Fit models to nested dataframes with map()

# Data importation
NYC_property_sales <- read_csv("NYC_property_sales.csv")
glimpse(NYC_property_sales)
#Verify if "R4" category is in the data
#Had a problem with the latest version of dataset online
#The csv file with the dataTransfo.R file doesn't contain R4 buildings
"R4" %in% unique(NYC_property_sales$building_class_at_time_of_sale)

# Filtering of the data (Condominium only)
NYC_condos <- NYC_property_sales %>%
  filter(building_class_at_time_of_sale == "R4")

# Data Vizualisation of sale_price explained by gross_square_feet
#1)
ggplot(data = NYC_condos, aes(x = gross_square_feet, 
                              y = sale_price)) +
  geom_point(aes(color = borough), alpha = 0.3) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 75000000)) +
  xlim(0, 10000) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Condominiums sale prices explained by gross square feet",
       x = "Size (Gross Square Feet)",
       y = "Sale Price (USD)") +
  theme(plot.title = element_text(hjust = 0.5))

# Data Vizualisation of sale_price explained by gross_square_feet
#2) Zoom in
#The majority of sizes of the condominiums is less than 5000 feet
#The plot has a linear tendency (as the size grow, the sale price tends to grow to)
ggplot(data = NYC_condos, aes(x = gross_square_feet, 
                              y = sale_price)) +
  geom_point(aes(color = borough), alpha = 0.3) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 20000000)) +
  xlim(0, 5000) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Condominiums sale prices explained by gross square feet",
       x = "Size (Gross Square Feet)",
       y = "Sale Price (USD)") +
  theme(plot.title = element_text(hjust = 0.5))

# Data Vizualisation of sale_price explained by gross_square_feet
#3) Facet_wrap
#Looking at the plot above, we see that, in general, 
#larger condominiums are associated with a higher sale price in each borough. 
#The data follows a somewhat linear pattern in each plot. But the spread 
#is difficult to see with the Manhattan scatterplot, potentially because 
#of the property sale of around $200 million visible to the far-right which may 
#be impacting the visualization. There is no obvious curvature with the shape of the data, 
#for any borough. The strength of the bivariate relationship is moderate for most boroughs, 
#except for the Queens borough which looks to have a weaker relationship between sale price 
#and size. 
ggplot(data = NYC_condos_original, aes(x = gross_square_feet, 
                              y = sale_price)) +
  geom_point(alpha = 0.3) +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ borough, scales = "free", ncol = 2) +
  labs(title = "Condominiums sale prices explained by gross square feet",
       x = "Size (Gross Square Feet)",
       y = "Sale Price (USD)") +
  theme(plot.title = element_text(hjust = 0.5))
  


## Outliers and Data Integrity Issues
#Creating a copy before the transformation of NYC_condos
NYC_condos_original <- NYC_condos

#Removing the sale record of 200 000 000$ because its an extrem value
NYC_condos <- NYC_condos %>%
  filter(address != "165 East 66th St, Resi")

#Removing the sale record of this address because it's an exception and 
#it will influence our model
View(NYC_condos %>%
  filter(address == "220 Central Park South, 50"))

NYC_condos <- NYC_condos %>%
  filter(address != "220 Central Park South, 50")


#Investigation of the forty sale records in the Brooklyn borough
#This record sales represent the same transaction, the sale price represent the price for all
#the forty units, not the price for each unit. Because of that, 
#View(
#  NYC_condos %>%
#    filter(borough == "Brooklyn") %>%
#    arrange(desc(sale_price)))

#This is sale records where there are multiple entries
#It contains the transaction of 40 units 
multi_unit_sales <- NYC_condos %>%
  group_by(sale_price, sale_date) %>%
  filter(n() >= 3) %>%
  arrange(desc(sale_price))

#Removing these multiple unit sales
NYC_condos <- NYC_condos %>%
  anti_join(multi_unit_sales)


## Linear Regression modeling
#Comparison of models with original data (with outliers)
#and data cleaned (without outliers)
#Data cleaned up
NYC_condos_lm <- lm(sale_price ~ gross_square_feet, data = NYC_condos)
summary(NYC_condos_lm)
confint(NYC_condos_lm)
sigma(NYC_condos_lm)

#Original Data
NYC_condos_original_lm <- lm(sale_price ~ gross_square_feet, data = NYC_condos_original)
summary(NYC_condos_original_lm)
confint(NYC_condos_original_lm)
sigma(NYC_condos_original_lm)

#Comparison
#These model represents a bivariate linear regression of the variables sale_price
#explained by gross_square_feet.
#When comparing the to models we see that both of them have a t-statistic high enough and
#a p-value low enough to show that there is a relationship between the two variables. 
#However, the model of clean data has a t statistic far more elevated and has a RSE smaller.
#In addition, we can see that the model with cleaned data has a far better R-squared value thant 
#the other. We can now declare that the cleaning process was successfull, 
#because the model of the cleaned dataset `NYC_condos` is far better than the other one.
  
#Vizualisation of the data cleaned up
ggplot(data = NYC_condos, aes(x = gross_square_feet, 
                              y = sale_price)) +
  geom_point(alpha = 0.3) +
  scale_y_continuous(labels = scales::comma) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ borough, scales = "free", ncol = 2) +
  labs(title = "Condominiums sale prices explained by gross square feet",
       x = "Size (Gross Square Feet)",
       y = "Sale Price (USD)") +
  theme(plot.title = element_text(hjust = 0.5))


## Linear Regression Models for each Borough - coefficient Estimates

#Nesting NYC_condos by borough
NYC_nested <- NYC_condos %>%
  group_by(borough) %>%
  nest()

























