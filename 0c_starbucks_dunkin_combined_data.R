##COMPARISON OF STARBUCKS AND DUNKIN DONUTS
#Load packages
library(magick)
library(tidyverse)
library(png)
library(kableExtra)

# Create dataframe for all drinks
starbucks_drinks_data_2 <- starbucks_drinks_data |> 
  rename(category = beverage_category) |> 
  mutate(retailer = "Starbucks")

dunkin_drinks_data_2 <- dunkin_donuts_drinks_data |> 
  mutate(retailer = "Dunkin Donuts")

all_drinks_data <- merge(
  (starbucks_drinks_data_2 |> 
     select(category, item, calories, fat_g, carb_g, fiber_g, protein_g, sugars_g, retailer)),
  (dunkin_drinks_data_2 |> 
     select(category, item, calories, fat_g, carb_g, fiber_g, protein_g, sugars_g, retailer)),
  all.x = TRUE, 
  all.y = TRUE)

all_drinks_data$category <- as.factor(all_drinks_data$category)
all_drinks_data$retailer <- as.factor(all_drinks_data$retailer)

write_csv(all_drinks_data, 
          "data/all_drinks_data.csv", 
          col_names=TRUE)

# Create dataframe for all food items
starbucks_food_2 <- starbucks_food_data |> 
  mutate(retailer = "Starbucks")
dunkin_food_2 <- dunkin_donuts_food_data |> 
  mutate(retailer = "Dunkin Donuts")

all_food_data <- merge(
  (starbucks_food_2 |> 
     select(item, calories, fat_g, carb_g, fiber_g, protein_g, retailer)),
  (dunkin_food_2 |> 
     select(item, calories, fat_g, carb_g, fiber_g, protein_g, retailer)),
  all.x = TRUE, 
  all.y = TRUE)

all_food_data$retailer <- as.factor(all_food_data$retailer)

write_csv(all_food_data, 
          "data/all_food_data.csv", 
          col_names=TRUE)



