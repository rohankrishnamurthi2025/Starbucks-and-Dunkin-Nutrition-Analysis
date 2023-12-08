##DUNKIN DONUTS DATA PREPARATION
#Load packages and data sets
library(magick)
library(tidyverse)
dunkin_donuts_data_raw <- read_csv("data/raw/dunkindonutsnutrition.csv")

#Make a copy to then work with
dunkin_donuts_data <- dunkin_donuts_data_raw

#Renaming variable names in food dataset
names(dunkin_donuts_data)[names(dunkin_donuts_data) == "Category"] <- "category"
names(dunkin_donuts_data)[names(dunkin_donuts_data) == "Item"] <- "item"
names(dunkin_donuts_data)[names(dunkin_donuts_data) == "Calories"] <- "calories"
names(dunkin_donuts_data)[names(dunkin_donuts_data) == "Total Fat (g)"] <- "fat_g"
names(dunkin_donuts_data)[names(dunkin_donuts_data) == "Saturated Fat (g)"] <- "saturated_fat_g"
names(dunkin_donuts_data)[names(dunkin_donuts_data) == "Trans Fat (g)"] <- "trans_fat_g"
names(dunkin_donuts_data)[names(dunkin_donuts_data) == "Cholesterol (mg)"] <- "cholesterol_mg"
names(dunkin_donuts_data)[names(dunkin_donuts_data) == "Sodium (mg)"] <- "sodium_mg"
names(dunkin_donuts_data)[names(dunkin_donuts_data) == "Carbs (g)"] <- "carb_g"
names(dunkin_donuts_data)[names(dunkin_donuts_data) == "Fiber (g)"] <- "fiber_g"
names(dunkin_donuts_data)[names(dunkin_donuts_data) == "Sugars (g)"] <- "sugars_g"
names(dunkin_donuts_data)[names(dunkin_donuts_data) == "Protein (g)"] <- "protein_g"
names(dunkin_donuts_data)[names(dunkin_donuts_data) == "Weight Watcher Pnts"] <- "weight_watcher_points"

#Add item type column (factor) to distinguish food and drink items
dunkin_donuts_data <- dunkin_donuts_data |> 
  mutate(item_type = NA)

for (i in 1:(length(dunkin_donuts_data$item))){
  if (dunkin_donuts_data$category[i] == "Cold Brew Coffee"){
    dunkin_donuts_data$item_type[i] <- "drink"}
  if (dunkin_donuts_data$category[i] == "Coolatta"){
    dunkin_donuts_data$item_type[i] <- "drink"}
  if (dunkin_donuts_data$category[i] == "Donuts"){
    dunkin_donuts_data$item_type[i] <- "food"}
  if (dunkin_donuts_data$category[i] == "Frozen Coffee"){
    dunkin_donuts_data$item_type[i] <- "drink"}
  if (dunkin_donuts_data$category[i] == "Hot Americano"){
    dunkin_donuts_data$item_type[i] <- "drink"}
  if (dunkin_donuts_data$category[i] == "Hot Cappuccino"){
    dunkin_donuts_data$item_type[i] <- "drink"}
  if (dunkin_donuts_data$category[i] == "Hot Chocolate"){
    dunkin_donuts_data$item_type[i] <- "drink"}
  if (dunkin_donuts_data$category[i] == "Hot Coffee"){
    dunkin_donuts_data$item_type[i] <- "drink"}
  if (dunkin_donuts_data$category[i] == "Hot Latte"){
    dunkin_donuts_data$item_type[i] <- "drink"}
  if (dunkin_donuts_data$category[i] == "Hot Macchiato"){
    dunkin_donuts_data$item_type[i] <- "drink"}
  if (dunkin_donuts_data$category[i] == "Iced Americano"){
    dunkin_donuts_data$item_type[i] <- "drink"}
  if (dunkin_donuts_data$category[i] == "Iced Cappuccino"){
    dunkin_donuts_data$item_type[i] <- "drink"}
  if (dunkin_donuts_data$category[i] == "Iced Coffee"){
    dunkin_donuts_data$item_type[i] <- "drink"}
  if (dunkin_donuts_data$category[i] == "Iced Latte"){
    dunkin_donuts_data$item_type[i] <- "drink"}
  if (dunkin_donuts_data$category[i] == "Iced Macchiato"){
    dunkin_donuts_data$item_type[i] <- "drink"}
  if (dunkin_donuts_data$category[i] == "Iced Tea"){
    dunkin_donuts_data$item_type[i] <- "drink"}
  if (dunkin_donuts_data$category[i] == "Dunkin Refreshers"){
    dunkin_donuts_data$item_type[i] <- "drink"}
  if (dunkin_donuts_data$category[i] == "Kolache"){
    dunkin_donuts_data$item_type[i] <- "food"}
  if (dunkin_donuts_data$category[i] == "Muffins"){
    dunkin_donuts_data$item_type[i] <- "food"}
  if (dunkin_donuts_data$category[i] == "Sandwiches"){
    dunkin_donuts_data$item_type[i] <- "food"}
  if (dunkin_donuts_data$category[i] == "Soft Serve"){
    dunkin_donuts_data$item_type[i] <- "food"}
  if (dunkin_donuts_data$category[i] == "Hash Browns"){
    dunkin_donuts_data$item_type[i] <- "food"}
  if (dunkin_donuts_data$category[i] == "Dunkin, Bowls"){
    dunkin_donuts_data$item_type[i] <- "food"}
  
}

dunkin_donuts_data$item_type <- as.factor(dunkin_donuts_data$item_type)

#Convert nutritional info variables into numeric vectors
dunkin_donuts_data$calories <- as.numeric(dunkin_donuts_data$calories)
dunkin_donuts_data$fat_g <- as.numeric(dunkin_donuts_data$fat_g)
dunkin_donuts_data$saturated_fat_g <- as.numeric(dunkin_donuts_data$saturated_fat_g)
dunkin_donuts_data$trans_fat_g <- as.numeric(dunkin_donuts_data$trans_fat_g)
dunkin_donuts_data$cholesterol_mg <- as.numeric(dunkin_donuts_data$cholesterol_mg)
dunkin_donuts_data$sodium_mg <- as.numeric(dunkin_donuts_data$sodium_mg)
dunkin_donuts_data$carb_g <- as.numeric(dunkin_donuts_data$carb_g)
dunkin_donuts_data$fiber_g <- as.numeric(dunkin_donuts_data$fiber_g)
dunkin_donuts_data$sugars_g <- as.numeric(dunkin_donuts_data$sugars_g)
dunkin_donuts_data$protein_g <- as.numeric(dunkin_donuts_data$protein_g)
dunkin_donuts_data$weight_watcher_points <- as.numeric(dunkin_donuts_data$weight_watcher_points)

#Add variable to record size (factor) of item
dunkin_donuts_data <- dunkin_donuts_data |> 
  mutate(size = NA)

for (i in 1:(length(dunkin_donuts_data$item))){

  if (grepl("Small", dunkin_donuts_data$item[i], fixed = TRUE)){
    dunkin_donuts_data$size[i] <- "small"
  }
  if (grepl("Medium", dunkin_donuts_data$item[i], fixed = TRUE)){
    dunkin_donuts_data$size[i] <- "medium"
  }
  if (grepl("Large", dunkin_donuts_data$item[i], fixed = TRUE)){
    dunkin_donuts_data$size[i] <- "large"
  }
  
  }
  

##Convert remaining sizes to the character "not applicable"
for (i in 1:(length(dunkin_donuts_data$item))){
  if (is.na(dunkin_donuts_data$size[i])){
    dunkin_donuts_data$size[i] <- "not applicable"}
}

dunkin_donuts_data$size <- as.factor(dunkin_donuts_data$size)

#Convert category variable into factor
dunkin_donuts_data$category <- as.factor(dunkin_donuts_data$category)


#Creating separate food and drink data sets
dunkin_donuts_drinks <- dunkin_donuts_data |> 
  filter(item_type == "drink")

dunkin_donuts_food <- dunkin_donuts_data |> 
  filter(item_type == "food")



