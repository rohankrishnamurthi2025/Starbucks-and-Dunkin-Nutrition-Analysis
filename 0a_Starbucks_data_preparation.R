##STARBUCKS DATA PREPARATION
#Load packages and data sets
library(magick)
library(tidyverse)
starbucks_drinks_data_raw <- read_csv("data/raw/starbucks.csv")
starbucks_food_data_raw <- read_csv("data/raw/starbucks_menu_nutrition_food_redo.csv")

#Make copies to then work with
starbucks_drinks_data <- starbucks_drinks_data_raw
starbucks_food_data <- starbucks_food_data_raw

#Renaming variable names in food dataset
names(starbucks_food_data)[names(starbucks_food_data) == "Food Item"] <- "food_item"
names(starbucks_food_data)[names(starbucks_food_data) == "Calories"] <- "calories"
names(starbucks_food_data)[names(starbucks_food_data) == "Fat g"] <- "fat_g"
names(starbucks_food_data)[names(starbucks_food_data) == "Carb g"] <- "carb_g"
names(starbucks_food_data)[names(starbucks_food_data) == "Fiber g"] <- "fiber_g"
names(starbucks_food_data)[names(starbucks_food_data) == "Protein g"] <- "protein_g"


#Renaming variable names in drinks dataset
names(starbucks_drinks_data)[names(starbucks_drinks_data) == "Beverage category"] <- "beverage_category"
names(starbucks_drinks_data)[names(starbucks_drinks_data) == "Beverage"] <- "beverage"
names(starbucks_drinks_data)[names(starbucks_drinks_data) == "Beverage prep"] <- "beverage_prep"
names(starbucks_drinks_data)[names(starbucks_drinks_data) == "Calories"] <- "calories"
names(starbucks_drinks_data)[names(starbucks_drinks_data) == "Total Fat g"] <- "fat_g"
names(starbucks_drinks_data)[names(starbucks_drinks_data) == "Trans Fat g"] <- "trans_fat_g"
names(starbucks_drinks_data)[names(starbucks_drinks_data) == "Saturated Fat g"] <- "saturated_fat_g"
names(starbucks_drinks_data)[names(starbucks_drinks_data) == "Sodium mg"] <- "sodium_mg"
names(starbucks_drinks_data)[names(starbucks_drinks_data) == "Total Carbohydrates g"] <- "carb_g"
names(starbucks_drinks_data)[names(starbucks_drinks_data) == "Cholesterol mg"] <- "cholesterol_mg"
names(starbucks_drinks_data)[names(starbucks_drinks_data) == "Dietary Fiber g"] <- "fiber_g"
names(starbucks_drinks_data)[names(starbucks_drinks_data) == "Sugars g"] <- "sugars_g"
names(starbucks_drinks_data)[names(starbucks_drinks_data) == "Protein g"] <- "protein_g"
names(starbucks_drinks_data)[names(starbucks_drinks_data) == "Vitamin A percent DV"] <- "vitamin_a_percent_dv"
names(starbucks_drinks_data)[names(starbucks_drinks_data) == "Vitamin C percent DV"] <- "vitamin_c_percent_dv"
names(starbucks_drinks_data)[names(starbucks_drinks_data) == "Calcium percent DV"] <- "calcium_percent_dv"
names(starbucks_drinks_data)[names(starbucks_drinks_data) == "Iron percent DV"] <- "iron_percent_dv"
names(starbucks_drinks_data)[names(starbucks_drinks_data) == "Caffeine mg"] <- "caffeine_mg"



#Adding size to beverage_prep column in drinks dataset
for (i in 1:(length(starbucks_drinks_data$beverage_prep) - 2)) {
  if (starbucks_drinks_data$beverage_prep[i] == "Short Nonfat Milk"){
    if (starbucks_drinks_data$beverage_prep[i+1] == "2% Milk"){
      starbucks_drinks_data$beverage_prep[i+1] <- "Short 2% Milk"
    }
    if (starbucks_drinks_data$beverage_prep[i+1] == "Whole Milk"){
      starbucks_drinks_data$beverage_prep[i+1] <- "Short Whole Milk"
    }
    if (starbucks_drinks_data$beverage_prep[i+2] == "Soymilk"){
      starbucks_drinks_data$beverage_prep[i+2] <- "Short Soymilk"
    }
  }
  if (starbucks_drinks_data$beverage_prep[i] == "Tall Nonfat Milk"){
    if (starbucks_drinks_data$beverage_prep[i+1] == "2% Milk"){
      starbucks_drinks_data$beverage_prep[i+1] <- "Tall 2% Milk"
    }
    if (starbucks_drinks_data$beverage_prep[i+1] == "Whole Milk"){
      starbucks_drinks_data$beverage_prep[i+1] <- "Tall Whole Milk"
    }
    if (starbucks_drinks_data$beverage_prep[i+2] == "Soymilk"){
      starbucks_drinks_data$beverage_prep[i+2] <- "Tall Soymilk"
    }
  }
  if (starbucks_drinks_data$beverage_prep[i] == "Grande Nonfat Milk"){
    if (starbucks_drinks_data$beverage_prep[i+1] == "2% Milk"){
      starbucks_drinks_data$beverage_prep[i+1] <- "Grande 2% Milk"
    }
    if (starbucks_drinks_data$beverage_prep[i+1] == "Whole Milk"){
      starbucks_drinks_data$beverage_prep[i+1] <- "Grande Whole Milk"
    }
    if (starbucks_drinks_data$beverage_prep[i+2] == "Soymilk"){
      starbucks_drinks_data$beverage_prep[i+2] <- "Grande Soymilk"
    }
  }
  if (starbucks_drinks_data$beverage_prep[i] == "Venti Nonfat Milk"){
    if (starbucks_drinks_data$beverage_prep[i+1] == "2% Milk"){
      starbucks_drinks_data$beverage_prep[i+1] <- "Venti 2% Milk"
    }
    if (starbucks_drinks_data$beverage_prep[i+1] == "Whole Milk"){
      starbucks_drinks_data$beverage_prep[i+1] <- "Venti Whole Milk"
    }
    if (starbucks_drinks_data$beverage_prep[i+2] == "Soymilk"){
      starbucks_drinks_data$beverage_prep[i+2] <- "Venti Soymilk"
    }
  }
  
}



#Add milk_type column to drinks dataset
starbucks_drinks_data <- starbucks_drinks_data |> 
  mutate(milk_type = NA)

for (i in 1:(length(starbucks_drinks_data$beverage_prep))){
  if (grepl("Nonfat Milk", starbucks_drinks_data$beverage_prep[i], fixed = TRUE)){
    starbucks_drinks_data$milk_type[i] <- "Nonfat Milk"
  }
  if (grepl("2% Milk", starbucks_drinks_data$beverage_prep[i], fixed = TRUE)){
    starbucks_drinks_data$milk_type[i] <- "2% Milk"
  }
  if (grepl("Soymilk", starbucks_drinks_data$beverage_prep[i], fixed = TRUE)){
    starbucks_drinks_data$milk_type[i] <- "Soymilk"
  }
  if (grepl("Whole Milk", starbucks_drinks_data$beverage_prep[i], fixed = TRUE)){
    starbucks_drinks_data$milk_type[i] <- "Whole Milk"
  }
}

#Add size column to drinks dataset
starbucks_drinks_data <- starbucks_drinks_data |> 
  mutate(size = NA)

for (i in 1:(length(starbucks_drinks_data$beverage_prep))){
  if (grepl("Short", starbucks_drinks_data$beverage_prep[i], fixed = TRUE)){
    starbucks_drinks_data$size[i] <- "Short"
  }
  if (grepl("Tall", starbucks_drinks_data$beverage_prep[i], fixed = TRUE)){
    starbucks_drinks_data$size[i] <- "Tall"
  }
  if (grepl("Grande", starbucks_drinks_data$beverage_prep[i], fixed = TRUE)){
    starbucks_drinks_data$size[i] <- "Grande"
  }
  if (grepl("Venti", starbucks_drinks_data$beverage_prep[i], fixed = TRUE)){
    starbucks_drinks_data$size[i] <- "Venti"
  }
  if (grepl("Solo", starbucks_drinks_data$beverage_prep[i], fixed = TRUE)){
    starbucks_drinks_data$size[i] <- "Solo"
  }
  if (grepl("Doppio", starbucks_drinks_data$beverage_prep[i], fixed = TRUE)){
    starbucks_drinks_data$size[i] <- "Doppio"
  }
}


#Creating combined `starbucks_all_data` dataset, from food and drinks dataset
names(starbucks_drinks_data)[names(starbucks_drinks_data) == "beverage"] <- "item"
names(starbucks_food_data)[names(starbucks_food_data) == "food_item"] <- "item"

starbucks_all_data <- merge(
  (starbucks_drinks_data |> 
     select(item, calories, fat_g, carb_g, fiber_g, protein_g)),
  (starbucks_food_data |> 
     select(item, calories, fat_g, carb_g, fiber_g, protein_g)),
  all.x = TRUE, 
  all.y = TRUE)


