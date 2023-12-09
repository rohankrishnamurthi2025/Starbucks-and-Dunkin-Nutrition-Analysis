##Food ANALYSIS
#Load packages
library(magick)
library(tidyverse)
library(png)
library(kableExtra)
library(knitr)

#A. Starbucks food nutrient information

#A1: Individual nutrient information
starbucks_food_protein <- starbucks_food_data |> 
  arrange(desc(protein_g)) %>%
  slice(1:5) |> 
  select(item, protein_g)

kable(starbucks_food_protein, caption = "Highest Protein Foods, Starbucks")

starbucks_food_fiber <- starbucks_food_data |> 
  arrange(desc(fiber_g)) %>%
  slice(1:5) |> 
  select(item, fiber_g)

kable(starbucks_food_fiber, caption = "Highest Fiber Foods, Starbucks")


starbucks_food_fat <- starbucks_food_data |> 
  arrange(fat_g) %>%
  slice(1:5) |> 
  select(item, fat_g)

kable(starbucks_food_fat, caption = "Least Fat Foods, Starbucks")

starbucks_food_carb <- starbucks_food_data |> 
  arrange(carb_g) %>%
  slice(1:5) |> 
  select(item, carb_g)

kable(starbucks_food_carb, caption = "Lowest Carbohydrate Foods, Starbucks")

starbucks_food_calories <- starbucks_food_data |> 
  arrange(calories) %>%
  slice(1:5) |> 
  select(item, calories)

kable(starbucks_food_calories, caption = "Lowest Calorie Foods, Starbucks")

#A2: analyzing multiple Starbucks food variables

starbucks_food_table <- starbucks_food_data |> 
  group_by(item) |> 
  summarize(protein = round(mean(protein_g, na.rm = TRUE)),
            fiber = round(mean(fiber_g, na.rm = TRUE)),
            fat = round(mean(fat_g, na.rm = TRUE)),
            carb = round(mean(carb_g, na.rm = TRUE)),
            fiber_g = round(mean(fiber_g, na.rm = TRUE)),
            ratio = round((protein*fiber)/(fat*carb), digits = 4)) |> 
  top_n(10, ratio) |> 
  arrange(desc(ratio)) |> 
  rename("Protein (g)" = protein, 
         "Fiber (g)" = fiber,
         "Carbohydrates (g)" = carb,
         "Fat (g)" = fiber,
         "Healthiness Ratio" = ratio,
         "Food Item" = item)


kable(starbucks_food_table, caption = "Health Ratio of Starbucks Food Items")

#B Dunkin Food Analysis



