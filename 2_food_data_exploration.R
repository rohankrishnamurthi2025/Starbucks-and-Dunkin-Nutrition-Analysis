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



## Analysis of Individual Nutrient in Food groups
dunkin_food_table <- dunkin_donuts_food_data |> 
  group_by(category) |> 
  summarize(protein = round(mean(protein_g, na.rm = TRUE)),
            fiber = round(mean(fiber_g, na.rm = TRUE)),
            carb = round(mean(carb_g, na.rm = TRUE)),
            fat = round(mean(fat_g, na.rm = TRUE)),
            calories = round(mean(calories, na.rm = TRUE))
  ) |> 
  rename("Average protein content (g)" = protein,
         "Average fiber content (g)" = fiber,
         "Average carbohydrate content (g)" = carb,
         "Average fat content (g)" = fat,
         "Average calories" = calories,
         "Food Category" = category)
kable(dunkin_food_table, caption = "Dunkin Donuts Food Nutrient Information")


graph_1 <- dunkin_donuts_food_data |> 
  ggplot(aes(x = protein_g, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = category)) + 
  labs(title = "Frequency Polygon of Protein (g) content by food category, Dunkin Donuts", 
       x = "Protein (g)", y = "Density") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/food_analysis/dunkin_food_protein_freqpoly.png"), plot = graph_1)

graph_2 <- dunkin_donuts_food_data |> 
  ggplot(aes(x = fiber_g, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = category)) + 
  labs(title = "Frequency Polygon of Fiber (g) content by food category, Dunkin Donuts", 
       x = "Fiber (g)", y = "Density") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/food_analysis/dunkin_food_fiber_freqpoly.png"), plot = graph_2)

graph_3 <- dunkin_donuts_food_data |> 
  ggplot(aes(x = carb_g, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = category)) + 
  labs(title = "Frequency Polygon of Carbohydrate (g) content by food category, Dunkin Donuts", 
       x = "Carbohydrate (g)", y = "Density") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/food_analysis/dunkin_food_carb_freqpoly.png"), plot = graph_3)

graph_4 <- dunkin_donuts_food_data |> 
  ggplot(aes(x = fat_g, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = category)) + 
  labs(title = "Frequency Polygon of Fat (g) content by food category, Dunkin Donuts", 
       x = "Fat (g)", y = "Density") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/food_analysis/dunkin_food_fat_freqpoly.png"), plot = graph_4)

graph_5 <- dunkin_donuts_food_data |> 
  ggplot(aes(x = calories, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = category)) + 
  labs(title = "Frequency Polygon of calories by food category, Dunkin Donuts", 
       x = "Calories", y = "Density") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/food_analysis/dunkin_food_calorie_freqpoly.png"), plot = graph_5)


#A2: analyzing multiple Dunkin food variables

dunkin_food_table_2 <- dunkin_donuts_food_data |> 
  group_by(category) |> 
  summarize(protein = round(mean(protein_g, na.rm = TRUE)),
            fiber = round(mean(fiber_g, na.rm = TRUE)),
            fat = round(mean(fat_g, na.rm = TRUE)),
            carb = round(mean(carb_g, na.rm = TRUE)),
            fiber_g = round(mean(fiber_g, na.rm = TRUE)),
            ratio = round((protein*fiber)/(fat*carb), digits = 4)) |> 
  arrange(desc(ratio)) |> 
  rename("Protein (g)" = protein, 
         "Fiber (g)" = fiber,
         "Carbohydrates (g)" = carb,
         "Fat (g)" = fiber,
         "Healthiness Ratio" = ratio,
         "Food Item" = item)

kable(dunkin_food_table, caption = "Dunkin Donuts Food Categories' Healthiness Ratio")

#A3: analyzing the donuts themselves

dunkin_donuts_table <- dunkin_donuts_food_data |> 
  filter(category == "Donuts")
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
  
kable(dunkin_donuts_table, caption = "Dunkin Donuts' Donuts Nutritional Information and Healthiness Ratio")

#AD: evaluating healthiness metric accuracy

graph_1 <- dunkin_donuts_food_data |> 
  ggplot(aes(x = (protein_g/calories), y = (fat_g/calories))) + 
  geom_point(aes(color = category)) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot comparing protein to fat content per calorie of Dunkin Donuts food items", 
       x = "Protein (g/calorie)", y = "Fat (g/calorie)") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/food_analysis/dunkin_food_protein_fat.png"), plot = graph_1)

graph_2 <- dunkin_donuts_food_data |> 
  ggplot(aes(x = protein_g/calories, y = carb_g/calories)) + 
  geom_point(aes(color = category)) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot comparing protein to carbohydrate content per calorie of Dunkin Donuts food items", 
     x = "Protein (g/calorie)", y = "Carbohydrate (g/calorie)") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/food_analysis/dunkin_food_protein_carb.png"), plot = graph_2)

graph_3 <- dunkin_donuts_food_data |> 
  ggplot(aes(x = fiber_g/calories, y = fat_g/calories)) + 
  geom_point(aes(color = category)) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot comparing fiber to fat content per calorie of Dunkin Donuts food items", 
     x = "Fiber (g/calorie)", y = "Fat (g/calorie)") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/food_analysis/dunkin_food_fiber_fat.png"), plot = graph_3)

graph_4 <- dunkin_donuts_food_data |> 
  ggplot(aes(x = fiber_g/calories, y = carb_g/calories)) + 
  geom_point(aes(color = category)) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatterplot comparing fiber to carbohydrate content per calorie of Dunkin Donuts food items", 
     x = "Fiber (g/calorie)", y = "Carbohydrate (g/calorie)") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/food_analysis/dunkin_food_fiber_carb.png"), plot = graph_4)




