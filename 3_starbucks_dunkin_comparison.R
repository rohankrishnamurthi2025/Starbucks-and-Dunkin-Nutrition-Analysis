##COMPARISON OF STARBUCKS AND DUNKIN ITEMS
#Load packages
library(magick)
library(tidyverse)
library(png)
library(kableExtra)

#A Comparison of Starbucks and Dunkin Drinks
##A1 Table
all_drinks_table <- all_drinks_data |> 
  group_by(retailer) |> 
  summarize(protein = round(mean(protein_g, na.rm = TRUE)),
            sugar = round(mean(sugars_g, na.rm = TRUE)),
            fat = round(mean(fat_g, na.rm = TRUE)),
            calories = round(mean(calories, na.rm = TRUE)),
            ratio = round((protein*fiber)/(fat*carb))
  ) |>   rename("Average protein  (g)" = protein, 
                "Average sugar (g)" = sugar,
                "Average fat (g)" = fat,
                "Average calories" = calories,
                "Healthiness ratio" = ratio,
                "Retailer" = retailer)

kable(all_drinks_table, caption = "Starbucks vs Dunkin Donuts Drinks Nutrition")
  


##A2 Compare Sugar Content 
graph_1 <- all_drinks_data |> 
  ggplot(aes(x = sugars_g)) + 
  geom_histogram() + facet_wrap(vars(retailer)) +
  labs(title = "Histogram of grams of sugar for drinks at Starbucks and Dunkin", 
       x = "Sugars (g)", y = "Count") +
  theme(axis.text.x = element_text(angle = 90))

ggsave(filename = ("figures/comparison/sugar_histogram.png"), plot = graph_1)

graph_2 <- all_drinks_data |> 
  ggplot(aes(x = sugars_g, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = retailer)) + 
  labs(title = "Frequency Polygon of Sugar in Drinks by Retailer", 
       x = "Sugar (g)", y = "Density") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/comparison/sugar_freqpoly.png"), plot = graph_2)

##A3 Compare Protein Content
graph_3 <- all_drinks_data |> 
  ggplot(aes(x = protein_g)) + 
  geom_histogram() + facet_wrap(vars(retailer)) +
  labs(title = "Histogram of grams of protein for drinks at Starbucks and Dunkin", 
       x = "Protein (g)", y = "Count") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/comparison/protein_histogram.png"), plot = graph_3)

graph_4 <- all_drinks_data |> 
  ggplot(aes(x = protein_g, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = retailer)) + 
  labs(title = "Frequency Polygon of Protein in Drinks by Retailer", 
       x = "Protein (g)", y = "Density") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/comparison/protein_freqpoly.png"), plot = graph_4)


##A4 Compare Fat Content
graph_5 <- all_drinks_data |> 
  ggplot(aes(x = fat_g)) + 
  geom_histogram() + facet_wrap(vars(retailer)) +
  labs(title = "Histogram of grams of fat for drinks at Starbucks and Dunkin", 
       x = "Fat (g)", y = "Count") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/comparison/fat_histogram.png"), plot = graph_5)

graph_6 <- all_drinks_data |> 
  ggplot(aes(x = fat_g, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = retailer)) + 
  labs(title = "Frequency Polygon of Fat in Drinks by Retailer", 
       x = "Fat (g)", y = "Density") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/comparison/fat_freqpoly.png"), plot = graph_6)

##A5 Compare Calories
graph_7 <- all_drinks_data |> 
  ggplot(aes(x = calories)) + 
  geom_histogram() + facet_wrap(vars(retailer)) +
  labs(title = "Histogram of calories for drinks at Starbucks and Dunkin", 
       x = "Calories", y = "Count") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/comparison/calories_histogram.png"), plot = graph_7)

graph_8 <- all_drinks_data |> 
  ggplot(aes(x = calories, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = retailer)) + 
  labs(title = "Frequency Polygon of Calories in Drinks by Retailer", 
       x = "Calories", y = "Density") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/comparison/calories_freqpoly.png"), plot = graph_8)


#B Comparison of Food Items
##B1: Nutrition Table
all_food_table <- all_food_data |> 
  group_by(retailer) |> 
  summarize(protein = round(mean(protein_g, na.rm = TRUE)),
            carb = round(mean(carb_g, na.rm = TRUE)),
            fiber = round(mean(fiber_g, na.rm = TRUE)),
            fat = round(mean(fat_g, na.rm = TRUE)),
            calories = round(mean(calories, na.rm = TRUE)),
            ratio = round((protein*fiber)/(fat*carb))
  ) |>   rename("Average protein (g)" = protein, 
                "Average carbohydrates (g)" = carb,
                "Average fiber (g)" = fiber,
                "Average fat (g)" = fat,
                "Average calories" = calories,
                "Healthiness ratio" = ratio,
                "Retailer" = retailer)

kable(all_food_table, caption = "Starbucks vs Dunkin Donuts Food Nutrition")

##B2: compare protein
graph_1 <- all_food_data |> 
  ggplot(aes(x = protein_g)) + 
  geom_histogram() + facet_wrap(vars(retailer)) +
  labs(title = "Histogram of grams of protein for food items at Starbucks and Dunkin", 
       x = "Protein (g)", y = "Count") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/comparison/food_protein_histogram.png"), plot = graph_1)

graph_2 <- all_food_data |> 
  ggplot(aes(x = protein_g, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = retailer)) + 
  labs(title = "Frequency Polygon of Protein in food items by Retailer", 
       x = "Protein (g)", y = "Density") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/comparison/food_protein_freqpoly.png"), plot = graph_2)

##B3: compare carbohydrates
graph_3 <- all_food_data |> 
  ggplot(aes(x = carb_g)) + 
  geom_histogram() + facet_wrap(vars(retailer)) +
  labs(title = "Histogram of grams of carbohydrates for food items at Starbucks and Dunkin", 
       x = "Carbohydrates (g)", y = "Count") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/comparison/food_carb_histogram.png"), plot = graph_3)

graph_4 <- all_food_data |> 
  ggplot(aes(x = carb_g, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = retailer)) + 
  labs(title = "Frequency Polygon of Carbohydrates in food items by Retailer", 
       x = "Carbohydrates (g)", y = "Density") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/comparison/food_carb_freqpoly.png"), plot = graph_4)

##B4: compare fiber
graph_5 <- all_food_data |> 
  ggplot(aes(x = fiber_g)) + 
  geom_histogram() + facet_wrap(vars(retailer)) +
  labs(title = "Histogram of grams of fiber for food items at Starbucks and Dunkin", 
       x = "Fiber (g)", y = "Count") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/comparison/food_fiber_histogram.png"), plot = graph_5)

graph_6 <- all_food_data |> 
  ggplot(aes(x = fiber_g, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = retailer)) + 
  labs(title = "Frequency Polygon of Fiber in food items by Retailer", 
       x = "Fiber (g)", y = "Density") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/comparison/food_fiber_freqpoly.png"), plot = graph_6)

##B5: compare fat
graph_7 <- all_food_data |> 
  ggplot(aes(x = fat_g)) + 
  geom_histogram() + facet_wrap(vars(retailer)) +
  labs(title = "Histogram of grams of fat for food items at Starbucks and Dunkin", 
       x = "Fat (g)", y = "Count") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/comparison/food_fat_histogram.png"), plot = graph_7)

graph_8 <- all_food_data |> 
  ggplot(aes(x = fat_g, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = retailer)) + 
  labs(title = "Frequency Polygon of fat in food items by retailer", 
       x = "Fat (g)", y = "Density") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/comparison/food_fat_freqpoly.png"), plot = graph_8)


##B6: compare calories
graph_9 <- all_food_data |> 
  ggplot(aes(x = calories)) + 
  geom_histogram() + facet_wrap(vars(retailer)) +
  labs(title = "Histogram of calories for food items at Starbucks and Dunkin", 
       x = "Calories", y = "Count") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/comparison/food_calories_histogram.png"), plot = graph_9)

graph_10 <- all_food_data |> 
  ggplot(aes(x = calories, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = retailer)) + 
  labs(title = "Frequency Polygon of calories in food items by retailer", 
       x = "Calories", y = "Density") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/comparison/food_calories_freqpoly.png"), plot = graph_10)






