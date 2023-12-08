##DRINKS ANALYSIS
#Load packages
library(magick)
library(tidyverse)
library(png)
library(kableExtra)

# A. Healthiest Drinks: content or sugar, calories, fat, etc.
##i: Starbucks

graph_1 <- starbucks_drinks_data |> 
  ggplot(aes(x = beverage_category, y = fat_g)) + 
  geom_boxplot() + 
  labs(title = "Boxplot of grams of fat by drink type, Starbucks", 
       x = "Drink Category", y = "Fat (g)") +
  theme(axis.text.x = element_text(angle = 90))

ggsave(filename = ("figures/drinks_analysis/starbucks_drinks_fat_boxplot.png"), plot = graph_1)
starbucks_drinks_fat_boxplot <- image_read("figures/drinks_analysis/starbucks_drinks_fat_boxplot.png")
#starbucks_drinks_fat_boxplot

graph_2 <- starbucks_drinks_data |> 
  ggplot(aes(x = beverage_category, y = sugars_g)) + 
  geom_boxplot() + 
  labs(title = "Boxplot of grams of sugar by drink type, Starbucks", 
       x = "Drink Category", y = "Sugar (g)") +
  theme(axis.text.x = element_text(angle = 90))

ggsave(filename = ("figures/drinks_analysis/starbucks_drinks_sugar_boxplot.png"), plot = graph_2)
starbucks_drinks_sugar_boxplot <- image_read("figures/drinks_analysis/starbucks_drinks_sugar_boxplot.png")
#beverage_sugar_boxplot

graph_3 <- starbucks_drinks_data |> 
  ggplot(aes(x = beverage_category, y = calories)) + 
  geom_boxplot() + 
  labs(title = "Boxplot of grams of calories by drink type, Starbucks", 
       x = "Drink Category", y = "Calories") +
  theme(axis.text.x = element_text(angle = 90))

ggsave(filename = ("figures/drinks_analysis/starbucks_drinks_calories_boxplot.png"), plot = graph_3)
starbucks_drinks_calories_boxplot <- image_read("figures/drinks_analysis/starbucks_drinks_calories_boxplot.png")
starbucks_drinks_calories_boxplot

starbucks_table <- starbucks_drinks_data |> 
  group_by(beverage_category) |> 
  summarize(fat = round(mean(fat_g, na.rm = TRUE)),
            sugar = round(mean(sugars_g, na.rm = TRUE)),
            calories = round(mean(calories, na.rm = TRUE))) |> 
  rename("Average sugar content (g)" = sugar, 
         "Average fat content (g)" = fat,
         "Average calories" = calories,
         "Drink Category" = beverage_category)

kable(starbucks_table, caption = "Starbucks Drinks Nutritional Info")

##ii: Dunkin

dunkin_table <- dunkin_donuts_drinks_data |> 
  group_by(category) |> 
  summarize(fat = round(mean(fat_g, na.rm = TRUE)),
            sugar = round(mean(sugars_g, na.rm = TRUE)),
            calories = round(mean(calories, na.rm = TRUE))) |> 
  rename("Average sugar content (g)" = sugar, 
         "Average fat content (g)" = fat,
         "Average calories" = calories,
         "Drink Category" = category)

kable(dunkin_table, caption = "Dunkin Donuts Drinks Nutritional Info")

graph_1 <- dunkin_donuts_drinks_data |> 
  ggplot(aes(x = category, y = fat_g)) + 
  geom_boxplot() + 
  labs(title = "Boxplot of grams of fat by drink type, Dunkin Donuts", 
       x = "Drink Category", y = "Fat (g)") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/drinks_analysis/dunkin_drinks_fat_boxplot.png"), plot = graph_1)

graph_2 <- dunkin_donuts_drinks_data |> 
  ggplot(aes(x = category, y = sugars_g)) + 
  geom_boxplot() + 
  labs(title = "Boxplot of grams of sugar by drink type, Dunkin Donuts", 
       x = "Drink Category", y = "Sugar (g)") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/drinks_analysis/dunkin_drinks_sugar_boxplot.png"), plot = graph_2)

graph_3 <- dunkin_donuts_drinks_data |> 
  ggplot(aes(x = category, y = calories)) + 
  geom_boxplot() + 
  labs(title = "Boxplot of calories by drink type, Dunkin Donuts", 
       x = "Drink Category", y = "Calories") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/drinks_analysis/dunkin_drinks_calories_boxplot.png"), plot = graph_3)

