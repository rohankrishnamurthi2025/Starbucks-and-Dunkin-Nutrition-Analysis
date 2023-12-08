##DRINKS ANALYSIS
#Load packages
library(magick)
library(tidyverse)
library(png)
library(kableExtra)

# A1. Healthiest Drinks: lowest sugar, calories, fat, etc.
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


# A2. Healthiest Drinks: highest protein, calcium, etc.
## i. Starbucks
graph_1 <- starbucks_drinks_data |> 
  ggplot(aes(x = beverage_category, y = protein_g)) + 
  geom_boxplot() + 
  labs(title = "Boxplot of grams of protein by drink type, Starbucks", 
       x = "Drink Category", y = "Protein (g)") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/drinks_analysis/starbucks_drinks_protein_boxplot.png"), plot = graph_1)


graph_2 <- starbucks_drinks_data |> 
  ggplot(aes(x = vitamin_a_percent_dv, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = beverage_category)) + 
  labs(title = "Frequency Polygon of Vitamin A (%DV) by beverage type, Starbucks", 
       x = "Vitamin A (%DV)", y = "Density") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/drinks_analysis/starbucks_drinks_vitamin_a_freqpoly.png"), plot = graph_2)

graph_3 <- starbucks_drinks_data |> 
  ggplot(aes(x = vitamin_c_percent_dv, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = beverage_category)) + 
  labs(title = "Frequency Polygon of Vitamin C (%DV) by beverage type, Starbucks", 
       x = "Vitamin C (%DV)", y = "Density") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/drinks_analysis/starbucks_drinks_vitamin_c_freqpoly.png"), plot = graph_3)

graph_4 <- starbucks_drinks_data |> 
  ggplot(aes(x = calcium_percent_dv, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = beverage_category)) + 
  labs(title = "Frequency Polygon of Calcium (%DV) by beverage type, Starbucks", 
       x = "Calcium (%DV)", y = "Density") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/drinks_analysis/starbucks_drinks_calcium_freqpoly.png"), plot = graph_4)


starbucks_table_2 <- starbucks_drinks_data |> 
  group_by(beverage_category) |> 
  summarize(protein = round(mean(protein_g, na.rm = TRUE)),
            calcium = round(mean(calcium_percent_dv, na.rm = TRUE)),
            vitamin_a = round(mean(vitamin_a_percent_dv, na.rm = TRUE)),
            vitamin_c = round(mean(vitamin_c_percent_dv, na.rm = TRUE))) |> 
  rename("Average protein content (g)" = protein, 
         "Average calcium (%DV)" = calcium,
         "Average vitamin A (%DV)" = vitamin_a,
         "Average vitamin C (%DV)" = vitamin_c,
         "Drink Category" = beverage_category)
kable(starbucks_table_2, caption = "Starbucks Drinks Nutritional Info, continued")

## ii. Dunkin Donuts
graph_1 <- dunkin_donuts_drinks_data |> 
  ggplot(aes(x = category, y = protein_g)) + 
  geom_boxplot() + 
  labs(title = "Boxplot of protein (g) by drink type, Dunkin Donuts", 
       x = "Drink Category", y = "Protein (g)") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/drinks_analysis/dunkin_drinks_protein_boxplot.png"), plot = graph_1)

dunkin_table_2 <- dunkin_donuts_drinks_data |> 
  group_by(category) |> 
  summarize(protein = round(mean(protein_g, na.rm = TRUE))) |> 
  rename("Average protein content (g)" = protein,
         "Drink Category" = category)
kable(starbucks_table_2, caption = "Dunkin Donuts Drinks Nutritional Info, continued")

# B: Nutrition by Milk Type
## i. Starbucks

starbucks_milk_table <- starbucks_drinks_data |> 
  group_by(milk_type) |> 
  summarize(protein = round(mean(protein_g, na.rm = TRUE)),
            calories = round(mean(calories, na.rm = TRUE)),
            sugar = round(mean(sugars_g, na.rm = TRUE)),
            fat = round(mean(fat_g, na.rm = TRUE))) |> 
  rename("Protein content (g)" = protein, 
         "Sugar content (g)" = sugar,
         "Calories" = calories,
         "Fat (g)" = fat,
         "Milk Type" = milk_type)
kable(starbucks_milk_table, caption = "Starbucks Drinks Average Nutritional Info by Type of Milk")

graph_8 <- starbucks_drinks_data |> 
  filter(is.na(milk_type) == FALSE) |> 
  ggplot(aes(x = milk_type, y = fat_g)) + 
  geom_boxplot() + 
  labs(title = "Boxplot of grams of fat by milk type, Starbucks", 
       x = "Milk Type", y = "Fat (g)") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/drinks_analysis/starbucks_milk_fat_boxplot.png"), plot = graph_8)

graph_9 <- starbucks_drinks_data |> 
  filter(is.na(milk_type) == FALSE) |> 
  ggplot(aes(x = milk_type, y = sugars_g)) + 
  geom_boxplot() + 
  labs(title = "Boxplot of grams of sugar by milk type, Starbucks", 
       x = "Milk Type", y = "Sugar (g)") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/drinks_analysis/starbucks_milk_sugar_boxplot.png"), plot = graph_9)


graph_10 <- starbucks_drinks_data |> 
  filter(is.na(milk_type) == FALSE) |> 
  ggplot(aes(x = milk_type, y = calories)) + 
  geom_boxplot() + 
  labs(title = "Boxplot of calories by milk type, Starbucks", 
       x = "Milk Type", y = "Calories") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/drinks_analysis/starbucks_milk_calories_boxplot.png"), plot = graph_10)

graph_11 <- starbucks_drinks_data |> 
  filter(is.na(milk_type) == FALSE) |> 
  ggplot(aes(x = milk_type, y = protein_g)) + 
  geom_boxplot() + 
  labs(title = "Boxplot of protein by milk type, Starbucks", 
       x = "Milk Type", y = "Protein") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/drinks_analysis/starbucks_milk_protein_boxplot.png"), plot = graph_11)



## ii. Dunkin Donuts
dunkin_milk_table <- dunkin_donuts_drinks_data |> 
  filter(is.na(milk_type) == FALSE) |> 
  group_by(milk_type) |> 
  summarize(protein = round(mean(protein_g, na.rm = TRUE)),
            calories = round(mean(calories, na.rm = TRUE)),
            sugar = round(mean(sugars_g, na.rm = TRUE)),
            fat = round(mean(fat_g, na.rm = TRUE))) |> 
  rename("Protein content (g)" = protein, 
         "Sugar content (g)" = sugar,
         "Calories" = calories,
         "Fat (g)" = fat,
         "Milk Type" = milk_type)
kable(dunkin_milk_table, caption = "Dunkin Donuts Drinks Average Nutritional Info by Type of Milk")

graph_1 <- dunkin_donuts_drinks_data |> 
  filter(is.na(milk_type) == FALSE) |> 
  ggplot(aes(x = milk_type, y = protein_g)) + 
  geom_boxplot() + 
  labs(title = "Boxplot of protein (g) by milk type, Dunkin Donuts", 
       x = "Milk Type", y = "Protein") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/drinks_analysis/dunkin_milk_protein_boxplot.png"), plot = graph_1)

graph_2 <- dunkin_donuts_drinks_data |> 
  filter(is.na(milk_type) == FALSE) |> 
  ggplot(aes(x = milk_type, y = calories)) + 
  geom_boxplot() + 
  labs(title = "Boxplot of calories milk type, Dunkin Donuts", 
       x = "Milk Type", y = "Calories") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/drinks_analysis/dunkin_milk_calories_boxplot.png"), plot = graph_2)

graph_3 <- dunkin_donuts_drinks_data |> 
  filter(is.na(milk_type) == FALSE) |> 
  ggplot(aes(x = milk_type, y = sugars_g)) + 
  geom_boxplot() + 
  labs(title = "Boxplot of sugars (g) by milk type, Dunkin Donuts", 
       x = "Milk Type", y = "Sugars (g)") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/drinks_analysis/dunkin_milk_sugar_boxplot.png"), plot = graph_3)

graph_4 <- dunkin_donuts_drinks_data |> 
  filter(is.na(milk_type) == FALSE) |> 
  ggplot(aes(x = milk_type, y = fat_g)) + 
  geom_boxplot() + 
  labs(title = "Boxplot of fat (g) by milk type, Dunkin Donuts", 
       x = "Milk Type", y = "Fat (g)") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/drinks_analysis/dunkin_milk_fat_boxplot.png"), plot = graph_4)

graph_5 <- dunkin_donuts_drinks_data |> 
  filter(is.na(milk_type) == FALSE) |> 
  ggplot(aes(x = protein_g, y = ..density..)) + 
  geom_density(mapping = aes(colour = milk_type)) + 
  labs(title = "Density Plot of protein (g) by milk type, Dunkin Donuts", 
       x = "Protein (g)", y = "Density") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/drinks_analysis/dunkin_milk_protein_density.png"), plot = graph_5)

graph_6 <- dunkin_donuts_drinks_data |> 
  filter(is.na(milk_type) == FALSE) |> 
  ggplot(aes(x = sugars_g, y = ..density..)) + 
  geom_density(mapping = aes(colour = milk_type)) + 
  labs(title = "Density Plot of sugars (g) by milk type, Dunkin Donuts", 
       x = "Sugars (g)", y = "Density") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/drinks_analysis/dunkin_milk_sugar_density.png"), plot = graph_6)

graph_7 <- dunkin_donuts_drinks_data |> 
  filter(is.na(milk_type) == FALSE) |> 
  ggplot(aes(x = fat_g, y = ..density..)) + 
  geom_density(mapping = aes(colour = milk_type)) + 
  labs(title = "Density Plot of fat (g) by milk type, Dunkin Donuts", 
       x = "Fat (g)", y = "Density") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/drinks_analysis/dunkin_milk_fat_density.png"), plot = graph_7)

graph_8 <- dunkin_donuts_drinks_data |> 
  filter(is.na(milk_type) == FALSE) |> 
  ggplot(aes(x = calories, y = ..density..)) + 
  geom_density(mapping = aes(colour = milk_type)) + 
  labs(title = "Density Plot of calories by milk type, Dunkin Donuts", 
       x = "Calories", y = "Density") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/drinks_analysis/dunkin_milk_calories_density.png"), plot = graph_8)


# C: New metrics
#C1: Protein to calories ratio

## i: Starbucks
graph_1 <- starbucks_drinks_data |> 
  ggplot(aes(x = calories, y = protein_g)) + 
  geom_point() + geom_smooth(method='lm', formula= y~x) + 
  facet_wrap(vars(beverage_category)) +
  labs(title = "Faceted scatterplots comparing calories to protein content (g) by beverage type, Starbucks ", 
       x = "Calories", y = "Protein (g)") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/drinks_analysis/starbucks_drinks_protein_to_calories_faceted.png"), plot = graph_1)

graph_2 <- starbucks_drinks_data |> 
  ggplot(aes(x = calories, y = protein_g)) + 
  geom_point(aes(color = beverage_category))+
  labs(title = "Scatterplot comparing calories to protein content (g) by beverage type, Starbucks ", 
       x = "Calories", y = "Protein (g)") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/drinks_analysis/starbucks_drinks_protein_to_calories.png"), plot = graph_2)


starbucks_protein_table <- starbucks_drinks_data |> 
  group_by(beverage_category) |> 
  summarize(protein = round(mean(protein_g, na.rm = TRUE)),
            calories = round(mean(calories, na.rm = TRUE)),
            ratio = round(protein/calories, digits = 4)) |> 
  rename("Protein content (g)" = protein, 
         "Calories" = calories,
         "Ratio of protein (g) to calories" = ratio,
         "Drink Category" = beverage_category)
kable(starbucks_protein_table, caption = "Protein to Calories, Starbucks ")

## i: Dunkin
dunkin_protein_table <- dunkin_donuts_drinks_data |> 
  group_by(category) |> 
  summarize(protein = round(mean(protein_g, na.rm = TRUE)),
            calories = round(mean(calories, na.rm = TRUE)),
            ratio = round(protein/calories, digits = 4)) |> 
  rename("Protein content (g)" = protein, 
         "Calories" = calories,
         "Ratio of protein (g) to calories" = ratio,
         "Drink Category" = category)
kable(starbucks_protein_table, caption = "Protein to Calories, Dunkin ")

graph_3 <- dunkin_donuts_drinks_data |> 
  ggplot(aes(x = calories, y = protein_g)) + 
  geom_point() + geom_smooth(method='lm', formula= y~x) + 
  facet_wrap(vars(category)) +
  labs(title = "Faceted scatterplots comparing calories to protein content (g) by beverage type, Dunkin ", 
       x = "Calories", y = "Protein (g)") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/drinks_analysis/dunkin_drinks_protein_to_calories_faceted.png"), plot = graph_3)

graph_4 <- dunkin_donuts_drinks_data |> 
  ggplot(aes(x = calories, y = protein_g)) + 
  geom_point(aes(color = category)) +
  labs(title = "Scatterplot comparing calories to protein content (g) by beverage type, Dunkin ", 
       x = "Calories", y = "Protein (g)") +
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = ("figures/drinks_analysis/dunkin_drinks_protein_to_calories.png"), plot = graph_4)


#C2: highest vitamin content

