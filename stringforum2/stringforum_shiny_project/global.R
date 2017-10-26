library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(shiny)
library(DT)
library(shinydashboard)

string_data <- read.csv(file = "./stringforum.csv")

#creating grouped dataframes
string_grouped = string_data %>% group_by(string_name)
racquet_grouped = string_data %>% group_by(tester_racquet)
tester_grouped = string_data %>% group_by(tester_name)

#creating dataframes with mean values per group
string_means = string_grouped %>% summarise(reviews = n(), comfort = mean(comfort, na.rm=TRUE), control = mean(control, na.rm=TRUE), durability = mean(durability, na.rm=TRUE), feel = mean(feel, na.rm=TRUE), power = mean(power, na.rm=TRUE), spin = mean(spin, na.rm=TRUE), tension_stab = mean(tension_stability, na.rm=TRUE), satisfaction = mean(tester_satisfaction, na.rm=TRUE))
tester_means = tester_grouped %>% summarise(reviews = n(), comfort = mean(comfort, na.rm=TRUE), control = mean(control, na.rm=TRUE), durability = mean(durability, na.rm=TRUE), feel = mean(feel, na.rm=TRUE), power = mean(power, na.rm=TRUE), spin = mean(spin, na.rm=TRUE), tension_stab = mean(tension_stability, na.rm=TRUE), satisfaction = mean(tester_satisfaction, na.rm=TRUE))
racquet_means = racquet_grouped %>% summarise(reviews = n(), comfort = mean(comfort, na.rm=TRUE), control = mean(control, na.rm=TRUE), durability = mean(durability, na.rm=TRUE), feel = mean(feel, na.rm=TRUE), power = mean(power, na.rm=TRUE), spin = mean(spin, na.rm=TRUE), tension_stab = mean(tension_stability, na.rm=TRUE), satisfaction = mean(tester_satisfaction, na.rm=TRUE))

#cleaning up the grouped means - removing meaningless/generic racquet types and anonymous reviews
tester_means_clean = tester_means %>% filter(str_detect(tester_name, 'anonymous') == FALSE)
racquet_means_clean = racquet_means %>% slice(30:1382)

racquet_reviews = stringforum %>% select(string_name, price, tester_name, tester_tension, tester_style, review_text, review_adjectives, comfort, control, durability, feel, power, spin, tension_stability, tester_satisfaction, tester_racquet)
tester_reviews = stringforum %>% select(string_name, price, tester_racquet, tester_tension, review_text, review_adjectives, comfort, control, durability, feel, power, spin, tension_stability, tester_satisfaction, tester_style, tester_name)