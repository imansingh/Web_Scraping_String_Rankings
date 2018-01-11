library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(shiny)
library(DT)
library(shinydashboard)

string_data <- read.csv(file = "./stringforum.csv")

##Create vectors for racquet_manufacturers and racquet_models

#Get racquet_names and racquet_specs from tester_racquet column
#specs are inside last set of parentheses, so split on last opening parenthesis
racquet_names_full = as.character(string_data$tester_racquet)
paren_indexes = gregexpr("\\(", racquet_names_full)
get_last_paren_index = function(vector){
  return(tail(vector, n=1))
}
last_paren_indexes = sapply(paren_indexes, get_last_paren_index)
racquet_names = substr(racquet_names_full, 1, last_paren_indexes-2)
racquet_specs = substr(racquet_names_full, last_paren_indexes+1, nchar(racquet_names_full)-1)

#Get racquet_manufacturer and racquet_model from racquet_names
racquet_names_split = strsplit(racquet_names, ' ')
get_manufacturer = function(vector){
  if(vector[1] == 'Pro' | vector[1] == "Pro's" | vector[1] == 'The' | 
     vector[1] == 'Boris'){
    return(paste(vector[1:2], collapse = ' '))
  }
  else{
    return(vector[1])
  }
}
get_model = function(vector){
  if(vector[1] == 'Pro' | vector[1] == "Pro's" | vector[1] == 'The' | 
     vector[1] == 'Boris'){
    return(paste(vector[-1:-2], collapse = ' '))
  }
  else{
    return(paste(vector[-1], collapse = ' '))
  }
}
racquet_manufacturers = sapply(racquet_names_split, get_manufacturer)
racquet_models = sapply(racquet_names_split, get_model)

##Create vector for adjusted_price
#Specify Exchange Rate
exchange_rate = 1.20    #modify this according to current rate

#Compute adjusted_price by stripping out currency symbols, converting to 
#dollars, and finding average if price range given
price_full = as.character(string_data$price)
price_split = strsplit(price_full, '-')
get_prices = function(vector){
  if(substr(vector[1],1,1) == '$'){
    if(length(vector) > 1){
      return((as.numeric(gsub('\\$|(\u20AC)', '', vector[1])) +
                as.numeric(gsub('\\$|(\u20AC)', '', vector[2]))) / 2)
    }
    else{
      return(as.numeric(gsub('\\$|(\u20AC)', '', vector)))
    }
  }
  if(substr(vector[1],1,1) == '\u20AC'){
    if(length(vector) > 1){
      return(exchange_rate * ((as.numeric(gsub('\\$|(\u20AC)', '', vector[1])) +
                                 as.numeric(gsub('\\$|(\u20AC)', '', vector[2]))) / 2))
    }
    else{
      return(exchange_rate * as.numeric(gsub('\\$|(\u20AC)', '', vector)))
    }
  }  
  if(length(vector) > 1){
    return((as.numeric(gsub('\\$|(\u20AC)', '', vector[1])) +
              as.numeric(gsub('\\$|(\u20AC)', '', vector[2]))) / 2)
  }
  else{
    return(as.numeric(gsub('\\$|(\u20AC)', '', vector)))
  }
}
price_adjusted = sapply(price_split, get_prices)


#Add racquet_manufacturer, racquet_model, and adjusted_price columns to string_data dataframe 
string_data = mutate(string_data, 
                     'racquet_manufacturer' = racquet_manufacturers,
                     'racquet_model' = racquet_models,
                     'adjusted_price' = price_adjusted)

#creating grouped dataframes by string_name, tester_racquet and tester_name
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

#dataframes with detailed review info
detailed_reviews = string_data %>% select(string_name, price, tester_name, tester_racquet, tester_tension, tester_style, review_text, review_adjectives, comfort, control, durability, feel, power, spin, tension_stability, tester_satisfaction)

genders = c('Male', 'Female')
