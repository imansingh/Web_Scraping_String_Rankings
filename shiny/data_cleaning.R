library(dplyr)
library(tidyr)
library(stringr)
library(DT)

summary(stringforum)

##Creating separate columns for racquet manufacturers and models

#Get racquet_names and racquet_specs from tester_racquet column
#specs are in parentheses at end, so will split on last opening parenthesis
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

string_data1 = mutate(string_data, 
       'racquet_manufacturer' = racquet_manufacturers, 
       "racquet_model" = racquet_models,
       "adjusted_price" = price_adjusted)

unique(racquet_manufacturers)
unique(racquet_models)



##Convert  prices to dollars, find average price, and create column for adjusted price

exchange_rate = 1.20 #modify this according to current rate
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



##
#Gather data from tester_style field and separate into columns for tester_gender, 
#tester_age, tester_level, tester_strokes, tester_spin, tester_style
tester_info_full = as.character(string_data$tester_style)
tester_info_split = strsplit(gsub(',', '', tester_styles_full), " ")

#identify 
is_male = function(vector){
  return('male' %in% vector)
}
is_female = function(vector){
  return('female' %in% vector)
}
is_boy = function(vector){
  return('boys' %in% vector)
}
is_girl = function(vector){
  return('girls' %in% vector)
}

gender[sapply(tester_styles_trimmed, is_male)] = 'male'
gender[sapply(tester_styles_trimmed, is_female)] = 'female'
gender[sapply(tester_styles_trimmed, is_boy)] = 'boy'
gender[sapply(tester_styles_trimmed, is_girl)] = 'girl'

table(tester_styles_trimmed)
unique(tester_styles_trimmed)
all_words = merge(tester_styles_trimmed)
all_words = unlist(tester_styles_trimmed)
table(all_words)

all_words
gender
sapply(tester_styles_trimmed, is_boy)




tester_styles_split = strsplit(tester_styles_full, ', ')
tester_styles_split
unique(strsplit(tester_styles_split[[1]], ' '))
split_first_vector = function(vector){
  return(strsplit(vector[1], ' '))
}
get_first_element = function(vector){
  return(vector[1])
}
first_list = sapply(tester_styles_split, split_first_vector)
first_elements = sapply(first_list, get_first_element)
first_elements
unique(first_elements)
first_list[[1]][1]
first_list
gender = c()

is_male = function(vector){
  return('male' %in% vector)
}
gender[sapply(first_list, is_male)] = 'male'
gender
unique(first_list)
for(x in length(first_list)){
  if('male' %in% first_list[[x]]){
    return*'male')
  }
}
for(x in length(first_list)){
  if('male' %in% first_list[[x]]){
    gender[x] = 'male'
  }
}
gender[1] = 'male'
gender

length(first_list)
tester_styles_split[[1]][1]
'male' %in% tester_styles_split[[1]][1]
x=1
if('male' %in% first_list[[x]]){
  gender[x] = 'male'
}

split_vector = function(vector){
    return(strsplit(vector, ' '))
}
sapply(tester_styles_split[[1]], split_vector)

tester_styles_split[[1000]][2]

strsplit(tester_styles_full[1])
sapply(tester_styles_split, length)
tester_styles_split[947]



string_data1 = mutate(string_data, 
                      'racquet_manufacturer' = racquet_manufacturers, 
                      "racquet_model" = racquet_models,
                      "adjusted_price" = price_adjusted)




#creating grouped dataframes
string_grouped = stringforum %> group_by(string_name)
racquet_grouped = stringforum %>% group_by(tester_racquet)
tester_grouped = stringforum %>% group_by(tester_name)

#creating dataframes with mean values per group
string_means = string_grouped %>% summarise(num_reviews = n(), mean_comfort = mean(comfort, na.rm=TRUE), mean_control = mean(control, na.rm=TRUE), mean_durability = mean(durability, na.rm=TRUE), mean_feel = mean(feel, na.rm=TRUE), mean_power = mean(power, na.rm=TRUE), mean_spin = mean(spin, na.rm=TRUE), mean_tension_stability = mean(tension_stability, na.rm=TRUE), mean_tester_satisfaction = mean(tester_satisfaction, na.rm=TRUE))
tester_means = tester_grouped %>% summarise(num_reviews = n(), mean_comfort = mean(comfort, na.rm=TRUE), mean_control = mean(control, na.rm=TRUE), mean_durability = mean(durability, na.rm=TRUE), mean_feel = mean(feel, na.rm=TRUE), mean_power = mean(power, na.rm=TRUE), mean_spin = mean(spin, na.rm=TRUE), mean_tension_stability = mean(tension_stability, na.rm=TRUE), mean_tester_satisfaction = mean(tester_satisfaction, na.rm=TRUE))
racquet_means = racquet_grouped %>% summarise(num_reviews = n(), mean_comfort = mean(comfort, na.rm=TRUE), mean_control = mean(control, na.rm=TRUE), mean_durability = mean(durability, na.rm=TRUE), mean_feel = mean(feel, na.rm=TRUE), mean_power = mean(power, na.rm=TRUE), mean_spin = mean(spin, na.rm=TRUE), mean_tension_stability = mean(tension_stability, na.rm=TRUE), mean_tester_satisfaction = mean(tester_satisfaction, na.rm=TRUE))

#cleaning up the grouped means - removing meaningless/generic racquet types and anonymous reviews
tester_means_clean = tester_means %>% filter(str_detect(tester_name, 'anonymous') == FALSE)
racquet_means_clean = racquet_means %>% slice(30:1382)

#calculate weighted means
string_means_grouped = string_means %>% group_by(string_name)
string_mean = 5
string_means_weighted = string_means %>% transmute(string_name, num_reviews, w_mean_comfort = mean_comfort * string_mean, w_mean_control = mean_control * string_mean, w_mean_durability = mean_durability * string_mean, w_mean_feel = mean_feel * string_mean, w_mean_power = mean_power * string_mean, w_mean_spin = mean_spin * string_mean, w_mean_tension_stability = mean_tension_stability * string_mean, w_mean_tester_satisfaction = mean_tester_satisfaction * string_mean,
                                                   sum_weights = (w_mean_comfort+ w_mean_control + w_mean_durability + w_mean_feel + w_mean_power + w_mean_spin + w_mean_tension_stability + w_mean_tester_satisfaction))

c(2:9)
string_weighted_arranged = string_means_weighted %>% arrange(desc(sum_weights))

string_means_weighted_2 = string_means %>% arrange(desc(mean_comfort * string_mean + mean_control * string_mean + mean_durability * string_mean + mean_feel * string_mean + mean_power * string_mean + mean_spin * string_mean + mean_tension_stability * string_mean + mean_tester_satisfaction * string_mean))
                                                                             
datatable(string_means_weighted_2, rownames=TRUE)

sum_weights = (w_mean_comfort+ w_mean_control + w_mean_durability + w_mean_feel + w_mean_power + w_mean_spin + w_mean_tension_stability + w_mean_tester_satisfaction))


string_means_arranged = string_means[order(match(string_means[,1],string_weighted_arranged[,1]))]


string_means_seleced = string_means %>% filter(num_reviews >= string_mean)


sum_weights = summarise(sum(w_mean_comfort, w_mean_control, w_mean_durability, w_mean_feel, w_mean_power, w_mean_spin, w_mean_tension_stability, w_mean_tester_satisfaction)))

selected_racquet = "Wilson KBlade 98 (MP, 18/20)"

racquet_selected <- racquet_means_clean %>% filter(tester_racquet == selected_racquet)

datatable(racquet_selected)

racquet_reviews = stringforum %>% select(string_name, price, tester_name, tester_tension, tester_style, comfort, control, durability, feel, power, spin, tension_stability, tester_satisfaction, review_text, review_adjectives, tester_racquet)

# move all price values after hypens to a new column, and remove hyphens

# convert prices in euros to dollars

# find average price when there was a range listed


# adding missing latitude and longitude for row 29826
solar[29826,30] = "Three Mile Bay, NY 13693\r(44.0814, -76.1983)"

# converting 'Reporting Period' to date object
solar$`Reporting Period` = as.Date(solar$`Reporting Period`, "%m/%d/%y")

# converting 'Date Appliation Received' to date object
solar$`Date Application Received` = as.Date(solar$`Date Application Received`, "%m/%d/%y")

# converting 'Date Completed' to date object
solar$`Date Completed` = as.Date(solar$`Date Completed`, "%m/%d/%y")

#removing '$' from $Incentives column name
solar = rename(solar, Incentive = `$Incentives`)

#removing '$' from values in Incentives column, and changing them to numeric
solar$'Incentive' <- as.numeric(gsub('\\$|,', '', solar$'Incentive'))

#removing '$' from values in Project Cost column, and changing them to numeric
solar$'Project Cost' <- as.numeric(gsub('\\$|,', '', solar$'Project Cost'))

#separating 'location 1' field into Location [city, state zip] and Coordinates (latitude, longitude)
solar = separate(solar, 'Location 1', into = c('Location', 'Coordinates'), sep="\\(")

#removing '$' from values in Project Cost column, and changing them to numeric
solar$'Project Cost' <- as.numeric(gsub('\\$|,', '', solar$'Project Cost'))

#removing closing parentheses from Coordinates column
solar$Coordinates <- gsub('\\)', '', solar$Coordinates)

#replacing 'comma space' with colon in Coordinates column
solar$Coordinates <- gsub(', ', ':', solar$Coordinates)

#separating 'Coordinates' field into latitude and longitude - keep original in case I want to ever use GoogleVis
solar = separate(solar, 'Coordinates', into = c('Latitude', 'Longitude'), sep=":", remove = FALSE)

#change Latitude and Longitude columns to numeric
solar$Latitude = as.numeric(solar$Latitude)
solar$Longitude = as.numeric(solar$Longitude)

#rename all the variables with spaces
solar = rename(solar, Reporting.Period = `Reporting Period`, Project.Number = `Project Number`, Zip.Code = `Zip Code`, Program.Type = `Program Type`, Electric.Utility = `Electric Utility`)
solar = rename(solar, Purchase.Type = `Purchase Type`, Date.Application.Received = `Date Application Received`, Date.Completed = `Date Completed`, Project.Status = `Project Status`)                       
solar = rename(solar, Total.Inverter.Quantity = `Total Inverter Quantity`, Total.PV.Module.Quantity = `Total PV Module Quantity`, Project.Cost = `Project Cost`, Total.Nameplate.kW.DC = `Total Nameplate kW DC`)               
solar = rename(solar, Expected.KWh.Annual.Production = `Expected KWh Annual Production`)
solar = rename(solar, Primary.Inverter.Manufacturer = `Primary Inverter Manufacturer`, Primary.PV.Module.Manufacturer = `Primary PV Module Manufacturer`)

#trim columns that are not needed
solar = select(solar, -X, -State, -Solicitation, -Primary.Inverter.Model.Number)
solar = select(solar, -PV.Module.Model.Number, -Location, -Coordinates)

#add columns derived from data
solar = mutate(solar, Days.To.Complete = Date.Completed - Date.Application.Received)
solar = mutate(solar, Net.Cost = Project.Cost - Incentive)
solar = mutate(solar, Year.Completed=format(Date.Completed, '%Y'), Year.Applied=format(Date.Application.Received, '%Y'))
solar = mutate(solar, Incentive.Per.Annual.KW = Incentive / Expected.KWh.Annual.Production)
solar = mutate(solar, Total.Cost.Per.Annual.KW = Project.Cost / Expected.KWh.Annual.Production)
solar = mutate(solar, Net.Cost.Per.Annual.KW = Net.Cost / Expected.KWh.Annual.Production)


#removing negative and zero 'days to complete', assuming these values are meaningless for analysis
solar$Days.To.Complete[solar$Days.To.Complete <= 0] = NA
