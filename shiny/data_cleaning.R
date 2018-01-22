library(dplyr)
library(tidyr)
library(stringr)
library(DT)

# Extract the following info from scraped stringforum data and add as columns 
# to string_data

# Tester Info: tester_gender, tester_age, tester_level, tester_strokes, 
             # tester_spin, tester_style
# Tester Racquet Info: racquet_manufacturer, racquet_model, frame_size, 
                     # string_pattern
# Tester Racquet Tension: main_tension, cross_tension
# String Info: string_material, string_construction, string_features
# String Gauge: string_gauge_metric, string_gauge_us
# Price: price_adjusted
# Review Adjectives: review_adjectives_split

string_criteria_filtered1 <- string_data1 %>% 
  filter(num_ratings >= 25) %>%
  filter(adjusted_price >= 5) %>%
  filter(adjusted_price <= 25) %>%
  filter(string_gauge_metric >= .01) %>%
  filter(string_gauge_metric <= 500) %>%
  filter(string_gauge_us >= 16) %>%
  filter(string_gauge_us <= 19)
string_criteria_filtered1
test_filtered = filter(string_data1, grepl(vec, review_adjectives))
test_filtered

vec = c('Aramid', 'Polyester', 'Polyamid', 'Natural Gut', 'Polyethylene', 'Polyurethane', 'Zyex')
vec1 = c('Aramid', 'Polyester', 'Polyamid', 'None Listed')


matrix = sapply(vec,
                function(string)
                  grepl(string,
                        string_data1$string_material))

matrix

string_material_filtered = string_data1[rowSums(matrix) > 0,]
string_material_filtered

vec = c('soft', 'comfortable')
vec = NULL
if(!is.null(vec)){
  matrix = sapply(vec, function(string) grepl(string, string_data1$review_adjectives))
  test_filtered1 = string_data1[rowSums(matrix) > 0,]
}

rowSums(matrix)
string_data1$review_adjectives[vec %in% string_data1$review_adjectives]

test1 = string_data1$string_material[[!(identical(string_data1$string_material, character(0)))]]
test1 = sapply(string_data1$string_material, function(vec) !identical(vec, character(0)))
sum(test1)
identical(test1[[3]], character(0))
identical(string_data1$string_material[[3]], character(0))
test_filtered

models_by_manufacturer
## Replace string vectors with single strings
string_data1$review_adjectives
review_adjectives_combined = review_adjectives_full

string_material_combined = sapply(string_material, function(vec) paste(vec, collapse = ', '))
string_construction_combined = sapply(string_construction, function(vec) paste(vec, collapse = ', '))
string_features_combined = sapply(string_features, function(vec) paste(vec, collapse = ', '))



## Created nested list of racquet models by manufacturer
models_by_manufacturer = list()
for(manufacturer in unique(string_data1$racquet_manufacturer[
  string_data1$racquet_manufacturer != ''])){
  models_by_manufacturer[[manufacturer]] = 
    sort(unique(string_data1$racquet_model[
      string_data1$racquet_manufacturer == manufacturer]))
}
models_by_manufacturer
names(models_by_manufacturer)
sort(unique(string_data1$racquet_manufacturer))
list(Eastern = c('NYC', 'PBJ'), Western = c('JOK', 'ADA'))
models_by_manufacturer
get_models_by_manufacturer = function(string){
  list = c()
  manufacturers = unique(string_data1[[string]])
  for(i in manufacturers[manufacturers != '']){
    assign(i, string_data1$racquet_model[string_data1$racquet_manufacturer == i])
    list = c(list, i)
  }
  return(list)
}

get_models_by_manufacturer = function(string){
  list = c()
  manufacturers = unique(string_data1[[string]])
  for(i in manufacturers[manufacturers != '']){
    assign(i, string_data1$racquet_model[string_data1$racquet_manufacturer == i])
    list = c(list, i)
  }
  return(list)
}

names(models_by_manufacturer)[[1]]
#string_data1$racquet_model[string_data1$racquet_manufacturer == 'Prince']
models2 = get_models_by_manufacturer('racquet_manufacturer')
models2[2]
#Prince
man = unique(string_data1[["racquet_manufacturer"]])
man[man != '']

na.omit(unique(string_data1$tester_gender))
for(manufacturer in unique(string_data$racquet_manufacturer)){
  models_by_manufacturer[[manufacturer]] = 
    string_data1$racquet_model[string_data1$racquet_manufacturer == manufacturer]
}
#assign(names(models_by_manufacturer)[1], models_by_manufacturer[names(models_by_manufacturer)[1]])

typeof(models_by_manufacturer)[[1]]
typeof(models_by_manufacturer[1])
names(models_by_manufacturer)
models_by_manufacturer[4]
model1[1]
names(model1)[1] = model1[names(model1)[1]]
rm(`names(model1)[1]`)
names(model1)
Babolat
string_data1$racquet_model['Head']
unique(string_data1$racquet_manufacturer)

manufacturer_grouped = string_data1 %>% group_by(racquet_manufacturer)
manufacturer_grouped
model1 = models_by_manufacturer

vec_models_by_manufacturer = unlist(models_by_manufacturer, recursive = TRUE, use.names = TRUE)
vec_models_by_manufacturer
c(unique(unlist(string_data1$string_material)), 'None Selected')

unlist(string_data1$review_adjectives)

col_name = paste0('string_data1$', 'string_material')
col_name
c(unique(unlist(as.name(col_name))), 'None Listed')
string_data1[['string_gauge_metric']]
col_name1 = 'string_material'
attach(string_data1)
detach()
as.name(col_name1)
string_material
unlist(as.name(col_name))
col_name
as.name(col_name)
parse(text = 'do')

unique(unlist(string_material))
unique(string_gauge_metric)
table(string_gauge)
table(string_data1$main_tension)
table(racquet_manufacturer)
string_data$racquet_manufacturer
min(string_data$num_ratings)
max(string_data$num_ratings)
min(string_data1$price_adjusted, na.rm = TRUE)
max(string_data1$price_adjusted)


print('Midsize (600 cm\u00B2)')
# read scraped data into dataframe
string_data <- read.csv(file = "./stringforum.csv")

## Tester Info
# Gather data from tester_style field and extract tester_gender, tester_age, 
# tester_level, tester_strokes, tester_spin, tester_style
tester_info_full = as.character(string_data$tester_style)
tester_info_split = strsplit(gsub(',', '', tester_styles_full), " ")

# tester_gender
# create functions to identify males vs females
is_male = function(vector){
  return('male' %in% vector | 'boys' %in% vector)
}
is_female = function(vector){
  return('female' %in% vector | 'girls' %in% vector)
}

# create vector that labels males and females
tester_gender = c()
tester_gender[sapply(tester_info_split, is_male)] = 'male'
tester_gender[sapply(tester_info_split, is_female)] = 'female'


# tester_age
# create functions to identify juniors vs adult vs young senior vs senior
is_junior = function(vector){
  return('boys' %in% vector | 'girls' %in% vector)
}
is_adult = function(vector){
  return(('male' %in% vector | 'female' %in% vector) & 
           !('girls' %in% vector | 'boys' %in% vector | 'seniors' %in% vector))
}
is_young_senior = function(vector){
  return('young' %in% vector)
}
is_senior = function(vector){
  return('seniors' %in% vector & !'young' %in% vector)
}

# create vector that labels juniors, adults, young seniors and seniors
tester_age = c()
tester_age[sapply(tester_info_split, is_junior)] = 'junior'
tester_age[sapply(tester_info_split, is_adult)] = 'adult'
tester_age[sapply(tester_info_split, is_young_senior)] = 'young_senior'
tester_age[sapply(tester_info_split, is_senior)] = 'senior'


# tester_level
# create functions to identify beginner vs recreational vs lower league vs
# upper league vs national tournament vs international tournament level
is_beginner = function(vector){
  return('beginner' %in% vector)
}
is_recreational = function(vector){
  return('recreational' %in% vector)
}
is_lower_league = function(vector){
  return('lower' %in% vector)
}
is_upper_league = function(vector){
  return('upper' %in% vector)
}
is_national_tournament = function(vector){
  return('national' %in% vector)
}
is_international_tournament = function(vector){
  return('international' %in% vector)
}

# create vector that labels beginner, recreational, lower league,
# upper league, national tournament, international tournament level
tester_level = c()
tester_level[sapply(tester_info_split, is_beginner)] = 'beginner'
tester_level[sapply(tester_info_split, is_recreational)] = 'recreational'
tester_level[sapply(tester_info_split, is_lower_league)] = 'lower_league'
tester_level[sapply(tester_info_split, is_upper_league)] = 'upper_league'
tester_level[sapply(tester_info_split, is_national_tournament)] = 
  'national_tournament'
tester_level[sapply(tester_info_split, is_international_tournament)] = 
  'international_tournament'

# tester_strokes
# create functions to identify hard vs medium vs soft strokes
is_hard_strokes = function(vector){
  return('hard' %in% vector)
}
is_medium_strokes = function(vector){
  return('medium' %in% vector)
}
is_soft_strokes = function(vector){
  return('soft' %in% vector)
}

# create vector that labels hard, medium, and soft strokes
tester_strokes = c()
tester_strokes[sapply(tester_info_split, is_hard_strokes)] = 'hard'
tester_strokes[sapply(tester_info_split, is_medium_strokes)] = 'medium'
tester_strokes[sapply(tester_info_split, is_soft_strokes)] = 'soft'


# tester_spin
# create functions to identify heavy vs moderate vs little spin
is_heavy_spin = function(vector){
  return('heavy' %in% vector)
}
is_moderate_spin = function(vector){
  return('moderate' %in% vector)
}
is_little_spin = function(vector){
  return('little' %in% vector)
}

# create vector that labels heavy, moderate, and little spin
tester_spin = c()
tester_spin[sapply(tester_info_split, is_heavy_spin)] = 'heavy'
tester_spin[sapply(tester_info_split, is_moderate_spin)] = 'moderate'
tester_spin[sapply(tester_info_split, is_little_spin)] = 'little'


# tester_style
# create functions to identify allround vs baseline defensive vs 
# baseline offensive vs serve & volley style
is_allround = function(vector){
  return('allround' %in% vector)
}
is_baseline_defensive = function(vector){
  return('defensive' %in% vector)
}
is_baseline_offensive = function(vector){
  return('offensive' %in% vector)
}
is_serve_volley = function(vector){
  return('serve' %in% vector)
}

# create vector that labels allround, baseline defensive, 
# baseline offensive , and serve & volley style
tester_style = c()
tester_style[sapply(tester_info_split, is_allround)] = 'allround'
tester_style[sapply(tester_info_split, is_baseline_defensive)] = 'baseline_defensive'
tester_style[sapply(tester_info_split, is_baseline_offensive)] = 'baseline_offensive'
tester_style[sapply(tester_info_split, is_serve_volley)] = 'serve & volley'
table(tester_style)


## Tester Racquet Info

# Create vector with racquet info and split racquet_names from racquet_specs 
# specs are in parentheses at end, so will split on last opening parenthesis
racquet_info_full = as.character(string_data$tester_racquet)
paren_indexes = gregexpr("\\(", racquet_info_full)
get_last_paren_index = function(vector){
    return(tail(vector, n=1))
}
last_paren_indexes = sapply(paren_indexes, get_last_paren_index)
racquet_names = substr(racquet_info_full, 1, last_paren_indexes-2)
racquet_specs = substr(racquet_info_full, last_paren_indexes+1, nchar(racquet_names_full)-1)

# Extract racquet_manufacturer and racquet_model from racquet_names
# Manufacturer is usually first word in racquet_names, with exceptions specified
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
racquet_manufacturer = sapply(racquet_names_split, get_manufacturer)
racquet_model = sapply(racquet_names_split, get_model)

#Extract frame_size, string_pattern, and is_widebody from racquet_specs
#split racquet_specs into one, two or three element list
racquet_specs_split = strsplit(racquet_specs, ", ")

#functions to extract frame size, string pattern and widebody info
get_frame_size = function(vector){
  if(!(is.na(vector[1]))){
    if(vector[1] == '0'){
      vector[1] = ''
      }
  }
  return(vector[1])
}
get_string_pattern = function(vector){
  if(!(is.na(vector[2]))){
    if(vector[2] == "WB"){
      return(vector[3])
    }
  } 
  return(vector[2])
}
is_widebody = function(vector){
  return(vector[2] == 'WB')
}

#create vectors to label tester racquets by frame_size, string_pattern and 
#is_widebody
frame_size = sapply(racquet_specs_split, get_frame_size)
string_pattern = sapply(racquet_specs_split, get_string_pattern)
is_widebody = sapply(racquet_specs_split, is_widebody)

## Tester Racquet Tension
# Extract main_tension and cross_tension from tester_tension
tension_full = as.character(string_data$tester_tension)
tension_trimmed = gsub(' lbs', '', tension_full)
tension_split = strsplit(tension_trimmed, '/')

main_tension = sapply(tension_split, function(vector) vector[1])
cross_tension = sapply(tension_split, function(vector) vector[2])

## String Info
# Extract string_material, string_construction and string_features from string_type
string_info_full = as.character(string_data$string_type)
string_info_split = strsplit(string_info_full, ', ')

# functions to extract string_material, string_construction and string_features
get_string_material = function(vector){
  material = vector[vector == 'Aramid' | vector == 'Polyester' | 
                             vector == 'Polyamid' | vector == 'Polyethylene' |
                             vector == 'Polyurethane' | vector == 'Zyex' |
                             vector == 'Natural Gut']
  return(material)
}
get_string_construction = function(vector){
  construction = vector[vector == 'Monofilament' | 
                                 vector == 'Central core with one wrap' |
                                 vector == 'Central core with one wrap' |
                                 vector == 'Multifilament' | 
                                 vector == 'Ribbon Construction']
  return(construction)
}
get_string_features = function(vector){
  features = vector[vector == 'Structured surface' | 
                      vector == 'Titanium Coating' | 
                      vector == 'Titanium Fibers' | vector == 'Hybrid String']
  return(features)
}
string_info_split

# creating vectors with string_material, string_construction_and string_features
string_material = sapply(string_info_split, get_string_material)
string_construction = sapply(string_info_split, get_string_construction)
string_features = sapply(string_info_split, get_string_features)

## String Gauge
# Extract string_gauge from string_name
string_names_full = as.character(string_data$string_name)
string_names_split = strsplit(string_names_full, ' ')
string_gauge = sapply(string_names_split, function(l) l[length(l)])

# convet US and metric measurements and create string_gauge_metric and 
# string_gauge_us

# label hybrids
label_hybrids = function(vector){
  if(vector[1] == 'N'){
    vector = ''
  } else if('~' %in% vector){
    vector = 'oblong'
  } else if('/' %in% vector) {
    vector = 'hybrid'
  } else{
    vector = vector
  }
  return(vector)
}
string_gauge_split = strsplit(string_gauge, '')
string_gauge_labeled = sapply(string_gauge_split, label_hybrids)
string_gauge_labeled
get_metric_gauge = function(vector){
  if('.' %in% vector){
    vector = as.numeric(paste(vector, collapse = ''))
  } else {
    vector = paste(vector, collapse = '')
    if(vector == '15'){
      vector = mean(c(1.41, 1.49))
    } else if(vector == '15L'){
      vector = mean(c(1.33, 1.41))
    } else if(vector == '16'){
      vector = mean(c(1.26, 1.34))
    } else if(vector == '16L'){
      vector = mean(c(1.22, 1.30))
    } else if(vector == '17' | vector == '17L'){
      vector = mean(c(1.16, 1.24))
    } else if(vector == '18' | vector == '18L'){
      vector = mean(c(1.06, 1.16))
    } else if(vector == '19' | vector == '19L'){
      vector = mean(c(0.9, 1.06))
    }
  }
  return(vector)
}
paste(string_gauge_labeled[[991]], collapse = '')
mean(c(1.41, 1.49))
1.48 %in% seq(1.41, 1.49)
seq(1.41, 1.49, .01)
seq(1,3)
string_gauge_metric = as.numeric(sapply(string_gauge_labeled, get_metric_gauge))
string_gauge_metric
table(string_gauge_metric)

get_us_gauge = function(vector){
  # if('.' %in% vector){
  #   metric = as.numeric(paste(vector, collapse = ''))
  #   if(metric %in% seq(1.34, 1.49, .01)){
  #     vector = 15
  #   } else if(metric %in% seq(1.23, 1.33, .01)){
  #     vector = 16
  #   } else if(metric %in% seq(1.16, 1.22, .01)){
  #     vector = 17
  #   } else if(metric %in% seq(1.06, 1.15, .01)){
  #     vector = 18
  #   } else if(metric %in% seq(.9, 1.05, .01)){
  #     vector = 19
  #   }
  if('.' %in% vector){
    metric = as.numeric(paste(vector, collapse = ''))
    if(metric >= 1.34 & metric <= 1.49){
      vector = 15
    } else if(metric >= 1.23 & metric <= 1.33){
      vector = 16
    } else if(metric >= 1.16 & metric <= 1.22){
      vector = 17
    } else if(metric >= 1.06 & metric <= 1.15){
      vector = 18
    } else if(metric >= .9 & metric <= 1.05){
      vector = 19
    }
  } else {
    vector = paste(vector, collapse = '')
    if(vector == '15' | vector == '15L'){
      vector = 15
    } else if(vector == '16' | vector == '16L'){
      vector = 16
    } else if(vector == '17' | vector == '17L'){
      vector = 17
    } else if(vector == '18' | vector == '18L'){
      vector = 18
    } else if(vector == '19' | vector == '19L'){
      vector = 19
    }
  }
  return(vector)
}
collapse = function(vector){
  gauge_string = paste(vector, collapse = '')
  return(gauge_string)
}
string_gauge_collapsed = sapply(string_gauge_labeled, collapse)
string_gauge_collapsed
summary(string_gauge_collapsed)

remove_l = function(string){
  if(string == '15' | string == '15L'){
    string = 15
  } else if(string == '16' | string == '16L'){
    string = 16
  } else if(string == '17' | string == '17L'){
    string = 17
  } else if(string == '18' | string == '18L'){
    string = 18
  }
  else if(string == '19' | string == '19L'){
    string = 16
  }
  return(string)
}

string_gauge_no_l = sapply(string_gauge_collapsed, remove_l)
string_gauge_no_l
summary(string_gauge_no_l)

get_us_gauge = function(string){
  gauge_num = as.numeric(string)
  if(!(is.na(gauge_num))){
    if(gauge_num >= 1.34 & gauge_num <= 1.49){
      gauge_num = 15
    } else if(gauge_num >= 1.23 & gauge_num < 1.34){
      gauge_num = 16
    } else if(gauge_num >= 1.16 & gauge_num < 1.23){
      gauge_num = 17
    } else if(gauge_num >= 1.06 & gauge_num < 1.16){
      gauge_num = 18
    } else if(gauge_num >= .9 & gauge_num < 1.06){
      gauge_num = 19
    }
  }
  return(gauge_num)
}

string_gauge_us = sapply(string_gauge_no_l, get_us_gauge)
string_gauge_us
as.numeric(string_gauge_no_l[7]) >= 1.16
table(string_gauge_us)
table(metric_gauge)
typeof(us_gauge)
summary(us_gauge)

  else if(gauge_string == '16' | gauge_string == '16L'){
    vector = 16
  } else if(gauge_string == '17' | gauge_string == '17L'){
    vector = 17
  } else if(gauge_string == '18' | gauge_string == '18L'){
    vector = 18
  } else if(gauge_string == '19' | gauge_string == '19L'){
    vector = 19
  }
  return(vector)
}


collapse = function(vector){
  gauge_string = paste(vector, collapse = '')
}
string_gauge_collapsed = sapply(string_gauge_labeled, collapse)
string_gauge_collapsed
string_gauge_collapsed_no_l = sapply(string_gauge_labeled, remove_l)
string_gauge_collapsed_no_l
paste(string_gauge_labeled[[1]], collapse = '')

string_gauge_collapsed = sapply(string_gauge_labeled, function(l) paste(l, collapse = ''))
string_gauge_collapsed
table(string_gauge_collapsed)

as.numeric('')

typeof(string_gauge_labeled[[1]]) == 'character'
string_gauge
forced_numerics = sapply(string_gauge_labeled, function(l) as.numeric(paste(l, collapse = '')))
table(forced_numerics)
%in% range(1.3, 1.4)
1.38 %in% range(1.3, 1.4)
us_gauge = as.numeric(sapply(string_gauge_labeled, get_us_gauge))
string_gauge_labeled
us_gauge

us_gauge1 = as.numeric(sapply(string_gauge_labeled, get_us_gauge1))
us_gauge1

which(string_gauge %in% '1.28')
which(string_gauge %in% '1.27')
which(string_gauge %in% '1.13')

as.numeric(string_gauge)
string_gauge_labeled[15120]
string_gauge_labeled
table(string_gauge)
table(string_gauge_labeled)
which(string_gauge %in% 'N/A')
string_gauge[15715]
string_data[15120,]
string_gauge_split
'/' %in% string_gauge_split[[967]]
'1' %in% string_gauge_split[[967]]
string_gauge_split[967]



string_gauge
string_gauge_hybrid

if(string_gag)
string_gauge

## Price
# Convert  prices to dollars, find average price, and create column for adjusted price
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


# Review Adjectives: review_adjectives_split
##Extract list of review_adjectives vectors
review_adjectives_full = as.character(string_data$review_adjectives)
review_adjectives_split = strsplit(review_adjectives_full, ', ')



## Update dataframe with new columns containing extracted data
string_data1 = mutate(string_data, 
                      'tester_gender' = tester_gender,
                      'tester_age' = tester_age,
                      'tester_level' = tester_level,
                      'tester_strokes'= tester_strokes,
                      'tester_spin' = tester_spin,
                      'tester_style' = tester_style,
                      'racquet_manufacturer' = racquet_manufacturer, 
                      'racquet_model' = racquet_model,
                      'frame_size' = frame_size,
                      'string_pattern' = string_pattern,
                      'main_tension' = main_tension,
                      'cross_tension' = cross_tension,
                      'string_material' = string_material,
                      'string_construction' = string_construction,
                      'string_features' = string_features,
                      'string_gauge_metric' = string_gauge_metric,
                      'string_gauge_us' = string_gauge_us,
                      'price_adjusted' = price_adjusted,
                      'review_adjectives' = review_adjectives_split)

string_data1
# Extract the following info from scraped stringforum data and add as columns 
# to string_data

# Tester Info: tester_gender, tester_age, tester_level, tester_strokes, 
# tester_spin, tester_style
# Tester Racquet Info: racquet_manufacturer, racquet_model, frame_size, 
# string_pattern
# Tester Racquet Tension: main_tension, cross_tension
# String Info: string_material, string_construction, string_features
# String Gauge: string_gauge
# Price: price_adjusted
# Review Adjectives: review_adjectives_split


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
