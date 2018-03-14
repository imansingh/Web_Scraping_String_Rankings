library(dplyr)
# library(tidyr)
# library(stringr)


# read scraped data into dataframe
string_data <- read.csv(file = "stringforum.csv")


# Check Overall Missingness:
sapply(string_data, function(vec) sum(is.na(vec)))

get_models_by_manufacturer = function(string){
  list = c()
  manufacturers = unique(string_data1[[string]])
  for(i in manufacturers[manufacturers != '']){
    assign(i, string_data1$racquet_model[string_data1$racquet_manufacturer == i])
    list = c(list, i)
  }
  return(list)
}



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


## Tester Info
# Gather data from tester_style field and extract tester_gender, tester_age, 
# tester_level, tester_strokes, tester_spin, tester_style
tester_info_full = as.character(string_data$tester_style)
tester_info_split = strsplit(gsub(',', '', tester_info_full), " ")

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
tester_gender[sapply(tester_info_split, is_male)] = 'Male'
tester_gender[sapply(tester_info_split, is_female)] = 'Female'

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
tester_age[sapply(tester_info_split, is_junior)] = 'Junior'
tester_age[sapply(tester_info_split, is_adult)] = 'Adult'
tester_age[sapply(tester_info_split, is_young_senior)] = 'Young Senior'
tester_age[sapply(tester_info_split, is_senior)] = 'Senior'


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
tester_level[sapply(tester_info_split, is_beginner)] = 'Beginner'
tester_level[sapply(tester_info_split, is_recreational)] = 'Recreational'
tester_level[sapply(tester_info_split, is_lower_league)] = 'Lower League'
tester_level[sapply(tester_info_split, is_upper_league)] = 'Upper League'
tester_level[sapply(tester_info_split, is_national_tournament)] = 
  'National Tournament'
tester_level[sapply(tester_info_split, is_international_tournament)] = 
  'International Tournament'

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
tester_strokes[sapply(tester_info_split, is_hard_strokes)] = 'Fast'
tester_strokes[sapply(tester_info_split, is_medium_strokes)] = 'Medium'
tester_strokes[sapply(tester_info_split, is_soft_strokes)] = 'Slow'


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
tester_spin[sapply(tester_info_split, is_heavy_spin)] = 'Heavy'
tester_spin[sapply(tester_info_split, is_moderate_spin)] = 'Moderate'
tester_spin[sapply(tester_info_split, is_little_spin)] = 'Low'


# tester_playstyle
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
tester_playstyle = c()
tester_playstyle[sapply(tester_info_split, is_allround)] = 'All-Around'
tester_playstyle[sapply(tester_info_split, is_baseline_defensive)] = 
  'Defensive Baseline'
tester_playstyle[sapply(tester_info_split, is_baseline_offensive)] = 
  'Offensive Baseline'
tester_playstyle[sapply(tester_info_split, is_serve_volley)] = 'Serve & Volley'


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
racquet_specs = substr(racquet_info_full, last_paren_indexes+1, 
                       nchar(racquet_info_full)-1)

# Extract racquet_manufacturer and racquet_model from racquet_names
# Manufacturer is usually first word in racquet_names, with exceptions specified
racquet_names_split = strsplit(racquet_names, ' ')
get_manufacturer = function(vector){
  if(!(is.na(vector[1]))){  
    if(vector[1] == 'Pro' | vector[1] == "Pro's" | vector[1] == 'The' | 
       vector[1] == 'Boris'){
      return(paste(vector[1:2], collapse = ' '))
    } else{
      return(vector[1])
    }
  }
}

get_model = function(vector){
  if(!(is.na(vector[1]))){  
    if(vector[1] == 'Pro' | vector[1] == "Pro's" | vector[1] == 'The' | 
       vector[1] == 'Boris'){
      return(paste(vector[-1:-2], collapse = ' '))
    } else {
    return(paste(vector[-1], collapse = ' '))
    }
  }
}

#function to convert empty string to NA, to identify missing values
empty_to_na = function(string){
  if(is.null(string)){
    string = NA
  } else if(string == ''){
    string = NA
  }
  return(string)
}

# create raw vectors with racquet_manufacturer and racquet_model
racquet_manufacturer_raw = sapply(racquet_names_split, get_manufacturer)
racquet_model_raw = sapply(racquet_names_split, get_model)

# replace empty strings with NAs for vectors just created
racquet_manufacturer = unname(sapply(racquet_manufacturer_raw, empty_to_na))
racquet_model = unname(sapply(racquet_model_raw, empty_to_na))

# Extract frame_size, string_pattern, and is_widebody from racquet_specs
# split racquet_specs into one, two or three element list
racquet_specs_split = strsplit(racquet_specs, ", ")

# functions to extract frame size, string pattern and widebody info
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

# create vectors to label tester racquets by frame_size, string_pattern and 
# is_widebody
frame_size = sapply(racquet_specs_split, get_frame_size)
string_pattern = sapply(racquet_specs_split, get_string_pattern)
is_widebody = sapply(racquet_specs_split, is_widebody)

# convert empty strings to NA
frame_size[frame_size == ''] = NA

# convert abbreviations in frame_size to full names
frame_size[frame_size == 'MS'] = 'Midsize'
frame_size[frame_size == 'MP'] = 'MidPlus'
frame_size[frame_size == 'OS'] = 'Oversize'

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

# Fix some capitalization errors in original data
string_info_full = gsub('core with one wrap', 'Core with One Wrap', 
                        string_info_full)
string_info_full = gsub('core with two wraps', 'Core with Two Wraps', 
                        string_info_full)
string_info_full = gsub('surface', 'Surface', string_info_full)

# split into list for further processing
string_info_split = strsplit(string_info_full, ', ')

# functions to extract string_material, string_construction and string_features
get_string_material = function(vector){
  material = vector[vector == 'Aramid' | vector == 'Polyester' | 
                      vector == 'Co-Polyester' | vector == 'Polyamid' | 
                      vector == 'Polyethylene' | vector == 'Polyurethane' | 
                      vector == 'Zyex' | vector == 'Natural Gut']
  return(material)
}
get_string_construction = function(vector){
  construction = vector[vector == 'Monofilament' | 
                                 vector == 'Central Core with One Wrap' |
                                 vector == 'Central Core with Two Wraps' |
                                 vector == 'Multifilament' | 
                                 vector == 'Ribbon Construction']
  return(construction)
}

get_string_features = function(vector){
  features = vector[vector == 'Structured Surface' | 
                      vector == 'Titanium Coating' | 
                      vector == 'Titanium Fibers' | vector == 'Hybrid String']
  return(features)
}

# function to replace 'character(0)' with NA, to identify missing data
char0_to_na = function(vec){
  if(identical(vec, character(0))){
    vec = NA
  }
  return(vec)
}

# creating lists for with string_material, string_construction_and string_features
string_material = sapply(string_info_split, get_string_material)
string_construction = sapply(string_info_split, get_string_construction)
string_features = sapply(string_info_split, get_string_features)

 
# # replacing empty string with NA to identify missing info in vectors just created
# string_material = sapply(string_material_list, char0_to_na)
# string_construction = sapply(string_construction_list, char0_to_na)
# string_features = sapply(string_features_list, char0_to_na)

# transforming the lists to character vectors for the dataframe
string_material = sapply(string_material, function(x) paste(x, collapse = ', '))
string_construction = sapply(string_construction, function(x) paste(x, collapse = ', '))
string_features = sapply(string_features, function(x) paste(x, collapse = ', '))



## String Gauge
# Extract string_gauge from string_name
string_names_full = as.character(string_data$string_name)
string_names_split = strsplit(string_names_full, ' ')
string_gauge = sapply(string_names_split, function(l) l[length(l)])
string_gauge_split = strsplit(string_gauge, '')
table(string_gauge)
# convet US and metric measurements and create string_gauge_metric and 
# string_gauge_us

# label hybrids - we will not convert these
label_hybrids = function(vector){
  if(vector[1] == 'N'){
    vector = NA
  } else if('~' %in% vector){
    vector = 'oblong'
  } else if('/' %in% vector) {
    vector = 'hybrid'
  } else{
    vector = vector
  }
  return(vector)
}

# string_gauge_labeled = list with hybrids labeled
string_gauge_labeled = sapply(string_gauge_split, label_hybrids)

# function to get return gauges in metric units
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

get_us_gauge = function(vector){
  gauge_string = paste(vector, collapse = '')
  if(gauge_string == '15L'){
    gauge_string = 15
  } else if(gauge_string == '16L'){
      gauge_string = 16
    } else if(gauge_string == '17L'){
      gauge_string = 17
    } else if(gauge_string == '18L'){
      gauge_string = 18
    } else if(gauge_string == '19L'){
      gauge_string = 19
    }
  gauge_num = as.numeric(gauge_string)
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

# create vectors for string_gauge_metric and string_gauge_us
string_gauge_metric = as.numeric(sapply(string_gauge_labeled, get_metric_gauge))
string_gauge_us = sapply(string_gauge_labeled, get_us_gauge)

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

# replace 'wire-like' with 'wire_like' to avoid issues with hyphen in column name
review_adjectives_full = gsub('wire-like', 'wire_like', review_adjectives_full)

#create list with split strings and replace 'character(0)' with NA
review_adjectives_split = strsplit(review_adjectives_full, ', ') 

soft = sapply(review_adjectives_split, function(x) 'soft' %in% x)
comfortable = sapply(review_adjectives_split, function(x) 'comfortable' %in% x)
flexible = sapply(review_adjectives_split, function(x) 'flexible' %in% x)
precise = sapply(review_adjectives_split, function(x) 'precise' %in% x)
resilient = sapply(review_adjectives_split, function(x) 'resilient' %in% x)
explosive = sapply(review_adjectives_split, function(x) 'explosive' %in% x)
innovative = sapply(review_adjectives_split, function(x) 'innovative' %in% x)
unique = sapply(review_adjectives_split, function(x) 'unique' %in% x)
spongy = sapply(review_adjectives_split, function(x) 'spongy' %in% x)
stiff = sapply(review_adjectives_split, function(x) 'stiff' %in% x)
dull = sapply(review_adjectives_split, function(x) 'dull' %in% x)
lively = sapply(review_adjectives_split, function(x) 'lively' %in% x)
stretchy = sapply(review_adjectives_split, function(x) 'stretchy' %in% x)
crispy = sapply(review_adjectives_split, function(x) 'crispy' %in% x)
boring = sapply(review_adjectives_split, function(x) 'boring' %in% x)
elastic = sapply(review_adjectives_split, function(x) 'elastic' %in% x)
solid = sapply(review_adjectives_split, function(x) 'solid' %in% x)
rough = sapply(review_adjectives_split, function(x) 'rough' %in% x)
wire_like = sapply(review_adjectives_split, function(x) 'wire_like' %in% x)
springy = sapply(review_adjectives_split, function(x) 'springy' %in% x)
sluggish = sapply(review_adjectives_split, function(x) 'sluggish' %in% x)
outdated = sapply(review_adjectives_split, function(x) 'outdated' %in% x)
num_adjecs = sapply(review_adjectives_split, function(x) length(x))


# ## Capitalize All Strings
# 
# # function to capitalize first letter of each word in string
# simpleCap = function(string){
#   if(!(is.na(string))){
#     split_string = strsplit(string, ' ')[[1]]
#     paste(toupper(substring(split_string, 1, 1)), substring(split_string, 2),
#           sep = '', collapse = ' ')
#   }
# }
# 
# simpleCap_list = function(vector){
#   unname(sapply(vector, simpleCap))
# }
# 
# # create vecs with capitalized strings strings
# tester_gender = unname(sapply(tester_gender, simpleCap))
# string_adjectives = sapply(review_adjectives_split, simpleCap_list)


# for characteristics scores,  add 3 and multiply by .06 to get interpretable 
# score on 0-100 range because characteristics are measured on -3 to +3 scale

# convert characteristics scores to % max
# all NAs are preserved

string_data1 = data.frame()
string_data1 = string_data %>%
  mutate(
    comfort = (comfort + 3) / .06,
    control = (control + 3) / .06,
    durability = (durability + 3) / .06,
    feel = (feel + 3) / .06,
    power = (power +3) / .06,
    spin = (spin + 3) / .06,
    tension_stability = (tension_stability + 3) / .06,
    tester_satisfaction = (tester_satisfaction + 1) / .02
  )


## Update dataframe with new columns containing extracted data
string_data1 = mutate(string_data1, 
                      'tester_gender' = tester_gender,
                      'tester_age' = tester_age,
                      'tester_level' = tester_level,
                      'tester_strokes'= tester_strokes,
                      'tester_spin' = tester_spin,
                      'tester_playstyle' = tester_playstyle,
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
                      'string_adjectives' = review_adjectives_full,
                      'soft' = soft,
                      'comfortable' = comfortable,
                      'flexible' = flexible,
                      'precise' = precise,
                      'resilient' = resilient,
                      'explosive' = explosive,
                      'innovative' = innovative,
                      'unique' = unique,
                      'spongy' = spongy,
                      'stiff' = stiff,
                      'dull' = dull,
                      'lively' = lively,
                      'stretchy' = stretchy,
                      'crispy' = crispy,
                      'boring' = boring,
                      'elastic' = elastic,
                      'solid' = solid,
                      'rough' = rough,
                      'wire_like' = wire_like,
                      'springy' = springy,
                      'sluggish' = sluggish,
                      'outdated' = outdated,
                      'num_adjecs' = num_adjecs
                      )


string_data_wrangled = string_data1 %>%
  select(string_name, num_ratings, price_adjusted, 
         string_material, string_construction, string_features, 
         string_gauge_metric, string_gauge_us, string_adjectives,
         tester_name, tester_reviews, tester_gender, tester_age, tester_level, 
         tester_playstyle, tester_strokes, tester_spin, 
         racquet_manufacturer, racquet_model, string_pattern, frame_size,
         main_tension, cross_tension,
         comfort, control, durability, feel, power, spin, tension_stability, 
         overall_satisfaction = tester_satisfaction, review_text, soft, 
         comfortable, flexible, precise, resilient, explosive, innovative, 
         unique, spongy, stiff, dull, lively, stretchy, crispy, boring, elastic,
         solid, rough, wire_like, springy, sluggish, outdated, num_adjecs)


# write.csv(string_data_wrangled, file = 'string_data_wrangled.csv')
