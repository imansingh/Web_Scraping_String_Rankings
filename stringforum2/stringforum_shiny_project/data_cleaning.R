library(dplyr)
library(tidyr)
library(stringr)
library(DT)

summary(stringforum)

#creating grouped dataframes
string_grouped = stringforum %>% group_by(string_name)
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
