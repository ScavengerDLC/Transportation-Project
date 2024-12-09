library(tidyverse)
library(xlsx)
### Todo:
### Regressions with GSS Data and Transportation Data
### Make Graphs



### Set Path Here
path <- "C:/Users/zades/Desktop/Transportation Project/"

### Read in Data
transportation_spending <- read.xlsx(paste0(path, "transportation_spending.xlsx"), "2022_2004")

transportation_usage <- read.xlsx(paste0(path, "transportation_usage.xlsx"), "2022_2010")

gss_averages <- read.csv(paste0(path, "gss_regional_averages.csv"))

gss <- read.csv(paste0(path, "gss_regression_ready.csv"))

### Get rid of indexing row

transportation_spending <- transportation_spending |> select(!NA.)

transportation_usage <- transportation_usage |> select(!NA.)

gss_averages <- gss_averages |> select(!X)

gss <- gss |> select(!X)

### Get rid of non-numbers from rows that should be all numbers and make variables
### Numeric. Not the most elegant code but man is it effective

### Usage
for (i in 1:116){
  column_number <- i + 2
  
  transportation_usage[,column_number] <- ifelse(transportation_usage[,column_number] == "N", 
                                                 NA, transportation_usage[,column_number])
  
  transportation_usage[,column_number] <- as.numeric(transportation_usage[,column_number])
}

### Spending
for (g in 1:24) {
  column_number <- g + 2
  
  transportation_spending[,column_number] <- ifelse(transportation_spending[,column_number] == "-", 
                                                 NA, transportation_spending[,column_number])
  
  transportation_spending[,column_number] <- as.numeric(transportation_spending[,column_number])
}


### Join dataframes for Regressions 
model_dataset_states <- transportation_usage |>
  left_join(transportation_spending, by = c("year", "state"))

### Create dataframe that joins usage to previous year's spending and a second that 
### joins previous year's usage to spending
transportation_spending <- transportation_spending |>
  mutate(previous_year = year - 1)

transportation_usage <- transportation_usage |>
  mutate(previous_year = year - 1)

usage_data_to_previous_spending <- transportation_usage |>
  left_join(transportation_spending, join_by(previous_year == year, state == state)) |>
  relocate(previous_year, .after = year)

spending_data_to_previous_usage <- transportation_spending |>
  left_join(transportation_usage, join_by(previous_year == year, state == state)) |>
  relocate(previous_year, .after = year) |>
  filter(!is.na(workers_total_estimate))

### Create Region Variable for model_data_set

state <- c("Alaska", "Hawaii", "California", "Oregon", "Washington", 
            "Montana", "Wyoming", "Idaho", "Nevada", "Utah", "Colorado", "Arizona", "New Mexico",
            "North Dakota", "South Dakota", "Nebraska", "Kansas", "Missouri", "Iowa", "Minnesota",
            "Texas", "Oklahoma", "Arkansas", "Louisiana",
            "Wisconsin", "Michigan", "Illinois", "Indiana", "Ohio",
            "Kentucky", "Tennessee", "Mississippi", "Alabama",
            "Florida", "Georgia", "South Carolina", "North Carolina", "Virginia", "West Virginia", "District of Columbia", "Maryland", "Delaware",
            "Pennsylvania", "New Jersey", "New York",
            "Connecticut", "Rhode Island", "Massachusetts", "Vermont", "New Hampshire", "Maine",
            "Total")



region <- c(rep("Pacific", 5), rep("Mountain", 8), rep("West North Central", 7),
             rep("West South Central", 4), rep("East North Central", 5),
             rep("East South Central", 4), rep("South Atlantic", 9),
             rep("Middle Atlantic", 3), rep("New England", 6), "Total")


state_regions <- data.frame(state, region)


rm(state)

rm(region)

### Join dataframe with model_dataframe to make regional data

model_dataset_regions <- model_dataset_states |>
  left_join(state_regions, join_by(state)) |>
  relocate(region, .after = state)

previous_spending_regional <- usage_data_to_previous_spending |>
  left_join(state_regions, join_by(state))  |>
  relocate(region, .after = state)

previous_usage_regional <- spending_data_to_previous_usage |>
  left_join(state_regions, join_by(state))  |>
  relocate(region, .after = state)

### Create Regional Totals and Regional Averages
### Model Dataset
region_totals <- model_dataset_regions |>
  group_by(region, year) |>
  mutate(
    across(
      c(workers_total_estimate:expense_transit_local_amount), sum
      )
    ) |>
  select(!state)|>
  distinct()

region_average <- model_dataset_regions |>
  group_by(region, year) |>
  mutate(
    across(
      c(workers_total_estimate:expense_transit_local_amount), mean
    )
  )|>
  select(!state) |>
  distinct()

### Previous Spending
previous_spending_region_totals <- previous_spending_regional |>
  group_by(region, year) |>
  mutate(
    across(
      c(workers_total_estimate:expense_transit_local_amount), sum
    )
  ) |>
  select(!state)|>
  distinct()

previous_spending_region_average <- previous_spending_regional |>
  group_by(region, year) |>
  mutate(
    across(
      c(workers_total_estimate:expense_transit_local_amount), mean
    )
  )|>
  select(!state) |>
  distinct()

### Previous Usage
previous_usage_region_totals <- previous_usage_regional |>
  group_by(region, year) |>
  mutate(
    across(
      c(workers_total_estimate:expense_transit_local_amount), sum
    )
  ) |>
  select(!state)|>
  distinct()

previous_usage_region_average <- previous_usage_regional |>
  group_by(region, year) |>
  mutate(
    across(
      c(workers_total_estimate:expense_transit_local_amount), mean
    )
  )|>
  select(!state) |>
  distinct()

### Join GSS Data with regional Data
gss_averages_same_year <- region_average|>
  left_join(gss_averages, join_by(region == REGION, year == YEAR))

gss_same_year <- region_totals |>
  left_join(gss, join_by(region == REGION, year == YEAR))

current_usage_previous_gss_averages_previous_spending <- previous_spending_region_average |>
  left_join(gss_averages, join_by(region == REGION, previous_year == YEAR))

current_usage_previous_gss_previous_spending <- previous_spending_region_totals |>
  left_join(gss, join_by(region == REGION, previous_year == YEAR))

current_spending_previous_gss_averages_previous_usage <- previous_usage_region_average |>
  left_join(gss_averages, join_by(region == REGION, previous_year == YEAR))

current_spending_previous_gss_previous_usage <- previous_usage_region_totals |> 
  left_join(gss_averages, join_by(region == REGION, previous_year == YEAR))

### Clean up environment
rm(model_dataset_regions)
rm(state_regions)
rm(transportation_spending)
rm(transportation_usage)
rm(region_average)
rm(region_totals)
rm(previous_spending_regional)
rm(previous_usage_regional)
rm(previous_spending_region_totals)
rm(previous_spending_region_average)
rm(previous_usage_region_average)
rm(previous_usage_region_totals)
rm(gss_averages)
rm(gss)

#######################
# Non GSS Regressions #
#######################

########################
# Same Year Comparison #
########################
### Check to see if there is an underlying relationship

### Car Alone Usage Vs Road Funding 
### Full Controls
### Age and income distribution 
car_alone_controls <- lm(expense_highways_state_and_local_amount ~ workers_car.alone_estimate 
                         + age_16_19_car.alone_estimate + age_20_24_car.alone_estimate 
                         + age_25_44_car.alone_estimate + age_45_54_car.alone_estimate
                         + age_55_59_car.alone_estimate + age_60_plus_car.alone_estimate
                         + income_9.9k_lower_car.alone_estimate + income_10k_14.9K_car.alone_estimate 
                         + income_15k_24.9K_car.alone_estimate + income_25k_34.9K_car.alone_estimate
                         + income_35k_49.9K_car.alone_estimate + income_50k_64.9K_car.alone_estimate
                         + income_65k_74.9K_car.alone_estimate + income_75k_plus_car.alone_estimate
                         + year+state ,
                         data = model_dataset_states) 
summary(car_alone_controls)
                           

### Minimal Controls
### Controlling for potentially influential groups and year
car_alone_minimal <- lm(expense_highways_state_and_local_amount ~ workers_car.alone_estimate 
                        + median_age_car.alone_estimate + income_75k_plus_car.alone_estimate + year + state,
                        data = model_dataset_states)

summary(car_alone_minimal)

### No Controls (except for year and state)
car_alone_no_controls_state <- lm(expense_highways_state_and_local_amount ~ workers_car.alone_estimate 
                            + year + state, data = model_dataset_states)

summary(car_alone_no_controls_state)

### No Controls (except for year)
car_alone_no_controls <- lm(expense_highways_state_and_local_amount ~ workers_car.alone_estimate 
                            + year, data = model_dataset_states)
summary(car_alone_no_controls)

### Car Pooling Vs Road Funding 
### Full Controls
### Age and income distribution 
carpool_controls <- lm(expense_highways_state_and_local_amount ~ workers_carpool_estimate 
                         + age_16_19_carpool_estimate + age_20_24_carpool_estimate 
                         + age_25_44_carpool_estimate + age_45_54_carpool_estimate
                         + age_55_59_carpool_estimate + age_60_plus_carpool_estimate
                         + income_9.9k_lower_carpool_estimate + income_10k_14.9K_carpool_estimate 
                         + income_15k_24.9K_carpool_estimate + income_25k_34.9K_carpool_estimate
                         + income_35k_49.9K_carpool_estimate + income_50k_64.9K_carpool_estimate
                         + income_65k_74.9K_carpool_estimate + income_75k_plus_carpool_estimate
                         + year + state,
                         data = model_dataset_states) 
summary(carpool_controls)


### Minimal Controls
### Controlling for potentially influential groups and year
carpool_minimal <- lm(expense_highways_state_and_local_amount ~ workers_carpool_estimate 
                        + median_age_carpool_estimate + income_75k_plus_carpool_estimate + year + state,
                        data = model_dataset_states)

summary(carpool_minimal)

### No Controls (except for year and state)
carpool_no_controls_state <- lm(expense_highways_state_and_local_amount ~ workers_carpool_estimate 
                                  + year + state, data = model_dataset_states)

summary(carpool_no_controls_state)

### No Controls (except for year)
carpool_no_controls <- lm(expense_highways_state_and_local_amount ~ workers_carpool_estimate 
                            + year, data = model_dataset_states)
summary(carpool_no_controls)


### Transit Usage Vs Transit Expenses
### Full Controls
### Age and income distribution 
transit_controls <- lm(expense_transit_state_and_local_amount ~ workers_transit_estimate 
                       + age_16_19_transit_estimate + age_20_24_transit_estimate 
                       + age_25_44_transit_estimate + age_45_54_transit_estimate
                       + age_55_59_transit_estimate + age_60_plus_transit_estimate
                       + income_9.9k_lower_transit_estimate + income_10k_14.9K_transit_estimate 
                       + income_15k_24.9K_transit_estimate + income_25k_34.9K_transit_estimate
                       + income_35k_49.9K_transit_estimate + income_50k_64.9K_transit_estimate
                       + income_65k_74.9K_transit_estimate + income_75k_plus_transit_estimate
                       + year + state,
                       data = model_dataset_states) 
summary(transit_controls)


### Minimal Controls
### Controlling for potentially influential groups and year
transit_minimal <- lm(expense_transit_state_and_local_amount ~ workers_transit_estimate 
                      + median_age_transit_estimate + income_75k_plus_transit_estimate + year + state,
                      data = model_dataset_states)

summary(transit_minimal)

### No Controls (except for year and state)
transit_no_controls_state <- lm(expense_transit_state_and_local_amount ~ workers_transit_estimate 
                                + year + state, data = model_dataset_states)

summary(transit_no_controls_state)

### No Controls (except for year)
transit_no_controls <- lm(expense_transit_state_and_local_amount ~ workers_transit_estimate 
                          + year, data = model_dataset_states)
summary(transit_no_controls)

#########################
# Previous Year Usage   #
#########################

### Car Alone vs Expenses
### Age and income distribution 
car_alone_controls <- lm(expense_highways_state_and_local_amount ~ workers_car.alone_estimate 
                         + age_16_19_car.alone_estimate + age_20_24_car.alone_estimate 
                         + age_25_44_car.alone_estimate + age_45_54_car.alone_estimate
                         + age_55_59_car.alone_estimate + age_60_plus_car.alone_estimate
                         + income_9.9k_lower_car.alone_estimate + income_10k_14.9K_car.alone_estimate 
                         + income_15k_24.9K_car.alone_estimate + income_25k_34.9K_car.alone_estimate
                         + income_35k_49.9K_car.alone_estimate + income_50k_64.9K_car.alone_estimate
                         + income_65k_74.9K_car.alone_estimate + income_75k_plus_car.alone_estimate
                         + year+state ,
                         data = spending_data_to_previous_usage) 
summary(car_alone_controls)


### Minimal Controls
### Controlling for potentially influential groups and year
car_alone_minimal <- lm(expense_highways_state_and_local_amount ~ workers_car.alone_estimate 
                        + median_age_car.alone_estimate + income_75k_plus_car.alone_estimate + year + state,
                        data = spending_data_to_previous_usage)

summary(car_alone_minimal)

### No Controls (except for year and state)
car_alone_no_controls_state <- lm(expense_highways_state_and_local_amount ~ workers_car.alone_estimate 
                                  + year + state, data = spending_data_to_previous_usage)

summary(car_alone_no_controls_state)

### No Controls (except for year)
car_alone_no_controls <- lm(expense_highways_state_and_local_amount ~ workers_car.alone_estimate 
                            + year, data = spending_data_to_previous_usage)
summary(car_alone_no_controls)

### Transit vs Expenses
### Full Controls
### Age and income distribution 
transit_controls <- lm(expense_transit_state_and_local_amount ~ workers_transit_estimate 
                       + age_16_19_transit_estimate + age_20_24_transit_estimate 
                       + age_25_44_transit_estimate + age_45_54_transit_estimate
                       + age_55_59_transit_estimate + age_60_plus_transit_estimate
                       + income_9.9k_lower_transit_estimate + income_10k_14.9K_transit_estimate 
                       + income_15k_24.9K_transit_estimate + income_25k_34.9K_transit_estimate
                       + income_35k_49.9K_transit_estimate + income_50k_64.9K_transit_estimate
                       + income_65k_74.9K_transit_estimate + income_75k_plus_transit_estimate
                       + year + state,
                       data = spending_data_to_previous_usage) 
summary(transit_controls)


### Minimal Controls
### Controlling for potentially influential groups and year
transit_minimal <- lm(expense_transit_state_and_local_amount ~ workers_transit_estimate 
                      + median_age_transit_estimate + income_75k_plus_transit_estimate + year + state,
                      data = spending_data_to_previous_usage)

summary(transit_minimal)

### No Controls (except for year and state)
transit_no_controls_state <- lm(expense_transit_state_and_local_amount ~ workers_transit_estimate 
                                + year + state, data = spending_data_to_previous_usage)

summary(transit_no_controls_state)

### No Controls (except for year)
transit_no_controls <- lm(expense_transit_state_and_local_amount ~ workers_transit_estimate 
                          + year, data = spending_data_to_previous_usage)
summary(transit_no_controls)

############################
# Previous Year Spending   #
############################

### Motivation here is too see if swapping usage and spending will show 
### that spending induces demand
### Car Alone vs Expenses
### Age and income distribution 
car_alone_controls <- lm(workers_car.alone_estimate ~  expense_highways_state_and_local_amount
                         + age_16_19_car.alone_estimate + age_20_24_car.alone_estimate 
                         + age_25_44_car.alone_estimate + age_45_54_car.alone_estimate
                         + age_55_59_car.alone_estimate + age_60_plus_car.alone_estimate
                         + income_9.9k_lower_car.alone_estimate + income_10k_14.9K_car.alone_estimate 
                         + income_15k_24.9K_car.alone_estimate + income_25k_34.9K_car.alone_estimate
                         + income_35k_49.9K_car.alone_estimate + income_50k_64.9K_car.alone_estimate
                         + income_65k_74.9K_car.alone_estimate + income_75k_plus_car.alone_estimate
                         + year+state ,
                         data = usage_data_to_previous_spending) 
summary(car_alone_controls)


### Minimal Controls
### Controlling for potentially influential groups and year
car_alone_minimal <- lm(workers_car.alone_estimate ~  expense_highways_state_and_local_amount
                        + median_age_car.alone_estimate + income_75k_plus_car.alone_estimate + year + state,
                        data = usage_data_to_previous_spending)

summary(car_alone_minimal)

### No Controls (except for year and state)
car_alone_no_controls_state <- lm(workers_car.alone_estimate ~  expense_highways_state_and_local_amount
                                  + year + state, data = usage_data_to_previous_spending)

summary(car_alone_no_controls_state)

### No Controls (except for year)
car_alone_no_controls <- lm(workers_car.alone_estimate ~  expense_highways_state_and_local_amount 
                            + year, data = usage_data_to_previous_spending)
summary(car_alone_no_controls)

### Transit vs Expenses
### Full Controls
### Age and income distribution 
transit_controls <- lm(workers_transit_estimate ~  expense_transit_state_and_local_amount
                       + age_16_19_transit_estimate + age_20_24_transit_estimate 
                       + age_25_44_transit_estimate + age_45_54_transit_estimate
                       + age_55_59_transit_estimate + age_60_plus_transit_estimate
                       + income_9.9k_lower_transit_estimate + income_10k_14.9K_transit_estimate 
                       + income_15k_24.9K_transit_estimate + income_25k_34.9K_transit_estimate
                       + income_35k_49.9K_transit_estimate + income_50k_64.9K_transit_estimate
                       + income_65k_74.9K_transit_estimate + income_75k_plus_transit_estimate
                       + year + state,
                       data = usage_data_to_previous_spending) 
summary(transit_controls)


### Minimal Controls
### Controlling for potentially influential groups and year
transit_minimal <- lm(workers_transit_estimate ~  expense_transit_state_and_local_amount
                      + median_age_transit_estimate + income_75k_plus_transit_estimate + year + state,
                      data = usage_data_to_previous_spending)

summary(transit_minimal)

### No Controls (except for year and state)
transit_no_controls_state <- lm(workers_transit_estimate ~  expense_transit_state_and_local_amount
                                + year + state, data = usage_data_to_previous_spending)

summary(transit_no_controls_state)

### No Controls (except for year)
transit_no_controls <- lm(workers_transit_estimate ~  expense_transit_state_and_local_amount
                          + year, data = usage_data_to_previous_spending)
summary(transit_no_controls)


###################
# GSS Regressions #
###################


### Raw Car Alone Usage vs Expense
model_dataset_states |> 
  filter(state != "Total") |>
  ggplot(aes(x = workers_car.alone_estimate, y = expense_highways_state_and_local_amount)) + 
  geom_point() +
  geom_smooth(method = lm)

### Raw Transit Usage vs Expense
model_dataset_states |> 
  filter(state != "Total") |>
  ggplot(aes(x = workers_transit_estimate, y = expense_transit_state_and_local_amount)) + 
  geom_point() +
  geom_smooth(method = lm)

