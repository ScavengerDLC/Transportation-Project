library("xlsx")
library("tidyverse")
library("dplyr")
library("stringr")
library("tidyselect")

setwd("C:/Users/zades/Desktop/Transportation Project/Census Data")

#########################################################
# Get National Transportation Spending Dataframe Set Up #
#########################################################

df <- read.xlsx("22slsstab1.xlsx", "2022_US_WY")

### Begin Removing some unnecessary rows that are artifacts from spreadsheet format
df <- df |> filter(!is.na(NA..1))

### Rename NA..X rows to proper names 
# Read state_names.csv
state_names <- read.csv("state_names.csv")

# For Loop that Renames column names to real column names
for (i in 1:52) {
  name <- state_names[i,]
  lower_bound <- 3 + 5 * (i-1)
  upper_bound <- 7 + 5 * (i-1)
  names(df)[lower_bound:upper_bound] <-
    paste0(name, "_", c("State and Local Amount", "State and Local CV", "State Amount", "Local Amount", "Local CV"))
}

### Filter More Unnecessary Rows that are artifacts from conversion from spreadsheet
df <- df |> filter(!is.na(NA.) & NA. != "Line") |>
  rename(Line = NA.)

### Select Necessary Budget Items. Identified through reading the spreadsheets and making
### a list of relevant entries

df <- df |> filter(Line == 13 | Line == 20 | Line == 28 | Line == 30 |
                     Line == 47 | Line == 86 | Line == 89 | Line == 118)

### Fix Variable 2's name and values

names(df)[2] <- "type"

df <- df |> mutate(type = str_trim(type, side = "both")) |>
  mutate(type = str_replace_all(type, " ", "_")) |>
  mutate(type = tolower(type))

### Rename Values in Type to Have information on if it is expense or revenue

df <- df |> 
  mutate(
    type = ifelse(
      Line == 13 | Line == 20 | Line == 28 | Line == 30 | Line == 47, 
      paste0("revenue_", type), 
      paste0("expense_", type)
    )
  )

### Pivot Data Frame
df <- df |>
  pivot_longer(cols = c(`Total_State and Local Amount`:`Wyoming_Local CV`),
               names_to = "govt_level")

### Make Final Adjustments to Type and govt_level Values and Change Line to designate Year.
df <- df |>
  mutate(type = paste0(type, "_", gsub("^[^_]*_", "", govt_level))) |>
  mutate(govt_level = gsub("_.*", "", govt_level)) |>
  mutate(Line = 2022) |>
  rename(year = Line) |>
  rename(state = govt_level)

df <- df |>
  mutate(type = str_replace_all(type, " ", "_")) |>
  mutate(type = tolower(type))

df <- df |>
  pivot_wider(names_from = type, values_from = value)

transportation_spending <- data.frame(matrix(NA, nrow = 1, ncol = 42))

names(transportation_spending) <- names(df)

### Bind Rows to main spending dataframe
transportation_spending <- rbind(transportation_spending, df)



############################################################
# Function to Get the Rest of the spreadsheets read into R #
############################################################

census_cleaner <- function(filename, year_observed){
  df <- read.xlsx(as.character({{filename}}), paste0({{year_observed}}, "_US_WY"))
  
  ### Begin Removing some unnecessary rows that are artifacts from spreadsheet format
  df <- df |> filter(!is.na(NA..1))
  
  ### Rename NA..X rows to proper names 
  # Read state_names.csv
  state_names <- read.csv("state_names.csv")
  
  # For Loop that Renames column names to real column names
  for (i in 1:52) {
    name <- state_names[i,]
    lower_bound <- 3 + 5 * (i-1)
    upper_bound <- 7 + 5 * (i-1)
    names(df)[lower_bound:upper_bound] <-
      paste0(name, "_", c("State and Local Amount", "State and Local CV", "State Amount", "Local Amount", "Local CV"))
  }
  
  ### Filter More Unnecessary Rows that are artifacts from conversion from spreadsheet
  df <- df |> filter(!is.na(NA.) & NA. != "Line") |>
    rename(Line = NA.)
  
  ### Select Necessary Budget Items. Identified through reading the spreadsheets and making
  ### a list of relevant entries
  
  df <- df |> filter(Line == 13 | Line == 20 | Line == 28 | Line == 30 |
                                             Line == 47 | Line == 86 | Line == 89 | Line == 118)
  
  ### Fix Variable 2's name and values
  
  names(df)[2] <- "type"
  
  df <- df |> mutate(type = str_trim(type, side = "both")) |>
    mutate(type = str_replace_all(type, " ", "_")) |>
    mutate(type = tolower(type))
  
  ### Rename Values in Type to Have information on if it is expense or revenue
  
  df <- df |> 
    mutate(
      type = ifelse(
        Line == 13 | Line == 20 | Line == 28 | Line == 30 | Line == 47, 
        paste0("revenue_", type), 
        paste0("expense_", type)
      )
    )
  
  ### Pivot Data Frame
  df <- df |>
    pivot_longer(cols = c(`Total_State and Local Amount`:`Wyoming_Local CV`),
                 names_to = "govt_level")
  
  ### Make Final Adjustments to Type and govt_level Values and Change Line to designate Year.
  df <- df |>
    mutate(type = paste0(type, "_", gsub("^[^_]*_", "", govt_level))) |>
    mutate(govt_level = gsub("_.*", "", govt_level)) |>
    mutate(Line = {{year_observed}}) |>
    rename(year = Line) |>
    rename(state = govt_level)
  
  df <- df |>
    mutate(type = str_replace_all(type, " ", "_")) |>
    mutate(type = tolower(type))
  
  df <- df |>
    pivot_wider(names_from = type, values_from = value)
  
  ### Bind Rows to main spending dataframe
  transportation_spending <- rbind(transportation_spending, df)
  
}

###############################################
# Reading the Rest of the Spreadsheets into R #
###############################################

### I tried to get a for loop working here. It did not want to cooperate
### SO rather than spending an hour trying to get it to work, here's some 
### copy and paste magic that got done in a fraction of the time

### Future Chris here: I was 100% correct in not doing a for loop here
### there are slight changes in how the spreadsheets are done and it
### would have been impossible to know ahead of time
transportation_spending <- census_cleaner("21slsstab1.xlsx", 2021)

transportation_spending <- census_cleaner("20slsstab1.xlsx", 2020)

transportation_spending <- census_cleaner("19slsstab1.xlsx", 2019)

transportation_spending <- census_cleaner("18slsstab1.xlsx", 2018)

transportation_spending <- census_cleaner("17slsstab1.xlsx", 2017)

#############################################################################
# Make Slight Edit to Function for Pre-2017 Sheets. Before 2017 they were   #
# broken into 2 sheets. I combined them on my own in google sheets          #
#############################################################################

census_cleaner_alt <- function(filename, year_observed){
  df <- read.xlsx(as.character({{filename}}), paste0({{year_observed}}, "_US_MS"))
  ### Begin Removing some unnecessary rows that are artifacts from spreadsheet format
  df <- df |> filter(!is.na(NA..1))
  
  ### Rename NA..X rows to proper names 
  # Read state_names.csv
  state_names <- read.csv("state_names.csv")
  
  # For Loop that Renames column names to real column names
  for (i in 1:52) {
    name <- state_names[i,]
    lower_bound <- 3 + 5 * (i-1)
    upper_bound <- 7 + 5 * (i-1)
    names(df)[lower_bound:upper_bound] <-
      paste0(name, "_", c("State and Local Amount", "State and Local CV", "State Amount", "Local Amount", "Local CV"))
  }
  

  
  ### Filter More Unnecessary Rows that are artifacts from conversion from spreadsheet
  df <- df |> filter(!is.na(NA.) & NA. != "Line") |>
    rename(Line = NA.)
  
  ### Select Necessary Budget Items. Identified through reading the spreadsheets and making
  ### a list of relevant entries
  
  df <- df |> filter(Line == 13 | Line == 20 | Line == 28 | Line == 30 |
                       Line == 47 | Line == 86 | Line == 89 | Line == 118)

  
  ### Fix Variable 2's name and values
  
  names(df)[2] <- "type"
  
  df <- df |> mutate(type = str_trim(type, side = "both")) |>
    mutate(type = str_replace_all(type, " ", "_")) |>
    mutate(type = tolower(type))
  
  ### Rename Values in Type to Have information on if it is expense or revenue
  
  df <- df |> 
    mutate(
      type = ifelse(
        Line == 13 | Line == 20 | Line == 28 | Line == 30 | Line == 47, 
        paste0("revenue_", type), 
        paste0("expense_", type)
      )
    )
  
  ### Pivot Data Frame
  df <- df |>
    pivot_longer(cols = c(`Total_State and Local Amount`:`Wyoming_Local CV`),
                 names_to = "govt_level")
  
  ### Make Final Adjustments to Type and govt_level Values and Change Line to designate Year.
  df <- df |>
    mutate(type = paste0(type, "_", gsub("^[^_]*_", "", govt_level))) |>
    mutate(govt_level = gsub("_.*", "", govt_level)) |>
    mutate(Line = {{year_observed}}) |>
    rename(year = Line) |>
    rename(state = govt_level)
  
  df <- df |>
    mutate(type = str_replace_all(type, " ", "_")) |>
    mutate(type = tolower(type))
  
  df <- df |>
    pivot_wider(names_from = type, values_from = value)
  
  ### Bind Rows to main spending dataframe
  transportation_spending <- rbind(transportation_spending, df)
}

transportation_spending <- census_cleaner_alt("16slsstab1.xlsx", 2016)

transportation_spending <- census_cleaner_alt("15slsstab1.xlsx", 2015)

transportation_spending <- census_cleaner_alt("14slsstab1.xlsx", 2014)

transportation_spending <- census_cleaner_alt("13slsstab1.xlsx", 2013)

transportation_spending <- census_cleaner_alt("11slsstab1.xlsx", 2011)

transportation_spending <- census_cleaner_alt("10slsstab1.xlsx", 2010)

transportation_spending <- census_cleaner_alt("09slsstab1.xlsx", 2009)

transportation_spending <- census_cleaner_alt("08slsstab1.xlsx", 2008)

transportation_spending <- census_cleaner_alt("06slsstab1.xlsx", 2006)

transportation_spending <- census_cleaner_alt("05slsstab1.xlsx", 2005)


#############################################################################
### Transit Subsidies are added as a separate item on the expenses for 2004 #
#############################################################################

df <- read.xlsx("04slsstab1.xlsx", "2004_US_MS")

### Begin Removing some unnecessary rows that are artifacts from spreadsheet format
df <- df |> filter(!is.na(NA..1))

### Rename NA..X rows to proper names 
# Read state_names.csv
state_names <- read.csv("state_names.csv")

# For Loop that Renames column names to real column names
for (i in 1:52) {
  name <- state_names[i,]
  lower_bound <- 3 + 5 * (i-1)
  upper_bound <- 7 + 5 * (i-1)
  names(df)[lower_bound:upper_bound] <-
    paste0(name, "_", c("State and Local Amount", "State and Local CV", "State Amount", "Local Amount", "Local CV"))
}



### Filter More Unnecessary Rows that are artifacts from conversion from spreadsheet
df <- df |> filter(!is.na(NA.) & NA. != "Line") |>
  rename(Line = NA.)

### Select Necessary Budget Items. Identified through reading the spreadsheets and making
### a list of relevant entries

df <- df |> filter(Line == 13 | Line == 20 | Line == 28 | Line == 30 | 
                     Line == 47 | Line == 86 | Line == 89 | Line == 91 | Line == 118)


### Fix Variable 2's name and values

names(df)[2] <- "type"

df <- df |> mutate(type = str_trim(type, side = "both")) |>
  mutate(type = str_replace_all(type, " ", "_")) |>
  mutate(type = tolower(type))

### Rename Values in Type to Have information on if it is expense or revenue

df <- df |> 
  mutate(
    type = ifelse(
      Line == 13 | Line == 20 | Line == 28 | Line == 30 | Line == 47, 
      paste0("revenue_", type), 
      paste0("expense_", type)
    )
  )

### Pivot Data Frame
df <- df |>
  pivot_longer(cols = c(`Total_State and Local Amount`:`Wyoming_Local CV`),
               names_to = "govt_level")


### Make Final Adjustments to Type and govt_level Values and Change Line to designate Year.
df <- df |>
  mutate(type = paste0(type, "_", gsub("^[^_]*_", "", govt_level))) |>
  mutate(govt_level = gsub("_.*", "", govt_level)) |>
  mutate(Line = 2004) |>
  rename(year = Line) |>
  rename(state = govt_level)

df <- df |>
  mutate(type = str_replace_all(type, " ", "_")) |>
  mutate(type = tolower(type))

df <- df |>
  pivot_wider(names_from = type, values_from = value)

### Combine Transit and  Transit Subsidy Expenses

transit <- df |> select(c("state", "expense_transit_subsidies_state_and_local_amount":"expense_transit_local_cv"))


### Note: I know that this isn't how cv's should be combined, I know that I will
### not be using them and I'm too far in to change all my code, so I'm working
### around it at this point
### Note: I know that adding the dollar amounts is fine because the census notes
### that duplicate transactions are excluded. All entries in this sheet are unique
### expenses
transit <- transit |> 
  mutate(
    expense_transit_local_amount = ifelse(expense_transit_local_amount == "-", NA, expense_transit_local_amount),
    expense_transit_local_cv = ifelse(expense_transit_local_cv == "-", NA, expense_transit_local_cv),
    expense_transit_state_amount = ifelse(expense_transit_state_amount == "-", NA, expense_transit_state_amount),
    expense_transit_state_and_local_amount = ifelse(expense_transit_state_and_local_amount == "-", NA, expense_transit_state_and_local_amount),
    expense_transit_state_and_local_cv = ifelse(expense_transit_state_and_local_cv == "-", NA, expense_transit_state_and_local_cv),
    expense_transit_subsidies_local_amount = ifelse(expense_transit_subsidies_local_amount == "-", NA, expense_transit_subsidies_local_amount),
    expense_transit_subsidies_local_cv = ifelse(expense_transit_subsidies_local_cv == "-", NA, expense_transit_subsidies_local_cv),
    expense_transit_subsidies_state_amount = ifelse(expense_transit_subsidies_state_amount == "-", NA, expense_transit_subsidies_state_amount),
    expense_transit_subsidies_state_and_local_amount = ifelse(expense_transit_subsidies_state_and_local_amount == "-", NA, expense_transit_subsidies_state_and_local_amount),
    expense_transit_subsidies_state_and_local_cv = ifelse(expense_transit_subsidies_state_and_local_cv == "-", NA, expense_transit_subsidies_state_and_local_cv),
  ) |>
  mutate(
    expense_transit_local_amount = as.numeric(expense_transit_local_amount) + as.numeric(expense_transit_subsidies_local_amount),
    expense_transit_local_cv = as.numeric(expense_transit_local_cv) + as.numeric(expense_transit_subsidies_local_cv),
    expense_transit_state_amount = as.numeric(expense_transit_state_amount) + as.numeric(expense_transit_subsidies_state_amount),
    expense_transit_state_and_local_amount = as.numeric(expense_transit_state_and_local_amount) + as.numeric(expense_transit_subsidies_state_and_local_amount),
    expense_transit_state_and_local_cv = as.numeric(expense_transit_state_and_local_cv) + as.numeric(expense_transit_subsidies_state_and_local_cv)
  ) |>
  select("state", "expense_transit_state_and_local_amount":"expense_transit_local_cv")

df <- df |> select(!c("expense_transit_subsidies_state_and_local_amount":"expense_transit_local_cv"))

df <- df |> inner_join(transit, by = "state")

### Bind Rows to main spending dataframe
transportation_spending <- rbind(transportation_spending, df)




### More Data Exists on the Census Website. However the further back I go, the 
### spreadsheets become increasingly non-standardized and difficult to read in
### quickly. For reference, I've been working on this part of the project for 
### 7 hours and I haven't even done any data analysis. I believe that 
### 2024-2004 will be a sufficient time span to look at and gather trends.
### Additionally, they don't have state by state information for 2003
### If this were a longer research project, this would be one point that I 
### expand my scope.

################################################################################
# Make Second Dataframe for 2012 and 2007 Data. They Did not calculate         #
# coefficients of variation in 2012.                                           #
################################################################################

### I tried to make a function here but it didn't want to play nice
### Even more vindicated in not spending more time on the special cases of 
### even further years in the past.
transportation_spending_no_cv <- transportation_spending |>
  select(!vars_select(names(transportation_spending), ends_with("_cv")))

census_cleaner_no_cv <- function(filename, year_observed){
  df <- read.xlsx(as.character({{filename}}), paste0({{year_observed}}, "_US_MS"))
  print("Successfully Read File")
  ### Begin Removing some unnecessary rows that are artifacts from spreadsheet format
  df <- df |> filter(!is.na(NA..1))
  
  ### Rename NA..X rows to proper names 
  # Read state_names.csv
  state_names <- read.csv("state_names.csv")
  
  # For Loop that Renames column names to real column names
  for (i in 1:52) {
    name <- state_names[i,]
    lower_bound <- 3 + 3 * (i-1)
    upper_bound <- 5 + 3 * (i-1)
    names(df)[lower_bound:upper_bound] <-
      paste0(name, "_", c("State and Local Amount", "State Amount", "Local Amount"))
  }
  
  
  
  ### Filter More Unnecessary Rows that are artifacts from conversion from spreadsheet
  df <- df |> filter(!is.na(NA.) & NA. != "Line") |>
    rename(Line = NA.)
  
  ### Select Necessary Budget Items. Identified through reading the spreadsheets and making
  ### a list of relevant entries
  
  df <- df |> filter(Line == 13 | Line == 20 | Line == 28 | Line == 30 |
                       Line == 47 | Line == 86 | Line == 89 | Line == 118)
  
  
  ### Fix Variable 2's name and values
  
  names(df)[2] <- "type"
  
  df <- df |> mutate(type = str_trim(type, side = "both")) |>
    mutate(type = str_replace_all(type, " ", "_")) |>
    mutate(type = tolower(type))
  
  ### Rename Values in Type to Have information on if it is expense or revenue
  
  df <- df |> 
    mutate(
      type = ifelse(
        Line == 13 | Line == 20 | Line == 28 | Line == 30 | Line == 47, 
        paste0("revenue_", type), 
        paste0("expense_", type)
      )
    )
  
  ### Pivot Data Frame
  df <- df |>
    pivot_longer(cols = c(`Total_State and Local Amount`:`Wyoming_Local Amount`),
                 names_to = "govt_level")
  
  
  ### Make Final Adjustments to Type and govt_level Values and Change Line to designate Year.
  df <- df |>
    mutate(type = paste0(type, "_", gsub("^[^_]*_", "", govt_level))) |>
    mutate(govt_level = gsub("_.*", "", govt_level)) |>
    mutate(Line = {{year_observed}}) |>
    rename(year = Line) |>
    rename(state = govt_level)
  
  df <- df |>
    mutate(type = str_replace_all(type, " ", "_")) |>
    mutate(type = tolower(type))
  
  df <- df |>
    pivot_wider(names_from = type, values_from = value)
  
  ### Bind Rows to main spending dataframe
  transportation_spending_no_cv <- rbind(transportation_spending_no_cv, df)
}

transportation_spending_no_cv <- census_cleaner_no_cv("12slsstab1.xlsx", 2012)

transportation_spending_no_cv <- census_cleaner_no_cv("07slsstab1.xlsx", 2007)

### Remove NA row that was made to create the dataframe

transportation_spending_no_cv <- transportation_spending_no_cv |> filter(!is.na(year))

write.xlsx(transportation_spending_no_cv, "transportation_spending.xlsx", sheetName = "2022_2004")
