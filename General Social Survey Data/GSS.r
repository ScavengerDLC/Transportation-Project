library(foreign)
library(tidyverse)

### This code came with the data, I will note when my work starts
  read.dct <- function(dct, labels.included = "yes") {
      temp <- readLines(dct)
      temp <- temp[grepl("_column", temp)]
      switch(labels.included,
             yes = {
                 pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+)[a-z]\\s+(.*)"
                 classes <- c("numeric", "character", "character", "numeric", "character")
                 N <- 5
                 NAMES <- c("StartPos", "Str", "ColName", "ColWidth", "ColLabel")
             },
             no = {
                 pattern <- "_column\\(([0-9]+)\\)\\s+([a-z0-9]+)\\s+(.*)\\s+%([0-9]+).*"
                 classes <- c("numeric", "character", "character", "numeric")
                 N <- 4
                 NAMES <- c("StartPos", "Str", "ColName", "ColWidth")
             })
      temp_metadata <- setNames(lapply(1:N, function(x) {
          out <- gsub(pattern, paste("\\", x, sep = ""), temp)
          out <- gsub("^\\s+|\\s+$", "", out)
          out <- gsub('\"', "", out, fixed = TRUE)
          class(out) <- classes[x] ; out }), NAMES)
      temp_metadata[["ColName"]] <- make.names(gsub("\\s", "", temp_metadata[["ColName"]]))
      temp_metadata
  }

  read.dat <- function(dat, metadata_var, labels.included = "yes") {
      read.table(dat, col.names = metadata_var[["ColName"]])
  }


GSS_metadata <- read.dct("GSS.dct")
GSS_ascii <- read.dat("GSS.dat", GSS_metadata)
attr(GSS_ascii, "col.label") <- GSS_metadata[["ColLabel"]]
GSS <- GSS_ascii


### My code work begins from here
### My goal is to get the data into the year - region level rather than 
### individual interview level. This will allow us to get average sentiment
### on topics with average demographic data

### Get Variables ready for regression
### Change Race to a group of dummy variables that check for specific categories
### Create Dummy variables for sex, and if there are kids in household
### Change regions from numbers to names 
### Change Some values variables to -1 to 1 when given a too little-right amount-too much
### scale or other applicable scale (-2 to 2 for 5 point scale) Scale high values and low
### values depend on if I believe the public thinking something is getting too little funding
### is a good thing for transit funding. For example, people thinking that too
### little is being spent on crime would lead to less funding for public transit
### as it is often seen as shady or a place where criminals lurk. 
### Code in NA's where applicable, for example some variables set unrealistic values
### like -100 for NA
### For this information, I looked through the variables on the website:
### https://gssdataexplorer.norc.org

GSS <- GSS |>
  mutate(
    REGION = ifelse(REGION == 1, "New England",
             ifelse(REGION == 2, "Middle Atlantic",
             ifelse(REGION == 3, "East North Central",
             ifelse(REGION == 4, "West North Central",
             ifelse(REGION == 5, "South Atlantic",
             ifelse(REGION == 6, "East South Atlantic",
             ifelse(REGION == 7, "West South Central",
             ifelse(REGION == 8, "Mountain",
             ifelse(REGION == 9, "Pacific", NA)
             )))))))),
    race_white = ifelse(RACE == 1, 1, ifelse(RACE == -100, NA, 0)),
    race_black = ifelse(RACE == 2, 1, ifelse(RACE == -100, NA, 0)),
    race_other = ifelse(RACE == 3, 1, ifelse(RACE == -100, NA, 0)),
    female = ifelse(SEX == 2, 1, 
             ifelse(SEX == 1, 0, NA)),
    NATCRIME = ifelse(NATCRIME == 1, -1,
               ifelse(NATCRIME == 2, 0, 
               ifelse(NATCRIME == 3, 1, NA))),
    NATMASS = ifelse(NATMASS == 1, 1,
              ifelse(NATMASS == 2, 0,
              ifelse(NATMASS == 3, -1, NA))),
    TRUST = ifelse(TRUST == 1, -1,
            ifelse(TRUST == 2, 0,
            ifelse(TRUST == 3, 1, NA))),
    FEAR = ifelse(FEAR == 1, 1,
           ifelse(FEAR == 2, 0, NA)),
    has_kids = ifelse(BABIES > 0 | PRETEEN > 0 | TEENS > 0, 1, 0),
    total_kids = BABIES + PRETEEN + TEENS,
    AGE = ifelse(AGE > 0, AGE, NA),
    HOMPOP = ifelse(HOMPOP >= 0, HOMPOP, NA),
    BABIES = ifelse(BABIES >= 0, BABIES, NA),
    PRETEEN = ifelse(PRETEEN >= 0, PRETEEN, NA),
    TEENS = ifelse(TEENS >= 0, TEENS, NA),
    ADULTS = ifelse(ADULTS > 0, ADULTS, NA),
    
  ) |>
  select(!c(SEX, RACE, BALLOT, ID_, SIZE))


### Takes GSS Data and Averages All variables by Region and Year
GSS_regional_averages <- data.frame()



for (i in seq(1972,2022)) {
  df <- GSS |>
    filter(YEAR == i)
  
  if(nrow(df) > 0) {
    ### National Totals
    df <- df |>
      mutate(
        AGE = mean(AGE, na.rm = TRUE),
        HOMPOP = mean(HOMPOP, na.rm = TRUE),
        BABIES = mean(BABIES, na.rm = TRUE),
        PRETEEN = mean(PRETEEN, na.rm = TRUE),
        TEENS = mean(TEENS, na.rm = TRUE),
        ADULTS = mean(ADULTS, na.rm = TRUE),
        NATCRIME = mean(NATCRIME, na.rm = TRUE),
        NATMASS = mean(NATMASS, na.rm = TRUE),
        TRUST = mean(TRUST, na.rm = TRUE),
        FEAR = mean(FEAR, na.rm = TRUE),
        race_white = mean(race_white, na.rm = TRUE),
        race_black = mean(race_black, na.rm = TRUE),
        race_other = mean(race_other, na.rm = TRUE),
        female = mean(female, na.rm = TRUE),
        has_kids = mean(has_kids, na.rm = TRUE),
        total_kids = mean(total_kids, na.rm = TRUE),
        REGION = "Total"
      ) |>
      distinct()
    
    GSS_regional_averages <- rbind(GSS_regional_averages, df)
    
    ### Regional Totals
    df <- GSS |>
      filter(YEAR == i) |>
      group_by(REGION) |>
      mutate(
        AGE = mean(AGE, na.rm = TRUE),
        HOMPOP = mean(HOMPOP, na.rm = TRUE),
        BABIES = mean(BABIES, na.rm = TRUE),
        PRETEEN = mean(PRETEEN, na.rm = TRUE),
        TEENS = mean(TEENS, na.rm = TRUE),
        ADULTS = mean(ADULTS, na.rm = TRUE),
        NATCRIME = mean(NATCRIME, na.rm = TRUE),
        NATMASS = mean(NATMASS, na.rm = TRUE),
        TRUST = mean(TRUST, na.rm = TRUE),
        FEAR = mean(FEAR, na.rm = TRUE),
        race_white = mean(race_white, na.rm = TRUE),
        race_black = mean(race_black, na.rm = TRUE),
        race_other = mean(race_other, na.rm = TRUE),
        female = mean(female, na.rm = TRUE),
        has_kids = mean(has_kids, na.rm = TRUE),
        total_kids = mean(total_kids, na.rm = TRUE)
      ) |>
      distinct()
    
    GSS_regional_averages <- rbind(GSS_regional_averages, df)
  }
  
  if(nrow(df) == 0){
    df <- data.frame(matrix(NA, 10, 18))
    names(df) <- names(GSS)
    
    df$REGION[1] <- "New England"
    df$REGION[2] <- "Middle Atlantic"
    df$REGION[3] <- "East North Central"
    df$REGION[4] <- "West North Central"
    df$REGION[5] <- "South Atlantic"
    df$REGION[6] <- "East South Atlantic"
    df$REGION[7] <- "West South Central"
    df$REGION[8] <- "Mountain"
    df$REGION[9] <- "Pacific"
    df$REGION[10]<- "Total"
    
    df$YEAR <- rep(i, 10)
    
    GSS_regional_averages <- rbind(GSS_regional_averages, df)
  }
    
}

### Export regional averages to folder
path <- "C:/Users/zades/Desktop/Transportation Project/"
write.csv(GSS_regional_averages, paste0(path, "gss_regional_averages.csv"))

### Export Modified GSS Data to Folder
### I might want to try some regressions on the non-averaged data
write.csv(GSS, paste0(path, "gss_regression_ready.csv"))
