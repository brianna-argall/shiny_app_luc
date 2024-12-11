#### Load the required packages ####
# if packages are not installed already,
# install them using function install.packages(" ")

library(tidyverse)
library(sf)
library(tmap)
library(readxl)
library(shiny) # shiny features
library(shinydashboard) # shinydashboard functions
library(shinycssloaders) # to add a loader while graph is populating
library(plotly)
library(reshape2)

## Importing the CSVs
get_census_data <- function(){
  # will need to change this to be generic
  folder_path <- "data/"
  
  file_prefix <- "2021Census_"
  file_suffix <- "_AUST_SA3.csv"
  
  file_infixes <- c("G19A", "G19B", "G19C",
                    "G20A", "G20B",
                    "G21A", "G21B", "G21C")
  
  # Obtain the file path for each file
  file_paths <- c()
  
  for(i in 1:length(file_infixes)) {
    file_paths[i] <- paste0(folder_path, 
                            file_prefix, 
                            file_infixes[i], 
                            file_suffix)
  }
  
  # Read each csv, assigning infix as name
  for(i in 1:length(file_paths)){
    assign(file_infixes[i], read.csv(file_paths[i]))
  }
  
  ## Combining dataframes
  # The first column of each is the SA3 Code - we do not want multiple copies
  
  # Type of Long-Term Health Condition by Age by Sex
  G19 <- cbind(G19A, G19B[,-1], G19C[,-1])
  
  # Count of Selected Long-Term Health Conditions by Age by Sex
  G20 <- cbind(G20A, G20B[,-1])
  
  # Type of Long-Term Health Condition by Selected Person Characteristics
  G21 <- cbind(G21A, G21B[,-1], G21C[,-1])
  
  # Remove the objects with A B or C from global environment
  rm(list = ls()[grep("A|B|C", ls())])
  return(list(G19 = G19, G20 = G20, G21 = G21))
}

# Stealing Dan's code:
get_AIHW_data <- function(){
  aihw <- read_excel("data/AIHW-PHC-17-Use-of-emergency-departments-for-lower-urgency-care-data-tables-2021-22.xlsx", 
                     sheet = "SA3 - Lower urgency")
  
  names(aihw) <- lapply(aihw[9, ], as.character)
  
  # Rename some more succinctly
  names(aihw)[3] <- "SA3_code"
  names(aihw)[4] <- "SA3_name"
  names(aihw)[5] <- "SA3_group"
  names(aihw)[6] <- "aihw_dem"
  names(aihw)[7] <- "luc_all_prop"
  names(aihw)[8] <- "luc_in_prop"
  names(aihw)[9] <- "luc_after_prop"
  names(aihw)[13] <- "SA3_popn"
  
  # Coerce from character to numeric class
  aihw$luc_all_prop <- as.numeric(aihw$luc_all_prop)
  aihw$luc_in_prop <- as.numeric(aihw$luc_in_prop)
  aihw$luc_after_prop <- as.numeric(aihw$luc_after_prop)
  aihw$SA3_popn <- as.numeric(aihw$SA3_popn)
  aihw$SA3_code <- as.integer(aihw$SA3_code)
  
  # Remove the "non-data" rows
  aihw <- aihw %>% filter(!row_number() %in% 1:9)
}

aihw <- get_AIHW_data()

census_ages_conv <- function(sex, cond, df){
  # Aggregates age columns for a sex_condition combination from a census df
  # Outputs a dataframe with the requested sex_condition_age columns
  # such that the age ranges match the age ranges in the AIHW data
  
  sex_cond <- paste(sex, cond, sep = "_")
  var_name_25_44 <- paste(sex_cond,"25_44", sep = "_")
  var_name_45_64 <- paste(sex_cond,"45_64", sep = "_")
  var_name_65_up <- paste(sex_cond,"65_up", sep = "_")
  
  
  df %>%
    select(SA3_CODE_2021,contains(sex_cond)) %>%
    # 25_64 = 25_34 + 35_44
    mutate(!!var_name_25_44 := 
             rowSums(select(., matches("25|35"))),
           # 45_64 = 45_54 + 55_64
           !!var_name_45_64 :=
             rowSums(select(., matches("45|55"))),
           # 65_up = 65_74 + 75_84 + 85_over
           !!var_name_65_up := 
             rowSums(select(., matches("65|75|85"))))
}

## Import SA3 shapefile
get_SA3 <- function() {
  st_read("data/SA3_2021_AUST_GDA2020.shp")
}

## Joing df and shapefile
get_df_geo <- function(df, SA3 = SA3) {
  df %>%
    #drop_na(M_0_cond_0_14) %>%
    mutate(SA3_CODE21 = as.character(SA3_CODE_2021)) %>%
    # having SA3 as first argument means output is sf, not just df
    inner_join(SA3, ., by = "SA3_CODE21") %>%
    st_difference()
}

## Creating vectors for UI
col_labeller <- function(string) {
  # Not intended for use with G21
  string %>%
    str_replace("^M", "Males") %>%
    str_replace("^F", "Females") %>%
    str_replace("^P", "People") %>%
    str_replace("cond", "conditions") %>%
    str_replace("Tot$", "all_ages") %>%
    str_replace_all("_"," ") %>%
    str_replace("mo", "more") %>%
    str_replace("Tot", "total") %>%
    str_replace("1m", "1 or more") %>%
    str_replace("1 conditions", "1 condition")
}

# to get G20 columns
get_G20_cols <- function(G20){
  G20 %>% select(-SA3_CODE_2021) %>% colnames()
}


## Data processing


G20_conds <- c("0_cond", "1_cond", "2_cond", "3_or_mo_cond", "cond_NS", "Tot") 
G19_conds <- c("Arthritis", "Asthma", "Cancer", "Dementia", "Diabetes", 
               "Heart_disease", "Kidney_disease", "Lung_cond", "Stroke", "Other", "None", "NS", "Tot")


census_prop_maker <- function(sex, cond, age){
  # requires dataframe containing the target variable and total for that
  # age_cond stratum. Outputs dataframe with prop added
  
  age_sex_tot <- paste(sex, "Tot", age, sep = "_")
  var <- paste(sex, cond, age, sep = "_")
  var_prop <- paste(var, "prop", sep = "_")
  
  if(cond %in% G19_conds){
    
    new_ages_G19 %>%
      # The !!, sym() and := instead of = are due to tidy evaluation
      # for the purpose of understanding you can ignore these
      
      # Present as per 1000
      mutate(!!var_prop := 1000 * !!sym(var) / !!sym(age_sex_tot))
  } else {
    
    new_ages_G20 %>%
      # Present as %
      mutate(!!var_prop := 100 * !!sym(var) / !!sym(age_sex_tot))
  }
  
}

# get the census data
data <- get_census_data()
G19 <- data$G19
G20 <- data$G20
G21 <- data$G21

# Make new df based on G19/G20 with correct age categories
census_ages_to_AIHW <- function(df, conds) {
  # unlike census ages conv (used in this function) the purpose is to output
  # an entire dataframe. 
  
  result <- df %>% select(SA3_CODE_2021, matches("14|24|Tot"))
  
  for(sex in c("F", "M", "P")){
    for(cond in conds){
      new_cols <- census_ages_conv(sex = sex, cond = cond, df = df) %>%
        select(SA3_CODE_2021, matches("25_44|45_64|65_up"))
      result <- left_join(result, new_cols)
    }
  }
  
  return (result)
}

new_ages_G19 <- census_ages_to_AIHW(G19, G19_conds)
new_ages_G20 <- census_ages_to_AIHW(G20, G20_conds)

# now the proportions conversions
get_prop <- function(df, conds) {
  ages <- c("0_14", "15_24", "25_44", "45_64", "65_up")
  
  result <- df %>% select(matches("SA3"))
  
  for(sex in c("F", "M", "P")){
    for(cond in conds){
      for(age in ages) {
        new_cols <- census_prop_maker(sex = sex, cond = cond, 
                                      age = age) %>%
          select(matches("SA3|prop"))
        result <- left_join(result, new_cols)
        
      }
    }
  }
  
  return (result)
}

props_G20 <- get_prop(new_ages_G20, G20_conds)

# for some reason it breaks on MH for F 0-14  
# an issue in the earlier join - I think there might be a weird value
cond_sub <- G19_conds[!G19_conds %in% c("Mental_Health_Cond", "Tot")]
props_G19 <- get_prop(new_ages_G19, cond_sub)

# get SA3
SA3 <- get_SA3()

## Seeing if combining G19 and G20 fixes issue
census_props <- left_join(props_G19, props_G20)
census_geo <- get_df_geo(census_props, SA3)

gender <- list("All" = "P", "Male" = "M", "Female" = "F")
ageGroup <- list("0-14"="0_14", "15-24"="15_24", "25-44"="25_44", "45-64"="45_64", "65 +"="65_up")
condition <- G19_conds
states <- list("New South Wales" = "New South Wales", "Victoria" = "Victoria", "Queensland" = "Queensland", "South Australia" = "South Australia",
               "Western Australia" = "Western Australia", "Tasmania" = "Tasmania", "Northern Territory" = "Northern Territory", "Other Territories" = "Other Territories")
g19_or_g20 <- list("Specific conditions" = "G19", "Number of conditions" = "G20")

states2 <- c("Qld", "NSW", "ACT", "Vic", "Tas", "SA", "WA", "NT")

ageGroup2 <- c("0 to 14 yrs" = "0_14","15 to 24 yrs" = "15_24", "25 to 44 yrs" = "25_44","45 to 64 yrs" = "45_64", "Over 65 yrs" = "65_up", "All Ages" = "Tot")

hours <- c("All-hours" = "all", "In-hours" = "in", "After-hours" = "after")

g19_condition <- c("Arthritis", "Asthma", "Cancer", "Dementia", "Diabetes", "Heart Disease" = "Heart_disease", 
  "Kidney Disease" = "Kidney_disease", "Lung Disease (not asthma)" = "Lung_cond", 
  "Mental Health Condition" = "Mental_health_cond", "Stroke", "Other", "No Chronic Condition" = "None")

# List of chronic medical conditions to be analyzed
cond_names <- c("Arthritis", "Asthma", "Cancer", "Dementia", 
                "Diabetes", "Heart_disease", "Kidney_disease", 
                "Lung_cond", "Mental_health_cond", "Stroke")
