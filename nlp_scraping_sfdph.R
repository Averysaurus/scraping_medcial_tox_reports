# The following code scrapes ocr'ed .txt from CC San Francisco medical examiner documents for all standardied variables, including the case narrative language. Note that optical character recognition (OCR) can leave errors in the text it imports, so check the data source with a command-f and the term you're looking for and see if there's errors. You may also command-f index values to locate potential bugs or asymetries, this code functions on 1044 sample documents, circa 2005-2012. ~AR


library(plyr)
library(tidyverse)
library(pdftools)
library(fs)

# import text document, set file path to folder.
PATH <- setwd("FILE PATH TO ALL THE TEXT DOCS")

file_paths <- fs::dir_ls(PATH)
file_contents <- list()

# function to import all files in the folder, should show up in global environment.  
for (i in seq_along(file_paths)){
  file_contents[[i]] <- readr::read_file(
    file_paths[[i]]
  )
}

# unlist file data. 
file_contents_unlist <- paste(unlist(file_contents), collapse = " ")

# create lines to parse strings into lines.
file_contents_lines <- 
  file_contents_unlist %>% 
  readr::read_lines() %>% 
  str_squish()


## CASE NUMBERS

### index variables
index_case_num_1 <- which(grepl("(Case#: \\d+[-]\\d+)", 
              file_contents_lines))
index_case_num_2 <- which(grepl("(Case#: \\d+[-]\\d+)", 
               file_contents_lines))

# function to index case numbers. 
pull_case_num <- 
  function(index_case_num_1, index_case_num_2){
    (file_contents_lines[index_case_num_1:index_case_num_2]
    )
  } 

# function to extract case numbers
case_nums <- map2(index_case_num_1, 
                  index_case_num_2, 
                  pull_case_num) 
case_nums_df <- as.data.frame.character(case_nums)

# parse case numbers from text
 case_nums_fin <- 
    case_nums_df %>% 
    mutate(case_nums = as.character(case_nums)) %>% 
    mutate(case_nums = str_extract_all(case_nums, 
                    '(\\d+[-]\\d+)')) %>% 
    mutate(case_nums = str_trim(case_nums)) %>% 
    mutate(case_nums = case_when(
         case_nums == "" ~ NA_character_, 
         TRUE ~ case_nums)) %>% 
    drop_na(case_nums) %>% 
    distinct(case_nums) 

 
## The essential pattern will be repeated with all other variables: 
## index, extract, and parse.

# index 
index_case_hist_1 <- 
  which(grepl("CASE HISTORY", file_contents_lines))
index_case_hist_2 <- 
  which(grepl("INVESTIGATOR: ", file_contents_lines))

# extract
pull_case_hist <- function(index_case_hist_1, 
                           index_case_hist_2 )
{(file_contents_lines[index_case_hist_1:index_case_hist_2])} 

case_hist <- map2(index_case_hist_1, 
                  index_case_hist_2, 
                  pull_case_hist)
case_hist_df <- as.data.frame.character(case_hist)

# parse
case_hist_fin <-
  case_hist_df %>% 
  mutate(case_hist = as.character(case_hist)) %>% 
  mutate(case_hist = str_remove_all(case_hist, 
                     "(CASE HISTORY)")) %>% 
  mutate(case_hist = str_remove_all(case_hist, 
                     "(INVESTIGATOR: (.*))")) %>%
  mutate(case_hist = str_remove_all(case_hist, 
                     "(MEDICAL EXAMINER)")) %>%
  mutate(case_hist = str_remove_all(case_hist, 
                     "((Case#: \\d+\\D+\\d+))")) %>% 
  mutate(case_hist = str_remove_all(case_hist,
                     "(INVESTIGATOR'S REPORT)")) %>% 
  mutate(case_hist = str_remove_all(case_hist, 
                     "(CITY AND COUNTY OF SAN FRANCISCO - RECORD OF DEATH)")) %>% 
  mutate(case_hist = gsub('[(\"]+','',case_hist)) %>% 
  mutate(case_hist = str_remove_all(case_hist, 
                                    "(c,)"))

## AGE OF DECEDENT

# index
index_age_1 <- which(grepl("Age: ", file_contents_lines))
index_age_2 <- which(grepl("Age: ", file_contents_lines))

pull_age <- function(index_age_1, index_age_2 )
{(file_contents_lines[index_age_1:index_age_2])} 

# extract
age_var <- map2(index_age_1, 
                index_age_2, 
                pull_age)
age_var_df <- as.data.frame.character(age_var)

# parse
age_var_fin <- 
  age_var_df %>% 
  mutate(age_var = as.character(age_var)) %>% 
  mutate(age_var = str_extract_all(age_var, "Age: \\d+")) %>% 
  mutate(age_var = str_remove_all(age_var, "Age: "))



# SEX OF DECEDENT

# index
index_sex_1 <- which(grepl("Age: ", file_contents_lines))
index_sex_2 <- which(grepl("Age: ", file_contents_lines))

# extract
pull_sex <- function(index_sex_1, index_sex_2 )
{(file_contents_lines[index_sex_1:index_sex_2])} 

sex_var <- map2(index_sex_1, 
                index_sex_2, 
                pull_sex)
sex_var_df <- as.data.frame.character(sex_var)

# parse
sex_var_fin <- 
  sex_var_df %>% 
  mutate(sex_var = as.character(sex_var)) %>% 
  mutate(sex_var = str_extract_all(sex_var, "Sex: \\w+")) %>% 
  mutate(sex_var = str_remove_all(sex_var, "Sex: "))


## RACE of decedent. 

#index
index_race_1 <- which(grepl("PLACE OF DEATH", file_contents_lines))
index_race_2 <- which(grepl("POLICE NOTIFIED ", file_contents_lines))

# extract
pull_race <- function(index_race_1, index_race_2 )
{(file_contents_lines[index_race_1:index_race_2])} 

race_var <- map2(index_race_1, 
                 index_race_2, 
                 pull_race)
race_var_df <- as.data.frame.character(race_var)

# parse
race_var_fin <- 
  race_var_df %>% 
  mutate(race_var = as.character(race_var)) %>% 
  mutate(race_var = str_extract(race_var, 'Race: (.*)"')) %>% 
  mutate(race_var = str_remove_all(race_var, '"(.*)' )) %>% 
  mutate(race_var = str_remove_all(race_var, 'Race: ' )) 


# DATE/TIME OF MORTALITY EVENT

# index
dt_death_1 <- 
  which(grepl("Date/Time of Death: ", file_contents_lines))
dt_death_2<- 
  which(grepl("Date/Time of Death: ", file_contents_lines))

pull_dt_death <- function(dt_death_1, dt_death_2 )
{(file_contents_lines[dt_death_1:dt_death_2])} 

# extract
dt_death_var <- map2(dt_death_1, 
                     dt_death_2, 
                     pull_dt_death)
dt_death_var_df <- as.data.frame.character(dt_death_var)

# parse
dt_death_var_fin <- 
  dt_death_var_df %>% 
  mutate(dt_death_var = as.character(dt_death_var)) %>% 
  mutate(dt_death_var = str_extract_all(dt_death_var, 
                                        "Date/Time of Death: (.*)M")) %>% 
  mutate(dt_death_var = str_remove_all(dt_death_var, 
                                       "Date/Time of Death: ")) %>% 
  separate(dt_death_var, c("death_date", "death_time", "ampm"),
           sep = " ") %>% 
  unite(time_of_death, death_time, ampm, sep = " ") %>% 
  mutate(time_of_death = str_replace(time_of_death,
                                     "PM NA", " PM")) %>% 
  mutate(time_of_death = str_replace(time_of_death,
                                     "AM NA", " AM"))


## LOCATION OF MORTALITY EVENT

# index
place_death_1 <- 
  which(grepl("PLACE OF DEATH ", file_contents_lines))
place_death_2<- 
  which(grepl("Age: ", file_contents_lines))

pull_place_death  <- function(place_death_1, place_death_2 )
{(file_contents_lines[place_death_1:place_death_2])} 

# extract
place_death_var <- map2(place_death_1, 
                        place_death_2, 
                        pull_place_death)
place_death_var_df <- as.data.frame.character(place_death_var)

# parse
place_death_var_fin <- 
  place_death_var_df %>% 
  mutate(place_death_var = as.character(place_death_var)) %>% 
  mutate(place_death_var = str_extract_all(place_death_var, 
                           "PLACE OF DEATH (.*)(\\sAge)")) %>% 
  mutate(place_death_var = str_remove_all(place_death_var, 
                           " Age")) %>% 
  mutate(place_death_var = str_remove_all(place_death_var, 
                           "PLACE OF DEATH ")) 


# ID'd BY VARIABLE

# index
index_ident_1 <- 
  which(grepl("IDENTIFIED BY: ", file_contents_lines))
index_ident_2<- 
  which(grepl("AT: ", file_contents_lines))

pull_ident <- function(index_ident_1, index_ident_2 )
{(file_contents_lines[index_ident_1:index_ident_2])} 

# extract
ident_var <- map2(index_ident_1, 
                  index_ident_2, 
                  pull_ident)
ident_var_df <- as.data.frame.character(ident_var)

# parse
ident_var_fin <- 
  ident_var_df %>% 
  mutate(ident_var = as.character(ident_var)) %>% 
  mutate(ident_var = str_extract_all(ident_var, 
                    "IDENTIFIED BY: (.*)(\\sAT:)")) %>% 
  mutate(ident_var = str_remove_all(ident_var, 
                                    "AT:")) %>% 
  mutate(ident_var = str_remove_all(ident_var, 
                                    "IDENTIFIED BY: ")) 


# DATE ID'd

# index
index_date_ided_1 <- 
  which(grepl("IDENTIFIED BY: ", file_contents_lines))
index_date_ided_2<- 
  which(grepl("AT: ", file_contents_lines))

pull_date_ided <- function(index_date_ided_1 , index_date_ided_2 )
{(file_contents_lines[index_date_ided_2:index_date_ided_2])} 

# extract
date_ided_var <- map2(index_date_ided_1, 
                      index_date_ided_2, 
                      pull_date_ided)
date_ided_var_df <- as.data.frame.character(date_ided_var)

# parse
date_ided_var_fin <- 
  date_ided_var_df %>% 
  mutate(date_ided_var = as.character(date_ided_var)) %>% 
  mutate(date_ided_var = str_extract_all(date_ided_var, 
                                         "DATE: (.*)")) %>% 
  mutate(date_ided_var = str_remove_all(date_ided_var, 
                                        "DATE: "))

# NATURE VARIABLE

# index
index_nature_1 <- 
  which(grepl("NATURE:", file_contents_lines))
index_nature_2<- 
  which(grepl("CASE HISTORY", file_contents_lines))

pull_nature<- function(index_nature_1 , index_nature_2 )
{(file_contents_lines[index_nature_1:index_nature_2])} 

# extract
nature_var <- map2(index_nature_1, 
                   index_nature_2, 
                   pull_nature)
nature_var_df <- as.data.frame.character(nature_var)

# parse
nature_var_fin <- 
  nature_var_df %>% 
  mutate(nature_var = as.character(nature_var)) %>% 
  mutate(nature_var = str_extract_all(nature_var, 
                                      "NATURE:(.*),")) %>% 
  mutate(nature_var = str_remove_all(nature_var, 
                                     "NATURE:")) %>% 
  mutate(nature_var = str_remove_all(nature_var, 
                                     '\\Q",\\E')) %>% 
  mutate(nature_var = str_remove_all(nature_var, 
                                     '"'))

# IDENTIFIED BY 

# index
index_ided_at_1 <-
  which(grepl("IDENTIFIED BY: ", file_contents_lines))
index_ided_at_2<-
  which(grepl("AT: ", file_contents_lines))

pull_ided_at <- function(index_ided_at_1, index_ided_at_2)
{(file_contents_lines[index_ided_at_1:index_ided_at_2])}

# extract
ided_at_var <- map2(index_ided_at_1,
                    index_ided_at_2,
                    pull_ided_at)
ided_at_var_df <- as.data.frame.character(ided_at_var)

# parse
ided_at_var_fin <-
  ided_at_var_df %>%
  mutate(ided_at_var = as.character(ided_at_var)) %>%
  mutate(ided_at_var = str_extract_all(ided_at_var,
                                       "AT: (.*)(\\sDATE:)")) %>%
  mutate(ided_at_var = str_remove_all(ided_at_var,
                                      "DATE:")) %>% 
  mutate(ided_at_var = str_remove_all(ided_at_var,
                                      "AT: "))

# SFPD CASE NUMBER

# index
index_pd_case_1 <-
  which(grepl("SFPD CASE#:", file_contents_lines))
index_pd_case_2<-
  which(grepl("SFPD CASE#:", file_contents_lines))

pull_pd_case <- function(index_pd_case_1, index_pd_case_2)
{(file_contents_lines[index_pd_case_1:index_pd_case_2])}

# extract
pd_case_var <- map2(index_pd_case_1,
                    index_pd_case_2,
                    pull_pd_case)
pd_case_var_df <- as.data.frame.character(pd_case_var)

# parse
pd_case_var_fin <-
  pd_case_var_df %>%
  mutate(pd_case_var = as.character(pd_case_var)) %>%
  mutate(pd_case_var = str_remove_all(pd_case_var,
                                      "SFPD CASE#:")) 


# bind all variables into dataframe
cases_comp <- cbind(case_nums_fin, 
                      age_var_fin,
                      sex_var_fin,
                      race_var_fin,
                      dt_death_var_fin,
                      place_death_var_fin,
                      ident_var_fin,
                      date_ided_var_fin,
                      ided_at_var_fin,
                      nature_var_fin,
                      pd_case_var_fin,
                      case_hist_fin)

# Write data frame as csv
write_csv(cases_comp_2, "me_toxevents.csv")


# ~fin








