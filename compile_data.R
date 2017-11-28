library(tidyverse)

Sys.setlocale("LC_ALL", "pt_BR")

# Functions
read_coverage <- function(file_path) { 
  
  file_scan <- scan(file_path, what = "character", sep = "\n")
  
  file_year <- parse_number(file_scan[[3]])
  
  tab_start_end <- grep("^\"Imuno|^\"Total", file_scan, ignore.case = TRUE)
  
  read_csv2(file_path, 
            skip = tab_start_end[1] - 1, 
            n_max =  diff(tab_start_end) - 1, 
            na = c("", "NA", "-", "..."),
            locale = locale(decimal_mark = ",", encoding = "latin1")) %>%
    select(-Total) %>%
    gather(region, coverage, -1) %>%
    extract(region, "region", "\\d+ [^ ]+ ([^ ]+)") %>%
    mutate(year = file_year) %>%
    select(year, region, vaccine = Imuno, coverage) %>%
    arrange(year, region, vaccine)
}

read_doses <- function(file_path) {
 
  file_scan <- scan(file_path, what = "character", sep = "\n")
  
  file_year <- parse_number(file_scan[[3]])
  
  tab_start_end <- grep("^\"Imunobiol|^\"Total", file_scan, ignore.case = TRUE)
   
  read_csv2(file_path, 
            skip = tab_start_end[1] - 1, 
            n_max = diff(tab_start_end) - 1,
            na = c("", "NA", "-", "..."),
            locale = locale(decimal_mark = ",", encoding = "latin1")) %>%
    select(-Total) %>%
    gather(region, doses, -1) %>%
    extract(region, "region", "\\d+ [^ ]+ ([^ ]+)") %>%
    mutate(year = file_year) %>%
    select(year, region, vaccine = 1, doses) %>%
    arrange(year, region, vaccine)
}

# Data

## Coverage
file_names_coverage <- 
  list.files("./Cobertura 1994-2017", 
             pattern = "^A15.+_39\\.csv", 
             full.names = TRUE)

coverage_df <- map_df(file_names_coverage, read_coverage) %>%
  arrange(year, region, vaccine)

write_delim(coverage_df, "./coverage_compiled.csv", delim = ";")

# Doses
file_names_doses <- 
  list.files("./Doses aplicadas 1994-2017/", 
             pattern = "^A16.+_39\\.csv",
             full.names = TRUE)

doses_df <- map_df(file_names_doses, read_doses) %>%
  arrange(year, region, vaccine)

write_delim(doses_df, "./doses_compiled.csv", delim = ";")

# temp
coverage_df %>%
  distinct(vaccine) %>%
  arrange(vaccine) %>%
  write_delim("./coverage_vaccine_types.csv", col_names = FALSE, delim = ";")

doses_df %>%
  distinct(vaccine) %>%
  arrange(vaccine) %>%
  write_delim("./doses_vaccine_types.csv", col_names = FALSE, delim = ";")
