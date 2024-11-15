######################################################
# Script to generate data download from IPUMS website
######################################################

# Set-up
setwd("C:/Users/Documents/") # Set working directory to correspond to where you want to save relevant files
outputs <- "MAIHDA alcohol/outputs/"

# Read in necessary packages
library(ipumsr)
library(tidyverse)

#### Option 1: Download the data via the IPUMS API 
# Obtain an API key by visiting https://account.ipums.org/api_keys.
# Save key in .Renviron for use across sessions
set_ipums_api_key("paste-your-key-here", save = TRUE)

# Generate data extract 
nhis_data_extract <- define_extract_micro(
  "nhis",
  "nhis extract for MAIHDA of alcohol consumption",
  samples = c("ih2000", "ih2001", "ih2002", "ih2003","ih2004", "ih2005", "ih2006", "ih2007", "ih2008", "ih2009",
              "ih2010", "ih2011","ih2012", "ih2013","ih2014", "ih2015","ih2016", "ih2017","ih2018"),
  variables = c("YEAR", "INTERVWMO", "INTERVWYR", "NHISPID", "PERWEIGHT", "SAMPWEIGHT", "AGE", "BIRTHYR", 
                "SEX", "RACENEW", "SEXORIEN", "EDUCREC2", "HISPYN", "USBORN", "CITIZEN", "INCFAM97ON2", 
                "ALCSTAT1", "ALCSTAT2", "ALCAMT", "ALCDAYSYR", "ALC5UPYR")
)

# save the extract
save_extract_as_json(nhis_data, file = file.path(outputs, "nhis_data_extract.json"))

# obtain the data from the extract
data <- nhis_data_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract() %>%
  read_ipums_micro()

# save the data
saveRDS(data, paste0(inputs, "nhis_data.RDS"))

#### Option 2: Manually download data from IPUMS website

# Lynn A. Blewett, Julia A. Rivera Drew, Miriam L. King, Kari C.W. Williams, Natalie Del Ponte and Pat Convey. IPUMS Health Surveys: National Health Interview Survey, Version 7.1 [dataset]. Minneapolis, MN: IPUMS, 2021). 
# Available at: https://doi.org/10.18128/D070.V7.1)
# Manually select variables needed (as below) for years needed (2000-2018)
# YEAR, INTERVWMO, INTERVWYR, NHISPID, PERWEIGHT, SAMPWEIGHT, 
# AGE, BIRTHYR, SEX, SEXORIEN, EDUCREC2, RACENEW, HISPYN, USBORN, CITIZEN, INCFAM97ON2
# ALCSTAT1, ALCSTAT2, ALCAMT, ALCDAYSYR, ALC5UPYR

# Read in your created data extract
ddi <- read_ipums_ddi(paste0(inputs, "/raw_data/file_name.xml"))
data <- read_ipums_micro(ddi)

# save the data
saveRDS(data, paste0(inputs, "nhis_data.RDS"))
