##############################################################################
# Process raw nhis data
##############################################################################

# Set-up
setwd("C:/Users/Documents/") # Set working directory to correspond to where you want to save relevant files
code <- "MAIHDA alcohol/"
inputs <- "MAIHDA alcohol/inputs/"
models <- "MAIHDA alcohol/models/"
outputs <- "MAIHDA alcohol/outputs/"

# Read in necessary R packages
library(tidyverse)
library(readr)
library(tidyr)
library(dplyr)
library(ipumsr) 
library(labelled)
library(mice)

# Source required functions
source(paste0(code,"functions/recode_to_NA_all.R"))
source(paste0(code,"functions/remove_na.R"))
source(paste0(code,"functions/generate_ALCSTAT.R"))
source(paste0(code,"functions/assign_grams_alcohol.R"))
source(paste0(code,"functions/recode_alc_cats.R"))
source(paste0(code,"functions/recode_race_ethnicity.R"))
source(paste0(code,"functions/recode_income.R"))
source(paste0(code,"functions/recode_education.R"))
source(paste0(code,"functions/recode_age.R"))
source(paste0(code,"functions/recode_sexorien.R"))
source(paste0(code,"functions/recode_cohort.R"))

# Set generic settings
theme_set(theme_bw(base_size = 12)) # set default theme for plots
options(scipen = 100) # bias towards non scientific notation

# Read in the data
data <- readRDS(paste0(inputs, "nhis_data.RDS"))

# Exclude individuals with alcohol questions Not In Universe (Universe = Sample adults aged 18+)
 nhis_subset <- data %>%
    filter(ALCSTAT1 != 0) 

# Create data dictionary for reference
dictionary <- labelled::generate_dictionary(nhis_subset)
 
# Zap labels from the dataframe to facilitate data manipulation 
nhis_subset <- nhis_subset %>% zap_formats() %>% zap_labels()
 
# Convert all types of NA to be consistent i.e. convert 'refused', 'not ascertained', 'don't know', 'inconsistent' and 'NIU' to NA
nhis_subset_converted_NA <- recode_to_NA_all(nhis_subset)

# Drop individuals aged <21
nhis_21 <- nhis_subset_converted_NA %>% filter(AGE>=21) 

# Recode race into 6 categories
nhis_subset_race <- recode_race_ethnicity_all(nhis_21)

# Keep only the 6 selected race and ethnicity groups
nhis_subset_race_6 <- nhis_subset_race %>% filter(!is.na(race_6_cats)) 

# Recode age
nhis_subset_age <- recode_age(nhis_subset_race_6)
nhis_subset_age$age_3_cats <- factor(nhis_subset_age$age_3_cats,
                                                 levels = c(1,2,3),
                                                 labels = c("18-24", "25-69", "70+"))
nhis_subset_age$age_diaz <- factor(nhis_subset_age$age_diaz,
                                                   levels = c(0,1,2,3),
                                                   labels = c("18-20", "21-24", "25-59","60+"))
# Drop individuals aged <21
nhis_21 <- nhis_subset_age %>% filter(AGE>=21)

## Recategorise age, sexual orientation, income, education and birth cohort

nhis_subset_sexorien <- recode_sexorien(nhis_subset_age)
nhis_subset_sexorien$SEXORIEN <- factor(nhis_subset_sexorien$SEXORIEN,
                              levels = c(1,2),
                              labels = c("Heterosexual", "Homosexual/bisexual/something else"))

nhis_subset_income <- recode_income(nhis_subset_sexorien)
nhis_subset_income$income <- factor(nhis_subset_income$income,
                    levels = c(1,2,3,4),
                    labels = c("$0 - $34,999", "$35,000-$74,999", "$75,000-$99,999","$100,000 and over"))

nhis_subset_education <- recode_education(nhis_subset_income)
nhis_subset_education$education_3_cats <- factor(nhis_subset_education$education_3_cats,
                                    levels = c(1,2,3),
                                    labels = c("high school or less", "some college", "4+ years college"))
nhis_subset_education$education_4_cats <- factor(nhis_subset_education$education_4_cats,
                                    levels = c(1,2,3,4),
                                    labels = c("no high school (<= grade 8)", "some high school or high school graduate", "some college", "4+ years college"))
nhis_subset_education$education_5_cats <- factor(nhis_subset_education$education_5_cats,
                                    levels = c(1,2,3,4,5),
                                    labels = c("no high school (<= grade 8)", "Some high school (grades 9-11)", "Finished high school (grade 12)", "Some college", "4+ years college"))

nhis_subset_decade <- nhis_subset_education %>% mutate(
      decade = case_when(
      YEAR == 2000 | YEAR == 2001 | YEAR == 2002 | YEAR == 2003 | YEAR == 2004 |
      YEAR == 2005 | YEAR == 2006 | YEAR == 2007 | YEAR == 2008 | YEAR == 2009 ~ 1, 
      YEAR == 2010 | YEAR == 2011 | YEAR == 2012 | YEAR == 2013 | YEAR == 2014 | 
      YEAR == 2015 | YEAR == 2016 | YEAR == 2017 | YEAR == 2018  ~ 2))
nhis_subset_decade$decade <- factor(nhis_subset_decade$decade,
                                          levels = c(1,2),
                                          labels = c("2000-2009","2010-2018"))

nhis_subset_recoded <- nhis_subset_decade

# Review demographics of the sample adult subset 
sample_adults_by_sex_race_age <- nhis_subset_recoded %>% 
  count(SEX, age_3_cats, race_6_cats) %>%
  mutate(percent = n/sum(n)*100) %>%
  group_by(SEX) %>%
  arrange(desc(percent), .by_group = TRUE)

# Review missing data
nhis_subset_recoded %>%
  dplyr::select(ALCSTAT1, age_3_cats, race_6_cats, SEX, education_3_cats, income) %>%
  md.pattern(rotate.names = TRUE)

missing_data <- nhis_subset_recoded %>% 
  summarise(
    total_pop = n(),  
    SEX_NA = sum(is.na(SEX)),
    Age_NA = sum(is.na(AGE)), 
    Educ_NA = sum(is.na(education_3_cats)),
    Sex_orien_NA = sum(is.na(SEXORIEN)),
    Income_NA = sum(is.na(income)),
    Alc_status_NA = sum(is.na(ALCSTAT1)),
    race_NA = sum(is.na(race_6_cats)),
    perc_missing_sex = (SEX_NA / total_pop) * 100,
    perc_missing_age = (Age_NA / total_pop) * 100,
    perc_missing_edu = (Educ_NA / total_pop) * 100,
    perc_missing_sex_orien = (Sex_orien_NA / total_pop) * 100,
    perc_missing_income = (Income_NA / total_pop) * 100,
    perc_missing_Alc_status = (Alc_status_NA / total_pop) * 100,
    perc_missing_race = (race_NA / total_pop) * 100) %>%
  dplyr::select(perc_missing_sex, perc_missing_income, perc_missing_edu, perc_missing_sex_orien, perc_missing_race, perc_missing_Alc_status) %>%
  unique()

# Review missing alcohol consumption patterns data for drinkers, by intersectional group:
drinkers_missing_consumption <- nhis_subset_recoded %>% 
  filter(ALCSTAT1==3) %>% 
  group_by(SEX, age_3_cats, race_6_cats, education_3_cats) %>%
  mutate(n=1) %>%
  summarise(total_pop = sum(n), 
            ALC_AMT_NA = sum(is.na(ALCAMT)),
            ALC_over5_NA = sum(is.na(ALC5UPYR)),
            ALC_DAYS_NA = sum(is.na(ALCDAYSYR)),
            perc_missing_ALC_amt = (ALC_AMT_NA/total_pop)*100,
            perc_missing_ALC_over5 = (ALC_over5_NA/total_pop)*100,
            perc_missing_ALC_days_yr = (ALC_DAYS_NA/total_pop)*100) %>%
    arrange(perc_missing_ALC_amt)

# Drop individuals missing essential data (education & alc status)
edu_variables <- c("education_3_cats", "education_4_cats", "education_5_cats")
nhis_subset_dropped_edu_na <- remove_na(nhis_subset_recoded, all_of(edu_variables)) 
nhis_subset_dropped_alcstat_na <- remove_na(nhis_subset_dropped_edu_na, "ALCSTAT1")

# Drop individuals who drink, but who are missing information on consumption patterns
drinkers <- subset(nhis_subset_dropped_alcstat_na, ALCSTAT1==3)
non_drinkers <- subset(nhis_subset_dropped_alcstat_na, ALCSTAT1==1| ALCSTAT1==2)
variables <- c("ALCAMT","ALC5UPYR","ALCDAYSYR")
drinkers_dropped_na <- remove_na(drinkers, variables)
nhis_subset_dropped_alc_na <- rbind(drinkers_dropped_na, non_drinkers)

# Estimate average grams of alcohol use per person and assign as a new variable ("alc_daily_g")
#(Expanded Quantity/Frequency (QF) Approach & assuming standard drink size)
nhis_subset_grams_alc <- assign_grams_alcohol(nhis_subset_dropped_alc_na)

# Create sub-categories of alcohol use based on average alcohol consumption (grams):
nhis_subset_alc_cats <- recode_alc_cats(nhis_subset_grams_alc)

nhis_subset_alc_cats$ALCSTAT1 <- factor(nhis_subset_alc_cats$ALCSTAT1,
                                      levels = c(1,2,3),
                                      labels = c("Lifetime abstainer", "Former drinker", "Current drinker"))

# Convert sex to a factor
nhis_subset_alc_cats$SEX <- factor(nhis_subset_alc_cats$SEX,
                                      levels = c(1,2),
                                      labels = c("Male", "Female"))

## Review consistency of alcohol data
# Classified as inconsistent if:
# a) individuals reporting number of days drinking 5 units+ as more than the total number of days drinking
# b) individuals report drinking > 5 drinks on average, but also report never drinking more than 5 drinks

# Add column with a binary variable for whether alc. information is missing or not
data_with_inconsistancies <- nhis_subset_alc_cats %>%
  mutate(inconsistent_alc = if_else(
    ALC5UPYR > ALCDAYSYR | ALC5UPYR ==0 & ALCAMT >= 5, 1, 0))

# Calculate number of drinkers with inconsistent alcohol information and calculate as a % of total drinkers
inconsistent_drinkers_n <- data_with_inconsistancies %>% 
  filter(ALCSTAT1=="Current drinker" & inconsistent_alc==1) %>%
  nrow()

consistent_drinkers_n <- data_with_inconsistancies %>% 
  filter(ALCSTAT1=="Current drinker" & inconsistent_alc==0) %>%
  nrow()

inconsistent_drinkers_n/(inconsistent_drinkers_n + consistent_drinkers_n)*100

# Drop people with inconsistent alcohol data
nhis_alc_clean <- data_with_inconsistancies %>% filter(inconsistent_alc==0)

# Cap consumption at 200grams
nhis_alc_clean <- nhis_alc_clean %>%
    mutate(alc_daily_g_capped_200 = if_else(alc_daily_g > 200, 200, alc_daily_g))

# Check distribution of gpd data for full sample
ggplot(nhis_alc_clean, aes(x=alc_daily_g_capped_200), y) + 
  geom_histogram() + 
  xlab("Daily grams of alcohol") +
  ylab("Frequency") +
ggtitle("Raw distribution of daily grams alcohol, capped, all sample adults")
ggsave(paste0(outputs,"analytic sample/raw_distribution_daily_grams_full_sample.png"), dpi=300, width=33, height=19, units="cm")

# Consider data transformation for alc daily grams
nhis_alc_clean <- nhis_alc_clean %>% 
  mutate(new_grams = alc_daily_g_capped_200 + 0.02) # add half of the smallest grams value (for drinkers) to zero values
# Check new variable
mean_new_grams <- nhis_alc_clean %>% 
  group_by(SEX, age_diaz, race_6_cats, education_3_cats) %>% 
  summarise(mean = mean(new_grams))

# Check recommended lambda with boxcox
b <- MASS::boxcox(lm(nhis_alc_clean$new_grams ~ 1))
lambda <- b$x[which.max(b$y)]
lambda2 <- forecast::BoxCox.lambda(nhis_alc_clean$new_grams)  

# As both suggested lambda are close to 0, log transform:
nhis_alc_clean$capped_daily_grams_log <- log(nhis_alc_clean$new_grams)
# Check new variable
mean_log_grams <- nhis_alc_clean %>% 
  group_by(SEX, age_diaz, race_6_cats, education_3_cats) %>% 
  summarise(mean = mean(capped_daily_grams_log))

# Distribution plot 
ggplot(nhis_alc_clean, aes(x=capped_daily_grams_log), y) + geom_histogram() + 
  ggtitle("Distribution of estimated daily grams post transformation, full sample")+ 
  xlab("Daily grams of alcohol, post transformation") +
  ylab("Frequency")

# Subset only current drinkers
nhis_alc_clean_drinkers <- nhis_alc_clean %>%
  filter(ALCSTAT1=="Current drinker") 

# View raw and transformed distributions for drinkers only
ggplot(nhis_alc_clean_drinkers, aes(x=alc_daily_g_capped_200), y) + 
  geom_histogram() + 
  ylim(0, 100000)+
  xlab("Daily grams of alcohol") +
  ylab("Frequency") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))
ggsave(paste0(outputs,"Supplementary figure 1.png"), dpi=300, width=33, height=19, units="cm")

ggplot(nhis_alc_clean_drinkers, aes(x=capped_daily_grams_log), y) + geom_histogram() + 
  xlab("Daily grams of alcohol, post transformation") +
  ylab("Frequency")+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))
ggsave(paste0(outputs,"Supplementary figure 2.png"), dpi=300, width=33, height=19, units="cm")

# View the analytic sample sizes if sample weights were used via replication method
sum(nhis_alc_clean$SAMPWEIGHT) 
sum(nhis_alc_clean_drinkers$SAMPWEIGHT) 

## SAVE CLEANED DATA
saveRDS(nhis_alc_clean, paste0(inputs,"nhis_alc_clean_full_sample.RDS"))
saveRDS(nhis_alc_clean_drinkers, paste0(inputs,"nhis_alc_clean_drinkers_only.RDS"))

