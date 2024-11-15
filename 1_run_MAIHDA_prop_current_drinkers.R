##############################################################################
# MAIHDA to estimate proportion of current drinkers 
##############################################################################

# Setup
setwd("C:/Users/Documents/")
code <- "intersectionality/MAIHDA alcohol/"
inputs <- "intersectionality/MAIHDA alcohol/inputs/"
models <- "intersectionality/MAIHDA alcohol/models/"
outputs <- "intersectionality/MAIHDA alcohol/outputs/"

library(tidyverse)
library(tidyr)
library(dplyr)
library(sjstats)
library(haven)
library(performance)
library(memisc)
library(gt)
library(R2MLwiN)
library(xlsx)
library(stringr)
library(boot)
library(memisc)
library(fastDummies)
options(MLwiN_path="C:/Program Files/MLwiN v3.05/")
options(scipen=10)

############################################################# PRE PROCESSING
# Read in data (full sample):
data <- readRDS(paste0(inputs,"nhis_alc_clean_full_sample.RDS"))

# Generate a binary drinking status variable
data_1 <- data %>%
  mutate(drinking_status =
        ifelse(alc_4_cats == "Abstainers (lifetime abstainers & former drinkers)", 0,1))
                   
# Generate intersections
data_2 <- data_1 %>% 
  group_by(SEX, race_6_cats, education_3_cats, age_diaz) %>% 
  mutate(intersections = cur_group_id()) %>%
  mutate(intersectional_names = as.character(paste(SEX, age_diaz, race_6_cats, education_3_cats))) %>%
  group_by(intersections) %>%
  mutate(denominator=n()) %>%
  group_by(drinking_status, intersections) %>%
  mutate(numerator=n(),
         proportion=numerator/denominator,
         percent_drinkers=proportion*100) %>%
  ungroup() 

# Subset data to keep only the variables of interest
data_3 <- data_2 %>%
  dplyr::select(intersections, intersectional_names, NHISPID, ALCSTAT1, drinking_status, numerator, denominator, percent_drinkers, 
                age_diaz, SEX, race_6_cats, education_3_cats, YEAR)

# Calculate proportion of total sample that are HEDs
data_3 %>%
  summarise(proportion_drinkers = mean(drinking_status, na.rm = TRUE)*100) # 62.1

# Generate reference table with intersectional names & proportion of observed HED per intersection
intersections_reference <- data_3 %>%
  filter(drinking_status==1) %>%
  distinct(intersections, intersectional_names, denominator, percent_drinkers)

# Prep data for use with Mlwin
model_data <- data_3 %>%
  mutate(cons=1) %>% 
  arrange(intersections, NHISPID)
model_data$age_diaz <- droplevels(model_data$age_diaz)
model_data$YEAR <- as.factor(model_data$YEAR)

# Save
saveRDS(model_data,  paste0(inputs, "data_pre_maihda_binary_drinking_status.rds"))

###################################################################### MODELLING

##### RUN THE MAIHDA MODELS

# Read in the data
model_data <- readRDS(paste0(inputs, "data_pre_maihda_binary_drinking_status.rds"))

# null model
(null_drinking_status <- runMLwiN(logit(drinking_status) ~ 1 + YEAR +
                      (1|intersections), 
                      D = "Binomial", data = model_data,
                      estoptions=list(EstM=1, resi.store=TRUE, resi.store.levs=c(1,2),
                                      mcmcMeth = list(burnin = 5000,
                                                      thinning = 50,
                                                      resi.store=TRUE))))                                           

# full model
(full_drinking_status <- runMLwiN(logit(drinking_status) ~ 1 + SEX + age_diaz + race_6_cats + education_3_cats +
                      YEAR + 
                      (1|intersections), 
                      D = "Binomial", data = model_data,
                      estoptions=list(EstM=1, resi.store=TRUE, resi.store.levs=c(1,2),
                                        mcmcMeth = list(burnin = 5000,
                                                        thinning = 50,
                                                        resi.store=TRUE))))                                           

# save the model objects
saveRDS(null_drinking_status, paste0(models, "null_binary_drinking_status.rds"))
saveRDS(full_drinking_status, paste0(models, "full_binary_drinking_status.rds"))

####################################################################### ANALYSIS

# read in the model objects
null_drinking_status <- readRDS(paste0(models, "null_binary_drinking_status.rds"))
full_drinking_status <- readRDS(paste0(models, "full_binary_drinking_status.rds"))

##### PRODUCE A TABLE OF MODEL COEFFICIENTS 
# comparing the null and full models
 
coefs_null <- getSummary(null_drinking_status)
coefs_null <- as.data.frame(coefs_null[["coef"]])
coefs_null <- round(coefs_null, 3) %>% dplyr::select(est,lwr,upr,p)
rownames(coefs_null) <- c("intercept_FE_1",
                          "Year 2001", "Year 2002", "Year 2003", "Year 2004",
                          "Year 2005", "Year 2006", "Year 2007", "Year 2008", "Year 2009",
                          "Year 2010", "Year 2011", "Year 2012", "Year 2013", "Year 2014",
                          "Year 2015", "Year 2016", "Year 2017", "Year 2018",
                          "strata_RE_1","individuals_RE_1")
 
coefs_full <- getSummary(full_drinking_status)
coefs_full <- as.data.frame(coefs_full[["coef"]])
coefs_full <- round(coefs_full, 3) %>% dplyr::select(est,lwr,upr,p)
rownames(coefs_full) <- c("intercept_FE_2","female","age 25-59", "age 60+",
                               "Hispanic", "Black", "Asian", "Multiple race", "AI/AN",
                               "Some college", "4+ years college",
                               "Year 2001", "Year 2002", "Year 2003", "Year 2004",
                               "Year 2005", "Year 2006", "Year 2007", "Year 2008", "Year 2009",
                               "Year 2010", "Year 2011", "Year 2012", "Year 2013", "Year 2014",
                               "Year 2015", "Year 2016", "Year 2017", "Year 2018",
                               "strata_RE_2", "RP1_var_bcons_1")

coefs_table <- rbind(coefs_null, coefs_full)
saveRDS(coefs_table, paste0(outputs, "binary drinking status/binary drinking status, model coefficients and variance.rds"))
write.csv(coefs_table,paste0(outputs, "binary drinking status/binary drinking status, model coefficients and variance.csv")) 

##### CALCULATE VPC AND PCV (from the parameter point estimates)
VPC_drinking_status_null <- print(VPC <- null_drinking_status["RP"][["RP2_var_Intercept"]]/(pi^2/3 + null_drinking_status["RP"][["RP2_var_Intercept"]]))
VPC_full_drinking_status <- print(VPC <- full_drinking_status["RP"][["RP2_var_Intercept"]]/(pi^2/3 + full_drinking_status["RP"][["RP2_var_Intercept"]]))
VPC_table <- data.frame(Model = c("null", "main effects"),
                         VPC = c(VPC_drinking_status_null, VPC_full_drinking_status))
write.csv(VPC_table, paste0(outputs, "binary drinking status/binary drinking status VPC table.csv"))

##### Extract data from relevant slots of s4 object (based upon full model)

# data frame
data <- full_drinking_status@data
intersections <- distinct(data, intersections, .keep_all = TRUE)
intersections <- intersections %>% dplyr::select(-c(drinking_status, "l1id", "_denom"))

# Estimates of fixed effects
fixed_effects <- full_drinking_status@FP
fixed_effects <- as.data.frame(fixed_effects)

# Estimates of random effects
random_effects <- full_drinking_status@RP
random_effects <- as.data.frame(random_effects)

##### PREPARE FIXED-PART PAREMETER CHAINS 
# Store the constant and estimated coef for each variable, for each iteration (100 iterations)

chains <- full_drinking_status@chains
chains <- as.data.frame(chains)
mb_prepped <- chains %>% dplyr::select(-c(deviance, RP2_var_Intercept, RP1_var_bcons_1))

mb_prepped <- dplyr::rename(mb_prepped,
                      b_cons = "FP_Intercept",
                      b_female = "FP_SEXFemale",
                      b_adult = "FP_age_diaz25-59",
                      b_older_adult = "FP_age_diaz60+",
                      b_Black = "FP_race_6_catsNH Black",
                      b_Asian = "FP_race_6_catsNH Asian",
                      b_AI_AN = "FP_race_6_catsNH AI/AN",
                      b_Hispanic = "FP_race_6_catsHispanic",
                      b_Multiple_race = "FP_race_6_catsNH Multiple race",
                      b_med = "FP_education_3_catssome college",
                      b_high = "FP_education_3_cats4+ years college",
                      b_2001 = "FP_YEAR2001", 
                      b_2002 = "FP_YEAR2002", 
                      b_2003 = "FP_YEAR2003", 
                      b_2004 = "FP_YEAR2004",
                      b_2005 = "FP_YEAR2005", 
                      b_2006 = "FP_YEAR2006", 
                      b_2007 = "FP_YEAR2007", 
                      b_2008 = "FP_YEAR2008", 
                      b_2009 = "FP_YEAR2009",
                      b_2010 = "FP_YEAR2010", 
                      b_2011 = "FP_YEAR2011", 
                      b_2012 = "FP_YEAR2012", 
                      b_2013 = "FP_YEAR2013", 
                      b_2014 = "FP_YEAR2014",
                      b_2015 = "FP_YEAR2015", 
                      b_2016 = "FP_YEAR2016", 
                      b_2017 = "FP_YEAR2017", 
                      b_2018 = "FP_YEAR2018")

mb_prepped$iteration <- rep(c(1:100))

##### PREPARE intersections RANDOM EFFECTS CHAINS
# Store the value of the random effect, for each intersectional group, for each iteration

# extract the residual chains
resi_chains_lev_2 <- full_drinking_status@resi.chains$resi_lev2
resi_chains_lev_2 <- as.data.frame(resi_chains_lev_2)

# reformat
mu_prepped <- resi_chains_lev_2
mu_prepped$iteration <- 1:nrow(mu_prepped)
mu_prepped <- pivot_longer(resi_chains_lev_2, u_0_1:u_0_108)
mu_prepped$iteration <- rep(c(1:100), each = 108)

##### MERGE DATA, FIXED-PART PARAMETER AND RANDOM EFFECT CHAINS TOGETHER
mdata_prepped <- inner_join(mb_prepped, mu_prepped, by = 'iteration')
mdata_prepped$name <- str_sub(mdata_prepped$name, 5)
mdata_prepped$name <- as.numeric(mdata_prepped$name)
mdata_prepped <- dplyr::rename(mdata_prepped, intersections = name, u = value)
mdata_prepped <- inner_join(mdata_prepped, intersections, by = 'intersections')

##### CALCULATE PERCENTAGES OF INTEREST (p = pA + pB)

# Percentage p based on fixed AND random part

mdata_prepped <- mdata_prepped %>% mutate(
  p = 100*inv.logit(b_cons*Intercept
                    + b_female*SEXFemale
                    + b_adult*`age_diaz25-59`
                    + b_older_adult*`age_diaz60+`  
                    + b_Hispanic*`race_6_catsHispanic`
                    + b_Asian*`race_6_catsNH Asian`
                    + b_AI_AN*`race_6_catsNH AI/AN`
                    + b_Black*`race_6_catsNH Black`
                    + b_Multiple_race*`race_6_catsNH Multiple race`
                    + b_med*`education_3_catssome college`
                    + b_high*`education_3_cats4+ years college`
                    + b_2009*`YEAR2009`
                    + u)
)

# Percentage pA based only on the fixed-part
mdata_prepped <- mdata_prepped %>% mutate(
   pA = 100*inv.logit(b_cons*Intercept
                    + b_female*SEXFemale
                    + b_adult*`age_diaz25-59`
                    + b_older_adult*`age_diaz60+`  
                    + b_Hispanic*`race_6_catsHispanic`
                    + b_Asian*`race_6_catsNH Asian`
                    + b_AI_AN*`race_6_catsNH AI/AN`
                    + b_Black*`race_6_catsNH Black`
                    + b_Multiple_race*`race_6_catsNH Multiple race`
                    + b_med*`education_3_catssome college`
                    + b_high*`education_3_cats4+ years college`
                    + b_2009*`YEAR2009`)
)

# Percentage calculated as the difference between p and pA
mdata_prepped <- mdata_prepped %>% mutate(
   pB = p - pA)

# Calculate the mean, 2.5th and 97.5th percentiles of the MCMC chains
 mdata_prepped <- mdata_prepped %>% 
   group_by(intersections) %>%
   mutate(pmn = mean(p),
          plo = quantile(p,.025),
          phi = quantile(p,.975),
          pAmn = mean(pA),
          pAlo = quantile(pA,.025),
          pAhi = quantile(pA,.975),
          pBmn = mean(pB),
          pBlo = quantile(pB,.025),
          pBhi = quantile(pB,.975))

# Drop chains and just keep their summaries (mean, 2.5th and 97.5th)
mdata_results <- mdata_prepped %>%
  dplyr::select(-"iteration", -"p",  -"pA", -contains(c("b_", "u_" ))) %>%
  distinct(intersections, .keep_all=TRUE)

# Merge with intersectional names reference table
mdata_results <- inner_join(mdata_results, intersections_reference)

# save results
saveRDS(mdata_results, paste0(outputs, "binary drinking status/results binary drinking status corrected CIs.rds"))

##### SUMMARY RESULTS TABLES
mdata_results <- readRDS(paste0(outputs, "binary drinking status/results binary drinking status corrected CIs.rds"))

# Summarise intersectional groups with the highest and lowest proportions of HEDs
mdata_max_5_overall <- mdata_results %>% ungroup %>% slice_max(pmn, n = 5) %>% 
  dplyr::select(intersectional_names, pmn, plo, phi, pAmn, pAlo, pAhi, pBmn, pBlo, pBhi)
mdata_min_5_overall <- mdata_results %>% ungroup %>% slice_min(pmn, n = 5) %>% 
  dplyr::select(intersectional_names, pmn, plo, phi, pAmn, pAlo, pAhi, pBmn, pBlo, pBhi)
mdata_overall <- rbind(mdata_max_5_overall, mdata_min_5_overall)

write.csv(mdata_overall, paste0(outputs, "binary drinking status/mdata_5_estimates_drinking_status_corrected_cis.csv"))

# Summarise which intersectional groups have the largest interaction effects
mdata_max_5_interactions <- mdata_results %>% ungroup %>% slice_max(pBmn, n = 5) %>% 
  dplyr::select(intersectional_names, pmn, plo, phi, pAmn, pAlo, pAhi, pBmn, pBlo, pBhi)
mdata_min_5_interactions <- mdata_results %>% ungroup %>% slice_min(pBmn, n = 5) %>% 
  dplyr::select(intersectional_names, pmn, plo, phi, pAmn, pAlo, pAhi, pBmn, pBlo, pBhi)
mdata_interactions <- rbind(mdata_max_5_interactions, mdata_min_5_interactions)

write.csv(mdata_interactions, paste0(outputs, "binary drinking status/mdata_5_interactions_drinking_status_corrected_cis.csv"))

##### Explore face validity of estimates

# Compare observed HED (overall) and estimated HED in a table
temp <- mdata_results %>% dplyr::select(intersectional_names, percent_drinkers, pmn) %>%
  mutate(percent_drinkers = percent_drinkers,
         difference = pmn - percent_drinkers,
         abs_difference = abs(difference),
         percent_difference = abs(difference/pmn*100))
write.csv(temp, paste0(outputs, "binary drinking status/observed vs estimated percent drinkers.csv"))
