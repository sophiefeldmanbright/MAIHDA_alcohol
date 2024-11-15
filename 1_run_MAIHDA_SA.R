##############################################################################
# MAIHDA of grams of alcohol per day - DRINKERS
##############################################################################

# Set-up
setwd("C:/Users/Documents/")
code <- "intersectionality/MAIHDA alcohol/"
inputs <- "intersectionality/MAIHDA alcohol/inputs/"
models <- "intersectionality/MAIHDA alcohol/models/"
outputs <- "intersectionality/MAIHDA alcohol/outputs/"

# Read in necessary R packages
library(tidyverse)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ragg)
library(bayesplot)
library(coda)
library(memisc)
library("R2MLwiN")
options(MLwiN_path="C:/Program Files/MLwiN v3.05/")
options(scipen=10)

########################################################## PRE PROCESSING

# Read in data (full sample):
data <- readRDS(paste0(inputs,"nhis_alc_clean_full_sample.RDS"))

# subset drinkers
data_drinkers <- data %>% filter(ALCSTAT1=="Current drinker")

# Drop individuals age <21
data_0 <- data_drinkers %>% filter(age_diaz!="18-20")

# Keep only the 6 selected race and ethnicity groups
data_1 <- data_0 %>% filter(!is.na(race_6_cats))

# Generate intersections
data_2 <- data_1 %>% 
  group_by(SEX, race_6_cats, education_3_cats, age_diaz) %>% 
  mutate(intersections = cur_group_id()) %>%
  mutate(intersectional_names = as.character(paste(SEX, age_diaz, race_6_cats, education_3_cats)))
  
# Subset sample to 2010 onwards
data_3 <- data_2 %>%
  dplyr::filter(YEAR>=2010)

# Check intersectional group sizes
temp <- data_3 %>% 
  group_by(intersections) %>%
  mutate(count=n())
group_sizes <- temp %>% distinct(intersections, count)
sum(group_sizes$count <= 20) 

# Add a column of the observed mean grams per day for each intersection
data_4 <- data_3 %>%
  group_by(intersections) %>%
  mutate(mean_observed_grams = mean(alc_daily_g_capped_200))

# Calculate overall mean grams for the sample of drinkers
mean(data_4$alc_daily_g_capped_200) 
sd(data_4$alc_daily_g_capped_200) 

# Generate reference table with intersectional names & mean observed grams per intersection
intersections_reference <- data_4 %>%
  group_by(intersectional_names) %>% 
  distinct(intersections, intersectional_names, mean_observed_grams)

# Prep data for use with Mlwin
model_data <- data_4 %>%
  mutate(cons=1) %>% 
  arrange(intersections, NHISPID)

model_data$age_diaz <- droplevels(model_data$age_diaz)
model_data$YEAR <- as.factor(model_data$YEAR)

# Save
saveRDS(model_data, paste0(inputs, "grams_data_pre_maihda_drinkers_2010_2018.rds"))

#################################################################### MODELLING

# Read in prepped data
model_data <- readRDS(paste0(inputs, "grams_data_pre_maihda_drinkers_2010_2018.rds"))

# Null model
(null_grams <- runMLwiN(capped_daily_grams_log ~ 1 + YEAR +
                          (1 | intersections) + 
                          (1 | NHISPID), 
                             data = model_data, 
                             estoptions = list(EstM=1, resi.store=TRUE, resi.store.levs=c(1,2),
                                               mcmcMeth = list(burnin = 5000,
                                                               thinning = 50,
                                                               resi.store=TRUE))))
# Full model
(full_grams <- runMLwiN(capped_daily_grams_log ~ 1 + YEAR +
                          SEX + age_diaz + race_6_cats + education_3_cats +
                          (1 | intersections) + 
                          (1 | NHISPID), 
                        data = model_data, 
                        estoptions = list(EstM=1, resi.store=TRUE, resi.store.levs=c(1,2),
                                          mcmcMeth = list(burnin = 5000,
                                                          thinning = 50,
                                                          resi.store=TRUE))))

# save the model objects
saveRDS(null_grams, paste0(models, "null_grams_drinkers_2010_2018.rds"))
saveRDS(full_grams, paste0(models, "full_grams_drinkers_2010_2018.rds"))

# Check convergence achieved
summary(full_grams@chains[, "FP_Intercept"])
mcmc_trace(full_grams@chains)

################################################################ ANALYSIS

# Read in the model objects
null_grams <- readRDS(paste0(models, "null_grams_drinkers_2010_2018.rds"))
full_grams <- readRDS(paste0(models, "full_grams_drinkers_2010_2018.rds"))

##### CHECK MODELLING ASSUMPTIONS

## Null model, level 1 residuals
hist(null_grams["residual"][["lev_1_resi_est_Intercept"]])
# Heteroskedasticity of residuals
plot(null_grams["residual"][["lev_1_resi_est_Intercept"]])
abline(h = 0, lty = 2, col = "red")
# QQ plot
qqnorm(null_grams["residual"][["lev_1_resi_est_Intercept"]])
qqline(null_grams["residual"][["lev_1_resi_est_Intercept"]], col = "steelblue", lwd = 2)
## Null model, level 2 residuals
hist(null_grams["residual"][["lev_2_resi_est_Intercept"]])
# Heteroskedasticity of residuals
plot(null_grams["residual"][["lev_2_resi_est_Intercept"]])
abline(h = 0, lty = 2, col = "red")
# QQ plot
qqnorm(null_grams["residual"][["lev_2_resi_est_Intercept"]])
qqline(null_grams["residual"][["lev_2_resi_est_Intercept"]], col = "steelblue", lwd = 2)

### Full model, level 1 residuals
hist(full_grams["residual"][["lev_1_resi_est_Intercept"]])
# Heteroskedasticity of residuals
plot(full_grams["residual"][["lev_1_resi_est_Intercept"]])
abline(h = 0, lty = 2, col = "red")
# QQ plot
qqnorm(full_grams["residual"][["lev_1_resi_est_Intercept"]])
qqline(full_grams["residual"][["lev_1_resi_est_Intercept"]], col = "steelblue", lwd = 2)
## Full model, level 2 residuals
hist(full_grams["residual"][["lev_2_resi_est_Intercept"]])
# Heteroskedasticity of residuals
plot(full_grams["residual"][["lev_2_resi_est_Intercept"]])
abline(h = 0, lty = 2, col = "red")
# QQ plot
qqnorm(full_grams["residual"][["lev_2_resi_est_Intercept"]])
qqline(full_grams["residual"][["lev_2_resi_est_Intercept"]], col = "steelblue", lwd = 2)


##### PRODUCE A TABLE OF MODEL COEFFICIENTS 
# comparing the null and full models

coefs_null <- getSummary(null_grams)
coefs_null <- as.data.frame(coefs_null[["coef"]])
coefs_null <- round(coefs_null, 3) %>% dplyr::select(est,lwr,upr,p)
rownames(coefs_null) <- c("intercept_FE_1", "Year 2011", "Year 2012", "Year 2013", "Year 2014",
                          "Year 2015", "Year 2016", "Year 2017", "Year 2018",
                          "strata_RE_1","individuals_RE_1")

coefs_full <- getSummary(full_grams)
coefs_full <- as.data.frame(coefs_full[["coef"]])
coefs_full <- round(coefs_full, 3) %>% dplyr::select(est,lwr,upr,p)
rownames(coefs_full) <- c("intercept_FE_2","Year 2011", "Year 2012", "Year 2013", 
                          "Year 2014", "Year 2015", "Year 2016", "Year 2017", "Year 2018",
                          "female","age 25-59", "age 60+",
                          "Hispanic", "Black", "Asian", "Multiple race", "AI/AN",
                          "Some college", "4+ years college", 
                          "RP2_var_intercept", "RP1_var_intercept")

coefs_table <- rbind(coefs_null, coefs_full)
saveRDS(coefs_table, paste0(outputs, "grams drinkers/model coefficients and variance_grams_drinkers_2010_2018.rds"))
write.csv(coefs_table, paste0(outputs, "grams drinkers/model coefficients and variance_grams_drinkers_2010_2018.csv"))

##### CALCULATE VPC AND PCV (from the parameter point estimates)
VPC_grams_null <- null_grams["RP"][["RP2_var_Intercept"]]/(null_grams["RP"][["RP1_var_Intercept"]] + null_grams["RP"][["RP2_var_Intercept"]])
VPC_grams_full <- full_grams["RP"][["RP2_var_Intercept"]]/(full_grams["RP"][["RP1_var_Intercept"]] + full_grams["RP"][["RP2_var_Intercept"]])
VPC_table <- data.frame(Model = c("null", "main effects"),
                        VPC = c(VPC_grams_null, VPC_grams_full))
write.csv(VPC_table, paste0(outputs, "grams drinkers/VPC_table_grams_drinkers_2010_2018.csv"))

##### Extract data from relevant slots of s4 object (based upon full model)

# Add intersectional group sizes as important indicator of expected level of shrinkage
model_data <- model_data %>% 
  group_by(intersections) %>%
  mutate(count=n())

# data frame
data <- full_grams@data
intersections <- distinct(data, intersections, .keep_all = TRUE)

# Estimates of fixed effects
fixed_effects <- full_grams@FP
fixed_effects <- as.data.frame(fixed_effects)

# Estimates of random effects
random_effects <- full_grams@RP
random_effects <- as.data.frame(random_effects)

##### PREPARE FIXED-PART PAREMETER CHAINS 
# Store the constant and estimated coef for each variable, for each iteration (100 iterations)

chains <- full_grams@chains
chains <- as.data.frame(chains)
mb_prepped <- chains %>% dplyr::select(-c(deviance, RP2_var_Intercept, RP1_var_Intercept))

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
resi_chains_lev_2 <- full_grams@resi.chains$resi_lev2
resi_chains_lev_2 <- as.data.frame(resi_chains_lev_2)

# reformat
mu_prepped <- resi_chains_lev_2
mu_prepped$iteration <- 1:nrow(mu_prepped)
mu_prepped <- pivot_longer(resi_chains_lev_2, u_0_1:u_0_107)
mu_prepped$iteration <- rep(c(1:100), each = 107)

# Generate a table with the intersectional groups to estimate for (i.e., year set to 2009 for all)
# Convert all years to 0s, except for the "YEAR2014" column
intersections_2014 <- intersections %>%
  mutate_at(vars(starts_with("YEAR")), ~ 0) %>%
  mutate(YEAR2014 = 1)

##### MERGE DATA, FIXED-PART PARAMETER AND RANDOM EFFECT CHAINS TOGETHER
mdata_prepped <- inner_join(mb_prepped, mu_prepped, by = 'iteration')
mdata_prepped$name <- str_sub(mdata_prepped$name, 5)
mdata_prepped$name <- as.numeric(mdata_prepped$name)
mdata_prepped <- dplyr::rename(mdata_prepped, intersections = name, u = value)
mdata_prepped <- inner_join(mdata_prepped, intersections_2014, by = 'intersections')

##### CALCULATE VALUES OF INTEREST (est = estA + estI)

## If assuming that exp(eij) is normally distributed:
# RP1_var_Intercept <- random_effects[rownames(random_effects) == "RP1_var_Intercept", "random_effects"]
# constant <- exp(0.5*RP1_var_Intercept)

# If cannot assumme that exp(eij) is normally distributed, integrate using their actual distribution:
eij <- full_grams["residual"][["lev_1_resi_est_Intercept"]]
constant <- mean(exp(eij))

RP2_var_Intercept <- random_effects[rownames(random_effects) == "RP2_var_Intercept", "random_effects"]
uj_constant <- exp(0.5*RP2_var_Intercept)

# Estimates including both additive and interaction effects:
mdata_prepped <- mdata_prepped %>% mutate(
  est = exp(b_cons*Intercept
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
                    + b_2014*`YEAR2014`
                    + u)*constant
)

# Estimates including additive effects only:
mdata_prepped <- mdata_prepped %>% mutate(
  estA = exp(b_cons*Intercept
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
                           + b_2014*`YEAR2014`)*uj_constant*constant
                           )

# Grams attributable to interaction calculated as the difference between est and estA
mdata_prepped <- mdata_prepped %>% 
  mutate(estI = est - estA)

# Calculate the mean, 2.5th and 97.5th percentiles of the MCMC chains
mdata_prepped <- mdata_prepped %>% 
  group_by(intersections) %>%
  mutate(estmn = mean(est),
         estlo = quantile(est,.025),
         esthi = quantile(est,.975),
         estAmn = mean(estA),
         estAlo = quantile(estA,.025),
         estAhi = quantile(estA,.975),
         estImn = mean(estI),
         estIlo = quantile(estI,.025),
         estIhi = quantile(estI,.975))

# Drop chains and just keep their summaries (mean, 2.5th and 97.5th)
mdata_results <- mdata_prepped %>%
  dplyr::select(-"iteration", -"est",  -"estA", -"estI", -contains(c("b_", "u_" ))) %>%
  distinct(intersections, .keep_all=TRUE)

# Merge with intersectional names reference table & group sizes
mdata_results <- inner_join(mdata_results, intersections_reference)
mdata_results <- inner_join(mdata_results, group_sizes)

# Select only the important variables
mdata_results <- mdata_results %>% 
  dplyr::select(intersectional_names, count, mean_observed_grams, 
                estmn, estlo, esthi,
                estAmn, estAlo, estAhi,
                estImn, estIlo, estIhi) %>% 
  mutate_if(is.numeric, round, 1) %>% mutate(
    difference = estmn-mean_observed_grams)

# save results
saveRDS(mdata_results, paste0(outputs, "grams drinkers/results_grams_drinkers_2010_2018.rds"))
write.csv(mdata_results, paste0(outputs, "grams drinkers/results_grams_drinkers_2010_2018.csv"))

##### SUMMARY RESULTS TABLES
mdata_results <- readRDS(paste0(outputs, "grams drinkers/results_grams_drinkers_2010_2018.rds"))

# Summarise intersectional groups with the highest and lowest estimated grams
mdata_max_5_overall <- mdata_results %>% ungroup %>% slice_max(estmn, n = 5) 
mdata_min_5_overall <- mdata_results %>% ungroup %>% slice_min(estmn, n = 5)
mdata_overall <- rbind(mdata_max_5_overall, mdata_min_5_overall)

write.csv(mdata_overall, paste0(outputs, "grams drinkers/mdata_5_estimates_drinkers_2010_2018.csv"))

# Summarise which intersectional groups have the largest differences in grams estimates,
# when comparing additive only estimates vs estimates which include interaction effects
mdata_max_5_interactions <- mdata_results %>% ungroup %>% slice_max(estImn, n = 5) 
mdata_min_5_interactions <- mdata_results %>% ungroup %>% slice_min(estImn, n = 5)  
mdata_interactions <- rbind(mdata_max_5_interactions, mdata_min_5_interactions)

write.csv(mdata_interactions, paste0(outputs, "grams drinkers/mdata_5_interactions_drinkers_2010_2018.csv"))

