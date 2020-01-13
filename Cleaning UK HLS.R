#####################################
#### Selecting Outcome Variables ####
#####################################

# Open data (make sure have unencrypted folder first)
library(data.table)
#usw1 <- fread("/Volumes/Fellowship/UKDA-6614-tab/tab/us_w1/a_indresp.tab") # Wave 1
usw2 <- fread("/Volumes/Fellowship/UKDA-6614-tab/tab/us_w2/b_indresp.tab") # Wave 2
usw3 <- fread("/Volumes/Fellowship/UKDA-6614-tab/tab/us_w3/c_indresp.tab") # Wave 3
usw4 <- fread("/Volumes/Fellowship/UKDA-6614-tab/tab/us_w4/d_indresp.tab") # Wave 4
usw5 <- fread("/Volumes/Fellowship/UKDA-6614-tab/tab/us_w5/e_indresp.tab") # Wave 5
usw6 <- fread("/Volumes/Fellowship/UKDA-6614-tab/tab/us_w6/f_indresp.tab") # Wave 6
usw7 <- fread("/Volumes/Fellowship/UKDA-6614-tab/tab/us_w7/g_indresp.tab") # Wave 7

# Selected outcome variables #

# sf1/scsf1 (latter is self-completed questionnaire) - General health:	categorical;	1 excellent, 2 very good, 3 good, 4 fair, 5 poor. 
# sf1 wave 2 only - no proxy variable
# We will take the proxy value if an individual has missing data 

# health - Long-standing illness or disability:	binary;	1 yes, 2 no
# Fine as is

# disdif - Type of impariment or disability:	categorical; 1 mentioned, 0 not mentioned. 13 variables
# if health == 1
# 1 mobility, 2 lifting/moving objects, 3 manual dexterity, 4 continence, 5 hearing, 6 sight, 7 communication/speech problems, 
# 8 memory/ability to concentrate/learn/understand, 9 recognising in physical danger, 10 physical coordination e.g. balance, 
# 11 difficulties with own personal care, 12 other problem/disability, 96 none of these
# Maybe take only one of these?

# hcondn - New health conditions since last interview:	categorical;	1-17 conditions, 96 none [each individual variables]
# If interviews at previous wave
# 1	Asthma, 2	Arthritis, 3	Congestive heart failure, 4	Coronary heart disease, 5	Angina, 6	Heart attack or myocardial infarction	
# 7	Stroke, 8	Emphysema, 9	Hyperthyroidism or an over-active thyroid, 10	Hypothyroidism or an under-active thyroid
# 11	Chronic bronchitis, 12	Any kind of liver condition, 13	Cancer or malignancy, 14	Diabetes, 15	Epilepsy
# 16	High blood pressure, 17	Clinical depression, 96	None of these

## Create single joined file of required variables ##

# Wave 2 #

# Edit variables
usw2$b_srh <- NA # Combine self-rated health measures (NB some people rate their health very differently across these measures for waves 2-5)
usw2$b_srh <- usw2$b_sf1 # Take 
usw2$b_srh[usw2$b_srh <= 0] <- usw2$b_scsf1[usw2$b_srh <= 0] # Take proxy if missing

# Subset variables required for analysis
vars <- c("pidp", "b_srh", "b_health", "b_disdif1", "b_disdif2", "b_disdif3", "b_disdif4", "b_disdif5", "b_disdif6", "b_disdif7", "b_disdif8", 
          "b_disdif9", "b_disdif10", "b_disdif11", "b_disdif12", "b_disdif96", "b_hcondn1", "b_hcondn2", "b_hcondn3", "b_hcondn4", 
          "b_hcondn5", "b_hcondn6", "b_hcondn7", "b_hcondn8", "b_hcondn9", "b_hcondn10", "b_hcondn11", "b_hcondn12", "b_hcondn13", 
          "b_hcondn14", "b_hcondn15", "b_hcondn16", "b_hcondn17", "b_hcondn96") # Select variables
outcome_dt <- usw2[, vars, with=FALSE] # Subset variables
rm(usw2)

# Wave 3 #

# Edit variables
usw3$c_srh <- NA # Combine self-rated health measures (NB some people rate their health very differently across these measures for waves 2-5)
usw3$c_srh <- usw3$c_sf1 # Take 
usw3$c_srh[usw3$c_srh <= 0] <- usw3$c_scsf1[usw3$c_srh <= 0] # Take proxy if missing

# Subset variables required for analysis
vars <- c("pidp", "c_srh", "c_health", "c_disdif1", "c_disdif2", "c_disdif3", "c_disdif4", "c_disdif5", "c_disdif6", "c_disdif7", "c_disdif8", 
          "c_disdif9", "c_disdif10", "c_disdif11", "c_disdif12", "c_disdif96", "c_hcondn1", "c_hcondn2", "c_hcondn3", "c_hcondn4", 
          "c_hcondn5", "c_hcondn6", "c_hcondn7", "c_hcondn8", "c_hcondn9", "c_hcondn10", "c_hcondn11", "c_hcondn12", "c_hcondn13", 
          "c_hcondn14", "c_hcondn15", "c_hcondn16", "c_hcondn17", "c_hcondn96") # Select variables
hold <- usw3[, vars, with=FALSE] # Subset variables
outcome_dt <- merge(outcome_dt, hold, by = "pidp", all.x = TRUE) # Join to only individuals with data in wave 2 (i.e. follow up data only - do not need new participants)
rm(usw3)

# Wave 4 #

# Edit variables
usw4$d_srh <- NA # Combine self-rated health measures (NB some people rate their health very differently across these measures for waves 2-5)
usw4$d_srh <- usw4$d_sf1 # Take 
usw4$d_srh[usw4$d_srh <= 0] <- usw4$d_scsf1[usw4$d_srh <= 0] # Take proxy if missing

# Subset variables required for analysis
vars <- c("pidp", "d_srh", "d_health", "d_disdif1", "d_disdif2", "d_disdif3", "d_disdif4", "d_disdif5", "d_disdif6", "d_disdif7", "d_disdif8", 
          "d_disdif9", "d_disdif10", "d_disdif11", "d_disdif12", "d_disdif96", "d_hcondn1", "d_hcondn2", "d_hcondn3", "d_hcondn4", 
          "d_hcondn5", "d_hcondn6", "d_hcondn7", "d_hcondn8", "d_hcondn9", "d_hcondn10", "d_hcondn11", "d_hcondn12", "d_hcondn13", 
          "d_hcondn14", "d_hcondn15", "d_hcondn16", "d_hcondn17", "d_hcondn96") # Select variables
hold <- usw4[, vars, with=FALSE] # Subset variables
outcome_dt <- merge(outcome_dt, hold, by = "pidp", all.x = TRUE) # Join to only individuals with data in wave 2 (i.e. follow up data only - do not need new participants)
rm(usw4)

# Wave 5 #

# Edit variables
usw5$e_srh <- NA # Combine self-rated health measures (NB some people rate their health very differently across these measures for waves 2-5)
usw5$e_srh <- usw5$e_sf1 # Take 
usw5$e_srh[usw5$e_srh <= 0] <- usw5$e_scsf1[usw5$e_srh <= 0] # Take proxy if missing

# Subset variables required for analysis
vars <- c("pidp", "e_srh", "e_health", "e_disdif1", "e_disdif2", "e_disdif3", "e_disdif4", "e_disdif5", "e_disdif6", "e_disdif7", "e_disdif8", 
          "e_disdif9", "e_disdif10", "e_disdif11", "e_disdif12", "e_disdif96", "e_hcondn1", "e_hcondn2", "e_hcondn3", "e_hcondn4", 
          "e_hcondn5", "e_hcondn6", "e_hcondn7", "e_hcondn8", "e_hcondn9", "e_hcondn10", "e_hcondn11", "e_hcondn12", "e_hcondn13", 
          "e_hcondn14", "e_hcondn15", "e_hcondn16", "e_hcondn17", "e_hcondn96") # Select variables
hold <- usw5[, vars, with=FALSE] # Subset variables
outcome_dt <- merge(outcome_dt, hold, by = "pidp", all.x = TRUE) # Join to only individuals with data in wave 2 (i.e. follow up data only - do not need new participants)
rm(usw5)

# Wave 6 #

# Edit variables
usw6$f_srh <- NA # Combine self-rated health measures
usw6$f_srh <- usw6$f_sf1 # Take 
usw6$f_srh[usw6$f_srh <= 0] <- usw6$f_scsf1[usw6$f_srh <= 0] # Take proxy if missing

# Subset variables required for analysis
vars <- c("pidp", "f_srh", "f_health", "f_disdif1", "f_disdif2", "f_disdif3", "f_disdif4", "f_disdif5", "f_disdif6", "f_disdif7", "f_disdif8", 
          "f_disdif9", "f_disdif10", "f_disdif11", "f_disdif12", "f_disdif96", "f_hcondn1", "f_hcondn2", "f_hcondn3", "f_hcondn4", 
          "f_hcondn5", "f_hcondn6", "f_hcondn7", "f_hcondn8", "f_hcondn9", "f_hcondn10", "f_hcondn11", "f_hcondn12", "f_hcondn13", 
          "f_hcondn14", "f_hcondn15", "f_hcondn16", "f_hcondn17", "f_hcondn96") # Select variables
hold <- usw6[, vars, with=FALSE] # Subset variables
outcome_dt <- merge(outcome_dt, hold, by = "pidp", all.x = TRUE) # Join to only individuals with data in wave 2 (i.e. follow up data only - do not need new participants)
rm(usw6)

# Wave 7 #

# Edit variables
usw7$g_srh <- NA # Combine self-rated health measures
usw7$g_srh <- usw7$g_sf1 # Take 
usw7$g_srh[usw7$g_srh <= 0] <- usw7$g_scsf1[usw7$g_srh <= 0] # Take proxy if missing

# Subset variables required for analysis
vars <- c("pidp", "g_srh", "g_health", "g_disdif1", "g_disdif2", "g_disdif3", "g_disdif4", "g_disdif5", "g_disdif6", "g_disdif7", "g_disdif8", 
          "g_disdif9", "g_disdif10", "g_disdif11", "g_disdif12", "g_disdif96", "g_hcondn1", "g_hcondn2", "g_hcondn3", "g_hcondn4", 
          "g_hcondn5", "g_hcondn6", "g_hcondn7", "g_hcondn8", "g_hcondn9", "g_hcondn10", "g_hcondn11", "g_hcondn12", "g_hcondn13", 
          "g_hcondn14", "g_hcondn15", "g_hcondn16", "g_hcondn17", "g_hcondn96") # Select variables
hold <- usw7[, vars, with=FALSE] # Subset variables
outcome_dt <- merge(outcome_dt, hold, by = "pidp", all.x = TRUE) # Join to only individuals with data in wave 2 (i.e. follow up data only - do not need new participants)
rm(usw7, hold, vars)
gc()

write.csv(outcome_dt, "/Volumes/Fellowship/Cleaned data/outcomes.tab") # Save
outcome_dt <- fread("/Volumes/Fellowship/Cleaned data/outcomes.tab")

## Create outcome variables ##

# Long-standing illness or disability #

# Count number of new cases per wave (nb. 1 yes, 2 no)
outcome_dt$health <- NA # Create blank variable
outcome_dt$health[outcome_dt$b_health == 2] <- 0 # Only those with good health at baseline
outcome_dt$health[outcome_dt$b_health == 2 & outcome_dt$c_health == 1] <- 1 # Identify new case cases
table(outcome_dt$health) # Print
outcome_dt$health[outcome_dt$b_health == 2 & outcome_dt$d_health == 1] <- 1 # Identify new case cases
table(outcome_dt$health) # Print
outcome_dt$health[outcome_dt$b_health == 2 & outcome_dt$e_health == 1] <- 1 # Identify new case cases
table(outcome_dt$health) # Print
outcome_dt$health[outcome_dt$b_health == 2 & outcome_dt$f_health == 1] <- 1 # Identify new case cases
table(outcome_dt$health) # Print
outcome_dt$health[outcome_dt$b_health == 2 & outcome_dt$g_health == 1] <- 1 # Identify new case cases
table(outcome_dt$health) # Print

# Self-rated health #

# Identify individuals who rated their health as fair (4) or poor (5) #
outcome_dt$poorhealth <- NA # Create blank variable
outcome_dt$poorhealth[outcome_dt$b_srh >= 1 & outcome_dt$b_srh <= 3] <- 0 # Only those with good health at baseline
outcome_dt$poorhealth[(outcome_dt$b_srh >= 1 & outcome_dt$b_srh <= 3) & outcome_dt$c_srh >=4] <- 1 # Identify new case cases
table(outcome_dt$poorhealth) # Print
outcome_dt$poorhealth[(outcome_dt$b_srh >= 1 & outcome_dt$b_srh <= 3) & outcome_dt$d_srh >=4] <- 1 # Identify new case cases
table(outcome_dt$poorhealth) # Print
outcome_dt$poorhealth[(outcome_dt$b_srh >= 1 & outcome_dt$b_srh <= 3) & outcome_dt$e_srh >=4] <- 1 # Identify new case cases
table(outcome_dt$poorhealth) # Print
outcome_dt$poorhealth[(outcome_dt$b_srh >= 1 & outcome_dt$b_srh <= 3) & outcome_dt$f_srh >=4] <- 1 # Identify new case cases
table(outcome_dt$poorhealth) # Print
outcome_dt$poorhealth[(outcome_dt$b_srh >= 1 & outcome_dt$b_srh <= 3) & outcome_dt$g_srh >=4] <- 1 # Identify new case cases
table(outcome_dt$poorhealth) # Print

# Health Conditions #

# Prevalence at baseline:
# 1	Asthma = 725, 2	Arthritis = 1715, 3	Congestive heart failure = 85, 4	Coronary heart disease = 214, 5	Angina = 285, 
# 6	Heart attack or myocardial infarction	= 198, 7	Stroke = 210, 8	Emphysema = 127, 9	Hyperthyroidism or an over-active thyroid = 100, 
# 10	Hypothyroidism or an under-active thyroid = 265, 11	Chronic bronchitis = 177, 12	Any kind of liver condition = 207, 
# 13	Cancer or malignancy = 459, 14	Diabetes = 711, 15	Epilepsy = 75, 16	High blood pressure = 2069, 17	Clinical depression = 813, 
# 96	None of these = 39725

# Maybe Arthritis and Hypertension are feasible - let's just look at these and see if potentially useful

# Arthritis
outcome_dt$arthritis <- NA # Create blank variable
outcome_dt$arthritis[outcome_dt$b_hcondn2 == 0] <- 0 # Only those with good health at baseline
outcome_dt$arthritis[outcome_dt$b_hcondn2 == 0 & outcome_dt$c_hcondn2 == 1] <- 1 # Identify new case cases
table(outcome_dt$arthritis) # Print
outcome_dt$arthritis[outcome_dt$b_hcondn2 == 0 & outcome_dt$d_hcondn2 == 1] <- 1 # Identify new case cases
table(outcome_dt$arthritis) # Print
outcome_dt$arthritis[outcome_dt$b_hcondn2 == 0 & outcome_dt$e_hcondn2 == 1] <- 1 # Identify new case cases
table(outcome_dt$arthritis) # Print
outcome_dt$arthritis[outcome_dt$b_hcondn2 == 0 & outcome_dt$f_hcondn2 == 1] <- 1 # Identify new case cases
table(outcome_dt$arthritis) # Print
outcome_dt$arthritis[outcome_dt$b_hcondn2 == 0 & outcome_dt$g_hcondn2 == 1] <- 1 # Identify new case cases
table(outcome_dt$arthritis) # Print

# Hypertension
outcome_dt$hypertension <- NA # Create blank variable
outcome_dt$hypertension[outcome_dt$b_hcondn16 == 0] <- 0 # Only those with good health at baseline
outcome_dt$hypertension[outcome_dt$b_hcondn16 == 0 & outcome_dt$c_hcondn16 == 1] <- 1 # Identify new case cases
table(outcome_dt$hypertension) # Print
outcome_dt$hypertension[outcome_dt$b_hcondn16 == 0 & outcome_dt$d_hcondn16 == 1] <- 1 # Identify new case cases
table(outcome_dt$hypertension) # Print
outcome_dt$hypertension[outcome_dt$b_hcondn16 == 0 & outcome_dt$e_hcondn16 == 1] <- 1 # Identify new case cases
table(outcome_dt$hypertension) # Print
outcome_dt$hypertension[outcome_dt$b_hcondn16 == 0 & outcome_dt$f_hcondn16 == 1] <- 1 # Identify new case cases
table(outcome_dt$hypertension) # Print
outcome_dt$hypertension[outcome_dt$b_hcondn16 == 0 & outcome_dt$g_hcondn16 == 1] <- 1 # Identify new case cases
table(outcome_dt$hypertension) # Print

# Type of impariment or disability #

# Prevalence at baseline (if health == 1)
# 1 mobility = 6364, 2 lifting/moving objects = 6968, 3 manual dexterity = 2628, 4 continence = 1728, 5 hearing = 1510, 
# 6 sight = 1443, 7 communication/speech problems = 544, 8 memory/ability to concentrate/learn/understand = 2202, 
# 9 recognising in physical danger = 402, 10 physical coordination e.g. balance = 2339, 11 difficulties with own personal care = 1810, 
# 12 other problem/disability = 2923, 96 none of these = 5885






