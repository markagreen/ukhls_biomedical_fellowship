## Descriptive Statistics ##

# Aim: Create a table of descriptive sample characteristics for inclusion in the paper.

# We will calcuate only for a few core characteristics to get an idea. Compare the following options
# 1. Wave 2 sample only (core analytical sample)
# 2. All people who were part of Nurse survey
# 3. Whole sample at wave 2

# Load libraries
library(data.table)

# Load data
my_sample <- fread("../../../../Desktop/Green_UKHLS/Green_Phenotype_sendout.txt") # Load data supplied of all with biomarker and genetic data
nurse <- fread("../../../../Desktop/Green_UKHLS/UKHLS general/nurse_survey_wave_2_3/UKDA-7251-tab/tab/b_indresp_ns.tab") # Nurse led survey
core <- fread("../../../../Desktop/Green_UKHLS/UKHLS general/ukhls_core/UKDA-6614-tab/tab/ukhls_w2/b_indresp.tab") # Core HLS data

# Sample size
dim(my_sample[my_sample$wave == 2]) # 1
dim(nurse) # 2
dim(core) # 3

# Sex
table(my_sample$sex[my_sample$wave == 2], exclude = NULL) # 1
table(nurse$b_nsex, exclude = NULL) # 2
table(core$b_sex, exclude = NULL) # 3

# Age 
my_sample$b_dvage[my_sample$b_dvage == "90-94" | my_sample$b_dvage == "95-99"] <- 90 # Convert older age categories into single 90+ group (but keep as single number for model purposes)
my_sample$b_dvage[my_sample$b_dvage == ""] <- NA # Set as missing 
my_sample$b_dvage <- as.numeric(my_sample$b_dvage) # Change character to numeric

mean(my_sample$b_dvage[my_sample$wave == 2], na.rm=T) # 1
mean(nurse$b_age, na.rm=T) # 2
mean(core$b_dvage, na.rm=T) # 3

# Education
table(my_sample$b_qfhigh_dv[my_sample$wave == 2], exclude = NULL) # 1

# 1 is no quals, 7-8 degree+ (0 is missing)

table(nurse$b_hiqual_dv, exclude = NULL) # 2
table(core$b_hiqual_dv, exclude = NULL) # 3

# Pos. = 328	Variable = b_hiqual_dv	Variable label = Highest qualification
# This variable is  numeric, the SPSS measurement level is SCALE
# Value label information for b_hiqual_dv
# Value = 1.0	Label = Degree
# Value = 2.0	Label = Other higher degree
# Value = 3.0	Label = A-level etc
# Value = 4.0	Label = GCSE etc
# Value = 5.0	Label = Other qualification
# Value = 9.0	Label = No qualification
# Value = -9.0	Label = Missing
# Value = -8.0	Label = Inapplicable
# Value = -1.0	Label = Don't know


# LLTI 
table(my_sample$b_health[my_sample$wave == 2], exclude = NULL) # 1
table(nurse$b_health, exclude = NULL) # 2
table(core$b_health, exclude = NULL) # 3

