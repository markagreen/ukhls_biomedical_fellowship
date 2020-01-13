###################################
#### Load and clean UKHLS Data ####
###################################

# To ensure consistency of results and processes based on random numbers
set.seed(250388) 

# Load libraries
library(data.table)

# Load core data
ukhls <- fread("../../../../Desktop/Green_UKHLS/Green_Phenotype_sendout.txt")

### Clean variables ###

# List of of all variables for analysis #

all_vars <- c("sex", "age", "ukborn", "pacob", "macob", "marstat", "times_mar", "anychild_dv", # personal
              "hhsize", "numbeds", "numbedrm", "hhown", "centralheat", "hhval", "edu", # social
              "prb_hhold", "prb_ctax", "benefit1", "benefit2", "benefit3", "benefit4", "benefit5", 
              "benefit6", "benefit8", "benefit96", "benefit_other", "inc_divi", "subj_fin", "gross_pay",
              "b_ppen", "b_save", "b_saved", "jbstat", "jbperm", "jbsoc00", "manager", "jbhrs", "jbot", 
              "jbsat", "jb2", "b_jbsec", "b_wkends", "b_wkaut1", "b_wkaut2", "b_wkaut3", "b_wkaut4", 
              "b_wkaut5", "b_depenth1", "b_depenth2", "b_depenth3", "b_depenth4", "b_depenth5", "b_depenth6",
              "paedqf", "maedqf", "paju", "maju", "pasoc00_nb", "masoc00_nb",
              "b_matdepa", "b_matdepb", "b_matdepd", "b_matdepe", "b_matdepf", "b_matdepg", "b_matdeph",
              "sky_cable", "tumble", "dishw", "pc", "cdplayer", "pcnet", "ncars", "caruse", "mobuse",
              "b_volun", "b_chargv", "sprt_party", "party_vote", "party_close", "pol_interest",
              "hcond1", "hcond2", "hcond13", "hcond17", "hcond14", "hcond_cvd", "hcond16", # health
              "hcond_oth", "hcond96", "food_spend", "fdout_spend", "alcohol_spend", "b_usdairy", 
              "b_usbread", "b_wkfruit", "b_wkvege", "b_fruvege", "b_wlk10m", "b_daywlk", "b_wlk30min", 
              "b_walkpace", "b_wkphys", "b_sportsfreq", "b_sports3freq", "b_smever", "b_smnow", 
              "b_ncigs", "b_smcigs", "b_aglquit", "htval", "wtval", "bfpcval", "wstval", "bmival", 
              "mmgsdval", "mmgsnval", "lung_cap_fvc", "lung_cap_fev", "lung_cap_ratio", "lung_cap_peak",
              "omsysval", "omdiaval", "ompulval", "concentrate", "sleep", "useful", "decisions", 
              "strain", "overcoming", "enjoy_act", "face_prob", "depressed", "confidence", 
              "self_worth", "happiness",
              "chol", "hdl", "trig", "hba1c", "hgb", "rtin", "testo_m", "igfi", "dheas", # biomarker
              "hscrp", "uscmg", "uscmm", "cfib", "ecre", "ure", "alb", "alkp", "alt", "ast", "ggt")

## 1. Personal ##

personal <- c("sex", "age", "ukborn", "pacob", "macob", "marstat", "times_mar", "anychild_dv")

# 1.1 Non-modifiable #
personal_non_mod <- c("sex", "age")

# Sex 
ukhls$sex[ukhls$sex == "male"] <- 1 # Male
ukhls$sex[ukhls$sex == "female"] <- 2 # Female
ukhls$sex <- as.numeric(ukhls$sex) # Change character to numeric
# No missing data

# Age
ukhls$b_dvage[ukhls$b_dvage == "90-94" | ukhls$b_dvage == "95-99"] <- 90 # Convert older age categories into single 90+ group (but keep as single number for model purposes)
ukhls$c_dvage[ukhls$c_dvage == "90-94" | ukhls$c_dvage == "95-99"] <- 90 # Convert older age categories into single 90+ group (but keep as single number for model purposes)
ukhls$b_dvage[ukhls$b_dvage == ""] <- 0 # Set as missing (0) 
ukhls$c_dvage[ukhls$c_dvage == ""] <- 0 # Set as missing (0) 
ukhls$b_dvage <- as.numeric(ukhls$b_dvage) # Change character to numeric
ukhls$c_dvage <- as.numeric(ukhls$c_dvage) # Change character to numeric

ukhls$age <- NA # Create new variable to combine together
ukhls$age[ukhls$wave == 2] <- ukhls$b_dvage[ukhls$wave == 2] # Wave 2 baseline age
ukhls$age[ukhls$wave == 3] <- ukhls$c_dvage[ukhls$wave == 3] # Wave 3 baseline age

# 1.2 Cultural identity #
personal_cult_id <- c("ukborn", "pacob", "macob")

# Country of birth
ukhls$ukborn[ukhls$ukborn == "yes, england"] <- 1 # England
ukhls$ukborn[ukhls$ukborn == "yes, scotland"] <- 2 # Scotland
ukhls$ukborn[ukhls$ukborn == "yes, wales"] <- 3 # Wales
ukhls$ukborn[ukhls$ukborn == "yes, northern ireland" | ukhls$ukborn == "not born in the uk"] <- 4 # Not born in Great Britain (N.I. small n so hence change)
ukhls$ukborn[ukhls$ukborn == ""] <- 0 # Set as missing (0) 
ukhls$ukborn <- as.numeric(ukhls$ukborn) # Change character to numeric

# Father's country of birth
ukhls$pacob[ukhls$pacob == "england"] <- 1 # England 
ukhls$pacob[ukhls$pacob == "scotland"] <- 2 # Scotland
ukhls$pacob[ukhls$pacob == "wales"] <- 3 # Wales
ukhls$pacob[ukhls$pacob == ""] <- 0 # Set as missing (0) 
ukhls$pacob[ukhls$pacob != 0 & ukhls$pacob != 1 & ukhls$pacob != 2 & ukhls$pacob != 3] <- 4 # Outside of G.B.
ukhls$pacob <- as.numeric(ukhls$pacob) # Change character to numeric

# Mother's country of birth
ukhls$macob[ukhls$macob == "england"] <- 1 # England 
ukhls$macob[ukhls$macob == "scotland"] <- 2 # Scotland
ukhls$macob[ukhls$macob == "wales"] <- 3 # Wales
ukhls$macob[ukhls$macob == ""] <- 0 # Set as missing (0) 
ukhls$macob[ukhls$macob != 0 & ukhls$macob != 1 & ukhls$macob != 2 & ukhls$macob != 3] <- 4 # Outside of G.B.
ukhls$macob <- as.numeric(ukhls$macob) # Change character to numeric

# 1.3 Family #
personal_fam <- c("marstat", "times_mar", "anychild_dv")

# Marital status 
ukhls$b_marstat[ukhls$b_marstat == "single, nvr marr/civ p"] <- 1 # single and never married
ukhls$b_marstat[ukhls$b_marstat == "married" | ukhls$b_marstat == "civil partner (legal)"] <- 2 # married or civil partnership
ukhls$b_marstat[ukhls$b_marstat == "divorced" | ukhls$b_marstat == "separated indcluding from civil partner"] <- 3 # separated or divorced
ukhls$b_marstat[ukhls$b_marstat == "widowed"] <- 4 # widowed
ukhls$b_marstat[ukhls$b_marstat == ""] <- 0 # Missing

ukhls$c_marstat[ukhls$c_marstat == "single and never married or never in a legally recognised ci"] <- 1 # single and never married
ukhls$c_marstat[ukhls$c_marstat == "married" | ukhls$c_marstat == "a civil partner in a legally recognised civil partnership"] <- 2 # married or civil partnership
ukhls$c_marstat[ukhls$c_marstat == "divorced" | ukhls$c_marstat == "separated indcluding from civil partner"] <- 3 # separated or divorced
ukhls$c_marstat[ukhls$c_marstat == "widowed"] <- 4 # widowed
ukhls$c_marstat[ukhls$c_marstat == ""] <- 0 # Missing

ukhls$marstat[ukhls$wave == 2] <- ukhls$b_marstat[ukhls$wave == 2]
ukhls$marstat[ukhls$wave == 3] <- ukhls$c_marstat[ukhls$wave == 3]
ukhls$marstat <- as.numeric(ukhls$marstat) # Change character to numeric

# Number of times married (for wave 3 use nmarn)
ukhls$nmar[ukhls$nmar == "4 or more"] <- 4 # Change to 3+ but code as 3
ukhls$nmar[ukhls$nmar == 3] <- 4
ukhls$nmar[ukhls$nmar == 2] <- 3
ukhls$nmar[ukhls$nmar == 1] <- 2
ukhls$nmar[ukhls$b_marstat == 1] <- 1 # Impute single and never married as 0 - leave other blanks as blank
ukhls$nmar[ukhls$nmar == ""] <- 0 # Missing
ukhls$nmar <- as.numeric(ukhls$nmar)

ukhls$nmarn[ukhls$nmarn == "4 or more"] <- 4 # Change to 3+ but code as 3
ukhls$nmarn[ukhls$nmarn == 3] <- 4
ukhls$nmarn[ukhls$nmarn == 2] <- 3
ukhls$nmarn[ukhls$nmarn == 1] <- 2
ukhls$nmarn[ukhls$c_marstat == 1] <- 1 # Impute single and never married as 0 - leave other blanks as blank
ukhls$nmarn[ukhls$nmarn == ""] <- 0 # Missing
ukhls$nmarn <- as.numeric(ukhls$nmarn)

ukhls$times_mar <- 0 # Create new variable for analysis at baseline
ukhls$times_mar[ukhls$wave == 2] <- ukhls$nmar[ukhls$wave == 2]
ukhls$times_mar[ukhls$wave == 3] <- ukhls$nmarn[ukhls$wave == 3]

# Any children
ukhls$anychild_dv[ukhls$anychild_dv == "Yes"] <- 1 # Yes
ukhls$anychild_dv[ukhls$anychild_dv == "No"] <- 2 # No
ukhls$anychild_dv[ukhls$anychild_dv == ""] <- 0 # Missing
ukhls$anychild_dv <- as.numeric(ukhls$anychild_dv)

## 2. Socio-economic ##

ses <- c("hhsize", "numbeds", "numbedrm", "hhown", "centralheat", "hhval", "edu",
         "prb_hhold", "prb_ctax", "benefit1", "benefit2", "benefit3", "benefit4", "benefit5", 
         "benefit6", "benefit8", "benefit96", "benefit_other", "inc_divi", "subj_fin", "gross_pay",
         "b_ppen", "b_save", "b_saved", "jbstat", "jbperm", "jbsoc00", "manager", "jbhrs", "jbot", 
         "jbsat", "jb2", "b_jbsec", "b_wkends", "b_wkaut1", "b_wkaut2", "b_wkaut3", "b_wkaut4", 
         "b_wkaut5", "b_depenth1", "b_depenth2", "b_depenth3", "b_depenth4", "b_depenth5", "b_depenth6",
         "paedqf", "maedqf", "paju", "maju", "pasoc00_nb", "masoc00_nb",
         "b_matdepa", "b_matdepb", "b_matdepd", "b_matdepe", "b_matdepf", "b_matdepg", "b_matdeph",
         "sky_cable", "tumble", "dishw", "pc", "cdplayer", "pcnet", "ncars", "caruse", "mobuse",
         "b_volun", "b_chargv", "sprt_party", "party_vote", "party_close", "pol_interest")

ses_bc <- c("hhsize", "numbeds", "numbedrm", "hhown", "centralheat", "hhval", "edu", 
            "prb_hhold", "prb_ctax", "benefit1", "benefit2", "benefit3", "benefit4", "benefit5", 
            "benefit6", "benefit8", "benefit96", "benefit_other", "inc_divi", "subj_fin", "gross_pay",
            "jbstat", "jbperm", "jbsoc00", "manager", "jbhrs", "jbot", "jbsat", "jb2", 
            "paedqf", "maedqf", "paju", "maju", "pasoc00_nb", "masoc00_nb",
            "sky_cable", "tumble", "dishw", "pc", "cdplayer", "pcnet", "ncars", "caruse", "mobuse",
            "sprt_party", "party_vote", "party_close", "pol_interest")

# Housing #

ses_house <- c("hhsize", "numbeds", "numbedrm", "hhown", "centralheat", "hhval")

# Number of people in household
ukhls$b_hhsize[ukhls$b_hhsize == 7 | ukhls$b_hhsize == "8-10"] <- 6 # Create a category for 6+
ukhls$c_hhsize[ukhls$c_hhsize == 7 | ukhls$c_hhsize == "8-10"] <- 6
ukhls$b_hhsize[ukhls$b_hhsize == ""] <- 0 # Missing
ukhls$c_hhsize[ukhls$c_hhsize == ""] <- 0
ukhls$b_hhsize <- as.numeric(ukhls$b_hhsize) # Convert to numeric
ukhls$c_hhsize <- as.numeric(ukhls$c_hhsize)

ukhls$hhsize <- 0 # Create new variable for analysis at baseline
ukhls$hhsize[ukhls$wave == 2] <- ukhls$b_hhsize[ukhls$wave == 2]
ukhls$hhsize[ukhls$wave == 3] <- ukhls$c_hhsize[ukhls$wave == 3]

# Number of beds in household
ukhls$b_hsbeds[ukhls$b_hsbeds == 7 | ukhls$b_hsbeds == "8-11"] <- 6 # Create a category for 6+
ukhls$c_hsbeds[ukhls$c_hsbeds == 7 | ukhls$c_hsbeds == "8-11"] <- 6
ukhls$b_hsbeds[ukhls$b_hsbeds == 0] <- 1 # Recode 0 and 1 to '1 or fewer'
ukhls$c_hsbeds[ukhls$c_hsbeds == 0] <- 1
ukhls$b_hsbeds[ukhls$b_hsbeds == ""] <- 0 # Missing
ukhls$c_hsbeds[ukhls$c_hsbeds == ""] <- 0
ukhls$b_hsbeds <- as.numeric(ukhls$b_hsbeds) # Convert to numeric
ukhls$c_hsbeds <- as.numeric(ukhls$c_hsbeds)

ukhls$numbeds <- 0 # Create new variable for analysis at baseline
ukhls$numbeds[ukhls$wave == 2] <- ukhls$b_hsbeds[ukhls$wave == 2]
ukhls$numbeds[ukhls$wave == 3] <- ukhls$c_hsbeds[ukhls$wave == 3]

# Number of bedrooms
ukhls$b_hsrooms[ukhls$b_hsrooms == 7 | ukhls$b_hsrooms == 8 | ukhls$b_hsrooms == "9-15"] <- 6 # Create a category for 6+
ukhls$c_hsrooms[ukhls$c_hsrooms == 7 | ukhls$c_hsrooms == 8 | ukhls$c_hsrooms == "9-15"] <- 6
ukhls$b_hsrooms[ukhls$b_hsrooms == ""] <- 0 # Missing
ukhls$c_hsrooms[ukhls$c_hsrooms == ""] <- 0
ukhls$b_hsrooms <- as.numeric(ukhls$b_hsrooms) # Convert to numeric
ukhls$c_hsrooms <- as.numeric(ukhls$c_hsrooms)

ukhls$numbedrm <- 0 # Create new variable for analysis at baseline
ukhls$numbedrm[ukhls$wave == 2] <- ukhls$b_hsrooms[ukhls$wave == 2]
ukhls$numbedrm[ukhls$wave == 3] <- ukhls$c_hsrooms[ukhls$wave == 3]

# Owner occupency status
ukhls$b_hsownd[ukhls$b_hsownd == "owned outright"] <- 4 # Owned outright
ukhls$c_hsownd[ukhls$c_hsownd == "owned outright"] <- 4
ukhls$b_hsownd[ukhls$b_hsownd == "owned/being bought on mortgage"] <- 3 # Recode 0 and 1 to '1 or fewer'
ukhls$c_hsownd[ukhls$c_hsownd == "owned/being bought on mortgage"] <- 3
ukhls$b_hsownd[ukhls$b_hsownd == "rented"] <- 2 # Recode 0 and 1 to '1 or fewer'
ukhls$c_hsownd[ukhls$c_hsownd == "rented"] <- 2
ukhls$b_hsownd[ukhls$b_hsownd == "rent free" | ukhls$b_hsownd == "shared ownership (part-owned part-rented)" | ukhls$b_hsownd == "other"] <- 1 # Other
ukhls$c_hsownd[ukhls$c_hsownd == "rent free" | ukhls$c_hsownd == "shared ownership (part-owned part-rented)" | ukhls$c_hsownd == "other"] <- 1
ukhls$b_hsownd[ukhls$b_hsownd == ""] <- 0 # Missing
ukhls$c_hsownd[ukhls$c_hsownd == ""] <- 0
ukhls$b_hsownd <- as.numeric(ukhls$b_hsownd) # Convert to numeric
ukhls$c_hsownd <- as.numeric(ukhls$c_hsownd)

ukhls$hhown <- 0 # Create new variable for analysis at baseline
ukhls$hhown[ukhls$wave == 2] <- ukhls$b_hsownd[ukhls$wave == 2]
ukhls$hhown[ukhls$wave == 3] <- ukhls$c_hsownd[ukhls$wave == 3]

# Has central heating
ukhls$b_heatch[ukhls$b_heatch == "yes"] <- 1 # Yes
ukhls$c_heatch[ukhls$c_heatch == "yes"] <- 1
ukhls$b_heatch[ukhls$b_heatch == "no"] <- 2 # No
ukhls$c_heatch[ukhls$c_heatch == "no"] <- 2
ukhls$b_heatch[ukhls$b_heatch == ""] <- 0 # Missing
ukhls$c_heatch[ukhls$c_heatch == ""] <- 0
ukhls$b_heatch <- as.numeric(ukhls$b_heatch) # Convert to numeric
ukhls$c_heatch <- as.numeric(ukhls$c_heatch)

ukhls$centralheat <- 0 # Create new variable for analysis at baseline
ukhls$centralheat[ukhls$wave == 2] <- ukhls$b_heatch[ukhls$wave == 2]
ukhls$centralheat[ukhls$wave == 3] <- ukhls$c_heatch[ukhls$wave == 3]

# House value (if own)
ukhls$b_hsval[ukhls$b_hsval == ""] <- 0 # Missing
ukhls$b_hsval[ukhls$b_hsval == "0.01-75000"] <- 1 # £0.01-75k
ukhls$b_hsval[ukhls$b_hsval == "75000-85000" | ukhls$b_hsval == "85000-90000" | ukhls$b_hsval == "90000-100000"] <- 2 # £75k-100k
ukhls$b_hsval[ukhls$b_hsval == "100000-110000" | ukhls$b_hsval == "110000-120000" | ukhls$b_hsval == "120000-125000"] <- 3 # £100k-125k
ukhls$b_hsval[ukhls$b_hsval == "125000-130000" | ukhls$b_hsval == "130000-135000" | ukhls$b_hsval == "135000-140000" | ukhls$b_hsval == "140000-145000" | ukhls$b_hsval == "145000-150000"] <- 4 # £125k-150k
ukhls$b_hsval[ukhls$b_hsval == "150000-160000" | ukhls$b_hsval == "160000-169000" | ukhls$b_hsval == "169000-170000" | ukhls$b_hsval == "170000-175000"] <- 5 # £150k-175k
ukhls$b_hsval[ukhls$b_hsval == "175000-180000" | ukhls$b_hsval == "180000-185000" | ukhls$b_hsval == "185000-190000" | ukhls$b_hsval == "190000-200000"] <- 6 # £175k-200k
ukhls$b_hsval[ukhls$b_hsval == "200000-210000" | ukhls$b_hsval == "210000-220000" | ukhls$b_hsval == "220000-225000"] <- 7 # £200k-225k
ukhls$b_hsval[ukhls$b_hsval == "225000-230000" | ukhls$b_hsval == "230000-240000" | ukhls$b_hsval == "240000-250000"] <- 8 # £225k-250k
ukhls$b_hsval[ukhls$b_hsval == "250000-255000" | ukhls$b_hsval == "255000-270000" | ukhls$b_hsval == "270000-280000" | ukhls$b_hsval == "280000-300000"] <- 9 # £250k-300k
ukhls$b_hsval[ukhls$b_hsval == "300000-330000" | ukhls$b_hsval == "330000-350000"] <- 10 # £300k-350k
ukhls$b_hsval[ukhls$b_hsval == "350000-360000" | ukhls$b_hsval == "360000-400000"] <- 11 # £350k-400k
ukhls$b_hsval[ukhls$b_hsval == "400000-450000" | ukhls$b_hsval == "450000-500000"] <- 12 # £400k-500k
ukhls$b_hsval[ukhls$b_hsval == "500000-600000" | ukhls$b_hsval == "600000-750000" | ukhls$b_hsval == "750000 or more"] <- 13 # £500k+
ukhls$b_hsval <- as.numeric(ukhls$b_hsval)

ukhls$c_hsval[ukhls$c_hsval == ""] <- 0 # Missing
ukhls$c_hsval[ukhls$c_hsval == "0.01-75000"] <- 1 # £0.01-75k
ukhls$c_hsval[ukhls$c_hsval == "75000-85000" | ukhls$c_hsval == "85000-90000" | ukhls$c_hsval == "90000-100000"] <- 2 # £75k-100k
ukhls$c_hsval[ukhls$c_hsval == "100000-110000" | ukhls$c_hsval == "110000-120000" | ukhls$c_hsval == "120000-125000"] <- 3 # £100k-125k
ukhls$c_hsval[ukhls$c_hsval == "125000-130000" | ukhls$c_hsval == "130000-135000" | ukhls$c_hsval == "135000-140000" | ukhls$c_hsval == "140000-145000" | ukhls$c_hsval == "145000-150000"] <- 4 # £125k-150k
ukhls$c_hsval[ukhls$c_hsval == "150000-160000" | ukhls$c_hsval == "160000-169000" | ukhls$c_hsval == "169000-170000" | ukhls$c_hsval == "170000-175000"] <- 5 # £150k-175k
ukhls$c_hsval[ukhls$c_hsval == "175000-180000" | ukhls$c_hsval == "180000-185000" | ukhls$c_hsval == "185000-190000" | ukhls$c_hsval == "190000-200000"] <- 6 # £175k-200k
ukhls$c_hsval[ukhls$c_hsval == "200000-210000" | ukhls$c_hsval == "210000-220000" | ukhls$c_hsval == "220000-225000"] <- 7 # £200k-225k
ukhls$c_hsval[ukhls$c_hsval == "225000-230000" | ukhls$c_hsval == "230000-240000" | ukhls$c_hsval == "240000-250000"] <- 8 # £225k-250k
ukhls$c_hsval[ukhls$c_hsval == "250000-255000" | ukhls$c_hsval == "255000-270000" | ukhls$c_hsval == "270000-280000" | ukhls$c_hsval == "280000-300000"] <- 9 # £250k-300k
ukhls$c_hsval[ukhls$c_hsval == "300000-330000" | ukhls$c_hsval == "330000-350000"] <- 10 # £300k-350k
ukhls$c_hsval[ukhls$c_hsval == "350000-360000" | ukhls$c_hsval == "360000-400000"] <- 11 # £350k-400k
ukhls$c_hsval[ukhls$c_hsval == "400000-450000" | ukhls$c_hsval == "450000-500000"] <- 12 # £400k-500k
ukhls$c_hsval[ukhls$c_hsval == "500000-600000" | ukhls$c_hsval == "600000-750000" | ukhls$c_hsval == "750000 or more"] <- 13 # £500k+
ukhls$c_hsval <- as.numeric(ukhls$c_hsval)

ukhls$hhval <- 0 # Create new variable for analysis at baseline
ukhls$hhval[ukhls$wave == 2] <- ukhls$b_hsval[ukhls$wave == 2]
ukhls$hhval[ukhls$wave == 3] <- ukhls$c_hsval[ukhls$wave == 3]

# Education #

ses_edu <- c("edu")

# Highest qualification
ukhls$b_qfhigh_dv[ukhls$b_qfhigh_dv == "Higher degree" | ukhls$b_qfhigh_dv == "Other higher degree"] <- 8 # 1st Degree or equivalent
ukhls$c_qfhigh_dv[ukhls$c_qfhigh_dv == "Higher degree" | ukhls$c_qfhigh_dv == "Other higher degree"] <- 8
ukhls$b_qfhigh_dv[ukhls$b_qfhigh_dv == "1st degree or equivalent"] <- 7 # 1st Degree or equivalent
ukhls$c_qfhigh_dv[ukhls$c_qfhigh_dv == "1st degree or equivalent"] <- 7
ukhls$b_qfhigh_dv[ukhls$b_qfhigh_dv == "Diploma in he"] <- 6 # Diploma in HE
ukhls$c_qfhigh_dv[ukhls$c_qfhigh_dv == "Diploma in he"] <- 6
ukhls$b_qfhigh_dv[ukhls$b_qfhigh_dv == "A level / IB" | ukhls$b_qfhigh_dv == "AS level" | ukhls$b_qfhigh_dv == "Highers (scot)" | ukhls$b_qfhigh_dv == "Cert 6th year studies"] <- 5 # A-level/AS-level/Highers (Scot)
ukhls$c_qfhigh_dv[ukhls$c_qfhigh_dv == "A level / IB" | ukhls$c_qfhigh_dv == "AS level" | ukhls$c_qfhigh_dv == "Highers (scot)" | ukhls$c_qfhigh_dv == "Cert 6th year studies"] <- 5
ukhls$b_qfhigh_dv[ukhls$b_qfhigh_dv == "Nursing/other med qual"] <- 4 # Nursing
ukhls$c_qfhigh_dv[ukhls$c_qfhigh_dv == "Nursing/other med qual"] <- 4
ukhls$b_qfhigh_dv[ukhls$b_qfhigh_dv == "Teaching qual not pgce"] <- 3 # Teaching not PGCE
ukhls$c_qfhigh_dv[ukhls$c_qfhigh_dv == "Teaching qual not pgce"] <- 3
ukhls$b_qfhigh_dv[ukhls$b_qfhigh_dv == "GCSE/O level" | ukhls$b_qfhigh_dv == "CSE" | ukhls$b_qfhigh_dv == "Standard/o/lower" | ukhls$b_qfhigh_dv == "Other school cert"] <- 2 # CSE/O-level
ukhls$c_qfhigh_dv[ukhls$c_qfhigh_dv == "GCSE/O level" | ukhls$c_qfhigh_dv == "CSE" | ukhls$c_qfhigh_dv == "Standard/o/lower" | ukhls$c_qfhigh_dv == "Other school cert"] <- 2
ukhls$b_qfhigh_dv[ukhls$b_qfhigh_dv == "None of the above"] <- 1 # No qualifications
ukhls$c_qfhigh_dv[ukhls$c_qfhigh_dv == "None of the above"] <- 1
ukhls$b_qfhigh_dv[ukhls$b_qfhigh_dv == ""] <- 0 # Missing
ukhls$c_qfhigh_dv[ukhls$c_qfhigh_dv == ""] <- 0
ukhls$b_qfhigh_dv <- as.numeric(ukhls$b_qfhigh_dv) # Convert to numeric
ukhls$c_qfhigh_dv <- as.numeric(ukhls$c_qfhigh_dv)
 
ukhls$edu <- 0 # Create new variable for analysis at baseline
ukhls$edu[ukhls$wave == 2] <- ukhls$b_qfhigh_dv[ukhls$wave == 2]
ukhls$edu[ukhls$wave == 3] <- ukhls$c_qfhigh_dv[ukhls$wave == 3]

# Income #

ses_inc_bc <- c("prb_hhold", "prb_ctax", "benefit1", "benefit2", "benefit3", "benefit4", "benefit5", 
                "benefit6", "benefit8", "benefit96", "benefit_other", "inc_divi", "subj_fin", "gross_pay")
ses_inc <- c("prb_hhold", "prb_ctax", "benefit1", "benefit2", "benefit3", "benefit4", "benefit5", 
             "benefit6", "benefit8", "benefit96", "benefit_other", "inc_divi", "subj_fin", "gross_pay",
             "b_ppen", "b_save", "b_saved")

# Whether contributes to pension
ukhls$b_ppen[ukhls$b_ppen == ""] <- 0 # Missing
ukhls$b_ppen[ukhls$b_ppen == "yes"] <- 1 # Yes
ukhls$b_ppen[ukhls$b_ppen == "no"] <- 2 # No
ukhls$b_ppen <- as.numeric(ukhls$b_ppen) # Convert to numeric

# Whether saves money
ukhls$b_save[ukhls$b_save == ""] <- 0 # Missing
ukhls$b_save[ukhls$b_save == "yes"] <- 1 # Yes
ukhls$b_save[ukhls$b_save == "no"] <- 2 # No
ukhls$b_save <- as.numeric(ukhls$b_save) # Convert to numeric

# Monthly amount saved
ukhls$b_saved[ukhls$b_saved == ""] <- 0 # Missing
ukhls$b_saved[ukhls$b_save == 1] <- 1 # Don't save
ukhls$b_saved[ukhls$b_saved == "0.01-10" | ukhls$b_saved == "10-20" | ukhls$b_saved == "20-25" | ukhls$b_saved == "25-30" | ukhls$b_saved == "30-40" | ukhls$b_saved == "40-50"] <- 2 # 0.01-50
ukhls$b_saved[ukhls$b_saved == "50-70" | ukhls$b_saved == "70-80" | ukhls$b_saved == "80-100"] <- 3 # 50-100
ukhls$b_saved[ukhls$b_saved == "100-120" | ukhls$b_saved == "120-150"] <- 4 # 100-150
ukhls$b_saved[ukhls$b_saved == "150-160" | ukhls$b_saved == "160-200"] <- 5 # 150-200
ukhls$b_saved[ukhls$b_saved == "200-250" | ukhls$b_saved == "250-300" | ukhls$b_saved == "300-350" | ukhls$b_saved == "350-400"] <- 6 # 200-400
ukhls$b_saved[ukhls$b_saved == "400-500" | ukhls$b_saved == "500-600" | ukhls$b_saved == "600-750" | ukhls$b_saved == "750-1000" | ukhls$b_saved == "1000 or more"] <- 7 # 400+
ukhls$b_saved <- as.numeric(ukhls$b_saved) # Convert to numeric

# Problems paying for housing
ukhls$b_xphsdb[ukhls$b_xphsdb == "yes"] <- 1 # Yes
ukhls$c_xphsdb[ukhls$c_xphsdb == "yes"] <- 1
ukhls$b_xphsdb[ukhls$b_xphsdb == "no"] <- 2 # No
ukhls$c_xphsdb[ukhls$c_xphsdb == "no"] <- 2
ukhls$b_xphsdb[ukhls$b_xphsdb == ""] <- 0 # Missing
ukhls$c_xphsdb[ukhls$c_xphsdb == ""] <- 0
ukhls$b_xphsdb <- as.numeric(ukhls$b_xphsdb) # Convert to numeric
ukhls$c_xphsdb <- as.numeric(ukhls$c_xphsdb)

ukhls$prb_hhold <- 0 # Create new variable for analysis at baseline
ukhls$prb_hhold[ukhls$wave == 2] <- ukhls$b_xphsdb[ukhls$wave == 2]
ukhls$prb_hhold[ukhls$wave == 3] <- ukhls$c_xphsdb[ukhls$wave == 3]

# Problems paying for Council tax
ukhls$b_xphsdct[ukhls$b_xphsdct == "yes"] <- 1 # Yes
ukhls$c_xphsdct[ukhls$c_xphsdct == "yes"] <- 1
ukhls$b_xphsdct[ukhls$b_xphsdct == "no"] <- 2 # No
ukhls$c_xphsdct[ukhls$c_xphsdct == "no"] <- 2
ukhls$b_xphsdct[ukhls$b_xphsdct == ""] <- 0 # Missing
ukhls$c_xphsdct[ukhls$c_xphsdct == ""] <- 0
ukhls$b_xphsdct <- as.numeric(ukhls$b_xphsdct) # Convert to numeric
ukhls$c_xphsdct <- as.numeric(ukhls$c_xphsdct)

ukhls$prb_ctax <- 0 # Create new variable for analysis at baseline
ukhls$prb_ctax[ukhls$wave == 2] <- ukhls$b_xphsdct[ukhls$wave == 2]
ukhls$prb_ctax[ukhls$wave == 3] <- ukhls$c_xphsdct[ukhls$wave == 3]

# Type of benefits received
# 1. Unemployment related
ukhls$b_btype1[ukhls$b_btype1 == "mentioned"] <- 1 # Yes
ukhls$c_btype1[ukhls$c_btype1 == "mentioned"] <- 1
ukhls$b_btype1[ukhls$b_btype1 == "not mentioned"] <- 2 # No
ukhls$c_btype1[ukhls$c_btype1 == "not mentioned"] <- 2
ukhls$b_btype1[ukhls$b_btype1 == ""] <- 0 # Missing
ukhls$c_btype1[ukhls$c_btype1 == ""] <- 0
ukhls$b_btype1 <- as.numeric(ukhls$b_btype1) # Convert to numeric
ukhls$c_btype1 <- as.numeric(ukhls$c_btype1)

ukhls$benefit1 <- 0 # Create new variable for analysis at baseline
ukhls$benefit1[ukhls$wave == 2] <- ukhls$b_btype1[ukhls$wave == 2]
ukhls$benefit1[ukhls$wave == 3] <- ukhls$c_btype1[ukhls$wave == 3]

# 2. Income support
ukhls$b_btype2[ukhls$b_btype2 == "mentioned"] <- 1 # Yes
ukhls$c_btype2[ukhls$c_btype2 == "mentioned"] <- 1
ukhls$b_btype2[ukhls$b_btype2 == "not mentioned"] <- 2 # No
ukhls$c_btype2[ukhls$c_btype2 == "not mentioned"] <- 2
ukhls$b_btype2[ukhls$b_btype2 == ""] <- 0 # Missing
ukhls$c_btype2[ukhls$c_btype2 == ""] <- 0
ukhls$b_btype2 <- as.numeric(ukhls$b_btype2) # Convert to numeric
ukhls$c_btype2 <- as.numeric(ukhls$c_btype2)

ukhls$benefit2 <- 0 # Create new variable for analysis at baseline
ukhls$benefit2[ukhls$wave == 2] <- ukhls$b_btype2[ukhls$wave == 2]
ukhls$benefit2[ukhls$wave == 3] <- ukhls$c_btype2[ukhls$wave == 3]

# 3. Incapacity
ukhls$b_btype3[ukhls$b_btype3 == "mentioned"] <- 1 # Yes
ukhls$c_btype3[ukhls$c_btype3 == "mentioned"] <- 1
ukhls$b_btype3[ukhls$b_btype3 == "not mentioned"] <- 2 # No
ukhls$c_btype3[ukhls$c_btype3 == "not mentioned"] <- 2
ukhls$b_btype3[ukhls$b_btype3 == ""] <- 0 # Missing
ukhls$c_btype3[ukhls$c_btype3 == ""] <- 0
ukhls$b_btype3 <- as.numeric(ukhls$b_btype3) # Convert to numeric
ukhls$c_btype3 <- as.numeric(ukhls$c_btype3)

ukhls$benefit3 <- 0 # Create new variable for analysis at baseline
ukhls$benefit3[ukhls$wave == 2] <- ukhls$b_btype3[ukhls$wave == 2]
ukhls$benefit3[ukhls$wave == 3] <- ukhls$c_btype3[ukhls$wave == 3]

# 4. Pension
ukhls$b_btype4[ukhls$b_btype4 == "mentioned"] <- 1 # Yes
ukhls$c_btype4[ukhls$c_btype4 == "mentioned"] <- 1
ukhls$b_btype4[ukhls$b_btype4 == "not mentioned"] <- 2 # No
ukhls$c_btype4[ukhls$c_btype4 == "not mentioned"] <- 2
ukhls$b_btype4[ukhls$b_btype4 == ""] <- 0 # Missing
ukhls$c_btype4[ukhls$c_btype4 == ""] <- 0
ukhls$b_btype4 <- as.numeric(ukhls$b_btype4) # Convert to numeric
ukhls$c_btype4 <- as.numeric(ukhls$c_btype4)

ukhls$benefit4 <- 0 # Create new variable for analysis at baseline
ukhls$benefit4[ukhls$wave == 2] <- ukhls$b_btype4[ukhls$wave == 2]
ukhls$benefit4[ukhls$wave == 3] <- ukhls$c_btype4[ukhls$wave == 3]

# 5. Child benefit
ukhls$b_btype5[ukhls$b_btype5 == "mentioned"] <- 1 # Yes
ukhls$c_btype5[ukhls$c_btype5 == "mentioned"] <- 1
ukhls$b_btype5[ukhls$b_btype5 == "not mentioned"] <- 2 # No
ukhls$c_btype5[ukhls$c_btype5 == "not mentioned"] <- 2
ukhls$b_btype5[ukhls$b_btype5 == ""] <- 0 # Missing
ukhls$c_btype5[ukhls$c_btype5 == ""] <- 0
ukhls$b_btype5 <- as.numeric(ukhls$b_btype5) # Convert to numeric
ukhls$c_btype5 <- as.numeric(ukhls$c_btype5)

ukhls$benefit5 <- 0 # Create new variable for analysis at baseline
ukhls$benefit5[ukhls$wave == 2] <- ukhls$b_btype5[ukhls$wave == 2]
ukhls$benefit5[ukhls$wave == 3] <- ukhls$c_btype5[ukhls$wave == 3]

# 6. Tax credits
ukhls$b_btype6[ukhls$b_btype6 == "mentioned"] <- 1 # Yes
ukhls$c_btype6[ukhls$c_btype6 == "mentioned"] <- 1
ukhls$b_btype6[ukhls$b_btype6 == "not mentioned"] <- 2 # No
ukhls$c_btype6[ukhls$c_btype6 == "not mentioned"] <- 2
ukhls$b_btype6benefit8[ukhls$b_btype6 == ""] <- 0 # Missing
ukhls$c_btype6[ukhls$c_btype6 == ""] <- 0
ukhls$b_btype6 <- as.numeric(ukhls$b_btype6) # Convert to numeric
ukhls$c_btype6 <- as.numeric(ukhls$c_btype6)

ukhls$benefit6 <- 0 # Create new variable for analysis at baseline
ukhls$benefit6[ukhls$wave == 2] <- ukhls$b_btype6[ukhls$wave == 2]
ukhls$benefit6[ukhls$wave == 3] <- ukhls$c_btype6[ukhls$wave == 3]

# 8. Housing/council tax
ukhls$b_btype8[ukhls$b_btype8 == "mentioned"] <- 1 # Yes
ukhls$c_btype8[ukhls$c_btype8 == "mentioned"] <- 1
ukhls$b_btype8[ukhls$b_btype8 == "not mentioned"] <- 2 # No
ukhls$c_btype8[ukhls$c_btype8 == "not mentioned"] <- 2
ukhls$b_btype8[ukhls$b_btype8 == ""] <- 0 # Missing
ukhls$c_btype8[ukhls$c_btype8 == ""] <- 0
ukhls$b_btype8 <- as.numeric(ukhls$b_btype8) # Convert to numeric
ukhls$c_btype8 <- as.numeric(ukhls$c_btype8)

ukhls$benefit8 <- 0 # Create new variable for analysis at baseline
ukhls$benefit8[ukhls$wave == 2] <- ukhls$b_btype8[ukhls$wave == 2]
ukhls$benefit8[ukhls$wave == 3] <- ukhls$c_btype8[ukhls$wave == 3]

# 9. Other (7 is Other family related and 9 is income from other benefit)
ukhls$benefit_other <- 0 # Create new variable for analysis at baseline
ukhls$benefit_other[ukhls$wave == 2 & (ukhls$b_btype7 == "not mentioned" | ukhls$b_btype9 == "not mentioned")] <- 2 # No
ukhls$benefit_other[ukhls$wave == 3 & (ukhls$c_btype7 == "not mentioned" | ukhls$c_btype9 == "not mentioned")] <- 2
ukhls$benefit_other[ukhls$wave == 2 & (ukhls$b_btype7 == "mentioned" | ukhls$b_btype9 == "mentioned")] <- 1 # yes
ukhls$benefit_other[ukhls$wave == 3 & (ukhls$c_btype7 == "mentioned" | ukhls$c_btype9 == "mentioned")] <- 1

# 96. None of these
ukhls$b_btype96[ukhls$b_btype96 == "mentioned"] <- 1 # Yes
ukhls$c_btype96[ukhls$c_btype96 == "mentioned"] <- 1
ukhls$b_btype96[ukhls$b_btype96 == "not mentioned"] <- 2 # No
ukhls$c_btype96[ukhls$c_btype96 == "not mentioned"] <- 2
ukhls$b_btype96[ukhls$b_btype96 == ""] <- 0 # Missing
ukhls$c_btype96[ukhls$c_btype96 == ""] <- 0
ukhls$b_btype96 <- as.numeric(ukhls$b_btype96) # Convert to numeric
ukhls$c_btype96 <- as.numeric(ukhls$c_btype96)

ukhls$benefit96 <- 0 # Create new variable for analysis at baseline
ukhls$benefit96[ukhls$wave == 2] <- ukhls$b_btype96[ukhls$wave == 2]
ukhls$benefit96[ukhls$wave == 3] <- ukhls$c_btype96[ukhls$wave == 3]

# Income received in interest/dividends
ukhls$b_fiyrdia[ukhls$b_fiyrdia == 0] <- 1 # 0
ukhls$c_fiyrdia[ukhls$c_fiyrdia == 0] <- 1
ukhls$b_fiyrdia[ukhls$b_fiyrdia == "0.01-8" | ukhls$b_fiyrdia == "8-20" | ukhls$b_fiyrdia == "20-40" | ukhls$b_fiyrdia == "40-80" | ukhls$b_fiyrdia == "80-100"] <- 2 # 0-100
ukhls$c_fiyrdia[ukhls$c_fiyrdia == "0.01-10" | ukhls$c_fiyrdia == "10-20" | ukhls$c_fiyrdia == "20-50" | ukhls$c_fiyrdia == "50-100"] <- 2
ukhls$b_fiyrdia[ukhls$b_fiyrdia == "100-200" | ukhls$b_fiyrdia == "200-400" | ukhls$b_fiyrdia == "400-900" | ukhls$b_fiyrdia == "900-2100" | ukhls$b_fiyrdia == "2100 or more"] <- 3 # 100+
ukhls$c_fiyrdia[ukhls$c_fiyrdia == "100-130" | ukhls$c_fiyrdia == "130-250" | ukhls$c_fiyrdia == "250-500" | ukhls$c_fiyrdia == "500-1000" | ukhls$c_fiyrdia == "1000-2500" | ukhls$c_fiyrdia == "2500 or more"] <- 3
ukhls$b_fiyrdia[ukhls$b_fiyrdia == ""] <- 0 # Missing
ukhls$c_fiyrdia[ukhls$c_fiyrdia == ""] <- 0
ukhls$b_fiyrdia <- as.numeric(ukhls$b_fiyrdia) # Convert to numeric
ukhls$c_fiyrdia <- as.numeric(ukhls$c_fiyrdia)

ukhls$inc_divi <- 0 # Create new variable for analysis at baseline
ukhls$inc_divi[ukhls$wave == 2] <- ukhls$b_fiyrdia[ukhls$wave == 2]
ukhls$inc_divi[ukhls$wave == 3] <- ukhls$c_fiyrdia[ukhls$wave == 3]

# Subjective financial situation
ukhls$b_finnow[ukhls$b_finnow == "living comfortably"] <- 1 # Living comfortably
ukhls$c_finnow[ukhls$c_finnow == "living comfortably"] <- 1
ukhls$b_finnow[ukhls$b_finnow == "doing alright"] <- 2 # Doing alright
ukhls$c_finnow[ukhls$c_finnow == "doing alright"] <- 2
ukhls$b_finnow[ukhls$b_finnow == "just about getting by"] <- 3 # Just about getting by
ukhls$c_finnow[ukhls$c_finnow == "just about getting by"] <- 3
ukhls$b_finnow[ukhls$b_finnow == "finding it quite diff" | ukhls$b_finnow == "finding it very diff"] <- 4 # Just about getting by
ukhls$c_finnow[ukhls$c_finnow == "finding it quite difficult" | ukhls$c_finnow == "or finding it very difficult?"] <- 4
ukhls$b_finnow[ukhls$b_finnow == ""] <- 0 # Missing
ukhls$c_finnow[ukhls$c_finnow == ""] <- 0
ukhls$b_finnow <- as.numeric(ukhls$b_finnow) # Convert to numeric
ukhls$c_finnow <- as.numeric(ukhls$c_fiyrdia)

ukhls$subj_fin <- 0 # Create new variable for analysis at baseline
ukhls$subj_fin[ukhls$wave == 2] <- ukhls$b_finnow[ukhls$wave == 2]
ukhls$subj_fin[ukhls$wave == 3] <- ukhls$c_finnow[ukhls$wave == 3]

# Gross pay
ukhls$b_paygu_dv[ukhls$b_paygu_dv == "0.01-173.4350891113281" | ukhls$b_paygu_dv == "173.4350891113281-250" | ukhls$b_paygu_dv == "250-325" | ukhls$b_paygu_dv == "325-403" | ukhls$b_paygu_dv == "403-476.6666564941406"] <- 1 # Decile 1
ukhls$c_paygu_dv[ukhls$c_paygu_dv == "0.01-173.3333282470703" | ukhls$c_paygu_dv == "173.3333282470703-251.3333282470703" | ukhls$c_paygu_dv == "251.3333282470703-333.3333435058594" | ukhls$c_paygu_dv == "333.3333435058594-411.6666564941406" | ukhls$c_paygu_dv == "411.6666564941406-478.2608642578125"] <- 1 
ukhls$b_paygu_dv[ukhls$b_paygu_dv == "476.6666564941406-540" | ukhls$b_paygu_dv == "540-600" | ukhls$b_paygu_dv == "600-666.6666870117188" | ukhls$b_paygu_dv == "666.6666870117188-725.2611083984375" | ukhls$b_paygu_dv == "725.2611083984375-800"] <- 2 # Decile 2
ukhls$c_paygu_dv[ukhls$c_paygu_dv == "478.2608642578125-536.25" | ukhls$c_paygu_dv == "536.25-600" | ukhls$c_paygu_dv == "600-664" | ukhls$c_paygu_dv == "664-729.5894775390625" | ukhls$c_paygu_dv == "729.5894775390625-800"] <- 2 
ukhls$b_paygu_dv[ukhls$b_paygu_dv == "800-849.3333129882813" | ukhls$b_paygu_dv == "849.3333129882813-902" | ukhls$b_paygu_dv == "902-975" | ukhls$b_paygu_dv == "975-1004" | ukhls$b_paygu_dv == "1004-1078.017211914063"] <- 3 # Decile 3
ukhls$c_paygu_dv[ukhls$c_paygu_dv == "800-860" | ukhls$c_paygu_dv == "860-910" | ukhls$c_paygu_dv == "910-979.3333129882813" | ukhls$c_paygu_dv == "979.3333129882813-1023.647399902344" | ukhls$c_paygu_dv == "1023.647399902344-1083.333374023438"] <- 3 
ukhls$b_paygu_dv[ukhls$b_paygu_dv == "1078.017211914063-1109.333374023438" | ukhls$b_paygu_dv == "1109.333374023438-1191.666625976563" | ukhls$b_paygu_dv == "1191.666625976563-1213.333374023438" | ukhls$b_paygu_dv == "1213.333374023438-1283" | ukhls$b_paygu_dv == "1283-1300.242065429688"] <- 4 # Decile 4
ukhls$c_paygu_dv[ukhls$c_paygu_dv == "1083.333374023438-1133.333374023438" | ukhls$c_paygu_dv == "1133.333374023438-1200" | ukhls$c_paygu_dv == "1200-1250" | ukhls$c_paygu_dv == "1250-1300" | ukhls$c_paygu_dv == "1300-1354.166625976563"] <- 4 
ukhls$b_paygu_dv[ukhls$b_paygu_dv == "1300.242065429688-1375" | ukhls$b_paygu_dv == "1375-1416.666625976563" | ukhls$b_paygu_dv == "1416.666625976563-1500" | ukhls$b_paygu_dv == "1500-1516.666625976563" | ukhls$b_paygu_dv == "1516.666625976563-1600"] <- 5 # Decile 5
ukhls$c_paygu_dv[ukhls$c_paygu_dv == "1354.166625976563-1400" | ukhls$c_paygu_dv == "1400-1473.6708984375" | ukhls$c_paygu_dv == "1473.6708984375-1500" | ukhls$c_paygu_dv == "1500-1570.833374023438" | ukhls$c_paygu_dv == "1570.833374023438-1615"] <- 5 
ukhls$b_paygu_dv[ukhls$b_paygu_dv == "1600-1650" | ukhls$b_paygu_dv == "1650-1708.333374023438" | ukhls$b_paygu_dv == "1708.333374023438-1780.285522460938" | ukhls$b_paygu_dv == "1780.285522460938-1833.333374023438" | ukhls$b_paygu_dv == "1833.333374023438-1900"] <- 6 # Decile 6
ukhls$c_paygu_dv[ukhls$c_paygu_dv == "1615-1700" | ukhls$c_paygu_dv == "1700-1750" | ukhls$c_paygu_dv == "1750-1800" | ukhls$c_paygu_dv == "1800-1900" | ukhls$c_paygu_dv == "1900-1969.270874023438"] <- 6 
ukhls$b_paygu_dv[ukhls$b_paygu_dv == "1900-1980" | ukhls$b_paygu_dv == "1980-2000" | ukhls$b_paygu_dv == "2000-2100" | ukhls$b_paygu_dv == "2100-2166.666748046875" | ukhls$b_paygu_dv == "2166.666748046875-2250"] <- 7 # Decile 7
ukhls$c_paygu_dv[ukhls$c_paygu_dv == "1969.270874023438-2009.061767578125" | ukhls$c_paygu_dv == "2009.061767578125-2097" | ukhls$c_paygu_dv == "2097-2166.666748046875" | ukhls$c_paygu_dv == "2166.666748046875-2231.833251953125" | ukhls$c_paygu_dv == "2231.833251953125-2333.333251953125"] <- 7
ukhls$b_paygu_dv[ukhls$b_paygu_dv == "2250-2341.7294921875" | ukhls$b_paygu_dv == "2341.7294921875-2458.333251953125" | ukhls$b_paygu_dv == "2458.333251953125-2560" | ukhls$b_paygu_dv == "2560-2666.666748046875" | ukhls$b_paygu_dv == "2666.666748046875-2773.828125"] <- 8 # Decile 8
ukhls$c_paygu_dv[ukhls$c_paygu_dv == "2333.333251953125-2400" | ukhls$c_paygu_dv == "2400-2500" | ukhls$c_paygu_dv == "2500-2583.513671875" | ukhls$c_paygu_dv == "2583.513671875-2666.666748046875" | ukhls$c_paygu_dv == "2666.666748046875-2800"] <- 8
ukhls$b_paygu_dv[ukhls$b_paygu_dv == "2773.828125-2875" | ukhls$b_paygu_dv == "2875-3000" | ukhls$b_paygu_dv == "3000-3166.666748046875" | ukhls$b_paygu_dv == "3166.666748046875-3350" | ukhls$b_paygu_dv == "3350-3544"] <- 9 # Decile 9
ukhls$c_paygu_dv[ukhls$c_paygu_dv == "2800-2916.666748046875" | ukhls$c_paygu_dv == "2916.666748046875-3033.333251953125" | ukhls$c_paygu_dv == "3033.333251953125-3208.333251953125" | ukhls$c_paygu_dv == "3208.333251953125-3400" | ukhls$c_paygu_dv == "3400-3600"] <- 9
ukhls$b_paygu_dv[ukhls$b_paygu_dv == "3544-3800" | ukhls$b_paygu_dv == "3800-4200" | ukhls$b_paygu_dv == "4200-4800" | ukhls$b_paygu_dv == "4800-5850" | ukhls$b_paygu_dv == "5850 or more"] <- 10 # Decile 10
ukhls$c_paygu_dv[ukhls$c_paygu_dv == "3600-3833.333251953125" | ukhls$c_paygu_dv == "3833.333251953125-4167" | ukhls$c_paygu_dv == "4167-4833.33349609375" | ukhls$c_paygu_dv == "4833.33349609375-5915.91650390625" | ukhls$c_paygu_dv == "5915.91650390625 or more"] <- 10
ukhls$b_paygu_dv[ukhls$b_paygu_dv == ""] <- 0 # Missing
ukhls$c_paygu_dv[ukhls$c_paygu_dv == ""] <- 0
ukhls$b_paygu_dv <- as.numeric(ukhls$b_paygu_dv) # Convert to numeric
ukhls$c_paygu_dv <- as.numeric(ukhls$c_paygu_dv)

ukhls$gross_pay <- 0 # Create new variable for analysis at baseline
ukhls$gross_pay[ukhls$wave == 2] <- ukhls$b_paygu_dv[ukhls$wave == 2]
ukhls$gross_pay[ukhls$wave == 3] <- ukhls$c_paygu_dv[ukhls$wave == 3]

# Occupation #

ses_occ_bc <- c("jbstat", "jbperm", "jbsoc00", "manager", "jbhrs", "jbot", "jbsat", "jb2")
ses_occ <- c("jbstat", "jbperm", "jbsoc00", "manager", "jbhrs", "jbot", "jbsat", "jb2", "b_jbsec",
             "b_wkends", "b_wkaut1", "b_wkaut2", "b_wkaut3", "b_wkaut4", "b_wkaut5", "b_depenth1",
             "b_depenth2", "b_depenth3", "b_depenth4", "b_depenth5", "b_depenth6")

# Current economic activity
ukhls$b_jbstat[ukhls$b_jbstat == ""] <- 0 # Missing
ukhls$c_jbstat[ukhls$c_jbstat == ""] <- 0
ukhls$b_jbstat[ukhls$b_jbstat == "paid employment(ft/pt)"] <- 1 # Employed
ukhls$c_jbstat[ukhls$c_jbstat == "in paid employment (full or part-time)"] <- 1 
ukhls$b_jbstat[ukhls$b_jbstat == "self employed"] <- 2 # Self-employed
ukhls$c_jbstat[ukhls$c_jbstat == "self employed"] <- 2 
ukhls$b_jbstat[ukhls$b_jbstat == "unemployed"] <- 3 # Unemployed
ukhls$c_jbstat[ukhls$c_jbstat == "unemployed"] <- 3 
ukhls$b_jbstat[ukhls$b_jbstat == "retired"] <- 4 # Retired
ukhls$c_jbstat[ukhls$c_jbstat == "retired"] <- 4 
ukhls$b_jbstat[ukhls$b_jbstat == "family care or home"] <- 5 # Family care/home
ukhls$c_jbstat[ukhls$c_jbstat == "looking after family or home"] <- 5
ukhls$b_jbstat[ukhls$b_jbstat == "lt sick or disabled"] <- 6 # Long term sick
ukhls$c_jbstat[ukhls$c_jbstat == "long term sick or disabled"] <- 6
ukhls$b_jbstat[ukhls$b_jbstat == "doing something else" | ukhls$b_jbstat == "full-time student" | ukhls$b_jbstat == "on maternity leave"] <- 7 # Other
ukhls$c_jbstat[ukhls$c_jbstat == "doing something else" | ukhls$c_jbstat == "full-time student" | ukhls$c_jbstat == "on maternity leave"] <- 7
ukhls$b_jbstat <- as.numeric(ukhls$b_jbstat) # Convert to numeric
ukhls$c_jbstat <- as.numeric(ukhls$c_jbstat)

ukhls$jbstat <- 0 # Create new variable for analysis at baseline
ukhls$jbstat[ukhls$wave == 2] <- ukhls$b_jbstat[ukhls$wave == 2]
ukhls$jbstat[ukhls$wave == 3] <- ukhls$c_jbstat[ukhls$wave == 3]

#  Is job permanent?
ukhls$b_jbterm1[ukhls$b_jbterm1 == ""] <- 0 # Missing
ukhls$c_jbterm1[ukhls$c_jbterm1 == ""] <- 0
ukhls$b_jbterm1[ukhls$b_jbterm1 == "a permanent job"] <- 1 # Yes
ukhls$c_jbterm1[ukhls$c_jbterm1 == "a permanent job"] <- 1 
ukhls$b_jbterm1[ukhls$b_jbterm1 == "not permanent job"] <- 2 # No
ukhls$c_jbterm1[ukhls$c_jbterm1 == "or is there some way that it is not permanent?"] <- 2 
ukhls$b_jbterm1 <- as.numeric(ukhls$b_jbterm1) # Convert to numeric
ukhls$c_jbterm1 <- as.numeric(ukhls$c_jbterm1)

ukhls$jbperm <- 0 # Create new variable for analysis at baseline
ukhls$jbperm[ukhls$wave == 2] <- ukhls$b_jbterm1[ukhls$wave == 2]
ukhls$jbperm[ukhls$wave == 3] <- ukhls$c_jbterm1[ukhls$wave == 3]

# Job type
# Too many categories (80 in b, 81 in c plus missing as category in both) 
# so will convert each character to unique number

# Check same occupations are present in each wave
# waveb <- as.data.frame(table(ukhls$b_jbsoc00_cc))
# waveb <- waveb + c(NA, NA) # Since one less add in blank row
# wavec <- as.data.frame(table(ukhls$c_jbsoc00_cc))
# combined <- merge(waveb, wavec, by = "Var1", all = TRUE)
# combined
# The missing level is 'Personal services occupations nec'

ukhls$b_jbsoc00_nb <- as.factor(ukhls$b_jbsoc00_cc)
levels(ukhls$b_jbsoc00_nb) <- 1:length(levels(ukhls$b_jbsoc00_nb))
ukhls$b_jbsoc00_nb <- as.numeric(ukhls$b_jbsoc00_nb)
ukhls$b_jbsoc00_nb <- ukhls$b_jbsoc00_nb - 1 # So that missing is 0

# Puts to bottom of levels so given value of 82 since missing from above wave
ukhls$c_jbsoc00_cc[ukhls$c_jbsoc00_cc == "Personal services occupations nec"] <-  "ZPersonal services occupations nec" 
ukhls$c_jbsoc00_nb <- as.factor(ukhls$c_jbsoc00_cc)
levels(ukhls$c_jbsoc00_nb) <- 1:length(levels(ukhls$c_jbsoc00_nb))
ukhls$c_jbsoc00_nb <- as.numeric(ukhls$c_jbsoc00_nb)
ukhls$c_jbsoc00_nb <- ukhls$c_jbsoc00_nb - 1 # So that missing is 0

ukhls$jbsoc00 <- 0 # Create new variable for analysis at baseline
ukhls$jbsoc00[ukhls$wave == 2] <- ukhls$b_jbsoc00_nb[ukhls$wave == 2]
ukhls$jbsoc00[ukhls$wave == 3] <- ukhls$c_jbsoc00_nb[ukhls$wave == 3]

# Have managerial responsibilites 
ukhls$b_jbmngrchk[ukhls$b_jbmngrchk == ""] <- 0 # Missing
ukhls$c_jbmngrchk[ukhls$c_jbmngrchk == ""] <- 0
ukhls$b_jbmngrchk[ukhls$b_jbmngrchk == "yes"] <- 1 # Yes
ukhls$c_jbmngrchk[ukhls$c_jbmngrchk == "yes"] <- 1 
ukhls$b_jbmngrchk[ukhls$b_jbmngrchk == "no"] <- 2 # No
ukhls$c_jbmngrchk[ukhls$c_jbmngrchk == "no"] <- 2 
ukhls$b_jbmngrchk <- as.numeric(ukhls$b_jbmngrchk) # Convert to numeric
ukhls$c_jbmngrchk <- as.numeric(ukhls$c_jbmngrchk)

ukhls$manager <- 0 # Create new variable for analysis at baseline
ukhls$manager[ukhls$wave == 2] <- ukhls$b_jbmngrchk[ukhls$wave == 2]
ukhls$manager[ukhls$wave == 3] <- ukhls$c_jbmngrchk[ukhls$wave == 3]

# Number of hours worked 
ukhls$b_jbhrs[ukhls$b_jbhrs == ""] <- 0 # Missing
ukhls$c_jbhrs[ukhls$c_jbhrs == ""] <- 0
ukhls$b_jbhrs[ukhls$b_jbhrs == "0-6" | ukhls$b_jbhrs == "6-9" | ukhls$b_jbhrs == "9-12" | ukhls$b_jbhrs == "12-15" | ukhls$b_jbhrs == "15-16" | ukhls$b_jbhrs == "16-18" | ukhls$b_jbhrs == "18-20"] <- 1 # <20
ukhls$c_jbhrs[ukhls$c_jbhrs == "0-6" | ukhls$c_jbhrs == "6-10" | ukhls$c_jbhrs == "10-12" | ukhls$c_jbhrs == "12-15" | ukhls$c_jbhrs == "15-16" | ukhls$c_jbhrs == "16-18" | ukhls$c_jbhrs == "18-20"] <- 1
ukhls$b_jbhrs[ukhls$b_jbhrs == "20-22" | ukhls$b_jbhrs == "22-23" | ukhls$b_jbhrs == "23-24.5" | ukhls$b_jbhrs == "24.5-25" ] <- 2 # 20-25
ukhls$c_jbhrs[ukhls$c_jbhrs == "20-22" | ukhls$c_jbhrs == "22-23" | ukhls$c_jbhrs == "23-24" | ukhls$c_jbhrs == "24-25"] <- 2
ukhls$b_jbhrs[ukhls$b_jbhrs == "25-27.5" | ukhls$b_jbhrs == "27.5-30"] <- 3 # 25-30
ukhls$c_jbhrs[ukhls$c_jbhrs == "25-27" | ukhls$c_jbhrs == "27-30"] <- 3
ukhls$b_jbhrs[ukhls$b_jbhrs == "30-31" | ukhls$b_jbhrs == "31-32.5" | ukhls$b_jbhrs == "32.5-35"] <- 4 # 30-35
ukhls$c_jbhrs[ukhls$c_jbhrs == "30-32" | ukhls$c_jbhrs == "32-35"] <- 4
ukhls$b_jbhrs[ukhls$b_jbhrs == "35-36" | ukhls$b_jbhrs == "36-37"] <- 5 # 35-37
ukhls$c_jbhrs[ukhls$c_jbhrs == "35-36" | ukhls$c_jbhrs == "36-37"] <- 5
ukhls$b_jbhrs[ukhls$b_jbhrs == "37-37.5" | ukhls$b_jbhrs == "37.5-38"] <- 6 # 37-38
ukhls$c_jbhrs[ukhls$c_jbhrs == "37-37.5" | ukhls$c_jbhrs == "37.5-38"] <- 6
ukhls$b_jbhrs[ukhls$b_jbhrs == "38-39" | ukhls$b_jbhrs == "39-40"] <- 7 # 38-40
ukhls$c_jbhrs[ukhls$c_jbhrs == "38-39" | ukhls$c_jbhrs == "39-40"] <- 7
ukhls$b_jbhrs[ukhls$b_jbhrs == "40-42" | ukhls$b_jbhrs == "42-45"] <- 8 # 40-45
ukhls$c_jbhrs[ukhls$c_jbhrs == "40-42" | ukhls$c_jbhrs == "42-45"] <- 8
ukhls$b_jbhrs[ukhls$b_jbhrs == "45-48" | ukhls$b_jbhrs == "48-50" | ukhls$b_jbhrs == "50 or more"] <- 9 # 45+
ukhls$c_jbhrs[ukhls$c_jbhrs == "45-48" | ukhls$c_jbhrs == "48-50" | ukhls$c_jbhrs == "50 or more"] <- 9
ukhls$b_jbhrs <- as.numeric(ukhls$b_jbhrs) # Convert to numeric
ukhls$c_jbhrs <- as.numeric(ukhls$c_jbhrs)

ukhls$jbhrs <- 0 # Create new variable for analysis at baseline
ukhls$jbhrs[ukhls$wave == 2] <- ukhls$b_jbhrs[ukhls$wave == 2]
ukhls$jbhrs[ukhls$wave == 3] <- ukhls$c_jbhrs[ukhls$wave == 3]

# Number of hours overtime
ukhls$b_jbot[ukhls$b_jbot == ""] <- 0 # Missing
ukhls$c_jbot[ukhls$c_jbot == ""] <- 0
ukhls$b_jbot[ukhls$b_jbot == "0"] <- 1 # 0
ukhls$c_jbot[ukhls$c_jbot == "0"] <- 1
ukhls$b_jbot[ukhls$b_jbot == "0.01-1" | ukhls$b_jbot == "1-2" | ukhls$b_jbot == "2-3" | ukhls$b_jbot == "3-4" | ukhls$b_jbot == "4-5"] <- 2 # 0.01-5
ukhls$c_jbot[ukhls$c_jbot == "0.01-1" | ukhls$c_jbot == "1-2" | ukhls$c_jbot == "2-3" | ukhls$c_jbot == "3-4" | ukhls$c_jbot == "4-5"] <- 2 
ukhls$b_jbot[ukhls$b_jbot == "5-6" | ukhls$b_jbot == "6-8" | ukhls$b_jbot == "8-10"] <- 3 # 5-10
ukhls$c_jbot[ukhls$c_jbot == "5-6" | ukhls$c_jbot == "6-7" | ukhls$c_jbot == "7-8" | ukhls$c_jbot == "8-10"] <- 3
ukhls$b_jbot[ukhls$b_jbot == "10-14" | ukhls$b_jbot == "14-15" | ukhls$b_jbot == "15-20" | ukhls$b_jbot == "20 or more"] <- 4 # 5-10
ukhls$c_jbot[ukhls$c_jbot == "10-12" | ukhls$c_jbot == "12-15" | ukhls$c_jbot == "15-20" | ukhls$c_jbot == "20 or more"] <- 4
ukhls$b_jbot <- as.numeric(ukhls$b_jbot) # Convert to numeric
ukhls$c_jbot <- as.numeric(ukhls$c_jbot)

ukhls$jbot <- 0 # Create new variable for analysis at baseline
ukhls$jbot[ukhls$wave == 2] <- ukhls$b_jbot[ukhls$wave == 2]
ukhls$jbot[ukhls$wave == 3] <- ukhls$c_jbot[ukhls$wave == 3]

# Job satisfaction
ukhls$b_jbsat[ukhls$b_jbsat == ""] <- 0 # Missing
ukhls$c_jbsat[ukhls$c_jbsat == ""] <- 0
ukhls$b_jbsat[ukhls$b_jbsat == "completedly satisfied"] <- 1 # Completely satisfied 
ukhls$c_jbsat[ukhls$c_jbsat == "completedly satisfied"] <- 1
ukhls$b_jbsat[ukhls$b_jbsat == "mostly satisfied"] <- 2 # Mostly satisfied 
ukhls$c_jbsat[ukhls$c_jbsat == "mostly satisfied"] <- 2
ukhls$b_jbsat[ukhls$b_jbsat == "somewhat satisfied"] <- 3 # Somewhat satisfied 
ukhls$c_jbsat[ukhls$c_jbsat == "somewhat satisfied"] <- 3
ukhls$b_jbsat[ukhls$b_jbsat == "neither satisfied or dissatisfied"] <- 4 # Neither 
ukhls$c_jbsat[ukhls$c_jbsat == "neither satisfied or dissatisfied"] <- 4
ukhls$b_jbsat[ukhls$b_jbsat == "somewhat dissatisfied"] <- 5 # Dissatisfied 
ukhls$c_jbsat[ukhls$c_jbsat == "somewhat dissatisfied"] <- 5
ukhls$b_jbsat[ukhls$b_jbsat == "mostly dissatisfied" | ukhls$b_jbsat == "completely dissatisfied"] <- 6 # Completely/mostly dissatisfied 
ukhls$c_jbsat[ukhls$c_jbsat == "mostly dissatisfied" | ukhls$c_jbsat == "completely dissatisfied"] <- 6
ukhls$b_jbsat <- as.numeric(ukhls$b_jbsat) # Convert to numeric
ukhls$c_jbsat <- as.numeric(ukhls$c_jbsat)

ukhls$jbsat <- 0 # Create new variable for analysis at baseline
ukhls$jbsat[ukhls$wave == 2] <- ukhls$b_jbsat[ukhls$wave == 2]
ukhls$jbsat[ukhls$wave == 3] <- ukhls$c_jbsat[ukhls$wave == 3]

# Has a second job
ukhls$b_j2has[ukhls$b_j2has == ""] <- 0 # Missing
ukhls$c_j2has[ukhls$c_j2has == ""] <- 0
ukhls$b_j2has[ukhls$b_j2has == "yes"] <- 1 # Yes 
ukhls$c_j2has[ukhls$c_j2has == "yes"] <- 1
ukhls$b_j2has[ukhls$b_j2has == "no"] <- 2 # No
ukhls$c_j2has[ukhls$c_j2has == "no"] <- 2
ukhls$b_j2has <- as.numeric(ukhls$b_j2has) # Convert to numeric
ukhls$c_j2has <- as.numeric(ukhls$c_j2has)

ukhls$jb2 <- 0 # Create new variable for analysis at baseline
ukhls$jb2[ukhls$wave == 2] <- ukhls$b_j2has[ukhls$wave == 2]
ukhls$jb2[ukhls$wave == 3] <- ukhls$c_j2has[ukhls$wave == 3]

# Job security
ukhls$b_jbsec[ukhls$b_jbsec == ""] <- 0 # Missing
ukhls$b_jbsec[ukhls$b_jbsec == "very likely"] <- 1 # Very likely
ukhls$b_jbsec[ukhls$b_jbsec == "likely"] <- 2 # Likely
ukhls$b_jbsec[ukhls$b_jbsec == "unlikely"] <- 3 # Unlikely
ukhls$b_jbsec[ukhls$b_jbsec == "very unlikely"] <- 4 # Very unlikely
ukhls$b_jbsec <- as.numeric(ukhls$b_jbsec) # Convert to numeric

# Work weekends
ukhls$b_wkends[ukhls$b_wkends == ""] <- 0 # Missing
ukhls$b_wkends[ukhls$b_wkends == "no weekend working"] <- 1 # No
ukhls$b_wkends[ukhls$b_wkends == "yes - some weekends"] <- 2 # Some weekends
ukhls$b_wkends[ukhls$b_wkends == "yes - most/every weekend"] <- 3 # Most/all
ukhls$b_wkends <- as.numeric(ukhls$b_wkends) # Convert to numeric

# Work automony
# Automony over job tasks
ukhls$b_wkaut1[ukhls$b_wkaut1 == ""] <- 0 # Missing
ukhls$b_wkaut1[ukhls$b_wkaut1 == "a lot"] <- 1 # A lot
ukhls$b_wkaut1[ukhls$b_wkaut1 == "some"] <- 2 # Some
ukhls$b_wkaut1[ukhls$b_wkaut1 == "a little"] <- 3 # A little
ukhls$b_wkaut1[ukhls$b_wkaut1 == "none"] <- 4 # None
ukhls$b_wkaut1 <- as.numeric(ukhls$b_wkaut1) # Convert to numeric

# Automony over work pace
ukhls$b_wkaut2[ukhls$b_wkaut2 == ""] <- 0 # Missing
ukhls$b_wkaut2[ukhls$b_wkaut2 == "a lot"] <- 1 # A lot
ukhls$b_wkaut2[ukhls$b_wkaut2 == "some"] <- 2 # Some
ukhls$b_wkaut2[ukhls$b_wkaut2 == "a little"] <- 3 # A little
ukhls$b_wkaut2[ukhls$b_wkaut2 == "none"] <- 4 # None
ukhls$b_wkaut2 <- as.numeric(ukhls$b_wkaut2) # Convert to numeric

# Automony over work manner
ukhls$b_wkaut3[ukhls$b_wkaut3 == ""] <- 0 # Missing
ukhls$b_wkaut3[ukhls$b_wkaut3 == "a lot"] <- 1 # A lot
ukhls$b_wkaut3[ukhls$b_wkaut3 == "some"] <- 2 # Some
ukhls$b_wkaut3[ukhls$b_wkaut3 == "a little"] <- 3 # A little
ukhls$b_wkaut3[ukhls$b_wkaut3 == "none"] <- 4 # None
ukhls$b_wkaut3 <- as.numeric(ukhls$b_wkaut3) # Convert to numeric

# Automony over task order
ukhls$b_wkaut4[ukhls$b_wkaut4 == ""] <- 0 # Missing
ukhls$b_wkaut4[ukhls$b_wkaut4 == "a lot"] <- 1 # A lot
ukhls$b_wkaut4[ukhls$b_wkaut4 == "some"] <- 2 # Some
ukhls$b_wkaut4[ukhls$b_wkaut4 == "a little"] <- 3 # A little
ukhls$b_wkaut4[ukhls$b_wkaut4 == "none"] <- 4 # None
ukhls$b_wkaut4 <- as.numeric(ukhls$b_wkaut4) # Convert to numeric

# Automony over work hours
ukhls$b_wkaut5[ukhls$b_wkaut5 == ""] <- 0 # Missing
ukhls$b_wkaut5[ukhls$b_wkaut5 == "a lot"] <- 1 # A lot
ukhls$b_wkaut5[ukhls$b_wkaut5 == "some"] <- 2 # Some
ukhls$b_wkaut5[ukhls$b_wkaut5 == "a little"] <- 3 # A little
ukhls$b_wkaut5[ukhls$b_wkaut5 == "none"] <- 4 # None
ukhls$b_wkaut5 <- as.numeric(ukhls$b_wkaut5) # Convert to numeric

# Feelings about work
# Feels tense about job
ukhls$b_depenth1[ukhls$b_depenth1 == ""] <- 0 # Missing
ukhls$b_depenth1[ukhls$b_depenth1 == "never"] <- 1 # Never
ukhls$b_depenth1[ukhls$b_depenth1 == "occasionally"] <- 2 # Occasionally
ukhls$b_depenth1[ukhls$b_depenth1 == "some of the time"] <- 3 # Some of the time
ukhls$b_depenth1[ukhls$b_depenth1 == "most of the time"] <- 4 # Most of the time
ukhls$b_depenth1[ukhls$b_depenth1 == "all of the time"] <- 5 # All of the time
ukhls$b_depenth1 <- as.numeric(ukhls$b_depenth1) # Convert to numeric

# Feels uneasy about job
ukhls$b_depenth2[ukhls$b_depenth2 == ""] <- 0 # Missing
ukhls$b_depenth2[ukhls$b_depenth2 == "never"] <- 1 # Never
ukhls$b_depenth2[ukhls$b_depenth2 == "occasionally"] <- 2 # Occasionally
ukhls$b_depenth2[ukhls$b_depenth2 == "some of the time"] <- 3 # Some of the time
ukhls$b_depenth2[ukhls$b_depenth2 == "most of the time"] <- 4 # Most of the time
ukhls$b_depenth2[ukhls$b_depenth2 == "all of the time"] <- 5 # All of the time
ukhls$b_depenth2 <- as.numeric(ukhls$b_depenth2) # Convert to numeric

# Feels worried about job
ukhls$b_depenth3[ukhls$b_depenth3 == ""] <- 0 # Missing
ukhls$b_depenth3[ukhls$b_depenth3 == "never"] <- 1 # Never
ukhls$b_depenth3[ukhls$b_depenth3 == "occasionally"] <- 2 # Occasionally
ukhls$b_depenth3[ukhls$b_depenth3 == "some of the time"] <- 3 # Some of the time
ukhls$b_depenth3[ukhls$b_depenth3 == "most of the time"] <- 4 # Most of the time
ukhls$b_depenth3[ukhls$b_depenth3 == "all of the time"] <- 5 # All of the time
ukhls$b_depenth3 <- as.numeric(ukhls$b_depenth3) # Convert to numeric

# Feels depressed about job
ukhls$b_depenth4[ukhls$b_depenth4 == ""] <- 0 # Missing
ukhls$b_depenth4[ukhls$b_depenth4 == "never"] <- 1 # Never
ukhls$b_depenth4[ukhls$b_depenth4 == "occasionally"] <- 2 # Occasionally
ukhls$b_depenth4[ukhls$b_depenth4 == "some of the time"] <- 3 # Some of the time
ukhls$b_depenth4[ukhls$b_depenth4 == "most of the time"] <- 4 # Most of the time
ukhls$b_depenth4[ukhls$b_depenth4 == "all of the time"] <- 5 # All of the time
ukhls$b_depenth4 <- as.numeric(ukhls$b_depenth4) # Convert to numeric

# Feels gloomy about job
ukhls$b_depenth5[ukhls$b_depenth5 == ""] <- 0 # Missing
ukhls$b_depenth5[ukhls$b_depenth5 == "never"] <- 1 # Never
ukhls$b_depenth5[ukhls$b_depenth5 == "occasionally"] <- 2 # Occasionally
ukhls$b_depenth5[ukhls$b_depenth5 == "some of the time"] <- 3 # Some of the time
ukhls$b_depenth5[ukhls$b_depenth5 == "most of the time"] <- 4 # Most of the time
ukhls$b_depenth5[ukhls$b_depenth5 == "all of the time"] <- 5 # All of the time
ukhls$b_depenth5 <- as.numeric(ukhls$b_depenth5) # Convert to numeric

# Feels gloomy about job
ukhls$b_depenth6[ukhls$b_depenth6 == ""] <- 0 # Missing
ukhls$b_depenth6[ukhls$b_depenth6 == "never"] <- 1 # Never
ukhls$b_depenth6[ukhls$b_depenth6 == "occasionally"] <- 2 # Occasionally
ukhls$b_depenth6[ukhls$b_depenth6 == "some of the time"] <- 3 # Some of the time
ukhls$b_depenth6[ukhls$b_depenth6 == "most of the time"] <- 4 # Most of the time
ukhls$b_depenth6[ukhls$b_depenth6 == "all of the time"] <- 5 # All of the time
ukhls$b_depenth6 <- as.numeric(ukhls$b_depenth6) # Convert to numeric

# Parental #

ses_par <- c("paedqf", "maedqf", "paju", "maju", "pasoc00_nb", "masoc00_nb")

# Father's education
ukhls$paedqf[ukhls$paedqf == ""] <- 0
ukhls$paedqf[ukhls$paedqf == "don't know" | ukhls$paedqf == "other"] <- 1 # Other or dont't know
ukhls$paedqf[ukhls$paedqf == "he did not go to school at all" | ukhls$paedqf == "he left school with no qualifications or certificates"] <- 2 # No quals
ukhls$paedqf[ukhls$paedqf == "he left school with some qualifications or certificates"] <- 3 # Some quals
ukhls$paedqf[ukhls$paedqf == "he gained post school quals or certs (e.g. city & guilds)"] <- 4 # Post-school quals
ukhls$paedqf[ukhls$paedqf == "he gained a university degree or higher degree"] <- 5 # Degree or higher
ukhls$paedqf <- as.numeric(ukhls$paedqf)

# Mother's education
ukhls$maedqf[ukhls$maedqf == ""] <- 0
ukhls$maedqf[ukhls$maedqf == "don't know" | ukhls$maedqf == "other"] <- 1 # Other or dont't know
ukhls$maedqf[ukhls$maedqf == "she did not go to school at all" | ukhls$maedqf == "she left school with no qualifications or certificates"] <- 2 # No quals
ukhls$maedqf[ukhls$maedqf == "she left school with some qualifications or certificates"] <- 3 # Some quals
ukhls$maedqf[ukhls$maedqf == "she gained post school quals or certs (e.g. city & guilds)"] <- 4 # Post-school quals
ukhls$maedqf[ukhls$maedqf == "she gained a university degree or higher degree"] <- 5 # Degree or higher
ukhls$maedqf <- as.numeric(ukhls$maedqf)

# Father employed when 14
ukhls$paju[ukhls$paju == ""] <- 0
ukhls$paju[ukhls$paju == "father not living with respondent so don't know"] <- 1 # Dont't know
ukhls$paju[ukhls$paju == "father deceased"] <- 2 # Deceased
ukhls$paju[ukhls$paju == "father not working"] <- 3 # Not working
ukhls$paju[ukhls$paju == "father working"] <- 4 # Working
ukhls$paju <- as.numeric(ukhls$paju)

# Mother employed when 14
ukhls$maju[ukhls$maju == ""] <- 0
ukhls$maju[ukhls$maju == "mother not living with respondent so don't know"] <- 1 # Dont't know
ukhls$maju[ukhls$maju == "mother deceased"] <- 2 # Deceased
ukhls$maju[ukhls$maju == "mother not working"] <- 3 # Not working
ukhls$maju[ukhls$maju == "mother working"] <- 4 # Working
ukhls$maju <- as.numeric(ukhls$maju)

# Father's occupation when 14
ukhls$pasoc00_nb <- as.factor(ukhls$pasoc00_smg)
levels(ukhls$pasoc00_nb) <- 1:length(levels(ukhls$pasoc00_nb))
ukhls$pasoc00_nb <- as.numeric(ukhls$pasoc00_nb)
ukhls$pasoc00_nb <- ukhls$pasoc00_nb - 1 # So that missing is 0

# Mother's occupation when 14
ukhls$masoc00_nb <- as.factor(ukhls$masoc00_smg)
levels(ukhls$masoc00_nb) <- 1:length(levels(ukhls$masoc00_nb))
ukhls$masoc00_nb <- as.numeric(ukhls$masoc00_nb)
ukhls$masoc00_nb <- ukhls$masoc00_nb - 1 # So that missing is 0

# Relative poverty #
 
ses_relpov_bc <- c("sky_cable", "tumble", "dishw", "pc", "cdplayer", "pcnet", "ncars", "caruse", "mobuse")
ses_relpov <- c("b_matdepa", "b_matdepb", "b_matdepd", "b_matdepe", "b_matdepf", "b_matdepg", "b_matdeph",
                "sky_cable", "tumble", "dishw", "pc", "cdplayer", "pcnet", "ncars", "caruse", "mobuse")

# Material deprivation
# Holiday
ukhls$b_matdepa[ukhls$b_matdepa == ""] <- 0 # Missing
ukhls$b_matdepa[ukhls$b_matdepa == "does not apply"] <- 1 # Does not apply
ukhls$b_matdepa[ukhls$b_matdepa == "don't need it now"] <- 2 # I/We do not want or need it currently
ukhls$b_matdepa[ukhls$b_matdepa == "can't afford it"] <- 3 # I/We would like to have but cannot afford it currently
ukhls$b_matdepa[ukhls$b_matdepa == "i/we have this"] <- 4 # I/We have this
ukhls$b_matdepa <- as.numeric(ukhls$b_matdepa) # Convert to numeric

# Social meal/drink
ukhls$b_matdepb[ukhls$b_matdepb == ""] <- 0 # Missing
ukhls$b_matdepb[ukhls$b_matdepb == "does not apply"] <- 1 # Does not apply
ukhls$b_matdepb[ukhls$b_matdepb == "don't need it now"] <- 2 # I/We do not want or need it currently
ukhls$b_matdepb[ukhls$b_matdepb == "can't afford it"] <- 3 # I/We would like to have but cannot afford it currently
ukhls$b_matdepb[ukhls$b_matdepb == "i/we have this"] <- 4 # I/We have this
ukhls$b_matdepb <- as.numeric(ukhls$b_matdepb) # Convert to numeric

# House
ukhls$b_matdepd[ukhls$b_matdepd == ""] <- 0 # Missing
ukhls$b_matdepd[ukhls$b_matdepd == "does not apply"] <- 1 # Does not apply
ukhls$b_matdepd[ukhls$b_matdepd == "don't need it now"] <- 2 # I/We do not want or need it currently
ukhls$b_matdepd[ukhls$b_matdepd == "can't afford it"] <- 3 # I/We would like to have but cannot afford it currently
ukhls$b_matdepd[ukhls$b_matdepd == "i/we have this"] <- 4 # I/We have this
ukhls$b_matdepd <- as.numeric(ukhls$b_matdepd) # Convert to numeric

# Contents insurance
ukhls$b_matdepe[ukhls$b_matdepe == ""] <- 0 # Missing
ukhls$b_matdepe[ukhls$b_matdepe == "does not apply"] <- 1 # Does not apply
ukhls$b_matdepe[ukhls$b_matdepe == "don't need it now"] <- 2 # I/We do not want or need it currently
ukhls$b_matdepe[ukhls$b_matdepe == "can't afford it"] <- 3 # I/We would like to have but cannot afford it currently
ukhls$b_matdepe[ukhls$b_matdepe == "i/we have this"] <- 4 # I/We have this
ukhls$b_matdepe <- as.numeric(ukhls$b_matdepe) # Convert to numeric

# Savings
ukhls$b_matdepf[ukhls$b_matdepf == ""] <- 0 # Missing
ukhls$b_matdepf[ukhls$b_matdepf == "does not apply"] <- 1 # Does not apply
ukhls$b_matdepf[ukhls$b_matdepf == "don't need it now"] <- 2 # I/We do not want or need it currently
ukhls$b_matdepf[ukhls$b_matdepf == "can't afford it"] <- 3 # I/We would like to have but cannot afford it currently
ukhls$b_matdepf[ukhls$b_matdepf == "i/we have this"] <- 4 # I/We have this
ukhls$b_matdepf <- as.numeric(ukhls$b_matdepf) # Convert to numeric

# Furniture
ukhls$b_matdepg[ukhls$b_matdepg == ""] <- 0 # Missing
ukhls$b_matdepg[ukhls$b_matdepg == "does not apply"] <- 1 # Does not apply
ukhls$b_matdepg[ukhls$b_matdepg == "don't need it now"] <- 2 # I/We do not want or need it currently
ukhls$b_matdepg[ukhls$b_matdepg == "can't afford it"] <- 3 # I/We would like to have but cannot afford it currently
ukhls$b_matdepg[ukhls$b_matdepg == "i/we have this"] <- 4 # I/We have this
ukhls$b_matdepg <- as.numeric(ukhls$b_matdepg) # Convert to numeric

# Electrical goods
ukhls$b_matdeph[ukhls$b_matdeph == ""] <- 0 # Missing
ukhls$b_matdeph[ukhls$b_matdeph == "does not apply"] <- 1 # Does not apply
ukhls$b_matdeph[ukhls$b_matdeph == "don't need it now"] <- 2 # I/We do not want or need it currently
ukhls$b_matdeph[ukhls$b_matdeph == "can't afford it"] <- 3 # I/We would like to have but cannot afford it currently
ukhls$b_matdeph[ukhls$b_matdeph == "i/we have this"] <- 4 # I/We have this
ukhls$b_matdeph <- as.numeric(ukhls$b_matdeph) # Convert to numeric

# Consumer Durables in accomodation
# Satellite dish/sky TV or cable TV
ukhls$sky_cable <- 0 # Missing
ukhls$sky_cable[ukhls$wave == 2 & (ukhls$b_cduse3 == "not mentioned" & ukhls$b_cduse4 == "not mentioned")] <- 2 # Does not have
ukhls$sky_cable[ukhls$wave == 2 & (ukhls$b_cduse3 == "mentioned" | ukhls$b_cduse4 == "mentioned")] <- 1 # Has
ukhls$sky_cable[ukhls$wave == 3 & (ukhls$c_cduse3 == "not mentioned" & ukhls$c_cduse4 == "not mentioned")] <- 2 # Does not have
ukhls$sky_cable[ukhls$wave == 3 & (ukhls$c_cduse3 == "mentioned" | ukhls$c_cduse4 == "mentioned")] <- 1 # Has

# Tumble drier
ukhls$tumble <- 0 # Missing
ukhls$tumble[ukhls$wave == 2 & ukhls$b_cduse7 == "not mentioned"] <- 2 # Does not have
ukhls$tumble[ukhls$wave == 2 & ukhls$b_cduse7 == "mentioned"] <- 1 # Has
ukhls$tumble[ukhls$wave == 3 & ukhls$c_cduse7 == "not mentioned"] <- 2 # Does not have
ukhls$tumble[ukhls$wave == 3 & ukhls$c_cduse7 == "mentioned"] <- 1 # Has

# Dish washer
ukhls$dishw <- 0 # Missing
ukhls$dishw[ukhls$wave == 2 & ukhls$b_cduse8 == "not mentioned"] <- 2 # Does not have
ukhls$dishw[ukhls$wave == 2 & ukhls$b_cduse8 == "mentioned"] <- 1 # Has
ukhls$dishw[ukhls$wave == 3 & ukhls$c_cduse8 == "not mentioned"] <- 2 # Does not have
ukhls$dishw[ukhls$wave == 3 & ukhls$c_cduse8 == "mentioned"] <- 1 # Has

# PC
ukhls$pc <- 0 # Missing
ukhls$pc[ukhls$wave == 2 & ukhls$b_cduse10 == "not mentioned"] <- 2 # Does not have
ukhls$pc[ukhls$wave == 2 & ukhls$b_cduse10 == "mentioned"] <- 1 # Has
ukhls$pc[ukhls$wave == 3 & ukhls$c_cduse10 == "not mentioned"] <- 2 # Does not have
ukhls$pc[ukhls$wave == 3 & ukhls$c_cduse10 == "mentioned"] <- 1 # Has

# CD player
ukhls$cdplayer <- 0 # Missing
ukhls$cdplayer[ukhls$wave == 2 & ukhls$b_cduse11 == "not mentioned"] <- 2 # Does not have
ukhls$cdplayer[ukhls$wave == 2 & ukhls$b_cduse11 == "mentioned"] <- 1 # Has
ukhls$cdplayer[ukhls$wave == 3 & ukhls$c_cduse11 == "not mentioned"] <- 2 # Does not have
ukhls$cdplayer[ukhls$wave == 3 & ukhls$c_cduse11 == "mentioned"] <- 1 # Has

# Has access to the internet at home
ukhls$b_pcnet[ukhls$b_pcnet == "yes"] <- 1 # yes
ukhls$c_pcnet[ukhls$c_pcnet == "yes"] <- 1
ukhls$b_pcnet[ukhls$b_pcnet == "no"] <- 2 # No
ukhls$c_pcnet[ukhls$c_pcnet == "no"] <- 2
ukhls$b_pcnet[ukhls$b_pcnet == ""] <- 0 # Missing
ukhls$c_pcnet[ukhls$c_pcnet == ""] <- 0
ukhls$b_pcnet <- as.numeric(ukhls$b_pcnet) # Convert to numeric
ukhls$c_pcnet <- as.numeric(ukhls$c_pcnet)

ukhls$pcnet <- 0 # Create new variable for analysis at baseline
ukhls$pcnet[ukhls$wave == 2] <- ukhls$b_pcnet[ukhls$wave == 2]
ukhls$pcnet[ukhls$wave == 3] <- ukhls$c_pcnet[ukhls$wave == 3]

# Number of cars in household
ukhls$b_ncars[ukhls$b_ncars == 4 | ukhls$b_ncars == 5 | ukhls$b_ncars == 6 | ukhls$b_ncars == "7-12"] <- 5 # 4+ cars
ukhls$c_ncars[ukhls$c_ncars == 4 | ukhls$c_ncars == 5 | ukhls$c_ncars == 6 | ukhls$c_ncars == "7-12"] <- 5
ukhls$b_ncars[ukhls$b_ncars == 3] <- 4 # 3 cars
ukhls$c_ncars[ukhls$c_ncars == 3] <- 4
ukhls$b_ncars[ukhls$b_ncars == 2] <- 3 # 2 cars
ukhls$c_ncars[ukhls$c_ncars == 2] <- 3
ukhls$b_ncars[ukhls$b_ncars == 1] <- 2 # 1 car
ukhls$c_ncars[ukhls$c_ncars == 1] <- 2
ukhls$b_ncars[ukhls$b_ncars == 0] <- 1 # 0 cars
ukhls$c_ncars[ukhls$c_ncars == 0] <- 1
ukhls$b_ncars[ukhls$b_ncars == ""] <- 0 # Missing
ukhls$c_ncars[ukhls$c_ncars == ""] <- 0
ukhls$b_ncars <- as.numeric(ukhls$b_ncars) # Convert to numeric
ukhls$c_ncars <- as.numeric(ukhls$c_ncars)

ukhls$ncars <- 0 # Create new variable for analysis at baseline
ukhls$ncars[ukhls$wave == 2] <- ukhls$b_ncars[ukhls$wave == 2]
ukhls$ncars[ukhls$wave == 3] <- ukhls$c_ncars[ukhls$wave == 3]

# Has use of car or van
ukhls$b_caruse[ukhls$b_caruse == "yes"] <- 1 # yes
ukhls$c_caruse[ukhls$c_caruse == "yes"] <- 1
ukhls$b_caruse[ukhls$b_caruse == "no"] <- 2 # No
ukhls$c_caruse[ukhls$c_caruse == "no"] <- 2
ukhls$b_caruse[ukhls$b_caruse == ""] <- 0 # Missing
ukhls$c_caruse[ukhls$c_caruse == ""] <- 0
ukhls$b_caruse <- as.numeric(ukhls$b_caruse) # Convert to numeric
ukhls$c_caruse <- as.numeric(ukhls$c_caruse)

ukhls$caruse <- 0 # Create new variable for analysis at baseline
ukhls$caruse[ukhls$wave == 2] <- ukhls$b_caruse[ukhls$wave == 2]
ukhls$caruse[ukhls$wave == 3] <- ukhls$c_caruse[ukhls$wave == 3]

# Has mobile phone
ukhls$b_mobuse[ukhls$b_mobuse == "yes"] <- 1 # yes
ukhls$c_mobuse[ukhls$c_mobuse == "yes"] <- 1
ukhls$b_mobuse[ukhls$b_mobuse == "no"] <- 2 # No
ukhls$c_mobuse[ukhls$c_mobuse == "no"] <- 2
ukhls$b_mobuse[ukhls$b_mobuse == ""] <- 0 # Missing
ukhls$c_mobuse[ukhls$c_mobuse == ""] <- 0
ukhls$b_mobuse <- as.numeric(ukhls$b_mobuse) # Convert to numeric
ukhls$c_mobuse <- as.numeric(ukhls$c_mobuse)

ukhls$mobuse <- 0 # Create new variable for analysis at baseline
ukhls$mobuse[ukhls$wave == 2] <- ukhls$b_mobuse[ukhls$wave == 2]
ukhls$mobuse[ukhls$wave == 3] <- ukhls$c_mobuse[ukhls$wave == 3]

# Social capital #

ses_soccap_bc <- c("sprt_party", "party_vote", "party_close", "pol_interest")
ses_soccap <- c("b_volun", "b_chargv", "sprt_party", "party_vote", "party_close", "pol_interest")

# Volunteered in last 12 months
ukhls$b_volun[ukhls$b_volun == "yes"] <- 1 # yes
ukhls$b_volun[ukhls$b_volun == "no"] <- 2 # no
ukhls$b_volun[ukhls$b_volun == ""] <- 0 # Missing
ukhls$b_volun <- as.numeric(ukhls$b_volun) # Convert to numeric

# Donates money to charity
ukhls$b_chargv[ukhls$b_chargv == "yes"] <- 1 # yes
ukhls$b_chargv[ukhls$b_chargv == "no"] <- 2 # no
ukhls$b_chargv[ukhls$b_chargv == ""] <- 0 # Missing
ukhls$b_chargv <- as.numeric(ukhls$b_chargv) # Convert to numeric

# Supports a political party
ukhls$b_vote1[ukhls$b_vote1 == "yes"] <- 1 # yes
ukhls$c_vote1[ukhls$c_vote1 == "yes"] <- 1
ukhls$b_vote1[ukhls$b_vote1 == "no"] <- 2 # No
ukhls$c_vote1[ukhls$c_vote1 == "no"] <- 2
ukhls$b_vote1[ukhls$b_vote1 == ""] <- 0 # Missing
ukhls$c_vote1[ukhls$c_vote1 == ""] <- 0
ukhls$b_vote1 <- as.numeric(ukhls$b_vote1) # Convert to numeric
ukhls$c_vote1 <- as.numeric(ukhls$c_vote1)

ukhls$sprt_party <- 0 # Create new variable for analysis at baseline
ukhls$sprt_party[ukhls$wave == 2] <- ukhls$b_vote1[ukhls$wave == 2]
ukhls$sprt_party[ukhls$wave == 3] <- ukhls$c_vote1[ukhls$wave == 3]

# Party would vote for tomorrow
ukhls$b_vote3[ukhls$b_vote3 == "conservative"] <- 1 # Conservative
ukhls$c_vote3[ukhls$c_vote3 == "conservative"] <- 1
ukhls$b_vote3[ukhls$b_vote3 == "liberal democrat"] <- 2 # Lib Dems
ukhls$c_vote3[ukhls$c_vote3 == "liberal democrat"] <- 2
ukhls$b_vote3[ukhls$b_vote3 == "labour"] <- 3 # Labour
ukhls$c_vote3[ukhls$c_vote3 == "labour"] <- 3
ukhls$b_vote3[ukhls$b_vote3 == "plaid cymru" | ukhls$b_vote3 == "scottish national party" | ukhls$b_vote3 == "green party" | ukhls$b_vote3 == "not elsewhere codable"] <- 4 # Other
ukhls$c_vote3[ukhls$c_vote3 == "plaid cymru" | ukhls$c_vote3 == "scottish national party" | ukhls$c_vote3 == "green party" | ukhls$c_vote3 == "not elsewhere codable"] <- 4
ukhls$b_vote3[ukhls$b_vote3 == "none"] <- 5 # None
ukhls$c_vote3[ukhls$c_vote3 == "none"] <- 5
ukhls$b_vote3[ukhls$b_vote3 == "" | ukhls$b_vote3 == "can't vote"] <- 0 # Missing
ukhls$c_vote3[ukhls$c_vote3 == "" | ukhls$c_vote3 == "can't vote"] <- 0
ukhls$b_vote3 <- as.numeric(ukhls$b_vote3) # Convert to numeric
ukhls$c_vote3 <- as.numeric(ukhls$c_vote3)

ukhls$party_vote <- 0 # Create new variable for analysis at baseline
ukhls$party_vote[ukhls$wave == 2] <- ukhls$b_vote3[ukhls$wave == 2]
ukhls$party_vote[ukhls$wave == 3] <- ukhls$c_vote3[ukhls$wave == 3]

# Party feel closest to
ukhls$b_vote4[ukhls$b_vote4 == "conservative"] <- 1 # Conservative
ukhls$c_vote4[ukhls$c_vote4 == "conservative"] <- 1
ukhls$b_vote4[ukhls$b_vote4 == "liberal democrat"] <- 2 # Lib Dems
ukhls$c_vote4[ukhls$c_vote4 == "liberal democrat"] <- 2
ukhls$b_vote4[ukhls$b_vote4 == "labour"] <- 3 # Labour
ukhls$c_vote4[ukhls$c_vote4 == "labour"] <- 3
ukhls$b_vote4[ukhls$b_vote4 == "plaid cymru" | ukhls$b_vote4 == "scottish national party" | ukhls$b_vote4 == "green party" | ukhls$b_vote4 == "not elsewhere codable"] <- 4 # Other
ukhls$c_vote4[ukhls$c_vote4 == "plaid cymru" | ukhls$c_vote4 == "scottish national party" | ukhls$c_vote4 == "green party" | ukhls$c_vote4 == "not elsewhere codable"] <- 4
ukhls$b_vote4[ukhls$b_vote4 == "" | ukhls$b_vote4 == "can't vote"] <- 0 # Missing
ukhls$c_vote4[ukhls$c_vote4 == "" | ukhls$c_vote4 == "can't vote"] <- 0
ukhls$b_vote4 <- as.numeric(ukhls$b_vote4) # Convert to numeric
ukhls$c_vote4 <- as.numeric(ukhls$c_vote4)

ukhls$party_close <- 0 # Create new variable for analysis at baseline
ukhls$party_close[ukhls$wave == 2] <- ukhls$b_vote4[ukhls$wave == 2]
ukhls$party_close[ukhls$wave == 3] <- ukhls$c_vote4[ukhls$wave == 3]

# Level of interest in politics
ukhls$b_vote6[ukhls$b_vote6 == "or not at all interested?"] <- 1 # Not at all
ukhls$c_vote6[ukhls$c_vote6 == "not at all interested?"] <- 1
ukhls$b_vote6[ukhls$b_vote6 == "not very"] <- 2 # Not very
ukhls$c_vote6[ukhls$c_vote6 == "not very, or"] <- 2
ukhls$b_vote6[ukhls$b_vote6 == "fairly"] <- 3 # Fairly
ukhls$c_vote6[ukhls$c_vote6 == "fairly"] <- 3
ukhls$b_vote6[ukhls$b_vote6 == "very"] <- 4 # very
ukhls$c_vote6[ukhls$c_vote6 == "very"] <- 4
ukhls$b_vote6[ukhls$b_vote6 == ""] <- 0 # Missing
ukhls$c_vote6[ukhls$c_vote6 == ""] <- 0
ukhls$b_vote6 <- as.numeric(ukhls$b_vote6) # Convert to numeric
ukhls$c_vote6 <- as.numeric(ukhls$c_vote6)

ukhls$pol_interest <- 0 # Create new variable for analysis at baseline
ukhls$pol_interest[ukhls$wave == 2] <- ukhls$b_vote6[ukhls$wave == 2]
ukhls$pol_interest[ukhls$wave == 3] <- ukhls$c_vote6[ukhls$wave == 3]


## 3. Health ##

health <- c("hcond1", "hcond2", "hcond13", "hcond17", "hcond14", "hcond_cvd", "hcond16", 
            "hcond_oth", "hcond96", "food_spend", "fdout_spend", "alcohol_spend", "b_usdairy", 
            "b_usbread", "b_wkfruit", "b_wkvege", "b_fruvege", "b_wlk10m", "b_daywlk", "b_wlk30min", 
            "b_walkpace", "b_wkphys", "b_sportsfreq", "b_sports3freq", "b_smever", "b_smnow", 
            "b_ncigs", "b_smcigs", "b_aglquit", "htval", "wtval", "bfpcval", "wstval", "bmival", 
            "mmgsdval", "mmgsnval", "lung_cap_fvc", "lung_cap_fev", "lung_cap_ratio", "lung_cap_peak",
            "omsysval", "omdiaval", "ompulval", "concentrate", "sleep", "useful", "decisions", 
            "strain", "overcoming", "enjoy_act", "face_prob", "depressed", "confidence", 
            "self_worth", "happiness")

health_bc <- c("hcond1", "hcond2", "hcond13", "hcond17", "hcond14", "hcond_cvd", "hcond16", 
               "hcond_oth", "hcond96", "food_spend", "fdout_spend", "alcohol_spend", "htval", 
               "wtval", "bfpcval", "wstval", "bmival", "mmgsdval", "mmgsnval", "lung_cap_fvc", 
               "lung_cap_fev", "lung_cap_ratio", "lung_cap_peak", "omsysval", "omdiaval", 
               "ompulval", "concentrate", "sleep", "useful", "decisions", "strain", "overcoming", 
               "enjoy_act", "face_prob", "depressed", "confidence", "self_worth", "happiness")

# 3.1 Health conditions/morbidity #

health_cond <- c("hcond1", "hcond2", "hcond13", "hcond17", "hcond14", "hcond_cvd", "hcond16", 
                 "hcond_oth", "hcond96") # Only really for wave b

# Health conditions
# 1	Asthma, 2	Arthritis, 3	Congestive heart failure, 4	Coronary heart disease, 5	Angina, 
# 6	Heart attack or myocardial infarction, 7	Stroke, 8	Emphysema, 9	Hyperthyroidism or an over-active thyroid, 
# 10	Hypothyroidism or an under-active thyroid, 11	Chronic bronchitis, 12	Any kind of liver condition, 
# 13	Cancer or malignancy, 14	Diabetes, 15	Epilepsy, 16	High blood pressure, 17	Clinical depression, 
# 96	None of these
# Select only those most prevalent conditions or combine together to make more useful

# Asthma
ukhls$hcond1[ukhls$hcond1 == "mentioned"] <- 1 # Yes
ukhls$hcond1[ukhls$hcond1 == "not mentioned"] <- 2 # No
ukhls$hcond1[ukhls$hcond1 == ""] <- 0 # Missing
ukhls$hcond1 <- as.numeric(ukhls$hcond1) # Convert to numeric

# Arthritis
ukhls$hcond2[ukhls$hcond2 == "mentioned"] <- 1 # Yes
ukhls$hcond2[ukhls$hcond2 == "not mentioned"] <- 2 # No
ukhls$hcond2[ukhls$hcond2 == ""] <- 0 # Missing
ukhls$hcond2 <- as.numeric(ukhls$hcond2) # Convert to numeric

# Cancer
ukhls$hcond13[ukhls$hcond13 == "mentioned"] <- 1 # Yes
ukhls$hcond13[ukhls$hcond13 == "not mentioned"] <- 2 # No
ukhls$hcond13[ukhls$hcond13 == ""] <- 0 # Missing
ukhls$hcond13 <- as.numeric(ukhls$hcond13) # Convert to numeric

# Clinical Depression
ukhls$hcond17[ukhls$hcond17 == "mentioned"] <- 1 # Yes
ukhls$hcond17[ukhls$hcond17 == "not mentioned"] <- 2 # No
ukhls$hcond17[ukhls$hcond17 == ""] <- 0 # Missing
ukhls$hcond17 <- as.numeric(ukhls$hcond17) # Convert to numeric

# Diabetes
ukhls$hcond14[ukhls$hcond14 == "mentioned"] <- 1 # Yes
ukhls$hcond14[ukhls$hcond14 == "not mentioned"] <- 2 # No
ukhls$hcond14[ukhls$hcond14 == ""] <- 0 # Missing
ukhls$hcond14 <- as.numeric(ukhls$hcond14) # Convert to numeric

# Cardiovascular
ukhls$hcond_cvd <- 0 # Missing
ukhls$hcond_cvd[ukhls$hcond3 == "not mentioned" | ukhls$hcond4 == "not mentioned" | ukhls$hcond5 == "not mentioned" | ukhls$hcond6 == "not mentioned"] <- 2 # No
ukhls$hcond_cvd[ukhls$hcond3 == "mentioned" | ukhls$hcond4 == "mentioned" | ukhls$hcond5 == "mentioned" | ukhls$hcond6 == "mentioned"] <- 1 # Yes

# Hypertension
ukhls$hcond16[ukhls$hcond16 == "mentioned"] <- 1 # Yes
ukhls$hcond16[ukhls$hcond16 == "not mentioned"] <- 2 # No
ukhls$hcond16[ukhls$hcond16 == ""] <- 0 # Missing
ukhls$hcond16 <- as.numeric(ukhls$hcond16) # Convert to numeric

# Other
ukhls$hcond_oth <- 0
ukhls$hcond_oth[ukhls$hcond7 == "not mentioned" | ukhls$hcond8 == "not mentioned" | ukhls$hcond9 == "not mentioned" | ukhls$hcond10 == "not mentioned" | ukhls$hcond8 == "not mentioned" | 
                  ukhls$hcond9 == "not mentioned" | ukhls$hcond10 == "not mentioned" | ukhls$hcond11 == "not mentioned" | ukhls$hcond12 == "not mentioned"| ukhls$hcond15 == "not mentioned"] <- 2 # No
ukhls$hcond_oth[ukhls$hcond7 == "mentioned" | ukhls$hcond8 == "mentioned" | ukhls$hcond9 == "mentioned" | ukhls$hcond10 == "mentioned" | ukhls$hcond8 == "mentioned" | 
                  ukhls$hcond9 == "mentioned" | ukhls$hcond10 == "mentioned" | ukhls$hcond11 == "mentioned" | ukhls$hcond12 == "mentioned"| ukhls$hcond15 == "mentioned"] <- 1 # Yes

# None of these
ukhls$hcond96[ukhls$hcond96 == 0] <- 2 # No
ukhls$hcond96[is.na(ukhls$hcond96)] <- 0 # Missing

# Behaviours # 

health_behav <- c("food_spend", "fdout_spend", "alcohol_spend", "b_usdairy", "b_usbread", "b_wkfruit", 
                  "b_wkvege", "b_fruvege", "b_wlk10m", "b_daywlk", "b_wlk30min", "b_walkpace", "b_wkphys",
                  "b_sportsfreq", "b_sports3freq", "b_smever", "b_smnow", "b_ncigs", "b_smcigs", 
                  "b_aglquit" )

health_behav_bc <- c("food_spend", "fdout_spend", "alcohol_spend")

# Amount spent on food from supermarket
ukhls$b_xpfood1_g3[ukhls$b_xpfood1_g3 == "0" | ukhls$b_xpfood1_g3 == "0.01-70" | ukhls$b_xpfood1_g3 == "70-90" | ukhls$b_xpfood1_g3 == "90-100"] <- 1 # 0-100
ukhls$c_xpfood1_g3[ukhls$c_xpfood1_g3 == "0" | ukhls$c_xpfood1_g3 == "0.01-70" | ukhls$c_xpfood1_g3 == "70-100"] <- 1 
ukhls$b_xpfood1_g3[ukhls$b_xpfood1_g3 == "100-110" | ukhls$b_xpfood1_g3 == "110-120" | ukhls$b_xpfood1_g3 == "120-130" | ukhls$b_xpfood1_g3 == "130-150"] <- 2 # 100-150
ukhls$c_xpfood1_g3[ukhls$c_xpfood1_g3 == "100-120" | ukhls$c_xpfood1_g3 == "120-140" | ukhls$c_xpfood1_g3 == "140-150"] <- 2 
ukhls$b_xpfood1_g3[ukhls$b_xpfood1_g3 == "150-160" | ukhls$b_xpfood1_g3 == "160-190" | ukhls$b_xpfood1_g3 == "190-200"] <- 3 # 150-200
ukhls$c_xpfood1_g3[ukhls$c_xpfood1_g3 == "150-160" | ukhls$c_xpfood1_g3 == "160-180" | ukhls$c_xpfood1_g3 == "180-200"] <- 3 
ukhls$b_xpfood1_g3[ukhls$b_xpfood1_g3 == "200-240" | ukhls$b_xpfood1_g3 == "240-250"] <- 4 # 200-250
ukhls$c_xpfood1_g3[ukhls$c_xpfood1_g3 == "200-210" | ukhls$c_xpfood1_g3 == "210-240" | ukhls$c_xpfood1_g3 == "240-250"] <- 4 
ukhls$b_xpfood1_g3[ukhls$b_xpfood1_g3 == "250-260" | ukhls$b_xpfood1_g3 == "260-280" | ukhls$b_xpfood1_g3 == "280-300"] <- 5 # 250-300
ukhls$c_xpfood1_g3[ukhls$c_xpfood1_g3 == "250-275" | ukhls$c_xpfood1_g3 == "275-280" | ukhls$c_xpfood1_g3 == "280-300"] <- 5 
ukhls$b_xpfood1_g3[ukhls$b_xpfood1_g3 == "300-320" | ukhls$b_xpfood1_g3 == "320-350"] <- 6 # 300-350
ukhls$c_xpfood1_g3[ukhls$c_xpfood1_g3 == "300-320" | ukhls$c_xpfood1_g3 == "320-350"] <- 6 
ukhls$b_xpfood1_g3[ukhls$b_xpfood1_g3 == "350-360" | ukhls$b_xpfood1_g3 == "360-400"] <- 7 # 350-400
ukhls$c_xpfood1_g3[ukhls$c_xpfood1_g3 == "350-360" | ukhls$c_xpfood1_g3 == "360-400"] <- 7 
ukhls$b_xpfood1_g3[ukhls$b_xpfood1_g3 == "400-420" | ukhls$b_xpfood1_g3 == "420-450" | ukhls$b_xpfood1_g3 == "450-480" | ukhls$b_xpfood1_g3 == "480-500"] <- 8 # 400-500
ukhls$c_xpfood1_g3[ukhls$c_xpfood1_g3 == "400-425" | ukhls$c_xpfood1_g3 == "425-450" | ukhls$c_xpfood1_g3 == "450-480" | ukhls$c_xpfood1_g3 == "480-500"] <- 8 
ukhls$b_xpfood1_g3[ukhls$b_xpfood1_g3 == "500-520" | ukhls$b_xpfood1_g3 == "520-600" | ukhls$b_xpfood1_g3 == "600-700" | ukhls$b_xpfood1_g3 == "700 or more"] <- 9 # 500+
ukhls$c_xpfood1_g3[ukhls$c_xpfood1_g3 == "500-600" | ukhls$c_xpfood1_g3 == "600-700" | ukhls$c_xpfood1_g3 == "700 or more"] <- 9 
ukhls$b_xpfood1_g3[ukhls$b_xpfood1_g3 == ""] <- 0 # Missing
ukhls$c_xpfood1_g3[ukhls$c_xpfood1_g3 == ""] <- 0 
ukhls$b_xpfood1_g3 <- as.numeric(ukhls$b_xpfood1_g3) # Convert to numeric
ukhls$c_xpfood1_g3 <- as.numeric(ukhls$c_xpfood1_g3)

ukhls$food_spend <- 0 # Create new variable for analysis at baseline
ukhls$food_spend[ukhls$wave == 2] <- ukhls$b_xpfood1_g3[ukhls$wave == 2]
ukhls$food_spend[ukhls$wave == 3] <- ukhls$c_xpfood1_g3[ukhls$wave == 3]

# Amount spent on meals/snacks outside of the home
ukhls$b_xpfdout_g3[ukhls$b_xpfdout_g3 == "0"] <- 1 # 0
ukhls$c_xpfdout_g3[ukhls$c_xpfdout_g3 == "0"] <- 1 
ukhls$b_xpfdout_g3[ukhls$b_xpfdout_g3 == "0.01-10"] <- 2 # 0.01-10
ukhls$c_xpfdout_g3[ukhls$c_xpfdout_g3 == "0.01-10"] <- 2 
ukhls$b_xpfdout_g3[ukhls$b_xpfdout_g3 == "10-15" | ukhls$b_xpfdout_g3 == "15-20"] <- 3 # 10-20
ukhls$c_xpfdout_g3[ukhls$c_xpfdout_g3 == "10-15" | ukhls$c_xpfdout_g3 == "15-20"] <- 3 
ukhls$b_xpfdout_g3[ukhls$b_xpfdout_g3 == "20-25" | ukhls$b_xpfdout_g3 == "25-30"] <- 4 # 20-30
ukhls$c_xpfdout_g3[ukhls$c_xpfdout_g3 == "20-24" | ukhls$c_xpfdout_g3 == "24-25" | ukhls$c_xpfdout_g3 == "25-30"] <- 4
ukhls$b_xpfdout_g3[ukhls$b_xpfdout_g3 == "30-40"] <- 5 # 30-40
ukhls$c_xpfdout_g3[ukhls$c_xpfdout_g3 == "30-40"] <- 5
ukhls$b_xpfdout_g3[ukhls$b_xpfdout_g3 == "40-45" | ukhls$b_xpfdout_g3 == "45-50"] <- 6 # 40-50
ukhls$c_xpfdout_g3[ukhls$c_xpfdout_g3 == "40-50"] <- 6
ukhls$b_xpfdout_g3[ukhls$b_xpfdout_g3 == "50-60"] <- 7 # 50-60
ukhls$c_xpfdout_g3[ukhls$c_xpfdout_g3 == "50-55" | ukhls$c_xpfdout_g3 == "55-60"] <- 7
ukhls$b_xpfdout_g3[ukhls$b_xpfdout_g3 == "60-70" | ukhls$b_xpfdout_g3 == "70-75" | ukhls$b_xpfdout_g3 == "75-80"] <- 8 # 60-80
ukhls$c_xpfdout_g3[ukhls$c_xpfdout_g3 == "60-70" | ukhls$c_xpfdout_g3 == "70-80"] <- 8
ukhls$b_xpfdout_g3[ukhls$b_xpfdout_g3 == "80-100"] <- 9 # 80-100
ukhls$c_xpfdout_g3[ukhls$c_xpfdout_g3 == "80-100"] <- 9 
ukhls$b_xpfdout_g3[ukhls$b_xpfdout_g3 == "100-120" | ukhls$b_xpfdout_g3 == "120-125" | ukhls$b_xpfdout_g3 == "125-150"] <- 10 # 100-150
ukhls$c_xpfdout_g3[ukhls$c_xpfdout_g3 == "100-120" | ukhls$c_xpfdout_g3 == "120-140" | ukhls$c_xpfdout_g3 == "140-150"] <- 10
ukhls$b_xpfdout_g3[ukhls$b_xpfdout_g3 == "150-180" | ukhls$b_xpfdout_g3 == "180-200"] <- 11 # 150-200
ukhls$c_xpfdout_g3[ukhls$c_xpfdout_g3 == "150-160" | ukhls$c_xpfdout_g3 == "160-200"] <- 11
ukhls$b_xpfdout_g3[ukhls$b_xpfdout_g3 == "200-250" | ukhls$b_xpfdout_g3 == "250-300" | ukhls$b_xpfdout_g3 == "300-400" | ukhls$b_xpfdout_g3 == "400 or more"] <- 12 # 200+
ukhls$c_xpfdout_g3[ukhls$c_xpfdout_g3 == "200-220" | ukhls$c_xpfdout_g3 == "220-250" | ukhls$c_xpfdout_g3 == "250-300" | ukhls$c_xpfdout_g3 == "300-400" | ukhls$c_xpfdout_g3 == "400 or more"] <- 12
ukhls$b_xpfdout_g3[ukhls$b_xpfdout_g3 == ""] <- 0 # Missing
ukhls$c_xpfdout_g3[ukhls$c_xpfdout_g3 == ""] <- 0 
ukhls$b_xpfdout_g3 <- as.numeric(ukhls$b_xpfdout_g3) # Convert to numeric
ukhls$c_xpfdout_g3 <- as.numeric(ukhls$c_xpfdout_g3)

ukhls$fdout_spend <- 0 # Create new variable for analysis at baseline
ukhls$fdout_spend[ukhls$wave == 2] <- ukhls$b_xpfdout_g3[ukhls$wave == 2]
ukhls$fdout_spend[ukhls$wave == 3] <- ukhls$c_xpfdout_g3[ukhls$wave == 3]

# Amount spent on alcohol
ukhls$b_xpaltob_g3[ukhls$b_xpaltob_g3 == "0"] <- 1 # 0
ukhls$c_xpaltob_g3[ukhls$c_xpaltob_g3 == "0"] <- 1 
ukhls$b_xpaltob_g3[ukhls$b_xpaltob_g3 == "0.01-5" | ukhls$b_xpaltob_g3 == "5-7" |ukhls$b_xpaltob_g3 == "7-10"] <- 2 # 0.01-10
ukhls$c_xpaltob_g3[ukhls$c_xpaltob_g3 == "0.01-5" | ukhls$c_xpaltob_g3 == "5-8" | ukhls$c_xpaltob_g3 == "8-10"] <- 2 
ukhls$b_xpaltob_g3[ukhls$b_xpaltob_g3 == "10-12" | ukhls$b_xpaltob_g3 == "12-15" | ukhls$b_xpaltob_g3 == "15-16"| ukhls$b_xpaltob_g3 == "16-20"] <- 3 # 10-20
ukhls$c_xpaltob_g3[ukhls$c_xpaltob_g3 == "10-12" | ukhls$c_xpaltob_g3 == "12-15" | ukhls$c_xpaltob_g3 == "15-20"] <- 3 
ukhls$b_xpaltob_g3[ukhls$b_xpaltob_g3 == "20-24" | ukhls$b_xpaltob_g3 == "24-25" | ukhls$b_xpaltob_g3 == "25-30"] <- 4 # 20-30
ukhls$c_xpaltob_g3[ukhls$c_xpaltob_g3 == "20-24" | ukhls$c_xpaltob_g3 == "24-25" | ukhls$c_xpaltob_g3 == "25-30"] <- 4
ukhls$b_xpaltob_g3[ukhls$b_xpaltob_g3 == "30-35" | ukhls$b_xpaltob_g3 == "35-40"] <- 5 # 30-40
ukhls$c_xpaltob_g3[ukhls$c_xpaltob_g3 == "30-35" | ukhls$c_xpaltob_g3 == "35-40"] <- 5
ukhls$b_xpaltob_g3[ukhls$b_xpaltob_g3 == "40-45" | ukhls$b_xpaltob_g3 == "45-50"] <- 6 # 40-50
ukhls$c_xpaltob_g3[ukhls$c_xpaltob_g3 == "40-50"] <- 6
ukhls$b_xpaltob_g3[ukhls$b_xpaltob_g3 == "50-55"| ukhls$b_xpaltob_g3 == "55-60"] <- 7 # 50-60
ukhls$c_xpaltob_g3[ukhls$c_xpaltob_g3 == "50-60"] <- 7
ukhls$b_xpaltob_g3[ukhls$b_xpaltob_g3 == "60-70" | ukhls$b_xpaltob_g3 == "70-80"] <- 8 # 60-80
ukhls$c_xpaltob_g3[ukhls$c_xpaltob_g3 == "60-70" | ukhls$c_xpaltob_g3 == "70-80"] <- 8
ukhls$b_xpaltob_g3[ukhls$b_xpaltob_g3 == "80-100"] <- 9 # 80-100
ukhls$c_xpaltob_g3[ukhls$c_xpaltob_g3 == "80-100"] <- 9 
ukhls$b_xpaltob_g3[ukhls$b_xpaltob_g3 == "100-110" | ukhls$b_xpaltob_g3 == "110-120" | ukhls$b_xpaltob_g3 == "120-150"] <- 10 # 100-150
ukhls$c_xpaltob_g3[ukhls$c_xpaltob_g3 == "100-110" | ukhls$c_xpaltob_g3 == "110-120" | ukhls$c_xpaltob_g3 == "120-150"] <- 10
ukhls$b_xpaltob_g3[ukhls$b_xpaltob_g3 == "150-200"] <- 11 # 150-200
ukhls$c_xpaltob_g3[ukhls$c_xpaltob_g3 == "150-200"] <- 11
ukhls$b_xpaltob_g3[ukhls$b_xpaltob_g3 == "200-220" | ukhls$b_xpaltob_g3 == "220-300" | ukhls$b_xpaltob_g3 == "300 or more"] <- 12 # 200+
ukhls$c_xpaltob_g3[ukhls$c_xpaltob_g3 == "200-300" | ukhls$c_xpaltob_g3 == "300 or more"] <- 12
ukhls$b_xpaltob_g3[ukhls$b_xpaltob_g3 == ""] <- 0 # Missing
ukhls$c_xpaltob_g3[ukhls$c_xpaltob_g3 == ""] <- 0 
ukhls$b_xpaltob_g3 <- as.numeric(ukhls$b_xpaltob_g3) # Convert to numeric
ukhls$c_xpaltob_g3 <- as.numeric(ukhls$c_xpaltob_g3)

ukhls$alcohol_spend <- 0 # Create new variable for analysis at baseline
ukhls$alcohol_spend[ukhls$wave == 2] <- ukhls$b_xpaltob_g3[ukhls$wave == 2]
ukhls$alcohol_spend[ukhls$wave == 3] <- ukhls$c_xpaltob_g3[ukhls$wave == 3]

# Dairy consumption type (only wave b)
ukhls$b_usdairy[ukhls$b_usdairy == ""] <- 0 # Missing
ukhls$b_usdairy[ukhls$b_usdairy == "spontaneous: don't use milk"] <- 1 # Don't use milk
ukhls$b_usdairy[ukhls$b_usdairy == "any other sort of milk" | ukhls$b_usdairy == "soya milk"] <- 2 # Other
ukhls$b_usdairy[ukhls$b_usdairy == "skimmed milk"] <- 3 # Skimmed milk
ukhls$b_usdairy[ukhls$b_usdairy == "semi-skimmed milk"] <- 4 # Semi skimmed milk
ukhls$b_usdairy[ukhls$b_usdairy == "whole milk"] <- 5 # Whole milk
ukhls$b_usdairy <- as.numeric(ukhls$b_usdairy) # Convert to numeric

# Bread consumption type (only wave b)
ukhls$b_usbread[ukhls$b_usbread == ""] <- 0 # Missing
ukhls$b_usbread[ukhls$b_usbread == "spontaneous: don't eat bread"] <- 1 # Don't use milk
ukhls$b_usbread[ukhls$b_usbread == "other brown" | ukhls$b_usbread == "other type of bread"] <- 2 # Other
ukhls$b_usbread[ukhls$b_usbread == "granary or wholegrain"] <- 3 # Granary/wholegrain
ukhls$b_usbread[ukhls$b_usbread == "wholemeal"] <- 4 # Wholemeal
ukhls$b_usbread[ukhls$b_usbread == "both brown and white"] <- 5 # Both brown and white
ukhls$b_usbread[ukhls$b_usbread == "white"] <- 6 # White
ukhls$b_usbread <- as.numeric(ukhls$b_usbread) # Convert to numeric

# Fruit consumption per week
ukhls$b_wkfruit[ukhls$b_wkfruit == ""] <- 0 # Missing
ukhls$b_wkfruit[ukhls$b_wkfruit == "never"] <- 1 # Never
ukhls$b_wkfruit[ukhls$b_wkfruit == "1 - 3 days"] <- 2 # 1-3 days per week
ukhls$b_wkfruit[ukhls$b_wkfruit == "4 - 6 days"] <- 3 # 4 -6 days per week
ukhls$b_wkfruit[ukhls$b_wkfruit == "every day"] <- 4 # Every day
ukhls$b_wkfruit <- as.numeric(ukhls$b_wkfruit) # Convert to numeric

# Vegetable consumption per week
ukhls$b_wkvege[ukhls$b_wkvege == ""] <- 0 # Missing
ukhls$b_wkvege[ukhls$b_wkvege == "never"] <- 1 # Never
ukhls$b_wkvege[ukhls$b_wkvege == "1 - 3 days"] <- 2 # 1-3 days per week
ukhls$b_wkvege[ukhls$b_wkvege == "4 - 6 days"] <- 3 # 4 -6 days per week
ukhls$b_wkvege[ukhls$b_wkvege == "every day"] <- 4 # Every day
ukhls$b_wkvege <- as.numeric(ukhls$b_wkvege) # Convert to numeric

# Usual portion of fruit/vegetables eaten per day
ukhls$b_fruvege[ukhls$b_fruvege == ""] <- 0 # Missing
# no need to recode values 1 - 5 - they remain the same
ukhls$b_fruvege[ukhls$b_fruvege >= 6 | ukhls$b_fruvege == "10" | ukhls$b_fruvege == "11-26"] <- 6 # 6+
ukhls$b_fruvege <- as.numeric(ukhls$b_fruvege) # Convert to numeric

# Have walked for 10 minutes
ukhls$b_wlk10m[ukhls$b_wlk10m == ""] <- 0 # Missing
ukhls$b_wlk10m[ukhls$b_wlk10m == "yes"] <- 1 # Yes
ukhls$b_wlk10m[ukhls$b_wlk10m == "no" | ukhls$b_wlk10m == "spontaneous: can't walk at all"] <- 2 # No
ukhls$b_wlk10m <- as.numeric(ukhls$b_wlk10m) # Convert to numeric

# Number of days have walked 10 minutes (if yes to above)
ukhls$b_daywlk[ukhls$b_daywlk == ""] <- 0 # Missing
ukhls$b_daywlk[ukhls$b_daywlk == "17-18"] <- 17.5
ukhls$b_daywlk <- as.numeric(ukhls$b_daywlk) # Convert to numeric
ukhls$b_daywlk[ukhls$b_daywlk > 0] <- ukhls$b_daywlk[ukhls$b_daywlk > 0] + 1
ukhls$b_daywlk[ukhls$b_wlk10m == 2] <- 1 # 0 days

# Number of days have walked 30 minutes (if yes to above)
ukhls$b_wlk30min <- as.numeric(ukhls$b_wlk30min) # Convert to numeric
ukhls$b_wlk30min[!is.na(ukhls$b_wlk30min)] <- ukhls$b_wlk30min[!is.na(ukhls$b_wlk30min)] + 1
ukhls$b_wlk30min[is.na(ukhls$b_wlk30min)] <- 0 # Missing
ukhls$b_wlk30min[ukhls$b_wlk10m == 2] <- 1 # 0 days

# Walking pace
ukhls$b_walkpace[ukhls$b_walkpace == ""] <- 0 # Missing
ukhls$b_walkpace[ukhls$b_wlk10m == 2] <- 1 # Do not walk
ukhls$b_walkpace[ukhls$b_walkpace == "a slow pace"] <- 2 # Slow pace
ukhls$b_walkpace[ukhls$b_walkpace == "a steady average pace"] <- 3 # Average pace
ukhls$b_walkpace[ukhls$b_walkpace == "a fairly brisk pace"] <- 4 # Brisk pace
ukhls$b_walkpace[ukhls$b_walkpace == "or a fast pace - at least 4 miles per hour?"] <- 5 # Fast pace
ukhls$b_walkpace <- as.numeric(ukhls$b_walkpace) # Convert to numeric

# Ever smoked
ukhls$b_smever[ukhls$b_smever == ""] <- 0 # Missing
ukhls$b_smever[ukhls$b_smever == "yes"] <- 1 # Yes
ukhls$b_smever[ukhls$b_smever == "no"] <- 2 # No
ukhls$b_smever <- as.numeric(ukhls$b_smever) # Convert to numeric

# Current smoker
ukhls$b_smnow[ukhls$b_smnow == ""] <- 0 # Missing
ukhls$b_smnow[ukhls$b_smnow == "yes"] <- 1 # Yes
ukhls$b_smnow[ukhls$b_smnow == "no" | ukhls$b_smever == 2] <- 2 # No
ukhls$b_smnow <- as.numeric(ukhls$b_smnow) # Convert to numeric

# Usual number of cigarettes smoked per day
ukhls$b_ncigs[ukhls$b_ncigs == "11-12"] <- 11.5 # Edit characters to make it easier to categorise
ukhls$b_ncigs[ukhls$b_ncigs == "21-25" | ukhls$b_ncigs == "26-30" | ukhls$b_ncigs == "31-40" | ukhls$b_ncigs == "41-80"] <- 20 
ukhls$b_ncigs <- as.numeric(ukhls$b_ncigs) # Convert to numeric
ukhls$b_ncigs[!is.na(ukhls$b_ncigs)] <- ukhls$b_ncigs[!is.na(ukhls$b_ncigs)] + 2
ukhls$b_ncigs[is.na(ukhls$b_ncigs)] <- 0 # Missing
ukhls$b_ncigs[ukhls$b_smnow == 2] <- 1 # Don't smoke

# Ever smoked regularly
ukhls$b_smcigs[ukhls$b_smcigs == ""] <- 0 # Missing
ukhls$b_smcigs[ukhls$b_smever == 2] <- 1 # Don't smoke
ukhls$b_smcigs[ukhls$b_smcigs == "spontaneous never really smoked cigarettes, just tried them once or twice"] <- 2 # Smoked once or twice
ukhls$b_smcigs[ukhls$b_smcigs == "smoke them only occasionally"] <- 3 # Occassionally
ukhls$b_smcigs[ukhls$b_smcigs == "smoked regularly, at least one per day"] <- 4 # Regularly
ukhls$b_smcigs <- as.numeric(ukhls$b_smcigs) # Convert to numeric

# Age quit smoking
ukhls$b_aglquit[ukhls$b_aglquit == "13-15"] <- 14 # Edit characters to make it easier to categorise
ukhls$b_aglquit[ukhls$b_aglquit == "7-12"] <- 12
ukhls$b_aglquit[ukhls$b_aglquit == "70-79"] <- 75 # Midpoint
ukhls$b_aglquit[ukhls$b_aglquit == "80-90"] <- 80
ukhls$b_aglquit <- as.numeric(ukhls$b_aglquit) # Convert to numeric
ukhls$b_aglquit[is.na(ukhls$b_aglquit)] <- 0 # Missing
ukhls$b_aglquit[ukhls$b_smnow == 1] <- 1 # Current smoker
ukhls$b_aglquit[ukhls$b_smever == 2] <- 2 # Never smoked

# Age started smoking
ukhls$b_smagbg[ukhls$b_smagbg == "31-35"] <- 33 # Edit characters to make it easier to categorise
ukhls$b_smagbg[ukhls$b_smagbg == "36-40"] <- 38
ukhls$b_smagbg[ukhls$b_smagbg == "41-50"] <- 45.5
ukhls$b_smagbg[ukhls$b_smagbg == "51-60"] <- 55.5
ukhls$b_smagbg[ukhls$b_smagbg == "3-6"] <- 6
ukhls$b_smagbg[ukhls$b_smagbg == ""] <- 0 # Missing
ukhls$b_smagbg <- as.numeric(ukhls$b_smagbg) # Convert to numeric
ukhls$b_smagbg[ukhls$b_smever == 2] <- 1 # Never smoked

# Physicality of job
ukhls$b_wkphys[ukhls$b_wkphys == "very physically active"] <- 1 # Very
ukhls$b_wkphys[ukhls$b_wkphys == "fairly physically active"] <- 2 # fairly
ukhls$b_wkphys[ukhls$b_wkphys == "not very physically active"] <- 3 # Not very
ukhls$b_wkphys[ukhls$b_wkphys == "or not at all physically active in your job?"] <- 4 # Not at all
ukhls$b_wkphys[ukhls$b_wkphys == ""] <- 0 # Missing
ukhls$b_wkphys <- as.numeric(ukhls$b_wkphys) # Convert to numeric

# Moderate sports frequency
ukhls$b_sportsfreq[ukhls$b_sportsfreq == "once in past year" | ukhls$b_sportsfreq == "twice in past year"] <- 1 # Rarely
ukhls$b_sportsfreq[ukhls$b_sportsfreq == "at least 3 - 4 times/yr"] <- 2 # 3-4 times a year
ukhls$b_sportsfreq[ukhls$b_sportsfreq == "at least once/month"] <- 3 # Once a month
ukhls$b_sportsfreq[ukhls$b_sportsfreq == "1-3 times a week"] <- 4 # Once a week a week
ukhls$b_sportsfreq[ukhls$b_sportsfreq == "3+ times a week"] <- 5 # 3+ times a week
ukhls$b_sportsfreq[ukhls$b_sportsfreq == ""] <- 0 # Missing
ukhls$b_sportsfreq <- as.numeric(ukhls$b_sportsfreq) # Convert to numeric

# Mild sports frequency
ukhls$b_sports3freq[ukhls$b_sports3freq == "once in past year" | ukhls$b_sports3freq == "twice in past year"] <- 1 # Rarely
ukhls$b_sports3freq[ukhls$b_sports3freq == "at least 3 - 4 times/yr"] <- 2 # 3-4 times a year
ukhls$b_sports3freq[ukhls$b_sports3freq == "at least once/month"] <- 3 # Once a month
ukhls$b_sports3freq[ukhls$b_sports3freq == "1-3 times a week"] <- 4 # Once a week a week
ukhls$b_sports3freq[ukhls$b_sports3freq == "3+ times a week"] <- 5 # 3+ times a week
ukhls$b_sports3freq[ukhls$b_sports3freq == ""] <- 0 # Missing
ukhls$b_sports3freq <- as.numeric(ukhls$b_sports3freq) # Convert to numeric

# Physical functioning #

health_physfun <- c("htval", "wtval", "bfpcval", "wstval", "bmival", "mmgsdval", "mmgsnval",
                    "lung_cap_fvc", "lung_cap_fev", "lung_cap_ratio", "lung_cap_peak",
                    "omsysval", "omdiaval", "ompulval")

# Valid height
ukhls$htval[ukhls$htval == "top 1%: 189-205"] <- 189 
ukhls$htval[ukhls$htval == "bottom 0.5%: 84-146"] <- 146
ukhls$htval[ukhls$htval == ""] <- 0
ukhls$htval <- as.numeric(ukhls$htval)

# Valid weight
ukhls$wtval[ukhls$wtval == "top 1%: 129-210"] <- 129 
ukhls$wtval[ukhls$wtval == "bottom 0.5%: 36-46"] <- 46
ukhls$wtval[ukhls$wtval == ""] <- 0
ukhls$wtval <- as.numeric(ukhls$wtval)

# Body fat %
ukhls$bfpcval[is.na(ukhls$bfpcval)] <- 0
ukhls$bfpcval <- as.numeric(ukhls$bfpcval)

# Waist circumference
ukhls$wstval[ukhls$wstval == "top 1%: 130-177"] <- 130
ukhls$wstval[ukhls$wstval == "bottom 0.5%: 60-65"] <- 65
ukhls$wstval[ukhls$wstval == ""] <- 0
ukhls$wstval <- as.numeric(ukhls$wstval)

# Valid BMI
ukhls$bmival[ukhls$bmival == "top 1%: 44.09999847412109-163.2"] <- 44.09999847412109
ukhls$bmival[ukhls$bmival == "bottom 1%: 14.5-18.60000038146973"] <- 18.60000038146973
ukhls$bmival[ukhls$bmival == "98-99%: 41.59999847412109-44.09999847412109"] <- 42.7
ukhls$bmival[ukhls$bmival == "97%-98%: 40.09999847412109-41.59999847412109"] <- 40.8
ukhls$bmival[ukhls$bmival == ""] <- 0
ukhls$bmival <- as.numeric(ukhls$bmival)

# Grip strength dominant hand
ukhls$mmgsdval[ukhls$mmgsdval == "bottom 0.5%: 0-7"] <- 7
ukhls$mmgsdval[ukhls$mmgsdval == "top 1%: 64-79"] <- 64
ukhls$mmgsdval[ukhls$mmgsdval == ""] <- 0
ukhls$mmgsdval <- as.numeric(ukhls$mmgsdval)

# Grip strength non-dominant hand
ukhls$mmgsnval[ukhls$mmgsnval == "bottom 0.5%: 0-8"] <- 8
ukhls$mmgsnval[ukhls$mmgsnval == "top 1%: 60-78"] <- 60
ukhls$mmgsnval[ukhls$mmgsnval == ""] <- 0
ukhls$mmgsnval <- as.numeric(ukhls$mmgsnval)

# Forced vital lung capacity (we will combine the Scottish and Eng/Wales data together)
ukhls$lung_cap_fvc <- ukhls$htfvc
ukhls$lung_cap_fvc[is.na(ukhls$ukhls$htfvc)] <- ukhls$htfvc_sc
ukhls$lung_cap_fvc[is.na(ukhls$lung_cap_fvc)] <- 0
ukhls$lung_cap_fvc <- as.numeric(ukhls$lung_cap_fvc)

# Forced expiratory lung volume (we will combine the Scottish and Eng/Wales data together)
ukhls$lung_cap_fev <- ukhls$htfev
ukhls$lung_cap_fev[is.na(ukhls$ukhls$htfev)] <- ukhls$htfev_sc
ukhls$lung_cap_fev[is.na(ukhls$lung_cap_fev)] <- 0
ukhls$lung_cap_fev <- as.numeric(ukhls$lung_cap_fev)

# Ratio of FEV/FVC
ukhls$lung_cap_ratio <- ukhls$htfevfvc
ukhls$lung_cap_ratio[is.na(ukhls$ukhls$htfevfvc)] <- ukhls$htfevfvc_sc
ukhls$lung_cap_ratio[is.na(ukhls$lung_cap_ratio)] <- 0
ukhls$lung_cap_ratio <- as.numeric(ukhls$lung_cap_ratio)

# Peak expiratory flow
ukhls$lung_cap_peak <- ukhls$htpef
ukhls$lung_cap_peak[ukhls$lung_cap_peak == "top 0.5%: 14-18"] <- 14
ukhls$lung_cap_peak[ukhls$lung_cap_peak == "bottom 0.5%: 1-2"] <- 2
ukhls$lung_cap_peak[is.na(ukhls$ukhls$htpef)] <- ukhls$htpef_sc
ukhls$lung_cap_peak[ukhls$lung_cap_peak == ""] <- 0
ukhls$lung_cap_peak <- as.numeric(ukhls$lung_cap_peak)

# Systolic blood pressure
ukhls$omsysval[ukhls$omsysval == "top 1%: 174-216"] <- 174
ukhls$omsysval[ukhls$omsysval == "98%-99%: 166-174"] <- 170
ukhls$omsysval[ukhls$omsysval == "bottom 0.5%: 75-93.5"] <- 93.5
ukhls$omsysval[ukhls$omsysval == ""] <- 0
ukhls$omsysval <- as.numeric(ukhls$omsysval)

# Diastolic blood pressure
ukhls$omdiaval[ukhls$omdiaval == "top 1%: 100.5-124"] <- 100.5
ukhls$omdiaval[ukhls$omdiaval == "bottom 0.5%: 40-48.5"] <- 48.5
ukhls$omdiaval[ukhls$omdiaval == ""] <- 0
ukhls$omdiaval <- as.numeric(ukhls$omdiaval)

# Pulse
ukhls$ompulval[ukhls$ompulval == "top 1%: 98-132.5"] <- 98
ukhls$ompulval[ukhls$ompulval == "bottom 0.5%: 34-43.5"] <- 43.5
ukhls$ompulval[ukhls$ompulval == ""] <- 0
ukhls$ompulval <- as.numeric(ukhls$ompulval)

# Wellbeing #

health_wellb <- c("concentrate", "sleep", "useful", "decisions", "strain", "overcoming", "enjoy_act", 
                  "face_prob", "depressed", "confidence", "self_worth", "happiness")

# General Health Questionnaire
# A: Concentration
ukhls$concentrate <- 0 # Missing
ukhls$concentrate[(ukhls$b_scghqa == "better than usual" & ukhls$wave == 2) | 
                    (ukhls$c_scghqa == "better than usual" & ukhls$wave == 3)] <- 1 # Better than usual
ukhls$concentrate[(ukhls$b_scghqa == "same as usual" & ukhls$wave == 2) | 
                    (ukhls$c_scghqa == "same as usual" & ukhls$wave == 3)] <- 2 # Same as usual
ukhls$concentrate[(ukhls$b_scghqa == "less than usual" & ukhls$wave == 2) | 
                    (ukhls$c_scghqa == "less than usual" & ukhls$wave == 3)] <- 3 # Less than usual
ukhls$concentrate[(ukhls$b_scghqa == "much less than usual" & ukhls$wave == 2) | 
                    (ukhls$c_scghqa == "much less than usual" & ukhls$wave == 3)] <- 4 # Much less than usual

# B: Loss of sleep
ukhls$sleep <- 0 # Missing
ukhls$sleep[(ukhls$b_scghqb == "much more than usual" & ukhls$wave == 2) | 
              (ukhls$c_scghqb == "much more than usual" & ukhls$wave == 3)] <- 1 # Much more than usual
ukhls$sleep[(ukhls$b_scghqb == "rather more than usual" & ukhls$wave == 2) | 
              (ukhls$c_scghqb == "rather more than usual" & ukhls$wave == 3)] <- 2 # Rather more than usual
ukhls$sleep[(ukhls$b_scghqb == "no more than usual" & ukhls$wave == 2) | 
              (ukhls$c_scghqb == "no more than usual" & ukhls$wave == 3)] <- 3 # No more than usual
ukhls$sleep[(ukhls$b_scghqb == "not at all" & ukhls$wave == 2) | 
              (ukhls$c_scghqb == "not at all" & ukhls$wave == 3)] <- 4 # Not at all

# C: Playing a useful role
ukhls$useful <- 0 # Missing
ukhls$useful[(ukhls$b_scghqc == "more than usual" & ukhls$wave == 2) | 
               (ukhls$c_scghqc == "more than usual" & ukhls$wave == 3)] <- 1 # More than usual
ukhls$useful[(ukhls$b_scghqc == "same as usual" & ukhls$wave == 2) | 
               (ukhls$c_scghqc == "same as usual" & ukhls$wave == 3)] <- 2 # Same as usual
ukhls$useful[(ukhls$b_scghqc == "less so that usual" & ukhls$wave == 2) | 
               (ukhls$c_scghqc == "less so than usual" & ukhls$wave == 3)] <- 3 # Less than usual
ukhls$useful[(ukhls$b_scghqc == "much less than usual" & ukhls$wave == 2) | 
               (ukhls$c_scghqc == "much less than usual" & ukhls$wave == 3)] <- 4 # Much less than usual

# D: Capable of making decisions
ukhls$decisions <- 0 # Missing
ukhls$decisions[(ukhls$b_scghqd == "more so than usual" & ukhls$wave == 2) | 
                  (ukhls$c_scghqd == "more so than usual" & ukhls$wave == 3)] <- 1 # More than usual
ukhls$decisions[(ukhls$b_scghqd == "same as usual" & ukhls$wave == 2) | 
                  (ukhls$c_scghqd == "same as usual" & ukhls$wave == 3)] <- 2 # Same as usual
ukhls$decisions[(ukhls$b_scghqd == "less than usual" & ukhls$wave == 2) | 
                  (ukhls$c_scghqd == "less than usual" & ukhls$wave == 3)] <- 3 # Less than usual
ukhls$decisions[(ukhls$b_scghqd == "much less capable" & ukhls$wave == 2) | 
                  (ukhls$c_scghqd == "much less capable" & ukhls$wave == 3)] <- 4 # Much less capable

# E: Constantly under strain
ukhls$strain <- 0 # Missing
ukhls$strain[(ukhls$b_scghqe == "much more than usual" & ukhls$wave == 2) | 
              (ukhls$c_scghqe == "much more than usual" & ukhls$wave == 3)] <- 1 # Much more than usual
ukhls$strain[(ukhls$b_scghqe == "rather more than usual" & ukhls$wave == 2) | 
              (ukhls$c_scghqe == "rather more than usual" & ukhls$wave == 3)] <- 2 # Rather more than usual
ukhls$strain[(ukhls$b_scghqe == "no more than usual" & ukhls$wave == 2) | 
              (ukhls$c_scghqe == "no more than usual" & ukhls$wave == 3)] <- 3 # No more than usual
ukhls$strain[(ukhls$b_scghqe == "not at all" & ukhls$wave == 2) | 
              (ukhls$c_scghqe == "not at all" & ukhls$wave == 3)] <- 4 # Not at all

# F: Problem overcoming difficulties
ukhls$overcoming <- 0 # Missing
ukhls$overcoming[(ukhls$b_scghqf == "much more than usual" & ukhls$wave == 2) | 
               (ukhls$c_scghqf == "much more than usual" & ukhls$wave == 3)] <- 1 # Much more than usual
ukhls$overcoming[(ukhls$b_scghqf == "rather more than usual" & ukhls$wave == 2) | 
               (ukhls$c_scghqf == "rather more than usual" & ukhls$wave == 3)] <- 2 # Rather more than usual
ukhls$overcoming[(ukhls$b_scghqf == "no more than usual" & ukhls$wave == 2) | 
               (ukhls$c_scghqf == "no more than usual" & ukhls$wave == 3)] <- 3 # No more than usual
ukhls$overcoming[(ukhls$b_scghqf == "not at all" & ukhls$wave == 2) | 
               (ukhls$c_scghqf == "not at all" & ukhls$wave == 3)] <- 4 # Not at all

# G: Enjoy day-to-day activities
ukhls$enjoy_act <- 0 # Missing
ukhls$enjoy_act[(ukhls$b_scghqg == "more than usual" & ukhls$wave == 2) | 
               (ukhls$c_scghqg == "more than usual" & ukhls$wave == 3)] <- 1 # More than usual
ukhls$enjoy_act[(ukhls$b_scghqg == "same as usual" & ukhls$wave == 2) | 
               (ukhls$c_scghqg == "same as usual" & ukhls$wave == 3)] <- 2 # Same as usual
ukhls$enjoy_act[(ukhls$b_scghqg == "less so that usual" & ukhls$wave == 2) | 
               (ukhls$c_scghqg == "less so than usual" & ukhls$wave == 3)] <- 3 # Less than usual
ukhls$enjoy_act[(ukhls$b_scghqg == "much less than usual" & ukhls$wave == 2) | 
               (ukhls$c_scghqg == "much less than usual" & ukhls$wave == 3)] <- 4 # Much less than usual

# H: Ability to face problems
ukhls$face_prob <- 0 # Missing
ukhls$face_prob[(ukhls$b_scghqh == "more so than usual" & ukhls$wave == 2) | 
                  (ukhls$c_scghqh == "more so than usual" & ukhls$wave == 3)] <- 1 # More than usual
ukhls$face_prob[(ukhls$b_scghqh == "same as usual" & ukhls$wave == 2) | 
                  (ukhls$c_scghqh == "same as usual" & ukhls$wave == 3)] <- 2 # Same as usual
ukhls$face_prob[(ukhls$b_scghqh == "less able than usual" & ukhls$wave == 2) | 
                  (ukhls$c_scghqh == "less able than usual" & ukhls$wave == 3)] <- 3 # Less than usual
ukhls$face_prob[(ukhls$b_scghqh == "much less able" & ukhls$wave == 2) | 
                  (ukhls$c_scghqh == "much less able" & ukhls$wave == 3)] <- 4 # Much less than usual

# I: Unhappy or depressed
ukhls$depressed <- 0 # Missing
ukhls$depressed[(ukhls$b_scghqi == "much more than usual" & ukhls$wave == 2) | 
                  (ukhls$c_scghqi == "much more than usual" & ukhls$wave == 3)] <- 1 # Much more than usual
ukhls$depressed[(ukhls$b_scghqi == "rather more than usual" & ukhls$wave == 2) | 
                  (ukhls$c_scghqi == "rather more than usual" & ukhls$wave == 3)] <- 2 # Rather more than usual
ukhls$depressed[(ukhls$b_scghqi == "no more than usual" & ukhls$wave == 2) | 
                  (ukhls$c_scghqi == "no more than usual" & ukhls$wave == 3)] <- 3 # No more than usual
ukhls$depressed[(ukhls$b_scghqi == "not al all" & ukhls$wave == 2) | 
                  (ukhls$c_scghqi == "not al all" & ukhls$wave == 3)] <- 4 # Not at all

# J: Unhappy or depressed
ukhls$confidence <- 0 # Missing
ukhls$confidence[(ukhls$b_scghqj == "much more than usual" & ukhls$wave == 2) | 
                  (ukhls$c_scghqj == "much more than usual" & ukhls$wave == 3)] <- 1 # Much more than usual
ukhls$confidence[(ukhls$b_scghqj == "rather more than usual" & ukhls$wave == 2) | 
                  (ukhls$c_scghqj == "rather more than usual" & ukhls$wave == 3)] <- 2 # Rather more than usual
ukhls$confidence[(ukhls$b_scghqj == "no more than usual" & ukhls$wave == 2) | 
                  (ukhls$c_scghqj == "no more than usual" & ukhls$wave == 3)] <- 3 # No more than usual
ukhls$confidence[(ukhls$b_scghqj == "not al all" & ukhls$wave == 2) | 
                  (ukhls$c_scghqj == "not al all" & ukhls$wave == 3)] <- 4 # Not at all

# K: Believe in self-worth
ukhls$self_worth <- 0 # Missing
ukhls$self_worth[(ukhls$b_scghqk == "much more than usual" & ukhls$wave == 2) | 
                   (ukhls$c_scghqk == "much more than usual" & ukhls$wave == 3)] <- 1 # Much more than usual
ukhls$self_worth[(ukhls$b_scghqk == "rather more than usual" & ukhls$wave == 2) | 
                   (ukhls$c_scghqk == "rather more than usual" & ukhls$wave == 3)] <- 2 # Rather more than usual
ukhls$self_worth[(ukhls$b_scghqk == "no more than usual" & ukhls$wave == 2) | 
                   (ukhls$c_scghqk == "no more than usual" & ukhls$wave == 3)] <- 3 # No more than usual
ukhls$self_worth[(ukhls$b_scghqk == "not al all" & ukhls$wave == 2) | 
                   (ukhls$c_scghqk == "not al all" & ukhls$wave == 3)] <- 4 # Not at all

# L: General happiness
ukhls$happiness <- 0 # Missing
ukhls$happiness[(ukhls$b_scghql == "more so than usual" & ukhls$wave == 2) | 
                  (ukhls$c_scghql == "more so than usual" & ukhls$wave == 3)] <- 1 # More than usual
ukhls$happiness[(ukhls$b_scghql == "about the same as usual" & ukhls$wave == 2) | 
                  (ukhls$c_scghql == "about the same as usual" & ukhls$wave == 3)] <- 2 # Same as usual
ukhls$happiness[(ukhls$b_scghql == "less so than usual" & ukhls$wave == 2) | 
                  (ukhls$c_scghql == "less so than usual" & ukhls$wave == 3)] <- 3 # Less than usual
ukhls$happiness[(ukhls$b_scghql == "much less than usual" & ukhls$wave == 2) | 
                  (ukhls$c_scghql == "much less than usual" & ukhls$wave == 3)] <- 4 # Much less than usual

## 4. Biomarker ##

biomarker <- c("chol", "hdl", "trig", "hba1c", "hgb", "rtin", "testo_m", "igfi", "dheas",
               "hscrp", "uscmg", "uscmm", "cfib", "ecre", "ure", "alb", "alkp", "alt", "ast", "ggt")

# Circulatory #

biomarker_circ <- c("chol", "hdl", "trig", "hba1c", "hgb", "rtin")

# Total cholesertol
ukhls$chol[ukhls$chol == "top 1%: 8.399999618530273-12.5"] <- 8.399999618530273
ukhls$chol[ukhls$chol == "bottom 0.5%: 2-2.900000095367432"] <- 2.900000095367432
ukhls$chol[ukhls$chol == ""] <- 0
ukhls$chol <- as.numeric(ukhls$chol)

# HDL cholesertol
ukhls$hdl[ukhls$hdl == "top 1%: 2.900000095367432-5.2"] <- 2.900000095367432
ukhls$hdl[ukhls$hdl == "bottom 0.5%: .4-.699999988079071"] <- 0.699999988079071
ukhls$hdl[ukhls$hdl == ""] <- 0
ukhls$hdl <- as.numeric(ukhls$hdl)

# Triglycerides
ukhls$trig[ukhls$trig == "top 1%: 6.099999904632568-15.3"] <- 6.099999904632568
ukhls$trig[ukhls$trig == "bottom 0.5%: .3-.5"] <- 0.5
ukhls$trig[ukhls$trig == ""] <- 0
ukhls$trig <- as.numeric(ukhls$trig)

# Glycated Haemoglobin
ukhls$hba1c[ukhls$hba1c == "top 1%: 73-133"] <- 73
ukhls$hba1c[ukhls$hba1c == "bottom 0.5%: 15-26"] <- 26
ukhls$hba1c[ukhls$hba1c == ""] <- 0
ukhls$hba1c <- as.numeric(ukhls$hba1c)

# Haemoglobin
ukhls$hgb[ukhls$hgb == "top 0.5%: 171-220"] <- 171
ukhls$hgb[ukhls$hgb == "bottom 0.5%: 2-97"] <- 97
ukhls$hgb[ukhls$hgb == ""] <- 0
ukhls$hgb <- as.numeric(ukhls$hgb)

# Ferritin
ukhls$rtin[ukhls$rtin == "90%-92.5%: 275-313"] <- 294 # take midpoint
ukhls$rtin[ukhls$rtin == "92.5%-95%: 313-370"] <- 314.5
ukhls$rtin[ukhls$rtin == "95%-97.5%: 370-480"] <- 425
ukhls$rtin[ukhls$rtin == "top 2.5%: 480-9779"] <- 480
ukhls$rtin[ukhls$rtin == "bottom 1%: 3-8"] <- 8
ukhls$rtin[ukhls$rtin == ""] <- 0
ukhls$rtin <- as.numeric(ukhls$rtin)

# Hormones #

biomarker_horm <- c("testo_m", "igfi", "dheas")

# Testosterone
ukhls$testo_m[ukhls$testo_m == "bottom 1%: 1-4.099999904632568"] <- 4.099999904632568
ukhls$testo_m[ukhls$testo_m == "1%-2%: 4.099999904632568-5.199999809265137"] <- 4.65
ukhls$testo_m[ukhls$testo_m == "2%-3%: 5.199999809265137-6"] <- 5.6
ukhls$testo_m[ukhls$testo_m == "94%-95%: 24.89999961853027-25.70000076293945"] <- 25.3
ukhls$testo_m[ukhls$testo_m == "95%-96%: 25.70000076293945-26.70000076293945"] <- 26.2
ukhls$testo_m[ukhls$testo_m == "96%-97%: 26.70000076293945-27.70000076293945"] <- 27.2
ukhls$testo_m[ukhls$testo_m == "97%-98%: 27.70000076293945-29"] <- 28.3
ukhls$testo_m[ukhls$testo_m == "98%-99%: 29-31.39999961853027"] <- 30.2
ukhls$testo_m[ukhls$testo_m == "top 1%: 31.39999961853027-40.09999847412109"] <- 31.39999961853027
ukhls$testo_m[ukhls$testo_m == ""] <- 0
ukhls$testo_m <- as.numeric(ukhls$testo_m)

# Insulin-like-growth factor 1
ukhls$igfi[ukhls$igfi == "top 1%: 44-78"] <- 44
ukhls$igfi[ukhls$igfi == "bottom 0.5%: 2-6"] <- 6
ukhls$igfi[ukhls$igfi == ""] <- 0
ukhls$igfi <- as.numeric(ukhls$igfi)

# Dihydroepiandrosterone sulphate
ukhls$dheas[ukhls$dheas == "98%-99%: 13.10000038146973-15.10000038146973"] <- 12.1
ukhls$dheas[ukhls$dheas == "top 1%: 15.10000038146973-25.3"] <- 15.10000038146973
ukhls$dheas[ukhls$dheas == "bottom 0.5%: .1-.300000011920929"] <- 0.300000011920929
ukhls$dheas[ukhls$dheas == ""] <- 0
ukhls$dheas <- as.numeric(ukhls$dheas)

# Inflammation #

biomarker_infl <- c("hscrp", "uscmg", "uscmm", "cfib")

# C-reactive protein
ukhls$hscrp[ukhls$hscrp == "98%-99%: 20-32"] <- 26
ukhls$hscrp[ukhls$hscrp == "top 1%: 32-228"] <- 32
ukhls$hscrp[ukhls$hscrp == ""] <- 0
ukhls$hscrp <- as.numeric(ukhls$hscrp)

# Cytomegalovirus IgG
ukhls$uscmg[ukhls$uscmg == "detected"] <- 1
ukhls$uscmg[ukhls$uscmg == "indeterminate"] <- 2
ukhls$uscmg[ukhls$uscmg == "not detected"] <- 3
ukhls$uscmg[ukhls$uscmg == ""] <- 0
ukhls$uscmg <- as.numeric(ukhls$uscmg)

# Cytomegalovirus IgM
ukhls$uscmm[ukhls$uscmm == "detected"] <- 1
ukhls$uscmm[ukhls$uscmm == "indeterminate"] <- 2
ukhls$uscmm[ukhls$uscmm == "not detected"] <- 3
ukhls$uscmm[ukhls$uscmm == ""] <- 0
ukhls$uscmm <- as.numeric(ukhls$uscmm)

# Clauss fibrinogen
ukhls$cfib[ukhls$cfib == "bottom 0.5%: .4-1.5"] <- 1.5
ukhls$cfib[ukhls$cfib == "top 0.5%: 4.800000190734863-5.7"] <- 4.800000190734863
ukhls$cfib[ukhls$cfib == ""] <- 0
ukhls$cfib <- as.numeric(ukhls$cfib)

# Kidney #

biomarker_kidn <- c("ecre", "ure")

# Creatinine
ukhls$ecre[ukhls$ecre == "98%-99%: 122-137"] <- 129.5
ukhls$ecre[ukhls$ecre == "top 1%: 137-352"] <- 137
ukhls$ecre[ukhls$ecre == "bottom 0.5%: 28-45"] <- 45
ukhls$ecre[ukhls$ecre == ""] <- 0
ukhls$ecre <- as.numeric(ukhls$ecre)

# Urea
ukhls$ure[ukhls$ure == "98%-99%: 10.39999961853027-11.5"] <- 10.95
ukhls$ure[ukhls$ure == "top 1%: 11.5-27.5"] <- 11.5
ukhls$ure[ukhls$ure == "bottom 0.5%: 1.2-2.900000095367432"] <- 2.900000095367432
ukhls$ure[ukhls$ure == ""] <- 0
ukhls$ure <- as.numeric(ukhls$ure)

# Liver #

biomarker_liver <- c("alb", "alkp", "alt", "ast", "ggt")

# Albumin
ukhls$alb[ukhls$alb == "bottom 0.5%: 21-39"] <- 39
ukhls$alb[ukhls$alb == "top 0.5%: 54-65"] <- 54
ukhls$alb[ukhls$alb == ""] <- 0
ukhls$alb <- as.numeric(ukhls$alb)

# Alkaline phosphatase
ukhls$alkp[ukhls$alkp == "98%-99%: 124-140"] <- 132
ukhls$alkp[ukhls$alkp == "top 1%: 140-824"] <- 140
ukhls$alkp[ukhls$alkp == "bottom 0.5%: 11-31"] <- 31
ukhls$alkp[ukhls$alkp == ""] <- 0
ukhls$alkp <- as.numeric(ukhls$alkp)

# Alanine transaminase
ukhls$alt[ukhls$alt == "bottom 0.5%: 4-9"] <- 9
ukhls$alt[ukhls$alt == "top 1%: 87-1769"] <- 87
ukhls$alt[ukhls$alt == ""] <- 0
ukhls$alt <- as.numeric(ukhls$alt)

# Aspartate transaminase
ukhls$ast[ukhls$ast == "bottom 0.5%: 10-17"] <- 17
ukhls$ast[ukhls$ast == "top 1%: 67-2312"] <- 67
ukhls$ast[ukhls$ast == ""] <- 0
ukhls$ast <- as.numeric(ukhls$ast)

# Gamma glutamyl transferase
ukhls$ggt[ukhls$ggt == "95%-97.5%: 87-125"] <- 106
ukhls$ggt[ukhls$ggt == "top 2.5%: 125-1304"] <- 125
ukhls$ggt[ukhls$ggt == "bottom 1%: 5-6"] <- 6
ukhls$ggt[ukhls$ggt == ""] <- 0
ukhls$ggt <- as.numeric(ukhls$ggt)

## 5. Genetic ##

# Join together into single file
# Currnetly this does not work as does not have enough memory (need to fix)

# # Create initial object for chromosome 1 to join others onto in order
# ukhls_genetic  <- fread("../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr1_sendout/Green_Genotype_chr1_sendout.raw", header=T)
# 
# filename <- c("../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr2_to_chr3_sendout/Green_Genotype_chr2_sendout.raw", 
#               "../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr2_to_chr3_sendout/Green_Genotype_chr3_sendout.raw", 
#               "../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr4_to_chr7_sendout/Green_Genotype_chr4_sendout.raw",
#               "../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr4_to_chr7_sendout/Green_Genotype_chr5_sendout.raw",
#               "../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr4_to_chr7_sendout/Green_Genotype_chr6_sendout.raw",
#               "../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr4_to_chr7_sendout/Green_Genotype_chr7_sendout.raw",
#               "../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr8_to_chr11_sendout/Green_Genotype_chr8_sendout.raw",
#               "../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr8_to_chr11_sendout/Green_Genotype_chr9_sendout.raw",
#               "../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr8_to_chr11_sendout/Green_Genotype_chr10_sendout.raw",
#               "../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr8_to_chr11_sendout/Green_Genotype_chr11_sendout.raw",
#               "../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr12_chr16_sendout/Green_Genotype_chr13_sendout.raw",
#               "../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr12_chr16_sendout/Green_Genotype_chr14_sendout.raw",
#               "../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr12_chr16_sendout/Green_Genotype_chr15_sendout.raw",
#               "../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr12_chr16_sendout/Green_Genotype_chr16_sendout.raw",
#               "../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr17_to_chr22_sendout/Green_Genotype_chr17_sendout.raw",
#               "../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr17_to_chr22_sendout/Green_Genotype_chr18_sendout.raw",
#               "../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr17_to_chr22_sendout/Green_Genotype_chr19_sendout.raw",
#               "../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr17_to_chr22_sendout/Green_Genotype_chr20_sendout.raw",
#               "../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr17_to_chr22_sendout/Green_Genotype_chr21_sendout.raw",
#               "../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr17_to_chr22_sendout/Green_Genotype_chr22_sendout.raw")
# 
# for (i in 1:10){
#   
#   print(i) # To show progress
#   
#   data <- fread(paste0(filename[i]), header=T) # Load data for a chromosome
#   data <- data[,-c("FID", "MAT", "PAT", "SEX", "PHENOTYPE")] # Variables not required (stored in chr1)
#   
#   ukhls_genetic <- merge(ukhls_genetic, data, by = "IID", all = TRUE) # Merge together
#   rm(data) # Delete the original data object
#   
# }

## 6. Outcomes ## 

# 6.1 Long-standing illness or disability #
# Calculate health at baseline (i.e. same wave data collected at)
ukhls$health_0yr[ukhls$wave == 2 & ukhls$b_health == "yes"] <- 1 # Yes
ukhls$health_0yr[ukhls$wave == 3 & ukhls$c_health == "yes"] <- 1
ukhls$health_0yr[ukhls$wave == 2 & ukhls$b_health == "no"] <- 0 # No 
ukhls$health_0yr[ukhls$wave == 3 & ukhls$c_health == "no"] <- 0
ukhls$health_0yr[ukhls$wave == 2 & ukhls$b_health == ""] <- NA # Missing 
ukhls$health_0yr[ukhls$wave == 3 & ukhls$c_health == ""] <- NA

# 1 year since baseline do have ill health?
ukhls$health_1yr <- 0 
ukhls$health_1yr[ukhls$wave == 2 & ukhls$c_health ==  "yes"] <- 1
ukhls$health_1yr[ukhls$wave == 3 & ukhls$d_health ==  "yes"] <- 1
ukhls$health_1yr[ukhls$wave == 2 & ukhls$c_health ==  ""] <- NA
ukhls$health_1yr[ukhls$wave == 3 & ukhls$d_health ==  ""] <- NA

# 2 years since baseline have reported ill health
ukhls$health_2yr <- 0 
ukhls$health_2yr[ukhls$wave == 2 & ukhls$d_health ==  "yes"] <- 1
ukhls$health_2yr[ukhls$wave == 3 & ukhls$e_health ==  "yes"] <- 1
ukhls$health_2yr[ukhls$wave == 2 & ukhls$d_health ==  ""] <- NA
ukhls$health_2yr[ukhls$wave == 3 & ukhls$e_health ==  ""] <- NA

# 3 years since baseline have reported ill health
ukhls$health_3yr <- 0 
ukhls$health_3yr[ukhls$wave == 2 & ukhls$e_health ==  "yes"] <- 1
ukhls$health_3yr[ukhls$wave == 3 & ukhls$f_health ==  "yes"] <- 1
ukhls$health_3yr[ukhls$wave == 2 & ukhls$e_health ==  ""] <- NA
ukhls$health_3yr[ukhls$wave == 3 & ukhls$f_health ==  ""] <- NA

# 4 years since baseline have reported ill health
ukhls$health_4yr <- 0 
ukhls$health_4yr[ukhls$wave == 2 & ukhls$f_health ==  "yes"] <- 1
ukhls$health_4yr[ukhls$wave == 3 & ukhls$g_health ==  "Yes"] <- 1
ukhls$health_4yr[ukhls$wave == 2 & ukhls$f_health ==  ""] <- NA
ukhls$health_4yr[ukhls$wave == 3 & ukhls$g_health ==  ""] <- NA

# 5 years since baseline have reported ill health (wave b only)
ukhls$health_5yr <- NA
ukhls$health_5yr[ukhls$wave == 2 & ukhls$g_health ==  "Yes"] <- 1
ukhls$health_5yr[ukhls$wave == 2 & ukhls$g_health ==  "No"] <- 0
ukhls$health_5yr[ukhls$wave == 2 & ukhls$f_health ==  ""] <- NA


# Examine the trend over time
# trend <- as.data.frame(table(ukhls$health_0yr, ukhls$wave))
# trend <- rbind(trend, as.data.frame(table(ukhls$health_1yr, ukhls$wave)))
# trend <- rbind(trend, as.data.frame(table(ukhls$health_2yr, ukhls$wave)))
# trend <- rbind(trend, as.data.frame(table(ukhls$health_3yr, ukhls$wave)))
# trend <- rbind(trend, as.data.frame(table(ukhls$health_4yr, ukhls$wave)))
# trend <- rbind(trend, as.data.frame(table(ukhls$health_5yr, ukhls$wave)))
# trend$since_baseline <- c(0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5)
# trend$health <- trend$Var1
# trend$wave <- trend$Var2
# 
# library(ggplot2)
# ggplot(trend[trend$health == 1,], aes(x = since_baseline, y = Freq, color = wave)) +
#   geom_line() + ylim(0,3000)

# 6.2 Self-rated health #

# Variable is coded as excellent, very good, good, fair or poor - recode as fair/poor or not

# Calculate health at baseline (i.e. same wave data collected at)
ukhls$srh_0yr[ukhls$wave == 2 & (ukhls$b_sf1 == "poor" | ukhls$b_sf1 == "fair")] <- 1 # Poor/Fair
ukhls$srh_0yr[ukhls$wave == 3 & (ukhls$c_sf1 == "poor" | ukhls$c_sf1 == "fair")] <- 1
ukhls$srh_0yr[ukhls$wave == 2 & (ukhls$b_sf1 == "excellent" | ukhls$b_sf1 == "very good" | ukhls$b_sf1 == "good")] <- 0 # Excellent to good 
ukhls$srh_0yr[ukhls$wave == 3 & (ukhls$c_sf1 == "excellent" | ukhls$c_sf1 == "very good" | ukhls$c_sf1 == "good")] <- 0
ukhls$srh_0yr[ukhls$wave == 2 & ukhls$b_sf1 == ""] <- NA # Missing
ukhls$srh_0yr[ukhls$wave == 3 & ukhls$c_sf1 == ""] <- NA
ukhls$srh_0yr <- as.numeric(ukhls$srh_0yr) # Convert to numeric

# 1 year since baseline do have ill health?
ukhls$srh_1yr[ukhls$wave == 2 & (ukhls$c_sf1 == "poor" | ukhls$c_sf1 == "fair")] <- 1 # Poor/Fair
ukhls$srh_1yr[ukhls$wave == 3 & (ukhls$d_sf1 == "poor" | ukhls$d_sf1 == "fair")] <- 1
ukhls$srh_1yr[ukhls$wave == 2 & (ukhls$c_sf1 == "excellent" | ukhls$c_sf1 == "very good" | ukhls$c_sf1 == "good")] <- 0 # Excellent to good 
ukhls$srh_1yr[ukhls$wave == 3 & (ukhls$d_sf1 == "excellent" | ukhls$d_sf1 == "very good" | ukhls$d_sf1 == "good")] <- 0
ukhls$srh_1yr[ukhls$wave == 2 & ukhls$c_sf1 == ""] <- NA # Missing
ukhls$srh_1yr[ukhls$wave == 3 & ukhls$d_sf1 == ""] <- NA
ukhls$srh_1yr <- as.numeric(ukhls$srh_1yr) # Convert to numeric

# 2 years since baseline have reported ill health at some point
ukhls$srh_2yr[ukhls$wave == 2 & (ukhls$d_sf1 == "poor" | ukhls$d_sf1 == "fair" | ukhls$srh_1yr == 1)] <- 1 # Poor/Fair
ukhls$srh_2yr[ukhls$wave == 3 & (ukhls$e_sf1 == "poor" | ukhls$e_sf1 == "fair" | ukhls$srh_1yr == 1)] <- 1
ukhls$srh_2yr[ukhls$wave == 2 & (ukhls$d_sf1 == "excellent" | ukhls$d_sf1 == "very good" | ukhls$d_sf1 == "good")] <- 0 # Excellent to good 
ukhls$srh_2yr[ukhls$wave == 3 & (ukhls$e_sf1 == "excellent" | ukhls$e_sf1 == "very good" | ukhls$e_sf1 == "good")] <- 0
ukhls$srh_2yr[ukhls$wave == 2 & ukhls$d_sf1 == ""] <- NA # Missing
ukhls$srh_2yr[ukhls$wave == 3 & ukhls$e_sf1 == ""] <- NA
ukhls$srh_2yr <- as.numeric(ukhls$srh_2yr) # Convert to numeric

# 3 years since baseline have reported ill health
ukhls$srh_3yr[ukhls$wave == 2 & (ukhls$e_sf1 == "poor" | ukhls$e_sf1 == "fair" | ukhls$srh_2yr == 1)] <- 1 # Poor/Fair
ukhls$srh_3yr[ukhls$wave == 3 & (ukhls$f_sf1 == "poor" | ukhls$f_sf1 == "fair" | ukhls$srh_2yr == 1)] <- 1
ukhls$srh_3yr[ukhls$wave == 2 & (ukhls$e_sf1 == "excellent" | ukhls$e_sf1 == "very good" | ukhls$e_sf1 == "good")] <- 0 # Excellent to good 
ukhls$srh_3yr[ukhls$wave == 3 & (ukhls$f_sf1 == "excellent" | ukhls$f_sf1 == "very good" | ukhls$f_sf1 == "good")] <- 0
ukhls$srh_3yr[ukhls$wave == 2 & ukhls$e_sf1 == ""] <- NA # Missing
ukhls$srh_3yr[ukhls$wave == 3 & ukhls$f_sf1 == ""] <- NA
ukhls$srh_3yr <- as.numeric(ukhls$srh_3yr) # Convert to numeric

# 4 years since baseline have reported ill health
ukhls$srh_4yr[ukhls$wave == 2 & (ukhls$f_sf1 == "poor" | ukhls$f_sf1 == "fair" | ukhls$srh_3yr == 1)] <- 1 # Poor/Fair
ukhls$srh_4yr[ukhls$wave == 3 & (ukhls$g_sf1 == "poor" | ukhls$g_sf1 == "fair" | ukhls$srh_3yr == 1)] <- 1
ukhls$srh_4yr[ukhls$wave == 2 & (ukhls$f_sf1 == "excellent" | ukhls$f_sf1 == "very good" | ukhls$f_sf1 == "good")] <- 0 # Excellent to good 
ukhls$srh_4yr[ukhls$wave == 3 & (ukhls$g_sf1 == "excellent" | ukhls$g_sf1 == "very good" | ukhls$g_sf1 == "good")] <- 0
ukhls$srh_4yr[ukhls$wave == 2 & ukhls$f_sf1 == ""] <- NA # Missing
ukhls$srh_4yr[ukhls$wave == 3 & ukhls$g_sf1 == ""] <- NA
ukhls$srh_4yr <- as.numeric(ukhls$srh_4yr) # Convert to numeric

# 5 years since baseline have reported ill health
ukhls$srh_5yr[ukhls$wave == 2 & (ukhls$g_sf1 == "poor" | ukhls$g_sf1 == "fair" | ukhls$srh_3yr == 1)] <- 1 # Poor/Fair
ukhls$srh_5yr[ukhls$wave == 2 & (ukhls$g_sf1 == "excellent" | ukhls$g_sf1 == "very good" | ukhls$g_sf1 == "good")] <- 0 # Excellent to good
ukhls$srh_5yr[ukhls$wave == 2 & ukhls$g_sf1 == ""] <- NA
ukhls$srh_5yr[ukhls$wave == 3] <- NA # Missing
ukhls$srh_5yr <- as.numeric(ukhls$srh_5yr) # Convert to numeric

# # Examine the trend over time
# trend <- as.data.frame(table(ukhls$srh_0yr, ukhls$wave))
# trend <- rbind(trend, as.data.frame(table(ukhls$srh_1yr, ukhls$wave)))
# trend <- rbind(trend, as.data.frame(table(ukhls$srh_2yr, ukhls$wave)))
# trend <- rbind(trend, as.data.frame(table(ukhls$srh_3yr, ukhls$wave)))
# trend <- rbind(trend, as.data.frame(table(ukhls$srh_4yr, ukhls$wave)))
# trend <- rbind(trend, as.data.frame(table(ukhls$srh_5yr, ukhls$wave)))
# trend$since_baseline <- c(0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5)
# trend$health <- trend$Var1
# trend$wave <- trend$Var2
# 
# library(ggplot2)
# ggplot(trend[trend$health == 1,], aes(x = since_baseline, y = Freq, color = wave)) +
#   geom_line() + ylim(0,3000)

## NB - this variable collapses after 4 years (waves g and f which have 9848 and 9841 missing respectively) - same for wave 3 people 3 years later. Suggest not using

# Health conditions
# 1	Asthma, 2	Arthritis, 3	Congestive heart failure, 4	Coronary heart disease, 5	Angina, 
# 6	Heart attack or myocardial infarction, 7	Stroke, 8	Emphysema, 9	Hyperthyroidism or an over-active thyroid, 
# 10	Hypothyroidism or an under-active thyroid, 11	Chronic bronchitis, 12	Any kind of liver condition, 
# 13	Cancer or malignancy, 14	Diabetes, 15	Epilepsy, 16	High blood pressure, 17	Clinical depression, 
# 96	None of these
# Select only those most prevalent conditions

# 6.3 Hypertension #
ukhls$hcondn16[ukhls$hcondn16 == "mentioned"] <- 1 # Yes
ukhls$hcondn16[ukhls$hcondn16 == "not mentioned"] <- 0 # No
ukhls$hcondn16[ukhls$hcondn16 == ""] <- NA # Missing
ukhls$hcondn16 <- as.numeric(ukhls$hcondn16) # Convert to numeric

# 6.4 Arthritis #
ukhls$hcondn2[ukhls$hcondn2 == "mentioned"] <- 1 # Yes
ukhls$hcondn2[ukhls$hcondn2 == "not mentioned"] <- 0 # No
ukhls$hcondn2[ukhls$hcondn2 == ""] <- NA # Missing
ukhls$hcondn2 <- as.numeric(ukhls$hcondn2) # Convert to numeric

# 6.5 No conditions #
# ukhls$hcondn96 # no changes required

# 6.6 Type of impairment or disability #

# Prevalence at baseline
# 1 mobility, 2 lifting/moving objects, 3 manual dexterity, 4 continence, 5 hearing, 
# 6 sight, 7 communication/speech problems, 8 memory/ability to concentrate/learn/understand, 
# 9 recognising in physical danger, 10 physical coordination e.g. balance, 11 difficulties with own personal care, 
# 12 other problem/disability, 96 none of these

# Large amount of missing data here (>6000 for each wave)


### Save file ###
write.csv(ukhls, "../../../../Desktop/Green_UKHLS/ukhls_cleaned.csv")
rm(list=ls(all=TRUE))
gc()

