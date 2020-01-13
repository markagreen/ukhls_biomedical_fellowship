##################################
##### Load in variable names #####
##################################

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
              "hcond_oth", "food_spend", "fdout_spend", "alcohol_spend", "b_usdairy", 
              "b_usbread", "b_wkfruit", "b_wkvege", "b_fruvege", "b_wlk10m", "b_daywlk", "b_wlk30min", 
              "b_walkpace", "b_wkphys", "b_sportsfreq", "b_sports3freq", "b_smever", "b_smnow", 
              "b_ncigs", "b_smcigs", "b_aglquit", "htval", "wtval", "bfpcval", "wstval", "bmival", 
              "mmgsdval", "mmgsnval", "lung_cap_fvc", "lung_cap_fev", "lung_cap_ratio", "lung_cap_peak",
              "omsysval", "omdiaval", "ompulval", "concentrate", "sleep", "useful", "decisions", 
              "strain", "overcoming", "enjoy_act", "face_prob", "depressed", "confidence", 
              "self_worth", "happiness",
              "chol", "hdl", "trig", "hba1c", "hgb", "rtin", "testo_m", "igfi", "dheas", # biomarker
              "hscrp", "uscmg", "uscmm", "cfib", "ecre", "ure", "alb", "alkp", "alt", "ast", "ggt",
              "PCA1", "PCA2", "PCA3", "PCA4", "PCA5", "PCA6", "PCA7", "PCA8", "PCA9", "PCA10") # Genetic

## 1. Personal ##

personal <- c("sex", "age", "ukborn", "pacob", "macob", "marstat", "times_mar", "anychild_dv")

# # 1.1 Non-modifiable #
# personal_non_mod <- c("sex", "age")
# 
# # 1.2 Cultural identity #
# personal_cult_id <- c("ukborn", "pacob", "macob")
# 
# # 1.3 Family #
# personal_fam <- c("marstat", "times_mar", "anychild_dv")

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

# ses_bc <- c("hhsize", "numbeds", "numbedrm", "hhown", "centralheat", "hhval", "edu", 
#             "prb_hhold", "prb_ctax", "benefit1", "benefit2", "benefit3", "benefit4", "benefit5", 
#             "benefit6", "benefit8", "benefit96", "benefit_other", "inc_divi", "subj_fin", "gross_pay",
#             "jbstat", "jbperm", "jbsoc00", "manager", "jbhrs", "jbot", "jbsat", "jb2", 
#             "paedqf", "maedqf", "paju", "maju", "pasoc00_nb", "masoc00_nb",
#             "sky_cable", "tumble", "dishw", "pc", "cdplayer", "pcnet", "ncars", "caruse", "mobuse",
#             "sprt_party", "party_vote", "party_close", "pol_interest")
# 
# # Housing #
# 
# ses_house <- c("hhsize", "numbeds", "numbedrm", "hhown", "centralheat", "hhval")
# 
# # Education #
# 
# ses_edu <- c("edu")
# 
# Income #
# 
# ses_inc_bc <- c("prb_hhold", "prb_ctax", "benefit1", "benefit2", "benefit3", "benefit4", "benefit5", 
#                 "benefit6", "benefit8", "benefit96", "benefit_other", "inc_divi", "subj_fin", "gross_pay")
# ses_inc <- c("prb_hhold", "prb_ctax", "benefit1", "benefit2", "benefit3", "benefit4", "benefit5", 
#              "benefit6", "benefit8", "benefit96", "benefit_other", "inc_divi", "subj_fin", "gross_pay",
#              "b_ppen", "b_save", "b_saved")
# 
# # Occupation #
# 
# # ses_occ_bc <- c("jbstat", "jbperm", "jbsoc00", "manager", "jbhrs", "jbot", "jbsat", "jb2")
# ses_occ <- c("jbstat", "jbperm", "jbsoc00", "manager", "jbhrs", "jbot", "jbsat", "jb2", "b_jbsec",
#              "b_wkends", "b_wkaut1", "b_wkaut2", "b_wkaut3", "b_wkaut4", "b_wkaut5", "b_depenth1",
#              "b_depenth2", "b_depenth3", "b_depenth4", "b_depenth5", "b_depenth6")
# 
# # Parental #
# 
# ses_par <- c("paedqf", "maedqf", "paju", "maju", "pasoc00_nb", "masoc00_nb")
# 
# # Relative poverty #
# 
# # ses_relpov_bc <- c("sky_cable", "tumble", "dishw", "pc", "cdplayer", "pcnet", "ncars", "caruse", "mobuse")
# ses_relpov <- c("b_matdepa", "b_matdepb", "b_matdepd", "b_matdepe", "b_matdepf", "b_matdepg", "b_matdeph",
#                 "sky_cable", "tumble", "dishw", "pc", "cdplayer", "pcnet", "ncars", "caruse", "mobuse")
# 
# # Social capital #
# 
# # ses_soccap_bc <- c("sprt_party", "party_vote", "party_close", "pol_interest")
# ses_soccap <- c("b_volun", "b_chargv", "sprt_party", "party_vote", "party_close", "pol_interest")

## 3. Health ##

health <- c("hcond1", "hcond2", "hcond13", "hcond17", "hcond14", "hcond_cvd", "hcond16", 
            "hcond_oth", "food_spend", "fdout_spend", "alcohol_spend", "b_usdairy", 
            "b_usbread", "b_wkfruit", "b_wkvege", "b_fruvege", "b_wlk10m", "b_daywlk", "b_wlk30min", 
            "b_walkpace", "b_wkphys", "b_sportsfreq", "b_sports3freq", "b_smever", "b_smnow", 
            "b_ncigs", "b_smcigs", "b_aglquit", "htval", "wtval", "bfpcval", "wstval", "bmival", 
            "mmgsdval", "mmgsnval", "lung_cap_fvc", "lung_cap_fev", "lung_cap_ratio", "lung_cap_peak",
            "omsysval", "omdiaval", "ompulval", "concentrate", "sleep", "useful", "decisions", 
            "strain", "overcoming", "enjoy_act", "face_prob", "depressed", "confidence", 
            "self_worth", "happiness")

# health_bc <- c("hcond1", "hcond2", "hcond13", "hcond17", "hcond14", "hcond_cvd", "hcond16", 
#                "hcond_oth", "hcond96", "food_spend", "fdout_spend", "alcohol_spend", "htval", 
#                "wtval", "bfpcval", "wstval", "bmival", "mmgsdval", "mmgsnval", "lung_cap_fvc", 
#                "lung_cap_fev", "lung_cap_ratio", "lung_cap_peak", "omsysval", "omdiaval", 
#                "ompulval", "concentrate", "sleep", "useful", "decisions", "strain", "overcoming", 
#                "enjoy_act", "face_prob", "depressed", "confidence", "self_worth", "happiness")
# 
# # 3.1 Health conditions/morbidity #
# 
# health_cond <- c("hcond1", "hcond2", "hcond13", "hcond17", "hcond14", "hcond_cvd", "hcond16", 
#                  "hcond_oth", "hcond96") # Only really for wave b
# 
# # Behaviours # 
# 
# health_behav <- c("food_spend", "fdout_spend", "alcohol_spend", "b_usdairy", "b_usbread", "b_wkfruit", 
#                   "b_wkvege", "b_fruvege", "b_wlk10m", "b_daywlk", "b_wlk30min", "b_walkpace", "b_wkphys",
#                   "b_sportsfreq", "b_sports3freq", "b_smever", "b_smnow", "b_ncigs", "b_smcigs", 
#                   "b_aglquit" )
# 
# health_behav_bc <- c("food_spend", "fdout_spend", "alcohol_spend")
# 
# # Physical functioning #
# 
# health_physfun <- c("htval", "wtval", "bfpcval", "wstval", "bmival", "mmgsdval", "mmgsnval",
#                     "lung_cap_fvc", "lung_cap_fev", "lung_cap_ratio", "lung_cap_peak",
#                     "omsysval", "omdiaval", "ompulval")
# 
# # Wellbeing #
# 
# health_wellb <- c("concentrate", "sleep", "useful", "decisions", "strain", "overcoming", "enjoy_act", 
#                   "face_prob", "depressed", "confidence", "self_worth", "happiness")

## 4. Biomarker ##

biomarker <- c("chol", "hdl", "trig", "hba1c", "hgb", "rtin", "testo_m", "igfi", "dheas",
               "hscrp", "uscmg", "uscmm", "cfib", "ecre", "ure", "alb", "alkp", "alt", "ast", "ggt")

# # Hormones #
# 
# biomarker_horm <- c("testo_m", "igfi", "dheas")
# 
# # Inflammation #
# 
# biomarker_infl <- c("hscrp", "uscmg", "uscmm", "cfib")
# 
# # Kidney #
# 
# biomarker_kidn <- c("ecre", "ure")
# 
# # Liver #
# 
# biomarker_liver <- c("alb", "alkp", "alt", "ast", "ggt")

## 5. Genetic ##

pca_gen_vars <- c("PCA1", "PCA2", "PCA3", "PCA4", "PCA5", "PCA6", "PCA7", "PCA8", "PCA9", "PCA10")