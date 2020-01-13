##################################
##### Load features selected #####
############ Model 1 #############
##################################

lasso_personal_vars <- c("age", "marstat", "times_mar")

lasso_ses_vars <- c("hhsize", "hhval", "edu", "prb_hhold", "benefit1", "benefit2", "benefit3", 
                    "benefit4", "benefit5", "benefit6", "benefit8", "benefit96", "inc_divi", 
                    "subj_fin", "gross_pay", "jbperm", "jbhrs", "jbot", "b_jbsec", "b_depenth3",
                    "b_depenth4", "maedqf", "paju", "pasoc00_nb", "masoc00_nb", "b_matdepa", 
                    "b_matdepb", "b_matdepd", "b_matdepf", "b_matdeph", "sky_cable", "tumble", 
                    "ncars", "mobuse", "b_volun", "b_chargv", "sprt_party", "party_close", "pol_interest")

lasso_health_vars <- c("hcond1", "hcond2", "hcond13", "hcond17", "hcond14", "hcond_cvd", "hcond16",
                       "hcond_oth", "food_spend", "fdout_spend", "alcohol_spend", "b_usdairy",
                       "b_usbread", "b_wkfruit", "b_wkvege", "b_fruvege",  "b_wlk10m", "b_wlk30min", "b_walkpace",
                       "b_wkphys", "b_sportsfreq", "b_smnow", "b_smcigs", "b_aglquit", "htval", "wtval",
                       "bfpcval", "wstval", "bmival", "mmgsdval", "mmgsnval", "lung_cap_fvc", "lung_cap_fev",
                       "lung_cap_ratio", "lung_cap_peak", "omdiaval", "ompulval", "concentrate", "sleep",
                       "useful", "decisions", "strain", "overcoming", "enjoy_act", "face_prob", "depressed", 
                       "confidence", "self_worth", "happiness")

lasso_biom_vars <- c("chol", "hdl", "trig", "hba1c", "hgb", "rtin", "igfi", "dheas", "hscrp", 
                     "uscmg", "uscmm", "cfib", "ecre", "alb", "alkp", "alt", "ast", "ggt")

pca_gen_vars <- c("PCA1", "PCA2", "PCA3", "PCA4", "PCA5", "PCA6", "PCA7", "PCA8", "PCA9", "PCA10")

lasso_all_vars <- c("age", "marstat", "times_mar", # Personal
                    "hhsize", "hhval", "edu", "prb_hhold", "benefit1", "benefit2", "benefit3",# Social
                    "benefit4", "benefit5", "benefit6", "benefit8", "benefit96", "inc_divi", 
                    "subj_fin", "gross_pay", "jbperm", "jbhrs", "jbot", "b_jbsec", "b_depenth3",
                    "b_depenth4", "maedqf", "paju", "pasoc00_nb", "masoc00_nb", "b_matdepa", 
                    "b_matdepb", "b_matdepd", "b_matdepf", "b_matdeph", "sky_cable", "tumble", 
                    "ncars", "mobuse", "b_volun", "b_chargv", "sprt_party", "party_close", "pol_interest",
                    "hcond1", "hcond2", "hcond13", "hcond17", "hcond14", "hcond_cvd", "hcond16", # Health
                    "hcond_oth", "food_spend", "fdout_spend", "alcohol_spend", "b_usdairy",
                    "b_usbread", "b_wkfruit", "b_wkvege", "b_fruvege",  "b_wlk10m", "b_wlk30min", "b_walkpace",
                    "b_wkphys", "b_sportsfreq", "b_smnow", "b_smcigs", "b_aglquit", "htval", "wtval",
                    "bfpcval", "wstval", "bmival", "mmgsdval", "mmgsnval", "lung_cap_fvc", "lung_cap_fev",
                    "lung_cap_ratio", "lung_cap_peak", "omdiaval", "ompulval", "concentrate", "sleep",
                    "useful", "decisions", "strain", "overcoming", "enjoy_act", "face_prob", "depressed", 
                    "confidence", "self_worth", "happiness",
                    "chol", "hdl", "trig", "hba1c", "hgb", "rtin", "igfi", "dheas", "hscrp", # Biomarkers
                    "uscmg", "uscmm", "cfib", "ecre", "alb", "alkp", "alt", "ast", "ggt",
                    "PCA1", "PCA2", "PCA3", "PCA4", "PCA5", "PCA6", "PCA7", "PCA8", "PCA9", "PCA10") # Genetic