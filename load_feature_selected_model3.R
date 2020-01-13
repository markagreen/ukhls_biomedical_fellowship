##################################
##### Load features selected #####
############ Model 3 #############
##################################

lasso_personal_vars <- c("sex", "age", "macob", "marstat")

lasso_ses_vars <- c("hhsize", "numbeds", "hhown", "hhval", "edu", "prb_hhold", "benefit1", "benefit2", "benefit3", "benefit5",
                    "benefit6", "benefit96", "subj_fin", "gross_pay", "b_ppen", "b_save", "jbperm", "jbsoc00", "manager", "jbot", "jbsat",
                    "jb2", "b_jbsec", "b_wkends", "b_depenth4", "b_depenth5", "maedqf", "maju", "pasoc00_nb",
                    "b_matdepa", "b_matdepb", "b_matdepd", "b_matdepe", "tumble", "caruse", "mobuse", 
                    "b_volun", "b_chargv", "sprt_party", "party_vote", "party_close")

lasso_health_vars <- c("hcond2", "hcond13", "hcond14", "hcond_cvd", "hcond16", "hcond_oth",  
                       "food_spend", "fdout_spend", "alcohol_spend", "b_usdairy", "b_wkfruit", "b_wkvege", "b_fruvege", 
                       "b_wlk10m", "b_wlk30min", "b_walkpace", "b_wkphys", "b_sportsfreq", "b_sports3freq", 
                       "b_ncigs", "b_aglquit", "htval", "wtval", "bfpcval", "wstval", "bmival", 
                       "mmgsdval", "mmgsnval", "lung_cap_fvc", "lung_cap_fev", "lung_cap_ratio", "omdiaval", "concentrate", "sleep",
                       "useful", "decisions", "strain", "overcoming", "enjoy_act", "face_prob", "depressed", 
                       "confidence", "self_worth", "happiness")

lasso_biom_vars <- c("chol", "hdl", "trig", "hba1c", "hgb", "igfi", "dheas", "hscrp", "uscmg", "uscmm",
                     "cfib", "ecre", "alb", "alkp", "alt", "ggt")

pca_gen_vars <- c("PCA1", "PCA2", "PCA3", "PCA4", "PCA5", "PCA6", "PCA7", "PCA8", "PCA9", "PCA10")

lasso_all_vars <- c("sex", "age", "macob", "marstat", # Personal
                    "hhsize", "numbeds", "hhown", "hhval", "edu", "prb_hhold", "benefit1", "benefit2", "benefit3", "benefit5",
                    "benefit6", "benefit96", "subj_fin", "gross_pay", "b_ppen", "b_save", "jbperm", "jbsoc00", "manager", "jbot", "jbsat",
                    "jb2", "b_jbsec", "b_wkends", "b_depenth4", "b_depenth5", "maedqf", "maju", "pasoc00_nb",
                    "b_matdepa", "b_matdepb", "b_matdepd", "b_matdepe", "tumble", "caruse", "mobuse", 
                    "b_volun", "b_chargv", "sprt_party", "party_vote", "party_close",
                    "hcond2", "hcond13", "hcond14", "hcond_cvd", "hcond16", "hcond_oth", # Health 
                    "food_spend", "fdout_spend", "alcohol_spend", "b_usdairy", "b_wkfruit", "b_wkvege", "b_fruvege", 
                    "b_wlk10m", "b_wlk30min", "b_walkpace", "b_wkphys", "b_sportsfreq", "b_sports3freq", 
                    "b_ncigs", "b_aglquit", "htval", "wtval", "bfpcval", "wstval", "bmival", 
                    "mmgsdval", "mmgsnval", "lung_cap_fvc", "lung_cap_fev", "lung_cap_ratio", "omdiaval", "concentrate", "sleep",
                    "useful", "decisions", "strain", "overcoming", "enjoy_act", "face_prob", "depressed", 
                    "confidence", "self_worth", "happiness",
                    "chol", "hdl", "trig", "hba1c", "hgb", "igfi", "dheas", "hscrp", "uscmg", "uscmm", # Biomarker
                    "cfib", "ecre", "alb", "alkp", "alt", "ggt",
                    "PCA1", "PCA2", "PCA3", "PCA4", "PCA5", "PCA6", "PCA7", "PCA8", "PCA9", "PCA10") # Genetic