##################################
##### Load features selected #####
############ Model 4 #############
##################################

lasso_personal_vars <- c("age", "macob", "times_mar")

lasso_ses_vars <- c("hhsize", "numbeds", "numbedrm", "hhown", "hhval", "edu", "prb_hhold",  "prb_ctax",
                    "benefit2", "benefit3", "benefit5", "benefit6", "benefit8", "benefit96", "benefit_other",
                    "subj_fin", "inc_divi", "b_save", "jbperm", "jbsoc00", "jb2", "b_jbsec", "b_wkaut1", "b_depenth4", 
                    "maedqf", "maju", "paju", "b_matdepa", "b_matdepb", "b_matdepd", "b_matdepe", "b_matdepg", 
                    "sky_cable", "tumble", "dishw", "pcnet", "ncars", "mobuse", 
                    "b_volun", "b_chargv", "sprt_party", "party_vote", "pol_interest")

lasso_health_vars <- c("hcond1", "hcond2", "hcond13", "hcond14", "hcond17", "hcond16",  
                       "fdout_spend", "alcohol_spend", "b_usdairy", "b_usbread", "b_wkfruit",  
                       "b_daywlk", "b_wlk30min", "b_walkpace", "b_wkphys", "b_sportsfreq", "b_sports3freq", "b_smever", "b_smnow", 
                       "b_ncigs", "b_aglquit", "htval", "wtval", "wstval", "bmival", "mmgsdval", 
                       "lung_cap_fvc", "lung_cap_fev", "lung_cap_ratio", "ompulval", "concentrate", "sleep", 
                       "useful", "decisions", "strain", "overcoming", "enjoy_act",  "face_prob", "depressed",
                       "confidence", "self_worth", "happiness")

lasso_biom_vars <- c("chol", "hdl", "trig", "hba1c", "hgb",  "rtin", "testo_m", "igfi", "dheas", "hscrp", "uscmg", "uscmm",
                     "cfib", "ecre", "ure", "alkp", "alt")

pca_gen_vars <- c("PCA1", "PCA2", "PCA3", "PCA4", "PCA5", "PCA6", "PCA7", "PCA8", "PCA9", "PCA10")

lasso_all_vars <- c("age", "macob", "times_mar", # Personal
                    "hhsize", "numbeds", "numbedrm", "hhown", "hhval", "edu", "prb_hhold",  "prb_ctax", # Social
                    "benefit2", "benefit3", "benefit5", "benefit6", "benefit8", "benefit96", "benefit_other",
                    "subj_fin", "inc_divi", "b_save", "jbperm", "jbsoc00", "jb2", "b_jbsec", "b_wkaut1", "b_depenth4", 
                    "maedqf", "maju", "paju", "b_matdepa", "b_matdepb", "b_matdepd", "b_matdepe", "b_matdepg", 
                    "sky_cable", "tumble", "dishw", "pcnet", "ncars", "mobuse", 
                    "b_volun", "b_chargv", "sprt_party", "party_vote", "pol_interest",
                    "hcond1", "hcond2", "hcond13", "hcond14", "hcond17", "hcond16",  # health
                    "fdout_spend", "alcohol_spend", "b_usdairy", "b_usbread", "b_wkfruit",  
                    "b_daywlk", "b_wlk30min", "b_walkpace", "b_wkphys", "b_sportsfreq", "b_sports3freq", "b_smever", "b_smnow", 
                    "b_ncigs", "b_aglquit", "htval", "wtval", "wstval", "bmival", "mmgsdval", 
                    "lung_cap_fvc", "lung_cap_fev", "lung_cap_ratio", "ompulval", "concentrate", "sleep", 
                    "useful", "decisions", "strain", "overcoming", "enjoy_act",  "face_prob", "depressed",
                    "confidence", "self_worth", "happiness",
                    "chol", "hdl", "trig", "hba1c", "hgb",  "rtin", "testo_m", "igfi", "dheas", "hscrp", "uscmg", "uscmm", # Biomarkers
                    "cfib", "ecre", "ure", "alkp", "alt",
                    "PCA1", "PCA2", "PCA3", "PCA4", "PCA5", "PCA6", "PCA7", "PCA8", "PCA9", "PCA10") # Genetic