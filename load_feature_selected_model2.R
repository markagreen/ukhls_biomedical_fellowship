##################################
##### Load features selected #####
############ Model 2 #############
##################################

lasso_personal_vars <- c("age", "marstat", "times_mar")

lasso_ses_vars <- c("hhsize", "edu", "benefit1", "benefit2", "benefit3", "benefit4", "benefit8",  
                    "subj_fin", "jbperm", "jb2", "b_wkaut1", "maedqf", "paju", "b_matdepb", "b_matdepf",
                    "sky_cable",  "mobuse", "b_chargv", "pol_interest")

lasso_health_vars <- c("hcond2", "hcond13", "hcond14", "hcond_cvd", "hcond16", "food_spend", "fdout_spend", "alcohol_spend",
                       "b_wkfruit", "b_wkvege", "b_fruvege", "b_wlk30min", "b_walkpace",
                       "b_wkphys", "b_sportsfreq", "b_ncigs", "b_aglquit", "htval", "wtval",
                       "wstval", "bmival", "lung_cap_fev",  "concentrate", "sleep",
                       "useful", "decisions", "strain", "enjoy_act", "confidence", "self_worth")

lasso_biom_vars <- c("chol", "hdl", "trig", "hba1c", "hgb", "igfi", "dheas", 
                     "cfib", "ecre", "ure", "alkp", "ast", "ggt")

pca_gen_vars <- c("PCA1", "PCA2", "PCA3", "PCA4", "PCA5", "PCA6", "PCA7", "PCA8", "PCA9", "PCA10")

lasso_all_vars <- c("age", "marstat", "times_mar", # Personal
                    "hhsize", "edu", "benefit1", "benefit2", "benefit3", "benefit4", "benefit8", # Social
                    "subj_fin", "jbperm", "jb2", "b_wkaut1", "maedqf", "paju", "b_matdepb", "b_matdepf",
                    "sky_cable",  "mobuse", "b_chargv", "pol_interest",
                    "hcond2", "hcond13", "hcond14", "hcond_cvd", "hcond16", "food_spend", "fdout_spend", "alcohol_spend", # health
                    "b_wkfruit", "b_wkvege", "b_fruvege", "b_wlk30min", "b_walkpace",
                    "b_wkphys", "b_sportsfreq", "b_ncigs", "b_aglquit", "htval", "wtval",
                    "wstval", "bmival", "lung_cap_fev",  "concentrate", "sleep",
                    "useful", "decisions", "strain", "enjoy_act", "confidence", "self_worth",
                    "chol", "hdl", "trig", "hba1c", "hgb", "igfi", "dheas", # Biomarker
                    "cfib", "ecre", "ure", "alkp", "ast", "ggt",
                    "PCA1", "PCA2", "PCA3", "PCA4", "PCA5", "PCA6", "PCA7", "PCA8", "PCA9", "PCA10") # Genetic