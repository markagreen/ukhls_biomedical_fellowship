## Sorting out the Genetic data ##

# Model 4 #

# To run first in notebook
library(data.table) # For general data editing
library(caret) # For splitting the data into train/test samples
library(glmnet) # Package for LASSO in generalisaed models

# ukhls <- fread("../../../../Desktop/Green_UKHLS/ukhls_cleaned.csv") # Load cleaned data
# model4_data <- ukhls[ukhls$wave == 2 & !is.na(ukhls$health_5yr) & ukhls$health_0yr == 0] # Subset data
# set.seed(250388)
# train_split <- readRDS(file = "./Data split files/train_split_health5yr_model4.rds") # Load
# train <- model4_data[train_split,] # Training data
# test <- model4_data[-train_split,] # Test data
# train_labels <- train[,"health_5yr"] # Training data labels
# test_labels <- test[,"health_5yr"] # Test data labels
# 
# # Define where files are
# filename <- c("../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr1_sendout/Green_Genotype_chr1_sendout.raw",
#               "../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr2_to_chr3_sendout/Green_Genotype_chr2_sendout.raw",
#               "../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr2_to_chr3_sendout/Green_Genotype_chr3_sendout.raw",
#               "../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr4_to_chr7_sendout/Green_Genotype_chr4_sendout.raw",
#               "../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr4_to_chr7_sendout/Green_Genotype_chr5_sendout.raw",
#               "../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr4_to_chr7_sendout/Green_Genotype_chr6_sendout.raw",
#               "../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr4_to_chr7_sendout/Green_Genotype_chr7_sendout.raw",
#               "../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr8_to_chr11_sendout/Green_Genotype_chr8_sendout.raw",
#               "../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr8_to_chr11_sendout/Green_Genotype_chr9_sendout.raw",
#               "../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr8_to_chr11_sendout/Green_Genotype_chr10_sendout.raw",
#               "../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr8_to_chr11_sendout/Green_Genotype_chr11_sendout.raw",
#               "../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr12_chr16_sendout/Green_Genotype_chr12_sendout.raw",
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
# for (i in 1:22){
#   
#   print(i) # To show progress
#   ukhls_chr <- fread(paste0(filename[i]), header=T) # Load data for a chromosome
#   
#   # Drop variables not required 
#   ukhls_chr <- ukhls_chr[,-c("FID", "MAT", "PAT", "SEX", "PHENOTYPE")]
#   
#   # Split data into train and test samples
#   ids <- model4_data[,c("id")]
#   hold <- merge(ids, ukhls_chr, by.x = "id", by.y = "IID", all.x = TRUE, sort = F) # Keep only those who are in the analysis (in same order)
#   train_chr <- hold[train_split,] # Training data
#   test_chr <- hold[-train_split,] # Test data
#   
#   # Normalise the data
#   mean <- apply(train_chr, 2, mean) # Calcuate the mean of each feature
#   std <- apply(train_chr, 2, sd) # Calculate the standard deviation of each feature
#   train_chr <- as.data.table(scale(train_chr, center = mean, scale = std)) # Scale training data
#   test_chr <- as.data.table(scale(test_chr, center = mean, scale = std)) # Scale test data
#   
#   # Drop variables that have no variants
#   cond <- train_chr[, lapply(.SD, function(x) sum(is.na(diff(x)))) < .N - 512, .SDcols = -1] # Drop if sum of missing data is fewer than row count - X (I have selected 10% - 512) 
#   train_chr <- train_chr[, c(TRUE, cond), with = FALSE]
#   rm(cond)
#   
#   # LASSO
#   temp <- as.matrix(train_chr) # Convert data to matrix format 
#   fit_chr <- cv.glmnet(temp, train_labels$health_5yr, alpha = 1, family = "binomial", nfolds = 4) # LASSO
#   
#   # Store coefficients
#   tmp_coeffs <- coef(fit_chr, s = "lambda.min")  # lambda.1se
#   hold <- data.frame(name_min = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coef_min = tmp_coeffs@x) # Change object from sparse matrix to data.frame
#   lasso_chr <- as.character(hold$name[2:nrow(hold)])
#   print(lasso_chr)
#   rm(hold)
#   
# }

## Results ##

# Chr3: "rs41294980_A" "rs1075498_A"
# Chr13: "rs9533783_G"  "rs10507701_T" "rs1041379_C"  "rs7326137_T"
# Chr17:  "exm1296241_C"  "rs230915_C"    "rs2138852_C"   "rs9900280_G"   "rs6505503_T"   "rs1032070_T"   "rs139657728_A" "rs5035_G"      
# "rs8065903_A"   "rs6503919_G"   "rs534645_A"    "rs8074685_C"  "rs2292645_C"   "rs147221289_A" "rs7501659_T"
# Chr19: "rs200878232_C" "rs4926222_G"   "rs200774189_T" "rs202026525_G" "rs4802915_T"   "rs2087115_T"   "rs61735033_A"
# Chr21: "rs2829388_A" "rs933153_A"  "rs2831503_C" "rs2838053_G"

# Subset variables selected #

# To be joined onto
ukhls_temp <- fread("../../../../Desktop/Green_UKHLS/ukhls_cleaned.csv")
train_split <- readRDS(file = "./Data split files/train_split_health5yr_model4.rds") # Load
model4 <- ukhls_temp[ukhls_temp$wave == 2 & !is.na(ukhls_temp$health_5yr) & ukhls_temp$health_0yr == 0]
model4 <- model4[, "id"] # Just keep the ID
rm(ukhls_temp)

# Subset by each chromosome and join onto main data
ukhls_chr3 <- fread("../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr2_to_chr3_sendout/Green_Genotype_chr3_sendout.raw", header=T)
ukhls_chr3 <- ukhls_chr3[,c("IID", "rs41294980_A", "rs1075498_A")]
gen_data_m4 <- merge(model4, ukhls_chr3, by.x = "id", by.y = "IID", all.x = TRUE, sort = F)
rm(ukhls_chr3, model4)

ukhls_chr13 <- fread("../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr12_chr16_sendout/Green_Genotype_chr13_sendout.raw", header=T)
ukhls_chr13 <- ukhls_chr13[,c("IID", "rs9533783_G", "rs10507701_T", "rs1041379_C", "rs7326137_T")]
gen_data_m4 <- merge(gen_data_m4, ukhls_chr13, by.x = "id", by.y = "IID", all.x = TRUE, sort = F)
rm(ukhls_chr13)

ukhls_chr17 <- fread("../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr17_to_chr22_sendout/Green_Genotype_chr17_sendout.raw", header=T)
ukhls_chr17 <- ukhls_chr17[,c("IID", "exm1296241_C", "rs230915_C", "rs2138852_C", "rs9900280_G", "rs6505503_T", "rs1032070_T", "rs139657728_A", "rs5035_G",      
                              "rs8065903_A", "rs6503919_G", "rs534645_A", "rs8074685_C", "rs2292645_C", "rs147221289_A", "rs7501659_T")]
gen_data_m4 <- merge(gen_data_m4, ukhls_chr17, by.x = "id", by.y = "IID", all.x = TRUE, sort = F)
rm(ukhls_chr17)

ukhls_chr19 <- fread("../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr17_to_chr22_sendout/Green_Genotype_chr19_sendout.raw", header=T)
ukhls_chr19 <- ukhls_chr19[,c("IID", "rs200878232_C", "rs4926222_G", "rs200774189_T", "rs202026525_G", "rs4802915_T", "rs2087115_T", "rs61735033_A")]
gen_data_m4 <- merge(gen_data_m4, ukhls_chr19, by.x = "id", by.y = "IID", all.x = TRUE, sort = F)
rm(ukhls_chr19)

ukhls_chr21 <- fread("../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr17_to_chr22_sendout/Green_Genotype_chr21_sendout.raw", header=T)
ukhls_chr21 <- ukhls_chr21[,c("IID", "rs2829388_A", "rs933153_A", "rs2831503_C", "rs2838053_G")]
gen_data_m4 <- merge(gen_data_m4, ukhls_chr21, by.x = "id", by.y = "IID", all.x = TRUE, sort = F)
rm(ukhls_chr21)

# Code missing data (rare)
gen_data_m4[is.na(gen_data_m4)] <- -1

# Subset data for standardisation
id <- gen_data_m4[, "id"] # Save ID for later
train_chr <- gen_data_m4[train_split,] # Split into training data
test_chr <- gen_data_m4[-train_split,] # Test data

# Normalise the data
mean <- apply(train_chr, 2, mean) # Calcuate the mean of each feature
std <- apply(train_chr, 2, sd) # Calculate the standard deviation of each feature
train_chr <- as.data.table(scale(train_chr, center = mean, scale = std)) # Scale training data
test_chr <- as.data.table(scale(test_chr, center = mean, scale = std)) # Scale test data

# Join back on IDs
train_chr$id <- id[train_split,id]
test_chr$id <- id[-train_split,id]

