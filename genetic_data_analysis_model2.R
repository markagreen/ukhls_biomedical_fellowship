## Sorting out the Genetic data ##

# Model 2 #

# To run first in notebook
library(data.table) # For general data editing
library(caret) # For splitting the data into train/test samples
library(glmnet) # Package for LASSO in generalisaed models

# ukhls <- fread("../../../../Desktop/Green_UKHLS/ukhls_cleaned.csv") # Load cleaned data
# model2_data <- ukhls[ukhls$wave == 2 & !is.na(ukhls$health_1yr) & ukhls$health_0yr == 0] # Subset data
# set.seed(250388)
# train_split <- readRDS(file = "./Data split files/train_split_health1yr_model2.rds") # Load
# train <- model2_data[train_split,] # Training data
# test <- model2_data[-train_split,] # Test data
# train_labels <- train[,"health_1yr"] # Training data labels
# test_labels <- test[,"health_1yr"] # Test data labels
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
#   ids <- model2_data[,c("id")]
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
#   fit_chr <- cv.glmnet(temp, train_labels$health_1yr, alpha = 1, family = "binomial", nfolds = 4) # LASSO
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

# Chr5: "rs1428739_T"   "rs2973319_G"   "rs10512943_G"  "rs7712875_G"   "rs2288390_T"   "rs201463346_T" "rs6862024_A"   "rs4868200_C"   "rs4868204_T"
# Chr7: "rs144315661_A"
# Chr11: "rs147009902_T" "rs141568989_A"
# Chr13: "rs4319602_C"   "rs201478358_C" "rs7323303_A"   "rs143941016_G" "rs10507462_T"  "rs200817424_T" "rs2146368_T"   "rs9527133_A"   "rs1287555_C"   "rs9589392_G"   "rs9583985_A"   "rs9516519_G"  
# "rs9516520_C"   "rs146927771_G" "rs17677552_A"  "rs140529753_C" "rs7989848_G"   "rs376476_A"    "rs149937585_T" "rs200853560_T" "rs142885298_T"
# Chr19: "rs61735544_T" "rs3745245_G"  "rs67712849_C" "rs34545902_A"
# Chr 21: "rs239677_T"    "rs2834278_T"   "rs138295040_T"

# To be joined onto
ukhls_temp <- fread("../../../../Desktop/Green_UKHLS/ukhls_cleaned.csv")
train_split <- readRDS(file = "./Data split files/train_split_health1yr_model2.rds") # Load
model2 <- ukhls_temp[ukhls_temp$wave == 2 & !is.na(ukhls_temp$health_1yr) & ukhls_temp$health_0yr == 0]
model2 <- model2[, "id"] # Just keep the ID
rm(ukhls_temp)

# Subset by each chromosome and join onto main data

ukhls_chr5 <- fread("../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr4_to_chr7_sendout/Green_Genotype_chr5_sendout.raw", header=T)
ukhls_chr5 <- ukhls_chr5[,c("IID", "rs1428739_T", "rs2973319_G", "rs10512943_G", "rs7712875_G", "rs2288390_T", "rs201463346_T", "rs6862024_A", "rs4868200_C", "rs4868204_T")]
gen_data_m2 <- merge(model2, ukhls_chr5, by.x = "id", by.y = "IID", all.x = TRUE, sort = F)
rm(ukhls_chr5, model2)

ukhls_chr7 <- fread("../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr4_to_chr7_sendout/Green_Genotype_chr7_sendout.raw", header=T)
ukhls_chr7 <- ukhls_chr7[,c("IID", "rs144315661_A")]
gen_data_m2 <- merge(gen_data_m2, ukhls_chr7, by.x = "id", by.y = "IID", all.x = TRUE, sort = F)
rm(ukhls_chr7)

ukhls_chr11 <- fread("../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr8_to_chr11_sendout/Green_Genotype_chr11_sendout.raw", header=T)
ukhls_chr11 <- ukhls_chr11[,c("IID", "rs147009902_T", "rs141568989_A")]
gen_data_m2 <- merge(gen_data_m2, ukhls_chr11, by.x = "id", by.y = "IID", all.x = TRUE, sort = F)
rm(ukhls_chr11)

ukhls_chr13 <- fread("../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr12_chr16_sendout/Green_Genotype_chr13_sendout.raw", header=T)
ukhls_chr13 <- ukhls_chr13[,c("IID", "rs4319602_C", "rs201478358_C", "rs7323303_A", "rs143941016_G", "rs10507462_T", "rs200817424_T", "rs2146368_T",   
                              "rs9527133_A", "rs1287555_C", "rs9589392_G", "rs9583985_A", "rs9516519_G", "rs9516520_C", "rs146927771_G", "rs17677552_A",  
                              "rs140529753_C", "rs7989848_G", "rs376476_A", "rs149937585_T", "rs200853560_T", "rs142885298_T")]
gen_data_m2 <- merge(gen_data_m2, ukhls_chr13, by.x = "id", by.y = "IID", all.x = TRUE, sort = F)
rm(ukhls_chr13)

ukhls_chr19 <- fread("../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr17_to_chr22_sendout/Green_Genotype_chr19_sendout.raw", header=T)
ukhls_chr19 <- ukhls_chr19[,c("IID", "rs61735544_T", "rs3745245_G", "rs67712849_C", "rs34545902_A")]
gen_data_m2 <- merge(gen_data_m2, ukhls_chr19, by.x = "id", by.y = "IID", all.x = TRUE, sort = F)
rm(ukhls_chr19)

ukhls_chr21 <- fread("../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr17_to_chr22_sendout/Green_Genotype_chr21_sendout.raw", header=T)
ukhls_chr21 <- ukhls_chr21[,c("IID", "rs239677_T", "rs2834278_T", "rs138295040_T")]
gen_data_m2 <- merge(gen_data_m2, ukhls_chr21, by.x = "id", by.y = "IID", all.x = TRUE, sort = F)
rm(ukhls_chr21)

# Code missing data (rare)
gen_data_m2[is.na(gen_data_m2)] <- -1

# Subset data for standardisation
id <- gen_data_m2[, "id"] # Save ID for later
train_chr <- gen_data_m2[train_split,] # Split into training data
test_chr <- gen_data_m2[-train_split,] # Test data

# Normalise the data
mean <- apply(train_chr, 2, mean) # Calcuate the mean of each feature
std <- apply(train_chr, 2, sd) # Calculate the standard deviation of each feature
train_chr <- as.data.table(scale(train_chr, center = mean, scale = std)) # Scale training data
test_chr <- as.data.table(scale(test_chr, center = mean, scale = std)) # Scale test data

# Join back on IDs
train_chr$id <- id[train_split,id]
test_chr$id <- id[-train_split,id]



