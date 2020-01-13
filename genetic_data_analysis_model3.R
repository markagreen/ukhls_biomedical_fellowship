## Sorting out the Genetic data ##

# Model 3 #

# To run first in notebook
library(data.table) # For general data editing
library(caret) # For splitting the data into train/test samples
library(glmnet) # Package for LASSO in generalisaed models

# ukhls <- fread("../../../../Desktop/Green_UKHLS/ukhls_cleaned.csv") # Load cleaned data
# model3_data <- ukhls[ukhls$wave == 2 & !is.na(ukhls$health_5yr)] # Subset data
# set.seed(250388)
# train_split <- readRDS(file = "./Data split files/train_split_health5yr_model3.rds") # Load
# train <- model3_data[train_split,] # Training data
# test <- model3_data[-train_split,] # Test data
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
#   ids <- model3_data[,c("id")]
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

# Chr1: "rs140027012_A", "rs2072947_C", "rs726764_C"
# Chr10: "rs10824534_C" "rs12778749_C"
# Chr13: "rs4281568_A", "rs4770908_A", "rs1324758_T", "rs7329613_G", "rs4534729_G", "rs10400612_A", "rs17088517_T", 
# "rs1572432_T", "rs7988162_C", "rs12585626_A", "rs7339187_G", "rs145707243_A"
# Chr20: rs3764712_G", "rs16991615_A", "rs714691_G", "rs6041604_T", "rs17826038_C", "rs1884711_T", "rs6091130_A", "rs2236415_A"
# Chr22: "rs2078734_T", "rs2518746_A", "rs2014654_C", "rs5762319_C", "rs2144922_G", "rs4149488_G", "rs5997893_A", "rs140962261_A", 
# "rs5998999_C", "rs10854702_G", "rs17394169_T", "rs137603_A", "rs141441163_C", "rs142851281_T", "rs200898876_T", "rs11704609_A", "rs145663285_T"

# Subset variables selected #

# To be joined onto
ukhls_temp <- fread("../../../../Desktop/Green_UKHLS/ukhls_cleaned.csv") # Load cleaned data
train_split <- readRDS(file = "./Data split files/train_split_health5yr_model3.rds") # Load
model3 <- ukhls_temp[ukhls_temp$wave == 2 & !is.na(ukhls_temp$health_5yr)] # Subset data
model3 <- model3[, "id"] # Just keep the ID
rm(ukhls_temp)

# Subset by each chromosome and join onto main data
ukhls_chr1 <- fread("../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr1_sendout/Green_Genotype_chr1_sendout.raw", header=T) # Load data
ukhls_chr1 <- ukhls_chr1[,c("IID", "rs140027012_A", "rs2072947_C", "rs726764_C")] # Subset selected features
gen_data_m3 <- merge(model3, ukhls_chr1, by.x = "id", by.y = "IID", all.x = TRUE, sort = F) # Join together
rm(ukhls_chr1, model3) # Tidy

ukhls_chr10 <- fread("../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr8_to_chr11_sendout/Green_Genotype_chr10_sendout.raw", header=T)
ukhls_chr10 <- ukhls_chr10[,c("IID", "rs10824534_C", "rs12778749_C")]
gen_data_m3 <- merge(gen_data_m3, ukhls_chr10, by.x = "id", by.y = "IID", all.x = TRUE, sort = F)
rm(ukhls_chr10)

ukhls_chr13 <- fread("../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr12_chr16_sendout/Green_Genotype_chr13_sendout.raw", header=T)
ukhls_chr13 <- ukhls_chr13[,c("IID", "rs4281568_A", "rs4770908_A", "rs1324758_T", "rs7329613_G", "rs4534729_G", "rs10400612_A", "rs17088517_T", 
                              "rs1572432_T", "rs7988162_C", "rs12585626_A", "rs7339187_G", "rs145707243_A")]
gen_data_m3 <- merge(gen_data_m3, ukhls_chr13, by.x = "id", by.y = "IID", all.x = TRUE, sort = F)
rm(ukhls_chr13)

ukhls_chr20 <- fread("../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr17_to_chr22_sendout/Green_Genotype_chr20_sendout.raw", header=T)
ukhls_chr20 <- ukhls_chr20[,c("IID", "rs3764712_G", "rs16991615_A", "rs714691_G", "rs6041604_T", "rs17826038_C", "rs1884711_T", "rs6091130_A", "rs2236415_A")]
gen_data_m3 <- merge(gen_data_m3, ukhls_chr20, by.x = "id", by.y = "IID", all.x = TRUE, sort = F)
rm(ukhls_chr20)

ukhls_chr22 <- fread("../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr17_to_chr22_sendout/Green_Genotype_chr22_sendout.raw", header=T)
ukhls_chr22 <- ukhls_chr22[,c("IID", "rs2078734_T", "rs2518746_A", "rs2014654_C", "rs5762319_C", "rs2144922_G", "rs4149488_G", "rs5997893_A", "rs140962261_A", 
                              "rs5998999_C", "rs10854702_G", "rs17394169_T", "rs137603_A", "rs141441163_C", "rs142851281_T", "rs200898876_T", "rs11704609_A", "rs145663285_T")]
gen_data_m3 <- merge(gen_data_m3, ukhls_chr22, by.x = "id", by.y = "IID", all.x = TRUE, sort = F)
rm(ukhls_chr22)

# Code missing data (rare)
gen_data_m3[is.na(gen_data_m3)] <- -1

# Subset data for standardisation
id <- gen_data_m3[, "id"] # Save ID for later
train_chr <- gen_data_m3[train_split,] # Split into training data
test_chr <- gen_data_m3[-train_split,] # Test data

# Normalise the data
mean <- apply(train_chr, 2, mean) # Calcuate the mean of each feature
std <- apply(train_chr, 2, sd) # Calculate the standard deviation of each feature
train_chr <- as.data.table(scale(train_chr, center = mean, scale = std)) # Scale training data
test_chr <- as.data.table(scale(test_chr, center = mean, scale = std)) # Scale test data

# Join back on IDs
train_chr$id <- id[train_split,id]
test_chr$id <- id[-train_split,id]



