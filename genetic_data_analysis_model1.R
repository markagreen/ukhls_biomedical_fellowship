## Sorting out the Genetic data ##

# Model 1 #

# To run first in notebook
library(data.table) # For general data editing
library(caret) # For splitting the data into train/test samples
library(glmnet) # Package for LASSO in generalisaed models

# ukhls <- fread("../../../../Desktop/Green_UKHLS/ukhls_cleaned.csv") # Load cleaned data
# model1_data <- ukhls[ukhls$wave == 2 & !is.na(ukhls$health_1yr)] # Subset data
# set.seed(250388)
# train_split <- readRDS(file = "./Data split files/train_split_health1yr.rds") # Load train/test split
# train <- model1_data[train_split,] # Training data
# test <- model1_data[-train_split,] # Test data
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
#  print(i) # To show progress
#  ukhls_chr <- fread(paste0(filename[i]), header=T) # Load data for a chromosome
# 
#  # Drop variables not required 
#  ukhls_chr <- ukhls_chr[,-c("FID", "MAT", "PAT", "SEX", "PHENOTYPE")]
#  
#  # Split data into train and test samples
#  ids <- model1_data[,c("id")]
#  hold <- merge(ids, ukhls_chr, by.x = "id", by.y = "IID", all.x = TRUE, sort = F) # Keep only those who are in the analysis (in same order)
#  train_chr <- hold[train_split,] # Training data
#  test_chr <- hold[-train_split,] # Test data
#  
#  # Normalise the data
#  mean <- apply(train_chr, 2, mean) # Calcuate the mean of each feature
#  std <- apply(train_chr, 2, sd) # Calculate the standard deviation of each feature
#  train_chr <- as.data.table(scale(train_chr, center = mean, scale = std)) # Scale training data
#  test_chr <- as.data.table(scale(test_chr, center = mean, scale = std)) # Scale test data
#  
#  # Drop variables that have no variants
#  cond <- train_chr[, lapply(.SD, function(x) sum(is.na(diff(x)))) < .N - 512, .SDcols = -1] # Drop if sum of missing data is fewer than row count - X (I have selected 10% - 512) 
#  train_chr <- train_chr[, c(TRUE, cond), with = FALSE]
#  rm(cond)
#  
#  # LASSO
#  temp <- as.matrix(train_chr) # Convert data to matrix format 
#  fit_chr <- cv.glmnet(temp, train_labels$health_1yr, alpha = 1, family = "binomial", nfolds = 4) # LASSO
#  
#  # Store coefficients
#  tmp_coeffs <- coef(fit_chr, s = "lambda.min")  # lambda.1se
#  hold <- data.frame(name_min = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coef_min = tmp_coeffs@x) # Change object from sparse matrix to data.frame
#  lasso_chr <- as.character(hold$name[2:nrow(hold)])
#  print(lasso_chr)
#  rm(hold)
#  
# }

## Results ##

# Chr 1: "rs709767_A"
# Chr 2: "rs147424111_T" "rs4851365_T"
# Chr 4: "rs2174558_T"
# Chr 6: "rs12192896_A"
# Chr 7:  [1] "rs10952094_C"  "rs13239626_A"  "rs12702794_T"  "rs6977730_G"   "rs713493_G"    "rs10277002_C"  "rs345395_G"    "rs2718021_G"   "rs12673437_G"  "rs10278370_T"  "rs1294962_A"   "rs2304693_A"  
# "rs4724557_T"   "rs6463430_G"   "rs10254410_T"  "rs6973521_G"   "rs1661606_A"   "rs11761352_A"  "rs11972637_A"  "rs1167800_G"   "rs182065276_G" "rs1522545_G"   "rs144262262_C" "rs2970471_T"  
# "rs3181009_C"   "rs6977206_T"   "rs2396676_A"   "rs4728265_C"   "rs2244307_G"   "rs2272096_C"   "rs1018771_A"   "rs10228407_T"  "rs12536735_T"
# Chr 8: "rs6990159_G" "rs2127035_T"
# Chr 14: "rs17103671_G"  "rs2236133_A"   "rs192689400_T" "rs1956970_G"   "rs12434716_C"  "rs12433785_A"  "rs12432740_T"  "rs4900734_T"   "rs7160618_T"   "rs12885618_A"  "rs3819015_T"   "rs7182_T"     
# "rs7148695_C"   "rs7149354_T"   "rs2282032_T"   "rs941591_C"    "rs730036_A"    "rs7155511_A"   "rs861539_A"
# Chr 17: "rs2172680_T"
# Chr 19: "rs2304207_G"
# Chr 20: "rs236114_T"  "rs6069473_T"
# Chr 21: "rs2825069_G"   "rs7279234_A"   "rs2825628_C"   "rs2826320_T"   "rs17003866_T"  "rs2828463_T"   "rs2211904_G"   "rs2829950_G"   "rs3177478_A"   "rs9975358_A"   "rs147398927_G" "rs2834303_T"  
# "rs754467_G"    "rs2835223_G"   "rs3746887_T"   "rs2837583_T"   "rs2051405_C"   "rs144281842_A" "rs2186358_C" 
# Chr 22: "rs9607293_A"


# To be joined onto
ukhls_temp <- fread("../../../../Desktop/Green_UKHLS/ukhls_cleaned.csv")
train_split <- readRDS(file = "./Data split files/train_split_health1yr_model1.rds") # Load
model1 <- ukhls_temp[ukhls_temp$wave == 2 & !is.na(ukhls_temp$health_1yr)]
model1 <- model1[, "id"] # Just keep the ID
rm(ukhls_temp)

# Subset by each chromosome and join onto main data

ukhls_chr1 <- fread("../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr1_sendout/Green_Genotype_chr1_sendout.raw", header=T)
ukhls_chr1 <- ukhls_chr1[,c("IID", "rs709767_A")]
gen_data_m1 <- merge(model1, ukhls_chr1, by.x = "id", by.y = "IID", all.x = TRUE, sort = F)
rm(ukhls_chr1, model1)

ukhls_chr2 <- fread("../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr2_to_chr3_sendout/Green_Genotype_chr2_sendout.raw", header=T)
ukhls_chr2 <- ukhls_chr2[,c("IID", "rs147424111_T", "rs4851365_T")]
gen_data_m1 <- merge(gen_data_m1, ukhls_chr2, by.x = "id", by.y = "IID", all.x = TRUE, sort = F)
rm(ukhls_chr2)

ukhls_chr4 <- fread("../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr4_to_chr7_sendout/Green_Genotype_chr4_sendout.raw", header=T)
ukhls_chr4 <- ukhls_chr4[,c("IID", "rs2174558_T")]
gen_data_m1 <- merge(gen_data_m1, ukhls_chr4, by.x = "id", by.y = "IID", all.x = TRUE, sort = F)
rm(ukhls_chr4)

ukhls_chr7 <- fread("../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr4_to_chr7_sendout/Green_Genotype_chr7_sendout.raw", header=T)
ukhls_chr7 <- ukhls_chr7[,c("IID", "rs10952094_C", "rs13239626_A", "rs12702794_T", "rs6977730_G", "rs713493_G", "rs10277002_C", "rs345395_G", "rs2718021_G", 
                            "rs12673437_G", "rs10278370_T", "rs1294962_A", "rs2304693_A", "rs4724557_T", "rs6463430_G", "rs10254410_T", "rs6973521_G", "rs1661606_A",
                            "rs11761352_A", "rs11972637_A", "rs1167800_G", "rs182065276_G", "rs1522545_G", "rs144262262_C", "rs2970471_T",  "rs3181009_C",  
                            "rs6977206_T", "rs2396676_A", "rs4728265_C", "rs2244307_G", "rs2272096_C", "rs1018771_A", "rs10228407_T", "rs12536735_T")]
gen_data_m1 <- merge(gen_data_m1, ukhls_chr7, by.x = "id", by.y = "IID", all.x = TRUE, sort = F)
rm(ukhls_chr7)

ukhls_chr8 <- fread("../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr8_to_chr11_sendout/Green_Genotype_chr8_sendout.raw", header=T)
ukhls_chr8 <- ukhls_chr8[,c("IID", "rs6990159_G", "rs2127035_T")]
gen_data_m1 <- merge(gen_data_m1, ukhls_chr8, by.x = "id", by.y = "IID", all.x = TRUE, sort = F)
rm(ukhls_chr8)

ukhls_chr14 <- fread("../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr12_chr16_sendout/Green_Genotype_chr14_sendout.raw", header=T)
ukhls_chr14 <- ukhls_chr14[,c("IID", "rs17103671_G", "rs2236133_A", "rs192689400_T", "rs1956970_G", "rs12434716_C", "rs12433785_A", "rs12432740_T", "rs4900734_T",
                              "rs7160618_T", "rs12885618_A", "rs3819015_T", "rs7182_T", "rs7148695_C", "rs7149354_T", "rs2282032_T", "rs941591_C", "rs730036_A",
                              "rs7155511_A", "rs861539_A")]
gen_data_m1 <- merge(gen_data_m1, ukhls_chr14, by.x = "id", by.y = "IID", all.x = TRUE, sort = F)
rm(ukhls_chr14)

ukhls_chr17 <- fread("../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr17_to_chr22_sendout/Green_Genotype_chr17_sendout.raw", header=T)
ukhls_chr17 <- ukhls_chr17[,c("IID", "rs2172680_T")]
gen_data_m1 <- merge(gen_data_m1, ukhls_chr17, by.x = "id", by.y = "IID", all.x = TRUE, sort = F)
rm(ukhls_chr17)

ukhls_chr19 <- fread("../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr17_to_chr22_sendout/Green_Genotype_chr19_sendout.raw", header=T)
ukhls_chr19 <- ukhls_chr19[,c("IID", "rs2304207_G")]
gen_data_m1 <- merge(gen_data_m1, ukhls_chr19, by.x = "id", by.y = "IID", all.x = TRUE, sort = F)
rm(ukhls_chr19)

ukhls_chr20 <- fread("../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr17_to_chr22_sendout/Green_Genotype_chr20_sendout.raw", header=T)
ukhls_chr20 <- ukhls_chr20[,c("IID", "rs236114_T", "rs6069473_T")]
gen_data_m1 <- merge(gen_data_m1, ukhls_chr20, by.x = "id", by.y = "IID", all.x = TRUE, sort = F)
rm(ukhls_chr20)

ukhls_chr21 <- fread("../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr17_to_chr22_sendout/Green_Genotype_chr21_sendout.raw", header=T)
ukhls_chr21 <- ukhls_chr21[,c("IID", "rs2825069_G", "rs7279234_A", "rs2825628_C", "rs2826320_T", "rs17003866_T", "rs2828463_T", "rs2211904_G", "rs2829950_G", 
                              "rs3177478_A", "rs9975358_A", "rs147398927_G", "rs2834303_T", "rs754467_G", "rs2835223_G", "rs3746887_T", "rs2837583_T",  
                              "rs2051405_C", "rs144281842_A", "rs2186358_C")]
gen_data_m1 <- merge(gen_data_m1, ukhls_chr21, by.x = "id", by.y = "IID", all.x = TRUE, sort = F)
rm(ukhls_chr21)

ukhls_chr22 <- fread("../../../../Desktop/Green_UKHLS/Genetic data RAW files/Green_Genotype_chr17_to_chr22_sendout/Green_Genotype_chr22_sendout.raw", header=T)
ukhls_chr22 <- ukhls_chr22[,c("IID", "rs9607293_A")]
gen_data_m1 <- merge(gen_data_m1, ukhls_chr22, by.x = "id", by.y = "IID", all.x = TRUE, sort = F)
rm(ukhls_chr22)

# Code missing data (rare)
gen_data_m1[is.na(gen_data_m1)] <- -1

# Subset data for standardisation
id <- gen_data_m1[, "id"] # Save ID for later
train_chr <- gen_data_m1[train_split,] # Split into training data
test_chr <- gen_data_m1[-train_split,] # Test data

# Normalise the data
mean <- apply(train_chr, 2, mean) # Calcuate the mean of each feature
std <- apply(train_chr, 2, sd) # Calculate the standard deviation of each feature
train_chr <- as.data.table(scale(train_chr, center = mean, scale = std)) # Scale training data
test_chr <- as.data.table(scale(test_chr, center = mean, scale = std)) # Scale test data

# Join back on IDs
train_chr$id <- id[train_split,id]
test_chr$id <- id[-train_split,id]
