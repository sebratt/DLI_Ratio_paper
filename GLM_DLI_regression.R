# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# 
# DLI 
# GLM model
# Step 1: Run regression with (non transformed data)
# Step 2: Diagnostics (DF_beta): 
# 
# DF_beta (its a test statistic, w/ a threshold recognized; diagnostic)
# Cook's distance (are the outliers having an outsized effect). 
# Jeff Oliver's recommendation: Poisson regression in R: https://stats.oarc.ucla.edu/r/dae/poisson-regression/ 
# Zero inflated Poisson regression, which might be important for your data: https://stats.oarc.ucla.edu/r/dae/zip/
# Select a statistical test: https://stats.oarc.ucla.edu/other/mult-pkg/whatstat/
# 
# See r script: DLI_regression.R in Dropbox for details
# 
# Used for glm poisson tutorial: https://www.dataquest.io/blog/tutorial-poisson-regression-in-r/
# 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

library(datasets)
# Read DLI data (created dummy for transformations) 

write.csv2(data, "C:/Users/sebratt/DropBox/dli_data_regress_clean.csv")
data <- read.csv2("C:/Users/sebratt/DropBox/dli_data_regress_clean.csv")
View(head(data))
data$X <- NULL

hist(data$citationcount)
??hist

mean(data$citationcount) #41
var(data$citationcount) # The variance is much greater than the mean  suggests  we will have over-dispersion in the model.

# # # # # # Outlier detection using dfBETA # # # # # # # # #
poisson.model <- glm(citationcount ~ DLI 
                     + year + journalimpactfactor + teamsize 
                     + SAC+field_of_study_2 + sd_career_age + mean_career_age, 
                     family = "poisson", 
                     data = data)

# Use the dfbeta function to calculate the effect on each coefficient of each observation. 
# Big DFBETA means point has oversized effect on model coefficient estimates
cite_dfbeta <- dfbeta(poisson.model) 

# But how big is too big? Standard threshold is 2/sqrt(N), where N is sample size
dfbeta_threshold <- 2/sqrt(nrow(data))

# Are any of the values of DFBETA above the threshold? DFBETAs can be positive 
# or negative indicating the direction of influence, but we are only interested 
# in *magnitude*, so we need to take the absolute value of the DFBETA scores. 
# We start by asking if ANY values have a magnitude larger than the threshold.
any(abs(cite_dfbeta) > dfbeta_threshold)

# True. 
# Yup. Find out which ones. The DFBETA scores are a matrix with as many columns 
# as we have coefficients in our model (in this case, three columns, including 
# one for the intercept in the model). We are not so concerned with which 
# *coefficients* (columns) are being affected, but rather if a particular 
# *observation* (row) is having an oversized effect. So we do some logic math 
# to find any rows of the DFBETA scores matrix that have at least one value 
# above the threshold.

dfbeta_test <- rowSums(abs(cite_dfbeta) > dfbeta_threshold)
# Which rows had at least one value too large?
oversized <- which(dfbeta_test > 0)
# Now we can print out those rows to see which are the offending rows.
View(head(data[oversized, ]))

# With those rows identified as having an oversized effect, we will then drop 
# them from the dataset, and do the entire model building and evaluation step 
# again.

# Create a subset of data, without the rows that had too-large DFBETAs
data_subset <- data[-oversized, ]
# Re-run the GLM on the smaller dataset
cite_glm_2 <- glm(citationcount ~ DLI + year + journalimpactfactor + teamsize + SAC+field_of_study_2 + sd_career_age + mean_career_age,
                  family = "poisson", 
                  data = data_subset)

cite_dfbeta_2 <- dfbeta(cite_glm_2)
dfbeta_threshold_2 <- 2/sqrt(nrow(data_subset))
# Any too-large DFBETA values?
any(abs(cite_dfbeta_2) > dfbeta_threshold_2)
# TRUE

# Still another row marked as having too-large effect on coefficients. Re-run 
# the process after dropping that one row.
# Start by identifying and removing that row.
dfbeta_test_2 <- rowSums(abs(cite_dfbeta_2) > dfbeta_threshold_2)
oversized <- which(dfbeta_test_2 > 0)
data_subset_2 <- data_subset[-oversized, ]

# Re-run GLM. Again, with that new, reduced dataset.
cite_glm_3 <- glm(citationcount ~ DLI + year + journalimpactfactor + teamsize + SAC+field_of_study_2 + sd_career_age + mean_career_age,
                  family = "poisson", 
                  data = data_subset_2)

cite_dfbeta_3 <- dfbeta(cite_glm_3)
dfbeta_threshold_3 <- 2/sqrt(nrow(data_subset_2))
# Any too-large DFBETA values?
any(abs(cite_dfbeta_3) > dfbeta_threshold_3)
# TRUE


# Still another row marked as having too-large effect on coefficients. Re-run 
# the process after dropping that one row.
# Start by identifying and removing that row.
dfbeta_test_3 <- rowSums(abs(cite_dfbeta_2) > dfbeta_threshold_2)
oversized <- which(dfbeta_test_3 > 0)
data_subset_3 <- data_subset[-oversized, ]

# Re-run GLM. Again, with that new, reduced dataset.
cite_glm_4 <- glm(citationcount ~ DLI + year + journalimpactfactor + teamsize + SAC+field_of_study_2 + sd_career_age + mean_career_age,
                  family = "poisson", 
                  data = data_subset_3)

cite_dfbeta_4 <- dfbeta(cite_glm_4)
dfbeta_threshold_4 <- 2/sqrt(nrow(data_subset_3))
# Any too-large DFBETA values?
any(abs(cite_dfbeta_4) > dfbeta_threshold_4)



