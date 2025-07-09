getwd()
setwd("/groups/sebratt/DLI_paper")
df <- read.csv2("/groups/sebratt/mrudanglangalia/Author_Overlap_[March-May][do not delete]/00_Master_files_[final versions][do_not_delete]/Reference_with_year_authoroverlap_Final.csv", sep=",", header=TRUE)
View(head(df))


colnames(df)
# precision: count of intersection / dataset authors
df$precision <- round(df$count_overlapping_names/df$count_datasets, 2)
summary(df$precision)

# there appears to be an issue with the count of overlapping authors. The values should be 0-1 
# The issue is not too bad: only ~11% are over 1:
d <- subset(df, df$precision > 1)
d
100*(nrow(d)/nrow(df))

d[13,445]
View(d[13,])

# okay, so we remove the rows with >1 precision?
# let's calculate recall first 
df$recall <-round(df$count_overlapping_names/df$count_publications, 2)
e <- subset(df, df$recall > 1)
e
100*(nrow(e)/nrow(df))
# great! only 13 rows with issues. Great Job Mrudang! V clever to format the names with lastNameFirstInitial in
# camelCase.

#F1 Score = 2 × (Precision × Recall) / (Precision + Recall)
df$f1 <- round(2*(df$precision*df$recall)/ (df$precision + df$recall),2)
head(df$f1)

f <- subset(df, df$f1 > 1)
f
100*(nrow(f)/nrow(df))
# only 10% so let's remove those rows.

df2 <- subset(df, df$f1 <= 1)
summary(df2$f1)
hist(df2$f1)


write.csv2(df2,"/groups/sebratt/DLI_paper/F1_DLI.csv")

#################################################
#
# CALCULATE Overlap Coefficient for DLI paper
#
#################################################
# Overlap Coefficient = |A ∩ B| / min(|A|, |B|)

#df2 <-read.csv2("/groups/sebratt/DLI_paper/F1_DLI.csv", sep=",")

df2$overlap_coefficient <- round((df2$count_overlapping_names)/pmin(df2$count_datasets, df2$count_publications),3)

summary(df2$overlap_coefficient) 
q <- subset(df2, df2$overlap_coefficient > 1)
q
100*(nrow(q)/nrow(df2))
# about 1%

df2 <- subset(df2, df2$overlap_coefficient <= 1)


#################################################
#
# CALCULATE % Percentage of Paper Authors Who Are Also Dataset Authors
#
#################################################

df2$percent_pub_authors_who_are_data_authors <- round(100*(df2$count_overlapping_names/df2$count_publications),2)
df2$percent_data_authors_who_are_pub_authors <- round(100*(df2$count_overlapping_names/df2$count_datasets),2)

summary(df2$percent_pub_authors_who_are_data_authors)

summary(df2$percent_data_authors_who_are_pub_authors)
hist(df2$percent_data_authors_who_are_pub_authors)
final_DLI_Df <- df2[,c("refid","index.x", "reference.x", "title.x","journal.x", "pubmed.x", "year_etc.x",
                              "index.y","reference.y","authors.y","title.y","journal.y","pubmed.y","year_etc.y",
"overlapping_names","non_overlapping_names","authors.y_dup","authors.y_dup2","count_overlapping_names",
"count_non_overlapping_names", "count_publications","count_datasets","precision","recall","f1","overlap_coefficient",
"percent_pub_authors_who_are_data_authors","percent_data_authors_who_are_pub_authors")]
colnames(final_DLI_Df)
nrow(final_DLI_Df)
View(head(final_DLI_Df))

write.csv2(final_DLI_Df,"/groups/sebratt/DLI_paper/final_DLI_Df.csv")
df2 <- read.csv2("/groups/sebratt/DLI_paper/final_DLI_Df.csv") #, sep=",", header=TRUE)
View(head(df2))

# # # # # # # # # # # # # # # # # # # # # # # #
# Merge with author_country_overlap:
# ref_id = ref_id 
# 
# # # # # # # # # # # # # # # # # # # # # # # # #

# Country, f1, overlap_coefficient, percent_data, and percent_pub
setwd("/groups/sebratt/mrudanglangalia/Combine_author_overlap_country[Sep][do not delete]/")

author_overlap_merged_df = read.csv("author_overlap_merged_df_to_be_uploaded.csv")
View(head(author_overlap_merged_df))
  
data <- merge(df2, author_overlap_merged_df, by="refid")
View(head(data))
colnames(data)
data2 <- data[,c("refid","count_overlapping_names.x", "count_non_overlapping_names.x","count_publications.x",
                "count_datasets.x","precision","recall","f1","overlap_coefficient","percent_pub_authors_who_are_data_authors",
                "percent_data_authors_who_are_pub_authors", "iso3c_combined_pub","sci_capacity_combined_pub",
                "income_class_combined_pub", "SAC_pub","SLC_pub","SDC_pub","HIC_pub","LIC_pub","LMIC_pub",
                "authors.y.y", "iso3c_combined_data","sci_capacity_combined_data", "income_class_combined_data", "SAC_data","SLC_data", "SDC_data", "HIC_data","LIC_data",
                "LMIC_data","HIC","LIC","LMIC","SAC","SLC","SDC","HIC_and_LIC", "HIC_and_LMIC","SAC_and_SLC",
                "SAC_and_SDC","DLI")]

write.csv(data2, "/groups/sebratt/DLI_paper/data2.csv")
data2 <- read.csv2("/groups/sebratt/DLI_paper/data2.csv", sep=",", header=TRUE)
View(head(data2))

regression_DLI_df_with_log <- read_delim("~/genbank2021/metadata-analytics/regression_DLI_df_with_log.csv", 
                                         +     delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(head(regression_DLI_df_with_log))

# merge on ssid to get the full df for regression

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
# Case Study 1: What are the team integration distributions for LIC and LMIC countries? 
# Step 1: create df for lowincome (south) and highincome (north) Filter for LIC and LMIC
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
install.packages("faraway")
library(faraway)

data2$HIC_and_LIC <- as.integer(as.logical(data2$HIC_and_LIC))
data2$HIC_and_LMIC <- as.integer(as.logical(data2$HIC_and_LMIC))
data2$SAC_and_SDC <- as.integer(as.logical(data2$SAC_and_SDC))
data2$SAC_and_SLC <- as.integer(as.logical(data2$SAC_and_SLC))

correlation_matrix <- cor(data2[, c("HIC_and_LIC", "HIC_and_LMIC", "SAC_and_SLC", "SAC_and_SDC")])
print(correlation_matrix)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
# Case Study 2: regression 
# 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
install.packages("car")
library(car)
DLIregressionModel <- lm(data = data2, formula = DLI ~ HIC_and_LIC + HIC_and_LMIC + SAC_and_SDC + SAC_and_SLC)
summary(DLIregressionModel)

f1regress <- lm(data = data2, formula = f1 ~ HIC_and_LIC + HIC_and_LMIC + SAC_and_SDC + SAC_and_SLC)
summary(f1regress)

Prec_regress <- lm(data = data2, formula = precision ~ HIC_and_LIC + HIC_and_LMIC + SAC_and_SDC + SAC_and_SLC)
summary(Prec_regress)

coeff_regress <- lm(data = data2, formula = overlap_coefficient ~ HIC_and_LIC + HIC_and_LMIC + SAC_and_SDC + SAC_and_SLC)
summary(coeff_regress)

vif_values <- car::vif(DLIregressionModel)
print(vif_values)

# SAC vs mixed (SAC + SDC + SLC)
# SLC vs mixed
# HIC vs mixed
# LIC vs mixed


