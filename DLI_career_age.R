# 09-27-2024
# Sarah Bratt
# Calculate Career age for each AUTHOR_ID (author)
# Calculate mean, max, and sd career age for each SSID (paper)

getwd()

setwd("C:/Users/sebratt/Downloads/")
dli <- read.csv2("career_age_calculations_table.csv", sep=",", header = TRUE)
dli$career_age <- dli$year - dli$earliest_year

View(head(dli))
summary(dli$career_age)

dliMAX <- dli[which(dli$career_age > 40),] #legit

mean_career <- aggregate(x = dli$career_age, by = list(dli$ssid), FUN = mean)
colnames(mean_career)[1] <- "ssid"
colnames(mean_career)[2] <- "mean_career_age"

sd_career <- aggregate(x = dli$career_age, by = list(dli$ssid), FUN = sd)
colnames(sd_career)[1] <- "ssid"
colnames(sd_career)[2] <- "sd_career_age"

career <- merge(x=dli, y = sd_career, by = "ssid")
View(head(career))
career$sd_career_age <- round(career$sd_career_age, digits = 2)
career_final <- merge(x= career, y=mean_career)
View(head(career_final))
career_final$mean_career_age <- round(career_final$mean_career_age, digits = 0)

write.csv2(career_final, "career_age_final.csv")

# merge with regression data
setwd("C:/Users/sebratt/Downloads/")
career_final <- read.csv2("career_age_final.csv")
jif <- read.csv2("processed_merged_file.csv", sep=",", header = TRUE)
colnames(jif)
View(head(jif))

redux_jif <- jif[,c("ssid","DLI","count_publications","count_datasets",
                    "year","field_of_study_2","author_id",
                    "sci_capacity_combined_pub","income_class_combined_pub","citationcount")]

setwd("C:/Users/sebratt/DropBox/")
write.csv(redux_jif, "redux_jif.csv")
#rm(jif)
redux_jif <- read.csv2("redux_jif.csv", sep=",", header=TRUE)                 
rm(h.index.data)


career_jif <- merge(x= redux_jif, y = career_final, by = "ssid")
View(head(career_jif))
write.csv(career_jif, "career_jif.csv")

rm(jif)
rm(redux_jif)
rm(career_final)
gc()

View(head(career_jif))
career_jif$X.x <- NULL
career_jif$X.y <- NULL
career_jif$year.x <- NULL
colnames(career_jif)[10] <- "year"
career_jif$authors_id <- NULL

career_jif2 <- career_jif[!duplicated(career_jif$ssid),]
rm(career_jif)

# Read in Danu's data with complete h-index info!
h.index.data <- read.csv2("C:/Users/sebratt/Downloads/DLIratio_combined_file_2.csv", sep=",", header=TRUE)
View(tail(h.index.data))
colnames(h.index.data)

# select only necessary columns
h.index.data_redux <- h.index.data[,c("id", "journalimpactfactor")]
colnames(h.index.data_redux)[1] <- "ssid"
View(head(h.index.data_redux))


final_df <- merge(x= career_jif2, y = h.index.data_redux, by = "ssid")

final_df$DLI <- round(as.numeric(final_df$DLI), digits = 2)
final_df$career_age <- NULL
final_df$earliest_year <- NULL


View(head(final_df))
write.csv(final_df, "final_df_DLI_regression.csv")
