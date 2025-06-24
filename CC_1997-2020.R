#This file is for calculating collaboration capacity from 1997 to 2020.
#Qiaoyi Liu


library(readr)
library(dplyr)
library(splitstackshape)

file_paths <- list.files(path = "/home/qliu11/yearly_auths/", pattern = ".csv", full.names = TRUE)
df <- data.frame()

for (file_path in file_paths) {
  data <- read_csv(file_path)
  df <- rbind(df, data)
}
rm(data)
rm(file_paths)

# create one data frame with unique ssid and all author IDs in a single column separated by comma.
new_df_authid_all <- df %>%
  group_by(ssid, reference_year) %>%
  summarize(author_id = toString(unique(author_id))) 

# create one data frame with ungrouped author ids
new_df <- df %>%
  group_by(ssid, reference_year) %>%
  summarize(author_id = toString(unique(author_id))) %>%
  separate_rows(author_id, sep = ",")

rm(df)

#new_df <- arrange(new_df, reference_year)

new_df <- new_df[new_df$reference_year >= 1997 & new_df$reference_year <= 2020, ]
new_df_authid_all <- new_df_authid_all[new_df_authid_all$reference_year >= 1997 & new_df_authid_all$reference_year <= 2020, ]

output_path <- "/home/qliu11/CC/"

file_name1 <- paste0(output_path, "ssid_yr_groupedauthID.csv")
write.csv(new_df_authid_all, file = file_name1)

file_name2 <- paste0(output_path, "ssid_yr_authID.csv")
write.csv(new_df, file = file_name2)

rm(new_df_authid_all)
rm(new_df)
rm(file_name1)
rm(file_name2)
gc()




# merge with 1002 active authors. Exclude the rest
act.auth <- read_csv("/home/qliu11/active_authors_list_perform_classification/active_authid_name_initial_pub.csv")
pub <- read_csv("/home/qliu11/CC/ssid_yr_authID.csv")


act.auth.pub <- act.auth %>%
  left_join(pub, by = "author_id") %>%
  select(author_id, name, ssid, reference_year) 

write_csv(act.auth.pub, file = "/home/qliu11/CC/active_author_ssid_yr.csv")
  
act.auth.grppub <- act.auth.pub %>%
  group_by(author_id, name, reference_year) %>%
  summarize(ssid = toString(ssid)) 

write_csv(act.auth.pub, file = "/home/qliu11/CC/active_author_groupedssid_yr.csv")

rm(act.auth)
rm(pub)
rm(act.auth.grppub)
gc()

#-------------------------there's duplicated ssids in different years i.e. different windows. 
#IMPORTANT DECISION#
#By tracing back to the raw data, the reason for having two rows of the same ssid is they have different GenBankIDs.
#This is bc one of them in the NCBI db shows the paper's status is "in press". Since we explore researchers' collaborations. 
#The "in press" status of the paper would not change the fact that authors already collaborated. Thus we use the earlier year data, 
#exclude the latter year. 

all.ssid.authid <- read_csv("/home/qliu11/CC/ssid_yr_groupedauthID.csv")

all.ssid.authid <- all.ssid.authid %>%
  rename(author_ids = author_id)

#all.ssid.authid[duplicated(all.ssid.authid$ssid),]
#print(all.ssid.authid[all.ssid.authid$ssid == "30f99f99ac287b09b89a12aa49d7e565b33732f0",])

dup.all.ssid.authid <- all.ssid.authid[duplicated(all.ssid.authid$ssid) | duplicated(all.ssid.authid$ssid, fromLast = TRUE), ]
dup.all.ssid.authid <- dup.all.ssid.authid %>% 
  arrange(ssid)

dup.all.ssid.authid <- dup.all.ssid.authid %>%
  mutate(window = case_when(
    reference_year >= 1997 & reference_year <= 1999 ~ "window1",
    reference_year >= 2000 & reference_year <= 2002 ~ "window2",
    reference_year >= 2003 & reference_year <= 2005 ~ "window3",
    reference_year >= 2006 & reference_year <= 2008 ~ "window4",
    reference_year >= 2009 & reference_year <= 2011 ~ "window5",
    reference_year >= 2012 & reference_year <= 2014 ~ "window6",
    reference_year >= 2015 & reference_year <= 2017 ~ "window7",
    reference_year >= 2018 & reference_year <= 2020 ~ "window8"
  ))

dup.all.ssid.authid <- dup.all.ssid.authid %>%
  group_by(ssid) %>%
  summarize(window = toString(window), reference_year = toString(reference_year)) 

#There are 267 ssids with two different years. 
dup.all.ssid.authid <- cSplit(dup.all.ssid.authid, "window", ",", direction = "wide")
write.csv(dup.all.ssid.authid, file = "/home/qliu11/CC/dupssid.csv")

#Subset from the 267 ssids, there are 64 ssids with two different years that r also in DIFFERENT WINDOWS.
result <- dup.all.ssid.authid %>%
  filter(window_1 != window_2 | is.na(window_1) | is.na(window_2))

write.csv(result, file = "/home/qliu11/CC/dupssid_with_diff_win.csv")


rm(result)
rm(dup.all.ssid.authid)
gc()



dupssid.diff.win <- read_csv("/home/qliu11/CC/dupssid_with_diff_win.csv")

file_paths <- list.files(path = "/home/qliu11/yearly_auths/", pattern = ".csv", full.names = TRUE)
df <- data.frame()

for (file_path in file_paths) {
  data <- read_csv(file_path)
  df <- rbind(df, data)
}

dupssid.diff.win <- dupssid.diff.win %>%
  left_join(df, by = "ssid") 

rm(data)
rm(file_paths)

dupssid.diff.win <- dupssid.diff.win[, -6]
dupssid.diff.win <- dupssid.diff.win[, -7]
dupssid.diff.win <- dupssid.diff.win[, -c(9:13)]
dupssid.diff.win <- distinct(dupssid.diff.win)

write_csv(dupssid.diff.win, file = "/home/qliu11/CC/dupssid_with_diff_win_GBID.csv")
rm(dupssid.diff.win)
gc()

#the 64 ssids with two different windows and all their GBIDs.
dupssid.diff.win <- read_csv("/home/qliu11/CC/dupssid_with_diff_win_GBID.csv")
dupssid.diff.win.gbcheck <- read_csv("/home/qliu11/CC/dupssid_with_diff_win_GBID_NCBIcheck.csv")

dupssid.diff.win <- dupssid.diff.win %>%
  left_join(dupssid.diff.win.gbcheck %>% select(Manual_check_yr, Genbank_ids), by = 'Genbank_ids')

write_csv(dupssid.diff.win, file = "/home/qliu11/CC/dupssid_with_diff_win_GBID.csv")
#-------------------------



#-------------------------------
all.ssid.authid <- all.ssid.authid[!duplicated(all.ssid.authid$ssid), ]
nrow(all.ssid.authid)
#after checking GBID of duplicated ssid, we use the earlier year for all duplicated ssids. 
#Exclude duplicated ssids with two incontinuous years. 

all.ssid.authid <- all.ssid.authid[all.ssid.authid[["ssid"]] != "40775f3a340936f766652504afc67c018f36b268", ]
all.ssid.authid <- all.ssid.authid[all.ssid.authid[["ssid"]] != "aa08838e75d3dfac911ff229fefb1ea7aec37542", ]


act.auth.pub.allauthid <- act.auth.pub %>%
  left_join(all.ssid.authid, by = "ssid") 

#run this twice
act.auth.pub.allauthid <- act.auth.pub.allauthid[,-5]

#exclude the active author id himself/herself from the author_ids column
for (i in 1: nrow(act.auth.pub.allauthid)) {
  value_to_remove <- paste0(act.auth.pub.allauthid$author_id[i], ", ")
  act.auth.pub.allauthid$author_ids[i] <- gsub(value_to_remove, "", act.auth.pub.allauthid$author_ids[i])
  rm(value_to_remove)
}

act.auth.pub.allauthid <- act.auth.pub.allauthid %>%
  rename(reference_year = reference_year.x)

act.auth.pub.allauthid <- act.auth.pub.allauthid %>%
  mutate(window = case_when(
    reference_year >= 1997 & reference_year <= 1999 ~ "window1",
    reference_year >= 2000 & reference_year <= 2002 ~ "window2",
    reference_year >= 2003 & reference_year <= 2005 ~ "window3",
    reference_year >= 2006 & reference_year <= 2008 ~ "window4",
    reference_year >= 2009 & reference_year <= 2011 ~ "window5",
    reference_year >= 2012 & reference_year <= 2014 ~ "window6",
    reference_year >= 2015 & reference_year <= 2017 ~ "window7",
    reference_year >= 2018 & reference_year <= 2020 ~ "window8"
  ))

#check if every row falls in a window.If result has 0 rows than every row has a window.
result <- act.auth.pub.allauthid[is.na(act.auth.pub.allauthid$window),]
rm(result)
  
act.auth.pub.allauthid <- act.auth.pub.allauthid %>%
  group_by(author_id, name, window) %>%
  summarize(author_ids = toString(author_ids)) 
#at this point act.auth.pub.allauthid should have 8016 rows. Every author has 8 rows for window 1-8.

#remove all duplicated coauthorids in each window for each author.
act.auth.pub.allauthid <- act.auth.pub.allauthid %>%
  separate_rows(author_ids, sep = ", ")

act.auth.pub.allauthid <- act.auth.pub.allauthid %>%
  group_by(author_id, name, window) %>%
  distinct(author_ids, .keep_all = TRUE)

act.auth.pub.allauthid <- act.auth.pub.allauthid %>%
  group_by(author_id, name, window) %>%
  summarize(author_ids = paste(author_ids, collapse = ", "))


act.auth.pub.allauthid <- act.auth.pub.allauthid %>%
  spread(key = window, value = author_ids)

write.csv(act.auth.pub.allauthid, file = "/home/qliu11/CC/act_auth_ccauth_win.csv")

rm(act.auth.pub)
rm(all.ssid.authid)
gc()

#-------------------calculate CC
act.auth.pub.allauthid <- read_csv("/home/qliu11/CC/act_auth_ccauth_win.csv")

#row <- 1
#i <- 4
#i <- 5
#strsplit(act.auth.pub.allauthid[[1,5]], ",")


for (i in c(4:10)) {
  
  if (i == 4) {
    for (row in 1:nrow(act.auth.pub.allauthid)) {
      act.auth.pub.allauthid[row, "window1_numcoauth"] <- length(unlist(strsplit(act.auth.pub.allauthid[[row, i]], ",")))
      
      allcoauth_ids <- list()
      row_ids <- list()
      new_ids <- vector("character", length = 0)
      
      current_ids <- c(new_ids, unlist(strsplit(act.auth.pub.allauthid[[row, i]], ",")))
      
      next_ids <- unlist(strsplit(act.auth.pub.allauthid[[row, i + 1]], ","))
      new_ids <- setdiff(next_ids, current_ids)
      #print(length(new_ids))
      
      allcoauth_ids <- union(current_ids, new_ids)
      #print(length(allcoauth_ids))
      
      act.auth.pub.allauthid[row, paste0("window", i-2, "_numnewcoauth")] <- length(new_ids)
      act.auth.pub.allauthid[row, paste0("window", i-2,"_sumcoauth")] <- paste(allcoauth_ids, collapse = ",")
    }
  } else {
    for (row in 1:nrow(act.auth.pub.allauthid)) {
      allcoauth_ids <- list()
      row_ids <- list()
      new_ids <- vector("character", length = 0)
      
      current_ids <- unlist(strsplit(act.auth.pub.allauthid[[row, paste0("window", i-3, "_sumcoauth")]], ","))
      
      next_ids <- unlist(strsplit(act.auth.pub.allauthid[[row, i + 1]], ","))
      new_ids <- setdiff(next_ids, current_ids)
      #print(length(new_ids))
      
      allcoauth_ids <- union(current_ids, new_ids)
      #print(length(allcoauth_ids))
      
      act.auth.pub.allauthid[row, paste0("window", i-2, "_numnewcoauth")] <- length(new_ids)
      
      if (i == 10) {
        act.auth.pub.allauthid[row, paste0("allcoauth")] <- paste(allcoauth_ids, collapse = ",")
        act.auth.pub.allauthid[row, "total_num_coauth"] <- length(allcoauth_ids)
      } else {
        act.auth.pub.allauthid[row, paste0("window", i-2,"_sumcoauth")] <- paste(allcoauth_ids, collapse = ",")
      }
    }
  }
  
  print(paste("window", i-2, "is complete"))
}

for (j in 1:nrow(act.auth.pub.allauthid)){
  act.auth.pub.allauthid[j, "check_total_num_coauth"] <- act.auth.pub.allauthid$window1_numcoauth[j] +
    act.auth.pub.allauthid$window2_numnewcoauth[j] + act.auth.pub.allauthid$window3_numnewcoauth[j] +
    act.auth.pub.allauthid$window4_numnewcoauth[j] + act.auth.pub.allauthid$window5_numnewcoauth[j] +
    act.auth.pub.allauthid$window6_numnewcoauth[j] + act.auth.pub.allauthid$window7_numnewcoauth[j] +
    act.auth.pub.allauthid$window8_numnewcoauth[j]
}

round(summary(act.auth.pub.allauthid$total_num_coauth))

#CC is defined as the average number of new co-authors across 8 windows
act.auth.pub.allauthid$aver_newco <- round(act.auth.pub.allauthid$total_num_coauth/8, 2)
write_csv(act.auth.pub.allauthid, file = "/home/qliu11/CC/act.auth.cc.1997.2020.csv")



rm(new_ids)
rm(current_ids)
rm(next_ids)
gc()


#merge with performance categories
class <- read_csv("/home/qliu11/active_authors_list_perform_classification/act_auth_pub_8yrwindow_classification.csv")
class[["performance"]] <- sub('consistently high', 'ConsistentlyHighPerf', class[["performance"]])
class[["performance"]] <- sub('consistently low', 'ConsistentlyLowPerf', class[["performance"]])
class[["performance"]] <- sub('high to low', 'HighToLow', class[["performance"]])
class[["performance"]] <- sub('low to high', 'LowToHigh', class[["performance"]])
class[["performance"]] <- sub('consistently median', 'typical', class[["performance"]])
print(unique(class[["performance"]]))

act.auth.pub.allauthid <- read_csv("/home/qliu11/CC/act.auth.cc.1997.2020.csv")

act.auth.CC.pubproduct <- left_join(act.auth.pub.allauthid, class, keep = TRUE, by = "author_id")
act.auth.CC.pubproduct$P_3YrAvg2 <- rowSums(act.auth.CC.pubproduct[, c("num_ssid_2000", "num_ssid_2001", "num_ssid_2002")], na.rm = TRUE)
act.auth.CC.pubproduct <- act.auth.CC.pubproduct %>% mutate(dummy=1) %>%
  spread(key=performance,value=dummy, fill=0)

write.csv(act.auth.CC.pubproduct, "/home/qliu11/CC/act.auth.CC.perforcat.csv")
rm(class)
rm(act.auth.pub.allauthid)
rm(act.auth.CC.pubproduct)
gc()
