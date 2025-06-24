#This file is for building the performance classification using the active authors' publication
#Joy

library(tidyverse)
library(ggplot2)

output_path1 <- "/home/qliu11/yearly_auths/"
output_path2 <- "/home/qliu11/active_authors_list_perform_classification/"


#Merge active author list with all authors, num of publication per year.
all.auth.pub <- read_csv("/home/qliu11/yearly_auths_pub_num_ssid/author_pub_1997_2020.csv")
all.auth.pub <- all.auth.pub[, -1]
act.auth <- read_csv("/home/qliu11/active_authors_list_perform_classification/active.authors.csv")

all.authid.name.pub <- merge(all.auth.pub, act.auth, act.auth, by.x = "author_id", by.y = "authorID", all.x = TRUE)
all.authid.name.pub <- all.authid.name.pub[, c(ncol(all.authid.name.pub), 1:(ncol(all.authid.name.pub) - 1))]

View(all.authid.name.pub)

file_name <- paste0(output_path1, "authid_name_pub_all.csv")
write.csv(all.authid.name.pub, file = file_name)

all.authid.name.pub <- all.authid.name.pub[!is.na(all.authid.name.pub$name),]

file_name <- paste0(output_path2, "active_authid_name_pub.csv")
write.csv(all.authid.name.pub, file = file_name)

rm(act.auth)
rm(all.auth.pub)
rm(all.authid.name.pub)
gc()

#Split active authors publications by year into 3 eight-year windows

act.auth.pub <- read_csv("/home/qliu11/active_authors_list_perform_classification/active_authid_name_pub.csv")
act.auth.pub <- act.auth.pub[, -1]
View(act.auth.pub)

window1 <- act.auth.pub[,c("num_ssid_1997", "num_ssid_1998", "num_ssid_1999", "num_ssid_2000", "num_ssid_2001", 
                           "num_ssid_2002", "num_ssid_2003", "num_ssid_2004")]
window2 <- act.auth.pub[,c("num_ssid_2005", "num_ssid_2006", "num_ssid_2007", "num_ssid_2008", "num_ssid_2009", 
                           "num_ssid_2010", "num_ssid_2011", "num_ssid_2012")]
window3 <- act.auth.pub[,c("num_ssid_2013", "num_ssid_2014", "num_ssid_2015", "num_ssid_2016", "num_ssid_2017",
                           "num_ssid_2018", "num_ssid_2019", "num_ssid_2020")]

window_1_pub_sum <- rowSums(window1, na.rm = TRUE)
window_2_pub_sum <- rowSums(window2, na.rm = TRUE)
window_3_pub_sum <- rowSums(window3, na.rm = TRUE)

act.auth.pub$window_1_pub_sum <- window_1_pub_sum
act.auth.pub$window_2_pub_sum <- window_2_pub_sum
act.auth.pub$window_3_pub_sum <- window_3_pub_sum


#Quartile
act.auth.pub$win1_1q <- summary(act.auth.pub$window_1_pub_sum)[2]
act.auth.pub$win1_3q <- summary(act.auth.pub$window_1_pub_sum)[5]

act.auth.pub$win2_1q <- summary(act.auth.pub$window_2_pub_sum)[2]
act.auth.pub$win2_3q <- summary(act.auth.pub$window_2_pub_sum)[5]

act.auth.pub$win3_1q <- summary(act.auth.pub$window_3_pub_sum)[2]
act.auth.pub$win3_3q <- summary(act.auth.pub$window_3_pub_sum)[5]


act.auth.pub$win1_category <- ifelse(act.auth.pub$window_1_pub_sum <= win1_1q, 'low',
                                      ifelse(act.auth.pub$window_1_pub_sum >= win1_3q, 'high', 'median'))

act.auth.pub$win2_category <- ifelse(act.auth.pub$window_2_pub_sum <= win2_1q, 'low',
                                      ifelse(act.auth.pub$window_2_pub_sum >= win2_3q, 'high', 'median'))

act.auth.pub$win3_category <- ifelse(act.auth.pub$window_3_pub_sum <= win3_1q, 'low',
                                      ifelse(act.auth.pub$window_3_pub_sum >= win3_3q, 'high', 'median'))
View(act.auth.pub)


#Percentage
category_t_win1 <- table(act.auth.pub$win1_category)
category_p_win1 <- (category_t_win1 / sum(category_t_win1)) * 100
print(category_p_win1)

category_t_win2 <- table(act.auth.pub$win2_category)
category_p_win2 <- (category_t_win2 / sum(category_t_win2)) * 100
print(category_p_win2)

category_t_win3 <- table(act.auth.pub$win3_category)
category_p_win3 <- (category_t_win3 / sum(category_t_win3)) * 100
print(category_p_win3)


#All three window classification
act.auth.pub$performance <- ifelse(
  (act.auth.pub$win1_category == "median" & act.auth.pub$win2_category == "high" & act.auth.pub$win3_category == "low" ) |
    (act.auth.pub$win1_category == "high" & act.auth.pub$win2_category == "low" & act.auth.pub$win3_category == "low") |
    (act.auth.pub$win1_category == "high" & act.auth.pub$win2_category == "low" & act.auth.pub$win3_category == "median") |
    (act.auth.pub$win1_category == "high" & act.auth.pub$win2_category == "median" & act.auth.pub$win3_category == "low") |
    (act.auth.pub$win1_category == "high" & act.auth.pub$win2_category == "median" & act.auth.pub$win3_category == "median") |
    (act.auth.pub$win1_category == "high" & act.auth.pub$win2_category == "high" & act.auth.pub$win3_category == "low") |
    (act.auth.pub$win1_category == "high" & act.auth.pub$win2_category == "high" & act.auth.pub$win3_category == "median") ,
  "high to low",
  ifelse(
    (act.auth.pub$win1_category == "low" & act.auth.pub$win2_category == "low" & act.auth.pub$win3_category == "high") |
      (act.auth.pub$win1_category == "low" & act.auth.pub$win2_category == "median" & act.auth.pub$win3_category == "high") |
      (act.auth.pub$win1_category == "low" & act.auth.pub$win2_category == "high" & act.auth.pub$win3_category == "high") |
      (act.auth.pub$win1_category == "median" & act.auth.pub$win2_category == "low" & act.auth.pub$win3_category == "high") |
      (act.auth.pub$win1_category == "median" & act.auth.pub$win2_category == "median" & act.auth.pub$win3_category == "high"),
    "low to high",
    ifelse(
      (act.auth.pub$win1_category == "low" & act.auth.pub$win2_category == "low" & act.auth.pub$win3_category == "median") |
        (act.auth.pub$win1_category == "low" & act.auth.pub$win2_category == "median" & act.auth.pub$win3_category == "median") |
        (act.auth.pub$win1_category == "low" & act.auth.pub$win2_category == "high" & act.auth.pub$win3_category == "median") |
        (act.auth.pub$win1_category == "median" & act.auth.pub$win2_category == "low" & act.auth.pub$win3_category == "median") |
        (act.auth.pub$win1_category == "median" & act.auth.pub$win2_category == "median" & act.auth.pub$win3_category == "median") |
        (act.auth.pub$win1_category == "median" & act.auth.pub$win2_category == "high" & act.auth.pub$win3_category == "median"),
      "consistently median",
      ifelse(
        (act.auth.pub$win1_category == "low" & act.auth.pub$win2_category == "low" & act.auth.pub$win3_category == "low") |
          (act.auth.pub$win1_category == "low" & act.auth.pub$win2_category == "median" & act.auth.pub$win3_category == "low") |
          (act.auth.pub$win1_category == "low" & act.auth.pub$win2_category == "high" & act.auth.pub$win3_category == "low") |
          (act.auth.pub$win1_category == "median" & act.auth.pub$win2_category == "low" & act.auth.pub$win3_category == "low") |
          (act.auth.pub$win1_category == "median" & act.auth.pub$win2_category == "median" & act.auth.pub$win3_category == "low"),
        "consistently low",
        ifelse(
          (act.auth.pub$win1_category == "median" & act.auth.pub$win2_category == "high" & act.auth.pub$win3_category == "high") |
            (act.auth.pub$win1_category == "high" & act.auth.pub$win2_category == "low" & act.auth.pub$win3_category == "high") |
            (act.auth.pub$win1_category == "high" & act.auth.pub$win2_category == "median" & act.auth.pub$win3_category == "high") |
            (act.auth.pub$win1_category == "high" & act.auth.pub$win2_category == "high" & act.auth.pub$win3_category == "high"),
          "consistently high",
          NA
        )
      )
    )
  )
)

View(act.auth.pub)

category_per <- table(act.auth.pub$performance)
performance_percentage <- (category_per / sum(category_per)) * 100
print(performance_percentage)

output_path <- "/home/qliu11/active_authors_list_perform_classification/"
filename <- paste0(output_path, "act_auth_pub_8yrwindow_classification.csv")
write_csv(act.auth.pub, file = filename)

class <- read_csv("/home/qliu11/active_authors_list_perform_classification/act_auth_pub_8yrwindow_classification.csv")
#REPORT THIS!!!!!
round(table(class$performance)/sum(as.numeric(table(class$performance))),3)




#04/26/2024 try line plots: x = performance window, y = num of publication
act.auth.perf <- read.csv("/home/qliu11/active_authors_list_perform_classification/act_auth_pub_8yrwindow_classification.csv")

library(ggplot2)
library(tidyverse)


#low to high
auth.lowtohigh <- subset(act.auth.perf, performance == "low to high")
auth.lowtohigh <- auth.lowtohigh[, c(1,2, 27:29)]


auth.lowtohigh <- auth.lowtohigh %>%
  pivot_longer(cols = starts_with("window"),
               names_to = "window",
               values_to = "publication") %>%
  mutate(window = as.numeric(str_extract(window, "\\d+")))

#one extremely large num of pub in window 2, ID = 2805081
auth.lowtohigh <- subset(auth.lowtohigh, author_id != "2805081")


ggplot(auth.lowtohigh) + aes(x = window, y = publication, color = name) +
  geom_line() +
  theme_minimal() +
  ylim(c(0,75)) +
  scale_color_manual(values = rep("#aa770044", 97)) +
  theme(legend.position="none") +
  scale_x_continuous(breaks = seq(1, max(auth.lowtohigh$window), by = 1))


#high to low
auth.hightolow <- subset(act.auth.perf, performance == "high to low")
auth.hightolow <- auth.hightolow[, c(1,2, 27:29)]


auth.hightolow <- auth.hightolow %>%
  pivot_longer(cols = starts_with("window"),
               names_to = "window",
               values_to = "publication") %>%
  mutate(window = as.numeric(str_extract(window, "\\d+")))

#one extremely large num of pub in window 2, ID = 145282802
auth.hightolow <- subset(auth.hightolow, author_id != "145282802")


ggplot(auth.hightolow) + aes(x = window, y = publication, color = name) +
  geom_line() +
  theme_minimal() +
  ylim(c(0,125)) +
  scale_color_manual(values = rep("#aa770044", nrow(auth.hightolow)/3)) +
  theme(legend.position="none") +
  scale_x_continuous(breaks = seq(1, max(auth.hightolow$window), by = 1))


#consistently high
auth.consistentlyhigh <- subset(act.auth.perf, performance == "consistently high")
auth.consistentlyhigh <- auth.consistentlyhigh[, c(1,2, 27:29)]


auth.consistentlyhigh <- auth.consistentlyhigh %>%
  pivot_longer(cols = starts_with("window"),
               names_to = "window",
               values_to = "publication") %>%
  mutate(window = as.numeric(str_extract(window, "\\d+")))

#one extremely large num of pub in window 2, ID = 145282802
auth.consistentlyhigh <- subset(auth.consistentlyhigh, author_id != "145282802")


ggplot(auth.consistentlyhigh) + aes(x = window, y = publication, color = name) +
  geom_line() +
  theme_minimal() +
  ylim(c(0,300)) +
  scale_color_manual(values = rep("#aa770044", nrow(auth.consistentlyhigh)/3)) +
  theme(legend.position="none") +
  scale_x_continuous(breaks = seq(1, max(auth.consistentlyhigh$window), by = 1))



act.auth.perf[["performance"]] <- sub('consistently high', 'ConsistentlyHighPerf', act.auth.perf[["performance"]])
act.auth.perf[["performance"]] <- sub('consistently low', 'ConsistentlyLowPerf', act.auth.perf[["performance"]])
act.auth.perf[["performance"]] <- sub('high to low', 'HighToLow', act.auth.perf[["performance"]])
act.auth.perf[["performance"]] <- sub('low to high', 'LowToHigh', act.auth.perf[["performance"]])
act.auth.perf[["performance"]] <- sub('consistently median', 'typical', act.auth.perf[["performance"]])

window1 <- act.auth.perf[,c("num_ssid_1997", "num_ssid_1998", "num_ssid_1999")]
window2 <- act.auth.perf[,c("num_ssid_2000", "num_ssid_2001", "num_ssid_2002")]
window3 <- act.auth.perf[,c("num_ssid_2003", "num_ssid_2004", "num_ssid_2005")]
window4 <- act.auth.perf[,c("num_ssid_2006", "num_ssid_2007", "num_ssid_2008")]
window5 <- act.auth.perf[,c("num_ssid_2009", "num_ssid_2010", "num_ssid_2011")]
window6 <- act.auth.perf[c("num_ssid_2012", "num_ssid_2013", "num_ssid_2014")]
window7 <- act.auth.perf[,c("num_ssid_2015", "num_ssid_2016", "num_ssid_2017")]
window8 <- act.auth.perf[,c("num_ssid_2018", "num_ssid_2019", "num_ssid_2020")]


act.auth.perf$window_1_pub_sum <- rowSums(window1, na.rm = TRUE)
act.auth.perf$window_2_pub_sum <- rowSums(window2, na.rm = TRUE)
act.auth.perf$window_3_pub_sum <- rowSums(window3, na.rm = TRUE)
act.auth.perf$window_4_pub_sum <- rowSums(window4, na.rm = TRUE)
act.auth.perf$window_5_pub_sum <- rowSums(window5, na.rm = TRUE)
act.auth.perf$window_6_pub_sum <- rowSums(window6, na.rm = TRUE)
act.auth.perf$window_7_pub_sum <- rowSums(window7, na.rm = TRUE)
act.auth.perf$window_8_pub_sum <- rowSums(window8, na.rm = TRUE)

act.auth.perf <- act.auth.perf[, c(1,2, 27:29, 40:44, 39)]
act.auth.perf <- act.auth.perf%>%
  pivot_longer(cols = starts_with("window"),
               names_to = "window",
               values_to = "publication") %>%
  mutate(window = as.numeric(str_extract(window, "\\d+")))


write.csv(act.auth.perf, "/home/qliu11/active_authors_list_perform_classification/act.auth.sumofPub.8Yr.perfCat.csv")


#------------------
act.auth.perf <- read.csv("/home/qliu11/active_authors_list_perform_classification/act.auth.sumofPub.8Yr.perfCat.csv")
act.auth.perf$performance <- gsub("typical", "Typical", act.auth.perf$performance)

c("HighToLow" = "#aa770044", "LowToHigh" = "#cc79a744",
  "ConsistentlyHighPerf" = "#0072b244", "ConsistentlyLowPerf" = "#009e7344",
  "typical" = "#99999955")

trend_data <- act.auth.perf %>%
  group_by(performance) %>%
  do(data.frame(window = unique(.$window), 
                publication = predict(lm(publication ~ window, data = .), newdata = data.frame(window = unique(.$window)))))

ggplot(trend_data, aes(window, publication, color = performance, group = performance)) +
  geom_line() +
  theme_light() + 
  xlab("Windows (1997-2020)") + ylab("Number of publications") +
  labs(color = paste0("Performance", "\n", "Categories")) +
  coord_cartesian(xlim = c(1,8)) +
  scale_x_continuous(breaks = c(1:8)) +
  scale_color_manual(values = c("HighToLow" = "#aa7700", "LowToHigh" = "#cc79a7",
                                "ConsistentlyHighPerf" = "#0072b2", "ConsistentlyLowPerf" = "#009e73",
                                "Typical" = "#999999")) +
  theme(legend.position="none")

#slope for trend line
slope_data <- act.auth.perf %>%
  group_by(performance) %>%
  do({
    lm_fit <- lm(publication ~ window, data = .)
    data.frame(performance = .$performance[1], slope = coef(lm_fit)[["window"]], name = .$performance[1])
  }) %>%
  ungroup()




#typical to uppercase
ggplot(act.auth.perf) + aes(x = window, y = publication, color = performance, group = name) +
  geom_line() +
  geom_line(data = trend_data, aes(group = performance), color = "white", linetype = "longdash") +
  #theme_light() +
  theme_classic() +
  ylim(c(0,70)) +
  xlab("Windows (1997-2020)") + ylab("Number of Publications") +
  scale_color_manual(values = c("HighToLow" = "#aa770044", "LowToHigh" = "#cc79a744",
                                "ConsistentlyHighPerf" = "#0072b244", "ConsistentlyLowPerf" = "#009e7344",
                                "Typical" = "#99999955")) +
  theme(legend.position="none", panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()) +
  scale_x_continuous(breaks = seq(1, max(act.auth.perf$window), by = 1)) +
  #facet_grid(.~performance)
  facet_wrap(~performance, ncol = 3, scales = "free") +
  geom_text(data = slope_data, aes(label = paste0(" ", slope_data$performance, "\n", " Slope = ", 
                                                  round(slope_data$slope, 3)), x = -Inf, y = Inf), hjust = 0, vjust = 1, 
            size = 3, color = "black", show.legend = FALSE) +
  theme(strip.text.y = element_blank(), strip.text.x = element_blank()) + 
  #labs(title = "Number of Publications Against 3-Year Windows") +
  labs(color = "Performance Category")

g + ggplot(trend_data) +
  geom_text(aes(label = slope), hjust = -0.1, vjust = 0.5, size = 3)



act.auth.perf.f <- act.auth.perf[act.auth.perf$performance != "ConsistentlyLowPerf",]
act.auth.perf.f <- act.auth.perf.f[act.auth.perf.f$performance != "typical",]

ggplot(act.auth.perf.f) + aes(x = window, y = publication, color = performance, group = name) +
  geom_line() +
  theme_minimal() +
  ylim(c(0,50)) +
  scale_color_manual(values = c("HighToLow" = "#aa770044", "LowToHigh" = "#cc79a744",
                                "ConsistentlyHighPerf" = "#0072b244")) +
  theme(legend.position="bottom") +
  scale_x_continuous(breaks = seq(1, max(act.auth.perf.f$window), by = 1))


#high to low
act.auth.perf.f <- act.auth.perf[act.auth.perf$performance == "HighToLow",]

ggplot(act.auth.perf.f) + aes(x = window, y = publication, color = performance, group = name) +
  geom_line() +
  theme_minimal() +
  ylim(c(0,50)) +
  scale_color_manual(values = c("HighToLow" = "#aa770044")) +
  theme(legend.position="bottom") +
  scale_x_continuous(breaks = seq(1, max(act.auth.perf.f$window), by = 1))


#low to high
act.auth.perf.f <- act.auth.perf[act.auth.perf$performance == "LowToHigh",]

ggplot(act.auth.perf.f) + aes(x = window, y = publication, color = performance, group = name) +
  geom_line() +
  theme_minimal() +
  ylim(c(0,75)) +
  scale_color_manual(values = c("LowToHigh" = "#cc79a744")) +
  theme(legend.position="bottom") +
  scale_x_continuous(breaks = seq(1, max(act.auth.perf.f$window), by = 1))


#consistently high
act.auth.perf.f <- act.auth.perf[act.auth.perf$performance == "ConsistentlyHighPerf",]

ggplot(act.auth.perf.f) + aes(x = window, y = publication, color = performance, group = name) +
  geom_line() +
  theme_minimal() +
  ylim(c(0,125)) +
  scale_color_manual(values = c("ConsistentlyHighPerf" = "#0072b244")) +
  theme(legend.position="bottom") +
  scale_x_continuous(breaks = seq(1, max(act.auth.perf.f$window), by = 1))


#consistently low
act.auth.perf.f <- act.auth.perf[act.auth.perf$performance == "ConsistentlyLowPerf",]

ggplot(act.auth.perf.f) + aes(x = window, y = publication, color = performance, group = name) +
  geom_line() +
  theme_minimal() +
  ylim(c(0,40)) +
  scale_color_manual(values = c("ConsistentlyLowPerf" = "#009e7344")) +
  theme(legend.position="bottom") +
  scale_x_continuous(breaks = seq(1, max(act.auth.perf.f$window), by = 1))


#typical
act.auth.perf.f <- act.auth.perf[act.auth.perf$performance == "typical",]

ggplot(act.auth.perf.f) + aes(x = window, y = publication, color = performance, group = name) +
  geom_line() +
  theme_minimal() +
  ylim(c(0,40)) +
  scale_color_manual(values = c("typical" = "#99999955")) +
  theme(legend.position="bottom") +
  scale_x_continuous(breaks = seq(1, max(act.auth.perf.f$window), by = 1))









