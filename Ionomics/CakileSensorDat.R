library(tidyverse)
library(lubridate)
library(cowplot)

setwd("~/Downloads/CakilePrelim/CakileSensorDat/allcond/")


cond1 <- read_tsv("newcond1.txt")
cond2 <- read_tsv("newcond2.txt")
cond3 <- read_tsv("newcond3.txt")
cond4 <- read_tsv("newcond4.txt")
distwater <- read_tsv("jan-distilledwater-reformat.txt")

view(cond4)


cond1$newDATE <- parse_date_time(cond1$Date, "mdy HMS")
cond2$newDATE <- parse_date_time(cond2$Date, "mdy HMS")
cond3$newDATE <- parse_date_time(cond3$Date, "mdy HMS")
cond4$newDATE <- parse_date_time(cond4$Date, "mdy HMS")
distwater$newDATE <- parse_date_time(distwater$Date, "mdy HMS")

cond1.2 <- cond1 %>% mutate(cond = "cond1")
cond2.2 <- cond2 %>% mutate(cond = "cond2")
cond3.2 <- cond3 %>% mutate(cond = "cond3")
cond4.2 <- cond4 %>% mutate(cond = "cond4")
distwater.2 <- distwater %>% mutate(cond = "cntrl")

library(RColorBrewer)
brewer.pal( n = 5, "Accent")

#Cond 1 = +Salt/-Cd
test1 <- cond1.2 %>% 
  filter(newDATE >= as_datetime("2021-12-07 14:00:00")) %>%
  filter(newDATE <= as_datetime("2021-12-10 14:00:00")) %>% 
  mutate(time_numeric = as.numeric(newDATE)) %>% 
  mutate(time_elapsed = time_numeric - time_numeric[1]) %>% 
  na.omit(.)
#Remove bad row
test1 <- test1[-c(375),]
view(test1)

c1 <- ggplot(test1, aes(x = time_elapsed, y = EC)) +
  geom_line(size = 1, color = "#E69F00") +
  geom_point(size = 2, alpha = 0.3, color = "#E69F00") +
  #geom_smooth(method = "lm") +
  geom_vline(aes(xintercept = as.numeric(as_datetime("2021-12-07 14:00:00")) - time_numeric[1]), 
             linetype = "longdash", size = 1) +
  geom_vline(aes(xintercept = as.numeric(as_datetime("2021-12-08 14:00:00")) - time_numeric[1]), 
             linetype = "longdash", size = 1) +
  geom_vline(aes(xintercept = as.numeric(as_datetime("2021-12-09 14:00:00")) - time_numeric[1]), 
             linetype = "longdash", size = 1) +
  xlab("Time Elapsed (s)") + ylab("Electrical Conductivity") +
  theme_linedraw()

#Cond 2 = -Salt/-Cd
test2 <- cond2.2 %>% 
  filter(newDATE >= as_datetime("2021-12-10 14:40:00")) %>%
  filter(newDATE <= as_datetime("2021-12-13 14:40:00")) %>% 
  mutate(time_numeric = as.numeric(newDATE)) %>% 
  mutate(time_elapsed = time_numeric - time_numeric[1]) %>% 
  na.omit(.)
#Remove bad row

c2 <- ggplot(test2, aes(x = time_elapsed, y = EC)) +
  geom_line(size = 1, color = "#009E73") +
  geom_point(size = 2, alpha = 0.3, color = "#009E73") +
  #geom_smooth(method = "lm") +
  geom_vline(aes(xintercept = as.numeric(as_datetime("2021-12-10 14:40:00")) - time_numeric[1]), 
             linetype = "longdash", size = 1) +
  geom_vline(aes(xintercept = as.numeric(as_datetime("2021-12-11 14:40:00")) - time_numeric[1]), 
             linetype = "longdash", size = 1) +
  geom_vline(aes(xintercept = as.numeric(as_datetime("2021-12-12 14:40:00")) - time_numeric[1]), 
             linetype = "longdash", size = 1) +
  xlab("Time Elapsed (s)") + ylab("Electrical Conductivity") +
  theme_linedraw()

#Cond 3 = -Salt/+Cd
test3 <- cond3.2 %>% 
  filter(newDATE >= as_datetime("2021-12-14 13:30:00")) %>%
  filter(newDATE <= as_datetime("2021-12-17 13:30:00")) %>% 
  mutate(time_numeric = as.numeric(newDATE)) %>% 
  mutate(time_elapsed = time_numeric - time_numeric[1]) %>% 
  na.omit(.)
#Remove bad row

c3 <- ggplot(test3, aes(x = time_elapsed, y = EC)) +
  geom_line(size = 1, color = "#0072B2") +
  geom_point(size = 2, alpha = 0.3, color = "#0072B2") +
  #geom_smooth(method = "lm") +
  geom_vline(aes(xintercept = as.numeric(as_datetime("2021-12-14 13:30:00")) - time_numeric[1]), 
             linetype = "longdash", size = 1) +
  geom_vline(aes(xintercept = as.numeric(as_datetime("2021-12-15 13:30:00")) - time_numeric[1]), 
             linetype = "longdash", size = 1) +
  geom_vline(aes(xintercept = as.numeric(as_datetime("2021-12-16 13:30:00")) - time_numeric[1]), 
             linetype = "longdash", size = 1) +
  xlab("Time Elapsed (s)") + ylab("Electrical Conductivity") +
  theme_linedraw()

#Cond 4 = +Salt/+Cd
test4 <- cond4.2 %>% 
  filter(newDATE >= as_datetime("2021-12-17 14:00:00")) %>%
  filter(newDATE <= as_datetime("2021-12-20 14:00:00")) %>% 
  mutate(time_numeric = as.numeric(newDATE)) %>% 
  mutate(time_elapsed = time_numeric - time_numeric[1]) %>% 
  na.omit(.)
#Remove bad row
test4 <- test4[-c(611),]

c4 <- ggplot(test4, aes(x = time_elapsed, y = EC)) +
  geom_line(size = 1, color = "#CC79A7") +
  geom_point(size = 2, alpha = 0.3, color = "#CC79A7") +
  #geom_smooth(method = "lm") +
  geom_vline(aes(xintercept = as.numeric(as_datetime("2021-12-17 14:00:00")) - time_numeric[1]), 
             linetype = "longdash", size = 1) +
  geom_vline(aes(xintercept = as.numeric(as_datetime("2021-12-18 14:00:00")) - time_numeric[1]), 
             linetype = "longdash", size = 1) +
  geom_vline(aes(xintercept = as.numeric(as_datetime("2021-12-19 14:00:00")) - time_numeric[1]), 
             linetype = "longdash", size = 1) +
  xlab("Time Elapsed (s)") + ylab("Electrical Conductivity") +
  theme_linedraw()

#Distilled Water control
testc <- distwater.2 %>% 
  filter(newDATE >= as_datetime("2022-1-2 14:00:00")) %>%
  filter(newDATE <= as_datetime("2022-1-5 14:00:00")) %>% 
  mutate(time_numeric = as.numeric(newDATE)) %>% 
  mutate(time_elapsed = time_numeric - time_numeric[1]) %>% 
  na.omit(.)

cc <- ggplot(testc, aes(x = time_elapsed, y = EC)) +
  geom_line(size = 1, color = "#CC79A7") +
  geom_point(size = 2, alpha = 0.3, color = "#000000") +
  #geom_smooth(method = "lm") +
  geom_vline(aes(xintercept = as.numeric(as_datetime("2022-1-2 14:00:00")) - time_numeric[1]), 
             linetype = "longdash", size = 1) +
  geom_vline(aes(xintercept = as.numeric(as_datetime("2022-1-3 14:00:00")) - time_numeric[1]), 
             linetype = "longdash", size = 1) +
  geom_vline(aes(xintercept = as.numeric(as_datetime("2022-1-4 14:00:00")) - time_numeric[1]), 
             linetype = "longdash", size = 1) +
  xlab("Time Elapsed (s)") + ylab("Electrical Conductivity") +
  theme_linedraw()

plot_grid(c2, c3, c1, c4, labels = c("-Salt/-Cd", "-Salt/+Cd", "+Salt/-Cd", "+Salt/+Cd"),
          scale = c(0.9, 0.9, 0.9, 0.9), label_size = 20, label_x = 0.32)

plot_grid(cc, labels = "Distilled Water", scale = 0.9, label_size = 20, label_x = 0.36)

#test all
ggplot() +
  geom_line(data = test1, aes(x = time_elapsed, y = EC, color = cond)) +
  geom_line(data = test2, aes(x = time_elapsed, y = EC, color = cond)) +
  geom_line(data = test3, aes(x = time_elapsed, y = EC, color = cond)) +
  geom_line(data = test4, aes(x = time_elapsed, y = EC, color = cond)) +
  geom_vline(aes(xintercept = 0), linetype = "longdash") +
  geom_vline(aes(xintercept = 86400), linetype = "longdash") +
  geom_vline(aes(xintercept = 172800), linetype = "longdash")

condlist <- list(cond1.2, cond2.2, cond3.2, cond4.2)

allcond <- bind_rows(condlist)
#HOW to join multiple tibbles???


ggplot() +
  geom_line(data = test1, aes(x = time_elapsed, y = SAL), color = "red")
  # geom_line(data = cond2, aes(x = as_datetime(newDATE), y = SAL), color = "blue") +
  # geom_line(data = cond3, aes(x = as_datetime(newDATE), y = SAL), color = "green") +
  # geom_line(data = cond4, aes(x = as_datetime(newDATE), y = SAL), color = "orange")

cond1long <- gather(cond1, type, value, EC:TDS, factor_key = TRUE)




ggplot(test1, aes(x = time_elapsed, y = SAL)) +
  geom_line(color = "#00BFC4") +
  geom_point(size = 1, alpha = 0.3, color = "#00BFC4") +
  geom_smooth(method = "lm") +
  #geom_line(aes(y = SAL*1.75), color = "#00BFC4") +
  #geom_point(aes(y = SAL*1.75), size = 1, alpha = 0.3, color = "#00BFC4") +
  geom_vline(aes(xintercept = as.numeric(as_datetime("2021-12-07 14:00:00")) - time_numeric[1]), linetype = "longdash") +
  geom_vline(aes(xintercept = as.numeric(as_datetime("2021-12-08 14:00:00")) - time_numeric[1]), linetype = "longdash") +
  geom_vline(aes(xintercept = as.numeric(as_datetime("2021-12-09 14:00:00")) - time_numeric[1]), linetype = "longdash")
  #scale_y_continuous(name = "EC",sec.axis = sec_axis(~./1.75, name = "SAL"))

ggplot(test1, aes(x = as_datetime(newDATE))) +
  geom_line(aes(y = EC), color = "#F8766D") +
  geom_line(aes(y = SAL*1.75), color = "#00BFC4") +
  geom_vline(aes(xintercept = as_datetime("2021-12-07 12:16:42")), linetype = "longdash") +
  geom_vline(aes(xintercept = as_datetime("2021-12-08 14:45:00")), linetype = "longdash") +
  geom_vline(aes(xintercept = as_datetime("2021-12-09 13:00:00")), linetype = "longdash") +
  scale_y_continuous(name = "EC",
                     sec.axis = sec_axis(~./1.75, name = "SAL"))
echmax <- median(test1$EC)
salhmax <- median(test1$SAL*1.75)
hlength <- length(test1$EC)

salH <- sort(runif(hlength, min = 14000, max = salhmax), decreasing = TRUE)
ecH <- sort(runif(hlength, min = 15000, max = echmax), decreasing = TRUE)
test1 <- test1 %>% mutate(sal_h = salH, ec_h = ecH)

#KV MU PLANT Feb 11 poster
ggplot(test1, aes(x = as_datetime(newDATE))) +
  geom_line(aes(y = EC), color = "#F8766D", size = 1.5) +
  geom_line(aes(y = ec_h), color = "#F8766D", linetype = "dashed", size = 1.5) +
  geom_line(aes(y = SAL*1.75), color = "#00BFC4", size = 1.5) +
  geom_line(aes(y = sal_h), color = "#00BFC4", linetype = "dashed", size = 1.5) +
  geom_vline(aes(xintercept = as_datetime("2021-12-07 12:16:42")), linetype = "longdash") +
  geom_vline(aes(xintercept = as_datetime("2021-12-08 14:45:00")), linetype = "longdash") +
  geom_vline(aes(xintercept = as_datetime("2021-12-09 13:00:00")), linetype = "longdash") +
  scale_y_continuous(name = "Electical Conductivity",
                     sec.axis = sec_axis(~./1.75, name = "Salinity")) +
  xlab("Date") +
  theme_minimal()

#Expected Results NAPPN2022
ggplot(test1, aes(x = as_datetime(newDATE))) +
  #geom_line(aes(y = EC), color = "#F8766D", size = 1.5) +
  geom_line(aes(y = ec_h), color = "#F8766D") +
  #geom_line(aes(y = SAL*1.75), color = "#00BFC4", size = 1.5) +
  geom_line(aes(y = sal_h), color = "#00BFC4") +
  geom_vline(aes(xintercept = as_datetime("2021-12-07 12:16:42")), linetype = "longdash") +
  geom_vline(aes(xintercept = as_datetime("2021-12-08 14:45:00")), linetype = "longdash") +
  geom_vline(aes(xintercept = as_datetime("2021-12-09 13:00:00")), linetype = "longdash") +
  scale_y_continuous(name = "Electical Conductivity",
                     sec.axis = sec_axis(~./1.75, name = "Salinity")) +
  xlab("Date") +
  theme_minimal()

#Observed Results NAPPN2022
ggplot(test1, aes(x = as_datetime(newDATE))) +
  geom_line(aes(y = EC), color = "#F8766D") +
  #geom_line(aes(y = ec_h), color = "#F8766D") +
  geom_line(aes(y = SAL*1.75), color = "#00BFC4") +
  #geom_line(aes(y = sal_h), color = "#00BFC4") +
  geom_vline(aes(xintercept = as_datetime("2021-12-07 12:16:42")), linetype = "longdash") +
  geom_vline(aes(xintercept = as_datetime("2021-12-08 14:45:00")), linetype = "longdash") +
  geom_vline(aes(xintercept = as_datetime("2021-12-09 13:00:00")), linetype = "longdash") +
  scale_y_continuous(name = "Electical Conductivity",
                     sec.axis = sec_axis(~./1.75, name = "Salinity")) +
  xlab("Date") +
  theme_minimal()

df <- tibble(time = 1:t, salinity = sal)

df %>% 
  ggplot(aes(x = time, y = salinity)) +
  geom_point()+
  geom_line()


cond2 <- read_csv("newcond2.csv")

view(cond2)


cond2$newDATE <- parse_date_time(cond2$DATE, "mdy HMS")

cond2long <- gather(cond2, type, value, EC:TDS, factor_key = TRUE)

view(cond2long)

test2 <- filter(cond2, as_date(newDATE) >= as_date("2021-12-13") &
                  as_date(newDATE) <= as_date("2021-12-13"))


ggplot(test2, aes(x = as_datetime(newDATE))) +
  geom_line(aes(y = EC), color = "#F8766D") +
  geom_line(aes(y = SAL*1.75), color = "#00BFC4") +
  geom_vline(aes(xintercept = as_datetime("2021-12-11 14:00:00")), linetype = "longdash") +
  geom_vline(aes(xintercept = as_datetime("2021-12-12 14:00:00")), linetype = "longdash") +
  geom_vline(aes(xintercept = as_datetime("2021-12-13 14:00:00")), linetype = "longdash") +
  scale_y_continuous(name = "EC",
                     sec.axis = sec_axis(~./1.75, name = "SAL"))

