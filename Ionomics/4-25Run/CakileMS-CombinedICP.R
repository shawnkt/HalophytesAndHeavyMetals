library(tidyverse)
library(cowplot)
library(Hmisc)
library("wesanderson")

names(wes_palettes)

setwd("~/Desktop/CakilePrelim/")

####################
# 3/31/22 ICP data #
####################

cddat <- read_tsv("Cd.prelim.txt")
nadat <- read_tsv("Na.prelim.txt")
kdat <- read_tsv("K.prelim.txt")
fedat <- read_tsv("Fe.prelim.txt")
zndat <- read_tsv("Zn.prelim.txt")

#Calibration avgs: neg control of nitric acid dilution in miliq water
cdcalib <- 0.003530006
fecalib <- 0.027049952
kcalib <- 0.092925808
nacalib <- 0.185461946
zncalib <- 0.007146691

#adjust concentrations with calibration
cddat2 <- 
  cddat %>%
  mutate(CorConc = ConcSamp - cdcalib)

fedat2 <- 
  fedat %>%
  mutate(CorConc = ConcSamp - fecalib)

kdat2 <- 
  kdat %>%
  mutate(CorConc = ConcSamp - kcalib)

nadat2 <- 
  nadat %>%
  mutate(CorConc = ConcSamp - nacalib)

zndat2 <- 
  zndat %>%
  mutate(CorConc = ConcSamp - zncalib)

alldat <- rbind(cddat2, nadat2, kdat2, fedat2, zndat2)

alldat[alldat < 0] <- 0
#view(alldat)

#calculate ug element / mg dry weight
#calculate ug element
#set tissue type and treatment as factors
alldat2 <-
  alldat %>% 
  mutate(ugElementPermgDW = ((CorConc * 6) * 1/VolDigestUsed)/(DryWeight*1000)) %>% 
  mutate(ugElement = ((CorConc * 6) * 1/VolDigestUsed)) %>% 
  mutate(TissueType = fct_relevel(TissueType, "root", "old leaf", "young leaf")) %>% 
  mutate(Salt = as_factor(Salt),
         Cadmium = as_factor(Cadmium))

write_csv(alldat2, "New033122ICPdata.csv")

#convert treatment factors from numbers to human readable names
treatment.labs <- c("-Salt / +Cd", "+Salt / -Cd")
names(treatment.labs) <- c("0", "1")

#plots
mycols <- c("#D55E00", "#009E73", "#0072B2")

alldat2 %>% 
  filter(Element == "Cd" | Element == "Zn" ) %>% 
  ggplot(aes(x = as_factor(Timepoint), y = ugElementPermgDW, color = TissueType, group = TissueType)) +
  geom_jitter(width = 0.1) + 
  stat_summary(fun.y = "mean", geom = "point", pch=21, size=5) +
  stat_summary(fun.y = "mean", geom = "line") +
  #stat_summary(fun.data = "mean_cl_boot") +
  facet_grid(Salt ~ Element, labeller = labeller(Salt = treatment.labs)) +
  labs(x = "Timepoint", y = "ug Element / mg Dry Weight", color = "Tissue") + 
  scale_color_manual(values = mycols) + theme_linedraw()
#####Check this diff theme and color palette now

alldat2 %>% 
  filter(Element == "Na" | Element == "K" ) %>% 
  ggplot(aes(x = as_factor(Timepoint), y = ugElementPermgDW, color = TissueType, group = TissueType)) +
  geom_jitter(width = 0.1) + 
  stat_summary(fun.y = "mean", geom = "point", pch=21, size=5) +
  stat_summary(fun.y = "mean", geom = "line") +
  #stat_summary(fun.data = "mean_cl_boot") +
  facet_grid(Salt ~ Element, labeller = labeller(Salt = treatment.labs)) +
  labs(x = "Timepoint", y = "ug Element / mg Dry Weight", color = "Tissue") +
  scale_color_manual(values = mycols) + theme_linedraw()

alldat2 %>% 
  filter(Element == "Fe") %>% 
  ggplot(aes(x = as_factor(Timepoint), y = ugElementPermgDW, color = TissueType, group = TissueType)) +
  geom_jitter(width = 0.1) + 
  stat_summary(fun.y = "mean", geom = "point", pch=21, size=5) +
  stat_summary(fun.y = "mean", geom = "line") +
  #stat_summary(fun.data = "mean_cl_boot") +
  facet_grid(Salt ~ Element, labeller = labeller(Salt = treatment.labs)) +
  labs(x = "Timepoint", y = "ug Element / mg Dry Weight", color = "Tissue") +
  theme_linedraw()

####################
# 4/25/22 ICP data #
####################

setwd("~/Desktop//CakilePrelim/4-25Run/")

cddat <- read_tsv("Cd.txt")
nadat <- read_tsv("Na.txt")
kdat <- read_tsv("K.txt")
fedat <- read_tsv("Fe.txt")
zndat <- read_tsv("Zn.txt")

#Calibration avgs: neg control of nitric acid dilution in miliq water
cdcalib <- 0.004445367
fecalib <- 0.021589123
kcalib <- 0.047471455
nacalib <- 0.362970374
zncalib <- 0.006968769

#adjust concentrations with calibration
cddat2 <- 
  cddat %>%
  mutate(CorConc = ConcSamp - cdcalib)

fedat2 <- 
  fedat %>%
  mutate(CorConc = ConcSamp - fecalib)

kdat2 <- 
  kdat %>%
  mutate(CorConc = ConcSamp - kcalib)

nadat2 <- 
  nadat %>%
  mutate(CorConc = ConcSamp - nacalib)

zndat2 <- 
  zndat %>%
  mutate(CorConc = ConcSamp - zncalib)

alldat <- rbind(cddat2, nadat2, kdat2, fedat2, zndat2)

alldat[alldat < 0] <- 0

#view(alldat)

#calculate ug element / mg dry weight
#calculate ug element
#set tissue type and treatment as factors
alldat2 <-
  alldat %>% 
  mutate(ugElementPermgDW = ((CorConc * 6) * (1/VolDigestUsed))/(DryWeight*1000)) %>% 
  mutate(ugElement = ((CorConc * 6) * (1/VolDigestUsed))) %>% 
  mutate(TissueType = fct_relevel(TissueType, "root", "old leaf", "young leaf")) %>% 
  mutate(Salt = as_factor(Salt), 
         Cadmium = as_factor(Cadmium))

write_csv(alldat2, "New042522ICPdata.csv")

#view(alldat2)

#convert treatment factors from numbers to human readable names
treatment.labs <- c("-Salt / -Cd", "+Salt / +Cd")
names(treatment.labs) <- c("0", "1")

####################
# All ICP data     #
####################

alldat2 <- read_csv("CakileCombinedICPData.csv")
#view(alldat2)

#Make TissueType and Treatment factor data type
alldat2 <- alldat2 %>% 
  mutate(SaltCadmium = as_factor(SaltCadmium)) %>% 
  mutate(TissueType = fct_relevel(TissueType, "root", "leaf"),
         SaltCadmium = fct_relevel(SaltCadmium, "00", "10", "01", "11")) %>% 
  mutate(Timepoint2 = Timepoint * 24)

#view(alldat2)

#Make readable labels for Treatment
treatment.labs <- c("-Salt / -Cd", "+Salt / -Cd", "-Salt / +Cd", "+Salt / +Cd")
names(treatment.labs) <- c("00", "10", "01", "11")


mycols <- c("#D55E00", "#009E73")

#K plot
alldat2 %>% 
  filter(Element == "K") %>% 
  ggplot(aes(x = as_factor(Timepoint2), y = ugElementPermgDW, color = TissueType, group = TissueType)) +
  geom_jitter(width = 0.1) +
  stat_summary(fun.y = "mean", geom = "point", pch=21, size=5) +
  stat_summary(fun.y = "mean", geom = "line") +
  facet_grid(SaltCadmium ~ Element, labeller = labeller(SaltCadmium = treatment.labs)) +
  scale_color_manual(values = mycols) + theme_linedraw() +
  theme(strip.background = element_blank(), strip.text = element_blank(), 
        axis.title = element_blank(), axis.text = element_text(size=15, face = "bold"), 
        legend.position = "none")

#Na plot
alldat2 %>% 
  filter(Element == "Na") %>% 
  ggplot(aes(x = as_factor(Timepoint2), y = ugElementPermgDW, color = TissueType, group = TissueType)) +
  geom_jitter(width = 0.1) +
  stat_summary(fun.y = "mean", geom = "point", pch=21, size=5) +
  stat_summary(fun.y = "mean", geom = "line") +
  facet_grid(SaltCadmium ~ Element, labeller = labeller(SaltCadmium = treatment.labs)) +
  scale_color_manual(values = mycols) + theme_linedraw() +
  theme(strip.background = element_blank(), strip.text = element_blank(), 
        axis.title = element_blank(), axis.text = element_text(size=15, face = "bold"), 
        legend.position = "none")

#Fe plot
alldat2 %>% 
  filter(Element == "Fe") %>% 
  ggplot(aes(x = as_factor(Timepoint2), y = ugElementPermgDW, color = TissueType, group = TissueType)) +
  geom_jitter(width = 0.1) +
  stat_summary(fun.y = "mean", geom = "point", pch=21, size=5) +
  stat_summary(fun.y = "mean", geom = "line") +
  facet_grid(SaltCadmium ~ Element, labeller = labeller(SaltCadmium = treatment.labs)) +
  scale_color_manual(values = mycols) + theme_linedraw() +
  theme(strip.background = element_blank(), strip.text = element_blank(), 
        axis.title = element_blank(), axis.text = element_text(size=15, face = "bold"), 
        legend.position = "none")

#Zn plot
alldat2 %>% 
  filter(Element == "Zn") %>% 
  ggplot(aes(x = as_factor(Timepoint2), y = ugElementPermgDW, color = TissueType, group = TissueType)) +
  geom_jitter(width = 0.1) +
  stat_summary(fun.y = "mean", geom = "point", pch=21, size=5) +
  stat_summary(fun.y = "mean", geom = "line") +
  facet_grid(SaltCadmium ~ Element, labeller = labeller(SaltCadmium = treatment.labs)) +
  scale_color_manual(values = mycols) + theme_linedraw() +
  theme(strip.background = element_blank(), strip.text = element_blank(), 
        axis.title = element_blank(), axis.text = element_text(size=15, face = "bold"), 
        legend.position = "none")

#Cd plot
alldat2 %>% 
  filter(Element == "Cd") %>% 
  ggplot(aes(x = as_factor(Timepoint2), y = ugElementPermgDW, color = TissueType, group = TissueType)) +
  geom_jitter(width = 0.1) +
  stat_summary(fun.y = "mean", geom = "point", pch=21, size=5) +
  stat_summary(fun.y = "mean", geom = "line") +
  facet_grid(SaltCadmium ~ Element, labeller = labeller(SaltCadmium = treatment.labs)) +
  scale_color_manual(values = mycols) + theme_linedraw() +
  theme(strip.background = element_blank(), strip.text = element_blank(), 
        axis.title = element_blank(), axis.text = element_text(size=15, face = "bold"), 
        legend.position = "none")
