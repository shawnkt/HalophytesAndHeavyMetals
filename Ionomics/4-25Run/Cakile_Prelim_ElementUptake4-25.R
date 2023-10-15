library(tidyverse)
library(cowplot)
library(Hmisc)


# setwd("/Users/shawnt/Downloads/CakilePrelim/4-25Run/")
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

view(alldat)

alldat2 <-
  alldat %>% 
  mutate(ugElementPermgDW = ((CorConc * 6) * (1/VolDigestUsed))/DryWeight) %>% 
  mutate(ugElement = ((CorConc * 6) * (1/VolDigestUsed))) %>% 
  mutate(TissueType = fct_relevel(TissueType, "root", "old leaf", "young leaf")) %>% 
  mutate(Salt = as_factor(Salt), 
         Cadmium = as_factor(Cadmium))



write_csv(alldat2, "4-25-cleaneddat.csv")

view(alldat2)

#new labels for facet
treatment.labs <- c("-Salt / -Cd", "+Salt / +Cd")
names(treatment.labs) <- c("0", "1")

#bar
alldat2 %>% 
  filter(Element == "Cd" | Element == "Zn" ) %>% 
  ggplot(aes(x = as_factor(Timepoint), y = ugElementPermgDW, color = TissueType, fill = TissueType, group = TissueType), color = TissueType) +
  stat_summary(fun.y = mean, position=position_dodge(width=0.95), geom = "bar") +
  facet_grid(Salt ~ Element, labeller = labeller(Salt = treatment.labs)) +
  labs(x = "Timepoint", y = "ug Element / mg Dry Weight", fill = "Tissue") +
  guides(color = FALSE) +
  theme_linedraw()

alldat2 %>% 
  filter(Element == "Na" | Element == "K" ) %>% 
  ggplot(aes(x = as_factor(Timepoint), y = ugElementPermgDW, fill = TissueType, color = TissueType, group = TissueType)) +
  stat_summary(fun.y = mean, position=position_dodge(width=0.95), geom = "bar") +
  facet_grid(Salt ~ Element, labeller = labeller(Salt = treatment.labs)) +
  labs(x = "Timepoint", y = "ug Element / mg Dry Weight", fill = "Tissue") +
  guides(color = FALSE) +
  theme_linedraw()

alldat2 %>% 
  filter(Element == "Fe") %>% 
  ggplot(aes(x = as_factor(Timepoint), y = ugElementPermgDW, fill = TissueType, color = TissueType, group = TissueType)) +
  stat_summary(fun.y = mean, position=position_dodge(width=0.95), geom = "bar") +
  facet_grid(Salt ~ Element, labeller = labeller(Salt = treatment.labs)) +
  labs(x = "Timepoint", y = "ug Element / mg Dry Weight", fill = "Tissue") +
  guides(color = FALSE) +
  theme_linedraw()

#line + mean + points
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

#line + mean + points all elements
alldat2 %>% 
  ggplot(aes(x = as_factor(Timepoint), y = ugElementPermgDW, color = TissueType, group = TissueType)) +
  geom_jitter(width = 0.1) + 
  stat_summary(fun.y = "mean", geom = "point", pch=21, size=5) +
  stat_summary(fun.y = "mean", geom = "line") +
  #stat_summary(fun.data = "mean_cl_boot") +
  facet_grid(Salt ~ Element, labeller = labeller(Salt = treatment.labs)) +
  labs(x = "Timepoint", y = "ug Element / mg Dry Weight", color = "Tissue") +
  theme_linedraw()

