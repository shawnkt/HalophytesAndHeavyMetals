library(tidyverse)
library(cowplot)
library(Hmisc)
library("wesanderson")

names(wes_palettes)

setwd("/Users/shawnt/Downloads/CakilePrelim/")

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

alldat2 <-
  alldat %>% 
  mutate(ugElementPermgDW = ((CorConc * 6) * 1/VolDigestUsed)/DryWeight) %>% 
  mutate(ugElement = ((CorConc * 6) * 1/VolDigestUsed)) %>% 
  mutate(TissueType = fct_relevel(TissueType, "root", "old leaf", "young leaf")) %>% 
  mutate(Salt = as_factor(Salt),
         Cadmium = as_factor(Cadmium))

write_csv(alldat2, "cleaneddat.csv")

#view(alldat2)

#new labels for facet
treatment.labs <- c("-Salt / +Cd", "+Salt / -Cd")
names(treatment.labs) <- c("0", "1")


# g DryWeight vs ug Element
#Zoomed in
alldat2 %>%   
  filter( ugElement > 0 ) %>%
  filter(Element == "Cd" | Element == "Fe" | Element == "Zn") %>% 
  ggplot(aes(x = DryWeight, y = ugElement, color = TissueType, 
             shape = as_factor(VolDigestUsed))) + 
  geom_point() + 
  scale_x_continuous(limits = c(0, 0.03), breaks = seq(0, 0.03, by = 0.0015)) +
  facet_wrap(Element ~ ., scales = "free", ncol = 1) +
  theme_linedraw()

alldat2 %>%   
  filter( ugElement > 0 ) %>%
  filter(Element == "Na" | Element == "K") %>% 
  ggplot(aes(x = DryWeight, y = ugElement, color = TissueType, 
             shape = as_factor(VolDigestUsed))) + 
  geom_point() + 
  scale_x_continuous(limits = c(0, 0.03), breaks = seq(0, 0.03, by = 0.0015)) +
  facet_wrap(Element ~ ., scales = "free", ncol = 1) +
  theme_linedraw()

#Fulldat
alldat2 %>%   
  filter( ugElement > 0 ) %>% 
  filter(Element == "Cd" | Element == "Fe" | Element == "Zn") %>% 
  ggplot(aes(x = DryWeight, y = ugElement, color = TissueType, 
             shape = as_factor(VolDigestUsed))) + 
  geom_point() + 
  scale_x_continuous(limits = c(0, 0.11), breaks = seq(0, 0.11, by = 0.005)) +
  facet_wrap(Element ~ ., scales = "free", ncol = 1) +
  theme_linedraw()

alldat2 %>%   
  filter( ugElement > 0 ) %>% 
  filter(Element == "Na" | Element == "K" ) %>% 
  ggplot(aes(x = DryWeight, y = ugElement, color = TissueType, 
             shape = as_factor(VolDigestUsed))) + 
  geom_point() + 
  scale_x_continuous(limits = c(0, 0.11), breaks = seq(0, 0.11, by = 0.005)) +
  facet_wrap(Element ~ ., scales = "free", ncol = 1) +
  theme_linedraw()


#line
alldat2 %>% 
  filter(Element == "Cd" | Element == "Zn" ) %>% 
  ggplot(aes(x = as_factor(Timepoint), y = ugElementPermgDW, color = TissueType, group = TissueType)) +
  stat_summary(fun.y = "mean", geom = "point") +
  stat_summary(fun.y = "mean", geom = "line") +
  stat_summary(fun.data = "mean_cl_boot") +
  facet_grid(Salt ~ Element, labeller = labeller(Salt = treatment.labs)) +
  labs(x = "Timepoint", y = "ug Element / mg Dry Weight", color = "Tissue") +
  theme_linedraw()

alldat2 %>% 
  filter(Element == "Na" | Element == "K" ) %>% 
  ggplot(aes(x = as_factor(Timepoint), y = ugElementPermgDW, color = TissueType, group = TissueType)) +
  geom_jitter(width = 0.1) + 
  stat_summary(fun.y = "mean", geom = "point", pch=21, size=5) +
  stat_summary(fun.y = "mean", geom = "line") +
  #stat_summary(fun.data = "mean_cl_boot") +
  facet_grid(Salt ~ Element, labeller = labeller(Salt = treatment.labs)) +
  labs(x = "Timepoint", y = "ug Element / mg Dry Weight", color = "Tissue") +
  theme_linedraw()

alldat2 %>% 
  filter(Element == "Fe") %>% 
  ggplot(aes(x = as_factor(Timepoint), y = ugElementPermgDW, color = TissueType, group = TissueType)) +
  stat_summary(fun.y = "mean", geom = "point") +
  stat_summary(fun.y = "mean", geom = "line") +
  stat_summary(fun.data = "mean_cl_boot") +
  facet_grid(Salt ~ Element, labeller = labeller(Salt = treatment.labs)) +
  labs(x = "Timepoint", y = "ug Element / mg Dry Weight", color = "Tissue") +
  theme_linedraw()
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


cddat2 <-
  cddat %>% 
  mutate(ugElementPermgDW = ((ConcCor * 6) * 1/VolDigestUsed)/DryWeight)



ggplot(data = cddat2, aes(x = as_factor(Timepoint), y = ugElementPermgDW)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = as_factor(Salt)))


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
