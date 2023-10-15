library(tidyverse)
library(cowplot)
library(Hmisc)

#Set Working Directory
#setwd("~/Downloads/CakilePrelim/4-25Run/")
setwd("~/Desktop/CakilePrelim/4-25Run/")


#Read data
alldat2 <- read_csv("4treatments-cleandat2.csv")
#view(alldat2)

#Make TissueType and Treatment factor data type
alldat2 <- alldat2 %>% 
  mutate(SaltCadmium = as_factor(SaltCadmium)) %>% 
  mutate(TissueType = fct_relevel(TissueType, "root", "old leaf", "young leaf"),
         SaltCadmium = fct_relevel(SaltCadmium, "00", "10", "01", "11"))

#view(alldat2)

#Make readable labels for Treatment
treatment.labs <- c("-Salt / -Cd", "+Salt / -Cd", "-Salt / +Cd", "+Salt / +Cd")
names(treatment.labs) <- c("00", "10", "01", "11")

##########
# PLOTS #
#########

#LINE + MEAN + POINTS
######################

#set colors
mycols <- c("#D55E00", "#009E73", "#0072B2")

#Microelements
alldat2 %>% 
  filter(Element == "Cd" | Element == "Zn") %>% 
  ggplot(aes(x = as_factor(Timepoint), y = ugElementPermgDW, color = TissueType, group = TissueType)) +
  geom_jitter(width = 0.1) +
  stat_summary(fun.y = "mean", geom = "point", pch=21, size=5) +
  stat_summary(fun.y = "mean", geom = "line") +
  facet_grid(SaltCadmium ~ Element, labeller = labeller(SaltCadmium = treatment.labs)) +
  labs(x = "Timepoint", y = "ug Element / mg Dry Weight", color = "Tissue") +
  scale_color_manual(values = mycols) + theme_linedraw() +
  theme(strip.text.y.right = element_text(angle = 0))

#Macroelements
alldat2 %>% 
  filter(Element == "Na" | Element == "K" ) %>% 
  ggplot(aes(x = as_factor(Timepoint), y = ugElementPermgDW, color = TissueType, group = TissueType)) +
  geom_jitter(width = 0.1) + 
  stat_summary(fun.y = "mean", geom = "point", pch=21, size=5) +
  stat_summary(fun.y = "mean", geom = "line") +
  facet_grid(SaltCadmium ~ Element, labeller = labeller(SaltCadmium = treatment.labs)) +
  labs(x = "Timepoint", y = "ug Element / mg Dry Weight", color = "Tissue") + 
  scale_color_manual(values = mycols) + theme_linedraw() +
  theme(strip.text.y.right = element_text(angle = 0))

#Iron
alldat2 %>% 
  filter(Element == "Fe") %>% 
  ggplot(aes(x = as_factor(Timepoint), y = ugElementPermgDW, color = TissueType, group = TissueType)) +
  geom_jitter(width = 0.1) + 
  stat_summary(fun.y = "mean", geom = "point", pch=21, size=5) +
  stat_summary(fun.y = "mean", geom = "line") +
  facet_grid(SaltCadmium ~ Element, labeller = labeller(SaltCadmium = treatment.labs)) +
  labs(x = "Timepoint", y = "ug Element / mg Dry Weight", color = "Tissue") + 
  scale_color_manual(values = mycols) + theme_linedraw() +
  theme(strip.text.y.right = element_text(angle = 0))
