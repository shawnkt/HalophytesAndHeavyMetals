library(readxl)
library(tidyverse)
library(cowplot)

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/2_WORKS/1_PROJECTS/Schenck lab/Shaun Paper/Root_stem_leaves")

dat <- read_xlsx("FAA_analysis_HD.xlsx", sheet = "Data Elaboration", col_names = TRUE, .name_repair="minimal")

df <- dat %>% select(c(4,27:46)) %>%
  pivot_longer(2:21, values_to = "Value", names_to = "Traits") %>%
  group_by(`ID`, `Traits`) %>% 
  summarise(`Average nMol per mg` =mean(Value)) #%>% 
  #pivot_wider(names_from = `Traits`, values_from = `Average`)

theme_set(theme_half_open())

# Relative composition
p1 <- ggplot(df, aes(fill = Traits, y = `Average nMol per mg`, x = ID)) + 
  geom_bar(position="fill", stat="identity") +
  ggtitle("FAA relative composition") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p1   

ggsave("FAA_relative_composition.pdf")

# Absolute composition
p2 <- ggplot(df, aes(fill = Traits, y =`Average nMol per mg`, x = ID)) + 
  geom_bar(position= position_stack(), stat="identity") +
  ggtitle("FAA absolute composition") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
p2

ggsave("FAA_absolute_composition.pdf")


