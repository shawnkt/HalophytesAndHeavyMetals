library(readxl)
library(tidyverse)
library(cowplot)
library(stringr)

theme_set(theme_half_open())

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/2_WORKS/1_PROJECTS/Schenck lab/Shaun Paper/Salt_treatment")

dat <- read_xlsx("072423_Salt_treatment_FAA_analysis.xlsx", sheet = "Data Elaboration", col_names = TRUE, .name_repair="minimal")

df <- dat %>% select(c(4,26:45)) %>%
  pivot_longer(2:21, values_to = "Value", names_to = "Traits") %>%
  group_by(`Name`, `Traits`) %>% 
  summarise(`Average nMol per mg` =mean(Value)) %>% na.omit()

### Relative composition
p1 <- ggplot(df, aes(fill = Traits, y = `Average nMol per mg`, x = Name)) + 
  geom_bar(position="fill", stat="identity") +
  ggtitle("FAA relative composition") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p1   

ggsave("FAA_relative_composition.pdf")

### Heatmap
# Generate data matrix for heatmap
df1 <- df %>% pivot_wider(names_from = "Traits", values_from = "Average nMol per mg") %>% as.data.frame()

df1_names <- df1[,1] %>% 
  str_replace_all("h", "") %>%
  str_replace_all("A", "Arabidopsis") %>% 
  str_replace_all("B", "Brassica") %>% 
  str_replace_all("C", "Cakile")

rownames(df1) <- df1_names

df1 <- df1[,-1] %>% scale(center = TRUE, scale = TRUE) %>% as.matrix() 

## Heatmap
library(grid)
library(ComplexHeatmap)
library(circlize)

# Generate color pallete for heatmap
col_fun = colorRamp2(c(-4, -2, 0, 2, 4), c("white", "#fffccc", "#fcc80c","#fc680c", "#660000"))

p2 <- Heatmap(df1, 
        name = "AA level", 
        col = col_fun,
        column_title = "Free Amino Acids",
        row_title = "Species",
        row_title_rot = 0,
        column_dend_height = unit(2,"cm"),
        row_dend_width = unit(2,"cm"),
        column_title_gp = gpar(fontface = "bold"),
        row_title_gp = gpar(fontface = "bold")
        )

p2


