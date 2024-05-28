library(tidyverse)
library(sacRebleu)
library(sentencepiece)
library(RColorBrewer)

data <- readRDS( "Stat_Data/data_test_all.rds")

names(data)
source("utils.R")
colors = palette.colors(8, "Accent")

## Plot  ###################################################

mutated_data <- data%>% 
  
  mutate(str_le = cut_number(length, 5) ) %>%  
  pivot_longer(cols = starts_with(c("standard", "add_k", "floor"))) %>% 
  select(length, str_le, name, BLEU = value) %>% 
  mutate(model = str_extract(name, "[a-z]+$")) %>% 
  mutate(type = str_extract(name, "^(add_k|floor|standard)")) 

labels <- c("MOSES-SP", "SOLAR10.7B", "XGLM1.7B")
mutated_data %>% 
  ggplot(aes(x = model, y = BLEU, fill = type ))  + 
  geom_boxplot() +
  theme_bw( base_size =11) + 
  scale_fill_grey(start = 0.2, end = 0.9, name = "") + 
  xlab("") + ylab("") + scale_x_discrete(label = labels) + 
  theme(legend.position = c(0.85,0.8), 
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12))  

ggsave(filename = "Chart/Bleu_Boxplot_All_20.png",
       width = 5.42, height = 3.68)


mutated_data %>% filter(type == "floor", 
                        model == "moses") %>% 
  ggplot(aes(x = BLEU,  fill = str_le ))  + 
  geom_density(alpha = 0.5) +
  theme_bw( base_size =11) + 
  scale_fill_grey(start = 0, end = 1, name = "Length of Ancient Text") + 
  xlab("") + ylab("Density") + 
  theme(legend.position = c(0.8,0.65), 
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12))  

ggsave(filename = "Chart/Moses_floor_Bleu_Density_All_20.png",
       width = 5.42, height = 3.68)


mutated_data %>% filter(type == "floor", 
                        model == "solar") %>% 
  ggplot(aes(x = BLEU,  fill = str_le ))  + 
  geom_density(alpha = 0.5) +
  theme_bw( base_size =11) + 
  #scale_fill_grey(start = 0, end = 1, name = "Length of Ancient Text") + 
  xlab("") + ylab("Density") + 
  theme(legend.position = c(0.8,0.65), 
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12))  

ggsave(filename = "Chart/Solar_floor_Bleu_Density_All_20.png",
       width = 5.42, height = 3.68)



aggregated_data <- mutated_data %>% group_by(model, type) %>% 
  dplyr::summarize(BLEU_Score = mean(BLEU), sd = sd(BLEU))

aggregated_data %>% 
  ggplot(aes(x = model, y = BLEU_Score, fill  = type)) + 
  geom_bar(stat = "identity",  position = "dodge") + 
  geom_errorbar(aes(ymin = BLEU_Score-sd, ymax= BLEU_Score+sd), 
                width = .2,
                position = position_dodge(.9)) +
  scale_fill_manual(name = "", values = colors) + 
  theme_bw()
## moses - stat onl

ggsave(filename = "Chart/Bleu_Errobar_All_20.png",
       width = 4.12*2, height = 2.67*2, units = "in",
       dpi = 600)

mutated_data %>% filter(model %in% c("solar", "moses")) %>% 
  filter(type == "standard") %>% 
  ggplot(aes(x = BLEU,  fill = model ))  + 
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = colors) + ggplot2::facet_wrap(~ str_le) + 
  theme_bw() 


ggsave(filename = "Chart/Bleu_Density_Moses_Solar.png",
       width = 4.12*2, height = 2.67*2, units = "in",
       dpi = 600)


#rm(list=ls())

