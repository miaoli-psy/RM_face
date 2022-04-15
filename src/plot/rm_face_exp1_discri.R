# libraries
library(readxl)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(emmeans)
library(sjstats)
library(lme4)
library(lmerTest)
library(MuMIn)

# set working path
setwd("D:/SCALab/projects/RM_face/data/")

data_preprocessed <- read_excel("exp1_disc_preprocessed.xlsx")

data_preprocessed <- data_preprocessed %>% 
  mutate(is_num_corr = if_else(deviation_score == 0, 1, 0))

# plot: percent correct - for each stim type ------------


data_by_subject <- data_preprocessed %>%
  group_by(participant, stimulus_types, size_scale) %>%
  summarise(is_num_corr_mean = mean(is_num_corr),
            is_num_corr_std = sd(is_num_corr),
            rt_mean = mean(rt),
            rt_std = sd(rt))

# data across subject
data_across_subject <- data_preprocessed %>%
  group_by(stimulus_types, size_scale) %>%
  summarise(is_num_corr_mean = mean(is_num_corr),
            is_num_corr_std = sd(is_num_corr),
            rt_mean = mean(rt),
            rt_std = sd(rt))

# samplesize = 144 (each condition 12 repetition * 12 participant)
data_across_subject <- data_across_subject %>%
  mutate(is_num_corr_SEM = is_num_corr_std / sqrt(144),
         rt_SEM = rt_std / sqrt(144))

my_plot <-  ggplot() +
  
  geom_point(data = data_across_subject, aes(x = stimulus_types,
                                              y = is_num_corr_mean,
                                              size = size_scale,
                                              color = size_scale),
             position = position_dodge(0.2), stat = "identity", alpha = 0.8, width = 0.2) +
  
  # each data point represents the average deviation of 1 participant
  geom_point(data = data_by_subject, aes(x = stimulus_types,
                                          y = is_num_corr_mean,
                                          size = size_scale,
                                          color = size_scale),
             alpha = 0.1,
             position = position_dodge(0.2))+
  
  geom_errorbar(data = data_across_subject, aes(x = stimulus_types,
                                                 y = is_num_corr_mean,
                                                 ymin = is_num_corr_mean - is_num_corr_SEM,
                                                 ymax = is_num_corr_mean + is_num_corr_SEM,
                                                 group = size_scale),
                
                color = "black",
                size  = 0.8,
                width = .00,
                position = position_dodge(0.2)) +
  
  geom_hline(yintercept = 0.9, linetype = "dashed") +
  
  labs(y = "Percent correct", x = "Stimulus type") +
  
  scale_color_manual(labels = c("large", "middle", "small"),
                     values = c("#004488", "#BB5566", "#DDAA33"),
                     name = "stimuli size") +
  
  
  scale_size_manual(labels = c("large", "middle", "small"),
                    values = c("large" = 6, "middle"= 4, "small" = 2),
                    name = "stimuli size") +
  
  scale_x_discrete(breaks = c("NF", "NF_scramble", "outline"),
                   labels = c("face", "scrambled face", "outline"),
                   expand = c(0.1, 0.1)) +
  
  
  theme(axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        panel.border = element_blank(),  
        # remove panel grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove panel background
        panel.background = element_blank(),
        # add axis line
        axis.line = element_line(colour = "grey"),
        # x,y axis tick labels
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        # legend size
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        # facet wrap title
        strip.text.x = element_text(size = 12, face = "bold"))

print(my_plot)
