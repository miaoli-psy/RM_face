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
setwd("D:/SCALab/projects/RM_face/exp1/data/")

all_data_average <- read_excel("rm_face_exp1_average.xlsx")

all_data_average = all_data_average[all_data_average$spacing_in_deg %in% c(0, 0.2, 0.39, 0.5, 0.6), ] 

# plot sep numerosity
my_plot <-  ggplot() +
  
  geom_point(data = all_data_average, aes(x = setsize,
                                y = mean_deviation_score,
                                color = stimulus_type,
                                shape = stimulus_type,
                                size = size_w),
           position = "dodge", stat = "identity", alpha = 0.6) +
  
  scale_shape_manual(values = c(16, 15, 18)) +
  
  scale_color_manual(values = c("#000080", "#E7B800", "#FC4E07"))+
  
  scale_x_continuous(breaks = c(3, 4, 5, 6)) +
  
  scale_y_continuous(limits = c(-1, 1)) +
  
  geom_hline(aes(x = setsize,
                 y = mean_deviation_score), yintercept = 0) +
  
  # # each data point represents the average deviation of 1 participant
  # geom_point(data = all_data_each_pp, aes(x = percent_triplets,
  #                                         y = mean_deviation_score,
  #                                         group = protectzonetype,
  #                                         colour = protectzonetype),
  #            alpha = 0.2,
  #            position = position_dodge(0.2))+
  
  # geom_errorbar(data = all_data_average, aes(x = setsize,
  #                                    y = mean_deviation_score,
  #                                    ymin = mean_deviation_score - SEM,
  #                                    ymax = mean_deviation_score + SEM,
  #                                    group = stimulus_type),
  #               color = "black",
  #               size  = 0.2,
  #               width = .00,
  #               position = position_dodge(0)) +
  
  # scale_fill_manual(values = c("radial" = "#ff4500",
  #                              "tangential" = "#4169e1")) +
  # 
  # scale_colour_manual(values = c("radial" = "#ff4500",
  #                                "tangential" = "#4169e1")) +
  
  labs(y = "Deviation score", x = "Set size") +
  
  theme_few() +
  
  facet_wrap( ~ spacing_in_deg , nrow = 2)

print(my_plot)