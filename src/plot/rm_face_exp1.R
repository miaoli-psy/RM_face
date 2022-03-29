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

data_preprocessed <- read_excel("exp1_preprocessed.xlsx")

# data by subject
data_by_subject <- data_preprocessed %>%
  group_by(setsize, participant, stimulus_types, spacing, size_scale) %>%
  summarise(deviation_score_mean = mean(deviation_score),
            deviation_score_std = sd(deviation_score),
            rt_mean = mean(rt),
            rt_std = sd(rt))

# data across subject
data_across_subject <- data_preprocessed %>%
  group_by(setsize, stimulus_types, spacing, size_scale) %>%
  summarise(deviation_score_mean = mean(deviation_score),
            deviation_score_std = sd(deviation_score),
            rt_mean = mean(rt),
            rt_std = sd(rt))

# samplesize = 60 (each condition 5 repitation * 12 participant)
data_across_subject <- data_across_subject %>%
  mutate(deviation_socre_SEM = deviation_score_std / sqrt(60),
         rt_SEM = rt_std / sqrt(60))


my_plot <-  ggplot() +
  
  geom_bar(data = data_across_subject, aes(x = spacing_in_deg,
                                        y = deviation_score_mean,
                                        fill = stimulus_types),
           position = "dodge", stat = "identity", alpha = 0.5, width = 0.2) +
  
  
  # each data point represents the average deviation of 1 participant
  geom_point(data = data_by_subject, aes(x = spacing_in_deg,
                                             y = deviation_score_mean,
                                             group = stimulus_types,
                                             color = stimulus_types),
             alpha = 0.2,
             position = position_dodge(0.2))+
  
  geom_errorbar(data = data_across_subject, aes(x = spacing_in_deg,
                                             y = deviation_score_mean,
                                             ymin = deviation_score_mean - deviation_socre_SEM,
                                             ymax = deviation_score_mean + deviation_socre_SEM,
                                             fill = stimulus_types), 
                
                color = "black",
                size  = 1.2,
                width = .00,
                position = position_dodge(0.2)) +
  
  labs(y = "Deviation score", x = "Spacing (deg)") +
  
  scale_color_manual(values = c("NF" = "#000080",
                                "outline" = "#E7B800",
                                "NF_scramble" = "#FC4E07")) +
  
  scale_fill_manual(values = c("NF" = "#000080",
                               "outline" = "#E7B800",
                               "NF_scramble" = "#FC4E07")) +
  
  scale_x_continuous(limits = c(-0.05, 1.6)) +
  
  scale_y_continuous(limits = c(-1.8, 1.5)) +
  
  theme(axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        panel.border = element_blank(),  
        # Remove panel grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Remove panel background
        panel.background = element_blank(),
        # Add axis line
        axis.line = element_line(colour = "grey")) +
  
  facet_wrap( ~ setsize + size_scale,
              nrow = 4,
              labeller = labeller(setsize =
                                    c("3" = "setsize = 3",
                                      "4" = "setsize = 4",
                                      "5" = "setsize = 5",
                                      "6" = "setsize = 6"),
                                  size_scale = c("small" = "small face 0.74*1.1",
                                                    "middle" = "medium face 0.936*1.39",
                                                    "large" = "large face 1.132*1.68")))

print(my_plot)




my_plot3 <-  ggplot() +
  
  geom_point(data = data_across_subject, aes(x = stimulus_types,
                                          y = deviation_score_mean,
                                          size = size_scale,
                                          color = size_scale),
             position = position_dodge(0.2), stat = "identity", alpha = 0.4, width = 0.2) +
  
  scale_size_manual(values = c("small" = 2, "medium"= 4, "large" = 6)) +
  
  geom_hline(aes(x = type,
                 y = mean_deviation_score), yintercept = 0) +
  
  
  # each data point represents the average deviation of 1 participant
  # geom_point(data = all_data_average_pp, aes(x = type,
  #                                            y = mean_deviation_score,
  #                                            size = stimulus_size),
  #            alpha = 0.2,
  #            position = position_dodge(0.2))+
  
  geom_errorbar(data = data_across_subject, aes(x = stimulus_types,
                                             y = deviation_score_mean,
                                             ymin = deviation_score_mean - deviation_socre_SEM,
                                             ymax = deviation_score_mean + deviation_socre_SEM,
                                             group = size_scale),
                
                color = "black",
                size  = 0.8,
                width = .00,
                position = position_dodge(0.2)) +
  
  labs(y = "Deviation score", x = "Type") +
  
  scale_color_manual(values = c("small" = "#000080",
                                "middle" = "#E7B800",
                                "large" = "#FC4E07")) +
  
  # scale_fill_manual(values = c("face" = "#000080",
  #                              "outline" = "#E7B800",
  #                              "surface" = "#FC4E07")) +
  # 
  # scale_x_continuous(limits = c(-0.05, 1.5)) +
  # 
  # scale_y_continuous(limits = c(-1.5, 1.5)) +
  
  theme(axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        panel.border = element_blank(),  
        # Remove panel grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Remove panel background
        panel.background = element_blank(),
        # Add axis line
        axis.line = element_line(colour = "grey")) +
  
  facet_wrap( ~ setsize + spacing,
              nrow = 4,
              labeller = labeller(setsize =
                                    c("3" = "setsize = 3",
                                      "4" = "setsize = 4",
                                      "5" = "setsize = 5",
                                      "6" = "setsize = 6")))

print(my_plot3)

