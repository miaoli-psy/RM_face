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
setwd("D:/SCALab/projects/RM_face/exp2/data/")

all_data_average <- read_excel("rm_face_exp2_average.xlsx")
all_data_average_pp <- read_excel("rm_face_exp2_average_perpp.xlsx")
all_data_average_usd <- read_excel("rm_face_exp2_usd.xlsx")
all_data_average_usd_pp <- read_excel("rm_face_exp2_usd_perpp.xlsx")

all_data_average = all_data_average[all_data_average$spacing_in_deg %in% c(0, 0.2, 0.39, 0.5, 0.6), ] 
all_data_average_pp = all_data_average_pp[all_data_average_pp$spacing_in_deg %in% c(0, 0.2, 0.39, 0.5, 0.6), ] 
all_data_average_usd = all_data_average_usd[all_data_average_usd$spacing_in_deg %in% c(0, 0.2, 0.39, 0.5, 0.6), ] 
all_data_average_usd_pp = all_data_average_usd_pp[all_data_average_usd_pp$spacing_in_deg %in% c(0, 0.2, 0.39, 0.5, 0.6), ] 


my_plot <-  ggplot() +
  
  geom_point(data = all_data_average, aes(x = setsize,
                                y = mean_deviation_score,
                                color = stimulus_types,
                                shape = stimulus_types),
           position = "dodge", stat = "identity", alpha = 0.6) +
  
  scale_shape_manual(values = c(16, 15, 18)) +
  
  scale_color_manual(values = c("#000080", "#E7B800"))+
  
  scale_x_continuous(breaks = c(3, 4, 5, 6)) +
  
  scale_y_continuous(limits = c(-1, 1)) +
  
  geom_hline(aes(x = setsize,
                 y = mean_deviation_score), yintercept = 0) +
  
  # # each data point represents the average deviation of 1 participant
  # geom_point(data = all_data_average_pp, aes(x = setsize,
  #                                         y = mean_deviation_score,
  #                                         group = stimulus_type,
  #                                         colour = stimulus_type),
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
  
  facet_wrap( ~ spacing_in_deg + size_w , nrow = 2)

print(my_plot)


my_plot2 <-  ggplot() +
  
  geom_bar(data = all_data_average, aes(x = setsize,
                                          y = mean_deviation_score,
                                          fill = stimulus_types),
             position = "dodge", stat = "identity", alpha = 0.5, width = 0.5) +
  
  
  # each data point represents the average deviation of 1 participant
  geom_point(data = all_data_average_pp, aes(x = setsize,
                                             y = mean_deviation_score,
                                             group = stimulus_types,
                                             color = stimulus_types),
             alpha = 0.2,
             position = position_dodge(0.5))+
  
  geom_errorbar(data = all_data_average, aes(x = setsize,
                                   y = mean_deviation_score,
                                   ymin = mean_deviation_score - SEM,
                                   ymax = mean_deviation_score + SEM,
                                   group = stimulus_types), 
    
              color = "black",
              size  = 1.2,
              width = .00,
              position = position_dodge(0.5)) +

# scale_fill_manual(values = c("radial" = "#ff4500",
#                              "tangential" = "#4169e1")) +
# 
# scale_colour_manual(values = c("radial" = "#ff4500",
#                                "tangential" = "#4169e1")) +

  labs(y = "Deviation score", x = "Set size") +
  
  scale_color_manual(values = c("stims/NF.png" = "#000080",
                                "stims/NF_usd.png" = "#E7B800")) +
  
  scale_fill_manual(values = c("stims/NF.png" = "#000080",
                                "stims/NF_usd.png" = "#E7B800")) +
  
  scale_x_continuous(breaks = c(3, 4, 5, 6)) +
  
  scale_y_continuous(limits = c(-2.5, 2)) +
  
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
  
  facet_wrap( ~ spacing_in_deg + size_w, 
              nrow = 2,
              labeller = labeller(spacing_in_deg = 
                                    c("0.2" = "spacing = 0.2",
                                      "0" = "spacing = 0.2",
                                      "0.39" = "spacing = 0.2",
                                      "0.5" = "spacing = 0.2",
                                      "0.6" = "spacing = 0.2"), 
                                  size_w = c("0.74" = "face_width = 0.74",
                                             "0.936" = "face_width = 0.936",
                                             "1.132" = "face_width = 1.132")))

print(my_plot2)



my_plot3 <-  ggplot() +
  
  geom_bar(data = all_data_average_usd, aes(x = setsize,
                                        y = mean_resp_usd,
                                        fill = stimulus_types),
           position = "dodge", stat = "identity", alpha = 0.5, width = 0.5) +
  
  geom_point(data = all_data_average_usd_pp, aes(x = setsize,
                                             y = mean_resp_usd,
                                             group = stimulus_types,
                                             color = stimulus_types),
             alpha = 0.2,
             position = position_dodge(0.5))+
  
  geom_errorbar(data = all_data_average_usd, aes(x = setsize,
                                             y = mean_resp_usd,
                                             ymin = mean_resp_usd - SEM,
                                             ymax = mean_resp_usd + SEM,
                                             group = stimulus_types), 
                
                color = "black",
                size  = 1.2,
                width = .00,
                position = position_dodge(0.5)) +
  
  
  labs(y = "Deviation score", x = "Set size") +
  
  scale_color_manual(values = c("stims/NF.png" = "#000080",
                                "stims/NF_usd.png" = "#E7B800")) +
  
  scale_fill_manual(values = c("stims/NF.png" = "#000080",
                               "stims/NF_usd.png" = "#E7B800")) +
  
  scale_x_continuous(breaks = c(3, 4, 5, 6)) +
  
  scale_y_continuous(limits = c(0, 1)) +
  
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
  
  facet_wrap( ~ spacing_in_deg + size_w, 
              nrow = 2,
              labeller = labeller(spacing_in_deg = 
                                    c("0.2" = "spacing = 0.2",
                                      "0" = "spacing = 0.2",
                                      "0.39" = "spacing = 0.2",
                                      "0.5" = "spacing = 0.2",
                                      "0.6" = "spacing = 0.2"), 
                                  size_w = c("0.74" = "face_width = 0.74",
                                             "0.936" = "face_width = 0.936",
                                             "1.132" = "face_width = 1.132")))

print(my_plot3)