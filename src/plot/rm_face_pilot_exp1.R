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
setwd("D:/SCALab/projects/RM_face/exp1/data/to_plot_ori_spacing/")

all_data_average <- read_excel("rm_face_exp1_average.xlsx")
all_data_average_pp <- read_excel("rm_face_exp1_average_perpp.xlsx")
first_face_block <- read_excel("rm_face_exp1_firstfaceblock.xlsx")
first_face_block_pp <- read_excel("rm_face_exp1_firstfaceblock_perpp.xlsx")

# all_data_average = all_data_average[all_data_average$spacing_in_deg %in% c(0, 0.2, 0.39, 0.5, 0.6), ] 
# all_data_average_pp = all_data_average_pp[all_data_average_pp$spacing_in_deg %in% c(0, 0.2, 0.39, 0.5, 0.6), ] 


my_plot <-  ggplot() +
  
  geom_bar(data = all_data_average, aes(x = spacing_in_deg,
                                        y = mean_deviation_score,
                                        fill = type),
           position = "dodge", stat = "identity", alpha = 0.5, width = 0.2) +
  
  
  # each data point represents the average deviation of 1 participant
  geom_point(data = all_data_average_pp, aes(x = spacing_in_deg,
                                             y = mean_deviation_score,
                                             group = type,
                                             color = type),
             alpha = 0.2,
             position = position_dodge(0.2))+
  
  geom_errorbar(data = all_data_average, aes(x = spacing_in_deg,
                                   y = mean_deviation_score,
                                   ymin = mean_deviation_score - SEM,
                                   ymax = mean_deviation_score + SEM,
                                   fill = type), 
    
              color = "black",
              size  = 1.2,
              width = .00,
              position = position_dodge(0.2)) +

  labs(y = "Deviation score", x = "Spacing (deg)") +
  
  scale_color_manual(values = c("NF" = "#000080",
                                "NF_scramble" = "#E7B800",
                                "outline" = "#FC4E07")) +

  scale_fill_manual(values = c("NF" = "#000080",
                               "NF_scramble" = "#E7B800",
                               "outline" = "#FC4E07")) +
  
  scale_x_continuous(limits = c(-0.05, 1.5)) +
  
  scale_y_continuous(limits = c(-1.5, 1.5)) +
  
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
                                                    "medium" = "medium face 0.936*1.39",
                                                    "large" = "large face 1.132*1.68")))

print(my_plot)


# exp start with face
my_plot2 <-  ggplot() +
  
  geom_point(data = first_face_block, aes(x = spacing_in_deg,
                                        y = mean_deviation_score,
                                        shape = stimulus_size,
                                        color = stimulus_size)) +
  
  

  # each data point represents the average deviation of 1 participant
  geom_point(data = first_face_block_pp, aes(x = spacing_in_deg,
                                             y = mean_deviation_score,
                                             shape = stimulus_size,
                                             color = stimulus_size),
             alpha = 0.2,
             position = position_dodge(0.2))+
  
  geom_errorbar(data = first_face_block, aes(x = spacing_in_deg,
                                             y = mean_deviation_score,
                                             ymin = mean_deviation_score - SEM,
                                             ymax = mean_deviation_score + SEM), 
                
                color = "black",
                size  = 0.5,
                width = .00,
                position = position_dodge(0.2)) +
  
  scale_color_manual(values = c("small" = "#000080",
                                "medium" = "#E7B800",
                                "large" = "#FC4E07")) +
  
  scale_shape_manual(values = c(15, 16, 17)) +
  
  scale_size_manual(values = c(2, 3, 4)) +

  
  labs(y = "Deviation score", x = "Spacing (deg)") +
  
  
  scale_x_continuous(limits = c(-0.05, 1.5)) +
  
  scale_y_continuous(limits = c(-1.5, 1.5)) +
  
  geom_hline(aes(x = setsize,
                 y = mean_deviation_score), yintercept = 0) +
  
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
  
  facet_wrap( ~ setsize,
              nrow = 2,
              labeller = labeller(setsize =
                                    c("3" = "setsize = 3",
                                      "4" = "setsize = 4",
                                      "5" = "setsize = 5",
                                      "6" = "setsize = 6")))

print(my_plot2)


# set working path: new spacing
setwd("D:/SCALab/projects/RM_face/exp1/data/to_plot_new_spacing/")

all_data_average <- read_excel("rm_face_exp1_average.xlsx")
all_data_average_pp <- read_excel("rm_face_exp1_average_perpp.xlsx")

all_data_average$spacing <- factor(all_data_average$spacing, levels = c("minimum spacing: 0 - 0.02",
                                                                        "small spacing 0.1 - 0.25",
                                                                        "large spacing 0.39, 0.5, 0.6 for small, medium, large size",
                                                                        "large spacing for setsize 3 to match"))


all_data_average_pp$spacing <- factor(all_data_average_pp$spacing, levels = c("minimum spacing: 0 - 0.02",
                                                                              "small spacing 0.1 - 0.25",
                                                                              "large spacing 0.39, 0.5, 0.6 for small, medium, large size",
                                                                              "large spacing for setsize 3 to match"))

my_plot3 <-  ggplot() +
  
  geom_point(data = all_data_average, aes(x = type,
                                        y = mean_deviation_score,
                                        size = stimulus_size,
                                        color = stimulus_size),
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

  geom_errorbar(data = all_data_average, aes(x = type,
                                             y = mean_deviation_score,
                                             ymin = mean_deviation_score - SEM,
                                             ymax = mean_deviation_score + SEM,
                                             group = stimulus_size),

                color = "black",
                size  = 0.8,
                width = .00,
                position = position_dodge(0.2)) +
  
  labs(y = "Deviation score", x = "Type") +
  
  scale_color_manual(values = c("small" = "#000080",
                                "medium" = "#E7B800",
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




