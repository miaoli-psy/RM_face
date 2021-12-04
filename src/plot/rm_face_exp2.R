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
setwd("D:/SCALab/projects/RM_face/exp2/data/to_plot_new_spacing/")

all_data_average <- read_excel("rm_face_exp2_average.xlsx")
all_data_average_pp <- read_excel("rm_face_exp2_average_perpp.xlsx")
all_data_average_usd <- read_excel("rm_face_exp2_usd.xlsx")
all_data_average_usd_pp <- read_excel("rm_face_exp2_usd_perpp.xlsx")


all_data_average$spacing <- factor(all_data_average$spacing, levels = c("minimum spacing: 0 - 0.02",
                                                                        "small spacing 0.1 - 0.25",
                                                                        "large spacing 0.39, 0.5, 0.6 for small, medium, large size",
                                                                        "large spacing for setsize 3 to match"))


all_data_average_pp$spacing <- factor(all_data_average_pp$spacing, levels = c("minimum spacing: 0 - 0.02",
                                                                        "small spacing 0.1 - 0.25",
                                                                        "large spacing 0.39, 0.5, 0.6 for small, medium, large size",
                                                                        "large spacing for setsize 3 to match"))


all_data_average_usd$spacing <- factor(all_data_average_usd$spacing, levels = c("minimum spacing: 0 - 0.02",
                                                                        "small spacing 0.1 - 0.25",
                                                                        "large spacing 0.39, 0.5, 0.6 for small, medium, large size",
                                                                        "large spacing for setsize 3 to match"))

all_data_average_usd_pp$spacing <- factor(all_data_average_usd_pp$spacing, levels = c("minimum spacing: 0 - 0.02",
                                                                        "small spacing 0.1 - 0.25",
                                                                        "large spacing 0.39, 0.5, 0.6 for small, medium, large size",
                                                                        "large spacing for setsize 3 to match"))


my_plot <-  ggplot() +
  
  geom_point(data = all_data_average, aes(x = type,
                                          y = mean_deviation_score,
                                          size = stimulus_size,
                                          color = stimulus_size),
             position = "dodge", stat = "identity", alpha = 0.4, width = 0.2) +
  
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
                                             ymax = mean_deviation_score + SEM),
                
                color = "black",
                size  = 0.6,
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

print(my_plot)


my_plot2 <-  ggplot() +
  
  geom_point(data = all_data_average_usd, aes(x = type,
                                        y = mean_resp_usd,
                                        size = stimulus_size,
                                        color = stimulus_size),
           position = "dodge", stat = "identity", alpha = 1, width = 0.2) +
  
  scale_size_manual(values = c("small" = 2, "medium"= 4, "large" = 6)) +
  
  geom_point(data = all_data_average_usd_pp, aes(x = type,
                                             y = mean_resp_usd,
                                             size = stimulus_size,
                                             color = stimulus_size),
             alpha = 0.05,
             position = position_dodge(0.2))+
  
  # geom_errorbar(data = all_data_average_usd, aes(x = type,
  #                                            y = mean_resp_usd,
  #                                            ymin = mean_resp_usd - SEM,
  #                                            ymax = mean_resp_usd + SEM),
  # 
  #               color = "black",
  #               size  = 0.6,
  #               width = 0.02,
  #               position = position_dodge(0.2),
  #               alpha = 0.5) +
  
  
  labs(y = "Percent correct", x = "Type") +
  
  scale_color_manual(values = c("small" = "#000080",
                                "medium" = "#E7B800",
                                "large" = "#FC4E07")) +
  
  # scale_fill_manual(values = c("face" = "#000080",
  #                              "outline" = "#E7B800",
  #                              "surface" = "#FC4E07")) +
  # 
  # scale_x_continuous(limits = c(-0.05, 1.5)) +

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
  
  facet_wrap( ~ setsize + spacing,
              nrow = 4,
              labeller = labeller(setsize =
                                    c("3" = "setsize = 3",
                                      "4" = "setsize = 4",
                                      "5" = "setsize = 5",
                                      "6" = "setsize = 6")))

print(my_plot2)
