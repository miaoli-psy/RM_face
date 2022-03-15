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
setwd("D:/SCALab/projects/RM_face/exp2/data/to_plot_discri/")

all_data_average <- read_excel("rm_face_exp2_discri_numtask.xlsx")
all_data_average_pp <- read_excel("rm_face_exp2_discri_numtask_perpp.xlsx")
all_data_average_usd <- read_excel("rm_face_exp2_discri_usdtask.xlsx")
all_data_average_usd_pp <- read_excel("rm_face_exp2_discri_usdtask_perpp.xlsx")



my_plot <-  ggplot() +
  
  geom_point(data = all_data_average, aes(x = setsize,
                                          y = percent_correct,
                                          color = type,
                                          size = stimulus_size),
             
             position = position_dodge(0.2), stat = "identity", alpha = 0.8) +
  
  scale_size_manual(values = c("small" = 2, "medium"= 4, "large" = 6)) +
  
  # each data point represents the average deviation of 1 participant
  geom_point(data = all_data_average_pp, aes(x = setsize,
                                             y = percent_correct,
                                             color = type,
                                             size = stimulus_size),
             alpha = 0.2,
             position = position_dodge(0.2))+
  
  # geom_errorbar(data = all_data_average, aes(x = setsize,
  #                                            y = percent_correct,
  #                                            ymin = percent_correct - SEM,
  #                                            ymax = percent_correct + SEM,
  #                                            group = stimulus_size,
  #                                            color = type),
  # 
  #               color = "black",
  #               size  = 1.2,
  #               width = .00,
  #               position = position_dodge(0.2)) +
  # 
  labs(y = "Percent correct discri num", x = "Set size") +
  
  scale_color_manual(values = c("normal" = "#000080",
                                "upside_down" = "#E7B800")) +
  

  
  # scale_fill_manual(values = c("face" = "#000080",
  #                              "outline" = "#E7B800",
  #                              "surface" = "#FC4E07")) +
  # 
  scale_x_discrete(limits = c("1", "2")) +

  scale_y_continuous(limits = c(0, 1.2)) +
  
  theme(axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        panel.border = element_blank(),  
        # Remove panel grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Remove panel background
        panel.background = element_blank(),
        # Add axis line
        axis.line = element_line(colour = "grey"))
  

print(my_plot)


my_plot2 <-  ggplot() +
  
  geom_point(data = all_data_average_usd, aes(x = setsize,
                                        y = mean_resp_usd,
                                        size = stimulus_size,
                                        color = type),
             position = position_dodge(0.2), stat = "identity", alpha = 1, width = 0.2) +
  
  scale_size_manual(values = c("small" = 2, "medium"= 4, "large" = 6)) +
  
  geom_point(data = all_data_average_usd_pp, aes(x = setsize,
                                             y = mean_resp_usd,
                                             size = stimulus_size,
                                             color = type),
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
  
  
  labs(y = "Percent correct discri normal/usd", x = "Set size") +
  
  scale_color_manual(values = c("normal" = "#000080",
                                "upside_down" = "#E7B800")) +

  scale_y_continuous(limits = c(0, 1)) +
  
  scale_x_discrete(limits = c("1", "2")) +
  
  theme(axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        panel.border = element_blank(),  
        # Remove panel grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Remove panel background
        panel.background = element_blank(),
        # Add axis line
        axis.line = element_line(colour = "grey"))
  

print(my_plot2)
