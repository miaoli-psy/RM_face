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
setwd("D:/SCALab/projects/RM_face/exp1/data/to_plot_discri/")

all_data_average <- read_excel("rm_face_exp1_discri.xlsx")
all_data_average_pp <- read_excel("rm_face_exp1_discri_perpp.xlsx")

all_data_average_pp$participant <- as.factor(all_data_average_pp$participant)


my_plot <- ggplot() +
  
  geom_point(data = all_data_average, aes(x = setsize,
                                          y = percent_correct,
                                          color = type,
                                          shape = type),
             position = "dodge", stat = "identity", alpha = 1) +
  
  # each data point represents the average deviation of 1 participant
  geom_point(data = all_data_average_pp, aes(x = setsize,
                                             y = percent_correct,
                                             color = type,
                                             shape = type),
             alpha = 0.2,
             position = position_dodge(0.2)) +
  
  
  scale_color_manual(values = c("face" = "#000080",
                                "outline" = "#E7B800",
                                "surface" = "#FC4E07")) +
  
  labs(y = "Percentage correct", x = "Setsize") +



  # scale_fill_manual(values = c("face" = "#000080",
  #                              "outline" = "#E7B800",
  #                              "surface" = "#FC4E07")) +
  # 
  scale_x_discrete(limits = c("1", "2")) +
  
  scale_y_continuous(limits = c(0, 1)) +
  
  geom_hline(aes(x = setsize,
                 y = percent_correct), yintercept = 0.5, linetype="dotted") +
  
  geom_hline(aes(x = setsize,
                 y = percent_correct), yintercept = 0.75, linetype="dotted") +
  
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
