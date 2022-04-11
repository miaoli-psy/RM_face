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

data_preprocessed <- read_excel("exp2_preprocessed.xlsx")


# plot: Deviation socre - stumuli type, for each group of spacing ---------


# data by subject
data_by_subject <- data_preprocessed %>%
  group_by(setsize, participant, identity, spacing, size_scale) %>%
  summarise(deviation_score_mean = mean(deviation_score),
            deviation_score_std = sd(deviation_score),
            resp_usd_mean = mean(response_usd),
            resp_usd_std = sd(response_usd))

# data across subject
data_across_subject <- data_preprocessed %>%
  group_by(setsize, identity, spacing, size_scale) %>%
  summarise(deviation_score_mean = mean(deviation_score),
            deviation_score_std = sd(deviation_score),
            resp_usd_mean = mean(response_usd),
            resp_usd_std = sd(response_usd))

# samplesize = 144(each condition 12 repitation * 12 participant)
data_across_subject <- data_across_subject %>%
  mutate(deviation_socre_SEM = deviation_score_std / sqrt(144),
         resp_usd_SEM = resp_usd_std / sqrt(144))

my_plot <-  ggplot() +
  
  geom_point(data = data_across_subject, aes(x = identity,
                                          y = deviation_score_mean,
                                          size = size_scale,
                                          color = size_scale),
             position = position_dodge(0.2), stat = "identity", alpha = 0.4) +
  
  scale_size_manual(values = c("small" = 2, "middle"= 4, "large" = 6)) +
  
  geom_hline(aes(x = identity,
                 y = deviation_score_mean), yintercept = 0) +
  
  
  # each data point represents the average deviation of 1 participant
  # geom_point(data = all_data_average_pp, aes(x = type,
  #                                            y = mean_deviation_score,
  #                                            size = stimulus_size),
  #            alpha = 0.2,
  #            position = position_dodge(0.2))+
  
  geom_errorbar(data = data_across_subject, aes(x = identity,
                                             y = deviation_score_mean,
                                             ymin = deviation_score_mean - deviation_socre_SEM,
                                             ymax = deviation_score_mean + deviation_socre_SEM,
                                             group = size_scale,
                                             size = size_scale),
                
                color = "black",
                size  = 0.8,
                width = .00,
                position = position_dodge(0.2)) +
  
  labs(y = "Deviation score", x = "Type") +
  
  scale_color_manual(values = c("small" = "#000080",
                                "middle" = "#E7B800",
                                "large" = "#FC4E07")) +
  
  
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
                                      "6" = "setsize = 6"),
                                  spacing =
                                    c("large" = "large: 0.39, 0.5, 0.6 for 3 sizes",
                                      "small" = "0.1-0.25",
                                      "minimum" = "0-0.02",
                                      "to_match" = "setsize 3 to match")))

print(my_plot)


my_plot2 <-  ggplot() +
  
  geom_point(data = data_across_subject, aes(x = identity,
                                              y = resp_usd_mean,
                                              size = size_scale,
                                              color = size_scale),
             position = position_dodge(0.2), stat = "identity", alpha = 0.8, width = 0.2) +
  
  scale_size_manual(values = c("small" = 2, "middle"= 4, "large" = 6)) +
  
  geom_point(data = data_by_subject, aes(x = identity,
                                                 y = resp_usd_mean,
                                                 size = size_scale,
                                                 color = size_scale),
             alpha = 0.05,
             position = position_dodge(0.2)) +

  
  geom_errorbar(data = data_across_subject, aes(x = identity,
                                             y = resp_usd_mean,
                                             ymin = resp_usd_mean - resp_usd_SEM,
                                             ymax = resp_usd_mean + resp_usd_SEM,
                                             group = size_scale),

                color = "black",
                size  = 0.8,
                width = .00,
                position = position_dodge(0.2)) +


labs(y = "Percent correct", x = "Type") +
  
  scale_color_manual(values = c("small" = "#000080",
                                "middle" = "#E7B800",
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
                                      "6" = "setsize = 6"),
                                  spacing =
                                    c("large" = "large: 0.39, 0.5, 0.6 for 3 sizes",
                                      "small" = "0.1-0.25",
                                      "minimum" = "0-0.02",
                                      "to_match" = "setsize 3 to match")))

print(my_plot2)


# plot: remove setsize 3 extra large spacing ------------------------------

# data by subject

data_preprocessed<-subset(data_preprocessed, spacing!="to_match")

data_by_subject2 <- data_preprocessed %>%
  group_by(setsize, participant, identity,size_scale, is_rm_trial) %>%
  summarise(deviation_score_mean = mean(deviation_score),
            deviation_score_std = sd(deviation_score),
            resp_usd_mean = mean(response_usd),
            resp_usd_std = sd(response_usd))

# data across subject
data_across_subject2 <- data_preprocessed %>%
  group_by(setsize, identity,size_scale, is_rm_trial) %>%
  summarise(deviation_score_mean = mean(deviation_score),
            deviation_score_std = sd(deviation_score),
            resp_usd_mean = mean(response_usd),
            resp_usd_std = sd(response_usd))

# samplesize = 144(each condition 12*3 repitation * 12 participant)
data_across_subject2 <- data_across_subject2 %>%
  mutate(deviation_socre_SEM = deviation_score_std / sqrt(432),
         resp_usd_SEM = resp_usd_std / sqrt(432))


summary <- data_preprocessed %>%
  group_by(is_rm_trial) %>%
  summarize(
    mean = mean(deviation_score, na.rm = TRUE),
    std_dev = sd(deviation_score, na.rm = TRUE)
  )
summary

table(data_preprocessed$is_rm_trial)


my_plot3 <-  ggplot(data = data_across_subject2, aes(x = identity,
                                                     y = resp_usd_mean,
                                                     size = size_scale,
                                                     color = is_rm_trial)) +
  
  geom_point(position = position_dodge(0.2), stat = "identity", alpha = 0.5) +
  
  scale_size_manual(values = c("small" = 2, "middle"= 4, "large" = 6)) +
  
  # geom_point(data = all_data_average_usd_pp, aes(x = type,
  #                                                y = mean_resp_usd,
  #                                                size = stimulus_size,
  #                                                color = stimulus_size),
  #            alpha = 0.05,
  #            position = position_dodge(0.2))+
  
  geom_errorbar(aes(x = identity, y = resp_usd_mean,
                    ymin = resp_usd_mean - resp_usd_SEM,
                    ymax = resp_usd_mean + resp_usd_SEM,
                    group = size_scale,
                    color = is_rm_trial),
                
                size  = 0.8,
                width = .00,
                position = position_dodge(0.2),
                alpha = 0.8) +
  
  
  labs(y = "Percent correct", x = "Type") +
  
  scale_color_manual(values = c("RM trials" = "#E69138",
                                "non RM trials" = "#674EA7")) +
  
  scale_fill_manual(values = c("RM trials" = "#E69138",
                                "non RM trials" = "#674EA7")) +
  
  # scale_x_continuous(limits = c(-0.05, 1.5)) +
  
# scale_y_continuous(limits = c(0, 1.5)) +
coord_cartesian(ylim = c(0, 1)) +
  
  geom_hline(yintercept = 0.75, linetype = "dotted") +
  
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
              labeller = labeller(setsize =
                                    c("3" = "setsize = 3",
                                      "4" = "setsize = 4",
                                      "5" = "setsize = 5",
                                      "6" = "setsize = 6")))

print(my_plot3)



