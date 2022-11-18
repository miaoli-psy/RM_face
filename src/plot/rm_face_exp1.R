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
library(svglite)

# set working path
setwd("D:/SCALab/projects/RM_face/data/")

data_preprocessed <- read_excel("exp1_preprocessed.xlsx")


# Plot: Deviation score - all spacing in deg ------------------------------

# data by subject
data_by_subject <- data_preprocessed %>%
  group_by(setsize, participant, stimulus_types, spacing_in_deg, size_scale) %>%
  summarise(deviation_score_mean = mean(deviation_score),
            deviation_score_std = sd(deviation_score),
            rt_mean = mean(rt),
            rt_std = sd(rt))

# data across subject
data_across_subject <- data_preprocessed %>%
  group_by(setsize, stimulus_types, spacing_in_deg, size_scale) %>%
  summarise(deviation_score_mean = mean(deviation_score),
            deviation_score_std = sd(deviation_score),
            rt_mean = mean(rt),
            rt_std = sd(rt))

# samplesize = 60 (each condition 6 repetition * 12 participant)
data_across_subject <- data_across_subject %>%
  mutate(deviation_socre_SEM = deviation_score_std / sqrt(72),
         rt_SEM = rt_std / sqrt(72))


my_plot <-  ggplot() +
  
  geom_point(data = data_across_subject, aes(x = spacing_in_deg,
                                             y = deviation_score_mean,
                                             color= stimulus_types,
                                             group = stimulus_types),
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
                                             group = stimulus_types,
                                             color = stimulus_types), 
                
                size  = 1.2,
                width = .00,
                position = position_dodge(0.2)) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  labs(y = "Deviation score", x = "Spacing (deg)") +
  
  scale_color_manual(labels = c("face", "scrambled face", "outline"),
                     values = c("#000080", "#E7B800","#FC4E07"),
                     name = "stimulus type" ) +
  
  scale_x_continuous(limits = c(-0.05, 1.5)) +
  
  
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
        strip.text.x = element_text(size = 12, face = "bold")) +
  
  facet_wrap( ~ setsize + size_scale,
              nrow = 4,
              labeller = labeller(setsize =
                                    c("3" = "setsize = 3",
                                      "4" = "setsize = 4",
                                      "5" = "setsize = 5",
                                      "6" = "setsize = 6"),
                                  size_scale = c("small" = "small 0.74*1.1",
                                                 "middle" = "medium 0.936*1.39",
                                                 "large" = "large 1.132*1.68")))

print(my_plot)


# Plot: Deviation score - each type of stimuli (combine close spacing)-------------------


# data by subject
data_by_subject2 <- data_preprocessed %>%
  group_by(setsize, participant, stimulus_types, spacing, size_scale) %>%
  summarise(deviation_score_mean = mean(deviation_score),
            deviation_score_std = sd(deviation_score),
            rt_mean = mean(rt),
            rt_std = sd(rt))

# data across subject
data_across_subject2 <- data_preprocessed %>%
  group_by(setsize, stimulus_types, spacing, size_scale) %>%
  summarise(deviation_score_mean = mean(deviation_score),
            deviation_score_std = sd(deviation_score),
            rt_mean = mean(rt),
            rt_std = sd(rt))

# samplesize = 60 (each condition 5 repetition * 12 participant)
data_across_subject2 <- data_across_subject2 %>%
  mutate(deviation_socre_SEM = deviation_score_std / sqrt(60),
         rt_SEM = rt_std / sqrt(60))


data_across_subject2$spacing <- as.factor(data_across_subject2$spacing)


my_plot2 <-  ggplot() +
  
  geom_point(data = data_across_subject2, aes(x = stimulus_types,
                                              y = deviation_score_mean,
                                              size = size_scale,
                                              color = size_scale),
             position = position_dodge(0.2), stat = "identity", alpha = 0.8) +
  
  scale_size_manual(values = c("small" = 2, "middle"= 4, "large" = 6)) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  
  # each data point represents the average deviation of 1 participant
  geom_point(data = data_by_subject2, aes(x = stimulus_types,
                                          y = deviation_score_mean,
                                          size = size_scale,
                                          color = size_scale),
             alpha = 0.1,
             position = position_dodge(0.2))+
  
  geom_errorbar(data = data_across_subject2, aes(x = stimulus_types,
                                             y = deviation_score_mean,
                                             ymin = deviation_score_mean - deviation_socre_SEM,
                                             ymax = deviation_score_mean + deviation_socre_SEM,
                                             group = size_scale),
                
                color = "black",
                size  = 0.8,
                width = .00,
                position = position_dodge(0.2)) +
  
  labs(y = "Deviation score", x = "Stimuli types") +
  
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
        strip.text.x = element_text(size = 12, face = "bold")) +
  
  
  facet_wrap( ~setsize + spacing, nrow = 4,
              labeller = labeller(spacing =
                                    c("large" = "large: 0.39, 0.5, 0.6 for 3 sizes",
                                      "minimum" = "0-0.02",
                                      "small" = "0.1-0.25",
                                      "to_match" = "setsize 3 to match")))

print(my_plot2)


# combine spacing, plot deviation score/CV - each type of stimuli, combined spacing-------------

data_by_subject3 <- data_preprocessed %>%
  group_by(stimulus_types,
           participant,
           setsize,
           size_scale) %>%
  summarise(
    deviation_score_mean = mean(deviation_score),
    deviation_score_std = sd(deviation_score),
    n = n(),
    rt_mean = mean(rt),
    rt_std = sd(rt),
  ) %>%
  mutate(
    deviation_socre_SEM = deviation_score_std / sqrt(n),
    deviation_socre_CI = deviation_socre_SEM * qt((1 - 0.05) / 2 + .5, n - 1),
    rt_SEM = rt_std / sqrt(n),
    cv = deviation_score_std / setsize,
    cv_SEM = cv / sqrt(n),
    rt_SEM = rt_std / sqrt(n)
  )


data_across_subject3 <- data_preprocessed %>%
  group_by(stimulus_types,
           setsize,
           size_scale) %>%
  summarise(
    deviation_score_mean = mean(deviation_score),
    deviation_score_std = sd(deviation_score),
    n = n(),
    rt_mean = mean(rt),
    rt_std = sd(rt),
  ) %>%
  mutate(
    deviation_socre_SEM = deviation_score_std / sqrt(n),
    deviation_socre_CI = deviation_socre_SEM * qt((1 - 0.05) / 2 + .5, n - 1),
    rt_SEM = rt_std / sqrt(n),
    cv = deviation_score_std / setsize,
    cv_SEM = cv / sqrt(n),
    rt_SEM = rt_std / sqrt(n)
  )

 
my_plot3 <-  ggplot() +
  
  geom_point(data = data_across_subject3, aes(x = stimulus_types,
                                              y = deviation_score_mean,
                                              size = size_scale,
                                              color = size_scale),
             position = position_dodge(0.4), stat = "identity", alpha = 0.8) +
  
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  
  # each data point represents the average deviation of 1 participant
  geom_point(data = data_by_subject3, aes(x = stimulus_types,
                                          y = deviation_score_mean,
                                          size = size_scale,
                                          color = size_scale),
             alpha = 0.1,
             position = position_dodge(0.4))+
  
  geom_errorbar(data = data_across_subject3, aes(x = stimulus_types,
                                                 y = deviation_score_mean,
                                                 ymin = deviation_score_mean - deviation_socre_SEM,
                                                 ymax = deviation_score_mean + deviation_socre_SEM,
                                                 group = size_scale),
                
                color = "black",
                size  = 0.8,
                width = .00,
                position = position_dodge(0.4)) +
  
  labs(y = "Deviation Score (DV)", x = "Stimulus Type") +
  
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
        strip.text.x = element_text(size = 12, face = "bold")) +
  
  facet_wrap( ~setsize, nrow = 1,
              labeller = labeller(setsize =
                                    c("3" = "set size 3",
                                      "4" = "set size 4",
                                      "5" = "set size 5",
                                      "6" = "set size 6")))


print(my_plot3)


my_plot3_b <-  ggplot() +
  
  geom_point(data = data_across_subject3, aes(x = stimulus_types,
                                              y = cv,
                                              size = size_scale,
                                              color = size_scale),
             position = position_dodge(0.4), stat = "identity", alpha = 0.8) +
  
  
  # each data point represents the average deviation of 1 participant
  geom_point(data = data_by_subject3, aes(x = stimulus_types,
                                          y = cv,
                                          size = size_scale,
                                          color = size_scale),
             alpha = 0.1,
             position = position_dodge(0.4))+
  
  geom_errorbar(data = data_across_subject3, aes(x = stimulus_types,
                                                 y = cv,
                                                 ymin =  cv - cv_SEM,
                                                 ymax = cv + cv_SEM,
                                                 group = size_scale),
                
                color = "black",
                size  = 0.8,
                width = .00,
                position = position_dodge(0.4)) +
  
  labs(y = "Coefficient of Variation (CV) ", x = "Stimulus Type") +
  
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
        strip.text.x = element_text(size = 12, face = "bold")) +
  
  facet_wrap( ~setsize, nrow = 1,
              labeller = labeller(setsize =
                                    c("3" = "set size 3",
                                      "4" = "set size 4",
                                      "5" = "set size 5",
                                      "6" = "set size 6")))

print(my_plot3_b)

ggsave(file = "3.svg", plot = my_plot3, width = 11.3, height = 3.36, units = "in")
ggsave(file = "3b.svg", plot = my_plot3_b,width = 11.3, height = 3.36, units = "in")


# plot: deviation socre - set size, seprate plot for stim type ------------

my_plot4 <-  ggplot() +
  
  geom_point(data = data_across_subject3, aes(x = setsize,
                                              y = deviation_score_mean,
                                              size = size_scale,
                                              color = size_scale),
             position = position_dodge(0.8), stat = "identity", alpha = 0.8, width = 0.2) +
  
  scale_size_manual(values = c("small" = 2, "middle"= 4, "large" = 6)) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  
  # each data point represents the average deviation of 1 participant
  geom_point(data = data_by_subject3, aes(x = setsize,
                                          y = deviation_score_mean,
                                          size = size_scale,
                                          color = size_scale),
             alpha = 0.1,
             position = position_dodge(0.8))+
  
  geom_errorbar(data = data_across_subject3, aes(x = setsize,
                                                 y = deviation_score_mean,
                                                 ymin = deviation_score_mean - deviation_socre_SEM,
                                                 ymax = deviation_score_mean + deviation_socre_SEM,
                                                 group = size_scale),
                
                color = "black",
                size  = 0.8,
                width = .00,
                position = position_dodge(0.8)) +
  
  labs(y = "Deviation score", x = "Set size") +
  
  scale_color_manual(labels = c("large", "middle", "small"),
                     values = c("#004488", "#BB5566", "#DDAA33"),
                     name = "stimuli size") +
  
  
  scale_size_manual(labels = c("large", "middle", "small"),
                    values = c("large" = 6, "middle"= 4, "small" = 2),
                    name = "stimuli size") +
  
  scale_x_continuous(breaks = c(3, 4, 5, 6),
                   labels = c("3", "4", "5", "6"),
                   expand = c(0.1, 0.5)) +
  
  
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
        strip.text.x = element_text(size = 12, face = "bold"),
        panel.spacing = unit(2.5, "lines")) +

  
  facet_wrap( ~stimulus_types,
              labeller = labeller(stimulus_types =
                                    c("NF" = "face",
                                      "NF_scramble" = "scrambled face",
                                      "outline" = "outline")))

print(my_plot4)

ggsave(file = "test.svg", plot = my_plot4, width = 11.3, height = 3.93, units = "in")


# bar plot deviation score-----------------------------------------

# all conditions, all stimuli

data_preprocessed$size_scale <- factor(data_preprocessed$size_scale,      
                                levels = c("small", "middle", "large"))

my_plot5 <-
  
  ggplot(data = data_preprocessed, 
         aes(x = response)) +
  
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count") +
  
  geom_text(aes(label = scales::percent(..prop..),
                 y = ..prop.. ), stat = "count", vjust = -.5, size = 2.5) +
  
  labs(y = "Percent response", fill = "Response", x = "Response") +
  
  scale_y_continuous(labels = scales::percent) +
  
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8)) +
  
  
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
        strip.text.x = element_text(size = 12, face = "bold"),
        panel.spacing = unit(2.5, "lines")) +
  
  
  facet_wrap( ~stimulus_types * size_scale * setsize,
              labeller = labeller(stimulus_types =
                                    c("NF" = "face",
                                      "NF_scramble" = "noise patch",
                                      "outline" = "outline"),
                                  size_scale = 
                                    c("large" = "large stimuli",
                                      "middle" = "medium stimuli",
                                      "small" = "small stimuli"),
                                  setsize = 
                                    c("3" = "set size 3",
                                      "4" = "set size 4",
                                      "5" = "set size 5",
                                      "6" = "set size 6")))

my_plot5

ggsave(file = "5.svg", plot = my_plot5, width = 16.7, height = 16, units = "in")

# only face

data_faces <- data_preprocessed %>% 
  filter((stimulus_types == "NF" & setsize == 3))

data_faces$size_scale <- factor(data_faces$size_scale,      
                         levels = c("small", "middle", "large"))


my_plot5_b <-
  
  ggplot(data = data_faces, aes(x = response)) +
  
  geom_bar(aes(y = ..prop..), stat = "count", fill = "#EEEEEE") +
  
  geom_text(aes(label = scales::percent(..prop..),
                y = ..prop.. ), stat = "count", vjust = -.5, size = 2.5) +
  
  labs(y = "Percent Response", x = "Response") +
  
  scale_y_continuous(labels = scales::percent) +
  
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8)) +
  
  
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
        strip.text.x = element_text(size = 12, face = "bold"),
        panel.spacing = unit(2.5, "lines")) +
  
  
  facet_wrap( ~ size_scale * setsize,
              labeller = labeller(size_scale = 
                                    c("large" = "large stimuli",
                                      "middle" = "medium stimuli",
                                      "small" = "small stimuli"),
                                  setsize = 
                                    c("3" = "set size 3",
                                      "4" = "set size 4",
                                      "5" = "set size 5",
                                      "6" = "set size 6")))

my_plot5_b

ggsave(file = "5b.svg", plot = my_plot5_b, width = 11.3, height = 3.4, units = "in")
