# libraires ---------------------------------------------------------------
if(!require(readxl)){
  install.packages("readxl")
  library(readxl)
}

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}

if(!require(ggthemes)){
  install.packages("ggthemes")
  library(ggthemes)
}

if(!require(svglite)){
  install.packages("svglite")
  library(svglite)
}

if(!require(sjPlot)){
  install.packages("sjPlot")
  library(sjPlot)
}

if(!require(ggpubr)){
  install.packages("ggpubr")
  library(ggpubr)
}

# Exp1--------------------------------------------------------------------------

# set working path
# setwd("c:/SCALab/projects/RM_face/data/")

# read data
# data_preprocessed <- read_excel("exp1_preprocessed.xlsx")
data_preprocessed <- read_excel(path = file.choose())



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


my_plot3_c <-  ggplot() +
  
  geom_point(data = data_across_subject3, aes(x = stimulus_types,
                                              y = deviation_score_mean,
                                              size = size_scale,
                                              color = size_scale),
             position = position_dodge(0.4), stat = "identity", alpha = 0.8) +
  
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  
  geom_errorbar(data = data_across_subject3, aes(x = stimulus_types,
                                                 y = deviation_score_mean,
                                                 ymin = deviation_score_mean - deviation_socre_CI,
                                                 ymax = deviation_score_mean + deviation_socre_CI,
                                                 group = size_scale),
                
                color = "black",
                size  = 0.8,
                width = .00,
                position = position_dodge(0.4)) +
  
  labs(y = "Deviation score", x = "Stimulus type") +
  
  scale_y_continuous(limits = c(-1.5, 0)) +
  
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
        strip.text.x = element_text(size = 12, face = "bold"),
        panel.spacing = unit(3, "lines")) +
  
  facet_wrap( ~setsize, nrow = 1,
              labeller = labeller(setsize =
                                    c("3" = "set size 3",
                                      "4" = "set size 4",
                                      "5" = "set size 5",
                                      "6" = "set size 6")))


print(my_plot3_c)

ggsave(file = "3.svg", plot = my_plot3, width = 11.3, height = 3.36, units = "in")
ggsave(file = "3b.svg", plot = my_plot3_b,width = 11.3, height = 3.36, units = "in")
ggsave(file = "3c.svg", plot = my_plot3_c,width = 13, height = 3.36, units = "in")


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
  # geom_point(data = data_by_subject3, aes(x = setsize,
  #                                         y = deviation_score_mean,
  #                                         size = size_scale,
  #                                         color = size_scale),
  #            alpha = 0.1,
  #            position = position_dodge(0.8))+
  
  geom_errorbar(data = data_across_subject3, aes(x = setsize,
                                                 y = deviation_score_mean,
                                                 ymin = deviation_score_mean - deviation_socre_CI,
                                                 ymax = deviation_score_mean + deviation_socre_CI,
                                                 group = size_scale,
                                                 color = size_scale),
                
                size  = 0.8,
                width = .00,
                position = position_dodge(0.8)) +
  
  labs(y = "Deviation Score (DV)", x = "Set Size") +
  
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
        panel.spacing = unit(1.0, "lines")) +

  
  facet_wrap( ~stimulus_types,
              labeller = labeller(stimulus_types =
                                    c("NF" = "face",
                                      "NF_scramble" = "scrambled face",
                                      "outline" = "outline")))

print(my_plot4)

ggsave(file = "test.svg", plot = my_plot4, width = 11.3, height = 3.93, units = "in")
ggsave(file = "test.svg", plot = my_plot4, width = 7.42, height = 2.7, units = "in")



# Bar plots: percent responses: only face

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
