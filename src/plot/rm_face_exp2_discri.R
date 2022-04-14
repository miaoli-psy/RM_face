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
library(psycho)

# set working path
setwd("D:/SCALab/projects/RM_face/data/")

data_preprocessed <- read_excel("exp2_disc_preprocessed.xlsx")

data_preprocessed <- data_preprocessed %>% 
  mutate(is_num_corr = if_else(deviation_score == 0, 1, 0))

# plot: percent correct of discrimination 1 or 2 - face ori ----------------------------------------

# data by subject
data_by_subject <- data_preprocessed %>%
  group_by(participant, identity, size_scale) %>%
  summarise(is_num_corr_mean = mean(is_num_corr),
            is_num_corr_std = sd(is_num_corr),
            resp_usd_mean = mean(response_usd),
            resp_usd_std = sd(response_usd))

# data across subject
data_across_subject <- data_preprocessed %>%
  group_by(identity, size_scale) %>%
  summarise(is_num_corr_mean = mean(is_num_corr),
            is_num_corr_std = sd(is_num_corr),
            resp_usd_mean = mean(response_usd),
            resp_usd_std = sd(response_usd))

# samplesize = 144(each condition 24 repetition * 12 participant)
data_across_subject <- data_across_subject %>%
  mutate(is_num_corr_SEM = is_num_corr_std / sqrt(288),
         resp_usd_SEM = resp_usd_std / sqrt(288))


my_plot <-  ggplot() +
  
  geom_point(data = data_across_subject, aes(x = identity,
                                              y = is_num_corr_mean,
                                              size = size_scale,
                                              group = size_scale,
                                              color = size_scale),
             position = position_dodge(0.2), stat = "identity", alpha = 0.6) +
  
  geom_point(data = data_by_subject, aes(x = identity,
                                          y = is_num_corr_mean,
                                          size = size_scale,
                                          color = size_scale),
             alpha = 0.1,
             position = position_dodge(0.2)) +
  
  
  geom_errorbar(data = data_across_subject, aes(x = identity,
                                                 y = is_num_corr_mean,
                                                 ymin = is_num_corr_mean - is_num_corr_SEM,
                                                 ymax = is_num_corr_mean + is_num_corr_SEM,
                                                 group = size_scale),
                color = "black",
                
                size  = 0.8,
                width = .00,
                position = position_dodge(0.2)) +
  
  geom_hline(yintercept = 0.95, linetype = "dashed") +
  
  
  labs(y = "Percent correct (to discriminate 1 or 2 faces)", x = "face orientation") +
  
  
  scale_color_manual(labels = c("large", "middle", "small"),
                     values = c("#004488", "#BB5566", "#DDAA33"),
                     name = "stimuli size") +
  
  
  scale_size_manual(labels = c("large", "middle", "small"),
                    values = c("large" = 6, "middle"= 4, "small" = 2),
                    name = "stimuli size") +
  
  # scale_y_continuous(limits = c(0, 1)) +
  
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
  
  scale_x_discrete(labels = c("NF" = "upright",
                              "NF_usd" = "upside down"))
  

print(my_plot)


# plot: percent correct - setsize -----------------------------------------

# data by subject
data_by_subject2 <- data_preprocessed %>%
  group_by(participant, setsize, size_scale) %>%
  summarise(is_num_corr_mean = mean(is_num_corr),
            is_num_corr_std = sd(is_num_corr),
            resp_usd_mean = mean(response_usd),
            resp_usd_std = sd(response_usd))

# data across subject
data_across_subject2 <- data_preprocessed %>%
  group_by(setsize, size_scale) %>%
  summarise(is_num_corr_mean = mean(is_num_corr),
            is_num_corr_std = sd(is_num_corr),
            resp_usd_mean = mean(response_usd),
            resp_usd_std = sd(response_usd))

# samplesize = 144(each condition 24 repetition * 12 participant)
data_across_subject2 <- data_across_subject2 %>%
  mutate(is_num_corr_SEM = is_num_corr_std / sqrt(288),
         resp_usd_SEM = resp_usd_std / sqrt(288))


my_plot2 <-  ggplot() +
  
  geom_point(data = data_across_subject2, aes(x = setsize,
                                              y = resp_usd_mean,
                                              size = size_scale,
                                              group = size_scale,
                                              color = size_scale),
             position = position_dodge(0.2), stat = "identity", alpha = 0.6) +
  
  geom_point(data = data_by_subject2, aes(x = setsize,
                                          y = resp_usd_mean,
                                          size = size_scale,
                                          color = size_scale),
             alpha = 0.1,
             position = position_dodge(0.2)) +
  
  
  geom_errorbar(data = data_across_subject2, aes(x = setsize,
                                                 y = resp_usd_mean,
                                                 ymin = resp_usd_mean - resp_usd_SEM,
                                                 ymax = resp_usd_mean + resp_usd_SEM,
                                                 group = size_scale),
                color = "black",
                size  = 0.8,
                width = .00,
                position = position_dodge(0.2)) +
  
  geom_hline(yintercept = 0.75, linetype = "dashed") +
  
  
  labs(y = "Percent correct", x = "set size") +
  
  
  scale_color_manual(labels = c("large", "middle", "small"),
                     values = c("#004488", "#BB5566", "#DDAA33"),
                     name = "stimuli size") +
  
  
  scale_size_manual(labels = c("large", "middle", "small"),
                    values = c("large" = 6, "middle"= 4, "small" = 2),
                    name = "stimuli size") +
  
  # scale_y_continuous(limits = c(0, 1)) +
  
  scale_x_continuous(breaks = c(1, 2),
                     labels = c("1", "2"),
                     expand = c(0.3, 0.3)) +
  
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
        strip.text.x = element_text(size = 12, face = "bold"))


print(my_plot2)


# get SDT data-----------------------------------------------------------------

# get "dprime.csv" and calculate in python

# data_by_subject3 <- data_preprocessed %>%
#   group_by(setsize, participant, size_scale) %>%
#   summarise(hit = sum(hit),
#             miss = sum(miss),
#             CR = sum(CR),
#             FA = sum(FA))
# 
# # data across subject
# data_across_subject3 <- data_preprocessed %>%
#   group_by(setsize, size_scale) %>%
#   summarise(hit = sum(hit),
#             miss = sum(miss),
#             CR = sum(CR),
#             FA = sum(FA))
# 
# write.csv(data_by_subject3, "rm_face_disc_to_cal_SDT.csv")

data_cdt <- read_excel("rm_face_disc_SDT.xlsx")


# plot: d_prime for each set size ----------------------------------

# data by subject (identical as data_cdt, no std could be calculated)
data_by_subject4 <- data_cdt %>%
  group_by(setsize, participant, size_scale) %>%
  summarise(d_prime_mean = mean(d_prime),
            d_prime_std = sd(d_prime),
            c_mean = mean(c),
            c_std = sd(c))

# data across subject
data_across_subject4 <- data_cdt %>%
  group_by(setsize, size_scale) %>%
  summarise(d_prime_mean = mean(d_prime),
            d_prime_std = sd(d_prime),
            c_mean = mean(c),
            c_std = sd(c))

# samplesize = 2 * 12 participants
data_across_subject4 <- data_across_subject4 %>%
  mutate(d_prime_SEM = d_prime_std / sqrt(24),
         c_SEM = c_std / sqrt(24))


my_plot3 <-  ggplot() +
  
  geom_point(data = data_across_subject4, aes(x = setsize,
                                              y = d_prime_mean,
                                              size = size_scale,
                                              group = size_scale,
                                              color = size_scale),
             
             position = position_dodge(0.2), stat = "identity", alpha = 0.6, width = 0.2) +
  
  geom_point(data = data_by_subject4, aes(x = setsize,
                                          y = d_prime_mean,
                                          size = size_scale,
                                          group = size_scale,
                                          color = size_scale),
             alpha = 0.05,
             position = position_dodge(0.2)) +
  
  
  geom_errorbar(data = data_across_subject4, aes(x = setsize,
                                                 y = d_prime_mean,
                                                 ymin = d_prime_mean - d_prime_SEM,
                                                 ymax = d_prime_mean + d_prime_SEM,
                                                 group = size_scale,
                                                 color = size_scale),
                
                size  = 0.8,
                width = .00,
                alpha = 0.8,
                position = position_dodge(0.2)) +
  
  
  labs(y = "sensitivity (d')", x = "set size") +
  
  # scale_y_continuous(limits = c(-0.5, 4)) +
  
  scale_x_continuous(breaks = c(1, 2),
                     labels = c("1", "2"),
                     expand = c(0.3, 0.3)) +
  
  
  scale_color_manual(labels = c("large", "middle", "small"),
                     values = c("#004488", "#BB5566", "#DDAA33"),
                     name = "stimuli size") +
  
  
  scale_size_manual(labels = c("large", "middle", "small"),
                    values = c("large" = 6, "middle"= 4, "small" = 2),
                    name = "stimuli size") +
  
  
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
        strip.text.x = element_text(size = 12, face = "bold"))


print(my_plot3)


# plot: c - setsize -------------------------------------------------------

my_plot4 <-  ggplot() +
  
  geom_point(data = data_across_subject4, aes(x = setsize,
                                              y = c_mean,
                                              size = size_scale,
                                              group = size_scale,
                                              color = size_scale),
             
             position = position_dodge(0.2), stat = "identity", alpha = 0.6, width = 0.2) +
  
  geom_point(data = data_by_subject4, aes(x = setsize,
                                          y = c_mean,
                                          size = size_scale,
                                          group = size_scale,
                                          color = size_scale),
             alpha = 0.05,
             position = position_dodge(0.2)) +
  
  
  geom_errorbar(data = data_across_subject4, aes(x = setsize,
                                                 y = c_mean,
                                                 ymin = c_mean - c_SEM,
                                                 ymax = c_mean + c_SEM,
                                                 group = size_scale,
                                                 color = size_scale),
                
                size  = 0.8,
                width = .00,
                alpha = 0.8,
                position = position_dodge(0.2)) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  
  labs(y = "bias (criterion)", x = "set size") +
  
  
  scale_x_continuous(breaks = c(1, 2),
                     labels = c("1", "2"),
                     expand = c(0.3, 0.3)) +
  
  
  scale_color_manual(labels = c("large", "middle", "small"),
                     values = c("#004488", "#BB5566", "#DDAA33"),
                     name = "stimuli size") +
  
  
  scale_size_manual(labels = c("large", "middle", "small"),
                    values = c("large" = 6, "middle"= 4, "small" = 2),
                    name = "stimuli size") +
  
  
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
        strip.text.x = element_text(size = 12, face = "bold"))


print(my_plot4)

