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

data_preprocessed <- read_excel("exp2_preprocessed.xlsx")


# check: number of RM trials ----------------------------------------------

count_rm_trails <- data_preprocessed %>% 
  group_by(setsize) %>% 
  count(is_rm_trial)

count_rm_trails

my_plot0 <- ggplot() +
  
  geom_bar(data = count_rm_trails, aes(x = identity,
                                       y = n,
                                       fill = is_rm_trial),
           position=position_dodge(), stat = "identity", alpha = 0.8) +
  
  geom_text(data = count_rm_trails, aes(x = identity,
                                        y = n,
                                        label = n), vjust = 1.6) +
  
  
  scale_fill_manual(labels = c("non RM trails", "RM Trials"),
                     values = c("#8e8bfe", "#E88482")) +
  
  labs(y = "count trial number", x = "face orientation") +
  
  facet_wrap(~setsize)

my_plot0

# plot: deviation score - stimuli type, for all spacing -------------------

# data by subject
data_by_subject <- data_preprocessed %>%
  group_by(setsize, participant, identity, spacing_in_deg, size_scale) %>%
  summarise(deviation_score_mean = mean(deviation_score),
            deviation_score_std = sd(deviation_score),
            resp_usd_mean = mean(response_usd),
            resp_usd_std = sd(response_usd))

# data across subject
data_across_subject <- data_preprocessed %>%
  group_by(setsize, identity, spacing_in_deg, size_scale) %>%
  summarise(deviation_score_mean = mean(deviation_score),
            deviation_score_std = sd(deviation_score),
            resp_usd_mean = mean(response_usd),
            resp_usd_std = sd(response_usd))

# samplesize = 144(each condition 12 repetitions  * 12 participant)
data_across_subject <- data_across_subject %>%
  mutate(deviation_socre_SEM = deviation_score_std / sqrt(72),
         resp_usd_SEM = resp_usd_std / sqrt(72))


my_plot <-  ggplot() +
  
  geom_point(data = data_across_subject, aes(x = spacing_in_deg,
                                             y = deviation_score_mean,
                                             fill = identity,
                                             color = identity),
             position = position_dodge(0.2), stat = "identity", alpha = 1, size = 2) +
  
  scale_size_manual(values = c("small" = 2, "middle"= 4, "large" = 6)) +
  
  geom_hline(aes(x = identity,
                 y = deviation_score_mean), yintercept = 0) +
  
  
  # each data point represents the average deviation of 1 participant
  geom_point(data = data_by_subject, aes(x = spacing_in_deg,
                                         y = deviation_score_mean,
                                         color = identity),
             alpha = 0.2,
             position = position_dodge(0.2))+
  
  geom_errorbar(data = data_across_subject, aes(x = spacing_in_deg,
                                                y = deviation_score_mean,
                                                ymin = deviation_score_mean - deviation_socre_SEM,
                                                ymax = deviation_score_mean + deviation_socre_SEM,
                                                group = identity),
                
                color = "black",
                size  = 0.8,
                width = .00,
                alpha = 0.8,
                position = position_dodge(0.2)) +
  
  labs(y = "Deviation score", x = "Spacing") +
  
  
  # scale_color_manual(values = c("NF" = "#8e8bfe",
  #                               "NF_usd" = "#E88482")) +
  
  scale_color_manual(labels = c("upright face", "upside down face"),
                     values = c("#8e8bfe", "#E88482"),
                     name = "face orientation") +
  
  scale_fill_manual(labels = c("upright face", "upside down face"),
                   values = c("#8e8bfe", "#E88482"),
                   name = "face orientation") +
  
  
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
                                    c("3" = "3 items",
                                      "4" = "4 items",
                                      "5" = "5 items",
                                      "6" = "6 items"),
                                  size_scale =
                                    c("large" = "large",
                                      "middle" = "middle",
                                      "small" = "small")))

print(my_plot)


# plot: Deviation socre - stumuli type, combined spacing---------

# data by subject
data_by_subject2 <- data_preprocessed %>%
  group_by(setsize, participant, identity, size_scale) %>%
  summarise(deviation_score_mean = mean(deviation_score),
            deviation_score_std = sd(deviation_score),
            resp_usd_mean = mean(response_usd),
            resp_usd_std = sd(response_usd))

# data across subject
data_across_subject2 <- data_preprocessed %>%
  group_by(setsize, identity, size_scale) %>%
  summarise(deviation_score_mean = mean(deviation_score),
            deviation_score_std = sd(deviation_score),
            resp_usd_mean = mean(response_usd),
            resp_usd_std = sd(response_usd))

# samplesize = 576(each condition 12 * 4 repitation * 12 participant)
data_across_subject2 <- data_across_subject %>%
  mutate(deviation_socre_SEM = deviation_score_std / sqrt(576),
         resp_usd_SEM = resp_usd_std / sqrt(576))


my_plot2 <-  ggplot() +
  
  geom_point(data = data_across_subject2, aes(x = identity,
                                             y = resp_usd_mean,
                                             size = size_scale,
                                             color = size_scale),
             position = position_dodge(0.2), stat = "identity", alpha = 0.6, width = 0.2) +
  
  geom_point(data = data_by_subject2, aes(x = identity,
                                         y = resp_usd_mean,
                                         size = size_scale,
                                         color = size_scale),
             alpha = 0.05,
             position = position_dodge(0.2)) +
  
  
  geom_errorbar(data = data_across_subject2, aes(x = identity,
                                                y = resp_usd_mean,
                                                ymin = resp_usd_mean - resp_usd_SEM,
                                                ymax = resp_usd_mean + resp_usd_SEM,
                                                group = size_scale),
                
                color = "black",
                size  = 0.8,
                width = .00,
                position = position_dodge(0.2)) +
  
  
  labs(y = "Percent correct", x = "face orientation") +
  

  scale_color_manual(labels = c("large", "middle", "small"),
                     values = c("#004488", "#BB5566", "#DDAA33"),
                     name = "stimuli size") +


  scale_size_manual(labels = c("large", "middle", "small"),
                    values = c("large" = 6, "middle"= 4, "small" = 2),
                    name = "stimuli size") +
  
  scale_y_continuous(limits = c(0, 1)) +
  
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
                              "NF_usd" = "upside down")) +
  
  facet_wrap( ~ setsize,
              nrow = 2,
              labeller = labeller(setsize =
                                    c("3" = "3 items",
                                      "4" = "4 items",
                                      "5" = "5 items",
                                      "6" = "6 items")))
print(my_plot2)



# plot: deviation score - set size,  separately for rm and non-rm --------

data_by_subject3 <- data_preprocessed %>%
  group_by(setsize, participant, identity, size_scale, is_rm_trial) %>%
  summarise(deviation_score_mean = mean(deviation_score),
            deviation_score_std = sd(deviation_score),
            resp_usd_mean = mean(response_usd),
            resp_usd_std = sd(response_usd))

data_across_subject3 <- data_preprocessed %>%
  group_by(setsize, identity,size_scale, is_rm_trial) %>%
  summarise(deviation_score_mean = mean(deviation_score),
            deviation_score_std = sd(deviation_score),
            resp_usd_mean = mean(response_usd),
            resp_usd_std = sd(response_usd))

# samplesize = 144(each condition 12*3 repitation * 12 participant)
data_across_subject3 <- data_across_subject2 %>%
  mutate(deviation_socre_SEM = deviation_score_std / sqrt(432),
         resp_usd_SEM = resp_usd_std / sqrt(432))





# plot: Deviation socre - stumuli type, combined spacing ---------


# data by subject
data_by_subject2 <- data_preprocessed %>%
  group_by(setsize, participant, identity, size_scale) %>%
  summarise(deviation_score_mean = mean(deviation_score),
            deviation_score_std = sd(deviation_score),
            resp_usd_mean = mean(response_usd),
            resp_usd_std = sd(response_usd))


# data across subject
data_across_subject2 <- data_preprocessed %>%
  group_by(setsize, identity, size_scale) %>%
  summarise(deviation_score_mean = mean(deviation_score),
            deviation_score_std = sd(deviation_score),
            resp_usd_mean = mean(response_usd),
            resp_usd_std = sd(response_usd))

# samplesize = 144(each condition 12 repetation * 12 participant)
data_across_subject2 <- data_across_subjec2t %>%
  mutate(deviation_socre_SEM = deviation_score_std / sqrt(144),
         resp_usd_SEM = resp_usd_std / sqrt(144))





# plot: remove setsize 3 extra large spacing ------------------------------

# data by subject

data_preprocessed<-subset(data_preprocessed, spacing!="to_match")

data_by_subject2 <- data_preprocessed %>%
  group_by(setsize, participant, identity,size_scale, is_rm_trial) %>%
  summarise(deviation_score_mean = mean(deviation_score),
            deviation_score_std = sd(deviation_score),
            resp_usd_mean = mean(response_usd),
            resp_usd_std = sd(response_usd))

#


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

# d prime -----------------------------------------------------------------

# get "dprime.csv" and calculate in python
# data_preprocessed<-subset(data_preprocessed, spacing!="to_match")
# 
# data_by_subject3 <- data_preprocessed %>%
#   group_by(setsize, participant, size_scale, is_rm_trial) %>%
#   summarise(hit = sum(hit),
#             miss = sum(miss),
#             CR = sum(CR),
#             FA = sum(FA))
# 
# # data across subject
# data_across_subject3 <- data_preprocessed %>%
#   group_by(setsize, size_scale, is_rm_trial) %>%
#   summarise(hit = sum(hit),
#             miss = sum(miss),
#             CR = sum(CR),
#             FA = sum(FA))
# 
# write.csv(data_by_subject3, "rm_face_to_cal_SDT.csv")

data_cdt <- read_excel("rm_face_SDT.xlsx")


# plot: d_prime - spacing -------------------------------------------------



