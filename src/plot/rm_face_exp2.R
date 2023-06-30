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
library(svglite)

# set working path
setwd("c:/SCALab/projects/RM_face/data/")

data_preprocessed <- read_excel("exp2_preprocessed.xlsx"
                                )


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

# plot: deviation score - face ori, for all spacing -------------------

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

# samplesize = 144(each condition 12 repetition  * 12 participant)
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


# plot: percent correct - face ori, combined spacing---------

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

# samplesize = 576(each condition 12 * 4 repetition * 12 participant)
data_across_subject2 <- data_across_subject2 %>%
  mutate(deviation_socre_SEM = deviation_score_std / sqrt(576),
         resp_usd_SEM = resp_usd_std / sqrt(576))


my_plot2 <-  ggplot() +
  
  geom_point(data = data_across_subject2, aes(x = identity,
                                             y = resp_usd_mean,
                                             size = size_scale,
                                             group = size_scale,
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



# plot: deviation score - face ori combined spacing -----------------------
my_plot6 <-  ggplot() +
  
  geom_point(data = data_across_subject2, aes(x = identity,
                                              y = deviation_score_mean,
                                              size = size_scale,
                                              group = size_scale,
                                              color = size_scale),
             position = position_dodge(0.5), stat = "identity", alpha = 0.6, width = 0.2) +
  
  geom_point(data = data_by_subject2, aes(x = identity,
                                          y = deviation_score_mean,
                                          size = size_scale,
                                          color = size_scale),
             alpha = 0.05,
             position = position_dodge(0.5)) +
  
  
  geom_errorbar(data = data_across_subject2, aes(x = identity,
                                                 y = deviation_score_mean,
                                                 ymin = deviation_score_mean - deviation_socre_SEM,
                                                 ymax = deviation_score_mean + deviation_socre_SEM,
                                                 group = size_scale),
                color = "black",
                
                size  = 0.8,
                width = .00,
                position = position_dodge(0.5)) +
  
  geom_jitter()+
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  
  labs(y = "Deviation score", x = "Face orientation") +
  
  
  scale_color_manual(labels = c("large", "middle", "small"),
                     values = c("#004488", "#BB5566", "#DDAA33"),
                     name = "face size") +
  
  
  scale_size_manual(labels = c("large", "middle", "small"),
                    values = c("large" = 6, "middle"= 4, "small" = 2),
                    name = "face size") +
  
  scale_y_continuous(limits = c(-2, 1)) +
  
  theme(axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        
        panel.border = element_blank(),  
        # remove panel grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove panel background
        panel.background = element_blank(),
        panel.spacing = unit(0.4, "cm"),
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
                              "NF_usd" = "upside-down")) +
  
  facet_wrap( ~ setsize,
              nrow = 1,
              labeller = labeller(setsize =
                                    c("3" = "3 faces",
                                      "4" = "4 faces",
                                      "5" = "5 faces",
                                      "6" = "6 faces")))
print(my_plot6)

ggsave(file = "test.svg", plot = my_plot6)


# plot: cv- setsize combined spacing, orientation -----------------------

data_by_subject3 <- data_preprocessed %>%
  group_by(participant,
           setsize,
           size_scale) %>%
  summarise(
    deviation_score_mean = mean(deviation_score),
    deviation_score_std = sd(deviation_score),
    n = n(),
    rt_mean = mean(response1_rt),
    rt_std = sd(response1_rt),
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
  group_by(setsize,
           size_scale) %>%
  summarise(
    deviation_score_mean = mean(deviation_score),
    deviation_score_std = sd(deviation_score),
    n = n(),
    rt_mean = mean(response1_rt),
    rt_std = sd(response1_rt),
  ) %>%
  mutate(
    deviation_socre_SEM = deviation_score_std / sqrt(n),
    deviation_socre_CI = deviation_socre_SEM * qt((1 - 0.05) / 2 + .5, n - 1),
    rt_SEM = rt_std / sqrt(n),
    cv = deviation_score_std / setsize,
    cv_SEM = cv / sqrt(n),
    rt_SEM = rt_std / sqrt(n)
  )



my_plot7 <-  ggplot() +
  
  geom_point(data = data_across_subject3, aes(x = setsize,
                                              y = cv,
                                              size = size_scale,
                                              group = size_scale,
                                              color = size_scale),
             position = position_dodge(0.4), stat = "identity", alpha = 0.8) +
  
  geom_point(data = data_by_subject3, aes(x = setsize,
                                          y = cv,
                                          size = size_scale,
                                          color = size_scale),
             alpha = 0.1,
             position = position_dodge(0.4)) +
  
  
  geom_errorbar(data = data_across_subject3, aes(x = setsize,
                                                 y = cv,
                                                 ymin = cv - cv_SEM,
                                                 ymax = cv + cv_SEM,
                                                 group = size_scale),
                color = "black",
                
                size  = 0.8,
                width = .00,
                position = position_dodge(0.4)) +
  
  geom_jitter()+
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  
  labs(y = "Coefficient of Variation (CV)", x = "Set Size") +
  
  
  scale_color_manual(labels = c("large", "middle", "small"),
                     values = c("#004488", "#BB5566", "#DDAA33"),
                     name = "face size") +
  
  
  scale_size_manual(labels = c("large", "middle", "small"),
                    values = c("large" = 6, "middle"= 4, "small" = 2),
                    name = "face size") +
  
  # scale_y_continuous(limits = c(-2, 1)) +
  
  theme(axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        
        panel.border = element_blank(),  
        # remove panel grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove panel background
        panel.background = element_blank(),
        panel.spacing = unit(0.4, "cm"),
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
  
  
  
  # scale_x_discrete(labels = c("NF" = "upright",
  #                             "NF_usd" = "upside-down"))
  
  # facet_wrap( ~ setsize,
  #             nrow = 1,
  #             labeller = labeller(setsize =
  #                                   c("3" = "3 faces",
  #                                     "4" = "4 faces",
  #                                     "5" = "5 faces",
  #                                     "6" = "6 faces")))
print(my_plot7)

# plot: deviation score- setsize combined spacing, orientation -----------------------

my_plot7b <-  ggplot() +
  
  geom_point(data = data_across_subject3, aes(x = setsize,
                                              y = deviation_score_mean,
                                              size = size_scale,
                                              group = size_scale,
                                              color = size_scale),
             position = position_dodge(0.4), stat = "identity", alpha = 0.8) +
  
  geom_point(data = data_by_subject3, aes(x = setsize,
                                          y = deviation_score_mean,
                                          size = size_scale,
                                          color = size_scale),
             alpha = 0.1,
             position = position_dodge(0.4)) +
  
  
  geom_errorbar(data = data_across_subject3, aes(x = setsize,
                                                 y = deviation_score_mean,
                                                 ymin = deviation_score_mean - deviation_socre_SEM,
                                                 ymax = deviation_score_mean + deviation_socre_SEM,
                                                 group = size_scale),
                color = "black",
                
                size  = 0.8,
                width = .00,
                position = position_dodge(0.4)) +
  
  geom_jitter()+
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  
  labs(y = "Deviation Score (DV)", x = "Set Size") +
  
  
  scale_color_manual(labels = c("large", "middle", "small"),
                     values = c("#004488", "#BB5566", "#DDAA33"),
                     name = "face size") +
  
  
  scale_size_manual(labels = c("large", "middle", "small"),
                    values = c("large" = 6, "middle"= 4, "small" = 2),
                    name = "face size") +
  
  # scale_y_continuous(limits = c(-2, 1)) +
  
  theme(axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        
        panel.border = element_blank(),  
        # remove panel grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove panel background
        panel.background = element_blank(),
        panel.spacing = unit(0.4, "cm"),
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



# scale_x_discrete(labels = c("NF" = "upright",
#                             "NF_usd" = "upside-down"))

# facet_wrap( ~ setsize,
#             nrow = 1,
#             labeller = labeller(setsize =
#                                   c("3" = "3 faces",
#                                     "4" = "4 faces",
#                                     "5" = "5 faces",
#                                     "6" = "6 faces")))
print(my_plot7b)


my_plot7c <-  ggplot() +
  
  geom_point(data = data_across_subject3, aes(x = setsize,
                                              y = deviation_score_mean,
                                              size = size_scale,
                                              group = size_scale,
                                              color = size_scale),
             position = position_dodge(0.4), stat = "identity", alpha = 0.8) +
  
  
  geom_errorbar(data = data_across_subject3, aes(x = setsize,
                                                 y = deviation_score_mean,
                                                 ymin = deviation_score_mean - deviation_socre_SEM,
                                                 ymax = deviation_score_mean + deviation_socre_SEM,
                                                 group = size_scale),
                color = "black",
                
                size  = 0.8,
                width = .00,
                position = position_dodge(0.4)) +
  
  geom_jitter()+
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  
  labs(y = "Deviation score", x = "Set Size") +
  
  scale_y_continuous(limits = c(-1.5, 0)) +
  
  
  scale_color_manual(labels = c("large", "middle", "small"),
                     values = c("#004488", "#BB5566", "#DDAA33"),
                     name = "face size") +
  
  
  scale_size_manual(labels = c("large", "middle", "small"),
                    values = c("large" = 6, "middle"= 4, "small" = 2),
                    name = "face size") +
  
  # scale_y_continuous(limits = c(-2, 1)) +
  
  theme(axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        
        panel.border = element_blank(),  
        # remove panel grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove panel background
        panel.background = element_blank(),
        panel.spacing = unit(0.4, "cm"),
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


print(my_plot7c)

ggsave(file = "my_plot7.svg", plot = my_plot7, width = 4.2, height = 3.36, units = "in")
ggsave(file = "my_plot7b.svg", plot = my_plot7b, width = 4.2, height = 3.36, units = "in")
ggsave(file = "my_plot7c.svg", plot = my_plot7c, width = 4.2, height = 3.36, units = "in")

# plot: percent correct - set size,  separately for rm and non-rm --------

data_by_subject3 <- data_preprocessed %>%
  group_by(setsize, participant, size_scale, is_rm_trial) %>%
  summarise(deviation_score_mean = mean(deviation_score),
            deviation_score_std = sd(deviation_score),
            resp_usd_mean = mean(response_usd),
            resp_usd_std = sd(response_usd))

data_across_subject3 <- data_preprocessed %>%
  group_by(setsize, size_scale, is_rm_trial) %>%
  summarise(deviation_score_mean = mean(deviation_score),
            deviation_score_std = sd(deviation_score),
            resp_usd_mean = mean(response_usd),
            resp_usd_std = sd(response_usd))

# samplesize = 144(each condition 12* 4 repetition * 12 participant)
data_across_subject3 <- data_across_subject3 %>%
  mutate(deviation_socre_SEM = deviation_score_std / sqrt(576),
         resp_usd_SEM = resp_usd_std / sqrt(576))


my_plot3 <-  ggplot() +
  
  geom_point(data = data_across_subject3, aes(x = setsize,
                                              y = resp_usd_mean,
                                              group = size_scale,
                                              size = size_scale,
                                              color = is_rm_trial),
             
             position = position_dodge(width = 0.2), stat = "identity", alpha = 0.6) +
  
  geom_point(data = data_by_subject3, aes(x = setsize,
                                          y = resp_usd_mean,
                                          size = size_scale,
                                          color = is_rm_trial),
             alpha = 0.05,
             position = position_dodge(width = 0.2)) +
  
  
  geom_errorbar(data = data_across_subject3, aes(x = setsize,
                                                 y = resp_usd_mean,
                                                 ymin = resp_usd_mean - resp_usd_SEM,
                                                 ymax = resp_usd_mean + resp_usd_SEM,
                                                 group = size_scale,
                                                 color = is_rm_trial),

                size  = 0.8,
                width = .00,
                alpha = 0.5,
                position = position_dodge(width = 0.2)) +
  
  
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  
  geom_hline(yintercept = 0.75, linetype = "dashed") +
  
  
  labs(y = "Percent correct", x = "Set size") +
  

  scale_color_manual(labels = c("non RM", "RM"),
                     values = c("#004488", "#BB5566"),
                     name = "RM or not") +

  
  scale_size_manual(labels = c("large", "middle", "small"),
                    values = c("large" = 6, "middle"= 4, "small" = 2),
                    name = "face size") +
  
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
        strip.text.x = element_text(size = 12, face = "bold"))
  
  
print(my_plot3)

ggsave(file = "test.svg", plot = my_plot3)

# check percent correct differences between rm and non-rm
data_by_subject3$setsize <- as.factor(data_by_subject3$setsize)
data_by_subject3$is_rm_trial <- as.factor(data_by_subject3$is_rm_trial)
data_by_subject3$participant <- as.factor(data_by_subject3$participant)
data_by_subject3$size_scale <- as.factor(data_by_subject3$size_scale)

str(data_by_subject3)

# data_by_subject4 <- subset(data_by_subject4, size_scale == "small")

alignment_con.model_random_slope <-
  lmer(
    resp_usd_mean ~ setsize + is_rm_trial + 
      (1 |participant),
    data = data_by_subject3,
    REML = FALSE
  )
alignment_con.model_random_slope


coef(alignment_con.model_random_slope)

alignment_con.null_random_slope <-
  lmer(
    resp_usd_mean ~ setsize + 
      (1 | participant),
    data = data_by_subject3,
    REML = FALSE
  )
alignment_con.null_random_slope


anova(alignment_con.model_random_slope, 
      alignment_con.null_random_slope)



# get SDT data-----------------------------------------------------------------

# get "dprime.csv" and calculate in python
# data_preprocessed<-subset(data_preprocessed, spacing!="to_match")

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

data_cdt <- subset(data_cdt, is_rm_trial == "RM trials" | is_rm_trial == "correct trials")

# plot: d_prime as a function of spacing ----------------------------------

# data by subject (identical as data_cdt, no std could be calcuated)
data_by_subject4 <- data_cdt %>%
  group_by(setsize, participant, size_scale, is_rm_trial) %>%
  summarise(d_prime_mean = mean(d_prime),
            d_prime_std = sd(d_prime),
            c_mean = mean(c),
            c_std = sd(c))

# data across subject
data_across_subject4 <- data_cdt %>%
  group_by(setsize, size_scale, is_rm_trial) %>%
  summarise(
    d_prime_mean = mean(d_prime),
    d_prime_std = sd(d_prime),
    c_mean = mean(c),
    c_std = sd(c),
    n = n()
    ) %>% 
  mutate(
    d_prime_SEM = d_prime_std / sqrt(n),
    c_SEM = c_std /sqrt(n)
    )

# plot: d_prime for each set size, separately for rm and non-rm trials--------

# reorder factor
data_across_subject4$is_rm_trial <- factor(data_across_subject4$is_rm_trial, 
                                           levels = c("RM trials","correct trials"))

data_by_subject4$is_rm_trial <- factor(data_by_subject4$is_rm_trial, 
                                           levels = c("RM trials","correct trials"))
my_plot4 <-  ggplot() +
  
  geom_point(data = data_across_subject4, aes(x = setsize,
                                              y = d_prime_mean,
                                              size = size_scale,
                                              group = size_scale,
                                              color = size_scale),
             
             position = position_dodge(0.5), stat = "identity", alpha = 0.6) +
  
  geom_point(data = data_by_subject4, aes(x = setsize,
                                          y = d_prime_mean,
                                          size = size_scale,
                                          color = size_scale),
             alpha = 0.05,
             position = position_dodge(0.5)) +
  
  
  geom_errorbar(data = data_across_subject4, aes(x = setsize,
                                                 y = d_prime_mean,
                                                 ymin = d_prime_mean - d_prime_SEM,
                                                 ymax = d_prime_mean + d_prime_SEM,
                                                 group = size_scale),
                color = "black",
                size  = 0.8,
                width = .00,
                alpha = 0.8,
                position = position_dodge(0.5)) +
  
  
  geom_hline(yintercept = 1, linetype = "dashed") +
  
  
  labs(y = "Sensitivity (d' +/- SEM)", x = "Set size") +
  
  
  scale_color_manual(labels = c("large", "middle", "small"),
                     values = c("#004488", "#BB5566", "#DDAA33"),
                     name = "face size") +
  
  
  scale_size_manual(labels = c("large", "middle", "small"),
                    values = c("large" = 6, "middle"= 4, "small" = 2),
                    name = "face size") +
  
  scale_y_continuous(limits = c(-0.5, 4)) +
  
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
  
  facet_wrap( ~ is_rm_trial,
                      nrow = 1,
                      labeller = labeller(is_rm_trial =
                                            c("correct trials" = "correct",
                                              "RM trials" = "RM")))


print(my_plot4)

ggsave(file = "test.svg", plot = my_plot4, width = 6, height = 3.36, units = "in")


my_plot4b <-  ggplot() +
  
  geom_point(data = data_across_subject4, aes(x = setsize,
                                              y = d_prime_mean,
                                              size = size_scale,
                                              group = size_scale,
                                              color = size_scale),
             
             position = position_dodge(0.5), stat = "identity", alpha = 0.6) +
  

  geom_errorbar(data = data_across_subject4, aes(x = setsize,
                                                 y = d_prime_mean,
                                                 ymin = d_prime_mean - d_prime_SEM,
                                                 ymax = d_prime_mean + d_prime_SEM,
                                                 group = size_scale),
                color = "black",
                size  = 0.8,
                width = .00,
                alpha = 0.8,
                position = position_dodge(0.5)) +
  
  
  geom_hline(yintercept = 1, linetype = "dashed") +
  
  
  labs(y = "Sensitivity (d' +/- SEM)", x = "Set size") +
  
  
  scale_color_manual(labels = c("large", "middle", "small"),
                     values = c("#004488", "#BB5566", "#DDAA33"),
                     name = "face size") +
  
  
  scale_size_manual(labels = c("large", "middle", "small"),
                    values = c("large" = 6, "middle"= 4, "small" = 2),
                    name = "face size") +
  
  scale_y_continuous(limits = c(0, 3)) +
  
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
  
  facet_wrap( ~ is_rm_trial,
              nrow = 1,
              labeller = labeller(is_rm_trial =
                                    c("correct trials" = "Correct trials",
                                      "RM trials" = "Redundancy masking trials")))


print(my_plot4b)

ggsave(file = "test.svg", plot = my_plot4b, width = 8, height = 3.36, units = "in")


# briefly check if d' was significant between rm and non-rm

data_by_subject4$setsize <- as.factor(data_by_subject4$setsize)
data_by_subject4$is_rm_trial <- as.factor(data_by_subject4$is_rm_trial)
data_by_subject4$participant <- as.factor(data_by_subject4$participant)
data_by_subject4$size_scale <- as.factor(data_by_subject4$size_scale)

str(data_by_subject4)

data_by_subject4 <- subset(data_by_subject4, size_scale == "small")

alignment_con.model_random_slope3 <-
  lmer(
    d_prime_mean ~ setsize + is_rm_trial + 
      (1 |participant),
    data = data_by_subject4,
    REML = FALSE
  )
alignment_con.model_random_slope3


coef(alignment_con.model_random_slope3)

alignment_con.null_random_slope3 <-
  lmer(
    d_prime_mean ~ setsize + 
      (1 | participant),
    data = data_by_subject4,
    REML = FALSE
  )
alignment_con.null_random_slope3


anova(alignment_con.model_random_slope3, 
      alignment_con.null_random_slope3)




# plot: criterion for each set size, separately for rm and non-rm trials--------

my_plot5 <-  ggplot() +
  
  geom_point(data = data_across_subject4, aes(x = setsize,
                                              y = c_mean,
                                              size = size_scale,
                                              group = size_scale,
                                              color = is_rm_trial),
             
             position = position_dodge(0.2), stat = "identity", alpha = 0.6, width = 0.2) +
  
  geom_point(data = data_by_subject4, aes(x = setsize,
                                          y = c_mean,
                                          size = size_scale,
                                          color = is_rm_trial),
             alpha = 0.05,
             position = position_dodge(0.2)) +
  
  
  geom_errorbar(data = data_across_subject4, aes(x = setsize,
                                                 y = c_mean,
                                                 ymin = c_mean - c_SEM,
                                                 ymax = c_mean + c_SEM,
                                                 group = size_scale,
                                                 color = is_rm_trial),
                
                size  = 0.8,
                width = .00,
                alpha = 0.8,
                position = position_dodge(0.2)) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  
  labs(y = "Bias (criterion)", x = "Set size") +
  
  
  scale_color_manual(labels = c("non RM", "RM"),
                     values = c("#004488", "#BB5566"),
                     name = "RM or not") +
  
  
  scale_size_manual(labels = c("large", "middle", "small"),
                    values = c("large" = 6, "middle"= 4, "small" = 2),
                    name = "face size") +
  
  scale_y_continuous(limits = c(-1.5, 1)) +
  
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


print(my_plot5)
ggsave(file = "test.svg", plot = my_plot5)

# bar plot deviation score-----------------------------------------

# all conditions, all stimuli

data_preprocessed$size_scale <- factor(data_preprocessed$size_scale,      
                                       levels = c("small", "middle", "large"))

my_plot6 <-
  
  ggplot(data = data_preprocessed, 
         aes(x = response_num)) +
  
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat = "count") +
  
  geom_text(aes(label = scales::percent(..prop..),
                y = ..prop.. ), stat = "count", vjust = -.5, size = 2.5) +
  
  labs(y = "Percent response", fill = "Response", x = "Response") +
  
  scale_y_continuous(labels = scales::percent) +
  
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9)) +
  
  
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
  
  
  facet_wrap( ~identity * size_scale * setsize,
              labeller = labeller(identity =
                                    c("NF" = "upright face",
                                      "NF_usd" = "upside-down face"),
                                  size_scale = 
                                    c("large" = "large",
                                      "middle" = "medium",
                                      "small" = "small "),
                                  setsize = 
                                    c("3" = "set size 3",
                                      "4" = "set size 4",
                                      "5" = "set size 5",
                                      "6" = "set size 6")))

my_plot6

ggsave(file = "try.svg", plot = my_plot6, width = 16.7, height = 16, units = "in")



# only face

data_faces <- data_preprocessed %>% 
  filter(setsize == 3)

data_faces$size_scale <- factor(data_faces$size_scale,      
                                levels = c("small", "middle", "large"))


my_plot6_b <-
  
  ggplot(data = data_faces, aes(x = response_num)) +
  
  geom_bar(aes(y = ..prop..), stat = "count", fill = "#EEEEEE") +
  
  geom_text(aes(label = scales::percent(..prop..),
                y = ..prop.. ), stat = "count", vjust = -.5, size = 2.5) +
  
  labs(y = "Percent Response", x = "Response") +
  
  scale_y_continuous(labels = scales::percent) +
  
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6)) +
  
  
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
  
  
  facet_wrap( ~ size_scale,
              labeller = labeller(size_scale = 
                                    c("large" = "large",
                                      "middle" = "medium",
                                      "small" = "small")))

my_plot6_b

ggsave(file = "5b.svg", plot = my_plot6_b, width = 11.3, height = 3.4, units = "in")





