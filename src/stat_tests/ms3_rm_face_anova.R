# libraires ---------------------------------------------------------------
library(readxl)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(emmeans)
library(sjstats)
library(lme4)
library(lmerTest)
library(MuMIn)
library(multcomp)
library(nlme)
library(r2glmm)
library(ggplot2)
library(ggthemes)
library(svglite)
library(sjPlot)
library(ggpubr)
# Exp1-------------------------------------------------------

# set working path
setwd("D:/SCALab/projects/RM_face/data/")

# read data
data_preprocessed <- read_excel("exp1_preprocessed.xlsx")
data_wide <- read_excel("exp1_data_wide.xlsx")

# check age, sex
df_check_age <- data_preprocessed %>%
  group_by(participant, age, sex) %>%
  tally() 

mean(df_check_age$age)

data_by_subject <- data_preprocessed %>%
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

# write.csv(data_by_subject, "data_by_subject_long.csv")
# summary statistics---------------------------------------

summary_stat <- data_by_subject %>%
  group_by(stimulus_types, setsize, size_scale) %>%
  get_summary_stats(cv, type = "mean_sd")

summary_stat

# check box plot--------------------------------------------

# data <- subset(data_by_subject, stimulus_types == "NF_scramble")

bxp <- ggboxplot(
  data_by_subject, x = "setsize", y = "rt_mean", 
  color = "size_scale", facet.by = "stimulus_types"
)
bxp

# normality assumption--------------------------------------

shapiro_test <- data_by_subject %>%
  group_by(setsize, size_scale, stimulus_types) %>%
  shapiro_test(cv)

shapiro_test


ggqqplot(data_by_subject, "cv", ggtheme = theme_bw()) +
  facet_grid(setsize + size_scale ~ stimulus_types, labeller = "label_both")

# 3 way repeat measure ANOVA --------------------------------

data_by_subject <- ungroup(data_by_subject)

str(data_by_subject)
data_by_subject$stimulus_types <- as.factor(data_by_subject$stimulus_types)
data_by_subject$setsize <- as.factor(data_by_subject$setsize)
data_by_subject$size_scale <- as.factor(data_by_subject$size_scale)


res.aov <- anova_test(
  data = data_by_subject,
  dv = deviation_score_mean,
  wid = participant,
  within = c(setsize, size_scale, stimulus_types),
  type = 3,
  effect.size = "pes"
)
get_anova_table(res.aov, correction =  "auto") # "auto"


# no 3 way interaction
# 看刺激类型的主效应
summary_stat2 <- data_by_subject %>%
  group_by(stimulus_types) %>%
  get_summary_stats(deviation_score_mean, type = "mean_sd")

summary_stat2

# pairwise comparison

pwc_stimtype1 <- data_by_subject %>%
  emmeans_test(cv ~ stimulus_types,  p.adjust.method = "holm")

pwc_stimtype1
get_emmeans(pwc_stimtype1)


pwc_setsize1 <- data_by_subject %>%
  emmeans_test(cv ~ setsize,  p.adjust.method = "holm")

pwc_setsize1
get_emmeans(pwc_setsize1)


pwc_setsize1 <- data_by_subject %>%
  group_by(size_scale) %>%
  pairwise_t_test(deviation_score_mean ~ setsize, paired = TRUE, p.adjust.method = "holm")

pwc_setsize2 <- data_by_subject %>%
  group_by(setsize) %>%
  pairwise_t_test(deviation_score_mean ~ size_scale, paired = TRUE, p.adjust.method = "holm")


pwc_setsize1 %>% filter(size_scale == "small")
  select(-p)     # remove p columns


# Visualization: box plots with p-values
pwc <- pwc_setsize %>% add_xy_position(x = "size_scale")

# pwc.filtered <- pwc %>%
#   filter(stimulus_types == "outline", size_scale == "middle")
bxp + 
  stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

# Exp2 number task------------------------------------------------
# read data
data_preprocessed2 <- read_excel("exp2_preprocessed.xlsx")

# check rm, correct, overestimat trials

df_check_rm <- data_preprocessed2 %>% 
  group_by(setsize, size_scale, identity, is_rm_trial) %>% 
  tally() 

df_check_rm <- data_preprocessed2 %>% 
  group_by(is_rm_trial) %>% 
  tally() 

# check age, sex
df_check_age <- data_preprocessed2 %>%
  group_by(participant, age, sex) %>%
  tally() 

mean(df_check_age$age)

data_by_subject2 <- data_preprocessed2 %>%
  group_by(identity,
           participant,
           setsize,
           size_scale) %>%
  summarise(
    deviation_score_mean = mean(deviation_score),
    deviation_score_std = sd(deviation_score),
    n = n(),
    rt_num_mean = mean(num_task_rt),
    rt_num_std = sd(num_task_rt),
    rt_ori_mean = mean(ori_task_rt),
    rt_ori_std = sd(ori_task_rt)
  ) %>%
  mutate(
    deviation_socre_SEM = deviation_score_std / sqrt(n),
    deviation_socre_CI = deviation_socre_SEM * qt((1 - 0.05) / 2 + .5, n - 1),
    rt_num_SEM = rt_num_std / sqrt(n),
    rt_ori_SEM = rt_ori_std / sqrt(n),
    cv = deviation_score_std / setsize,
    cv_SEM = cv / sqrt(n),
    rt_SEM = rt_num_std / sqrt(n)
  )

# check box plot--------------------------------------------

bxp <- ggboxplot(
  data_by_subject2, x = "setsize", y = "rt_ori_mean", 
  color = "size_scale", palette = "identity", facet.by = "identity"
)
bxp

# normality assumption--------------------------------------
shapiro_test <- data_by_subject2 %>%
  group_by(setsize, size_scale, identity) %>%
  shapiro_test(cv)

shapiro_test


ggqqplot(data_by_subject2, "deviation_score_mean", ggtheme = theme_bw()) +
  facet_grid(setsize + size_scale ~ identity, labeller = "label_both")


# 3 way repeat measure ANOVA --------------------------------
data_by_subject2 <- ungroup(data_by_subject2)

str(data_by_subject2)
data_by_subject2$identity <- as.factor(data_by_subject2$identity)
data_by_subject2$setsize <- as.factor(data_by_subject2$setsize)
data_by_subject2$size_scale <- as.factor(data_by_subject2$size_scale)


res.aov <- anova_test(
  data = data_by_subject2,
  dv = rt_ori_mean,
  wid = participant,
  within = c(setsize, size_scale, identity),
  type = 3,
  effect.size = "pes"
)
get_anova_table(res.aov, correction =  "auto") # "auto"


# no 3 way interaction

summary_stat2 <- data_by_subject2 %>%
  group_by(size_scale) %>%
  get_summary_stats(cv, type = "mean_sd")

summary_stat2


pwc_stimtype1 <- data_by_subject2 %>%
  emmeans_test(deviation_score_mean ~ identity,  p.adjust.method = "holm")

pwc_stimtype1

get_emmeans(pwc_stimtype1)


pwc_setsize1 <- data_by_subject2 %>%
  emmeans_test(deviation_score_mean ~ setsize,  p.adjust.method = "holm")

pwc_setsize1
get_emmeans(pwc_setsize1)


pwc_size_scale1 <- data_by_subject2 %>%
  emmeans_test(deviation_score_mean ~ size_scale,  p.adjust.method = "holm")

pwc_size_scale1
get_emmeans(pwc_size_scale1)


pwc_setsize1 <- data_by_subject2 %>%
  group_by(size_scale) %>%
  pairwise_t_test(deviation_score_mean ~ setsize, paired = TRUE, p.adjust.method = "holm")

pwc_size_scale2 <- data_by_subject2 %>%
  group_by(setsize) %>%
  pairwise_t_test(deviation_score_mean ~ size_scale, paired = TRUE, p.adjust.method = "holm")


pwc_setsize1 %>% filter(size_scale == "small")
select(-p)

# Exp2 orientation task------------------------------------------------
# read data
data_preprocessed2_2 <- read_excel("rm_face_SDT.xlsx")

# remove overestimation trials
data_preprocessed2_2 <- subset(data_preprocessed2_2, is_rm_trial == "RM trials" | is_rm_trial == "correct trials")

# data across subject
data_by_subject3 <- data_preprocessed2_2 %>%
  group_by(setsize, size_scale, is_rm_trial, participant) %>%
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


# check box plot
bxp <- ggboxplot(
  data_by_subject3, x = "setsize", y = "c_mean", 
  color = "size_scale", palette = "is_rm_trial", facet.by = "is_rm_trial"
)
bxp

# data
data_by_subject3 <- ungroup(data_by_subject3)

str(data_by_subject3)
data_by_subject3$is_rm_trial <- as.factor(data_by_subject3$is_rm_trial)
data_by_subject3$setsize <- as.factor(data_by_subject3$setsize)
data_by_subject3$size_scale <- as.factor(data_by_subject3$size_scale)


# normality assumption--------------------------------------
shapiro_test <- data_by_subject3 %>%
  group_by(setsize, size_scale, is_rm_trial) %>%
  shapiro_test(d_prime_mean)

shapiro_test


ggqqplot(data_by_subject3, "d_prime_mean", ggtheme = theme_bw()) +
  facet_grid(setsize + size_scale ~ is_rm_trial, labeller = "label_both")


res.aov <- anova_test(
  data = data_by_subject3,
  dv = c_mean,
  wid = participant,
  within = c(setsize, size_scale, is_rm_trial),
  type = 3,
  effect.size = "pes"
)
get_anova_table(res.aov, correction =  "auto") # "auto"


summary_stat3 <- data_by_subject3 %>%
  group_by(is_rm_trial, size_scale) %>%
  get_summary_stats(d_prime_mean, type = "mean_sd")

summary_stat3


pwc_setsize <- data_by_subject3 %>%
  group_by(is_rm_trial) %>%
  pairwise_t_test(d_prime_mean ~ size_scale, paired = FALSE, p.adjust.method = "holm")

pwc_setsize

pwc_trial_type <- data_by_subject3 %>%
  group_by(size_scale) %>%
  pairwise_t_test(d_prime_mean ~ is_rm_trial, paired = FALSE, p.adjust.method = "holm")

pwc_trial_type


