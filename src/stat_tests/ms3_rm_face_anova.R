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
 summarize(
    deviation_score_mean = mean(deviation_score),
    deviation_score_std = sd(deviation_score),
    n = n()
  ) %>%
mutate(
    deviation_socre_SEM = deviation_score_std / sqrt(n),
    deviation_socre_CI = deviation_socre_SEM * qt((1 - 0.05) / 2 + .5, n - 1),
    cv = deviation_score_std / setsize
  )

# summary statistics---------------------------------------

summary_stat <- data_by_subject %>%
  group_by(stimulus_types, setsize, size_scale) %>%
  get_summary_stats(cv, type = "mean_sd")

summary_stat

# check box plot--------------------------------------------

bxp <- ggboxplot(
  data_by_subject, x = "setsize", y = "deviation_score_mean", 
  color = "size_scale", palette = "stimulus_types", facet.by = "stimulus_types"
)
bxp

# normality assumption--------------------------------------

shapiro_test <- data_by_subject %>%
  group_by(setsize, size_scale, stimulus_types) %>%
  shapiro_test(deviation_score_mean)

shapiro_test


ggqqplot(data_by_subject, "cv", ggtheme = theme_bw()) +
  facet_grid(setsize + size_scale ~ stimulus_types, labeller = "label_both")

# 3 way repeat measure ANOVA --------------------------------

data_by_subject <- ungroup(data_by_subject)

res.aov <- anova_test(
  data = data_by_subject,
  dv = cv,
  wid = participant,
  within = c(setsize, size_scale, stimulus_types)
)
get_anova_table(res.aov)

# no 3 way interaction
# 看刺激类型的主效应
summary_stat2 <- data_preprocessed %>%
  group_by(stimulus_types) %>%
  get_summary_stats(deviation_score, type = "mean_sd")

pwc_setsize1 <- data_by_subject %>%
  pairwise_t_test(deviation_score_mean ~ stimulus_types, paired = TRUE, p.adjust.method = "holm")


# simple main effects
pwc_setsize <- data_by_subject %>%
  group_by(size_scale) %>%
  pairwise_t_test(deviation_score_mean ~ setsize, paired = TRUE, p.adjust.method = "holm")


# Show comparison results for "scale:small, stim type:NF" groups
pwc_setsize %>% filter(size_scale == "small", setsize == 3)
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

