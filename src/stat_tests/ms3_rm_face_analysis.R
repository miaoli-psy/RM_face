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

# set working path
setwd("D:/SCALab/projects/RM_face/data/")

# read data
data_preprocessed <- read_excel("exp1_preprocessed.xlsx")

# check age, sex
df_check_age <- data_preprocessed %>%
  group_by(participant, age, sex) %>%
  tally() 

mean(df_check_age$age)


# data by subject
data_by_subject <- data_preprocessed %>%
  group_by(stimulus_types,
           participant,
           setsize,
           size_scale) %>%
  summarise(
    deviation_score_mean = mean(deviation_score),
    deviation_score_std = sd(deviation_score),
    n = n()
  ) %>%
  mutate(
    deviation_socre_SEM = deviation_score_std / sqrt(n),
    deviation_socre_CI = deviation_socre_SEM * qt((1 - 0.05) / 2 + .5, n -
                                                    1)
  )

# as factors
str(data_by_subject)
data_by_subject$stimulus_types <- as.factor(data_by_subject$stimulus_types)
data_by_subject$setsize <- as.factor(data_by_subject$setsize)
data_by_subject$size_scale <- as.factor(data_by_subject$size_scale)
data_by_subject$participant <- as.factor(data_by_subject$participant)


# interaction effect

# full model with 3 way interaction
model_random_slope <-
  lmer(
    deviation_score_mean ~ setsize * size_scale * stimulus_types+
      (1 + stimulus_types | participant) + (1 + stimulus_types | participant),
    data = data_by_subject,
    REML = FALSE
  )
model_random_slope

# without 3 way interaction
model_reduced <-
  lmer(
    deviation_score_mean ~ setsize + size_scale + stimulus_types +
      setsize * size_scale + setsize * stimulus_types + size_scale * stimulus_types +
      (1 + stimulus_types | participant) + (1 + stimulus_types | participant),
    data = data_by_subject,
    REML = FALSE
  )

model_reduced


model_none <-
  lmer(
    deviation_score_mean ~ setsize + size_scale + stimulus_types +
      (1 + stimulus_types | participant) + (1 + stimulus_types | participant),
    data = data_by_subject,
    REML = FALSE
  )
model_none

anova(model_random_slope, model_reduced)

# visualize the interaction (if exists)
emmip(model_random_slope, size_scale ~ setsize|stimulus_types)

# test 2 way interaction

