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


# Exp1 discrimination-----------------------------------------

# set working path
setwd("D:/SCALab/projects/RM_face/data/")

# read data
data_preprocessed <- read_excel("exp1_disc_preprocessed.xlsx")

data_preprocessed <- data_preprocessed %>% 
  mutate(corr = if_else(deviation_score == 0, 1, 0))

data_by_subject <- data_preprocessed %>%
  group_by(stimulus_types,
           participant,
           size_scale) %>%
  summarise(
    propotion_correct = mean(corr),
    propotion_correct_std = sd(corr),
    n = n()
  )

# check box plot--------------------------------------------

bxp <- ggboxplot(
  data_by_subject, x = "stimulus_types", y = "propotion_correct", 
  color = "size_scale"
)
bxp


summary_stat <- data_by_subject %>%
  group_by(stimulus_types, size_scale) %>%
  get_summary_stats(propotion_correct, type = "mean_sd")

summary_stat


# Exp2 discrimination ----------------------------------------
# read data
data_preprocessed2 <- read_excel("exp2_disc_preprocessed.xlsx")

data_preprocessed2 <- data_preprocessed2 %>% 
  mutate(corr = if_else(deviation_score == 0, 1, 0))

data_by_subject2 <- data_preprocessed2 %>%
  group_by(identity,
           participant,
           size_scale) %>%
  summarise(
    propotion_correct = mean(corr),
    propotion_correct_std = sd(corr),
    n = n()
  )

# summary statistics---------------------------------------

summary_stat2 <- data_by_subject2 %>%
  group_by(identity, size_scale) %>%
  get_summary_stats(propotion_correct, type = "mean_sd")

summary_stat2

# check box plot--------------------------------------------

bxp2 <- ggboxplot(
  data_by_subject2, x = "identity", y = "propotion_correct", 
  color = "size_scale"
)
bxp2


summary_stat2 <- data_by_subject2 %>%
  group_by(identity, size_scale) %>%
  get_summary_stats(propotion_correct, type = "mean_sd")

summary_stat2
