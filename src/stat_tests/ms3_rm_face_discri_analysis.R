#install.packages("readxl")

library(tidyverse)

# set working path
# setwd("d:/OneDrive/projects/RM_face/src/")

# Exp1 discrimination-----------------------------------------------------------

data_discr <- readxl::read_excel(path = file.choose()) # "exp1_disc_preprocessed.xlsx"

# add col answer is corr or not
data_discr$corr <- ifelse(data_discr$deviation_score == 0, 1, 0)

data_discr_across_subject <- data_discr %>% 
  group_by(
    size_scale,
    stimulus_types
  ) %>% 
  summarise(
    corr_mean = mean(corr),
    corr_std = sd(corr),
    n = n()
  ) %>% 
  mutate(
    corr_SEM = corr_std / sqrt(n)
  )

# Exp1 discrimination-----------------------------------------------------------

data_discr2 <- readxl::read_excel(path = file.choose()) # "exp2_disc_preprocessed.xlsx"

data_discr2$corr <- ifelse(data_discr2$deviation_score == 0, 1, 0)

data_discr_across_subject2 <- data_discr2 %>% 
  group_by(
    size_scale,
    identity
  ) %>% 
  summarise(
    corr_mean = mean(corr),
    corr_std = sd(corr),
    n = n()
  ) %>% 
  mutate(
    corr_SEM = corr_std / sqrt(n)
  )
