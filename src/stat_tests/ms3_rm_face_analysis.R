# libraires ---------------------------------------------------------------
if(!require(readxl)){
  install.packages("readxl")
  library(readxl)
}

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

if(!require(lme4)){
  install.packages("lme4")
  library(lme4)
}


if(!require(sjPlot)){
  install.packages("sjPlot")
  library(sjPlot)
}

if(!require(glmmTMB)){
  install.packages("glmmTMB")
  library(glmmTMB)
}


if(!require(rstatix)){
  install.packages("rstatix")
  library(rstatix)
}

if(!require(emmeans)){
  install.packages("emmeans")
  library(emmeans)
}

if(!require(sjstats)){
  install.packages("sjstats")
  library(sjstats)
}


if(!require(lmerTest)){
  install.packages("lmerTest")
  library(lmerTest)
}

if(!require(MuMIn)){
  install.packages("MuMIn")
  library(MuMIn)
}

if(!require(multcomp)){
  install.packages("multcomp")
  library(multcomp)
}

if(!require(nlme)){
  install.packages("nlme")
  library(nlme)
}

if(!require(r2glmm)){
  install.packages("r2glmm")
  library(r2glmm)
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


if(!require(ggpubr)){
  install.packages("ggpubr")
  library(ggpubr)
}

# Exp1--------------------------------------------------------------------------

# set working path
# setwd("d:/OneDrive/projects/RM_face/src/")

# read data
# data_preprocessed <- read_excel("exp1_preprocessed.xlsx")
data_preprocessed <- read_excel(path = file.choose())

# check age, sex
df_check_age <- data_preprocessed %>%
  group_by(participant, age, sex) %>%
  tally() 

mean(df_check_age$age)

# ---------------------------Exp1 DV analysis -GLMM------------------------------

str(data_preprocessed)

data_preprocessed$stimulus_types <- as.factor(data_preprocessed$stimulus_types)
data_preprocessed$size_scale<- as.factor(data_preprocessed$size_scale)
data_preprocessed$setsize<- as.integer(data_preprocessed$setsize)
data_preprocessed$participant<- as.factor(data_preprocessed$participant)

# categorize Deviation score

data_preprocessed <- data_preprocessed %>% 
  mutate(DV_cate = case_when(
    deviation_score < 0 ~ 0,
    deviation_score >= 0 ~ 1)
  )

data_preprocessed$DV_cate<- as.factor(data_preprocessed$DV_cate)


# GLMM model

model.glmer <- glmer(DV_cate ~ stimulus_types + size_scale + setsize +(1|participant), data = data_preprocessed, family = binomial)
model1.1.glmer <- glmer(DV_cate ~ size_scale + setsize +(1|participant), data = data_preprocessed, family = binomial)

summary(model.glmer)
anova(model.glmer, model1.1.glmer)

# pairwise comparisons
emms <- emmeans(
  model.glmer,
  list(pairwise ~ stimulus_types),
  adjust = "tukey"
)

summary(emms, infer = TRUE)

# try glmmTMB package
model.glmer2 <- glmmTMB(
  DV_cate ~ stimulus_types + size_scale + setsize + (1 | participant),
  family = binomial, 
  data = data_preprocessed
)

summary(model.glmer2)

# an APA style table: https://cran.r-project.org/web/packages/sjPlot/vignettes/tab_mixed.html
# ????? table only works with model fitting by glmmTMB

tab_model(model.glmer2, p.val = "kr", show.df = TRUE, show.std = TRUE, show.se = TRUE, show.stat = TRUE)

# calculate expected probability
# way 1: general expected probability (ignoring the individual differences,i.e.random effects)
data_to_predict1 = data.frame(stimulus_types = "NF_scramble", size_scale = "small", setsize = 3)
data_to_predict2 = data.frame(stimulus_types = "NF", size_scale = "large", setsize = 6)
## you set change parameters based on what you want to do more prediction
predict(model.glmer, newdata = data_to_predict2, type = "response", re.form = NA)

# way 2: average individual's predicted probability
attach(data_preprocessed)
predict_conditions <- tidyr::crossing(stimulus_types, 
                                      size_scale, 
                                      setsize, 
                                      participant)
predict_n <- nrow(predict_conditions)

prediction_results <- data.frame()
for (i in 1:predict_n){
  predict_person <- predict(model.glmer, newdata = predict_conditions[i,], type = "response")
  prediction_results <- rbind(prediction_results, data.frame(predict_person))
}

person_prob_all <- cbind(predict_conditions, prediction_results)

# test
predict(model.glmer, newdata = predict_conditions[420,], type = "response")
person_prob_all[420,] # correct


# calculate probability < 0 

person_prob_all <- person_prob_all %>% 
  mutate(predict_person_lessthan0 = 1 - predict_person)


# calculate mean for certain conditions (you can change conditions by yourself)
colMeans(subset(person_prob_all, stimulus_types == "NF_scramble"& size_scale == "small"& setsize == 3, 
                select = (predict_person_lessthan0)))


# calculate mean/std/sem/ci for each condition
predic_res <- person_prob_all %>% 
  group_by(stimulus_types,
           setsize,
           size_scale) %>% 
  summarise(prob = mean(predict_person_lessthan0),
            prob_std = sd(predict_person_lessthan0),
            n = n()
            ) %>% 
  mutate(
  prob_SEM = prob_std / sqrt(n),
  prob_CI = prob_SEM * qt((1 - 0.05) / 2 + .5, n -
                                                1)
)

# visualization prediction res

my_plot_predic <-  ggplot() +
  
  geom_point(
    data = predic_res,
    aes(
      x = setsize,
      y = prob,
      size = size_scale,
      color = size_scale
    ),
    position = position_dodge(0.8),
    stat = "identity",
    alpha = 0.8
  ) +
  
  scale_size_manual(values = c(
    "small" = 2,
    "middle" = 4,
    "large" = 6
  )) +
  
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  
  geom_errorbar(
    data = predic_res,
    aes(
      x = setsize,
      y = prob,
      ymin = prob - prob_SEM,
      ymax = prob + prob_SEM,
      group = size_scale,
      color = size_scale
    ),
    
    size  = 0.8,
    width = .00,
    position = position_dodge(0.8)
  ) +
  
  labs(y = "Prediction", x = "Set Size") +
  
  scale_color_manual(
    labels = c("large", "middle", "small"),
    values = c("#004488", "#BB5566", "#DDAA33"),
    name = "stimuli size"
  ) +
  
  
  scale_size_manual(
    labels = c("large", "middle", "small"),
    values = c(
      "large" = 6,
      "middle" = 4,
      "small" = 2
    ),
    name = "stimuli size"
  ) +
  
  scale_x_continuous(
    breaks = c(3, 4, 5, 6),
    labels = c("3", "4", "5", "6"),
    expand = c(0.1, 0.5)
  ) +
  
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
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
    panel.spacing = unit(1.0, "lines")
  ) +
  
  
  facet_wrap(~ stimulus_types,
             labeller = labeller(
               stimulus_types =
                 c(
                   "NF" = "face",
                   "NF_scramble" = "scrambled face",
                   "outline" = "outline"
                 )
             ))

print(my_plot_predic)
ggsave(file = "test2.svg", plot = my_plot_predic, width = 7.42, height = 2.7, units = "in")


# Exp2-------------------------------------------------------
# read data
data_preprocessed2 <- read_excel(path = file.choose())

  # check age, sex
df_check_age2 <- data_preprocessed2 %>%
  group_by(participant, age, sex) %>%
  tally() 

mean(df_check_age2$age)


# ------------------Exp 2 number task ------------------------

str(data_preprocessed2)

colnames(data_preprocessed2)

data_preprocessed2$identity <- as.factor(data_preprocessed2$identity)
data_preprocessed2$size_scale<- as.factor(data_preprocessed2$size_scale)
data_preprocessed2$setsize<- as.integer(data_preprocessed2$setsize)
data_preprocessed2$participant<- as.factor(data_preprocessed2$participant)


# categorize Deviation score

data_preprocessed2 <- data_preprocessed2 %>% 
  mutate(DV_cate = case_when(
    deviation_score < 0 ~ 0,
    deviation_score >= 0 ~ 1)
  )

data_preprocessed2$DV_cate<- as.factor(data_preprocessed2$DV_cate)


# GLMM model

model2.glmer <- glmer(DV_cate ~ identity + size_scale + setsize +(1|participant), data = data_preprocessed2, family = binomial)
model2.1.glmer <- glmer(DV_cate ~ identity + setsize +(1|participant), data = data_preprocessed2, family = binomial)

summary(model2.glmer)
anova(model2.glmer, model2.1.glmer)

# pairwise comparisons
emms <- emmeans(
  model2.glmer,
  list(pairwise ~ size_scale),
  adjust = "tukey"
)



# calculate expected probability

attach(data_preprocessed2)
predict_conditions <- tidyr::crossing(identity, 
                                      size_scale, 
                                      setsize, 
                                      participant)
predict_n <- nrow(predict_conditions)

prediction_results <- data.frame()
for (i in 1:predict_n){
  predict_person <- predict(model2.glmer, newdata = predict_conditions[i,], type = "response")
  prediction_results <- rbind(prediction_results, data.frame(predict_person))
}

person_prob_all <- cbind(predict_conditions, prediction_results)

# test
predict(model2.glmer, newdata = predict_conditions[288,], type = "response")
person_prob_all[288,] # correct


# calculate probability < 0 

person_prob_all <- person_prob_all %>% 
  mutate(predict_person_lessthan0 = 1 - predict_person)


# calculate mean for certain conditions (you can change conditions by yourself)
colMeans(subset(person_prob_all, identity == "NF_usd"& size_scale == "small"& setsize == 3, 
                select = (predict_person_lessthan0)))


# calculate mean/std/sem/ci for each condition
predic_res <- person_prob_all %>% 
  group_by(identity,
           setsize,
           size_scale) %>% 
  summarise(prob = mean(predict_person_lessthan0),
            prob_std = sd(predict_person_lessthan0),
            n = n()
  ) %>% 
  mutate(
    prob_SEM = prob_std / sqrt(n),
    prob_CI = prob_SEM * qt((1 - 0.05) / 2 + .5, n -
                              1)
  )

my_plot_predic2 <-  ggplot() +
  
  geom_point(
    data = predic_res,
    aes(
      x = setsize,
      y = prob,
      size = size_scale,
      color = size_scale
    ),
    position = position_dodge(0.8),
    stat = "identity",
    alpha = 0.8
  ) +
  
  scale_size_manual(values = c(
    "small" = 2,
    "middle" = 4,
    "large" = 6
  )) +
  
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  
  geom_errorbar(
    data = predic_res,
    aes(
      x = setsize,
      y = prob,
      ymin = prob - prob_SEM,
      ymax = prob + prob_SEM,
      group = size_scale,
      color = size_scale
    ),
    
    size  = 0.8,
    width = .00,
    position = position_dodge(0.8)
  ) +
  
  labs(y = "Prediction", x = "Set Size") +
  
  scale_color_manual(
    labels = c("large", "middle", "small"),
    values = c("#004488", "#BB5566", "#DDAA33"),
    name = "stimuli size"
  ) +
  
  
  scale_size_manual(
    labels = c("large", "middle", "small"),
    values = c(
      "large" = 6,
      "middle" = 4,
      "small" = 2
    ),
    name = "stimuli size"
  ) +
  
  scale_x_continuous(
    breaks = c(3, 4, 5, 6),
    labels = c("3", "4", "5", "6"),
    expand = c(0.1, 0.5)
  ) +
  
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
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
    panel.spacing = unit(1.0, "lines")
  ) +
  
  
  facet_wrap(~ identity,
             labeller = labeller(
               identity =
                 c(
                   "NF" = "face",
                   "NF_usd" = "unside-down face"
                 )
             ))

print(my_plot_predic2)
ggsave(file = "test2.svg", plot = my_plot_predic2, width = 7.42, height = 2.7, units = "in")


# ------------------Exp 2 orientation task ------------------------

# get SDT data-----------------------------------------------------

data_SDT_by_subject <- data_preprocessed2 %>%
  group_by(setsize, participant, size_scale, is_rm_trial) %>%
  summarise(hit = sum(hit),
            miss = sum(miss),
            CR = sum(CR),
            FA = sum(FA))


data_SDT_across_subject <- data_preprocessed2 %>%
  group_by(setsize, size_scale, is_rm_trial) %>%
  summarise(hit = sum(hit),
            miss = sum(miss),
            CR = sum(CR),
            FA = sum(FA))

# write.csv(data_SDT_by_subject, "rm_face_to_cal_SDT.csv")


# read data exp2 SDT
data_cdt <- read_excel(path = file.choose()) #rm_face_SDT.xlsx
table(data_preprocessed2$is_rm_trial)


# remove overestimation trials?
# data_cdt <- subset(data_cdt, is_rm_trial == "RM trials" | is_rm_trial == "correct trials")

table(data_cdt$is_rm_trial)

data_cdt <- data_cdt %>% 
  mutate(is_rm_trial_new = case_when(
    is_rm_trial == "RM trials" ~ "RM",
    is_rm_trial == "correct trials" ~ "non-RM",
    is_rm_trial =="overestimation trials" ~ "non-RM"))

table(data_cdt$is_rm_trial_new)

str(data_cdt)
# subset data by selecting rows with RM and correct trials
# data_cdt.sub <- data_cdt[data_cdt$is_rm_trial%in% c("correct trials", 'RM trials'),]
# table(data_cdt.sub$is_rm_trial)


data_cdt$is_rm_trial<- as.factor(data_cdt$is_rm_trial_new)
data_cdt$setsize<- as.integer(data_cdt$setsize)
data_cdt$participant<- as.factor(data_cdt$participant)
data_cdt$size_scale<- as.factor(data_cdt$size_scale)


# create a single variable to indicate groups
data_cdt$group_index <- paste(data_cdt$size_scale, data_cdt$is_rm_trial_new, sep = ':')

# Fit the linear mixed model with the categorical variable
model <- lmer(d_prime ~ group_index + (1|participant), data = data_cdt)
summary(model)

# Compute estimated marginal means
means <- emmeans(model, ~ group_index)

# Perform pairwise comparisons with Tukey correction
comparison <- pairs(means, by = NULL, test = "t", adjust = "tukey") 
# by parameter can set reference group

# adjust can be set by other methods, like " bonferroni", "mvt", etc
pwpp(means, adjust = "tukey") # a nice plot to summarize P-value of group comparison


# pairwise comparisons
emms2 <- emmeans(
  model,
  list(pairwise ~ group_index),
  adjust = "tukey"
)

summary(emms2, infer = TRUE)
