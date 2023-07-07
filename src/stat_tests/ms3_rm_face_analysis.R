# libraires ---------------------------------------------------------------
#install.packages("readxl","tidyverse","lme4", "sjPlot", "glmmTMB", "rstatix", "emmeans", "sjstats")
#install.packages("lmerTest", "MuMIn", "multcomp", "nlme", "r2glmm","ggplot2", "ggthemes", "svglite","ggpubr")
# install.packages('MCMCglmm')

# how to library multiple packages at the same time.
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(svglite)
library(ggpubr)

# Exp1--------------------------------------------------------------------------

# set working path
# setwd("d:/OneDrive/projects/RM_face/src/")

# read data
# data_preprocessed <- read_excel("exp1_preprocessed.xlsx")
data_preprocessed <- readxl::read_excel(path = file.choose())

# check age, sex
df_check_age <- data_preprocessed %>%
  group_by(participant, age, sex) %>%
  tally() 

mean(df_check_age$age)

# 95% CI

res_DV <- data_preprocessed %>% 
  group_by(
    stimulus_types,
    setsize,
    size_scale
  ) %>% 
  summarise(
    DV_mean = mean(deviation_score),
    DV_std = sd(deviation_score),
    n = n()
  ) %>% 
  mutate(
    DV_SEM = DV_std / sqrt(n),
    DV_CI = DV_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )

res_DV$CI_95_low <- res_DV$DV_mean - res_DV$DV_CI
res_DV$CI_95_up <- res_DV$DV_mean + res_DV$DV_CI

# mean DV, combined sizes and setsize

mean_DV <- data_preprocessed %>% 
  group_by(
    stimulus_types,
  ) %>% 
  summarise(
    DV_mean = mean(deviation_score),
    DV_std = sd(deviation_score),
    n = n()
  ) %>% 
  mutate(
    DV_SEM = DV_std / sqrt(n),
    DV_CI = DV_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )

# ---------------------------Exp1 DV analysis -LMM------------------------------

str(data_preprocessed)

data_preprocessed$stimulus_types <- as.factor(data_preprocessed$stimulus_types)
data_preprocessed$size_scale<- as.factor(data_preprocessed$size_scale)
data_preprocessed$setsize<- as.integer(data_preprocessed$setsize)
data_preprocessed$participant<- as.factor(data_preprocessed$participant)

# categorize Deviation score
# data_preprocessed$DV_cate <- ifelse(data_preprocessed$deviation_score<0, 1, 0) 
# data_preprocessed$DV_cate <- as.factor(data_preprocessed$DV_cate)

# # GLMM model
# 
# e1.model.glmm.1 <-
#   lme4::glmer(
#     DV_cate ~ stimulus_types + size_scale + setsize + (1 | participant),
#     data = data_preprocessed,
#     family = binomial
#   )

# e1.model.glmm.inter1 <-
#   lme4::glmer(
#     DV_cate ~ stimulus_types + size_scale + setsize + stimulus_types:setsize +
#       (1 | participant),
#     data = data_preprocessed,
#     family = binomial
#   )
# anova(e1.model.glmm.1, e1.model.glmm.inter1)
# 
# e1.model.glmm.2 <- lme4::glmer(DV_cate ~ size_scale + setsize +(1|participant), 
#                                data = data_preprocessed, family = binomial)
# summary(e1.model.glmm.1)
# # model comparison: find whether certian predictors are significant
# anova(e1.model.glmm.1, e1.model.glmm.2)

# possible problem: interaction effects from descriptive analysis (figure 2 in the paper)
# solution: model comparison between the simple model and complex model (with interaction terms)


# continuous outcome: try linear mixed model
e1.lmm1 <- lme4::lmer(deviation_score ~ stimulus_types + size_scale + setsize +(1|participant), 
                      data = data_preprocessed)
e1.lmm2 <- lme4::lmer(deviation_score ~ stimulus_types + setsize +(1|participant), 
                      data = data_preprocessed)
e1.lmm3 <- lme4::lmer(deviation_score ~ setsize + size_scale +(1|participant), 
                      data = data_preprocessed)

summary(e1.lmm1)

# check whether stimulus_types/size_scale is significant globally
anova(e1.lmm1, e1.lmm3) 

# how to create a table: https://cran.r-project.org/web/packages/sjPlot/vignettes/tab_mixed.html
sjPlot::tab_model(
  e1.lmm1,
  p.style = 'scientific_stars',
  show.se = T,
  show.stat = T,
  digits = 3
) #change scientific p-value to numeric manually


# pairwise comparisons
emms <- emmeans::emmeans(e1.lmm1,
                         list(pairwise ~ stimulus_types),
                         adjust = "tukey")

summary(emms, infer = TRUE)


# calculate expected mean

# way 1: general expected mean (ignoring the individual differences,i.e.random effects)
predict_conditions <-
  tidyr::crossing(
    data_preprocessed$stimulus_types,
    data_preprocessed$size_scale,
    data_preprocessed$setsize
  )
names(predict_conditions) <- c('stimulus_types', 'size_scale', 'setsize')

prediction_results <- data.frame()
for (i in 1:nrow(predict_conditions)) {
  prediction <-
    predict(
      e1.lmm1,
      newdata = predict_conditions[i, ],
      type = "response",
      re.form = NA
    )
  prediction_results <-
    rbind(prediction_results, data.frame(prediction))
}

prob_condi_combi <- cbind(predict_conditions, prediction_results) 

# way 2: calculated individual expected mean
predict_person <- tidyr::crossing(data_preprocessed$stimulus_types, data_preprocessed$size_scale, 
                                  data_preprocessed$setsize, data_preprocessed$participant)
names(predict_person) <- c('stimulus_types','size_scale','setsize','participant')

predict_results_per <- data.frame()
for (i in 1:nrow(predict_person)){
  prediction2<- predict(e1.lmm1, newdata = predict_person[i,])
  predict_results_per <- rbind(predict_results_per, data.frame(prediction2))
}

prob_person <- cbind(predict_person, predict_results_per)

# test
predict(e1.lmm1, newdata = predict_person[420,])
prob_person[420,] # correct

# the results of way 2 are close to way 1



# calculate mean/std/sem/ci for each condition
predic_res <- prob_person %>% 
  group_by(stimulus_types,
           size_scale,
           setsize) %>% 
  summarise(prediction_res = mean(prediction2),
            prediction_res_std = sd(prediction2),
            n = n()) %>% 
  mutate(prediction_res_SEM = prediction_res_std / sqrt(n),
         prediction_res_CI = prediction_res_SEM * qt((1 - 0.05) / 2 + .5, n -1))



# visualization prediction res

my_plot_predic <-  ggplot() +
  
  geom_point(
    data = predic_res,
    aes(
      x = setsize,
      y = prediction_res,
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
      y = prediction_res,
      ymin = prediction_res - prediction_res_CI,
      ymax = prediction_res + prediction_res_CI,
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
data_preprocessed2 <- readxl::read_excel(path = file.choose())

  # check age, sex
df_check_age2 <- data_preprocessed2 %>%
  group_by(participant, age, sex) %>%
  tally() 

mean(df_check_age2$age)


# 95% CI

res_DV2 <- data_preprocessed2 %>% 
  group_by(
    setsize,
    size_scale
  ) %>% 
  summarise(
    DV_mean = mean(deviation_score),
    DV_std = sd(deviation_score),
    n = n()
  ) %>% 
  mutate(
    DV_SEM = DV_std / sqrt(n),
    DV_CI = DV_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )

res_DV2$CI_95_low <- res_DV2$DV_mean - res_DV2$DV_CI
res_DV2$CI_95_up <- res_DV2$DV_mean + res_DV2$DV_CI



# ------------------Exp 2 number task ------------------------

str(data_preprocessed2)

colnames(data_preprocessed2)

data_preprocessed2$identity <- as.factor(data_preprocessed2$identity)
data_preprocessed2$size_scale<- as.factor(data_preprocessed2$size_scale)
data_preprocessed2$setsize<- as.integer(data_preprocessed2$setsize)
data_preprocessed2$participant<- as.factor(data_preprocessed2$participant)


# categorize Deviation score

# data_preprocessed2 <- data_preprocessed2 %>% 
#   mutate(DV_cate = case_when(
#     deviation_score < 0 ~ 0,
#     deviation_score >= 0 ~ 1)
#   )
# 
# data_preprocessed2$DV_cate<- as.factor(data_preprocessed2$DV_cate)
# 
# 
# # GLMM model
# 
# model2.glmer <- glmer(DV_cate ~ identity + size_scale + setsize +(1|participant), data = data_preprocessed2, family = binomial)
# model2.1.glmer <- glmer(DV_cate ~ identity + setsize +(1|participant), data = data_preprocessed2, family = binomial)
# 
# summary(model2.glmer)
# anova(model2.glmer, model2.1.glmer)
# 
# # pairwise comparisons
# emms <- emmeans(
#   model2.glmer,
#   list(pairwise ~ size_scale),
#   adjust = "tukey"
# )


e2.lmm1 <- lme4::lmer(deviation_score ~ identity + size_scale + setsize +(1|participant), 
                      data = data_preprocessed2)

e2.lmm2 <- lme4::lmer(deviation_score ~ size_scale + setsize +(1|participant), 
                      data = data_preprocessed2) #this one is a better model, without identity (face ori)

anova(e2.lmm1, e2.lmm2)

e3.lmm3 <- lme4::lmer(deviation_score ~ setsize +(1|participant), 
                      data = data_preprocessed2)

anova(e2.lmm2, e3.lmm3)

summary(e2.lmm2)

# pairwise comparisons
emms <- emmeans::emmeans(
  e2.lmm2,
  list(pairwise ~ size_scale),
  adjust = "tukey"
)

summary(emms)

# APA table
sjPlot::tab_model(
  e2.lmm2,
  p.style = 'scientific_stars',
  show.se = T,
  show.stat = T,
  digits = 3
) #change scientific p-value to numeric manually


# prediction DV exp2

predict_person2 <- tidyr::crossing(
  data_preprocessed2$size_scale,
  data_preprocessed2$setsize,
  data_preprocessed2$participant
)
names(predict_person2) <- c('size_scale', 'setsize', 'participant')

predict_results_per2 <- data.frame()
for (i in 1:nrow(predict_person2)) {
  prediction2 <- predict(e2.lmm2, newdata = predict_person2[i, ])
  predict_results_per2 <-
    rbind(predict_results_per2, data.frame(prediction2))
}

prob_person2 <- cbind(predict_person2, predict_results_per2)


predic_res2 <- prob_person2 %>% 
  group_by(size_scale,
           setsize) %>% 
  summarise(prob = mean(prediction2),
            prob_std = sd(prediction2),
            n = n()) %>% 
  mutate(prob_SEM = prob_std / sqrt(n),
         prob_CI = prob_SEM * qt((1 - 0.05) / 2 + .5, n -1))

# visulization prediction results
my_plot_predic2 <-  ggplot() +
  
  geom_point(
    data = predic_res2,
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
    data = predic_res2,
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
    panel.spacing = unit(1.0, "lines"))


print(my_plot_predic2)

# ggsave(file = "test2.svg", plot = my_plot_predic2, width = 7.42, height = 2.7, units = "in")


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


# read data exp2 SDT---------------------------------------------------------
data_sdt <- readxl::read_excel(path = file.choose()) #rm_face_SDT.xlsx
table(data_preprocessed2$is_rm_trial)


# remove overestimation trials?
# data_sdt <- subset(data_sdt, is_rm_trial == "RM trials" | is_rm_trial == "correct trials")

table(data_sdt$is_rm_trial)

data_sdt <- data_sdt %>% 
  mutate(is_rm_trial_new = case_when(
    is_rm_trial == "RM trials" ~ "RM",
    is_rm_trial == "correct trials" ~ "non-RM",
    is_rm_trial =="overestimation trials" ~ "non-RM"))

table(data_sdt$is_rm_trial_new)

str(data_sdt)
# subset data by selecting rows with RM and correct trials
# data_sdt.sub <- data_sdt[data_sdt$is_rm_trial%in% c("correct trials", 'RM trials'),]
# table(data_sdt.sub$is_rm_trial)


data_sdt$is_rm_trial<- as.factor(data_sdt$is_rm_trial_new)
data_sdt$setsize<- as.integer(data_sdt$setsize)
data_sdt$participant<- as.factor(data_sdt$participant)
data_sdt$size_scale<- as.factor(data_sdt$size_scale)

data_sdt <- as.data.frame(data_sdt)


# create a single variable to indicate groups
# data_sdt$group_index <- paste(data_sdt$size_scale, data_sdt$is_rm_trial_new, sep = ':')

# 
# # Fit the linear mixed model with the categorical variable
# model <- lmer(d_prime ~ group_index + (1|participant), data = data_sdt)
# summary(model)
# 
# # Compute estimated marginal means
# means <- emmeans(model, ~ group_index)
# 
# # Perform pairwise comparisons with Tukey correction
# comparison <- pairs(means, by = NULL, test = "t", adjust = "tukey") 
# # by parameter can set reference group
# 
# # adjust can be set by other methods, like " bonferroni", "mvt", etc
# pwpp(means, adjust = "tukey") # a nice plot to summarize P-value of group comparison
# 
# 
# # pairwise comparisons
# emms2 <- emmeans(
#   model,
#   list(pairwise ~ group_index),
#   adjust = "tukey"
# )   
# 
# summary(emms2, infer = TRUE)


# exp2: task2: H0: no difference between rm and non-rm
# Bayesian linear mixed model


e2.t2.mcmc2 <- MCMCglmm::MCMCglmm(d_prime ~ setsize + size_scale + is_rm_trial_new, 
                                  random =~participant, family = 'gaussian',
                                  data = data_sdt)
# check diagnosis plots
plot(e2.t2.mcmc2)
# check auto-correlation for random effects
coda::autocorr(e2.t2.mcmc2$VCV)
# check auto-correlation for fixed effects
coda::autocorr(e2.t2.mcmc2$Sol)

summary(e2.t2.mcmc2)

