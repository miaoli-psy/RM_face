# libraires ---------------------------------------------------------------
if(!require(readxl)){
  install.packages("readxl")
  library(readxl)
}

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
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


if(!require(lme4)){
  install.packages("lme4")
  library(lme4)
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

if(!require(sjPlot)){
  install.packages("sjPlot")
  library(sjPlot)
}

if(!require(ggpubr)){
  install.packages("ggpubr")
  library(ggpubr)
}

# Exp1--------------------------------------------------------------------------

# set working path
# setwd("c:/SCALab/projects/RM_face/data/")

# read data
data_preprocessed <- read_excel(path = file.choose())

# -----------------------------GLMM---------------------------------------------

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


# model

model.glmer <- glmer(DV_cate ~ stimulus_types + size_scale + setsize +(1|participant), data = data_preprocessed, family = binomial)

summary(model.glmer)

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

# calculate mean for certain conditions (you can chage conditions by yourself)
colMeans(subset(person_prob_all, stimulus_types == "NF_scramble"& size_scale == "small"& setsize == 3, 
            select = (predict_person)))



