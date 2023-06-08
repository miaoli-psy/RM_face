## miao: project_20230422
## multigroup comparison under mixed model settings
library(Matrix)
library(lme4)
library(emmeans)

mydata <- readxl::read_xlsx(path =file.choose())
str(mydata)
# subset data by selecting rows with RM and correct trials
mydata.sub <- mydata[mydata$is_rm_trial%in% c("correct trials", 'RM trials'),]
table(mydata.sub$is_rm_trial)
# create a single variable to indicate groups
mydata.sub$group_index <- paste(mydata.sub$size_scale, mydata.sub$is_rm_trial, sep = ':')
# Fit the linear mixed model with the categorical variable
model <- lmer(d_prime ~ group_index + (1|participant), data = mydata.sub, RM)
summary(model)
# Compute estimated marginal means
means2 <- emmeans(model, ~ group_index)
# Perform pairwise comparisons with Tukey correction
comparison <- pairs(means, by = NULL, test = "t", adjust = "tukey") 
# by parameter can set reference group
# adjust can be set by other methods, like " bonferroni", "mvt", etc
pwpp(means2, adjust = "tukey") # a nice plot to summarize P-value of group comparison
