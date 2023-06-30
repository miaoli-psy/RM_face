# libraries --------------------------------------------------------------------
library(lme4)
library(mixedpower)

# Exp1 read data----------------------------------------------------------------

data_exp1 <- readxl::read_excel(path = file.choose())

str(data_exp1)

colnames(data_exp1)

data_exp1$stimulus_types <- as.factor(data_exp1$stimulus_types)
data_exp1$size_scale<- as.factor(data_exp1$size_scale)
data_exp1$setsize<- as.integer(data_exp1$setsize)

# model
model <-
  lmer(deviation_score ~ stimulus_types + setsize + size_scale + (1 |
                                                                    participant),
       data = data_exp1)

# estimate power
power <- mixedpower(model = model, data = data_exp1,
                    fixed_effects = c("stimulus_types", "size_scale", "setsize"),
                    simvar = "participant", steps = c(20, 25, 30, 40, 50),
                    critical_value = 2, n_sim = 1000)

# Exp2 read data----------------------------------------------------------------

data_exp2 <- readxl::read_excel(path = file.choose())

str(data_exp2)

colnames(data_exp2)

data_exp2$identity <- as.factor(data_exp2$identity)
data_exp2$size_scale<- as.factor(data_exp2$size_scale)
data_exp2$setsize<- as.integer(data_exp2$setsize)

# model

model.2 <-
  lmer(deviation_score ~ size_scale + setsize + (1 |
                                                   participant),
       data = data_exp2)


power2 <- mixedpower(model = model.2, data = data_exp2,
                    fixed_effects = c("size_scale", "setsize"),
                    simvar = "participant", steps = c(10, 20, 23, 28, 30, 40),
                    critical_value = 2, n_sim = 1000)