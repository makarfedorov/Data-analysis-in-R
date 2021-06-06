diabet <- read_csv("diabetes2.csv")
summary(diabet)
diabet %>%
  filter(Insulin != 0) %>%
  filter(BloodPressure != 0) %>%
  filter(SkinThickness != 0) %>%
  filter(BMI != 0) %>%
  filter(Glucose != 0) -> diabet2
logit_model <- glm(data = diabet2, Outcome ~ Glucose + BloodPressure + Insulin + BMI + Age, family = "binomial")

summary(logit_model)

library(pROC)
library(pscl)
pR2(logit_model)
