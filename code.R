library(caret)
library(VGAM)
library(nnet)
library(car)
library(nnet)
library(lmtest)


# Questions
  # 1. check that latex is ok
  # secondly executive vs technical i thought assumtions would make more sense in technical
  # likilihood vs null model [1 vs 2]or likilihood of one model vs another
  # how to interpret models
  # 2. I'm not sure I understand the effects plot fully
# 2. Is there anything else I'm missing/expected to do which I haven't done yet?
# ----------------------------------------------------------------------------
data <-read.csv('admissions.csv')
colnames(data) <- c('provnum', 'Death', 'White', 'Length_Of_Stay', 'age80', 'age', 'admission')


str(data)
table(as.vector(data$admission))
table(as.vector(data$Death))
table(as.vector(data$White))
table(as.vector(data$age))
table(as.vector(data$age80))
boxplot(as.vector(data$Length_Of_Stay))


hist(data$age)



# colnames(data) <- c('provnum', 'Death Yes/No', 'White Yes/No', 'Length Of Stay', 'age80', 'age', 'admission')
data$Death <- as.factor(data$Death)
data$White <- as.factor(data$White)
data$age80 <- as.factor(data$age80)
str(data)

set.seed(7)
train.index <- createDataPartition(data$admission, p = .7, list = FALSE)
train <- data[ train.index,]
test  <- data[-train.index,]

levels(data$admission)

vglm_model <- vglm(admission ~ Death + White + Length_Of_Stay + age + age80, family=multinomial(refLevel = "Elective"),dat=train)
summary(vglm_model)
confint(vglm_model)

confint(nnet_model)

nnet_model <- multinom(admission ~ Death + White + Length_Of_Stay + age + age80, dat=train)
summary(nnet_model)

step_model <- step(nnet_model)

summary(step_model)



Anova(vglm_model)
Anova(nnet_model)

# ----------------------------------------------------------------------------
nnet_update_model <- update(nnet_model, .~. -age80 -age)
summary(nnet_update_model)

vglm_update_model <- update(vglm_model, .~. -age80 -age)
summary(vglm_update_model)
exp(confint(vglm_update_model))
Anova(nnet_update_model)
Anova(vglm_update_model)
# ----------------------------------------------------------------------------

summary(vglm_update_model)

length(coef(nnet_model))
length(coef(nnet_update_model))
summary(nnet_update_model)

lik <- nnet_update_model$deviance-nnet_model$deviance
1-pchisq(lik, df=4)

lrtest(nnet_update_model, nnet_model)

nnet_model1 <- multinom(admission ~ 1, dat=train)
summary(nnet_model1)

nnet_model1$deviance
nnet_update_model$deviance
lik <- nnet_model1$deviance - nnet_update_model$deviance
1-pchisq(lik, df=6)
lrtest(nnet_model1, nnet_update_model)



# lik <- vglm_update_model$deviance-vglm_model$deviance
# 1-pchisq(lik, df=1)

# lrtest(vglm_model, vglm_update_model)

AIC(nnet_model, nnet_update_model)
# AIC(vglm_model, vglm_update_model)
# ----------------------------------------------------------------------------
png("yeet.png")
plot(allEffects(nnet_update_model))
dev.off()

# ----------------------------------------------------------------------------

res_1 <- predict(nnet_update_model, test)
# res_2 <- predict(vglm_update_model, test, type="response")
#
# predict_2 <- c()
# for (i in 1:nrow(res_2)){
#   result <- colnames(res_2)[as.numeric(which(res_2[i,] == max(res_2[i,]), arr.ind=TRUE))]
#   predict_2 <- c(predict_2,result)
# }
#
# all.equal(as.vector(res_1), predict_2)

confusionMatrix(res_1, test$admission)

# ----------------------------------------------------------------------------

plot(residuals(nnet_update_model))

install.packages("hmftest")
summary()
(exp(coef(nnet_update_model))-1)*100
round(exp(coef(nnet_update_model)),3)

round(exp(confint(nnet_update_model)),3)
