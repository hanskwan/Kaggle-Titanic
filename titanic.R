## Import dataset  ##
train <- read.csv("titanic_train.csv")
eval_set <- read.csv("titanic_test.csv")
sessionInfo()
.libPaths()


## Import Library ##

install.packages("pbkrtest")
library(caret)
library(tidyverse)
library(MASS)
library(ggplot2)
library(mgcv)
library(lmtest)
library(dplyr)
library(mtcars)

# Trouble shoot for package can't find
setRepositories() # check repository
ap <- available.packages()
View(ap)
"rpart.plot" %in% rownames(ap)

## Missing data ##
is.na(train)
train_na <- na.omit(train)

## EDA ##
# plot_1, higher class with higher survival prob
ggplot(data = train_na) + geom_bar(aes( x = Pclass, fill = as.factor(train_na$Survived))) +
  ggtitle(label = "Survival rate between class") +
  theme_bw()

Pclass_table <- train_na %>%
  group_by(Pclass, Survived) %>%
  summarise(n())
  as.data.frame()

Pclass_ratio <- matrix(c((sum(Pclass_table[1:2,3]/nrow(train_na))), 
                         (sum(Pclass_table[3:4,3]/nrow(train_na))),
                         (sum(Pclass_table[5:6,3]/nrow(train_na))),
                         (Pclass_table[2,3])/sum(Pclass_table[1:2,3]),
                         (Pclass_table[4,3])/sum(Pclass_table[3:4,3]),
                         (Pclass_table[6,3])/sum(Pclass_table[5:6,3])
                         ),nrow = 2,ncol = 3, byrow = TRUE)
dimnames(Pclass_ratio) = list(c("Proportion","Survival_ratio"), c("P1","P2","P3"))
Pclass_ratio

## Discard variables by common sense
train_na_col <- train_na[c(-1,-4,-9,-11)]

## Train and test set
set.seed(51)
sample_T <- sample.int(n= nrow(train_na_col), size = floor(.8*nrow(train_na_col)), replace = F)
train_set <- train_na_col[sample_T,]
test_set <- train_na_col[-sample_T,]


## Multicollinearity by "caret" package ##


## Logistic model, removed ID, Name and Ticket 
log_01 <-  glm(Survived~., family = "binomial", data = train_set)
log_01
summary(log_01)


## Follow from ***, model with Pclass + Sex + Age + SibSp
log_02 <- glm(Survived ~       Pclass + Sex + Age + SibSp , family = "binomial", data = train_set)
log_02
summary(log_02)
## All variables are significant 


# Look at stepAIC function #
log_step <- stepAIC(log_01, direction = "both")
log_step
## stepAIC same as backward model "log_02"  


## prediction of logit  ##
pred_log_02 <- predict(log_02)

## Convert into formal 0 to 1 probability ##
prob_log_02 <- 1/(1+exp(-pred_log_02))
summary(prob_log_02)

## K-fold Cross Validation ##

## Confusion Matrix  ##
pred <- predict(log_02, newdata=train_set)
pred_y <- as.numeric(pred>0)
true_y <- as.numeric(train_na_col$Survived=="1")
true_pos <- (true_y==1) & (pred_y==1)
true_neg <- (true_y==0) & (pred_y==0)
false_pos <- (true_y==0) & (pred_y==1)
false_neg <- (true_y==1) & (pred_y==0)
conf_mat <- matrix(c(sum(true_pos), sum(false_pos), sum(false_neg), sum(true_neg)), 2, 2)
colnames(conf_mat) <- c('Yhat = 1', 'Yhat = 0')
rownames(conf_mat) <- c('y = 1', 'y = 0')
conf_mat

## Accuracy of model ##
accuracy <- sum(conf_mat[1,1],conf_mat[2,2])/sum(conf_mat)
accuracy
# confusion matrix measure performance with fixed threshold

# ROC examines the performance of a classifer without fixing the threshold

## Likelihood Ratio test  ##
anova(log_01, log_02, test = "Chisq")
lrtest(log_01, log_02)

## Trying GAM ##
log_gam <- gam(Survived ~ Pclass + Sex + s(Age) + SibSp, family = "binomial", data = train_set)
pred_gam <- predict(log_gam)
summary(pred_gam)

## Analysis of residual ##
terms <- predict(log_gam, type = "terms")
partial_resid <- resid(log_gam) + terms
df <- data.frame(Age = train_ona[, 'Age'],
                 terms = terms[, 's(Age)'],
                 partial_resid = partial_resid[, 's(Age)'])
ggplot(df, aes(x=Age, y=partial_resid, solid = FALSE ))+
  geom_point(shape=46, alpha=.4)+
  geom_line(aes(x=Age, y=terms),
            color = 'red', alpha=.5, size=1.5)+
  labs(y='Partial Residual')

# Tree model
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

# method = class, indicates classification tree, default is regression
Titanic_tree <- rpart(Survived~., data = train_set, method = "class", control = rpart.control(cp=.008))
plot(Titanic_tree,uniform=TRUE,margin=.001)
text(Titanic_tree)





