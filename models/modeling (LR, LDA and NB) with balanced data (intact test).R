load("/Users/lingcai/Desktop/final project/Option2/Data/oversampling_train_and_unchanged_test.RData")
dim(train_oversampling)
#head(train_oversampling)
table(train_oversampling$Trait_130)

dim(testset)
table(testset$Trait_130)

# logistic regression for Trait_130
library(caret)
library(ROCR)
mod_log = glm(Trait_130 ~ ., data=train_oversampling, family="binomial")
#coef(mod_log)
mod_log_probs = predict(mod_log, newdata = testset, type = 'response')
mod_log_pred = ifelse(mod_log_probs > 0.5, 1, 0)
table(mod_log_pred)
err.log = mean(testset$Trait_130 != mod_log_pred)
err.log
confusionMatrix(as.factor(mod_log_pred), as.factor(testset$Trait_130))

# Receiver Operating Characteristic (ROC) curve
pred = prediction(mod_log_probs, testset$Trait_130)
roc = performance(pred,"tpr","fpr")
plot(roc,
     colorize = T,
     main = "Logistic regression ROC Curve",
     ylab = "Sensitivity",
     xlab = "1-Specificity")
abline(a=0,b=1)
  
# Area Under Curve (AUC)
auc.lr = performance(pred, "auc")
auc.lr = unlist(slot(auc.lr,"y.values"))
auc.lr = round(auc.lr,4)
legend(.6,.4, auc.lr, title = "AUC", cex=0.9)

# LDA for Trait_130 
library(MASS)
mod_lda = lda(Trait_130 ~ ., data = train_oversampling)
mod_lda
lda.pred = predict(mod_lda, testset)
#head(lda.pred$class)
#head(lda.pred$posterior)
table(lda.pred$class)
#sum(lda.pred$posterior[,2]>0.5)
err.lda = mean(testset$Trait_130 != lda.pred$class)
err.lda
confusionMatrix(as.factor(lda.pred$class), as.factor(testset$Trait_130))

# Receiver Operating Characteristic (ROC) curve
pred = prediction(lda.pred$posterior[,2], testset$Trait_130)
roc = performance(pred,"tpr","fpr")
plot(roc,
     colorize = T,
     main = "LDA ROC Curve",
     ylab = "Sensitivity",
     xlab = "1-Specificity")
abline(a=0,b=1)

# Area Under Curve (AUC)
auc.lda = performance(pred, "auc")
auc.lda = unlist(slot(auc.lda,"y.values"))
auc.lda = round(auc.lda,4)
legend(.6,.4, auc.lda, title = "AUC", cex=0.9)

# Naive Bayes for Trait_130
library(e1071)
mod_nb = naiveBayes(Trait_130 ~ ., data= train_oversampling)
mod_nb$apriori
nb.pred = predict(mod_nb, testset)
nb.post = predict(mod_nb, testset, type='raw')
head(nb.post)
nb_pred = ifelse(nb.post[,2]>0.5,1,0)
table(nb_pred)
err.nb = mean(testset$Trait_130 != nb_pred)
err.nb
confusionMatrix(as.factor(nb_pred), as.factor(testset$Trait_130))

# Receiver Operating Characteristic (ROC) curve
pred = prediction(nb.post[,2], testset$Trait_130)
roc = performance(pred,"tpr","fpr")
plot(roc,
     colorize = T,
     main = "Naive Bayes ROC Curve",
     ylab = "Sensitivity",
     xlab = "1-Specificity")
abline(a=0,b=1)

# Area Under Curve (AUC)
auc.nb = performance(pred, "auc")
auc.nb = unlist(slot(auc.nb,"y.values"))
auc.nb = round(auc.nb,4)
legend(.6,.4, auc.nb, title = "AUC", cex=0.9)




