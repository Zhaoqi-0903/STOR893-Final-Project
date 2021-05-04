library(R.matlab)
library(tidyverse)
library(ggplot2)

FC = readMat("HCP_cortical_DesikanAtlas_FC.mat")
names(FC)
hcp.cortical.fc = FC$hcp.cortical.fc
length(hcp.cortical.fc)

hcp.cortical.fc11 = hcp.cortical.fc[[1]][[1]]
dim(hcp.cortical.fc11)

SC = readMat("HCP_cortical_DesikanAtlas_SC.mat")
names(SC)
hcp.sc.count = SC$hcp.sc.count
dim(hcp.sc.count)

TNPCA_FC = readMat("TNPCA_Coeff_HCP_Functional_Connectome.mat")

fc.pca.coeff = TNPCA_FC$PCA.Coeff
fc.pca.coeff = fc.pca.coeff[1,,]
dim(fc.pca.coeff)
fc.subid = TNPCA_FC$network.subject.ids
dim(fc.subid)
fc.pca = data.frame(subID = t(fc.subid))
fc.pca = cbind(fc.pca, fc.pca.coeff)
head(fc.pca)

n.feature.fc = ncol(fc.pca.coeff)
colnames(fc.pca)[-1] = paste("FC_PCA", as.character(c(1:n.feature.fc)), sep = "_")
head(fc.pca)

TNPCA_SC = readMat("TNPCA_Coeff_HCP_Structural_Connectome.mat")
sc.pca.coeff = TNPCA_SC$PCA.Coeff
dim(sc.pca.coeff)
sc.pca.coeff = sc.pca.coeff[1,,]

n.feature.sc = ncol(sc.pca.coeff)

sc.subid = TNPCA_SC$sub.id
dim(sc.subid)
sc.pca = data.frame(subID = sc.subid)
sc.pca = cbind(sc.pca, sc.pca.coeff)
colnames(sc.pca)[-1] = paste("SC_PCA", as.character(c(1:n.feature.sc)), sep = "_")
head(sc.pca)

Traits_175 = readMat("HCP_175Traits.mat")
names(Traits_175)
hcp.subj.id = Traits_175$hcp.subj.id
dim(hcp.subj.id)
traits.175 = Traits_175$traits.175
dim(traits.175)

traits.175.df = data.frame(subID = hcp.subj.id)
traits.175.df = cbind(traits.175.df, t(traits.175))
colnames(traits.175.df)[-1] = paste("Trait", as.character(c(1:175)), sep = "_")

df = data.frame(subID = hcp.subj.id)
df1 = merge(df, fc.pca, by = "subID", all.x = TRUE)
df1 = merge(df1, sc.pca, by = "subID", all.x = TRUE)
df1 = merge(df1, traits.175.df, by = "subID", all.x = TRUE)
dim(df1)

# SC data
df.sc = merge(df, traits.175.df, by = "subID", all.x = TRUE)
df.sc = merge(df.sc, sc.pca, by = "subID")
dim(df.sc)

# FC data
df.fc = merge(df, traits.175.df, by = "subID", all.x = TRUE)
df.fc = merge(df.fc, fc.pca, by = "subID")
dim(df.fc)

# Trait_130 depression
df.sc.t130 = df.sc[,c(1,131,177:236)]
df.sc.t130.1 = na.omit(df.sc.t130)
df.sc.t130.1$depression = ifelse(df.sc.t130.1$Trait_130 == 5,1,0)
table(df.sc.t130.1$depression)
dim(df.sc.t130.1)
#head(df.sc.t130.1)
aov_result = aov(df.sc.t130.1$depression ~ df.sc.t130.1$SC_PCA_1+df.sc.t130.1$SC_PCA_2+df.sc.t130.1$SC_PCA_3)
summary(aov_result)

df.sc.t130.1.sort = df.sc.t130.1 %>% 
  arrange(., desc(Trait_130))
head(df.sc.t130.1.sort)
tail(df.sc.t130.1.sort)

# logistic regression for Trait_130
# split 2/3 of data for train, 1/3 for test and keep the similar ratio of
# depression in these two subsets
library(caret)
set.seed(15)
df.sc.log = df.sc.t130.1[,c(-1,-2)]
dim(df.sc.log)
idx_130 = createDataPartition(df.sc.log$depression, p = 2/3, list = FALSE)
trn_130 = df.sc.log[idx_130,]
dim(trn_130)
tst_130 = df.sc.log[-idx_130,]
dim(tst_130)
table(trn_130$depression)
table(tst_130$depression)

mod_log = glm(depression ~ ., data=trn_130, family="binomial")
#coef(mod_log)
mod_log_probs = predict(mod_log, newdata = tst_130, type = 'response')
mod_log_pred = ifelse(mod_log_probs > 0.5, 1, 0)
table(mod_log_pred)
err.log = mean(tst_130$depression != mod_log_pred)
err.log

# LDA for Trait_130
library(MASS)
mod_lda = lda(depression ~ ., data = trn_130)
mod_lda
lda.pred = predict(mod_lda, tst_130)
#head(lda.pred$class)
#head(lda.pred$posterior)
table(lda.pred$class)
sum(lda.pred$posterior[,2]>0.5)
err.lda = mean(tst_130$depression != lda.pred$class)
err.lda

# QDA for Trait_130
mod_qda = qda(depression ~ ., data = trn_130)
mod_qda
qda.pred = predict(mod_qda, tst_130)
head(qda.pred$class)
head(qda.pred$posterior)
table(qda.pred$class)
sum(qda.pred$posterior[,2]>0.5)
err.qda = mean(tst_130$depression != qda.pred$class)
err.qda

# Naive Bayes for Trait_130
library(e1071)
mod_nb = naiveBayes(depression ~ ., data= trn_130)
mod_nb$apriori
nb.pred = predict(mod_nb, tst_130)
nb.post = predict(mod_nb, tst_130, type='raw')
head(nb.post)
nb_pred = ifelse(nb.post[,2]>0.5,1,0)
table(nb_pred)
err.nb = mean(tst_130$depression != nb_pred)
err.nb

