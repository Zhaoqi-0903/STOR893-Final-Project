### Since the depression trait is imbalanced. After cleaning the missing values, there are only 95 participants have experienced diagnosed DSMIV major depressive episode among the 1028 participants. 
### Thus We tried to use oversampling to deal with the imbalanced dataset. We used the modification for SMOTE techiniques from package "imbalance" in R.
### SMOTE stands for the Synthetic Minority Over-sampling Techniques, which works by utilizing a k-nearest neighbor algorithm to create synthetic data for the minority data.
### We first split the data into training and test set, and only add synthetic data into the training set. 

load("~/Downloads/STOR893/FinalProject/STOR-893-Final-Project/Data/merge_df.RData")
if(!require(imbalance)) { install.packages("imbalance", repos = "http://cran.us.r-project.org"); library("imbalance") }

df_dep<-df[,c(1:121,251)]
df_dep<-na.omit(df_dep) #1028 obs
df_dep_sorted<-df_dep
df_dep_sorted$Trait_130<-as.factor(ifelse(df_dep_sorted$Trait_130==1,0,1))
df_dep_sorted<-df_dep_sorted[order(df_dep_sorted$Trait_130),]

table(df_dep_sorted$Trait_130)

set.seed(893)
train<-sample(c(TRUE,FALSE),replace=TRUE,size=nrow(df_dep_sorted),prob=2:1)
trainset<-df_dep_sorted[train,]# 693 observations before oversampling
test=(!train)
testset<-df_dep_sorted[test,] #502 observations 

table(trainset$Trait_130)
table(testset$Trait_130) # leave the testset alone

table(trainset$Trait_130)
#62 subjects have experienced a diagnosed DSMIV Major Depressive Episode over his/her lifetime in the training set
#imbalancedRatio
imbalanceRatio(trainset,"Trait_130")

# generate 200 synthetic data
set.seed(893)
train_smote <- mwmote(trainset, numInstances = 200, classAttr="Trait_130")
train_oversampling<-rbind(trainset,train_smote)
#save(df_dep_oversampling, file = "oversampling_df.RData")


#plot the difference between original dataset and oversampling dataset by using the first two FC feature scores and first two SC feature scores
plotComparison(trainset, train_oversampling, attrs = names(train_oversampling)[2:3], classAttr = "Trait_130")
plotComparison(trainset, train_oversampling, attrs = names(train_oversampling)[62:63], classAttr = "Trait_130")

#save the oversampled trainset and untouched testset 
save(train_oversampling,file="train_oversampling.RData")
save(testset,file="test.RData")

