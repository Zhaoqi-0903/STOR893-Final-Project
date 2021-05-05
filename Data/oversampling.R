### Since the depression trait is imbalanced. After cleaning the missing values, there are only 95 participants have experienced diagnosed DSMIV major depressive episode among the 1028 participants. 
### Thus We tried to use oversampling to deal with the imbalanced dataset. We used the modification for SMOTE techiniques from package "imbalance" in R.
### SMOTE stands for the Synthetic Minority Over-sampling Techniques, which works by utilizing a k-nearest neighbor algorithm to create synthetic data for the minority data.

load("~/Downloads/STOR893/FinalProject/STOR-893-Final-Project/Data/merge_df.RData")
if(!require(imbalance)) { install.packages("imbalance", repos = "http://cran.us.r-project.org"); library("imbalance") }

df_dep<-df[,c(1:121,251)]
df_dep<-na.omit(df_dep) #1028 obs
df_dep_sorted<-df_dep
df_dep_sorted$Trait_130<-as.factor(ifelse(df_dep_sorted$Trait_130==1,0,1))
df_dep_sorted<-df_dep_sorted[order(df_dep_sorted$Trait_130),]

table(df_dep_sorted$Trait_130)
#95 obs have experienced a diagnosed DSMIV Major Depressive Episode over his/her lifetime among 1028 obs
#imbalancedRatio
imbalanceRatio(df_dep_sorted,"Trait_130")

# generate 500 synthetic data
set.seed(893)
df_dep_RWO <- mwmote(df_dep_sorted, numInstances = 500, classAttr="Trait_130")
df_dep_oversampling<-rbind(df_dep_sorted,df_dep_RWO)
#df_dep_oversampling<-df_dep_oversampling[,-1]#remove the subjectID

#plot the difference between original dataset and oversampling dataset by using the first two FC feature scores and first two SC feature scores
plotComparison(df_dep_sorted, df_dep_oversampling, attrs = names(df_dep_oversampling)[2:3], classAttr = "Trait_130")
plotComparison(df_dep_sorted, df_dep_oversampling, attrs = names(df_dep_oversampling)[62:63], classAttr = "Trait_130")

#split the dataset into training and test (2:1)
#set.seed(893)
train<-sample(c(TRUE,FALSE),replace=TRUE,size=nrow(df_dep_oversampling),prob=2:1)
trainset<-df_dep_oversampling[train,]# 1026 observations
test=(!train)
testset<-df_dep_oversampling[test,] #502 observations
