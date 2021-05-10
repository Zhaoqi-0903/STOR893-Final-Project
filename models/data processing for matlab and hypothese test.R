library(R.matlab)
library(reshape2)

# SC data
SC = readMat("HCP_cortical_DesikanAtlas_SC.mat")
names(SC)
hcp.sc.count = SC$hcp.sc.count
dim(hcp.sc.count)
sc.subj.id = SC$all.id
dim(sc.subj.id)

# Trait data
Traits_175 = readMat("HCP_175Traits.mat")
names(Traits_175)
hcp.subj.id = Traits_175$hcp.subj.id
dim(hcp.subj.id)
traits.175 = Traits_175$traits.175
dim(traits.175)

traits.175.df = data.frame(subID = hcp.subj.id)
traits.175.df = cbind(traits.175.df, t(traits.175))
colnames(traits.175.df)[-1] = paste("Trait", as.character(c(1:175)), sep = "_")

# Merge SC data with Trait
df = data.frame(subID = sc.subj.id)
df.sc = merge(df, traits.175.df, by = "subID", all.x = TRUE)
dim(df.sc)

# Extract the SC data in the depression group
# Trait_130 is depression
df.sc.t130 = df.sc[,c(1,131)]
df.sc.t130.1 = na.omit(df.sc.t130)
df.sc.t130.1$depression = ifelse(df.sc.t130.1$Trait_130 == 5,1,0)
table(df.sc.t130.1$depression)
df.sc.t130.dep = subset(df.sc.t130.1,depression==1)
head(df.sc.t130.dep)
df.sc.t130.dep.id = df.sc.t130.dep[,c(1,3)]
head(df.sc.t130.dep.id)
dep = as.numeric(rownames(df.sc.t130.dep.id))
dep[1:5]

sc.dep = hcp.sc.count[,,c(dep)]
dim(sc.dep)

# Extract the SC data in the no_depression group
df.sc.t130.nodep = subset(df.sc.t130.1,depression==0)
idx.nodep = sample(nrow(df.sc.t130.nodep),95)
df.sc.t130.nodep1 = df.sc.t130.nodep[idx.nodep,]
dim(df.sc.t130.nodep1)
df.sc.t130.nodep1.id = df.sc.t130.nodep1[,c(1,3)]
head(df.sc.t130.nodep1.id)
nodep = as.numeric(rownames(df.sc.t130.nodep1.id))
nodep[1:5]

sc.nodep = hcp.sc.count[,,c(nodep)]
dim(sc.nodep)

# get the mean matrix for two groups
mean.dep.mat = matrix(0, nrow=68, ncol=68)
mean.nodep.mat = matrix(0, nrow=68, ncol=68)
for (i in 1:68) {
  for (j in 1:68) {
    # mean of count of connection (i,j) from all subjects in depression group
    mean.dep.mat[i,j] = mean(sc.dep[i,j,1:95]) 
    # mean of count of connection (i,j) from all subjects in no_depression group
    mean.nodep.mat[i,j] = mean(sc.nodep[i,j,1:95]) 
  }
}

# write .mat files for making circular plots
filename = paste("mean.dep",".mat", sep = "")
writeMat(filename, mean.dep=mean.dep.mat)
writeMat("mean.nodep.mat", mean.nodep=mean.nodep.mat)

pvalue.mat = matrix(1, nrow=68, ncol=68)

for (i in 1:67) {
  for (j in (i+1):68) {
    #connection (i,j) from all subjects in depression group
    edgeij.dep = sc.dep[i,j,1:95] 
    #connection (i,j) from all subjects in no_depression group
    edgeij.nodep = sc.nodep[i,j,1:95] 
    
    #test mean difference between these two groups using t-test
    testij = t.test(edgeij.dep,edgeij.nodep)
    #extract p-value
    pvalue.mat[i,j] = as.numeric(testij["p.value"])
   
  }
}
pvalue.mat[is.na(pvalue.mat)] <- 1
#sum(is.na(pvalue.mat))

writeMat("pvalues.trait130.mat", pvalues = pvalue.mat)
writeMat("neg.log10.pvalues.trait130.mat", nlogpvalues = -log10(pvalue.mat))
#pvalue.mat[1:10,1:10]

# Extract highly different edges with p-value cutoff of 0.05 and 0.01
library(reshape2)
p1 = pvalue.mat %>%
  melt(varname=c("ROIa", "ROIb")) %>%
  as_tibble() %>%
  mutate(ROIa = paste0("ROI",ROIa), ROIb = paste0("ROI",ROIb))
dim(p1)

sub.p1 = subset(p1, value <= 0.05)
sub.p2 = subset(p1, value <= 0.01)
dim(sub.p1)
dim(sub.p2)
min(sub.p1$value)
max(sub.p1$value)

write.csv(sub.p1,"top.dif.edge.csv")
read.csv("top.dif.edge.csv")

write.csv(sub.p2,"top.dif.edge2.csv")
read.csv("top.dif.edge2.csv")



