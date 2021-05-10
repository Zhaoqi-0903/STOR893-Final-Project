library(R.matlab)
fc = readMat("Data/FC/HCP_cortical_DesikanAtlas_FC.mat")
hcp.cortical.fc = fc$hcp.cortical.fc
length(hcp.cortical.fc)

HCP_175Traits = readMat("Data/traits/175traits/HCP_175Traits.mat")
names(HCP_175Traits)
hcp.subj.id = HCP_175Traits$hcp.subj.id
traits.175 = HCP_175Traits$traits.175
dim(traits.175)
traits.175.df = data.frame(subID = hcp.subj.id)
traits.175.df = cbind(traits.175.df, t(traits.175))
colnames(traits.175.df)[-1] = paste("Trait", as.character(c(1:175)), sep = "_")


table1_hcp = read.csv("Data/traits/table1_hcp.csv")
table2_hcp = read.csv("Data/traits/table2_hcp.csv")

hcp.cortical.fc11 = hcp.cortical.fc[[1]][[1]]
dim(hcp.cortical.fc11)

n.subject = length(hcp.subj.id)

TNPCA_Coeff_HCP_FC = readMat("Data/TNPCA_Result/TNPCA_Coeff_HCP_Functional_Connectome.mat")
TNPCA_Coeff_HCP_SC = readMat("Data/TNPCA_Result/TNPCA_Coeff_HCP_Structural_Connectome.mat")
sc.pca.coeff = TNPCA_Coeff_HCP_SC$PCA.Coeff
dim(sc.pca.coeff)
sc.pca.coeff = sc.pca.coeff[1,,]

n.feature.sc = ncol(sc.pca.coeff)

sc.subid = TNPCA_Coeff_HCP_SC$sub.id
sc.pca = data.frame(subID = sc.subid)
sc.pca = cbind(sc.pca, sc.pca.coeff)
colnames(sc.pca)[-1] = paste("SCfeature", as.character(c(1:n.feature.sc)), sep = "_")


fc.pca.coeff = TNPCA_Coeff_HCP_FC$PCA.Coeff
fc.pca.coeff = fc.pca.coeff[1,,]
fc.subid = TNPCA_Coeff_HCP_FC$network.subject.ids
fc.pca = data.frame(subID = t(fc.subid))
fc.pca = cbind(fc.pca, fc.pca.coeff)

n.feature.fc = ncol(fc.pca.coeff)
colnames(fc.pca)[-1] = paste("FCfeature", as.character(c(1:n.feature.fc)), sep = "_")



df = data.frame(subID = hcp.subj.id)
df = merge(df, fc.pca, by = "subID", all.x = TRUE)
df = merge(df, sc.pca, by = "subID", all.x = TRUE)
df = merge(df, traits.175.df, by = "subID", all.x = TRUE)



