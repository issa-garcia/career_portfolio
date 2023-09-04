#install.packages("remotes")
#remotes::install_github("angelgar/voxel")
library(oro.nifti)
library(voxel)

folder_withdata <- "C:/Users/vissa/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_all/realSGEjobarray/"
subjects <- list.dirs(path=folder_withdata,full.names = FALSE,recursive = FALSE)
i=1
subjects[i] = "sub-001"
atlas_Caud2Medulla <- readNIfTI(paste(folder_withdata,subjects[i],"/ses-t0/dwi/",subjects[i],"_ses-t0_space-individual_desc-13Caud2MedullaINfsorig_dwi.nii.gz",sep = ""), reorient = FALSE)
atlas_Caud2Palli <- readNIfTI(paste(folder_withdata,subjects[i],"/ses-t0/dwi/",subjects[i],"_ses-t0_space-individual_desc-14Caud2PalliINfsorig_dwi.nii.gz",sep = ""), reorient = FALSE)
atlas_DLPFC2Caud <- readNIfTI(paste(folder_withdata,subjects[i],"/ses-t0/dwi/",subjects[i],"_ses-t0_space-individual_desc-15DLPFC2CaudINfsorig_dwi.nii.gz",sep = ""), reorient = FALSE)
atlas_DLPFC2Thal <- readNIfTI(paste(folder_withdata,subjects[i],"/ses-t0/dwi/",subjects[i],"_ses-t0_space-individual_desc-16DLPFC2ThalINfsorig_dwi.nii.gz",sep = ""), reorient = FALSE)
atlas_Thal2Medulla <- readNIfTI(paste(folder_withdata,subjects[i],"/ses-t0/dwi/",subjects[i],"_ses-t0_space-individual_desc-17Thal2MedullaINfsorig_dwi.nii.gz",sep = ""), reorient = FALSE)
atlas_Thal2Palli <- readNIfTI(paste(folder_withdata,subjects[i],"/ses-t0/dwi/",subjects[i],"_ses-t0_space-individual_desc-18Thal2PalliINfsorig_dwi.nii.gz",sep = ""), reorient = FALSE)
atlas_DLPFC2rvmPFC <- readNIfTI(paste(folder_withdata,subjects[i],"/ses-t0/dwi/",subjects[i],"_ses-t0_space-individual_desc-14DLPFC2rvmPFCINfsorig_dwi.nii.gz",sep = ""), reorient = FALSE)
atlas_rAngG2rDLPFC <- readNIfTI(paste(folder_withdata,subjects[i],"/ses-t0/dwi/",subjects[i],"_ses-t0_space-individual_desc-14rAngG2rDLPFCINfsorig_dwi.nii.gz",sep = ""), reorient = FALSE)
atlas_vmPFC2DLPFC <- readNIfTI(paste(folder_withdata,subjects[i],"/ses-t0/dwi/",subjects[i],"_ses-t0_space-individual_desc-14vmPFC2DLPFCINfsorig_dwi.nii.gz",sep = ""), reorient = FALSE)

OD <- readNIfTI(paste(folder_withdata,subjects[i],"/ses-t0/dwi/",subjects[i],"_ses-t0_space-individual_desc-09NODDIfitICVF_dwi.nii.gz",sep = ""), reorient = FALSE)
lmerNIfTI(OD,atlas_Caud2Medulla,)




image <- oro.nifti::nifti(img = array(1:1600, dim =c(4,4,4,25)))
mask <- oro.nifti::nifti(img = array(c(rep(0,14),1,1), dim = c(4,4,4,1)))
set.seed(1)
covs <- data.frame(x = runif(25), id = rep(1:5,5))
fm1 <- "~ x + (1|id)"
Maps <- lmerNIfTI(image, mask, formula = fm1, subjData = covs, method="fdr", ncores = 1)

models <- lmerVoxel(image, mask, formula = fm1, subjData = covs, ncores = 1, REML=T)
