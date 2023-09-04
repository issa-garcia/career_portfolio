library(oro.nifti)
library(neurobase)
MNI152 <- readNIfTI("C:/Users/vissa/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_all/realSGEjobarray/code/IITmean_t1_256.nii.gz", reorient = F)

ICVF_VAS_Caud2Medulla <- readNIfTI("C:/Users/vissa/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_all/ROIs_new/ICVF/lmer_voxelwise/Caud2Medulla/3wayinteractionICVF_Tmap_corrected_FWHM2.nii", reorient = F)
slice <- seq(256-165,256-126,length.out = 16)
overlay.nifti(x = MNI152, NA.x = T,NA.y = T,col.y = "red",
              y = ICVF_VAS_Caud2Medulla, plane = "axial",
              z = slice,plot.type = "single")

ICVF_VAS_Caud2Palli <- readNIfTI("C:/Users/vissa/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_all/ROIs_new/ICVF/lmer_voxelwise/Caud2Palli/3wayinteractionICVF_Tmap_corrected_FWHM2.nii", reorient = F)
slice <- seq(256-165,256-126,length.out = 16)
overlay.nifti(x = MNI152, NA.x = T,NA.y = T,col.y = "red",
              y = ICVF_VAS_Caud2Palli, plane = "axial",
              z = slice,plot.type = "single")

ICVF_BIS_DLPFC2rvmPFC <- readNIfTI("C:/Users/vissa/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_all/ROIs_new/ICVF/lmer_voxelwise/DLPFC2rvmPFC/2wayinteractionICVF_BIStot_Tmap_corrected_FWHM2.nii", reorient = F)
slice <- seq(256-165,256-126,length.out = 16)
overlay.nifti(x = MNI152, NA.x = T,NA.y = T,col.y = "red",
              y = ICVF_BIS_DLPFC2rvmPFC, plane = "axial",
              z = slice,plot.type = "single")

ICVF_VAS_Thal2Medulla <- readNIfTI("C:/Users/vissa/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_all/ROIs_new/ICVF/lmer_voxelwise/Thal2Medulla/3wayinteractionICVF_Tmap_corrected_FWHM2.nii", reorient = F)
slice <- seq(256-165,256-126,length.out = 16)
overlay.nifti(x = MNI152, NA.x = T,NA.y = T,col.y = "red",
              y = ICVF_VAS_Thal2Medulla, plane = "axial",
              z = slice,plot.type = "single")

ICVF_VAS_Thal2Palli <- readNIfTI("C:/Users/vissa/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_all/ROIs_new/ICVF/lmer_voxelwise/Thal2Palli/3wayinteractionICVF_Tmap_corrected_FWHM2.nii", reorient = F)
slice <- seq(256-165,256-126,length.out = 16)
overlay.nifti(x = MNI152, NA.x = T,NA.y = T,col.y = "red",
              y = ICVF_VAS_Thal2Palli, plane = "axial",
              z = slice,plot.type = "single")

ICVF_BIS_vmPFC2DLPFC <- readNIfTI("C:/Users/vissa/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_all/ROIs_new/ICVF/lmer_voxelwise/vmPFC2DLPFC/2wayinteractionICVF_BIStot_Tmap_corrected_FWHM2.nii", reorient = F)
slice <- seq(256-165,256-126,length.out = 16)
overlay.nifti(x = MNI152, NA.x = T,NA.y = T,col.y = "red",
              y = ICVF_BIS_vmPFC2DLPFC, plane = "axial",
              z = slice,plot.type = "single")



ISOVF_BIS_DLPFC2rvmPFC <- readNIfTI("C:/Users/vissa/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_all/ROIs_new/ISOVF/lmer_voxelwise/DLPFC2rvmPFC/3wayinteractionISOVF_BIStot_Tmap_corrected_FWHM2.nii", reorient = F)
slice <- seq(256-165,256-126,length.out = 16)
overlay.nifti(x = MNI152, NA.x = T,NA.y = T,col.y = "red",
              y = ISOVF_BIS_DLPFC2rvmPFC, plane = "axial",
              z = slice,plot.type = "single")

ISOVF_CCQnow_DLPFC2rvmPFC <- readNIfTI("C:/Users/vissa/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_all/ROIs_new/ISOVF/lmer_voxelwise/DLPFC2rvmPFC/3wayinteractionISOVF_CCQnow_Tmap_corrected_FWHM2.nii", reorient = F)
slice <- seq(256-165,256-126,length.out = 16)
overlay.nifti(x = MNI152, NA.x = T,NA.y = T,col.y = "red",
              y = ISOVF_CCQnow_DLPFC2rvmPFC, plane = "axial",
              z = slice,plot.type = "single")

ISOVF_CCQnow_rAngG2rDLPFC <- readNIfTI("C:/Users/vissa/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_all/ROIs_new/ISOVF/lmer_voxelwise/rAngG2rDLPFC/2wayinteractionISOVF_CCQnow_Tmap_corrected_FWHM2.nii", reorient = F)
slice <- seq(256-165+24,256-126+24,length.out = 16)
overlay.nifti(x = MNI152, NA.x = T,NA.y = T,col.y = "red",
              y = ISOVF_CCQnow_rAngG2rDLPFC, plane = "axial",
              z = slice,plot.type = "single")

ISOVF_BIS_vmPFC2DLPFC <- readNIfTI("C:/Users/vissa/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_all/ROIs_new/ISOVF/lmer_voxelwise/vmPFC2DLPFC/3wayinteractionISOVF_BIStot_Tmap_corrected_FWHM2.nii", reorient = F)
slice <- seq(256-165,256-126,length.out = 16)
overlay.nifti(x = MNI152, NA.x = T,NA.y = T,col.y = "red",
              y = ISOVF_BIS_vmPFC2DLPFC, plane = "axial",
              z = slice,plot.type = "single")




OD_BIS_DLPFC2rvmPFC <- readNIfTI("C:/Users/vissa/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_all/ROIs_new/OD/lmer_voxelwise/DLPFC2rvmPFC/3wayinteractionOD_BIStot_Tmap_corrected_FWHM2.nii", reorient = F)
slice <- seq(256-165,256-126,length.out = 16)
overlay.nifti(x = MNI152, NA.x = T,NA.y = T,col.y = "red",
              y = OD_BIS_DLPFC2rvmPFC, plane = "axial",
              z = slice,plot.type = "single")

OD_BIS_vmPFC2DLPFC <- readNIfTI("C:/Users/vissa/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_all/ROIs_new/OD/lmer_voxelwise/vmPFC2DLPFC/2wayinteractionOD_BIStot_Tmap_corrected_FWHM2.nii", reorient = F)
slice <- seq(256-165,256-126,length.out = 16)
overlay.nifti(x = MNI152, NA.x = T,NA.y = T,col.y = "red",
              y = OD_BIS_vmPFC2DLPFC, plane = "axial",
              z = slice,plot.type = "single")


