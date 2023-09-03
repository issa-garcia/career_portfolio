library(oro.nifti)
library(tidyverse)
library(ggpubr)
library(gt)
library(hrbrthemes)
library(viridis)

folder_withdata <- "C:/Users/vissa/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_all/realSGEjobarray/"
subjects <- list.dirs(path=folder_withdata,full.names = FALSE,recursive = FALSE)

atlas_Caud2Medulla <- list()
atlas_Caud2Palli <- list()
atlas_DLPFC2Caud <- list()
atlas_DLPFC2Thal <- list()
atlas_Thal2Medulla <- list()
atlas_Thal2Palli <- list()
atlas_DLPFC2rvmPFC <- list()
atlas_rAngG2rDLPFC <- list()
atlas_vmPFC2DLPFC <- list()
OD <- list()
OD_sub_i <- list()
df_sub_i_Caud2Medulla <- list()
df_sub_i_Caud2Palli <- list()
df_sub_i_DLPFC2Caud <- list()
df_sub_i_DLPFC2Thal <- list()
df_sub_i_Thal2Medulla <- list()
df_sub_i_Thal2Palli <- list()
df_sub_i_DLPFC2rvmPFC <- list()
df_sub_i_rAngG2rDLPFC <- list()
df_sub_i_vmPFC2DLPFC <- list()
for(i in 1:50) {
  atlas_Caud2Medulla <- readNIfTI(paste(folder_withdata,subjects[i],"/ses-t0/dwi/",subjects[i],"_ses-t0_space-individual_desc-13Caud2MedullaINfsorig_dwi.nii.gz",sep = ""), reorient = FALSE)
  atlas_Caud2Palli <- readNIfTI(paste(folder_withdata,subjects[i],"/ses-t0/dwi/",subjects[i],"_ses-t0_space-individual_desc-14Caud2PalliINfsorig_dwi.nii.gz",sep = ""), reorient = FALSE)
  atlas_DLPFC2Caud <- readNIfTI(paste(folder_withdata,subjects[i],"/ses-t0/dwi/",subjects[i],"_ses-t0_space-individual_desc-15DLPFC2CaudINfsorig_dwi.nii.gz",sep = ""), reorient = FALSE)
  atlas_DLPFC2Thal <- readNIfTI(paste(folder_withdata,subjects[i],"/ses-t0/dwi/",subjects[i],"_ses-t0_space-individual_desc-16DLPFC2ThalINfsorig_dwi.nii.gz",sep = ""), reorient = FALSE)
  atlas_Thal2Medulla <- readNIfTI(paste(folder_withdata,subjects[i],"/ses-t0/dwi/",subjects[i],"_ses-t0_space-individual_desc-17Thal2MedullaINfsorig_dwi.nii.gz",sep = ""), reorient = FALSE)
  atlas_Thal2Palli <- readNIfTI(paste(folder_withdata,subjects[i],"/ses-t0/dwi/",subjects[i],"_ses-t0_space-individual_desc-18Thal2PalliINfsorig_dwi.nii.gz",sep = ""), reorient = FALSE)
  atlas_DLPFC2rvmPFC <- readNIfTI(paste(folder_withdata,subjects[i],"/ses-t0/dwi/",subjects[i],"_ses-t0_space-individual_desc-14DLPFC2rvmPFCINfsorig_dwi.nii.gz",sep = ""), reorient = FALSE)
  atlas_rAngG2rDLPFC <- readNIfTI(paste(folder_withdata,subjects[i],"/ses-t0/dwi/",subjects[i],"_ses-t0_space-individual_desc-14rAngG2rDLPFCINfsorig_dwi.nii.gz",sep = ""), reorient = FALSE)
  atlas_vmPFC2DLPFC <- readNIfTI(paste(folder_withdata,subjects[i],"/ses-t0/dwi/",subjects[i],"_ses-t0_space-individual_desc-14vmPFC2DLPFCINfsorig_dwi.nii.gz",sep = ""), reorient = FALSE)
  
  OD <- readNIfTI(paste(folder_withdata,subjects[i],"/ses-t0/dwi/",subjects[i],"_ses-t0_space-individual_desc-11NODDIfitOD_dwi.nii.gz",sep = ""), reorient = FALSE)
  
  logind <- atlas_Caud2Medulla@.Data == 1
  tmp <- OD@.Data[logind]
  OD_sub_i[[i]] <- mean(tmp)#[tmp > 0]
  df_sub_i_Caud2Medulla[[i]] <- data_frame(val = OD_sub_i[[i]]) %>%
    mutate(., sub = subjects[i]) %>%
    mutate(., atlas = "Caud2Medulla")
  
  logind <- atlas_Caud2Palli@.Data == 1
  tmp <- OD@.Data[logind]
  OD_sub_i[[i]] <- mean(tmp)#[tmp > 0]
  df_sub_i_Caud2Palli[[i]] <- data_frame(val = OD_sub_i[[i]]) %>%
    mutate(., sub = subjects[i]) %>%
    mutate(., atlas = "Caud2Palli")
  
  logind <- atlas_DLPFC2Caud@.Data == 1
  tmp <- OD@.Data[logind]
  OD_sub_i[[i]] <- mean(tmp)#[tmp > 0]
  df_sub_i_DLPFC2Caud[[i]] <- data_frame(val = OD_sub_i[[i]]) %>%
    mutate(., sub = subjects[i]) %>%
    mutate(., atlas = "DLPFC2Caud")
  
  logind <- atlas_DLPFC2Thal@.Data == 1
  tmp <- OD@.Data[logind]
  OD_sub_i[[i]] <- mean(tmp)#[tmp > 0]
  df_sub_i_DLPFC2Thal[[i]] <- data_frame(val = OD_sub_i[[i]]) %>%
    mutate(., sub = subjects[i]) %>%
    mutate(., atlas = "DLPFC2Thal")
  
  logind <- atlas_Thal2Medulla@.Data == 1
  tmp <- OD@.Data[logind]
  OD_sub_i[[i]] <- mean(tmp)#[tmp > 0]
  df_sub_i_Thal2Medulla[[i]] <- data_frame(val = OD_sub_i[[i]]) %>%
    mutate(., sub = subjects[i]) %>%
    mutate(., atlas = "Thal2Medulla")
  
  logind <- atlas_Thal2Palli@.Data == 1
  tmp <- OD@.Data[logind]
  OD_sub_i[[i]] <- mean(tmp)#[tmp > 0]
  df_sub_i_Thal2Palli[[i]] <- data_frame(val = OD_sub_i[[i]]) %>%
    mutate(., sub = subjects[i]) %>%
    mutate(., atlas = "Thal2Palli")
  
  logind <- atlas_DLPFC2rvmPFC@.Data == 1
  tmp <- OD@.Data[logind]
  OD_sub_i[[i]] <- mean(tmp)#[tmp > 0]
  df_sub_i_DLPFC2rvmPFC[[i]] <- data_frame(val = OD_sub_i[[i]]) %>%
    mutate(., sub = subjects[i]) %>%
    mutate(., atlas = "DLPFC2rvmPFC")
  
  logind <- atlas_rAngG2rDLPFC@.Data == 1
  tmp <- OD@.Data[logind]
  OD_sub_i[[i]] <- mean(tmp)#[tmp > 0]
  df_sub_i_rAngG2rDLPFC[[i]] <- data_frame(val = OD_sub_i[[i]]) %>%
    mutate(., sub = subjects[i]) %>%
    mutate(., atlas = "rAngG2rDLPFC")
  
  logind <- atlas_vmPFC2DLPFC@.Data == 1
  tmp <- OD@.Data[logind]
  OD_sub_i[[i]] <- mean(tmp)#[tmp > 0]
  df_sub_i_vmPFC2DLPFC[[i]] <- data_frame(val = OD_sub_i[[i]]) %>%
    mutate(., sub = subjects[i]) %>%
    mutate(., atlas = "vmPFC2DLPFC")
  
}

color_col <- c("coral", "cadetblue","burlywood","skyblue","bisque","beige")#,"azure","aquamarine","antiquewhite","aliceblue","dodgerblue","dimgray","deeppink","darkviolet","darkturquoise","darkslategrey","darkslateblue","darkseagreen","darksalmon","darkred","darkorchid","darkorange","darkolivegreen","coral", "cadetblue","burlywood","skyblue","bisque","beige","azure","aquamarine","antiquewhite","aliceblue","dodgerblue","dimgray","deeppink","darkviolet","darkturquoise","darkslategrey","darkslateblue","darkseagreen","darksalmon","darkred","darkorchid","darkorange","darkolivegreen","coral", "cadetblue")

df_Caud2Medulla <- do.call(rbind.data.frame, df_sub_i_Caud2Medulla)
df_Caud2Palli <- do.call(rbind.data.frame, df_sub_i_Caud2Palli)
df_DLPFC2Caud <- do.call(rbind.data.frame, df_sub_i_DLPFC2Caud)
df_DLPFC2Thal <- do.call(rbind.data.frame, df_sub_i_DLPFC2Thal)
df_Thal2Medulla <- do.call(rbind.data.frame, df_sub_i_Thal2Medulla)
df_Thal2Palli <- do.call(rbind.data.frame, df_sub_i_Thal2Palli)

df_DLPFC2rvmPFC <- do.call(rbind.data.frame, df_sub_i_DLPFC2rvmPFC)
df_rAngG2rDLPFC <- do.call(rbind.data.frame, df_sub_i_rAngG2rDLPFC)
df_vmPFC2DLPFC <- do.call(rbind.data.frame, df_sub_i_vmPFC2DLPFC)

subs_df <- rbind(df_Caud2Medulla,df_Caud2Palli,df_DLPFC2Caud,
                 df_DLPFC2Thal,df_Thal2Medulla,df_Thal2Palli,
                 df_DLPFC2rvmPFC,df_rAngG2rDLPFC,df_vmPFC2DLPFC)
saveRDS(subs_df,file = "C:/Users/vissa/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_all/ROIs_new/OD/meanODinROIs_new.RDS")


subs_df <- readRDS("C:/Users/vissa/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_all/ROIs_new/OD/meanODinROIs_new.RDS")
participants_groups <- read_tsv("C:/Users/vissa/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_all/participants.tsv")
colnames(subs_df) <- c("val","participant_id","atlas")
subs_df <- merge(subs_df, participants_groups)
subs_df$group[subs_df$group==1] <- "sham"
subs_df$group[subs_df$group==2] <- "active"
subs_df <- mutate(subs_df,group=factor(group,levels = c("sham","active")))

plots_violin <- ggplot(subs_df, aes(fill=group,x=atlas,y=val)) + 
  #geom_violin(trim=FALSE) +
  labs(x="",y = "Orientation dispersion (OD)")+
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_point(pch = 21,size = 1, position = position_jitterdodge()) +
  #geom_jitter(aes(colour = factor(cyl))color="black", size=0.85, alpha=1,width=.2) +
  #theme_ipsum() +
  #theme(
  #  legend.position="none",
  #  plot.title = element_text(size=11)
  #) +
  #geom_point(position=position_dodge(width=0.75))+
  #geom_jitter() +
  ggtitle("Orientation dispersion (OD): sham and active groups at 0 weeks") +
  theme(text = element_text(size = 20),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ylim(0.1, 0.6) +
  stat_summary(fun.y=mean, geom="point", aes(group=group), position=position_dodge(.75), 
               color="black", size=3, shape=18) +
  facet_wrap(~atlas, scale="free")
plots_violin
ggsave(plots_violin, file='C:/Users/vissa/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_all/ROIs_new/OD/OD_WMregions_shamVSactive.png', width = 1366/72, height = 768/72, dpi = 72)

t_tests_list <- list()
p_values <- c()
mean1 <- c()
mean2 <- c()
std1 <- c()
std2 <- c()
#delta_means <- c()
i=1
for(k in sort(unique(subs_df$atlas))) {
  group1 <- filter(subs_df,atlas==k,group=="sham")
  group2 <- filter(subs_df,atlas==k,group=="active")
  t_tests_list[[i]] <- t.test(group1$val, group2$val)
  mean1[i] <- t_tests_list[[i]]$estimate[[1]]
  mean2[i] <- t_tests_list[[i]]$estimate[[2]]
  std1[i] <- sd(group1$val)
  std2[i] <- sd(group2$val)
  p_values[i] <- t_tests_list[[i]]$p.value
  #delta_means[i] <- mean(t_tests_list[[i]]$conf.int[1:2])
  i=i+1
}
p_values_rd <- round(p_values,4)
t_tests_tib <- dplyr::tibble("WM region" = sort(unique(subs_df$atlas)),
                             #"Mean difference" = round(delta_means,3),
                             sham = paste(round(mean1,2),"±",round(std1,2)),
                             active = paste(round(mean2,2),"±",round(std2,2)),
                             "p-value" = p_values_rd)
gt_tbl <- gt(data = t_tests_tib, auto_align = FALSE) %>%
  tab_header("Orientation dispersion (OD): sham and active groups at 0 weeks")
gt_tbl <- cols_align(gt_tbl,
           align = "center",
           columns = "WM region")
gt_tbl


gt_tbl %>%
  gtsave("OD_t_tests.html", inline_css = TRUE,
         path = 'C:/Users/vissa/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_all/ROIs_new/OD/'
  )
