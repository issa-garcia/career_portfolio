library(oro.nifti)
library(tidyverse)
library(ggpubr)
library(gt)

sub_001_atlas <- readNIfTI('C:/Users/Victor/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/sub-001_ses-t0_space-individual_desc-icbmdti81INfsorig_dwi.nii.gz', reorient = FALSE)
sub_001_ICVF <- readNIfTI('C:/Users/Victor/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/AMICO_sub-001/AMICO/NODDI/FIT_ICVF.nii.gz', reorient = FALSE)

sub_005_atlas <- readNIfTI('C:/Users/Victor/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/sub-005_ses-t0_space-individual_desc-icbmdti81INfsorig_dwi.nii.gz', reorient = FALSE)
sub_005_ICVF <- readNIfTI('C:/Users/Victor/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/AMICO_sub-005/AMICO/NODDI/FIT_ICVF.nii.gz', reorient = FALSE)

atlas_labels <- read.delim('C:/Users/Victor/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/JHU-labels.txt',sep=",",header=FALSE)


color_col <- c("coral", "cadetblue","burlywood","skyblue","bisque","beige","azure","aquamarine","antiquewhite","aliceblue","dodgerblue","dimgray","deeppink","darkviolet","darkturquoise","darkslategrey","darkslateblue","darkseagreen","darksalmon","darkred","darkorchid","darkorange","darkolivegreen","coral", "cadetblue","burlywood","skyblue","bisque","beige","azure","aquamarine","antiquewhite","aliceblue","dodgerblue","dimgray","deeppink","darkviolet","darkturquoise","darkslategrey","darkslateblue","darkseagreen","darksalmon","darkred","darkorchid","darkorange","darkolivegreen","coral", "cadetblue")
sub_001_ICVF_list <- list()
sub_005_ICVF_list <- list()
plots_violin <- list()
for(i in 1:48) {
  sub_001_logind <- sub_001_atlas@.Data == i
  sub_001_tmp <- sub_001_ICVF@.Data[sub_001_logind]
  sub_001_ICVF_list[[i]] <- sub_001_tmp[sub_001_tmp > 0]
  sub_001_df <- data_frame(val = sub_001_ICVF_list[[i]]) %>% mutate(., sub = "sub-001")
  
  sub_005_logind <- sub_005_atlas@.Data == i
  sub_005_tmp <- sub_005_ICVF@.Data[sub_005_logind]
  sub_005_ICVF_list[[i]] <- sub_005_tmp[sub_005_tmp > 0]
  sub_005_df <- data_frame(val = sub_005_ICVF_list[[i]]) %>% mutate(., sub = "sub-005")
  
  subs_df <- rbind(sub_001_df,sub_005_df)
  plots_violin[[i]] <- ggplot(subs_df, aes(x=sub,y=val)) + 
    geom_violin(trim=FALSE, fill=color_col[i]) +
    labs(x="",y = "Neurite density (ICVF)")+
    geom_boxplot(width=0.1, fill="white")+
    theme_classic()+
    ggtitle(atlas_labels$V2[i+1])
  #print(plots_violin[[i]])
}
t_tests_list <- list()
p_values <- c()
delta_means <- c()
for(k in 1:47) {
  t_tests_list[[k]] <- t.test(sub_001_ICVF_list[[k]], sub_005_ICVF_list[[k]])
  p_values[k] <- t_tests_list[[k]]$p.value
  delta_means[k] <- mean(t_tests_list[[k]]$conf.int[1:2])
}
p_values_rd <- round(p_values,4)
p_values_rd[p_values_rd < 0.0001] <- "< 0.0001"
t_tests_tib <- dplyr::tibble(index = 1:48,
                             "WM region: Neurite density (ICVF) between sub-001 and sub-005" = atlas_labels$V2[2:49],
                             "Mean difference" = c(round(delta_means,3),'-'),
                             "p-value" = c(p_values_rd,'-'))
gt_tbl <- gt(data = t_tests_tib, auto_align = FALSE)
gt_tbl <- cols_align(gt_tbl,
           align = "left",
           columns = "WM region: Neurite density (ICVF) between sub-001 and sub-005")
gt_tbl


gt_tbl %>%
  gtsave("ICVF_t_tests.html", inline_css = TRUE,
         path = 'C:/Users/Victor/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_sub1VSsub5/ICVF/'
  )

fifth1 <- ggarrange(plots_violin[[1]],
          plots_violin[[2]],
          plots_violin[[3]],
          plots_violin[[4]],
          plots_violin[[5]],
          plots_violin[[6]],
          plots_violin[[7]],
          plots_violin[[8]],
          plots_violin[[9]],
          plots_violin[[10]],
          ncol = 5,nrow = 2)
ggsave(fifth1, file='C:/Users/Victor/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_sub1VSsub5/ICVF/ICVF_WMregions1_10.png', width = 1366/72, height = 768/72, dpi = 72)

fifth2 <- ggarrange(plots_violin[[11]],
          plots_violin[[12]],
          plots_violin[[13]],
          plots_violin[[14]],
          plots_violin[[15]],
          plots_violin[[16]],
          plots_violin[[17]],
          plots_violin[[18]],
          plots_violin[[19]],
          plots_violin[[20]],
          ncol = 5,nrow = 2)
ggsave(fifth2, file='C:/Users/Victor/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_sub1VSsub5/ICVF/ICVF_WMregions11_20.png', width = 1366/72, height = 768/72, dpi = 72)

fifth3 <- ggarrange(plots_violin[[21]],
          plots_violin[[22]],
          plots_violin[[23]],
          plots_violin[[24]],
          plots_violin[[25]],
          plots_violin[[26]],
          plots_violin[[27]],
          plots_violin[[28]],
          plots_violin[[29]],
          plots_violin[[30]],
          ncol = 5,nrow = 2)
ggsave(fifth3, file='C:/Users/Victor/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_sub1VSsub5/ICVF/ICVF_WMregions21_30.png', width = 1366/72, height = 768/72, dpi = 72)

fifth4 <- ggarrange(plots_violin[[31]],
          plots_violin[[32]],
          plots_violin[[33]],
          plots_violin[[34]],
          plots_violin[[35]],
          plots_violin[[36]],
          plots_violin[[37]],
          plots_violin[[38]],
          plots_violin[[39]],
          plots_violin[[40]],
          ncol = 5,nrow = 2)
ggsave(fifth4, file='C:/Users/Victor/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_sub1VSsub5/ICVF/ICVF_WMregions31_40.png', width = 1366/72, height = 768/72, dpi = 72)

fifth5 <- ggarrange(plots_violin[[41]],
          plots_violin[[42]],
          plots_violin[[43]],
          plots_violin[[44]],
          plots_violin[[45]],
          plots_violin[[46]],
          plots_violin[[47]],
          plots_violin[[48]],
          ncol = 4,nrow = 2)
ggsave(fifth5, file='C:/Users/Victor/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_sub1VSsub5/ICVF/ICVF_WMregions41_48.png', width = 1366/72, height = 768/72, dpi = 72)

