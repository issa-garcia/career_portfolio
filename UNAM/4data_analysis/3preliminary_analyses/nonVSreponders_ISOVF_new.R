library(tidyverse)
library(ggpubr)
library(gt)
library(hrbrthemes)
library(viridis)
library(pivottabler)
library(normwhn.test)


subs_df <- readRDS("C:/Users/vissa/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_all/ROIs_new/ISOVF/meanISOVFinROIs_new.RDS")
VAS <- read_csv("C:/Users/vissa/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_all/Addimex_clinical_Feb20/csv/VAS.csv")
CCQ_N <- read_csv("C:/Users/vissa/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_all/Addimex_clinical_Feb20/csv/CCQ-N.csv")
Inventory <- read_csv("C:/Users/vissa/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_all/Addimex_clinical_Feb20/csv/Inventory.csv")
BIS <- read_csv("C:/Users/vissa/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_all/Addimex_clinical_Feb20/csv/BIS-11.csv")


colnames(subs_df) <- c("val","rid","atlas")
subs_ids <- data.frame(do.call('rbind', strsplit(as.character(subs_df$rid),'sub-',fixed=TRUE)))
subs_df$rid <- as.numeric(subs_ids$X2)

VAS_t0t1 <- filter(VAS,(stage=='T0'|stage=='T1'))
CCQ_N_t0t1 <- filter(CCQ_N,(stage=='T0'|stage=='T1'))
Inventory_t0t1 <- filter(Inventory,(stage=='T0'|stage=='T1'))
BIS_t0t1 <- filter(BIS,(stage=='T0'|stage=='T1'))

qhpvt(bhmtrains, "TOC", "TrainCategory", "n()")

create_pivot_table <- function(table_name,score_name){
  pt <- PivotTable$new()
  pt$addData(table_name)
  pt$addColumnDataGroups("stage")
  pt$addRowDataGroups("rid")
  pt$defineCalculation(calculationName=score_name,  summariseExpression=paste("sum(",score_name,")",sep = ""))
  pt$renderPivot()
  table_VAS_t0t1 <- pt$asDataFrame()
  table_VAS_t0t1 <- mutate(table_VAS_t0t1, rid = rownames(table_VAS_t0t1))
  table_VAS_t0t1 <- subset(table_VAS_t0t1,select = -c(Total))
  table_VAS_t0t1 <- table_VAS_t0t1[!(row.names(table_VAS_t0t1) %in% "Total"), ]
  return(table_VAS_t0t1)
}

table_BIStot_t0t1 <- create_pivot_table(BIS_t0t1,"tot_score")
table_VAS_t0t1 <- create_pivot_table(VAS_t0t1,"vas")
table_CCQ_N_t0t1 <- create_pivot_table(CCQ_N_t0t1,"ccq_n")


demographics <- read_csv("C:/Users/vissa/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_all/Addimex_clinical_Feb20/csv/DEMOGRAPHIC.csv")
table_VAS_t0t1 <- mutate(table_VAS_t0t1,delta_T = T1-T0)
table_VAS_t0t1 <- mutate(table_VAS_t0t1,delta_Tper = delta_T/T0*100)
table_VAS_t0t1 <- mutate(table_VAS_t0t1,responderVAS = delta_Tper<(-50))

table_CCQ_N_t0t1 <- mutate(table_CCQ_N_t0t1,delta_T = T1-T0)
table_CCQ_N_t0t1 <- mutate(table_CCQ_N_t0t1,delta_Tper = delta_T/T0*100)
table_CCQ_N_t0t1 <- mutate(table_CCQ_N_t0t1,responderCCQ = delta_Tper<(-50))

table_BIStot_t0t1 <- mutate(table_BIStot_t0t1,delta_T = T1-T0)
table_BIStot_t0t1 <- mutate(table_BIStot_t0t1,delta_Tper = delta_T/T0*100)
table_BIStot_t0t1 <- mutate(table_BIStot_t0t1,responderBIS = delta_Tper<(-50))

subs_df <- merge(subs_df,demographics,by = "rid")
subs_df$group[subs_df$group==1] <- "sham"
subs_df$group[subs_df$group==2] <- "active"
subs_df <- mutate(subs_df,rTMS=factor(group,levels=c("sham","active")))


subs_df_VAS <- merge(subs_df,table_VAS_t0t1,by = "rid")
plots_scatter <- subs_df_VAS %>%
  filter(delta_T != 'NA') %>%
  ggplot(aes(x=val,y=delta_T,fill=rTMS)) + 
  #geom_violin(trim=FALSE) +
  labs(x="CSF volume fraction (ISOVF) at 0 weeks",y = "Change in craving VAS after rTMS")+
  geom_point(shape = 21,stroke = 1,colour = "black",aes(colour = rTMS))+
  geom_smooth(method = lm,aes(colour = rTMS)) +
  xlim(0,0.6) +
  ylim(-10,10) +
  stat_cor(method = "pearson") +
  scale_fill_manual(values=c("skyblue","red")) +
  scale_color_manual(values=c("skyblue","red"))+
  ggtitle("") +
  theme(text = element_text(size = 20)) +
  facet_wrap(~atlas, scale="free")
plots_scatter
ggsave(plots_scatter, file='C:/Users/vissa/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_all/ROIs_new/ISOVF/corr_predict_response/ISOVF_predict_deltaresponse_VAS.png', width = 1366/72, height = 768/72, dpi = 72)


subs_df_CCQ_N <- merge(subs_df,table_CCQ_N_t0t1,by = "rid")
plots_scatter <- subs_df_CCQ_N %>%
  filter(delta_T != 'NA') %>%
  ggplot(aes(x=val,y=delta_T,fill=rTMS)) + 
  #geom_violin(trim=FALSE) +
  labs(x="CSF volume fraction (ISOVF) at 0 weeks",y = "Change in craving CCQnow after rTMS")+
  geom_point(shape = 21,stroke = 1,colour = "black",aes(colour = rTMS))+
  geom_smooth(method = lm,aes(colour = rTMS)) +
  xlim(0,0.6) +
  ylim(-125,100) +
  stat_cor(method = "pearson") +
  scale_fill_manual(values=c("skyblue","red")) +
  scale_color_manual(values=c("skyblue","red"))+
  ggtitle("") +
  theme(text = element_text(size = 20)) +
  facet_wrap(~atlas, scale="free")
plots_scatter
ggsave(plots_scatter, file='C:/Users/vissa/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_all/ROIs_new/ISOVF/corr_predict_response/ISOVF_predict_deltaresponse_CCQnow.png', width = 1366/72, height = 768/72, dpi = 72)

subs_df_BIS <- merge(subs_df,table_BIStot_t0t1,by = "rid")
plots_scatter <- subs_df_BIS %>%
  filter(delta_T != 'NA') %>%
  ggplot(aes(x=val,y=delta_T,fill=rTMS)) + 
  #geom_violin(trim=FALSE) +
  labs(x="CSF volume fraction (ISOVF) at 0 weeks",y = "Change in total BIS11 after rTMS")+
  geom_point(shape = 21,stroke = 1,colour = "black",aes(colour = rTMS))+
  geom_smooth(method = lm,aes(colour = rTMS)) +
  xlim(0,0.6) +
  ylim(-50,30) +
  stat_cor(method = "pearson") +
  scale_fill_manual(values=c("skyblue","red")) +
  scale_color_manual(values=c("skyblue","red"))+
  ggtitle("") +
  theme(text = element_text(size = 20)) +
  facet_wrap(~atlas, scale="free")
plots_scatter
ggsave(plots_scatter, file='C:/Users/vissa/Documents/Universitaeten/TEC/Clínicas/2021/UNAM/TMS/postprocess_HARDI/NODDI_all/ROIs_new/ISOVF/corr_predict_response/ISOVF_predict_deltaresponse_BIStot.png', width = 1366/72, height = 768/72, dpi = 72)
