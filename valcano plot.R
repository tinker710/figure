library(ggplot2)
options(stringsAsFactors = F)

dat_all <- read.csv("E:/TCGA/all_results.csv")

deg <- data.frame( "name"=dat_all$name ,"log2FC"=dat_all$log2FoldChange,"padj"=dat_all$padj )
deg <- deg[complete.cases(deg),]
deg[which(deg$padj < 0.01 & deg$log2FC >= 1), "diff"] <- "up"
deg[which(deg$padj < 0.01 & deg$log2FC <= -1), "diff"] <- "down"
deg[!(deg$diff %in% c("up",'down')), "diff"] <- "no"


p1 <- ggplot(deg, aes(x=log2FC,y=-log10(padj)))+
  geom_point(aes(color=diff),size=1,alpha=0.6) +
  scale_colour_manual(limits = c('up', 'down', 'no'), values = c("#0072B5","#BC3C28","grey"), labels = c('up', 'down', 'no')) +
  labs(x = 'log2FC', y = '-log10 padj')+
  geom_vline(xintercept = c(-1, 1), color = 'gray', linetype = 2, size = 0.8) + 
  geom_hline(yintercept = -log10(0.01), color = 'gray', linetype = 2, size = 0.8)+
  theme(panel.background = element_rect(color = 'black', fill = 'transparent')) +
  xlim(-4,8)+theme(legend.title = element_blank(), legend.key = element_rect(fill = 'transparent'), legend.background = element_rect(fill = 'transparent'), legend.position = c(0.3, 0.9))
 

  
  


