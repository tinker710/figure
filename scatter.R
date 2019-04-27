library(corrplot)
library(ggplot2)
library(pheatmap)
data <- pro_R[,1:17]


a <- pro_R[,18:19]
a1 <- log2(a)

ggplot(log2,aes(R_FC,P_FC,colour=Loc))+geom_point(alpha=0.8, size = 1)+
  geom_hline(yintercept = 0,lty=4,lwd=0.6,alpha=0.8)+
  geom_vline(xintercept = 0,lty=4,lwd=0.6,alpha=0.8)+
  xlim(-8,8)+theme_bw(base_size = 18)+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())


