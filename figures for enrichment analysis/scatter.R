library(ggplot2)

TF <- factor(final$TF[1:15],final$TF[15:1])
df1 <- data.frame('TF'= TF,"num"=c(15:1),"p"=-log10(final$p[1:15]),"fdr"=final$fdr[1:15],'count'=final$Num.of.genes[1:15])

ggplot(df1,aes(TF,p,size=count))+geom_point(aes(colour=fdr))+scale_colour_gradient(low = 'red', high = 'darkred')+
  theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(),axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size=14),axis.text = element_text(size = 14))+coord_flip()+labs(x="",y="-log10(p)")

