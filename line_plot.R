library(ggplot2)


HFD <- read.delim("E:/test/yqing/HFD.txt", header=T)
CHOW <- read.delim("E:/test/yqing/CHOW.txt", header=T)
df1 <- t(HFD)
ave=list()
SD=list()
group=list()
for (i in c(1:length(row.names(df1)))){
  ave=append(ave,mean(df1[i,]))
  SD=append(SD,sd(df1[i,])/sqrt(30))
  group=append(group,"HFD")
}

df2 <- t(CHOW)
for (i in c(1:length(row.names(df2)))){
  ave=append(ave,mean(df2[i,]))
  SD=append(SD,sd(df2[i,])/sqrt(5))
  group=append(group,"CHOW")
}
time=append(row.names(df1),row.names(df2))
time1=factor(c(time),row.names(df1))
HFD1=data.frame(time=time1,aver=c(unlist((ave))),SD=c(unlist(SD)),group=c(unlist(group)))
#names(HFD1) <- c("time",'aver','sd')

ggplot(data=HFD1,aes(x=time,y=aver,group=group,color=group,shape=group))+geom_line(size=1)+geom_point(size=2)+
  geom_errorbar(aes(ymin=aver-SD,ymax=aver+SD),width=0.25)+
  theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "black"),
        axis.title = element_text(size=14),axis.text = element_text(size = 14))+
  labs(x = "Time", y = "Weight(g)", title)
