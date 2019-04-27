library(Vennerable)
library(dplyr)
gene_anno <- read.table("/home/galaxy/lee/database/protein/gene_annotation",sep="\t",header = T,quote="")

file_name <- "/home/galaxy/lee/wanghongliang/mute/mutil_omics/trans_proto"

MC <- read.table("~/lee/wanghongliang/mute/ESs/overlap/MC.txt", quote="\"", comment.char="")$V1
GC <- read.table("~/lee/wanghongliang/mute/ESs/overlap/GC.txt", quote="\"", comment.char="")$V1
up <- read.delim("/home/galaxy/lee/wanghongliang/mute/20190124/table/XVE.ES_vs_XVE_ES/up.xls")$Gene
down <- read.delim("/home/galaxy/lee/wanghongliang/mute/20190124/table/XVE.ES_vs_XVE_ES/down.xls")$Gene
genes <- read.table("~/lee/wanghongliang/mute/ESs/overlap/genes_express_in_stomata.txt", quote="\"", comment.char="")$V1

Venn1 <- Venn(list("MC"=MC, "down"=down))

tiff(file=paste0(file_name,".tiff"))
plot(Venn1,
     show=list(Universe=F,setLabels.fontsize=36,set3.FaceText.fontsize=25,
               setLabels.x.add=c(0,0),setLabels.y.add=c(0,0)))
dev.off()

venn2 <- Venn1@IntersectionSets
df_total <- data.frame()
for (list1 in venn2){
  if (length(list1)){
    df1 <- data.frame("gene" =list1)
    merge_data <- merge(df1,gene_anno,by="gene",all.x =T)
    col1 <- paste(merge_data$gene,merge_data$annotation)
    col1 <- data.frame("num"=1:length(list1),col1)
    colnames(col1)=c("num",length(list1))
    if (length(df_total)==0){
      df_total <- col1
    }
    else {
      df_total <- dplyr::full_join(df_total,col1,by="num")
    }
  }
}
df_total <- df_total[,-1]
write.table(df_total,paste0(file_name,"1.xls"),sep="\t",quote=F,row.names = F)
