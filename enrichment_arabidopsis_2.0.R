library(clusterProfiler)
library(org.At.tair.db)
options(stringsAsFactors = F)

path <- "E:/test/wanghongliang"    #modified
deg <- read.delim("E:/test/wanghongliang/deg.txt")
keyword <- "GCP_6h_vs_MCP_6h"
if (!dir.exists(paste0(path,paste0("/",keyword)))){
  dir.create(paste0(path,paste0("/",keyword)))
}
setwd(paste0(path,paste0("/",keyword)))
FDR <- which(names(deg) == paste0(keyword,"_FDR"))
FC <- which(names(deg) == paste0(keyword,"_logFC"))
data1 <- deg[which(deg[,FDR] < 0.05),]
data2_up <- data1[which(data1[,FC] > 1),]
data2_down <- data1[which(data1[,FC] < -1),]

genes_up <- data2_up$Gene
genes_down <- data2_down$Gene

#up
ego_up <- enrichGO(gene = genes_up, 
                OrgDb = org.At.tair.db, 
                keyType = 'TAIR', pAdjustMethod = "BH", ont = "CC", 
                pvalueCutoff  = 0.05, qvalueCutoff  = 0.05)

ekp_up <- enrichKEGG(gene= genes_up, keyType = "kegg", 
                  organism = 'ath', pvalueCutoff = 0.05)

pdf("go_up.pdf")
barplot(ego_up,showCategory = 15)
dev.off()

pdf("kegg_up.pdf")
dotplot(ekp_up)
dev.off()

write.table(ekp_up@result, "kegg_up.xls",quote = F,row.names = F,sep = "\t")
write.table(ego_up@result, "go_up.xls",quote = F,row.names = F,sep = "\t")

#down
ego_down <- enrichGO(gene = genes_down, 
                   OrgDb = org.At.tair.db, 
                   keyType = 'TAIR', pAdjustMethod = "BH", ont = "CC", 
                   pvalueCutoff  = 0.05, qvalueCutoff  = 0.05)

ekp_down <- enrichKEGG(gene= genes_down, keyType = "kegg", 
                     organism = 'ath', pvalueCutoff = 0.05)

pdf("go_down.pdf")
barplot(ego_down,showCategory = 15)
dev.off()

pdf("kegg_down.pdf")
dotplot(ekp_down)
dev.off()

write.table(ekp_down@result, "kegg_down.xls",quote = F,row.names = F,sep = "\t")
write.table(ego_down@result, "go_down.xls",quote = F,row.names = F,sep = "\t")

