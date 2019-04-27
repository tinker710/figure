library(clusterProfiler)
library(org.At.tair.db)

path <- ""    #modified
file_name <- "yeast.txt"   #modified
file1 <- read.table(paste0(path,file_name), quote="\"", comment.char="")
genes <- file1$V1
ego <- enrichGO(gene = genes, 
                   OrgDb = org.At.tair.db, 
                   keyType = 'TAIR', pAdjustMethod = "BH", ont = "CC", 
                   pvalueCutoff  = 0.05, qvalueCutoff  = 0.05)

ekp <- enrichKEGG(gene= genes, keyType = "kegg", 
                       organism = 'ath', pvalueCutoff = 0.05)

pdf(paste0(path,"go.pdf"))
barplot(ego,showCategory = 15)
dev.off()

pdf(paste0(path,"kegg.pdf"))
dotplot(ekp)
dev.off()

write.table(ekp@result, paste0(path,"kegg.xls"),quote = F,row.names = F,sep = "\t")
write.table(ego@result, paste0(path,"go.xls"),quote = F,row.names = F,sep = "\t")
