library(pheatmap)

options(stringsAsFactors = FALSE)
specific_gene <- read.table("E:/R_WGCNA_test/GTEx_Analysis_2017-06-05_v8_RNASeQCv1.1.9_gene_median_tpm.gct/step5_optimize/specific_gene.txt", quote="\"", comment.char="")
gene_T <- read.delim("E:/R_WGCNA_test/GTEx_Analysis_2017-06-05_v8_RNASeQCv1.1.9_gene_median_tpm.gct/GTEx_Analysis_2017-06-05_v8_RNASeQCv1.1.9_gene_median_tpm.gct")
name <- unlist(gene_T$Name)
list1=list()
for (i in c(1:length(name))){
  name1 <- strsplit(name[i],split = ".",fixed=T)
  list1 <- append(list1,name1[[1]][1])
}
df1 <- data.frame("name1"=unlist(list1),gene_T)
df <- df1[,c(1,4:57)]
names(specific_gene) <- "name1"
df1 <- merge(specific_gene,df,by="name1")
df <- df1[order(df1$Pancreas,decreasing = T),]
rownames(df) <- df$name1
df <- df[,2:55]

df_f <- log(df+0.01)
pheatmap(df_f,cluster_row = F,border=F,cutree_col = 2,display_numbers = ifelse(df_f > 2, "*", ""))

         