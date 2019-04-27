library(corrplot)
library(ggplot2)
library(pheatmap)
data <- pro_R[,1:17]
b <- cor(data)
corrplot(b,method = "color",col=col(100))
col =colorRampPalette(c("navy","white","firebrick3"))
corrplot(b,type="upper",col=col(10),tl.pos = "d")
corrplot(b,add=TRUE,type = "lower",method = "number",diag = FALSE,tl.pos = "n",col=col(10))


