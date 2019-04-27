
tri.gradient <- function(n=100, x1=0, x2=1, y1=0, y2=1, col.begin="navy",col.mid=NULL,col.end="darkred",
                         title="", mai=par()$mai, add=FALSE, vertical=FALSE, ...){
  if(is.null(col.mid)){
    ramp <- colorRamp(c(col.begin,col.end));
  } else {
    ramp <- colorRamp(c(col.begin,col.mid,col.end));
  }
  mycol <- rgb( ramp(seq(0, 1, length = n)), max = 255);
  x.step <- (x2-x1) / (n);
  y.step <- (y2-y1) / (n);
  xi1 <- x1+x.step;
  yi1 <- y1+y.step;
  xi2 <- xi1;
  yi2 <- yi1;
  if(vertical){
    x <- c(x1,xi1,x1);
    y <- c(y1,yi1,yi1);
  } else {
    x <- c(x1,xi1,xi1);
    y <- c(y1,yi1,y1);
  }
  if(! add) plot(0, type="n", xaxt="n", yaxt="n", bty="n", xlim=c(x1,x2), ylim=c(y1,y2), xlab="", ylab="", main=title);
  polygon(x,y,col=mycol[1], border=mycol[1], ...);
  for(i in 2:n){
    xi1 <- xi2; xi2 <- xi1+x.step;
    yi1 <- yi2; yi2 <- yi1+y.step;
    if(vertical){
      x <- c(x1,xi1,xi2,x1);
      y <- c(yi1,yi1,yi2,yi2);
    } else {
      x <- c(xi1,xi1,xi2,xi2);
      y <- c(y1,yi1,yi2,y1);
    }
    polygon(x, y, col=mycol[i], border=mycol[i], ...);
  }
}

ramp <- colorRamp(c("white", "darkred"))

mycol <- rgb( ramp(seq(0, 1, length = 50)), max = 255);
tiff(file="/home/galaxy/lee/BR_rnaseq/output/1/barplot_S1S2common_down_bp1.tiff")
par(mai=c(0.8,4.1,0.0,0.15),mgp=c(3,0.6,0));
y=barplot(all.S2.down.bp[14:1,3],col=mycol[-log10(all.S2.down.bp[14:1,5])*5],horiz=T,names.arg=rep("",14),xlab="",cex.axis=1.5)
text(x=4,y=-2.3,labels="Number of genes",xpd=T,cex=1.5);
text(-0.2,y,labels=all.S2.down.bp[14:1,6],srt=0,adj=1,xpd=T,cex=1.25);
tri.gradient(x1=36.95,x2=39.7,y1=3,y2=9,add=T,vertical=T,col.begin="white",col.end="darkred",xpd=T);
axis(2,at=seq(3,9,1.2),line=-9.0,labels=seq(0,10,2),cex.axis=1.25);
text(38,6,labels="-log10(p-value)",srt=90,adj=0.5,cex=1.5,xpd=T);
dev.off();

ramp <- colorRamp(c("white", "darkred"))
mycol <- rgb( ramp(seq(0, 1, length = 50)), max = 255);
tiff(file="/home/galaxy/lee/BR_rnaseq/output/1/barplot_S1S2common_down_bp.tiff")
par(mai=c(0.8,3.8,0.0,0.3),mgp=c(3,0.6,0));
y=barplot(all.S1S2common.down.bp[15:1,3],col=mycol[-log10(all.S1S2common.down.bp[15:1,5])*5],horiz=T,names.arg=rep("",15),xlab="",cex.axis=1.25,xlim=c(0,34),xaxt="n");
axis(1,at=c(seq(0,25,5),29,34),labels=rep("",8));
mtext(c(seq(0,25,5),105,110), side=1, at=c(seq(0,25,5),29.2,34.7),cex=1.25,line=1);
polygon(c(26.3,27.7,27.7,26.3),c(12.1,12.1,13.3,13.3),col="white",border=NA);
polygon(c(26.3,27.7,27.7,26.3),c(-.8,-.8,.2,.2),col="white",border=NA,xpd=T);
segments(c(26.3,27.7),c(-0.1,-0.1),c(26.3,27.7),c(-0.9,-0.9),lwd=2,xpd=T);
text(x=17,y=-2.5,labels="Number of genes",xpd=T,cex=1.5);
text(-2,y,labels=all.S1S2common.down.bp[15:1,6],srt=0,adj=1,xpd=T,cex=1.25);
tri.gradient(x1=33.5,x2=36.5,y1=3,y2=9,add=T,vertical=T,col.begin="white",col.end="darkred",xpd=T);
axis(2,at=seq(3,9,1.2),line=-12.7,labels=seq(0,10,2),cex.axis=1.25);
text(26.9,6,labels="-log10(p-value)",srt=90,adj=0.5,cex=1.5,xpd=T);
dev.off()
