#This script creates correlation heatmaps for eNFTR flows, FTR flows and 
#BS positions for each sector.

#First, we need to load the required packages. INSTALL FIRST.
library(ggplot2)
library(reshape2)
library(scales)

#load your tfm time series datasets
load("data-eurostat/d_fbs_cb.RData")
load("data-eurostat/d_ftr_cb.RData")
load("data-eurostat/d_nftr.RData")

#This for loop creates a time series for every asset we have in our balance sheet
# dataset for two periods, 2002Q1-2007Q4 and 2008Q1-2013Q4
fbsrows<-nrow(d_fbs)
for(i in 1:fbsrows){
name_a<-paste(d_fbs[i,]$indic_na,d_fbs[i,]$sector,d_fbs[i,]$finpos,"a",sep="_")
name_b<-paste(d_fbs[i,]$indic_na,d_fbs[i,]$sector,d_fbs[i,]$finpos,"b",sep="_")

x<-as.numeric(d_fbs[i,1:24])
y<-as.numeric(d_fbs[i,25:48])

x<-ts(x, start=c(2002,1),end=c(2007,4),freq=4)
y<-ts(y, start=c(2008,1),freq=4)

assign(name_a, x)
assign(name_b, y)
}

HH_ASSETS_a<-grep("S14_S15_ASS_a",ls(),value=T)
FC_ASSETS_a<-grep("S12_ASS_a",ls(),value=T)
NFC_ASSETS_a<-grep("S11_ASS_a",ls(),value=T)
GG_ASSETS_a<-grep("S13_ASS_a",ls(),value=T)
ROW_ASSETS_a<-grep("S2_ASS_a",ls(),value=T)

HH_ASSETS_b<-grep("S14_S15_ASS_b",ls(),value=T)
FC_ASSETS_b<-grep("S12_ASS_b",ls(),value=T)
NFC_ASSETS_b<-grep("S11_ASS_b",ls(),value=T)
GG_ASSETS_b<-grep("S13_ASS_b",ls(),value=T)
ROW_ASSETS_b<-grep("S2_ASS_b",ls(),value=T)

HH_LIAB_a<-grep("S14_S15_LIAB_a",ls(),value=T)
FC_LIAB_a<-grep("S12_LIAB_a",ls(),value=T)
NFC_LIAB_a<-grep("S11_ASS_a",ls(),value=T)
GG_LIAB_a<-grep("S13_LIAB_a",ls(),value=T)
ROW_LIAB_a<-grep("S2_LIAB_a",ls(),value=T)

HH_LIAB_b<-grep("S14_S15_LIAB_b",ls(),value=T)
FC_LIAB_b<-grep("S12_LIAB_b",ls(),value=T)
NFC_LIAB_b<-grep("S11_ASS_b",ls(),value=T)
GG_LIAB_b<-grep("S13_LIAB_b",ls(),value=T)
ROW_LIAB_b<-grep("S2_LIAB_b",ls(),value=T)


# Let's create the outputs, these will simply find the correlations between assets
# and liabilities for each sector. 

#Create our directories first
dir.create(file.path("visuals-heatmap","FBS","1st time period"), recursive = TRUE)
dir.create(file.path("visuals-heatmap","FBS","2nd time period"), recursive = TRUE)

#############################################################################
#TIME PERIOD 1
#############################################################################

#HOUSEHOLD SECTOR
HH_SECTOR<-append(HH_ASSETS_a,HH_LIAB_a)
nftr_HH<-sapply(HH_SECTOR,get)
cor_melt_HH<-melt(cor(nftr_HH,use="p"))
png("visuals-heatmap/FBS/1st time period/hh_sector.png", units="in", width=10, height=10, res=150)
qplot(x=Var1,y=Var2,data=cor_melt_HH,xlab=NULL,ylab=NULL, fill=value, geom="tile",axes=F) +scale_fill_gradientn(colours=c("red","salmon","white","blue","darkblue"),values=rescale(c(-1,-0.90, 0,0.90,1)),guide="colorbar")+theme(axis.title.x = element_text(face="bold", colour="#990000", size="20"), axis.text.x  = element_text(angle=90, vjust=0.5, size=10))
dev.off()

#FINANCIAL SECTOR
FC_SECTOR<-append(FC_ASSETS_a,FC_LIAB_a)
nftr_FC<-sapply(FC_SECTOR,get)
cor_melt_FC<-melt(cor(nftr_FC,use="p"))
png("visuals-heatmap/FBS/1st time period/FC_sector.png", units="in", width=10, height=10, res=150)
qplot(x=Var1,y=Var2,data=cor_melt_FC,xlab=NULL,ylab=NULL, fill=value, geom="tile",axes=F) +scale_fill_gradientn(colours=c("red","salmon","white","blue","darkblue"),values=rescale(c(-1,-0.90, 0,0.90,1)),guide="colorbar")+theme(axis.title.x = element_text(face="bold", colour="#990000", size="20"), axis.text.x  = element_text(angle=90, vjust=0.5, size=10))
dev.off()

#NON_FINANCIAL SECTOR
NFC_SECTOR<-append(NFC_ASSETS_a,NFC_LIAB_a)
nftr_NFC<-sapply(NFC_SECTOR,get)
cor_melt_NFC<-melt(cor(nftr_NFC,use="p"))
png("visuals-heatmap/FBS/1st time period/NFC_sector.png", units="in", width=10, height=10, res=150)
qplot(x=Var1,y=Var2,data=cor_melt_NFC,xlab=NULL,ylab=NULL, fill=value, geom="tile",axes=F) +scale_fill_gradientn(colours=c("red","salmon","white","blue","darkblue"),values=rescale(c(-1,-0.90, 0,0.90,1)),guide="colorbar")+theme(axis.title.x = element_text(face="bold", colour="#990000", size="20"), axis.text.x  = element_text(angle=90, vjust=0.5, size=10))
dev.off()

#GOVERNMENT SECTOR
GG_SECTOR<-append(GG_ASSETS_a,GG_LIAB_a)
nftr_GG<-sapply(GG_SECTOR,get)
cor_melt_GG<-melt(cor(nftr_GG,use="p"))
png("visuals-heatmap/FBS/1st time period/GG_sector.png", units="in", width=10, height=10, res=150)
qplot(x=Var1,y=Var2,data=cor_melt_GG,xlab=NULL,ylab=NULL, fill=value, geom="tile",axes=F) +scale_fill_gradientn(colours=c("red","salmon","white","blue","darkblue"),values=rescale(c(-1,-0.90, 0,0.90,1)),guide="colorbar")+theme(axis.title.x = element_text(face="bold", colour="#990000", size="20"), axis.text.x  = element_text(angle=90, vjust=0.5, size=10))
dev.off()

#REST OF WORLD SECTOR
ROW_SECTOR<-append(ROW_ASSETS_a,ROW_LIAB_a)
nftr_ROW<-sapply(ROW_SECTOR,get)
cor_melt_ROW<-melt(cor(nftr_ROW,use="p"))
png("visuals-heatmap/FBS/1st time period/ROW_sector.png", units="in", width=10, height=10, res=150)
qplot(x=Var1,y=Var2,data=cor_melt_ROW,xlab=NULL,ylab=NULL, fill=value, geom="tile",axes=F) +scale_fill_gradientn(colours=c("red","salmon","white","blue","darkblue"),values=rescale(c(-1,-0.90, 0,0.90,1)),guide="colorbar")+theme(axis.title.x = element_text(face="bold", colour="#990000", size="20"), axis.text.x  = element_text(angle=90, vjust=0.5, size=10))
dev.off()




#############################################################################
#TIME PERIOD 1
#############################################################################

#HOUSEHOLD SECTOR
HH_SECTOR<-append(HH_ASSETS_b,HH_LIAB_b)
nftr_HH<-sapply(HH_SECTOR,get)
cor_melt_HH<-melt(cor(nftr_HH,use="p"))
png("visuals-heatmap/FBS/2nd time period/hh_sector.png", units="in", width=10, height=10, res=150)
qplot(x=Var1,y=Var2,data=cor_melt_HH,xlab=NULL,ylab=NULL, fill=value, geom="tile",axes=F) +scale_fill_gradientn(colours=c("red","salmon","white","blue","darkblue"),values=rescale(c(-1,-0.90, 0,0.90,1)),guide="colorbar")+theme(axis.title.x = element_text(face="bold", colour="#990000", size="20"), axis.text.x  = element_text(angle=90, vjust=0.5, size=10))
dev.off()

#FINANCIAL SECTOR
FC_SECTOR<-append(FC_ASSETS_b,FC_LIAB_b)
nftr_FC<-sapply(FC_SECTOR,get)
cor_melt_FC<-melt(cor(nftr_FC,use="p"))
png("visuals-heatmap/FBS/2nd time period/FC_sector.png", units="in", width=10, height=10, res=150)
qplot(x=Var1,y=Var2,data=cor_melt_FC,xlab=NULL,ylab=NULL, fill=value, geom="tile",axes=F) +scale_fill_gradientn(colours=c("red","salmon","white","blue","darkblue"),values=rescale(c(-1,-0.90, 0,0.90,1)),guide="colorbar")+theme(axis.title.x = element_text(face="bold", colour="#990000", size="20"), axis.text.x  = element_text(angle=90, vjust=0.5, size=10))
dev.off()

#NON_FINANCIAL SECTOR
NFC_SECTOR<-append(NFC_ASSETS_b,NFC_LIAB_b)
nftr_NFC<-sapply(NFC_SECTOR,get)
cor_melt_NFC<-melt(cor(nftr_NFC,use="p"))
png("visuals-heatmap/FBS/2nd time period/NFC_sector.png", units="in", width=10, height=10, res=150)
qplot(x=Var1,y=Var2,data=cor_melt_NFC,xlab=NULL,ylab=NULL, fill=value, geom="tile",axes=F) +scale_fill_gradientn(colours=c("red","salmon","white","blue","darkblue"),values=rescale(c(-1,-0.90, 0,0.90,1)),guide="colorbar")+theme(axis.title.x = element_text(face="bold", colour="#990000", size="20"), axis.text.x  = element_text(angle=90, vjust=0.5, size=10))
dev.off()

#GOVERNMENT SECTOR
GG_SECTOR<-append(GG_ASSETS_b,GG_LIAB_b)
nftr_GG<-sapply(GG_SECTOR,get)
cor_melt_GG<-melt(cor(nftr_GG,use="p"))
png("visuals-heatmap/FBS/2nd time period/GG_sector.png", units="in", width=10, height=10, res=150)
qplot(x=Var1,y=Var2,data=cor_melt_GG,xlab=NULL,ylab=NULL, fill=value, geom="tile",axes=F) +scale_fill_gradientn(colours=c("red","salmon","white","blue","darkblue"),values=rescale(c(-1,-0.90, 0,0.90,1)),guide="colorbar")+theme(axis.title.x = element_text(face="bold", colour="#990000", size="20"), axis.text.x  = element_text(angle=90, vjust=0.5, size=10))
dev.off()

#REST OF WORLD SECTOR
ROW_SECTOR<-append(ROW_ASSETS_b,ROW_LIAB_b)
nftr_ROW<-sapply(ROW_SECTOR,get)
cor_melt_ROW<-melt(cor(nftr_ROW,use="p"))
png("visuals-heatmap/FBS/2nd time period/ROW_sector.png", units="in", width=10, height=10, res=150)
qplot(x=Var1,y=Var2,data=cor_melt_ROW,xlab=NULL,ylab=NULL, fill=value, geom="tile",axes=F) +scale_fill_gradientn(colours=c("red","salmon","white","blue","darkblue"),values=rescale(c(-1,-0.90, 0,0.90,1)),guide="colorbar")+theme(axis.title.x = element_text(face="bold", colour="#990000", size="20"), axis.text.x  = element_text(angle=90, vjust=0.5, size=10))
dev.off()





















