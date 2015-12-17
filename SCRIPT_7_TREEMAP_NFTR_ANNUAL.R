#set your working directory, it will be different for you
install.packages("treemap")
library(treemap)
install.packages("reshape2")
library(reshape2)
install.packages("SmarterPoland")
library(SmarterPoland)

#This function PRINTS OUT a filter dataset base on the country code and financial position
#IT DOESN'T CREATE THE FILTERED OBJECT FOR YOU ON IT'S OWN
D3tree_nftr<-function(country, direction){
  #   #First we read in our dictionary values and define their column names
  geo<-data.frame(getEurostatDictionary("geo"))
  names(geo)<-c("V1", "geo_desc")
  indic_na<-data.frame(getEurostatDictionary("indic_na"))
  names(indic_na)<-c("V1", "indic_na_desc")
  sector<-data.frame(getEurostatDictionary("sector"))
  names(sector)<-c("V1", "sector_desc")
  unit<-data.frame(getEurostatDictionary("unit"))
  names(unit)<-c("V1", "unit_desc")
  
  
  #   geo<-read.table(file="geo.dic", header=F, sep="\t",col.names=c("V1", "geo_desc"))
  #   indic_na<-read.table(file="indic_na.dic", header=F, sep="\t",col.names=c("V1", "indic_na_desc"))
  #   sector<-read.table(file="sector.dic", header=F, sep="\t",col.names=c("V1", "sector_desc"))
  #   unit<-read.table(file="unit.dic",quote="", header=F, sep="\t",col.names=c("V1", "unit_desc"))
  #   
  
  
  #Next, we read in data as a matrix to prevent conversion trouble later.
  nasa_nf_tr<-getEurostatRaw("nasa_nf_tr")
  
  #We must remove any whitespace and Xs in the column names, as R is very sensitive to such things.
  colnames(nasa_nf_tr)<-sub("X","",colnames(nasa_nf_tr))
  colnames(nasa_nf_tr)<-sub(" ","",colnames(nasa_nf_tr))
  
  nasa_nf_tr_colsplit<-data.frame(nasa_nf_tr[2:length(nasa_nf_tr)], colsplit(nasa_nf_tr[["unit,direct,indic_na,sector,geo\\time"]], ",", c("unit","direct","indic_na","sector","geo")), check.names=F)
  
  #We need to merge the dictionary values with the main database.
  m1<-merge(nasa_nf_tr_colsplit, sector, by.x="sector", by.y="V1")
  m1<-merge(m1, geo, by.x="geo", by.y="V1")
  m1<-merge(m1, unit, by.x="unit", by.y="V1")
  m1<-merge(m1, indic_na, by.x="indic_na", by.y="V1")
  m1$a<-rep("a",length(m1$indic_na))
  #We redefine our merged dataset as nasa_nf_tr and define as a matrix to prevent conversion trouble later
  nasa_nf_tr<-as.matrix(m1)
  
  
  #Next we filter co_nco data to leave only row with "CO" values in, as requested .
  #AND then select the row which matches the definitions selected in the argument list and 
  #create a new object from this.
  subset1<-as.data.frame(subset(nasa_nf_tr, nasa_nf_tr[,c("geo")]==country & nasa_nf_tr[,c("direct")]==direction & nasa_nf_tr[,c("unit")]=="MIO_NAC"))
  #subset1<-as.data.frame(subset(nasa_nf_tr, nasa_nf_tr[,c("geo")]=="IE"  & nasa_nf_tr[,c("unit")]=="MIO_EUR"))
  subset2<-subset(subset1,subset1$indic_na=="D1"|subset1$indic_na=="D2"|subset1$indic_na=="D3"|subset1$indic_na=="D4"|subset1$indic_na=="D5"|subset1$indic_na=="D6"|subset1$indic_na=="D7"|subset1$indic_na=="D8"|subset1$indic_na=="D9"|subset1$indic_na=="P3"|subset1$indic_na=="P5"|subset1$indic_na=="P6"|subset1$indic_na=="P7"|subset1$indic_na=="K1"|subset1$indic_na=="K2")
  subset3<-subset(subset2,subset2[,c("sector")]=="S11"|subset2[,c("sector")]=="S12"|subset2[,c("sector")]=="S13"|subset2[,c("sector")]=="S14_S15")
  #subset2[,c("sector")]=="S2"|
  subset3$sector_desc2<-NA
  subset3$sector_desc2[subset3$sector=="S14_S15"]<-"HH"
  subset3$sector_desc2[subset3$sector=="S12"]<-"FC"
  subset3$sector_desc2[subset3$sector=="S11"]<-"NFC"
  subset3$sector_desc2[subset3$sector=="S13"]<-"GG"
  subset3$sector_desc2[subset3$sector=="S2"]<-"RoW"
  
  subset3$indic_na_desc2<-NA
  subset3$indic_na_desc2[subset3$indic_na=="D1"]<-"Wages"
  subset3$indic_na_desc2[subset3$indic_na=="D2"]<-"Prod Taxes"
  subset3$indic_na_desc2[subset3$indic_na=="D3"]<-"Prod Subsidies"
  subset3$indic_na_desc2[subset3$indic_na=="D4"]<-"Prop Inc"
  subset3$indic_na_desc2[subset3$indic_na=="D5"]<-"Inc/Wealth Taxes"
  subset3$indic_na_desc2[subset3$indic_na=="D6"]<-"Social con/benefits"
  subset3$indic_na_desc2[subset3$indic_na=="D7"]<-"Oth Current Inc"
  subset3$indic_na_desc2[subset3$indic_na=="D8"]<-"Pension Adj"
  subset3$indic_na_desc2[subset3$indic_na=="D9"]<-"Capital Transfers"
  subset3$indic_na_desc2[subset3$indic_na=="K1"]<-"Consumption of fixed capital"
  subset3$indic_na_desc2[subset3$indic_na=="K2"]<-"Net Acq of nonfin assets"
  #subset3$indic_na_desc2[subset3$indic_na=="B1G"]<-"GDP"
  subset3$indic_na_desc2[subset3$indic_na=="P6"]<-"Exports"
  subset3$indic_na_desc2[subset3$indic_na=="P7"]<-"Imports"
  subset3$indic_na_desc2[subset3$indic_na=="P5"]<-"GFCF"
  subset3$indic_na_desc2[subset3$indic_na=="P3"]<-"Consumption"
  
  #paste together the columns you created
  subset3$indic_na_desc3<-paste(subset3$sector_desc2,subset3$indic_na_desc2,sep="_")
  for(i in 2002:2012){
    subset3[,as.character(i)]<-sapply(subset3[,as.character(i)],as.character)
    subset3[,as.character(i)]<-sapply(subset3[,as.character(i)],as.numeric)  
  }
  print(subset3)
  
}


#To create the dataset
# <your object name> <- D3tree_bs(country = "IE",position="ASS")
IE_RECV<-D3tree_nftr(country = "IE",direction="RECV")
IE_PAID<-D3tree_nftr(country = "IE",direction="PAID")

#This function creates a treemap of the data frame we created.
#YOU MUST USE THE OBJECT NAME YOU GAVE THE DATASET
out.treemap<-function(objname){
  dir.create(file.path("treemap","visuals","NFTRA",objname),recursive=TRUE)
  for(i in 2002:2013){
    filename=paste("treemap/visuals/NFTRA/",objname,"/",objname,"_",i,".png",sep="")
    png(filename)
    treemap(get(objname),
            index=c("sector_desc2","indic_na_desc2"),
            vSize=c(as.character(i)),
            type="index",
            sortID=c("indic_na_desc2"))
    dev.off()
  }
}

#out.treemap(<your object name>)
out.treemap(objname = "IE_RECV")
out.treemap(objname = "IE_PAID")

