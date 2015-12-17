# THIS SCRIPT USES SMATERPOLAND TO RIP NATIONAL ACCOUNTS DATA FROM EUROSTAT
# AND CONVERT IT TO THE LONG DATA FORMAT

#Set your working directory
# setwd()

## Create output directory if it doesn't already exist
dir.create(file.path("data-eurostat","raw"), recursive = TRUE)
dir.create(file.path("data-eurostat","long"), recursive = TRUE)

library(reshape2)
library(SmarterPoland)
#esa 95
nasa_95_nf_tr<-getEurostatRaw("nasa_nf_tr")
nasa_95_f_tr<-getEurostatRaw("nasa_f_tr")
nasa_95_f_bs<-getEurostatRaw("nasa_f_bs")
nasq_95_nf_tr<-getEurostatRaw("nasq_nf_tr")
nasq_95_f_tr<-getEurostatRaw("nasq_f_tr")
nasq_95_f_bs<-getEurostatRaw("nasq_f_bs")
#esa 2010
nasa_10_nf_tr<-getEurostatRaw("nasa_10_nf_tr")
nasa_10_f_tr<-getEurostatRaw("nasa_10_f_tr")
nasa_10_f_bs<-getEurostatRaw("nasa_10_f_bs")
nasq_10_nf_tr<-getEurostatRaw("nasq_10_nf_tr")
nasq_10_f_tr<-getEurostatRaw("nasq_10_f_tr")
nasq_10_f_bs<-getEurostatRaw("nasq_10_f_bs")

##################################################################################
#SAVING RAW DATA, MAKE SURE PROPER WORKING DIRECTORY
##################################################################################
#Raw esa 95
save(nasa_95_nf_tr, file="data-eurostat/raw/nasa_95_nf_tr.RData")
save(nasa_95_f_tr, file="data-eurostat/raw/nasa_95_f_tr.RData")
save(nasa_95_f_bs, file="data-eurostat/raw/nasa_95_f_bs.RData")
save(nasq_95_nf_tr, file="data-eurostat/raw/nasq_95_nf_tr.RData")
save(nasq_95_f_tr, file="data-eurostat/raw/nasq_95_f_tr.RData")
save(nasq_95_f_bs, file="data-eurostat/raw/nasq_95_f_bs.RData")

#raw esa 95
save(nasa_10_nf_tr, file="data-eurostat/raw/nasa_10_nf_tr.RData")
save(nasa_10_f_tr, file="data-eurostat/raw/nasa_10_f_tr.RData")
save(nasa_10_f_bs, file="data-eurostat/raw/nasa_10_f_bs.RData")
save(nasq_10_nf_tr, file="data-eurostat/raw/nasq_10_nf_tr.RData")
save(nasq_10_f_tr, file="data-eurostat/raw/nasq_10_f_tr.RData")
save(nasq_10_f_bs, file="data-eurostat/raw/nasq_10_f_bs.RData")

##################################################################################
#transformation function
##################################################################################

#This transformation function converts the raw data to long format
#it assumes you've already loaded the data
trans<-function(df){
data<-melt(df)
colsplit<-colsplit(data[,1],",", as.vector(strsplit(names(data)[1],split=",")[[1]]))
output<-data.frame(colsplit,data[2:length(data)])
names<-names(output)
names<-gsub("variable","time",names)
names(output)<-names
return(output)
}
##################################################################################
#TRANSFORMING DATA
##################################################################################
#now we transform the raw data into long format
#trans esa 95
nasa_95_nf_tr<-trans(nasa_95_nf_tr)
nasa_95_f_tr<-trans(nasa_95_f_tr)
nasa_95_f_bs<-trans(nasa_95_f_bs)
nasq_95_nf_tr<-trans(nasq_95_nf_tr)
nasq_95_f_tr<-trans(nasq_95_f_tr)
nasq_95_f_bs<-trans(nasq_95_f_bs)

#trans esa 95
nasa_10_nf_tr<-trans(nasa_10_nf_tr)
nasa_10_f_tr<-trans(nasa_10_f_tr)
nasa_10_f_bs<-trans(nasa_10_f_bs)
nasq_10_nf_tr<-trans(nasq_10_nf_tr)
nasq_10_f_tr<-trans(nasq_10_f_tr)
nasq_10_f_bs<-trans(nasq_10_f_bs)
##################################################################################



##################################################################################
#SAVING LONG DATA, MAKE SURE PROPER WORKING DIRECTORY
##################################################################################
#long esa 95
save(nasa_95_nf_tr, file="data-eurostat/long/nasa_95_nf_tr.RData")
save(nasa_95_f_tr, file="data-eurostat/long/nasa_95_f_tr.RData")
save(nasa_95_f_bs, file="data-eurostat/long/nasa_95_f_bs.RData")
save(nasq_95_nf_tr, file="data-eurostat/long/nasq_95_nf_tr.RData")
save(nasq_95_f_tr, file="data-eurostat/long/nasq_95_f_tr.RData")
save(nasq_95_f_bs, file="data-eurostat/long/nasq_95_f_bs.RData")

#long esa 95
save(nasa_10_nf_tr, file="data-eurostat/long/nasa_10_nf_tr.RData")
save(nasa_10_f_tr, file="data-eurostat/long/nasa_10_f_tr.RData")
save(nasa_10_f_bs, file="data-eurostat/long/nasa_10_f_bs.RData")
save(nasq_10_nf_tr, file="data-eurostat/long/nasq_10_nf_tr.RData")
save(nasq_10_f_tr, file="data-eurostat/long/nasq_10_f_tr.RData")
save(nasq_10_f_bs, file="data-eurostat/long/nasq_10_f_bs.RData")


##################################################################################
#Let's edit the nasq_95_nftr data for easy access to our heatmap script
##################################################################################
#Load the nasq_95 data you got from the smarterpoland script
load("data-eurostat/raw/nasq_95_nf_tr.RData")
nasq_nf_tr<-nasq_95_nf_tr
rm(nasq_95_nf_tr)

#OR

#get the nftr data (for all countries) and create as an object
#nasq_nf_tr<-getEurostatRaw("nasq_nf_tr")

#OR

#if eurostat fails to dl for whatever reason, download the package directly fro eurostat's bulkd
#download facility and read it into r
#nasq_nf_tr<-read.table("nasq_nf_tr.tsv.gz", header=T, fill =T)

#remove any whitespace and Xs in the column names, as R is very sensitive to such things
names(nasq_nf_tr)<-sub("X","",names(nasq_nf_tr))
names(nasq_nf_tr)<-sub(" ","",names(nasq_nf_tr))

#we need to seperate columns to filter the data (think of it as text to columns in Excel)
nasq_nf_tr_colsplit<-data.frame(nasq_nf_tr[2:length(nasq_nf_tr)], colsplit(nasq_nf_tr[["unit,s_adj,direct,indic_na,sector,geo\\time"]], ",", c("unit","s_adj","direct","indic_na","sector","geo")), check.names=F)
#this is an older version, TO USE TO THE TSV.GZ FILE??
#nasq_nf_tr_colsplit<-data.frame(nasq_nf_tr[2:length(nasq_nf_tr)], colsplit(nasq_nf_tr[["unit.s_adj.direct.indic_na.sector.geo.time"]], ",", c("unit","s_adj","direct","indic_na","sector","geo")), check.names=F)

#remove any whitespace in the cloumn names, as R is very sensitive to such things
names(nasq_nf_tr_colsplit)<-sub(" ","",names(nasq_nf_tr_colsplit))
names(nasq_nf_tr)<-sub("X","",names(nasq_nf_tr))

#here we filter to get the irish data
data<-subset(nasq_nf_tr_colsplit,nasq_nf_tr_colsplit$s_adj=="NSA"&nasq_nf_tr_colsplit$geo=="IE"&nasq_nf_tr_colsplit$unit=="MIO_NAC")

#here I am removing data for the years we don't need, because they will affect 
#the time series if we keep them in. If data updates you can change this.
data<-data[,!names(data) %in% c("2014Q1","2001Q1","2001Q2","2001Q3","2001Q4","2000Q1","2000Q2","2000Q3","2000Q4","1999Q1","1999Q2","1999Q3","1999Q4")]
# our vector is backwards, so we use this function to remove unwanted columns and
# reverse the vector. Keep an eye on these column numbers if you update the
# above code line.
data<-data[c(48:1,49:54)]
names(data)<-sub("X","",names(data))
#here we define the column names that are time variables only
columnlist<-names(data)[!names(data) %in% c("s_adj","direct","indic_na","sector","geo","unit","finpos")]
#here we make sure that the data are numeric
for(i in columnlist){
  data[[as.character(i)]]<-as.numeric(as.character((data[[as.character(i)]])))
}

#We save the data as d_nftr for later use
d_nftr<-data
save(d_nftr, file="data-eurostat/d_nftr.RData")
