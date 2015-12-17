#setwd()
################################################################################
#LOAD YOUR PACKAGES
################################################################################
library(SmarterPoland)
library(reshape2)
library(tables)
library(gdata)
library(sqldf)
library(xlsx)
################################################################################
# LOADING AND EDITING YOUR DATA
################################################################################
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


##########################################################################################################################################################################################################################################################################
#GENERATING BS data (multiple methods)
##########################################################################################################################################################################################################################################################################
#load your central bank data from the data folder in your working directory
#THIS HAS THE MOST COMPLETE DATA
load("data-eurostat/d_fbs_cb.RData")

# #Load the nasq_95 data you got from the smarterpoland script
# load("data-eurostat/raw/nasq_95_f_bs.RData")
# nasq_f_bs<-nasq_95_f_bs
# rm(nasq_95_f_bs)

#OR

#get the nftr data (for all countries) and create as an object
#nasq_f_bs<-getEurostatRaw("nasq_f_bs")

#OR

#if eurostat fails to dl for whatever reason, download the package directly fro eurostat's bulkd
#download facility and read it into r
#nasq_f_bs<-read.table("nasq_f_bs.tsv.gz", header=T, fill =T)

#remove any whitespace and Xs in the column names, as R is very sensitive to such things
names(nasq_f_bs)<-sub("X","",names(nasq_f_bs))
names(nasq_f_bs)<-sub(" ","",names(nasq_f_bs))

#we need to seperate columns to filter the data (think of it as text to columns in Excel)
#nasq_f_bs_colsplit<-data.frame(nasq_f_bs[2:length(nasq_f_bs)], colsplit(nasq_f_bs[["unit,s_adj,direct,indic_na,sector,geo\\time"]], ",", c("unit","s_adj","direct","indic_na","sector","geo")), check.names=F)
nasq_f_bs_colsplit<-data.frame(nasq_f_bs[2:length(nasq_f_bs)], colsplit(nasq_f_bs[["unit,co_nco,sector,indic_na,finpos,geo\\time"]], ",", c("unit","co_nco","sector","indic_na","finpos","geo")), check.names=F)

#remove any whitespace in the cloumn names, as R is very sensitive to such things
names(nasq_f_bs_colsplit)<-sub(" ","",names(nasq_f_bs_colsplit))
names(nasq_f_bs_colsplit)<-sub("X","",names(nasq_f_bs_colsplit))

MFI_BS<-read.xlsx("data-eurostat/cb_qfa.xlsx",sheetName = "MFI_BS")

#here we filter to get the irish data
databs<-subset(nasq_f_bs_colsplit,nasq_f_bs_colsplit$co_nco=="NCO"&nasq_f_bs_colsplit$geo=="IE"&nasq_f_bs_colsplit$unit=="MIO_NAC")


#here I am removing data for the years we don't need, because they will affect 
#the time series if we keep them in. If data updates you can change this.
databs<-databs[,!names(databs) %in% c("2014Q1","2001Q1","2001Q2","2001Q3","2001Q4","2000Q1","2000Q2","2000Q3","2000Q4","1999Q1","1999Q2","1999Q3","1999Q4")]

#reorder columns
databs<-databs[c(48:1,49:54)]

#use the previous column list tomake all data numeric
for(i in columnlist){
  databs[[as.character(i)]]<-as.numeric(as.character((databs[[as.character(i)]])))
}
##########################################################################################################################################################################################################################################################################
#GENERATING FTR DATA
##########################################################################################################################################################################################################################################################################
#load your central bank data from the data folder in your working directory
#THIS HAS THE MOST COMPLETE DATA
load("data-eurostat/d_ftr_cb.RData")

#OR

# #Load the nasq_95 data you got from the smarterpoland script
# load("data-eurostat/raw/nasq_95_f_tr.RData")
# nasq_f_tr<-nasq_95_f_tr
# rm(nasq_95_f_tr)

#OR

#get the nftr data (for all countries) and create as an object
#nasq_f_tr<-getEurostatRaw("nasq_f_tr")

#OR

#if eurostat fails to dl for whatever reason, download the package directly fro eurostat's bulkd
#download facility and read it into r
#nasq_f_tr<-read.table("nasq_f_tr.tsv.gz", header=T, fill =T)

#remove any whitespace and Xs in the column names, as R is very sensitive to such things
names(nasq_f_tr)<-sub("X","",names(nasq_f_tr))
names(nasq_f_tr)<-sub(" ","",names(nasq_f_tr))

#we need to seperate columns to filter the data (think of it as text to columns in Excel)
#nasq_f_tr_colsplit<-data.frame(nasq_f_tr[2:length(nasq_f_tr)], colsplit(nasq_f_tr[["unit,s_adj,direct,indic_na,sector,geo\\time"]], ",", c("unit","s_adj","direct","indic_na","sector","geo")), check.names=F)
nasq_f_tr_colsplit<-data.frame(nasq_f_tr[2:length(nasq_f_tr)], colsplit(nasq_f_tr[["unit,co_nco,sector,indic_na,finpos,geo\\time"]], ",", c("unit","co_nco","sector","indic_na","finpos","geo")), check.names=F)

#remove any whitespace in the cloumn names, as R is very sensitive to such things
names(nasq_f_tr_colsplit)<-sub(" ","",names(nasq_f_tr_colsplit))
names(nasq_f_tr_colsplit)<-sub("X","",names(nasq_f_tr_colsplit))

unique(nasq_f_tr_colsplit$indic_na)
#here we filter to get the irish data
dataftr<-subset(nasq_f_tr_colsplit,nasq_f_tr_colsplit$co_nco=="NCO"&nasq_f_tr_colsplit$geo=="IE"&nasq_f_tr_colsplit$unit=="MIO_NAC")

#here I am removing data for the years we don't need, because they will affect 
#the time series if we keep them in. If data updates you can change this.
dataftr<-dataftr[,!names(dataftr) %in% c("2014Q1","2001Q1","2001Q2","2001Q3","2001Q4","2000Q1","2000Q2","2000Q3","2000Q4","1999Q1","1999Q2","1999Q3","1999Q4")]

#reorder columns
dataftr<-dataftr[c(48:1,49:54)]

#use the previous column list to make all data numeric
for(i in columnlist){
  databs[[as.character(i)]]<-as.numeric(as.character((databs[[as.character(i)]])))
}

##########################################################################################################################################################################################################################################################################
#GENERATING NFTR TABLE
##########################################################################################################################################################################################################################################################################
# The following function creates a blank matrix, which we fill with values from
# our data, based on the subsets for each cell. National Account items are 
# detailed in the row names and sectors are detailed in the column names.

generate_quarterNFTR<-function(quarter){
  quarterMatrix=matrix(data=0,ncol=10,nrow=13, dimnames=list(c("GDP","Wage Bill", "Net Taxes", "Property Income:Interest","Incomes: Pensions & other","Dividends","Financial Consumption", "Consumption", "Capital Transfers","Gross capital formation", "Net Exports", "net social contributions", "Net Lending/Borrowing in non-financial account"),c("incomes_hh", "expenditures_hh", "incomes_fc", "expenditures_fc", "incomes_nfc", "expenditures_nfc", "incomes_gg", "expenditures_gg", "incomes_row", "expenditures_row")))
  #create datapoints for each entry
  #for 1 Net Output
  quarterMatrix[1,1]<-sum(data[[quarter]][(data$indic_na=="B1G") & (data$sector=="S14_S15") & (data$direct=="RECV")])
  quarterMatrix[1,3]<-sum(data[[quarter]][(data$indic_na=="B1G") & (data$sector=="S12") & (data$direct=="RECV")])
  quarterMatrix[1,5]<-sum(data[[quarter]][(data$indic_na=="B1G") & (data$sector=="S11") & (data$direct=="RECV")])
  quarterMatrix[1,7]<-sum(data[[quarter]][(data$indic_na=="B1G") & (data$sector=="S13") & (data$direct=="RECV")])+sum(data[[quarter]][(data$indic_na=="K2") & (data$sector=="S13") & (data$direct=="PAID")])
  quarterMatrix[1,9]<-sum(data[[quarter]][(data$indic_na=="B1G") & (data$sector=="S2") & (data$direct=="RECV")])
  
  #Wage bill
  quarterMatrix[2,1]<-sum(data[[quarter]][(data$indic_na=="D1") & (data$sector=="S14_S15") & (data$direct=="RECV")])-sum(data[[quarter]][(data$indic_na=="D1") & (data$sector=="S14_S15") & (data$direct=="PAID")])
  quarterMatrix[2,3]<-sum(data[[quarter]][(data$indic_na=="D1") & (data$sector=="S12") & (data$direct=="RECV")])
  quarterMatrix[2,4]<-sum(data[[quarter]][(data$indic_na=="D1") & (data$sector=="S12") & (data$direct=="PAID")])
  quarterMatrix[2,5]<-sum(data[[quarter]][(data$indic_na=="D1") & (data$sector=="S11") & (data$direct=="RECV")])
  quarterMatrix[2,6]<-sum(data[[quarter]][(data$indic_na=="D1") & (data$sector=="S11") & (data$direct=="PAID")])-(sum(data[[quarter]][(data$indic_na=="D1") & (data$sector=="S2") & (data$direct=="RECV")])-sum(data[[quarter]][(data$indic_na=="D1") & (data$sector=="S2") & (data$direct=="PAID")]))
  quarterMatrix[2,7]<-sum(data[[quarter]][(data$indic_na=="D1") & (data$sector=="S13") & (data$direct=="RECV")])
  quarterMatrix[2,8]<-sum(data[[quarter]][(data$indic_na=="D1") & (data$sector=="S13") & (data$direct=="PAID")])
  
  #for 3 net taxes
  quarterMatrix[3,2]<-sum(data[[quarter]][(data$indic_na=="D2"|data$indic_na=="D3"|data$indic_na=="D5") & (data$sector=="S14_S15") & (data$direct=="PAID")])-sum(data[[quarter]][(data$indic_na=="D2"|data$indic_na=="D3"|data$indic_na=="D5") & (data$sector=="S14_S15") & (data$direct=="RECV")])
  quarterMatrix[3,4]<-sum(data[[quarter]][(data$indic_na=="D2"|data$indic_na=="D3"|data$indic_na=="D5") & (data$sector=="S12") & (data$direct=="PAID")])-sum(data[[quarter]][(data$indic_na=="D2"|data$indic_na=="D3"|data$indic_na=="D5") & (data$sector=="S12") & (data$direct=="RECV")])
  quarterMatrix[3,6]<-sum(data[[quarter]][(data$indic_na=="D2"|data$indic_na=="D3"|data$indic_na=="D5") & (data$sector=="S11") & (data$direct=="PAID")])-sum(data[[quarter]][(data$indic_na=="D2"|data$indic_na=="D3"|data$indic_na=="D5") & (data$sector=="S11") & (data$direct=="RECV")])
  quarterMatrix[3,7]<-sum(data[[quarter]][(data$indic_na=="D2"|data$indic_na=="D3"|data$indic_na=="D5") & (data$sector=="S13") & (data$direct=="RECV")])-sum(data[[quarter]][(data$indic_na=="D2"|data$indic_na=="D3"|data$indic_na=="D5") & (data$sector=="S13") & (data$direct=="PAID")])
  quarterMatrix[3,10]<-sum(data[[quarter]][(data$indic_na=="D2"|data$indic_na=="D3"|data$indic_na=="D5") & (data$sector=="S2") & (data$direct=="PAID")])-sum(data[[quarter]][(data$indic_na=="D2"|data$indic_na=="D3"|data$indic_na=="D5") & (data$sector=="S2") & (data$direct=="RECV")])
  
  #for 4 property income: interest payment
  quarterMatrix[4,1]<-sum(data[[quarter]][(data$indic_na=="D41") & (data$sector=="S14_S15") & (data$direct=="RECV")])
  quarterMatrix[4,2]<-sum(data[[quarter]][(data$indic_na=="D41") & (data$sector=="S14_S15") & (data$direct=="PAID")])
  quarterMatrix[4,3]<-sum(data[[quarter]][(data$indic_na=="D41") & (data$sector=="S12") & (data$direct=="RECV")])
  quarterMatrix[4,4]<-sum(data[[quarter]][(data$indic_na=="D41") & (data$sector=="S12") & (data$direct=="PAID")])
  quarterMatrix[4,5]<-sum(data[[quarter]][(data$indic_na=="D41") & (data$sector=="S11") & (data$direct=="RECV")])
  quarterMatrix[4,6]<-sum(data[[quarter]][(data$indic_na=="D41") & (data$sector=="S11") & (data$direct=="PAID")])
  quarterMatrix[4,7]<-sum(data[[quarter]][(data$indic_na=="D41") & (data$sector=="S13") & (data$direct=="RECV")])
  quarterMatrix[4,8]<-sum(data[[quarter]][(data$indic_na=="D41") & (data$sector=="S13") & (data$direct=="PAID")])
  quarterMatrix[4,9]<-sum(data[[quarter]][(data$indic_na=="D41") & (data$sector=="S2") & (data$direct=="RECV")])
  quarterMatrix[4,10]<-sum(data[[quarter]][(data$indic_na=="D41") & (data$sector=="S2") & (data$direct=="PAID")])
  
  #for pensions and ITR income
  quarterMatrix[5,1]<-sum(data[[quarter]][(data$indic_na=="D44") & (data$sector=="S14_S15") & (data$direct=="RECV")])+sum(data[[quarter]][(data$indic_na=="D8") & (data$sector=="S14_S15") & (data$direct=="RECV")])
  quarterMatrix[5,4]<-sum(data[[quarter]][(data$indic_na=="D44") & (data$sector=="S12") & (data$direct=="PAID")])+sum(data[[quarter]][(data$indic_na=="D8") & (data$sector=="S12") & (data$direct=="PAID")])
  quarterMatrix[5,10]<-sum(data[[quarter]][(data$indic_na=="D44") & (data$sector=="S2") & (data$direct=="PAID")])+sum(data[[quarter]][(data$indic_na=="D8") & (data$sector=="S2") & (data$direct=="PAID")])-sum(data[[quarter]][(data$indic_na=="D44") & (data$sector=="S2") & (data$direct=="RECV")])+sum(data[[quarter]][(data$indic_na=="D8") & (data$sector=="S2") & (data$direct=="RECV")])
  
  #for dividends
  quarterMatrix[6,1]<-sum(data[[quarter]][(data$indic_na=="D43"|data$indic_na=="D42") & (data$sector=="S14_S15") & (data$direct=="RECV")])
  quarterMatrix[6,2]<-sum(data[[quarter]][(data$indic_na=="D43"|data$indic_na=="D42") & (data$sector=="S14_S15") & (data$direct=="PAID")])
  quarterMatrix[6,3]<-sum(data[[quarter]][(data$indic_na=="D43"|data$indic_na=="D42") & (data$sector=="S12") & (data$direct=="RECV")])
  quarterMatrix[6,4]<-sum(data[[quarter]][(data$indic_na=="D43"|data$indic_na=="D42") & (data$sector=="S12") & (data$direct=="PAID")])
  quarterMatrix[6,5]<-sum(data[[quarter]][(data$indic_na=="D43"|data$indic_na=="D42") & (data$sector=="S11") & (data$direct=="RECV")])
  quarterMatrix[6,6]<-sum(data[[quarter]][(data$indic_na=="D43"|data$indic_na=="D42") & (data$sector=="S11") & (data$direct=="PAID")])+(sum(data[[quarter]][(data$indic_na=="D43"|data$indic_na=="D42") & (data$sector=="S14_S15") & (data$direct=="RECV")])+sum(data[[quarter]][(data$indic_na=="D43"|data$indic_na=="D42") & (data$sector=="S12") & (data$direct=="RECV")])-sum(data[[quarter]][(data$indic_na=="D43"|data$indic_na=="D42") & (data$sector=="S12") & (data$direct=="PAID")])+sum(data[[quarter]][(data$indic_na=="D43"|data$indic_na=="D42") & (data$sector=="S11") & (data$direct=="RECV")])-sum(data[[quarter]][(data$indic_na=="D43"|data$indic_na=="D42") & (data$sector=="S11") & (data$direct=="PAID")])+sum(data[[quarter]][(data$indic_na=="D43"|data$indic_na=="D42") & (data$sector=="S13") & (data$direct=="RECV")]))*-1
  quarterMatrix[6,7]<-sum(data[[quarter]][(data$indic_na=="D43"|data$indic_na=="D42") & (data$sector=="S13") & (data$direct=="RECV")])
  quarterMatrix[6,8]<-sum(data[[quarter]][(data$indic_na=="D43"|data$indic_na=="D42") & (data$sector=="S13") & (data$direct=="PAID")])
  quarterMatrix[6,9]<-(sum(data[[quarter]][(data$indic_na=="D43"|data$indic_na=="D42") & (data$sector=="S14_S15") & (data$direct=="RECV")])+sum(data[[quarter]][(data$indic_na=="D43"|data$indic_na=="D42") & (data$sector=="S12") & (data$direct=="RECV")])-sum(data[[quarter]][(data$indic_na=="D43"|data$indic_na=="D42") & (data$sector=="S12") & (data$direct=="PAID")])+sum(data[[quarter]][(data$indic_na=="D43"|data$indic_na=="D42") & (data$sector=="S11") & (data$direct=="RECV")])-sum(data[[quarter]][(data$indic_na=="D43"|data$indic_na=="D42") & (data$sector=="S11") & (data$direct=="PAID")])+sum(data[[quarter]][(data$indic_na=="D43"|data$indic_na=="D42") & (data$sector=="S13") & (data$direct=="RECV")]))*-1
  
  #for Financial consumption 
  quarterMatrix[7,1]<-sum(data[[quarter]][(data$indic_na=="D7") & (data$sector=="S14_S15") & (data$direct=="RECV")])
  quarterMatrix[7,2]<-sum(data[[quarter]][(data$indic_na=="D7") & (data$sector=="S14_S15") & (data$direct=="PAID")])
  quarterMatrix[7,3]<-sum(data[[quarter]][(data$indic_na=="D7") & (data$sector=="S12") & (data$direct=="RECV")])
  quarterMatrix[7,4]<-sum(data[[quarter]][(data$indic_na=="D7") & (data$sector=="S12") & (data$direct=="PAID")])
  quarterMatrix[7,5]<-sum(data[[quarter]][(data$indic_na=="D7") & (data$sector=="S11") & (data$direct=="RECV")])
  quarterMatrix[7,6]<-sum(data[[quarter]][(data$indic_na=="D7") & (data$sector=="S11") & (data$direct=="PAID")])
  quarterMatrix[7,7]<-sum(data[[quarter]][(data$indic_na=="D7") & (data$sector=="S13") & (data$direct=="RECV")])
  quarterMatrix[7,8]<-sum(data[[quarter]][(data$indic_na=="D7") & (data$sector=="S13") & (data$direct=="PAID")])
  quarterMatrix[7,9]<-sum(data[[quarter]][(data$indic_na=="D7") & (data$sector=="S2") & (data$direct=="RECV")])
  quarterMatrix[7,10]<-sum(data[[quarter]][(data$indic_na=="D7") & (data$sector=="S2") & (data$direct=="PAID")])
  
  #for final consumption expenditure
  quarterMatrix[8,1]<-sum(data[[quarter]][(data$indic_na=="P3") & (data$sector=="S14_S15") & (data$direct=="RECV")])
  quarterMatrix[8,2]<-sum(data[[quarter]][(data$indic_na=="P3") & (data$sector=="S14_S15") & (data$direct=="PAID")])
  quarterMatrix[8,3]<-sum(data[[quarter]][(data$indic_na=="P3") & (data$sector=="S12") & (data$direct=="RECV")])
  quarterMatrix[8,4]<-sum(data[[quarter]][(data$indic_na=="P3") & (data$sector=="S12") & (data$direct=="PAID")])
  quarterMatrix[8,5]<-sum(data[[quarter]][(data$indic_na=="P3") & (data$sector=="S11") & (data$direct=="RECV")])+sum(data[[quarter]][(data$indic_na=="P3") & (data$sector=="S14_S15") & (data$direct=="PAID")])+sum(data[[quarter]][(data$indic_na=="P3") & (data$sector=="S13") & (data$direct=="PAID")])
  quarterMatrix[8,6]<-sum(data[[quarter]][(data$indic_na=="P3") & (data$sector=="S11") & (data$direct=="PAID")])
  quarterMatrix[8,7]<-sum(data[[quarter]][(data$indic_na=="P3") & (data$sector=="S13") & (data$direct=="RECV")])
  quarterMatrix[8,8]<-sum(data[[quarter]][(data$indic_na=="P3") & (data$sector=="S13") & (data$direct=="PAID")])
  quarterMatrix[8,9]<-sum(data[[quarter]][(data$indic_na=="P3") & (data$sector=="S2") & (data$direct=="RECV")])
  quarterMatrix[8,10]<-sum(data[[quarter]][(data$indic_na=="P3") & (data$sector=="S2") & (data$direct=="PAID")])
  #for capital transfers
  quarterMatrix[9,1]<-sum(data[[quarter]][(data$indic_na=="D9") & (data$sector=="S14_S15") & (data$direct=="RECV")])-sum(data[[quarter]][(data$indic_na=="D9") & (data$sector=="S14_S15") & (data$direct=="PAID")])
  quarterMatrix[9,3]<-sum(data[[quarter]][(data$indic_na=="D9") & (data$sector=="S12") & (data$direct=="RECV")])-sum(data[[quarter]][(data$indic_na=="D9") & (data$sector=="S12") & (data$direct=="PAID")])
  quarterMatrix[9,5]<-sum(data[[quarter]][(data$indic_na=="D9") & (data$sector=="S11") & (data$direct=="RECV")])-sum(data[[quarter]][(data$indic_na=="D9") & (data$sector=="S11") & (data$direct=="PAID")])
  quarterMatrix[9,8]<-sum(data[[quarter]][(data$indic_na=="D9") & (data$sector=="S13") & (data$direct=="PAID")])-sum(data[[quarter]][(data$indic_na=="D9") & (data$sector=="S13") & (data$direct=="RECV")])
  quarterMatrix[9,9]<-sum(data[[quarter]][(data$indic_na=="D9") & (data$sector=="S2") & (data$direct=="RECV")])-sum(data[[quarter]][(data$indic_na=="D9") & (data$sector=="S2") & (data$direct=="PAID")])
  
  #for Gross fixed capital formation
  quarterMatrix[10,2]<-sum(data[[quarter]][(data$indic_na=="P5") & (data$sector=="S14_S15") & (data$direct=="PAID")])
  quarterMatrix[10,4]<-sum(data[[quarter]][(data$indic_na=="P5") & (data$sector=="S12") & (data$direct=="PAID")])
  quarterMatrix[10,5]<-sum(data[[quarter]][(data$indic_na=="P5") & (data$sector=="S14_S15") & (data$direct=="PAID")])+sum(data[[quarter]][(data$indic_na=="P5") & (data$sector=="S12") & (data$direct=="PAID")])+sum(data[[quarter]][(data$indic_na=="P5") & (data$sector=="S13") & (data$direct=="PAID")])+sum(data[[quarter]][(data$indic_na=="P5") & (data$sector=="S11") & (data$direct=="PAID")])
  quarterMatrix[10,8]<-sum(data[[quarter]][(data$indic_na=="P5") & (data$sector=="S11") & (data$direct=="PAID")])
  quarterMatrix[10,8]<-sum(data[[quarter]][(data$indic_na=="P5") & (data$sector=="S13") & (data$direct=="PAID")])
  #for Net Exports
  quarterMatrix[11,1]<-sum(data[[quarter]][(data$indic_na=="P6"|data$indic_na=="P7") & (data$sector=="S14_S15") & (data$direct=="RECV")])
  quarterMatrix[11,2]<-sum(data[[quarter]][(data$indic_na=="P6"|data$indic_na=="P7") & (data$sector=="S14_S15") & (data$direct=="PAID")])
  quarterMatrix[11,3]<-sum(data[[quarter]][(data$indic_na=="P6"|data$indic_na=="P7") & (data$sector=="S12") & (data$direct=="RECV")])
  quarterMatrix[11,4]<-sum(data[[quarter]][(data$indic_na=="P6"|data$indic_na=="P7") & (data$sector=="S12") & (data$direct=="PAID")])
  quarterMatrix[11,5]<-sum(data[[quarter]][(data$indic_na=="P6"|data$indic_na=="P7") & (data$sector=="S2") & (data$direct=="PAID")])
  quarterMatrix[11,6]<-sum(data[[quarter]][(data$indic_na=="P6"|data$indic_na=="P7") & (data$sector=="S2") & (data$direct=="RECV")])+(sum(data[[quarter]][(data$indic_na=="D1") & (data$sector=="S2") & (data$direct=="RECV")])-sum(data[[quarter]][(data$indic_na=="D1") & (data$sector=="S2") & (data$direct=="PAID")]))
  quarterMatrix[11,7]<-sum(data[[quarter]][(data$indic_na=="P6"|data$indic_na=="P7") & (data$sector=="S13") & (data$direct=="RECV")])
  quarterMatrix[11,8]<-sum(data[[quarter]][(data$indic_na=="P6"|data$indic_na=="P7") & (data$sector=="S13") & (data$direct=="PAID")])
  quarterMatrix[11,9]<-sum(data[[quarter]][(data$indic_na=="P6"|data$indic_na=="P7") & (data$sector=="S2") & (data$direct=="RECV")])+(sum(data[[quarter]][(data$indic_na=="D1") & (data$sector=="S2") & (data$direct=="RECV")])-sum(data[[quarter]][(data$indic_na=="D1") & (data$sector=="S2") & (data$direct=="PAID")]))
  quarterMatrix[11,10]<-sum(data[[quarter]][(data$indic_na=="P6"|data$indic_na=="P7") & (data$sector=="S2") & (data$direct=="PAID")])
  
  #social benefits
  quarterMatrix[12,1]<-sum(data[[quarter]][(data$indic_na=="D62") & (data$sector=="S14_S15") & (data$direct=="RECV")])-sum(data[[quarter]][(data$indic_na=="D62") & (data$sector=="S14_S15") & (data$direct=="PAID")])-(sum(data[[quarter]][(data$indic_na=="D61") & (data$sector=="S14_S15") & (data$direct=="PAID")])-sum(data[[quarter]][(data$indic_na=="D61") & (data$sector=="S14_S15") & (data$direct=="RECV")]))
  quarterMatrix[12,3]<-sum(data[[quarter]][(data$indic_na=="D62") & (data$sector=="S12") & (data$direct=="RECV")])-sum(data[[quarter]][(data$indic_na=="D62") & (data$sector=="S12") & (data$direct=="PAID")])-(sum(data[[quarter]][(data$indic_na=="D61") & (data$sector=="S12") & (data$direct=="PAID")])-sum(data[[quarter]][(data$indic_na=="D61") & (data$sector=="S12") & (data$direct=="RECV")]))
  quarterMatrix[12,5]<-sum(data[[quarter]][(data$indic_na=="D62") & (data$sector=="S11") & (data$direct=="RECV")])-sum(data[[quarter]][(data$indic_na=="D62") & (data$sector=="S11") & (data$direct=="PAID")])-(sum(data[[quarter]][(data$indic_na=="D61") & (data$sector=="S11") & (data$direct=="PAID")])-sum(data[[quarter]][(data$indic_na=="D61") & (data$sector=="S11") & (data$direct=="RECV")]))
  quarterMatrix[12,8]<-sum(data[[quarter]][(data$indic_na=="D62") & (data$sector=="S13") & (data$direct=="PAID")])-sum(data[[quarter]][(data$indic_na=="D62") & (data$sector=="S13") & (data$direct=="RECV")])-(sum(data[[quarter]][(data$indic_na=="D61") & (data$sector=="S13") & (data$direct=="RECV")])-sum(data[[quarter]][(data$indic_na=="D61") & (data$sector=="S13") & (data$direct=="PAID")]))
  quarterMatrix[12,9]<-sum(data[[quarter]][(data$indic_na=="D62") & (data$sector=="S2") & (data$direct=="RECV")])-sum(data[[quarter]][(data$indic_na=="D62") & (data$sector=="S2") & (data$direct=="PAID")])-(sum(data[[quarter]][(data$indic_na=="D61") & (data$sector=="S2") & (data$direct=="PAID")])-sum(data[[quarter]][(data$indic_na=="D61") & (data$sector=="S2") & (data$direct=="RECV")]))
  
  #for Net Lending/Borrowing in nonfinancial accounts
  quarterMatrix[13,2]<-sum(data[[quarter]][(data$indic_na=="B9") & (data$sector=="S14_S15") & (data$direct=="PAID")])
  quarterMatrix[13,4]<-sum(data[[quarter]][(data$indic_na=="B9") & (data$sector=="S12") & (data$direct=="PAID")])
  quarterMatrix[13,6]<-sum(data[[quarter]][(data$indic_na=="B9") & (data$sector=="S11") & (data$direct=="PAID")])
  quarterMatrix[13,8]<-sum(data[[quarter]][(data$indic_na=="B9") & (data$sector=="S13") & (data$direct=="PAID")])
  quarterMatrix[13,10]<-sum(data[[quarter]][(data$indic_na=="B9") & (data$sector=="S2") & (data$direct=="PAID")])+sum(data[[quarter]][(data$indic_na=="K2") & (data$sector=="S2") & (data$direct=="PAID")])
  
  #the quartermatrix as defined is returned
  return(quarterMatrix)
}


#Create your directories to enter your TFM/BS RIPs
dir.create(file.path("TFM_qtr","NFTR"),recursive=T)
dir.create(file.path("TFM_qtr","FTR"),recursive=T)
dir.create(file.path("TFM_qtr","FBS"),recursive=T)
dir.create(file.path("TFM_qtr","eNFTR"),recursive=T)
dir.create(file.path("TFM_qtr","TFM"),recursive=T)


table_nf_tr<-list(.Names=c("incomes_hh", "expenditures_hh", "incomes_fc", "expenditures_fc", "incomes_nfc", "expenditures_nfc", "incomes_gg", "expenditures_gg", "incomes_row", "expenditures_row"), row.names=c("Loans", "Deposits", "Bonds", "Equities", "Other Accounts P/R"))
for(i in columnlist){
  #quarter is defined
  quarter <- paste(i,"",sep="")
  #generates the quartermatrix from generate_NFTR for the specific quarter
  quarterMatrix = generate_quarterNFTR(quarter)
  #defines table_nf_tr<quarter> as the above quartermatrix
  table_nf_tr[[as.character(i)]]=quarterMatrix
  #writes the file to a csv format
  write.csv(quarterMatrix,file=paste("TFM_qtr/NFTR/table_nf_tr_",quarter,".csv",sep=""))
}


#we need to edit the nfc exp column to make the matrix stock flow consistent
#load transaction files
#(assume all necessary csv files are in their proper folders)
for(i in columnlist){
  obj_name<-paste("table_nf_tr", i, sep="_")
  file_name<-file.path("TFM_qtr/NFTR",paste(obj_name, 'csv', sep="."))
  input<-data.frame(read.csv(file_name))
  assign(obj_name, value=input)
}
#created the edited NFTR table
for(i in columnlist){
  nm9<-paste("table_nf_tr","_", i, sep="")
  tmp9<-get(nm9)
  tmp9$flows_rowsum<-(tmp9$incomes_nfc+tmp9$incomes_fc+tmp9$incomes_gg+tmp9$incomes_row+tmp9$incomes_hh)-(tmp9$expenditures_nfc+tmp9$expenditures_fc+tmp9$expenditures_gg+tmp9$expenditures_row+tmp9$expenditures_hh)
  tmp9$expenditures_nfc_ed<-(tmp9$expenditures_nfc+tmp9$flows_rowsum)
  tmp9$expenditures_nfc<-tmp9$expenditures_nfc_ed
  tmp91<-tmp9[, c(1,2,3,4,5,6,7,8,9,10,11)]
  assign(paste("edited_nf_tr_",i,sep=""), tmp91)
}

#save the edited NFTR file to csv
for(i in columnlist){
  obj_name_enf2<-paste("edited_nf_tr", i, sep="_")
  file_name3<-file.path("TFM_qtr/eNFTR",paste(obj_name_enf2, 'csv', sep="."))
  write.csv(get(obj_name_enf2), file_name3)
}


##########################################################################################################################################################################################################################################################################
#GENERATING BS MATRIX
##########################################################################################################################################################################################################################################################################
# this function creates a matrix that is filled with values from the balance 
# sheet data

generate_quarterBS<-function(quarter){
  quarterMatrix=matrix(data=0,ncol=10,nrow=7, dimnames=list(c("F21_F22: Cash","F29: Deposits", "F3: Bonds", "F4: Loans", "F5: Equities", "F6: ITRs", "BF90: Net financial Position (balancing liabilities)"),c("assets_hh", "liabilities_hh", "assets_fc", "liabilities_fc", "assets_nfc", "liabilities_nfc", "assets_gg", "liabilities_gg", "assets_row", "liabilities_row")))
  #create d_fbspoints for each entry
  #F21: Cash
  quarterMatrix[1, 1] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F21_F22") & (d_fbs$sector == "S14_S15") & (d_fbs$finpos == "ASS")])
  quarterMatrix[1, 2] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F21_F22") & (d_fbs$sector == "S14_S15") & (d_fbs$finpos == "LIAB")])
  quarterMatrix[1, 3] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F21_F22") & (d_fbs$sector == "S12") & (d_fbs$finpos == "ASS")])
  quarterMatrix[1, 4] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F21_F22") & (d_fbs$sector == "S12") & (d_fbs$finpos == "LIAB")])
  quarterMatrix[1, 5] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F21_F22") & (d_fbs$sector == "S11") & (d_fbs$finpos == "ASS")])
  quarterMatrix[1, 6] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F21_F22") & (d_fbs$sector == "S11") & (d_fbs$finpos == "LIAB")])
  quarterMatrix[1, 7] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F21_F22") & (d_fbs$sector == "S13") & (d_fbs$finpos == "ASS")])
  quarterMatrix[1, 8] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F21_F22") & (d_fbs$sector == "S13") & (d_fbs$finpos == "LIAB")])
  quarterMatrix[1, 9] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F21_F22") & (d_fbs$sector == "S2") & (d_fbs$finpos == "ASS")])
  quarterMatrix[1, 10] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F21_F22") & (d_fbs$sector == "S2") & (d_fbs$finpos == "LIAB")])
  
  #F21: Deposits
  quarterMatrix[2, 1] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F29") & (d_fbs$sector == "S14_S15") & (d_fbs$finpos == "ASS")])
  quarterMatrix[2, 2] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F29") & (d_fbs$sector == "S14_S15") & (d_fbs$finpos == "LIAB")])
  quarterMatrix[2, 3] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F29") & (d_fbs$sector == "S12") & (d_fbs$finpos == "ASS")])
  quarterMatrix[2, 4] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F29") & (d_fbs$sector == "S12") & (d_fbs$finpos == "LIAB")])
  quarterMatrix[2, 5] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F29") & (d_fbs$sector == "S11") & (d_fbs$finpos == "ASS")])
  quarterMatrix[2, 6] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F29") & (d_fbs$sector == "S11") & (d_fbs$finpos == "LIAB")])
  quarterMatrix[2, 7] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F29") & (d_fbs$sector == "S13") & (d_fbs$finpos == "ASS")])
  quarterMatrix[2, 8] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F29") & (d_fbs$sector == "S13") & (d_fbs$finpos == "LIAB")])
  quarterMatrix[2, 9] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F29") & (d_fbs$sector == "S2") & (d_fbs$finpos == "ASS")])
  quarterMatrix[2, 10] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F29") & (d_fbs$sector == "S2") & (d_fbs$finpos == "LIAB")])
  
  
  #F3: Bonds
  quarterMatrix[3, 1] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F3") & (d_fbs$sector == "S14_S15") & (d_fbs$finpos == "ASS")])+sum(d_fbs[[quarter]][(d_fbs$indic_na == "F7") & (d_fbs$sector == "S14_S15") & (d_fbs$finpos == "ASS")])
  quarterMatrix[3, 2] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F3") & (d_fbs$sector == "S14_S15") & (d_fbs$finpos == "LIAB")])+sum(d_fbs[[quarter]][(d_fbs$indic_na == "F7") & (d_fbs$sector == "S14_S15") & (d_fbs$finpos == "LIAB")])
  quarterMatrix[3, 3] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F3") & (d_fbs$sector == "S12") & (d_fbs$finpos == "ASS")])+sum(d_fbs[[quarter]][(d_fbs$indic_na == "F7") & (d_fbs$sector == "S12") & (d_fbs$finpos == "ASS")])-sum(d_fbs[[quarter]][(d_fbs$indic_na == "F6") & (d_fbs$sector == "S11") & (d_fbs$finpos == "ASS")])+sum(d_fbs[[quarter]][(d_fbs$indic_na == "F1") & (d_fbs$sector == "S12") & (d_fbs$finpos == "ASS")])
  quarterMatrix[3, 4] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F3") & (d_fbs$sector == "S12") & (d_fbs$finpos == "LIAB")])+sum(d_fbs[[quarter]][(d_fbs$indic_na=="F1") & (d_fbs$sector=="S12") & (d_fbs$finpos=="ASS")])+sum(d_fbs[[quarter]][(d_fbs$indic_na == "F7") & (d_fbs$sector == "S12") & (d_fbs$finpos == "LIAB")])
  quarterMatrix[3, 5] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F3") & (d_fbs$sector == "S11") & (d_fbs$finpos == "ASS")])+sum(d_fbs[[quarter]][(d_fbs$indic_na == "F7") & (d_fbs$sector == "S11") & (d_fbs$finpos == "ASS")])+sum(d_fbs[[quarter]][(d_fbs$indic_na == "F6") & (d_fbs$sector == "S11") & (d_fbs$finpos == "ASS")])
  quarterMatrix[3, 6] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F3") & (d_fbs$sector == "S11") & (d_fbs$finpos == "LIAB")])+sum(d_fbs[[quarter]][(d_fbs$indic_na == "F7") & (d_fbs$sector == "S11") & (d_fbs$finpos == "LIAB")])
  quarterMatrix[3, 7] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F3") & (d_fbs$sector == "S13") & (d_fbs$finpos == "ASS")])+sum(d_fbs[[quarter]][(d_fbs$indic_na == "F7") & (d_fbs$sector == "S13") & (d_fbs$finpos == "ASS")])
  quarterMatrix[3, 8] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F3") & (d_fbs$sector == "S13") & (d_fbs$finpos == "LIAB")])+sum(d_fbs[[quarter]][(d_fbs$indic_na == "F7") & (d_fbs$sector == "S13") & (d_fbs$finpos == "LIAB")])
  quarterMatrix[3, 9] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F3") & (d_fbs$sector == "S2") & (d_fbs$finpos == "ASS")])+sum(d_fbs[[quarter]][(d_fbs$indic_na == "F7") & (d_fbs$sector == "S2") & (d_fbs$finpos == "ASS")])
  quarterMatrix[3, 10] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F3") & (d_fbs$sector == "S2") & (d_fbs$finpos == "LIAB")])+sum(d_fbs[[quarter]][(d_fbs$indic_na == "F7") & (d_fbs$sector == "S2") & (d_fbs$finpos == "LIAB")])
  
  #F4:Loans
  quarterMatrix[4, 1] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F4") & (d_fbs$sector == "S14_S15") & (d_fbs$finpos == "ASS")])
  quarterMatrix[4, 2] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F4") & (d_fbs$sector == "S14_S15") & (d_fbs$finpos == "LIAB")])
  quarterMatrix[4, 3] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F4") & (d_fbs$sector == "S12") & (d_fbs$finpos == "ASS")])
  quarterMatrix[4, 4] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F4") & (d_fbs$sector == "S12") & (d_fbs$finpos == "LIAB")])
  quarterMatrix[4, 5] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F4") & (d_fbs$sector == "S11") & (d_fbs$finpos == "ASS")])
  quarterMatrix[4, 6] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F4") & (d_fbs$sector == "S11") & (d_fbs$finpos == "LIAB")])
  quarterMatrix[4, 7] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F4") & (d_fbs$sector == "S13") & (d_fbs$finpos == "ASS")])
  quarterMatrix[4, 8] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F4") & (d_fbs$sector == "S13") & (d_fbs$finpos == "LIAB")])
  quarterMatrix[4, 9] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F4") & (d_fbs$sector == "S2") & (d_fbs$finpos == "ASS")])
  quarterMatrix[4, 10] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F4") & (d_fbs$sector == "S2") & (d_fbs$finpos == "LIAB")])
  
  #Shares and other equities
  quarterMatrix[5, 1] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F5") & (d_fbs$sector == "S14_S15") & (d_fbs$finpos == "ASS")])
  quarterMatrix[5, 2] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F5") & (d_fbs$sector == "S14_S15") & (d_fbs$finpos == "LIAB")])
  quarterMatrix[5, 3] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F5") & (d_fbs$sector == "S12") & (d_fbs$finpos == "ASS")])
  quarterMatrix[5, 4] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F5") & (d_fbs$sector == "S12") & (d_fbs$finpos == "LIAB")])
  quarterMatrix[5, 5] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F5") & (d_fbs$sector == "S11") & (d_fbs$finpos == "ASS")])
  quarterMatrix[5, 6] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F5") & (d_fbs$sector == "S11") & (d_fbs$finpos == "LIAB")])
  quarterMatrix[5, 7] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F5") & (d_fbs$sector == "S13") & (d_fbs$finpos == "ASS")])
  quarterMatrix[5, 8] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F5") & (d_fbs$sector == "S13") & (d_fbs$finpos == "LIAB")])
  quarterMatrix[5, 9] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F5") & (d_fbs$sector == "S2") & (d_fbs$finpos == "ASS")])
  quarterMatrix[5, 10] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F5") & (d_fbs$sector == "S2") & (d_fbs$finpos == "LIAB")])
  
  #Insurace technical reserves
  quarterMatrix[6, 1] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F6") & (d_fbs$sector == "S14_S15") & (d_fbs$finpos == "ASS")])
  #quarterMatrix[6, 2] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F6") & (d_fbs$sector == "S14_S15") & (d_fbs$finpos == "LIAB")])
  #quarterMatrix[6, 3] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F6") & (d_fbs$sector == "S12") & (d_fbs$finpos == "ASS")])
  quarterMatrix[6, 4] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F6") & (d_fbs$sector == "S12") & (d_fbs$finpos == "LIAB")])-sum(d_fbs[[quarter]][(d_fbs$indic_na == "F6") & (d_fbs$sector == "S12") & (d_fbs$finpos == "ASS")])-sum(d_fbs[[quarter]][(d_fbs$indic_na == "F6") & (d_fbs$sector == "S11") & (d_fbs$finpos == "ASS")])
  #quarterMatrix[6, 5] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F6") & (d_fbs$sector == "S11") & (d_fbs$finpos == "ASS")])
  #quarterMatrix[6, 6] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F6") & (d_fbs$sector == "S11") & (d_fbs$finpos == "LIAB")])
  #quarterMatrix[6, 7] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F6") & (d_fbs$sector == "S13") & (d_fbs$finpos == "ASS")])
  #quarterMatrix[6, 8] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F6") & (d_fbs$sector == "S13") & (d_fbs$finpos == "LIAB")])
  quarterMatrix[6, 9] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F6") & (d_fbs$sector == "S2") & (d_fbs$finpos == "ASS")])-sum(d_fbs[[quarter]][(d_fbs$indic_na == "F6") & (d_fbs$sector == "S2") & (d_fbs$finpos == "LIAB")])
  #quarterMatrix[6, 10] <- sum(d_fbs[[quarter]][(d_fbs$indic_na == "F6") & (d_fbs$sector == "S2") & (d_fbs$finpos == "LIAB")])
  
  #   #for other accounts receivable payable
  #   quarterMatrix[7, 1] <- 
  #   quarterMatrix[7, 2] <- 
  #   quarterMatrix[7, 3] <- 
  #   quarterMatrix[7, 4] <- 
  #   quarterMatrix[7, 5] <- 
  #   quarterMatrix[7, 6] <- 
  #   quarterMatrix[7, 7] <- 
  #   quarterMatrix[7, 8] <- 
  #   quarterMatrix[7, 9] <- +sum(d_fbs[[quarter]][(d_fbs$indic_na == "F7") & (d_fbs$sector == "S2") & (d_fbs$finpos == "ASS")])-sum(d_fbs[[quarter]][(d_fbs$indic_na == "F7") & (d_fbs$sector == "S2") & (d_fbs$finpos == "LIAB")])
  #   quarterMatrix[7, 10] <- 
  #   
  #net financial position
  quarterMatrix[7,2]<-sum(d_fbs[[quarter]][(d_fbs$indic_na=="FA") & (d_fbs$sector=="S14_S15") & (d_fbs$finpos=="ASS")])-sum(d_fbs[[quarter]][(d_fbs$indic_na=="FL") & (d_fbs$sector=="S14_S15") & (d_fbs$finpos=="LIAB")])
  quarterMatrix[7,4]<-sum(d_fbs[[quarter]][(d_fbs$indic_na=="FA") & (d_fbs$sector=="S12") & (d_fbs$finpos=="ASS")])-sum(d_fbs[[quarter]][(d_fbs$indic_na=="FL") & (d_fbs$sector=="S12") & (d_fbs$finpos=="LIAB")])-sum(d_fbs[[quarter]][(d_fbs$indic_na=="F1") & (d_fbs$sector=="S12") & (d_fbs$finpos=="ASS")])
  quarterMatrix[7,6]<-sum(d_fbs[[quarter]][(d_fbs$indic_na=="FA") & (d_fbs$sector=="S11") & (d_fbs$finpos=="ASS")])-sum(d_fbs[[quarter]][(d_fbs$indic_na=="FL") & (d_fbs$sector=="S11") & (d_fbs$finpos=="LIAB")])
  quarterMatrix[7,8]<-sum(d_fbs[[quarter]][(d_fbs$indic_na=="FA") & (d_fbs$sector=="S13") & (d_fbs$finpos=="ASS")])-sum(d_fbs[[quarter]][(d_fbs$indic_na=="FL") & (d_fbs$sector=="S13") & (d_fbs$finpos=="LIAB")])
  quarterMatrix[7,10]<-sum(d_fbs[[quarter]][(d_fbs$indic_na=="FA") & (d_fbs$sector=="S2") & (d_fbs$finpos=="ASS")])-sum(d_fbs[[quarter]][(d_fbs$indic_na=="FL") & (d_fbs$sector=="S2") & (d_fbs$finpos=="LIAB")])
  
  return(quarterMatrix)  
}


#here we define the time period columns that are in our dataset
columnlist_fbs<-names(d_fbs)[!names(d_fbs) %in% c("2014Q1","s_adj","direct","indic_na","sector","geo","unit","finpos","co_nco")]


table_f_bs<-list(.Names=c("assets_hh", "liabilities_hh", "assets_fc", "liabilities_fc", "assets_nfc", "liabilities_nfc", "assets_gg", "liabilities_gg", "assets_row", "liabilities_row"), row.names=c("Loans", "Deposits", "Bonds", "Equities", "Other Accounts P/R"))
for(i in columnlist_fbs){
  #quarter is defined
  quarter <- paste(i,"",sep="")
  #generates the quartermatrix from generate_NFTR for the specific quarter
  quarterMatrix = generate_quarterBS(quarter)
  #defines table_nf_tr<quarter> as the above quartermatrix
  table_f_bs[[as.character(i)]]=quarterMatrix
  #writes the file to a csv format
  write.csv(quarterMatrix,file=paste("TFM_qtr/FBS/table_f_bs_",quarter,".csv",sep=""),append=F)
}




##########################################################################################################################################################################################################################################################################
#GENERATING FTR TFM
##########################################################################################################################################################################################################################################################################
# this function creates a matrix that is filled with values from the financial
# transactions data
generate_quarterFTR<-function(quarter){
  quarterMatrix=matrix(data=0,ncol=10,nrow=7, dimnames=list(c("F21_F22: Cash","F29: Deposits", "F3: Bonds", "F4: Loans", "F5: Equities", "F6: ITRs", "BF90: Net financial Position (balancing liabilities)"),c("assets_hh", "liabilities_hh", "assets_fc", "liabilities_fc", "assets_nfc", "liabilities_nfc", "assets_gg", "liabilities_gg", "assets_row", "liabilities_row")))
  #create d_ftrpoints for each entry
  #F21: Cash
  quarterMatrix[1, 1] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F21_F22") & (d_ftr$sector == "S14_S15") & (d_ftr$finpos == "ASS")])
  quarterMatrix[1, 2] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F21_F22") & (d_ftr$sector == "S14_S15") & (d_ftr$finpos == "LIAB")])
  quarterMatrix[1, 3] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F21_F22") & (d_ftr$sector == "S12") & (d_ftr$finpos == "ASS")])
  quarterMatrix[1, 4] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F21_F22") & (d_ftr$sector == "S12") & (d_ftr$finpos == "LIAB")])
  quarterMatrix[1, 5] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F21_F22") & (d_ftr$sector == "S11") & (d_ftr$finpos == "ASS")])
  quarterMatrix[1, 6] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F21_F22") & (d_ftr$sector == "S11") & (d_ftr$finpos == "LIAB")])
  quarterMatrix[1, 7] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F21_F22") & (d_ftr$sector == "S13") & (d_ftr$finpos == "ASS")])
  quarterMatrix[1, 8] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F21_F22") & (d_ftr$sector == "S13") & (d_ftr$finpos == "LIAB")])
  quarterMatrix[1, 9] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F21_F22") & (d_ftr$sector == "S2") & (d_ftr$finpos == "ASS")])
  quarterMatrix[1, 10] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F21_F22") & (d_ftr$sector == "S2") & (d_ftr$finpos == "LIAB")])
  
  #F21: Deposits
  quarterMatrix[2, 1] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F29") & (d_ftr$sector == "S14_S15") & (d_ftr$finpos == "ASS")])
  quarterMatrix[2, 2] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F29") & (d_ftr$sector == "S14_S15") & (d_ftr$finpos == "LIAB")])
  quarterMatrix[2, 3] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F29") & (d_ftr$sector == "S12") & (d_ftr$finpos == "ASS")])
  quarterMatrix[2, 4] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F29") & (d_ftr$sector == "S12") & (d_ftr$finpos == "LIAB")])
  quarterMatrix[2, 5] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F29") & (d_ftr$sector == "S11") & (d_ftr$finpos == "ASS")])
  quarterMatrix[2, 6] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F29") & (d_ftr$sector == "S11") & (d_ftr$finpos == "LIAB")])
  quarterMatrix[2, 7] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F29") & (d_ftr$sector == "S13") & (d_ftr$finpos == "ASS")])
  quarterMatrix[2, 8] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F29") & (d_ftr$sector == "S13") & (d_ftr$finpos == "LIAB")])
  quarterMatrix[2, 9] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F29") & (d_ftr$sector == "S2") & (d_ftr$finpos == "ASS")])
  quarterMatrix[2, 10] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F29") & (d_ftr$sector == "S2") & (d_ftr$finpos == "LIAB")])
  
  
  #F3: Bonds
  quarterMatrix[3, 1] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F3") & (d_ftr$sector == "S14_S15") & (d_ftr$finpos == "ASS")])+sum(d_ftr[[quarter]][(d_ftr$indic_na == "F7") & (d_ftr$sector == "S14_S15") & (d_ftr$finpos == "ASS")])
  quarterMatrix[3, 2] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F3") & (d_ftr$sector == "S14_S15") & (d_ftr$finpos == "LIAB")])+sum(d_ftr[[quarter]][(d_ftr$indic_na == "F7") & (d_ftr$sector == "S14_S15") & (d_ftr$finpos == "LIAB")])
  quarterMatrix[3, 3] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F3") & (d_ftr$sector == "S12") & (d_ftr$finpos == "ASS")])+sum(d_ftr[[quarter]][(d_ftr$indic_na == "F7") & (d_ftr$sector == "S12") & (d_ftr$finpos == "ASS")])-sum(d_ftr[[quarter]][(d_ftr$indic_na == "F6") & (d_ftr$sector == "S11") & (d_ftr$finpos == "ASS")])+sum(d_ftr[[quarter]][(d_ftr$indic_na == "F1") & (d_ftr$sector == "S12") & (d_ftr$finpos == "ASS")])
  quarterMatrix[3, 4] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F3") & (d_ftr$sector == "S12") & (d_ftr$finpos == "LIAB")])+sum(d_ftr[[quarter]][(d_ftr$indic_na=="F1") & (d_ftr$sector=="S12") & (d_ftr$finpos=="ASS")])+sum(d_ftr[[quarter]][(d_ftr$indic_na == "F7") & (d_ftr$sector == "S12") & (d_ftr$finpos == "LIAB")])
  quarterMatrix[3, 5] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F3") & (d_ftr$sector == "S11") & (d_ftr$finpos == "ASS")])+sum(d_ftr[[quarter]][(d_ftr$indic_na == "F7") & (d_ftr$sector == "S11") & (d_ftr$finpos == "ASS")])+sum(d_ftr[[quarter]][(d_ftr$indic_na == "F6") & (d_ftr$sector == "S11") & (d_ftr$finpos == "ASS")])
  quarterMatrix[3, 6] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F3") & (d_ftr$sector == "S11") & (d_ftr$finpos == "LIAB")])+sum(d_ftr[[quarter]][(d_ftr$indic_na == "F7") & (d_ftr$sector == "S11") & (d_ftr$finpos == "LIAB")])
  quarterMatrix[3, 7] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F3") & (d_ftr$sector == "S13") & (d_ftr$finpos == "ASS")])+sum(d_ftr[[quarter]][(d_ftr$indic_na == "F7") & (d_ftr$sector == "S13") & (d_ftr$finpos == "ASS")])
  quarterMatrix[3, 8] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F3") & (d_ftr$sector == "S13") & (d_ftr$finpos == "LIAB")])+sum(d_ftr[[quarter]][(d_ftr$indic_na == "F7") & (d_ftr$sector == "S13") & (d_ftr$finpos == "LIAB")])
  quarterMatrix[3, 9] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F3") & (d_ftr$sector == "S2") & (d_ftr$finpos == "ASS")])+sum(d_ftr[[quarter]][(d_ftr$indic_na == "F7") & (d_ftr$sector == "S2") & (d_ftr$finpos == "ASS")])
  quarterMatrix[3, 10] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F3") & (d_ftr$sector == "S2") & (d_ftr$finpos == "LIAB")])+sum(d_ftr[[quarter]][(d_ftr$indic_na == "F7") & (d_ftr$sector == "S2") & (d_ftr$finpos == "LIAB")])
  
  #F4:Loans
  quarterMatrix[4, 1] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F4") & (d_ftr$sector == "S14_S15") & (d_ftr$finpos == "ASS")])
  quarterMatrix[4, 2] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F4") & (d_ftr$sector == "S14_S15") & (d_ftr$finpos == "LIAB")])
  quarterMatrix[4, 3] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F4") & (d_ftr$sector == "S12") & (d_ftr$finpos == "ASS")])
  quarterMatrix[4, 4] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F4") & (d_ftr$sector == "S12") & (d_ftr$finpos == "LIAB")])
  quarterMatrix[4, 5] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F4") & (d_ftr$sector == "S11") & (d_ftr$finpos == "ASS")])
  quarterMatrix[4, 6] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F4") & (d_ftr$sector == "S11") & (d_ftr$finpos == "LIAB")])
  quarterMatrix[4, 7] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F4") & (d_ftr$sector == "S13") & (d_ftr$finpos == "ASS")])
  quarterMatrix[4, 8] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F4") & (d_ftr$sector == "S13") & (d_ftr$finpos == "LIAB")])
  quarterMatrix[4, 9] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F4") & (d_ftr$sector == "S2") & (d_ftr$finpos == "ASS")])
  quarterMatrix[4, 10] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F4") & (d_ftr$sector == "S2") & (d_ftr$finpos == "LIAB")])
  
  #Shares and other equities
  quarterMatrix[5, 1] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F5") & (d_ftr$sector == "S14_S15") & (d_ftr$finpos == "ASS")])
  quarterMatrix[5, 2] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F5") & (d_ftr$sector == "S14_S15") & (d_ftr$finpos == "LIAB")])
  quarterMatrix[5, 3] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F5") & (d_ftr$sector == "S12") & (d_ftr$finpos == "ASS")])
  quarterMatrix[5, 4] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F5") & (d_ftr$sector == "S12") & (d_ftr$finpos == "LIAB")])
  quarterMatrix[5, 5] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F5") & (d_ftr$sector == "S11") & (d_ftr$finpos == "ASS")])
  quarterMatrix[5, 6] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F5") & (d_ftr$sector == "S11") & (d_ftr$finpos == "LIAB")])
  quarterMatrix[5, 7] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F5") & (d_ftr$sector == "S13") & (d_ftr$finpos == "ASS")])
  quarterMatrix[5, 8] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F5") & (d_ftr$sector == "S13") & (d_ftr$finpos == "LIAB")])
  quarterMatrix[5, 9] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F5") & (d_ftr$sector == "S2") & (d_ftr$finpos == "ASS")])
  quarterMatrix[5, 10] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F5") & (d_ftr$sector == "S2") & (d_ftr$finpos == "LIAB")])
  
  #Insurace technical reserves
  quarterMatrix[6, 1] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F6") & (d_ftr$sector == "S14_S15") & (d_ftr$finpos == "ASS")])
  #quarterMatrix[6, 2] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F6") & (d_ftr$sector == "S14_S15") & (d_ftr$finpos == "LIAB")])
  #quarterMatrix[6, 3] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F6") & (d_ftr$sector == "S12") & (d_ftr$finpos == "ASS")])
  quarterMatrix[6, 4] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F6") & (d_ftr$sector == "S12") & (d_ftr$finpos == "LIAB")])-sum(d_ftr[[quarter]][(d_ftr$indic_na == "F6") & (d_ftr$sector == "S12") & (d_ftr$finpos == "ASS")])-sum(d_ftr[[quarter]][(d_ftr$indic_na == "F6") & (d_ftr$sector == "S11") & (d_ftr$finpos == "ASS")])
  #quarterMatrix[6, 5] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F6") & (d_ftr$sector == "S11") & (d_ftr$finpos == "ASS")])
  #quarterMatrix[6, 6] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F6") & (d_ftr$sector == "S11") & (d_ftr$finpos == "LIAB")])
  #quarterMatrix[6, 7] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F6") & (d_ftr$sector == "S13") & (d_ftr$finpos == "ASS")])
  #quarterMatrix[6, 8] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F6") & (d_ftr$sector == "S13") & (d_ftr$finpos == "LIAB")])
  quarterMatrix[6, 9] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F6") & (d_ftr$sector == "S2") & (d_ftr$finpos == "ASS")])-sum(d_ftr[[quarter]][(d_ftr$indic_na == "F6") & (d_ftr$sector == "S2") & (d_ftr$finpos == "LIAB")])
  #quarterMatrix[6, 10] <- sum(d_ftr[[quarter]][(d_ftr$indic_na == "F6") & (d_ftr$sector == "S2") & (d_ftr$finpos == "LIAB")])
  
  #   #for other accounts receivable payable
  #   quarterMatrix[7, 1] <- 
  #   quarterMatrix[7, 2] <- 
  #   quarterMatrix[7, 3] <- 
  #   quarterMatrix[7, 4] <- 
  #   quarterMatrix[7, 5] <- 
  #   quarterMatrix[7, 6] <- 
  #   quarterMatrix[7, 7] <- 
  #   quarterMatrix[7, 8] <- 
  #   quarterMatrix[7, 9] <- +sum(d_ftr[[quarter]][(d_ftr$indic_na == "F7") & (d_ftr$sector == "S2") & (d_ftr$finpos == "ASS")])-sum(d_ftr[[quarter]][(d_ftr$indic_na == "F7") & (d_ftr$sector == "S2") & (d_ftr$finpos == "LIAB")])
  #   quarterMatrix[7, 10] <- 
  #   
  #net financial position
  quarterMatrix[7,2]<-sum(d_ftr[[quarter]][(d_ftr$indic_na=="FA") & (d_ftr$sector=="S14_S15") & (d_ftr$finpos=="ASS")])-sum(d_ftr[[quarter]][(d_ftr$indic_na=="FL") & (d_ftr$sector=="S14_S15") & (d_ftr$finpos=="LIAB")])
  quarterMatrix[7,4]<-sum(d_ftr[[quarter]][(d_ftr$indic_na=="FA") & (d_ftr$sector=="S12") & (d_ftr$finpos=="ASS")])-sum(d_ftr[[quarter]][(d_ftr$indic_na=="FL") & (d_ftr$sector=="S12") & (d_ftr$finpos=="LIAB")])-sum(d_ftr[[quarter]][(d_ftr$indic_na=="F1") & (d_ftr$sector=="S12") & (d_ftr$finpos=="ASS")])
  quarterMatrix[7,6]<-sum(d_ftr[[quarter]][(d_ftr$indic_na=="FA") & (d_ftr$sector=="S11") & (d_ftr$finpos=="ASS")])-sum(d_ftr[[quarter]][(d_ftr$indic_na=="FL") & (d_ftr$sector=="S11") & (d_ftr$finpos=="LIAB")])
  quarterMatrix[7,8]<-sum(d_ftr[[quarter]][(d_ftr$indic_na=="FA") & (d_ftr$sector=="S13") & (d_ftr$finpos=="ASS")])-sum(d_ftr[[quarter]][(d_ftr$indic_na=="FL") & (d_ftr$sector=="S13") & (d_ftr$finpos=="LIAB")])
  quarterMatrix[7,10]<-sum(d_ftr[[quarter]][(d_ftr$indic_na=="FA") & (d_ftr$sector=="S2") & (d_ftr$finpos=="ASS")])-sum(d_ftr[[quarter]][(d_ftr$indic_na=="FL") & (d_ftr$sector=="S2") & (d_ftr$finpos=="LIAB")])
  
  return(quarterMatrix)  
}

#here we define the time period columns that are in our dataset
columnlist_ftr<-names(d_ftr)[!names(d_ftr) %in% c("2014Q1","s_adj","direct","indic_na","sector","geo","unit","finpos","co_nco")]


table_f_tr<-list(.Names=c("assets_hh", "liabilities_hh", "assets_fc", "liabilities_fc", "assets_nfc", "liabilities_nfc", "assets_gg", "liabilities_gg", "assets_row", "liabilities_row"), row.names=c("Loans", "Deposits", "Bonds", "Equities", "Other Accounts P/R"))
for(i in columnlist_ftr){
  #quarter is defined
  quarter <- paste(i,"",sep="")
  #generates the quartermatrix from generate_NFTR for the specific quarter
  quarterMatrix = generate_quarterFTR(quarter)
  #defines table_nf_tr<quarter> as the above quartermatrix
  table_f_tr[[as.character(i)]]=quarterMatrix
  #writes the file to a csv format
  write.csv(quarterMatrix,file=paste("TFM_qtr/FTR/table_f_tr_",quarter,".csv",sep=""))
}


#now we must bind the FTR and NFTR csvs to create a proper tfm
#here we load the NFTR and FTR csv files in a list
ed_nftr_fn<-list.files("TFM_qtr/eNFTR", pattern="*.csv", full.names=T)
ftr_fn<-list.files("TFM_qtr/FTR", pattern="*.csv", full.names=T)
nchar(ftr_fn[20])

# this tests what numbers give us teh correct substrings for "edited_nf_tr_"
# and "table_f_tr_" for our list
objname<-substr(ed_nftr_fn[23],15,33)
objname<-substr(ftr_fn[3],13,29)


#we extract the above substrings and assign them the data
for(i in 1:length(ed_nftr_fn)){
  objname<-substr(ed_nftr_fn[i],15,33)
  input<-read.csv(ed_nftr_fn[i])
  assign(objname,input)
}

for(i in 1:length(ftr_fn)){
  objname<-substr(ftr_fn[i],13,29)
  input<-read.csv(ftr_fn[i])
  assign(objname,input)
}

#we stitch together the above data frames for each time period
for(i in columnlist_ftr){
  obj1<-paste("edited_nf_tr_",i,sep="")
  obj2<-paste("table_f_tr_",i,sep="")
  tmp1<-get(obj1)
  tmp2<-get(obj2)
  tmp11<-tmp1[,2:12]
  names(tmp2) <- names(tmp11)
  tfm<-rbind(tmp2,tmp11)
  assign(paste("TFM_",i,sep=""),tfm)
}

#save the tfm files to the TFM folder
for(i in columnlist_ftr){
  obj_name_enf2<-paste("TFM", i, sep="_")
  file_name3<-as.character(paste("TFM_qtr/TFM/", obj_name_enf2, ".csv", sep=""))
  write.csv(get(obj_name_enf2), file_name3,append=F)
} 