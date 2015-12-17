#load the XLCOnnect package
library(XLConnect)
library(RCurl)

#define the url and download the file to your data folder
url<-"http://www.centralbank.ie/polstats/stats/qfaccounts/Documents/ie_qfa_publication_tables.xls"
download.file(url,destfile="data-eurostat/cb_qfa.xls")
#workbook function creates a workbook object

wb <- loadWorkbook("data-eurostat/cb_qfa.xls")
#readworksheet converts wb to a large list
QFA = readWorksheet(wb, sheet = getSheets(wb))

#this code shaves off the first 6 sheets in your list, as they are not needed
names(QFA)
QFA[[1]]<-NULL
QFA[[1]]<-NULL
QFA[[1]]<-NULL
QFA[[1]]<-NULL
QFA[[1]]<-NULL
QFA[[1]]<-NULL
#make sure all sectors are there!
names(QFA[[2]])




#this code pares down your data frame so that only assets are shown
#lapply applies this to all data frames in your list

#IF WORKBOOK IS UPDATED, UPDATE THE COLUMNS IN THIS BLOCK OF CODE BY ANALYSING
#THE STRUCTURE OF THE SHEETS

CB_ASSETS<-lapply(QFA, function(x) {
x <- x[-c(1:5,7:8,58:nrow(x)),-c(1)]
x[1,]<-gsub("AF.","F",x[1,])
names(x) <- as.character(x[1,])
x <- x[-c(1),-c(1)]
rownames(x)<-c("2002Q1","2002Q2","2002Q3","2002Q4","2003Q1","2003Q2","2003Q3","2003Q4","2004Q1","2004Q2","2004Q3","2004Q4","2005Q1","2005Q2","2005Q3","2005Q4","2006Q1","2006Q2","2006Q3","2006Q4","2007Q1","2007Q2","2007Q3","2007Q4","2008Q1","2008Q2","2008Q3","2008Q4","2009Q1","2009Q2","2009Q3","2009Q4","2010Q1","2010Q2","2010Q3","2010Q4","2011Q1","2011Q2","2011Q3","2011Q4","2012Q1","2012Q2","2012Q3","2012Q4","2013Q1","2013Q2","2013Q3","2013Q4","2014Q1")
x
})

#this code pares down your data frame so that only liabilites are shown
#lapply applies this to all data frames in your list

#IF WORKBOOK IS UPDATED, UPDATE THE COLUMNS IN THIS BLOCK OF CODE BY ANALYSING
#THE STRUCTURE OF THE SHEETS

CB_LIAB<-lapply(QFA, function(x) {
  x <- x[c(67,70:nrow(x)),-c(1)]
  x[1,]<-gsub("AF.","F",x[1,])
  names(x) <- as.character(x[1,])
  x <- x[-c(1),-c(1)]
  rownames(x)<-c("2002Q1","2002Q2","2002Q3","2002Q4","2003Q1","2003Q2","2003Q3","2003Q4","2004Q1","2004Q2","2004Q3","2004Q4","2005Q1","2005Q2","2005Q3","2005Q4","2006Q1","2006Q2","2006Q3","2006Q4","2007Q1","2007Q2","2007Q3","2007Q4","2008Q1","2008Q2","2008Q3","2008Q4","2009Q1","2009Q2","2009Q3","2009Q4","2010Q1","2010Q2","2010Q3","2010Q4","2011Q1","2011Q2","2011Q3","2011Q4","2012Q1","2012Q2","2012Q3","2012Q4","2013Q1","2013Q2","2013Q3","2013Q4","2014Q1")
  x
  })


# we transform the list into seperate data frames 
# and distinguish between assets and liabilities
for( i in names(CB_ASSETS)){
  name<-paste(i,"A",sep="_")
  y<-CB_ASSETS[[i]]
  y[,]<-as.numeric(gsub(",", "", as.matrix(y)))
  assign(name,y)
}

for( i in names(CB_LIAB)){
  name<-paste(i,"L",sep="_")
  y<-CB_LIAB[[i]]
  y[,]<-as.numeric(gsub(",", "", as.matrix(y)))
  assign(name,y)
}

#The financial sector is disaggregated so here we aggregate
FC_BS_A<-MFI_BS_A+OFI_BS_A+ICPF_BS_A
FC_BS_L<-MFI_BS_L+OFI_BS_L+ICPF_BS_L
FC_Trans_A<-MFI_Trans_A+OFI_Trans_A+ICPF_Trans_A
FC_Trans_L<-MFI_Trans_L+OFI_Trans_L+ICPF_Trans_L

#remove FC subsectors
rm(list = ls(pattern = "MFI_"))
rm(list = ls(pattern = "OFI_"))
rm(list = ls(pattern = "ICPF_"))

#classfilter for dataframes we created
df_ClassFilter <- function(x) inherits(get(x), 'data.frame' )

#get list of data frames in a vector called Objs
Objs<-Filter(df_ClassFilter,ls())

# For all data frames in Objs, we transpose the data frames 
# and add appropriate columns
for(i in 1:length(Objs)){
  x<-get(paste(Objs[i],sep=""))
  x<-data.frame(t(x))
  names(x)<-gsub("X","",names(x))
  names(x)<-gsub(" ","",names(x))
  name<-Objs[i]
  x$geo<-rep("IE",nrow(x))
  x$unit<-rep("MIO_NAC",nrow(x))
  x$co_nco<-rep("NCO",nrow(x))
  x$indic_na<-row.names(x)
  x$indic_na<-gsub("/","_F",x$indic_na)
  x$indic_na<-gsub("\\.","",x$indic_na)
  
  if(grepl('_A',name)){
  x$finpos<-rep("ASS",nrow(x))
  }else{
    if(grepl('_L',name)){
      x$finpos<-rep("LIAB",nrow(x))
    }
  }
  
  if(grepl('NFC',name)){
    x$sector<-rep("S12",nrow(x))
  }else{
    if(grepl('FC',name)){
      x$sector<-rep("S11",nrow(x))
    }else{
      if(grepl('HH',name)){
        x$sector<-rep("S14_S15",nrow(x))
      }else{
        if(grepl('GG',name)){
          x$sector<-rep("S13",nrow(x))
        }else{
          if(grepl('ROW',name)){
            x$sector<-rep("S2",nrow(x))
          }
        }
      }
    }
  }
  
assign(name,x)
}

#From our Objs vector, we identify all Balance sheet data frames
d_fbs_names<-grep(pattern = "_BS_",Objs,value=T)
#this line turns them into a list and rbinds them into our fbs data frame
d_fbs<-do.call(rbind,lapply(d_fbs_names,get))

#From our Objs vector, we identify all Balance sheet data frames
d_ftr_names<-grep(pattern = "_Trans_",Objs,value=T)
#this line turns them into a list and rbinds them into our fbs data frame
d_ftr<-do.call(rbind,lapply(d_ftr_names,get))

#here we save our central bank data
save(d_fbs,file="data-eurostat/d_fbs_cb.RData")
save(d_ftr,file = "data-eurostat/d_ftr_cb.RData")
