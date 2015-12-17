source("visuals-sankey/sankey.R")

#LOAD DATA FROM WORKING DIRECTORY


load("data-eurostat/long/nasq_10_nf_tr.RData")

#FILTER TO GET IRISH DATA
d_nftr<-subset(nasq_10_nf_tr,geo.time=="IE"&s_adj=="NSA"&unit=="CP_MEUR")
#SORT SO THAT FUTURE TIMES SERIES WILL BE IN ORDER
d_nftr<-d_nftr[order(d_nftr$value,decreasing = T),]

#create sankey folder and subfolders for each sector
dir.create(file.path("visuals-sankey","govt"),recursive=TRUE)
dir.create(file.path("visuals-sankey","hh"),recursive=TRUE)
dir.create(file.path("visuals-sankey","nfc"),recursive=TRUE)
dir.create(file.path("visuals-sankey","fc"),recursive=TRUE)

##acquire government income flows and acquire unique codes
#manually pare down codes
govtcodes<-c("D2","D21","D29","D4","D41","D42","D45","D5","D6","D61","D7","D72","D74_TO_D76","D9","D91","D92_TO_D99","P51C")
item<-c("Taxes on Production",
        "Taxes on Products",
        "Other Taxes on Production",
        "Property Income",
        "Interest",
        "Dividends",
        "Rent",
        "Direct taxes",
        "Welfare",
        "Social Contributions",
        "Other Current transfers",
        "Insurance Claims",
        "Miscellaneous",
        "Capital Transfers",
        "Capital Taxes",
        "Other Capital Transfers",
        "Consumption of Fixed Investment",
        "Total revenue"
)
parent<-c("Total revenue",
          "Taxes on Production",
          "Taxes on Production",
          "Total revenue",
          "Property Income",
          "Property Income",
          "Property Income",
          "Total revenue",
          "Total revenue",
          "Welfare",
          "Total revenue",
          "Other Current transfers",
          "Other Current transfers",
          "Total revenue",
          "Capital Transfers",
          "Capital Transfers",
          "Total revenue",
          ""
)
# In this block of code, we select all income flows for the government in 2004Q2,
# select only the codes used in the above vector, find their sum and match them
# with the items and parent vectors
govt_recv_2004<-d_nftr[d_nftr$direct=="RECV"&d_nftr$sector=="S13"&d_nftr$time=="2004Q2",]
govt_recv_2004<-govt_recv_2004[,c("na_item","value")]
govt_recv_2004<-govt_recv_2004[govt_recv_2004$na_item %in% govtcodes,]
govt_recv_2004<-govt_recv_2004[order(govt_recv_2004$na_item,decreasing = F),]
sumcodes_2004<-govt_recv_2004[govt_recv_2004$na_item %in% c("D2","D4","D5","D6","D7","D9", "P51C"),]
govt_recv_2004["sum",]<-cbind("", sum(sumcodes_2004$value))
govt_recv_2004$parent<-parent
govt_recv_2004$name<-item
govt_recv_2004$na_item<-NULL
govt_recv_2004$value<-as.numeric(govt_recv_2004$value)
govt_recv_2004<-govt_recv_2004[,3:1]

##2007Q2
govt_recv_2007<-d_nftr[d_nftr$direct=="RECV"&d_nftr$sector=="S13"&d_nftr$time=="2007Q2",]
govt_recv_2007<-govt_recv_2007[,c("na_item","value")]
govt_recv_2007<-govt_recv_2007[govt_recv_2007$na_item %in% govtcodes,]
govt_recv_2007<-govt_recv_2007[order(govt_recv_2007$na_item,decreasing = F),]
sumcodes_2007<-govt_recv_2007[govt_recv_2007$na_item %in% c("D2","D4","D5","D6","D7","D9", "P51C"),]
govt_recv_2007["sum",]<-cbind("", sum(sumcodes_2007$value))
govt_recv_2007$parent<-parent
govt_recv_2007$name<-item
govt_recv_2007$na_item<-NULL
govt_recv_2007$value<-as.numeric(govt_recv_2007$value)
govt_recv_2007<-govt_recv_2007[,3:1]

##2010Q2
govt_recv_2010<-d_nftr[d_nftr$direct=="RECV"&d_nftr$sector=="S13"&d_nftr$time=="2010Q2",]
govt_recv_2010<-govt_recv_2010[,c("na_item","value")]
govt_recv_2010<-govt_recv_2010[govt_recv_2010$na_item %in% govtcodes,]
govt_recv_2010<-govt_recv_2010[order(govt_recv_2010$na_item,decreasing = F),]
sumcodes_2010<-govt_recv_2010[govt_recv_2010$na_item %in% c("D2","D4","D5","D6","D7","D9", "P51C"),]
govt_recv_2010["sum",]<-cbind("", sum(sumcodes_2010$value))
govt_recv_2010$parent<-parent
govt_recv_2010$name<-item
govt_recv_2010$na_item<-NULL
govt_recv_2010$value<-as.numeric(govt_recv_2010$value)
govt_recv_2010<-govt_recv_2010[,3:1]

##2015Q2
govt_recv_2015<-d_nftr[d_nftr$direct=="RECV"&d_nftr$sector=="S13"&d_nftr$time=="2015Q2",]
govt_recv_2015<-govt_recv_2015[,c("na_item","value")]
govt_recv_2015<-govt_recv_2015[govt_recv_2015$na_item %in% govtcodes,]
govt_recv_2015<-govt_recv_2015[order(govt_recv_2015$na_item,decreasing = F),]
sumcodes_2015<-govt_recv_2015[govt_recv_2015$na_item %in% c("D2","D4","D5","D6","D7","D9", "P51C"),]
govt_recv_2015["sum",]<-cbind("", sum(sumcodes_2015$value))
govt_recv_2015$parent<-parent
govt_recv_2015$name<-item
govt_recv_2015$na_item<-NULL
govt_recv_2015$value<-as.numeric(govt_recv_2015$value)
govt_recv_2015<-govt_recv_2015[,3:1]

#time to plot our sankey diagrams
#load the modified source code
source("sankey.R")
##We now generate a pdf of a sankey diagram and place it in its proper folder
pdf(file="visuals-sankey/govt/govt_recv_2004.pdf",bg="white",paper="A4r",height=16.54,width = 23.38)
sankey(govt_recv_2004,title ="2004Q2")
dev.off()
pdf(file="visuals-sankey/govt/govt_recv_2007.pdf",bg="white",paper="A4r",height=16.54,width = 23.38)
sankey(govt_recv_2007,title ="2007Q2")
dev.off()
pdf(file="visuals-sankey/govt/govt_recv_2010.pdf",bg="white",paper="A4r",height=16.54,width = 23.38)
sankey(govt_recv_2010,title="2010Q2")
dev.off()
pdf(file="visuals-sankey/govt/govt_recv_2015.pdf",bg="white",paper="A4r",height=16.54,width = 23.38)
sankey(govt_recv_2015,title="2015Q2")
dev.off()

###########################################################################################
#Here we do the same for the household sector
###########################################################################################
#acquire hh_recv flows and acquire unique code
#manually pare down codes
hh_recv_2004<-d_nftr[d_nftr$direct=="RECV"&d_nftr$sector=="S14_S15"&d_nftr$time=="2004Q2",]
hhcodes<-sort(unique(hh_recv_2004$na_item),decreasing = F)
hhcodes<-hhcodes[11:length(hhcodes)]
hhcodes<-hhcodes[!hhcodes %in% c("D41G","D99","D8NET","D42_TO_D45","D75","D72","D74_TO_D76")]
# create item columns for each code, describing them
hhitem<-c("Wages",
          "Subsidies on Production",
          "Subsidies on Products",
          "Property Income",
          "Interest",
          "Dividends",
          "Reinvested Earnings on FDI",
          "Insurance policy property income",
          "Rent",
          "Welfare",
          "Social Contributions",
          "Social Benefits",
          "Social transfers in kind",
          "Other Current Transfers",
          "Pension Adjustments",
          "Capital Transfers",
          "Capital Taxes",
          "Other Capital Transfers",
          "Consumption of Fixed Investment",
          "Total revenue"
)

#create a parent column for each code, describing the code's "parent".
#the bottom code will have nothing, as it will be the sum value that we ill create later
hhparent<-c("Total revenue",
            "Total revenue",
            "Subsidies on Production",
            "Total revenue",
          "Property Income",
          "Property Income",
          "Property Income",
          "Property Income",
          "Property Income",
          "Total revenue",
          "Welfare",
          "Welfare",
          "Welfare",
          "Total revenue",
          "Total revenue",
          "Total revenue",
          "Capital Transfers",
          "Capital Transfers",
          "Total revenue",
          ""
)


hh_recv_2004<-d_nftr[d_nftr$direct=="RECV"&d_nftr$sector=="S14_S15"&d_nftr$time=="2004Q2",]
hh_recv_2004<-hh_recv_2004[,c("na_item","value")]
hh_recv_2004<-hh_recv_2004[hh_recv_2004$na_item %in% hhcodes,]
hh_recv_2004<-hh_recv_2004[order(hh_recv_2004$na_item,decreasing = F),]
sumcodes_2004<-hh_recv_2004[hh_recv_2004$na_item %in% c("D1","D4","D3","D8","D6","D7","D9", "P51C"),]
hh_recv_2004["sum",]<-cbind("", sum(sumcodes_2004$value))
hh_recv_2004$parent<-hhparent
hh_recv_2004$name<-hhitem
hh_recv_2004$na_item<-NULL
hh_recv_2004$value<-as.numeric(hh_recv_2004$value)
hh_recv_2004<-hh_recv_2004[,3:1]

hh_recv_2007<-d_nftr[d_nftr$direct=="RECV"&d_nftr$sector=="S14_S15"&d_nftr$time=="2007Q2",]
hh_recv_2007<-hh_recv_2007[,c("na_item","value")]
hh_recv_2007<-hh_recv_2007[hh_recv_2007$na_item %in% hhcodes,]
hh_recv_2007<-hh_recv_2007[order(hh_recv_2007$na_item,decreasing = F),]
sumcodes_2007<-hh_recv_2007[hh_recv_2007$na_item %in% c("D1","D4","D3","D8","D6","D7","D9", "P51C"),]
hh_recv_2007["sum",]<-cbind("", sum(sumcodes_2007$value))
hh_recv_2007$parent<-hhparent
hh_recv_2007$name<-hhitem
hh_recv_2007$na_item<-NULL
hh_recv_2007$value<-as.numeric(hh_recv_2007$value)
hh_recv_2007<-hh_recv_2007[,3:1]

hh_recv_2010<-d_nftr[d_nftr$direct=="RECV"&d_nftr$sector=="S14_S15"&d_nftr$time=="2010Q2",]
hh_recv_2010<-hh_recv_2010[,c("na_item","value")]
hh_recv_2010<-hh_recv_2010[hh_recv_2010$na_item %in% hhcodes,]
hh_recv_2010<-hh_recv_2010[order(hh_recv_2010$na_item,decreasing = F),]
sumcodes_2010<-hh_recv_2010[hh_recv_2010$na_item %in% c("D1","D4","D3","D8","D6","D7","D9", "P51C"),]
hh_recv_2010["sum",]<-cbind("", sum(sumcodes_2010$value))
hh_recv_2010$parent<-hhparent
hh_recv_2010$name<-hhitem
hh_recv_2010$na_item<-NULL
hh_recv_2010$value<-as.numeric(hh_recv_2010$value)
hh_recv_2010<-hh_recv_2010[,3:1]

hh_recv_2015<-d_nftr[d_nftr$direct=="RECV"&d_nftr$sector=="S14_S15"&d_nftr$time=="2015Q2",]
hh_recv_2015<-hh_recv_2015[,c("na_item","value")]
hh_recv_2015<-hh_recv_2015[hh_recv_2015$na_item %in% hhcodes,]
hh_recv_2015<-hh_recv_2015[order(hh_recv_2015$na_item,decreasing = F),]
sumcodes_2015<-hh_recv_2015[hh_recv_2015$na_item %in% c("D1","D4","D3","D8","D6","D7","D9", "P51C"),]
hh_recv_2015["sum",]<-cbind("", sum(sumcodes_2015$value))
hh_recv_2015$parent<-hhparent
hh_recv_2015$name<-hhitem
hh_recv_2015$na_item<-NULL
hh_recv_2015$value<-as.numeric(hh_recv_2015$value)
hh_recv_2015<-hh_recv_2015[,3:1]

##We now generate a pdf of a sankey diagram and place it in its proper folder
pdf(file="visuals-sankey/hh/hh_recv_2004.pdf",bg="white",paper="A4r",height=16.54,width = 23.38)
sankey(hh_recv_2004,title ="2004Q2")
dev.off()
pdf(file="visuals-sankey/hh/hh_recv_2007.pdf",bg="white",paper="A4r",height=16.54,width = 23.38)
sankey(hh_recv_2007,title ="2007Q2")
dev.off()
pdf(file="visuals-sankey/hh/hh_recv_2010.pdf",bg="white",paper="A4r",height=16.54,width = 23.38)
sankey(hh_recv_2010,title="2010Q2")
dev.off()
pdf(file="visuals-sankey/hh/hh_recv_2015.pdf",bg="white",paper="A4r",height=16.54,width = 23.38)
sankey(hh_recv_2015,title="2015Q2")
dev.off()

#############################################################################################
#Financial sector
#############################################################################################
fccodes<-sort(unique(fc_recv_2004$na_item),decreasing = F)
fccodes<-fccodes[11:length(fccodes)]
fccodes<-fccodes[!fccodes %in% c("D41G","D99","D8NET","D43S2I","D43S21","D43S22","D43S2X","D42_TO_D45","D75","D72","D74_TO_D76","EMP_HW_DC","EMP_PS_DC","D71")]
fcitem<-c(
          "Subsidies on Production",
          "Subsidies on Products",
          "Property Income",
          "Interest",
          "Dividends",
          "Reinvested Earnings on FDI",
          "Insurance policy property income",
          "Rent",
          "Welfare",
          "Social Contributions",
          "Other Current Transfers",
          "Capital Transfers",
          "Capital Taxes",
          "Other Capital Transfers",
          "Consumption of Fixed Investment",
          "Total revenue"
)

fcparent<-c(
            "Total revenue",
            "Subsidies on Production",
            "Total revenue",
            "Property Income",
            "Property Income",
            "Property Income",
            "Property Income",
            "Property Income",
            "Total revenue",
            "Welfare",
            "Total revenue",
            "Total revenue",
            "Capital Transfers",
            "Capital Transfers",
            "Total revenue",
            ""
)


fc_recv_2004<-d_nftr[d_nftr$direct=="RECV"&d_nftr$sector=="S12"&d_nftr$time=="2004Q2",]
fc_recv_2004<-fc_recv_2004[,c("na_item","value")]
fc_recv_2004<-fc_recv_2004[fc_recv_2004$na_item %in% fccodes,]
fc_recv_2004<-fc_recv_2004[order(fc_recv_2004$na_item,decreasing = F),]
sumcodes_2004<-fc_recv_2004[fc_recv_2004$na_item %in% c("D1","D4","D3","D6","D7","D9", "P51C"),]
fc_recv_2004["sum",]<-cbind("", sum(sumcodes_2004$value))
fc_recv_2004$parent<-fcparent
fc_recv_2004$name<-fcitem
fc_recv_2004$na_item<-NULL
fc_recv_2004$value<-as.numeric(fc_recv_2004$value)
fc_recv_2004<-fc_recv_2004[,3:1]

fc_recv_2007<-d_nftr[d_nftr$direct=="RECV"&d_nftr$sector=="S12"&d_nftr$time=="2007Q2",]
fc_recv_2007<-fc_recv_2007[,c("na_item","value")]
fc_recv_2007<-fc_recv_2007[fc_recv_2007$na_item %in% fccodes,]
fc_recv_2007<-fc_recv_2007[order(fc_recv_2007$na_item,decreasing = F),]
sumcodes_2007<-fc_recv_2007[fc_recv_2007$na_item %in% c("D1","D4","D3","D6","D7","D9", "P51C"),]
fc_recv_2007["sum",]<-cbind("", sum(sumcodes_2007$value))
fc_recv_2007$parent<-fcparent
fc_recv_2007$name<-fcitem
fc_recv_2007$na_item<-NULL
fc_recv_2007$value<-as.numeric(fc_recv_2007$value)
fc_recv_2007<-fc_recv_2007[,3:1]

fc_recv_2010<-d_nftr[d_nftr$direct=="RECV"&d_nftr$sector=="S12"&d_nftr$time=="2010Q2",]
fc_recv_2010<-fc_recv_2010[,c("na_item","value")]
fc_recv_2010<-fc_recv_2010[fc_recv_2010$na_item %in% fccodes,]
fc_recv_2010<-fc_recv_2010[order(fc_recv_2010$na_item,decreasing = F),]
sumcodes_2010<-fc_recv_2010[fc_recv_2010$na_item %in% c("D1","D4","D3","D6","D7","D9", "P51C"),]
fc_recv_2010["sum",]<-cbind("", sum(sumcodes_2010$value))
fc_recv_2010$parent<-fcparent
fc_recv_2010$name<-fcitem
fc_recv_2010$na_item<-NULL
fc_recv_2010$value<-as.numeric(fc_recv_2010$value)
fc_recv_2010<-fc_recv_2010[,3:1]

fc_recv_2015<-d_nftr[d_nftr$direct=="RECV"&d_nftr$sector=="S12"&d_nftr$time=="2015Q2",]
fc_recv_2015<-fc_recv_2015[,c("na_item","value")]
fc_recv_2015<-fc_recv_2015[fc_recv_2015$na_item %in% fccodes,]
fc_recv_2015<-fc_recv_2015[order(fc_recv_2015$na_item,decreasing = F),]
sumcodes_2015<-fc_recv_2015[fc_recv_2015$na_item %in% c("D1","D4","D3","D6","D7","D9", "P51C"),]
fc_recv_2015["sum",]<-cbind("", sum(sumcodes_2015$value))
fc_recv_2015$parent<-fcparent
fc_recv_2015$name<-fcitem
fc_recv_2015$na_item<-NULL
fc_recv_2015$value<-as.numeric(fc_recv_2015$value)
fc_recv_2015<-fc_recv_2015[,3:1]

##We now generate a pdf of a sankey diagram and place it in its proper folder
pdf(file="visuals-sankey/fc/fc_recv_2004.pdf",bg="white",paper="A4r",height=16.54,width = 23.38)
sankey(fc_recv_2004,title ="2004Q2")
dev.off()
pdf(file="visuals-sankey/fc/fc_recv_2007.pdf",bg="white",paper="A4r",height=16.54,width = 23.38)
sankey(fc_recv_2007,title ="2007Q2")
dev.off()
pdf(file="visuals-sankey/fc/fc_recv_2010.pdf",bg="white",paper="A4r",height=16.54,width = 23.38)
sankey(fc_recv_2010,title="2010Q2")
dev.off()
pdf(file="visuals-sankey/fc/fc_recv_2015.pdf",bg="white",paper="A4r",height=16.54,width = 23.38)
sankey(fc_recv_2015,title="2015Q2")
dev.off()



#############################################################################################
#non-Financial sector
#############################################################################################

#############################################################################################
nfc_recv_2004<-d_nftr[d_nftr$direct=="RECV"&d_nftr$sector=="S11"&d_nftr$time=="2004Q2",]
nfccodes<-sort(unique(nfc_recv_2004$na_item),decreasing = F)
nfccodes<-nfccodes[11:length(nfccodes)]
nfccodes<-nfccodes[!nfccodes %in% c("D41G","D99","D8NET","D43S2I","D43S21","D43S22","D43S2X","D42_TO_D45","D75","D72","D74_TO_D76","EMP_HW_DC","EMP_PS_DC","D71")]
nfcitem<-c(
  "Subsidies on Production",
  "Subsidies on Products",
  "Property Income",
  "Interest",
  "Dividends",
  "Reinvested Earnings on FDI",
  "Insurance policy property income",
  "Rent",
  "Welfare",
  "Social Contributions",
  "Other Current Transfers",
  "Capital Transfers",
  "Capital Taxes",
  "Other Capital Transfers",
  "Consumption of Fixed Investment",
  "Total revenue"
)

nfcparent<-c(
  "Total revenue",
  "Subsidies on Production",
  "Total revenue",
  "Property Income",
  "Property Income",
  "Property Income",
  "Property Income",
  "Property Income",
  "Total revenue",
  "Welfare",
  "Total revenue",
  "Total revenue",
  "Capital Transfers",
  "Capital Transfers",
  "Total revenue",
  ""
)


nfc_recv_2004<-d_nftr[d_nftr$direct=="RECV"&d_nftr$sector=="S11"&d_nftr$time=="2004Q2",]
nfc_recv_2004<-nfc_recv_2004[,c("na_item","value")]
nfc_recv_2004<-nfc_recv_2004[nfc_recv_2004$na_item %in% nfccodes,]
nfc_recv_2004<-nfc_recv_2004[order(nfc_recv_2004$na_item,decreasing = F),]
sumcodes_2004<-nfc_recv_2004[nfc_recv_2004$na_item %in% c("D1","D4","D3","D6","D7","D9", "P51C"),]
nfc_recv_2004["sum",]<-cbind("", sum(sumcodes_2004$value))
nfc_recv_2004$parent<-nfcparent
nfc_recv_2004$name<-nfcitem
nfc_recv_2004$na_item<-NULL
nfc_recv_2004$value<-as.numeric(nfc_recv_2004$value)
nfc_recv_2004<-nfc_recv_2004[,3:1]

nfc_recv_2007<-d_nftr[d_nftr$direct=="RECV"&d_nftr$sector=="S11"&d_nftr$time=="2007Q2",]
nfc_recv_2007<-nfc_recv_2007[,c("na_item","value")]
nfc_recv_2007<-nfc_recv_2007[nfc_recv_2007$na_item %in% nfccodes,]
nfc_recv_2007<-nfc_recv_2007[order(nfc_recv_2007$na_item,decreasing = F),]
sumcodes_2007<-nfc_recv_2007[nfc_recv_2007$na_item %in% c("D1","D4","D3","D6","D7","D9", "P51C"),]
nfc_recv_2007["sum",]<-cbind("", sum(sumcodes_2007$value))
nfc_recv_2007$parent<-nfcparent
nfc_recv_2007$name<-nfcitem
nfc_recv_2007$na_item<-NULL
nfc_recv_2007$value<-as.numeric(nfc_recv_2007$value)
nfc_recv_2007<-nfc_recv_2007[,3:1]

nfc_recv_2010<-d_nftr[d_nftr$direct=="RECV"&d_nftr$sector=="S11"&d_nftr$time=="2010Q2",]
nfc_recv_2010<-nfc_recv_2010[,c("na_item","value")]
nfc_recv_2010<-nfc_recv_2010[nfc_recv_2010$na_item %in% nfccodes,]
nfc_recv_2010<-nfc_recv_2010[order(nfc_recv_2010$na_item,decreasing = F),]
sumcodes_2010<-nfc_recv_2010[nfc_recv_2010$na_item %in% c("D1","D4","D3","D6","D7","D9", "P51C"),]
nfc_recv_2010["sum",]<-cbind("", sum(sumcodes_2010$value))
nfc_recv_2010$parent<-nfcparent
nfc_recv_2010$name<-nfcitem
nfc_recv_2010$na_item<-NULL
nfc_recv_2010$value<-as.numeric(nfc_recv_2010$value)
nfc_recv_2010<-nfc_recv_2010[,3:1]

nfc_recv_2015<-d_nftr[d_nftr$direct=="RECV"&d_nftr$sector=="S11"&d_nftr$time=="2015Q2",]
nfc_recv_2015<-nfc_recv_2015[,c("na_item","value")]
nfc_recv_2015<-nfc_recv_2015[nfc_recv_2015$na_item %in% nfccodes,]
nfc_recv_2015<-nfc_recv_2015[order(nfc_recv_2015$na_item,decreasing = F),]
sumcodes_2015<-nfc_recv_2015[nfc_recv_2015$na_item %in% c("D1","D4","D3","D6","D7","D9", "P51C"),]
nfc_recv_2015["sum",]<-cbind("", sum(sumcodes_2015$value))
nfc_recv_2015$parent<-nfcparent
nfc_recv_2015$name<-nfcitem
nfc_recv_2015$na_item<-NULL
nfc_recv_2015$value<-as.numeric(nfc_recv_2015$value)
nfc_recv_2015<-nfc_recv_2015[,3:1]

sankey(nfc_recv_2004)
source("sankey.R")

##We now generate a pdf of a sankey diagram and place it in its proper folder
pdf(file="visuals-sankey/nfc/nfc_recv_2004.pdf",bg="white",paper="A4r",height=16.54,width = 23.38)
sankey(nfc_recv_2004,title ="2004Q2")
dev.off()
pdf(file="visuals-sankey/nfc/nfc_recv_2007.pdf",bg="white",paper="A4r",height=16.54,width = 23.38)
sankey(nfc_recv_2007,title ="2007Q2")
dev.off()
pdf(file="visuals-sankey/nfc/nfc_recv_2010.pdf",bg="white",paper="A4r",height=16.54,width = 23.38)
sankey(nfc_recv_2010,title="2010Q2")
dev.off()
pdf(file="visuals-sankey/nfc/nfc_recv_2015.pdf",bg="white",paper="A4r",height=16.54,width = 23.38)
sankey(nfc_recv_2015,title="2015Q2")
dev.off()

