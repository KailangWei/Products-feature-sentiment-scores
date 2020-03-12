### data preparation
library(stringr)
library(data.table)
library(readr)
library(sqldf)
library(dplyr)

# data import
data_t <- unique(X000000_0) 
rm(X000000_0)
data_t <- as.data.frame(sapply(data_t,  function(x) {gsub("!","",x)}))
datai<-data_t

data_t <- read_delim("C:/Users/14702/OneDrive/Desktop/Data_Inventory/000023_0", 
                        "~", escape_double = FALSE, col_names = FALSE, 
                        trim_ws = TRUE)

data_t <- unique(data_t) 
data_t <- as.data.frame(sapply(data_t,  function(x) {gsub("!","",x)}))
datai<-rbind(datai, data_t)
datai = unique(datai)

names(data) <- c("sku", "model",	"name",	"brand",	"store_id",	"calendar_dt",	"inv_5(Inventory at 5 AM)",	
"inv_10(Inventory at 10 AM)",	"inv_15(Inventory at 3 PM)",	
"inv_20Inventory at 8 PM)",	"price",	"sales_units",	"web_url",	"store_name",	
"city", "state",	"zip",	"is_main",	"street",	"phone",	"type")

### omit similar lines

write.csv(data, file = "C:/Users/14702/OneDrive/Desktop/inventory_data.csv",row.names=FALSE)

# inventory data cleaning
data <- data[!str_detect(data$price, 'N'),]
data <- data[!str_detect(data$sales_units, 'N'),]
store_6378<-data[data$store_id=='6378']
x<-store_6378[store_6378$sku=='205334546']
x<-x[order(x$calendar_dt),]
sku_205334546 <- data[data$sku == "205334546"]

write.csv(sku_205334546, file = "C:/Users/14702/OneDrive/Desktop/sku_205334546.csv",row.names=FALSE)


# data cleaning
data<-unique(data)
data<-na.omit(data)

data <- data[data$stars==1 | data$stars==2 | data$stars==3 | data$stars==4 | data$stars==5,]
data <- data[str_detect(data$fullcategory, '->'),]
data <- data[data$text !='Rating provided by a verified purchaser',]
data <- data[!str_detect(data$caption, 'Star Review'),]

# different online retails
data <- data.table(data)
data<-data[,retail:=ifelse(str_detect(data$weburl, 'amazon'),'amazon',
                           ifelse(str_detect(data$weburl, 'homedepot'),'homedepot',
                                  ifelse(str_detect(data$weburl, 'walmart'),'walmart','lowes')))]
nrow(data[data$retail=='lowes'])
nrow(data[data$retail=='amazon'])
nrow(data[data$retail=='homedepot'])
nrow(data[data$retail=='walmart'])

write.csv(data, file = "C:/Users/14702/OneDrive/Desktop/SBD_data.csv")

data[,sku_number:=.N,by=c('sku')]
sku_cnt <- unique(data[,c('sku','sku_number')])
sku_cnt <- sku_cnt[order(sku_cnt[,2],decreasing=T),]
hist(sku_cnt$sku_number)
sku_overcnt <- sku_cnt[sku_cnt$sku_number>100,]
hist(sku_overcnt$sku_number)

nrow(data[data$brand=='STANLEY'])
nrow(data[data$brand=='Stanley FatMax'])

data<-data[, brand_binary:=ifelse(brand=='STANLEY'|brand=='Stanley FatMax'|brand=='DEWALT'|
                                    brand=='Black & Decker'|brand=='Craftsman'|
                                    brand=='IRWIN'|brand=='Porter Cable'|brand=='LENOX'|
                                    brand=='BOSTITCH'|brand=='PROTO', 1,0)]
nrow(data[data$brand_binary==1])
nrow(data[data$category=='Tape Measures'])

# Tape Measures
tape_measures <- data[category=='Tape Measures']
nrow(tape_measures[brand_binary==1])

write.csv(tape_measures, file = "C:/Users/14702/OneDrive/Desktop/tapemeasure_data.csv")

# find the models in competitor dataset
SBD<-data.table(unique(SBDCompetitorsProducts$`SBD Part #`))
setkeyv(SBD,c('sku'))
setkeyv(data,c('model'))
zdata<-data[SBD,nomatch=0]
zdata<-na.omit(zdata)

# find the competitor 
competitor<-data.table(unique(SBDCompetitorsProducts$`Cross Matching Data: Competitive Part Number`))
setkeyv(competitor,c('V1'))
setkeyv(data,c('model'))
ydata<-data[competitor,nomatch=0]
ydata<-na.omit(ydata)

# find 
cp<-data.table(SBDCompetitorsProducts[,c(1,3,5)])
setkeyv(cp,c('`Cross Matching Data: Competitive Part Number`'))
setkeyv(data,c('model'))
xdata<-data[cp,nomatch=0]
xdata<-na.omit(xdata)

setkeyv(xdata,c('`SBD Part #`'))
setkeyv(data,c('model'))
wdata<-xdata[data,nomatch=0]
wdata<-wdata[category==i.category]

# average rating
wdata[,sbdavg := mean(as.numeric(as.character(i.stars))), by=c('`SBD Part #`')]
wdata[,cpavg := mean(as.numeric(as.character(stars))), by=c('model')]
# wdata[,sbdcmt := paste(unique(as.character(text))),by=c('`SBD Part #`')]

dt<-wdata[,c('model','brand','text','retail','sku_number','cpavg','i.sku','SBD.Part..','Strength.of.Match','i.text','i.retail','i.sku_number','sbdavg')]
dt<-unique(dt)

dt[,sbdcmt := paste(unique(as.character(dt[,dt$i.text]))),by=c('SBD.Part..')]
nrow(dt)

textfull=vector()
dt$cpcmt='NULL'
dt$sbdcmt='NULL'

for (i in 1:196122){
  for (j in 2:196122){
    if (dt[i,1] == dt[j,1] & dt[i,3]!=dt[j,3]){
      textfull <- paste(textfull,dt[j,3],sep = "| ")
    }
  }
  textfull <- paste(textfull,dt[i,3],sep = "| ")
  dt[i,c('cpcmt')] = textfull
  print(i)
  textfull=vector()
}

colnames(dt)<-c('cp_model','cp_brand','cp_cmt','cp_retail','cp_sku_number','cp_rate','sbd_sku','sbd_model','match','sbd_cmt','sbd_retail','sbd_sku_number','sbd_rate','cpcmt_all','sbdcmt_all')

write.csv(dt, file = "C:/Users/14702/OneDrive/Desktop/SBD_Competitors.csv",row.names=FALSE)

# category level 
data = data[complete.cases(category)]
data = data[-which(substr(category, start = 1, stop = 1)=='$'),] 

data[,SBDsum:= sum(brand_binary), by=c("category")]

data_sbd = data[SBDsum!=0,]

sbd_cmt = data_sbd[,c("model","brand","text","category","stars","retail","sku_number","brand_binary","SBDsum")]
write.csv(sbd_cmt, file = "C:/Users/14702/OneDrive/Desktop/sbd_cmt.csv",row.names=FALSE)

competitor = read.csv("C:/Users/14702/OneDrive/Desktop/SBD data manipulation/SBD_Competitors.csv")






