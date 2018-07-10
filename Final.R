library(data.table)
library(reshape2)
require(magrittr); require(tidyr)

library(plyr)
library(plotly)
library(wnominate)

Sys.setlocale("LC_ALL", "Czech_Czech Republic.1250")


download.file("https://www.psp.cz/eknih/cdrom/opendata/poslanci.zip", destfile="poslanci.zip")
unzip(zipfile="poslanci.zip")

download.file("https://www.psp.cz/eknih/cdrom/opendata/hl-2013ps.zip", destfile="osoby.zip")
unzip(zipfile="osoby.zip")
osoby2<-read.table("poslanec.unl",sep = "|", fileEncoding = "Windows-1250")

osoby <-read.table("osoby.unl",sep = "|", fileEncoding = "Windows-1250")

#osoby<-fread("osoby.unl",sep = "|")


osoby$V10 <- NULL
osoby$V9 <- NULL
osoby$V8 <- NULL
osoby$V7 <- NULL
osoby$V6 <- NULL
osoby$V5 <- NULL
osoby$V2 <- NULL

osoby_ready <- within(osoby, V3 <- paste(V3, V4, sep=' '))
osoby_ready$V4 <- NULL


hlasovani<-read.table("hl2013h1.unl",sep = "|", fileEncoding = "Windows-1250")
hlasovani$V4 <- NULL



data_wide <- dcast.data.table(hlasovani, V1 ~ V2, value.var="V3",drop = FALSE)



test<-data.frame(data_wide$V1)


#test2<-data.frame(rename(test, c("data_wide.V1"="V1")))

test2 <- setnames(test, "data_wide.V1", "V1")


merge <- merge(test2,osoby2, by = c('V1'),all.x =TRUE)


merge[5:16] <- NULL  
merge$V3 <- NULL  

organy<-fread("organy.unl",sep = "|")
organy$V2 <- NULL
organy$V3 <- NULL 

organy$V5 <- NULL 
organy$V6 <- NULL 
organy$V7 <- NULL 
organy$V8 <- NULL 
organy$V9 <- NULL 
organy$V10 <- NULL 
organy$V11 <- NULL 

#organy_ready <-rename(organy, c("V4"="V3"))

organy_ready <- setnames(organy, "V4", "V3")



merge_se_stranou <- merge(merge,organy_ready, by.x = "V4", by.y = "V1",all.x =TRUE)



merge_osob <- merge(merge_se_stranou, osoby_ready, by.x = "V2", by.y = "V1",all.x =TRUE)

merge_osob$V2<-NULL
merge_osob$V4<-NULL

#merge_osob_cisto<-rename(merge_osob, c("data_wide.V1"="V1"))



hotovo <- merge(merge_osob,data_wide, by = c('V1'),all.x =TRUE)

UN<-hotovo
UN$V1<-NULL

UNnames<-UN$V3.y
UN$V3.y<-NULL
legData <-UN$V3.x
#legData<-rename(legData, c("V3.x"="party"))

legData <- matrix(UN[,1], length(UN[, 1]), 1)  
colnames(legData) <- "party"


UN$V3.x<-NULL

UN[is.na(UN)] <- 0



rc <- rollcall(UN, yea = "A", nay = "B", missing = c("C", "F", "@", "K", "W","0"), legis.names = UNnames,  legis.data = legData, desc = "TEST")


result <- wnominate(rc, polarity = c(1, 1))

#plot.coords(result) 

options(encoding = "Windows-1250")

pal <- c("brown", "orange", "#106F2B", "red", "blue", "purple", "#FF00DA" )
p <- plot_ly( hoverinfo ="text" , text = ~paste(UNnames), symbol = I("square"), y = result$legislators$coord1D, x = -(result$legislators$coord2D), type = 'scatter', colors = pal, color = result$legislators$party, mode = 'markers')
p



