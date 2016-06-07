pollutantmean <- function(dir,con,n=1:332)
{
t = getwd()
F_list  <- NULL
dir_p <-paste(t,dir,sep="/")
for (i in n)
{
z <- (paste("00" ,i,".csv" ,sep =""))
F_list <-c(F_list,paste(dir_p,substr(z,nchar(z)-6, nchar(z)),sep="/"))
}
csv_tab <-lapply(F_list,read.csv)
tab <- do.call(rbind,csv_tab)
poll_m <- tab[[con]]
round(mean(poll_m, na.rm = TRUE),digits =3)
}

setwd("/Users/ranjeetapegu/Documents/Coursera/R Programming/R Development")

complete <- function (dir,n) {
library(plyr)
 t = getwd()
F_list  <- NULL
library(plyr)
dir_p <-paste(t,dir,sep="/")
for (i in n)
{
z <- (paste("00" ,i,".csv" ,sep =""))
F_list <-c(F_list,paste(dir_p,substr(z,nchar(z)-6, nchar(z)),sep="/"))
}
csv_tab <-lapply(F_list,read.csv)
tab <- do.call(rbind,csv_tab)
tab1 <- subset(tab, sulfate !="NA" & nitrate !="NA")
t <- count(tab1, "ID")
print(t)
}

corr <- function (dir,n1=0) {
library(plyr)
t = getwd()
F_list  <- NULL
dir_p <-paste(t,dir,sep="/")
F_list <-dir(dir_p,pattern = '\\.csv',full.names=TRUE)
csv_tab <-lapply(F_list,read.csv)
tab <- do.call(rbind,csv_tab)
tab1 <- subset(tab, sulfate !="NA" & nitrate !="NA")
tab2 <- count(tab1, "ID")
tab3 <- subset(tab2, freq > n1,select = c(ID))
p <- NULL
 for (j in 1:nrow(tab3)){
   tab4 <- subset(tab1, ID == tab3[j,1])
   x<-tab4[2]
   y <-tab4[3]
   p <- c(p,round(cor(x,y),digits=5))
}
p
}

