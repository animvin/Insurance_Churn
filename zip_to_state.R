library(readxl)
library(zipcode)
data("zipcode")

data_zip <- read_excel("E:/Travelers/zipcodes.xlsx",col_names = FALSE)
colnames(data_zip) <- c("zip","seq")

getState <- function(x){
  ifelse(x==11111,"Unkown",zipcode[zipcode$zip==x,]$state)
}


getCity <- function(x){
  ifelse(x==11111,"Unkown",zipcode[zipcode$zip==x,]$city)
}

t1=unique(data_zip$zip)
t2=data.frame("zip"=t1)

t3_1=data.frame(t2,apply(t2,1,getCity),apply(t2,1,getState))
colnames(t3) <- c("zip","city","state")
op1=merge(t3[, c("zip", "city","state")], data_zip[, c("zip", "seq")], by="zip")

op = data.frame(op$seq,op$zip,"Location"=paste(op$city,op$state, sep=","))
op=op[order(op$op.seq),]

write.csv(op,"zipcode-to-city.csv")

#______________________________________________________________________________________________________________________________________

data_zip <- read_excel("E:/Travelers/test_zipcodes.xlsx", 
                       col_names = FALSE)
colnames(data_zip) <- c("zip","seq")
t1=unique(data_zip$zip)
t2=data.frame("zip"=t1)

t3=data.frame(t2,apply(t2,1,getCity),apply(t2,1,getState))
colnames(t3) <- c("zip","city","state")
op=merge(t3[, c("zip", "city","state")], data_zip[, c("zip", "seq")], by="zip")

op = data.frame(op$seq,op$zip,"Location"=paste(op$city,op$state, sep=","))
op=op[order(op$op.seq),]

write.csv(op,"zipcode-to-cityfor-test.csv")
