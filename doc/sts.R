data1 <- read.csv("/Users/zhuoyikang/elua/doc/async.csv",sep=";")
data2 <- read.csv("/Users/zhuoyikang/elua/doc/sync.csv",sep=";")

plot(data1$id,data1$time,col="red")
par(new=TRUE)
plot(data2$id,data2$time,col="green")
