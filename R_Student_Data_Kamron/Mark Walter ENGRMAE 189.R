# source("D:/R Code/RcodeSource.r")

# Replace your directory 
data <- read.csv("D:/Data/Mark Walter ENGRMAE 189.csv", header = TRUE)
data[1:10,]

data$ethnic.cd <- gsub(" ", "", data$ethnic.cd)

data$account <- 1
data$mae <- 0
data$mae[c(which(data$major.code == "2"), which(data$major.code == "277"))] <- 1

newdata <- data[data$mae == 1,]

# install the doBy package before you run
library(doBy)

length(unique(data$studentid))

my.summary <- summaryBy(units ~ studentid, FUN = sum, data = data, id = ~pascd)
my.summary[1:10,]

boxplot(units.sum ~ factor(pascd), data = my.summary, main = "All Students")

table(my.summary$units.sum, my.summary$pascd)

get.table2 <- function(x, y){
t.1 <- table(x,y)
col.total <- apply(t.1, 2, sum)
t.rf <- round(t.1/col.total*100,2)
t.out <- cbind(t.1[,1], t.rf[,1], t.1[,2], t.rf[,2])
colnames(t.out) <- c(colnames(t.1)[1], "percent", colnames(t.1)[2], "percent")
t.out
}

get.table2(my.summary$units.sum, my.summary$pascd)

new.summary <- summaryBy(units ~ studentid + level, FUN = sum, data = data, id = ~pascd)
new.summary[1:10,]

table(data$major.code)

newdata2 <- data[data$mae == 0,]
my.summary <- summaryBy(units ~ studentid, FUN = sum, data = newdata2, id = ~pascd)
my.summary[1:10,]

windows()
boxplot(units.sum ~ factor(pascd), ylim = c(0,20), data = my.summary, main = "non-MAE Students")



my.summary <- summaryBy(units ~ studentid, FUN = sum, data = newdata, id = ~pascd)
my.summary[1:10,]

windows()
boxplot(units.sum ~ factor(pascd),  ylim = c(0,20), data = my.summary, main = "MAE Students")

table(my.summary$units.sum, my.summary$pascd)

get.table2 <- function(x, y){
t.1 <- table(x,y)
col.total <- apply(t.1, 2, sum)
t.rf <- round(t.1/col.total*100,2)
t.out <- cbind(t.1[,1], t.rf[,1], t.1[,2], t.rf[,2])
colnames(t.out) <- c(colnames(t.1)[1], "percent", colnames(t.1)[2], "percent")
t.out
}

get.table2(my.summary$units.sum, my.summary$pascd)

new.summary <- summaryBy(units ~ studentid + level, FUN = sum, data = data, id = ~pascd)
new.summary[1:10,]

table(data$major.code)
