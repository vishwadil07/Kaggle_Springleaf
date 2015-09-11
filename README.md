# Kaggle_Springleaf

############ Clear Environments #########

rm(list = ls())
dev.off()

############ Getting/Setting working directory #############

getwd()
setwd("Z:/Team/Individual/Dileep/SpringLeaf/SpringLeaf")

######## Libraries to be load ###############

library(dplyr)
library(data.table)
library(car)
library(reshape)
library(plyr)


######## Functions ######################

mod.freq <- function(df){
  
  df <- as.data.frame(df)
  a <- numeric()
  
  for(i in 1:length(df)){
    a[i] <- max(tabulate(match(df[,i], 
                               unique(df[,i]))))
  }
  
  return(a)
}

MODE <- function(dataframe){
  DF <- as.data.frame(dataframe)
  
  MODE2 <- function(x){      
    if (is.numeric(x) == FALSE){
      df <- as.data.frame(table(x))  
      df <- df[order(df$Freq), ]         
      m <- max(df$Freq)        
      MODE1 <- as.vector(as.character(subset(df, Freq == m)[, 1]))
      
      if (sum(df$Freq)/length(df$Freq)==1){
        warning("No Mode: Frequency of all values is 1", call. = FALSE)
      }else{
        return(MODE1)
      }
      
    }else{ 
      df <- as.data.frame(table(x))  
      df <- df[order(df$Freq), ]         
      m <- max(df$Freq)        
      MODE1 <- as.vector(as.numeric(as.character(subset(df, Freq == m)[, 1])))
      
      if (sum(df$Freq)/length(df$Freq)==1){
        warning("No Mode: Frequency of all values is 1", call. = FALSE)
      }else{
        return(MODE1)
      }
    }
  }
  
  return(as.vector(lapply(DF, MODE2)))
}


#test <- fread("D:/Study/Learning/Kaggle/Springleaf Marketing Response/Recieved/test.csv/test.csv", header = T, sep = ',')

#setwd("D:/Study/Learning/Kaggle/Springleaf Marketing Response/Working")

#load("D:/Study/Learning/Kaggle/Springleaf Marketing Response/Recieved/train.csv/train.RData")

load("Z:/Team/Individual/Dileep/SpringLeaf/SpringLeaf/Workspace.RData")

######### Splitting data according to type of variables #############

train_numr = train[, sapply(train, is.numeric)]
train_char = train[, sapply(train, is.character)]

capture.output(glimpse(train_numr), file = "glimpse.train_numr.txt")
capture.output(summary(train_numr), file = "summary.train_numr.txt")

capture.output(glimpse(train_char), file = "glimpse.train_char.txt")
capture.output(summary(train_char), file = "summary.train_char.txt")

train.short.numr <- as.data.frame(train_numr[1:5000,])
train.short.char <- as.data.frame(train_char[1:5000,])


train_numr_tb <- as.data.table(train_numr)

decr <- train_numr_tb %>%
      summarise_each(funs(min(.,na.rm = T), max(.,na.rm = T), mean(.,na.rm = T),  
             median(.,na.rm = T), sd(.,na.rm = T)), train_numr_tb[,2:length(train_numr_tb)])

decr <- as.data.frame(decr)

Var.min <- t(select(decr,ends_with("_min")))
Var.max <- t(select(decr,ends_with("_max")))
Var.mean <- t(select(decr,ends_with("_mean")))
Var.med <- t(select(decr,ends_with("_median")))
Var.sd <- t(select(decr,ends_with("_sd")))
Var.mod.freq <- mod.freq(train_numr[,2:length(train_numr)])
var.mod.val <- MODE(train_numr[,2:length(train_numr)])

var.mod.val.df <- do.call(rbind.data.frame, var.mod.val)

write.csv(var.mod.val.df,"var.mod.val.df.csv")

ms1 <- seq(1,1871,10)
ms2 <- seq(10,1880,10)

miss <- rep(list(numeric()),188)

for(i in 1:188){
  miss[i] <- list(apply(train_numr[ ,ms1[i]:ms2[i]], 2, function(x){sum(is.na(x))}))
}

m <- matrix( ,nrow = 10,ncol = 188)

for(i in 1:188){
  for(j in 1:10){
    m[j,i] <- miss[[i]][j][[1]]
  }
}

miss.count <- melt(m)

v.names <- colnames(train_numr[,1:1880])
row.names(miss.count) <- v.names

decriptive <- cbind(Var.min,Var.max,Var.mean,Var.med,var.mod.val.df)

min.df <- as.numeric(min)
max.df <- as.data.frame(max)

load("Workspace.RData")

# Getting unique numbers of all variables
s1 <- seq(1,1921,10)
s2 <- seq(10,1930,10)

rm(df)

uniq <- rep(list(list()),193)
for(i in 1:193){
  uniq[i] <- list(apply(train[ ,s1[i]:s2[i]], 2, function(x){unique(x)}))
}


#Unable to create a list of 2.1 GB
# uniq1 <- list()
# uniq1 <- list(apply(train, 2, function(x){unique(x)}))

u <- matrix( ,nrow = 10,ncol = 193)

for(i in 1:193){
  for(j in 1:10){
    u[j,i] <- length(uniq[[i]][j][[1]])
  }
}

uniq.count <- melt(u)
v.names <- colnames(train[,1:1930])
row.names(uniq.count) <- v.names
uniq.count <- uniq.count[,2:3]

############# Remaining three variables #############

var.mean <- apply()
uniq.rem <- list(apply(train[ ,1931:1934], 2, function(x){unique(x)}))

View(unique(train[,2:4]))s



############ Getting/Looking outputs ##############

rm()
View(uniq.count)
capture.output(uniq, file = "unique.txt")
write.csv(uniq.count,"unique.count.csv",row.names = T)

