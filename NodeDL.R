library(data.table)
library(dplyr)


#tags is now an argument in loadnodes function
tags <- read.csv("C:/Users/eliwi/OneDrive/Documents/PrairieDog/PrairieDogCollars.csv")
#make sure tags column is named "TagId"##########
colnames(tags)[1] <- "TagId"
infile <- "C:/Users/eliwi/OneDrive/Documents/R/NodeDL/NodeTest"
t <- 1209600

#############get range of dates you want to filter by#########################
startvec <- seq.POSIXt(from = as.POSIXct("2020-07-15 00:00:00", tz= "GMT") ,to = as.POSIXct("2020-09-15 00:00:00", tz= "GMT"),by='2 weeks')
#Date <- as.POSIXct("2020-07-15", format='%Y-%m-%d')


###################load node data function############################
#must specify either t or endvec depending on how you want to split up incoming data. t is in seconds
#such as t=1209600 for 2 week intervals and endvec is user-specified intervals of differing lengths used
#in situations where perhaps nodes were rearranged
LoadNodes <- function(infile, startvec, t=NULL, tags, endvec=NULL) {
  files <- list.files(infile, pattern = "beep*", full.names = TRUE, recursive = TRUE)
  Sam3 <- lapply(files, function(x) {
    df <- tryCatch({
      if (file.size(x) > 0) {
        read.csv(x,as.is=TRUE, na.strings=c("NA", ""))
      }}, error = function(err) {
        # error handler picks up where error was generated
        print(paste("Read.table didn't work!:  ",err))
      })
    #if(!all((c("time", "id", "rssi") %in% colnames(df)))) {df <- NULL}
    df <- df[df$id %in% tags$TagId,]
    df$time <- as.POSIXct(df$time,format="%Y-%m-%dT%H:%M:%SZ" , time="GMT")
    df$filepath <- dirname(x)
    #direcnames <- basename(dirname(x))
    #breaks beep data into timeframes 
    if (is.null(t)){
      DateRanges <- data.frame("Start"=startvec, "End"=endvec)
      df <- lapply(1:nrow(DateRanges), function(i) {
        df[which(df$time >= DateRanges[i, "Start"] &
                   df$time <= DateRanges[i, "End"]), , drop = FALSE]})   
    }
    else {df <- lapply(startvec, function(x) subset(df, time >= x & time <= x + t))}
    return(df)
    })
  
Sam3 <- Sam3[!duplicated(Sam3)]
files <- files[!duplicated(Sam3)]

#name nested list, rbinds by common timespan, adds other columns

nodenames <- sapply (files, function (x) {
            if (file.size(x) > 0) {basename(dirname(x))}
            })
if (is.null(endvec)){varB = sapply(nodenames, function (x) paste(x, format(as.Date(startvec), "%m-%d"), format(as.Date(startvec + 60*60*24*14), "%m-%d"),sep = "."),simplify = "vector")}
else{varB=sapply(nodenames, function (x) paste(x, format(as.Date(startvec), "%m-%d"), format(as.Date(endvec), "%m-%d"),sep = "."),simplify = "vector")}
p <- unlist(Sam3, recursive=FALSE)
#set names of node-date dataframes
p <- setNames(p,as.vector(varB))
#make names for 2nd level list, just date
if (is.null(endvec)){varC <-paste(format(as.Date(startvec), "%m-%d"), format(as.Date(startvec + t), "%m-%d"),sep = ".")}
else{varC <-paste(format(as.Date(startvec), "%m-%d"), format(as.Date(endvec), "%m-%d"),sep = ".")}
#rearrange into lists based on time frame, unnesting list
lst1 <- split(p, varC)
lst2 <- lapply(lst1, function (x) data.table::rbindlist(x,idcol = "file"))

lst2 <- lapply(lst2, function (x) {x$RadioId <- 4
                                    colnames(x)[c(2,3,4)] <- c("DateTime","TagId", "TagRSSI")
                                    x$Validated <- 0
                                    ;x})
lst2 <- lapply(lst2, function (x) x[!duplicated(x)])
return(lst2)}

#get rid of empty dataframe############################################################
df.list[which(lapply(df.list, nrow) != 0)]

#run as function!######################################################################
x <- LoadNodes(infile=infile, startvec=startvec,t=1209600, tags = tags)


######################scrap paper, ignore###########################################
main_data = data.frame(Day=c(1:100000))

spans_to_filter = 
  data.frame(Span_number = c(1:9),
             Start = c(2,7,1,15,12,23,90,9000,50000),
             End = c(5,10,4,18,15,26,100,9100,50100))

x <- Sam3[[1]][Sam3[[1]]$id %in% tags$TagId,]
y <- Sam3[[2]][Sam3[[2]]$id %in% tags$TagId,]
range(Sam3[[1]]$Date)
range(Sam3[[2]]$Date)

df <- lapply(startvec, function(x) subset(Sam3[[1]], time >= x & time <= x + 60*60*24*7))

c <- object.size(Sam3)
d <- object.size(Sam3)

varA = paste0("varA", 1:10)
varB = paste0("varB", 1:3)

library(foreach)
tabs = foreach(j = 1:length(varA)) %do% {
  main = varA[j]
  mytabs = lapply(1:length(varB), class)
}
tabs <- setNames(lapply(tabs, setNames, varB), varA)

lapply(Sam3, function(x) ls(x, pattern = varC))

p <- unlist(Sam3, recursive=TRUE)
p <- setNames(p,as.vector(varB))
str1 <- names(p)
#belwo this works
lst1 <- split(p, varC)
lst2 <- lapply(lst1, function (x) data.table::rbindlist(x,idcol = "file"))

lst2 <- lapply(lst1, function(x) do.call(rbind, p))
        lapply(lst1, function(sublst) do.call(rbind, sublst))


Pattern1<-grep("local",names(.GlobalEnv),value=TRUE)
Patterns <- lapply(str1, grep)
Pattern1_list<-do.call("list",mget(Pattern1))

g2 <- lapply(split(str1, varC), function(x) do.call(rbind, p))
library(tidyverse)

g2 <- map(split(str1, names(g)), bind_cols)

g2 <- lapply(varC, function (x) grep(p, pattern = x))


y <- Sam3[[1]]
z <- Sam3[[2]]


y <- y[y$id %in% tags$TagId,]
z <- z[z$id %in% tags$TagId,]

y$time <- as.POSIXct(y$time,format="%Y-%m-%dT%H:%M:%SZ" , time="UTC")
z$time <- as.POSIXct(z$time,format="%Y-%m-%dT%H:%M:%SZ" , time="UTC")

y <- y[y$time > start[1] &  y$time < end[1],]
z <- z[z$time > start &  z$time < end,]

range(y$time)

Sam3 <- lapply(files, function(x) {
  df <- tryCatch({
    if (file.size(x) > 0) {
      read.csv(x,as.is=TRUE, na.strings=c("NA", ""))
    }}, error = function(err) {
      # error handler picks up where error was generated
      print(paste("Read.table didn't work!:  ",err))
    })
})


Sam4 <- lapply(Sam3, function (x) {
  df <- x[x$id %in% tags$TagId,]
})
