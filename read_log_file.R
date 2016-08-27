library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)

file<-"C:\\Work\\nMARQ\\Investigations\\AE\\All Data\\Ospedale F Miulli 525\\525-042_07OCT2014\\ABL6.txt"

#s<-read.table(file,header = FALSE,skip = 6,nrow=3)
#classes <- sapply(s, class)

n_header=6
con <- file(file, "r", blocking = FALSE)
lines<-readLines(con) 
close(con)

#a<-read.table(file,header = FALSE,skip = n_header,fill=TRUE,stringsAsFactors = FALSE, sep='\t')
header<-lines[1:n_header]
footer<-lines[length(lines)]

if (grepl("Ablation", footer)){
     body<-lines[(n_header+1):(length(lines)-1)]
     StopReason<-trimws(str_split(footer,"\t")[[1]][2])
}else{
     body<-lines[(n_header+1):length(lines)]
     StopReason<-NA
}


s<-header[6]
n_elec<-str_count(s,"\\|")

s<-trimws(strsplit(s, "[\\/\\|\t0-9]")[[1]])
s<-s[s!=""]
titles<-s[-c(1:3)]

n_params=(length(titles)-1)/n_elec
titles_single<-titles[1+1:n_params]

for (k in c(1:n_elec)){
     titles[1+(k-1)*n_elec+1:n_elec]<-paste(titles[1+(k-1)*n_elec+1:n_elec],k,sep= "_")
}


m<-matrix(NA, nrow=length(body), ncol=length(titles), byrow=TRUE)

for (k in 1:length(body)){
     s<-trimws(str_split(body[k],"[\\t\\/\\|]")[[1]])
     s<-s[s!=""]
     m[k,] <- as.numeric(s[-1])
}

d<-as.data.frame(m)
names(d)<-titles


#Check for a negative time difference

dt<-diff(d$Time)
while (any(dt<0)){
     ind=which(dt<0)
     stop<-ind[1]
     d$Time[(stop+1):nrow(d)]<-d$Time[(stop+1):nrow(d)]-d$Time[stop+1]+0.06+d$Time[stop]
     #start<-which(d$Time>=d$Time[ind[1]+1])[1]
     #d <-filter(d,row_number()<start | row_number()>stop)
     dt<-diff(d$Time)
}
# indices<-1
# for (k in c(1:n_elec)){
#      indices<-c(indices,params+(k-1)*10+1)
# }
# 
# df<-dplyr::select(d,indices)


dfz<- gather(d, "variable", "value", -1) %>% 
     separate(variable, c("variable", "Electrode"), sep = "_") %>%
     spread(variable, value)

dfz$Electrode<-as.numeric(dfz$Electrode)

dfz<-dplyr::select(dfz,match(c("Electrode", "Time", titles_single), names(dfz))) %>%
     arrange(Electrode, Time)
dfz$Electrode<-as.factor(dfz$Electrode)
