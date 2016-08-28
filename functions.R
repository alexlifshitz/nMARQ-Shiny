library(lubridate)
library(plotly)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)

setClass(Class="ablation",
         representation(
              AblNum="numeric",
              Date="POSIXct",
              SW_version="character",
              HW_version="character",
              Mode="character",
              Catheter="character",
              MaxDuration="numeric",
              ElecNum="numeric",
              StopReason="character",
              Data="data.frame"
         )
)


read_log_file <- function(path) {
     
     Abl<-new("ablation")
     
     n_header<-6
     con <- file(path, "r", blocking = FALSE)
     lines<-readLines(con) 
     close(con)
     
     header<-lines[1:n_header]
     
     r <- str_match(header[1], "number (\\d*?) started")
     Abl@AblNum<-as.numeric(r[2])
     
     r <- str_match(header[1], "at (.*?)\t")
     Abl@Date<-mdy_hms(r[2])
     
     r <- str_match(header[1], "Version Ablator (.*?) ")
     Abl@SW_version<-r[2]
     
     r <- str_match(header[1], "(S_.*)")
     Abl@HW_version<-r[2]
     
     r <- str_match(header[2], "Catheter[:blank:]+(.+?)[:blank:]+\t")
     Abl@Catheter<-gsub("[^0-9A-Za-z///' ]", "", r[2])
     
     r <- str_match(header[3], "Polarity:[:blank:]+(.*?)\t")
     Abl@Mode<-r[2]
     
     r <- str_match(header[3], "Duration:[:blank:]+([:digit:]+)")
     Abl@MaxDuration<-as.numeric(r[2])
     
     footer<-lines[length(lines)]
     
     if (grepl("Ablation", footer)){
          body<-lines[(n_header+1):(length(lines)-1)]
          Abl@StopReason<-trimws(str_split(footer,"\t")[[1]][2])
     }else{
          body<-lines[(n_header+1):length(lines)]
          Abl@StopReason<-"Unspecified"
     }
     
     
     s<-header[6]
     n_elec<-str_count(s,"\\|")
     
     Abl@ElecNum<-n_elec
     
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
     
     dfz<- gather(d, "variable", "value", -1) %>% 
          separate(variable, c("variable", "Electrode"), sep = "_") %>%
          spread(variable, value)
     
     dfz$Electrode<-as.numeric(dfz$Electrode)
     
     dfz<-dplyr::select(dfz,match(c("Electrode", "Time", titles_single), names(dfz))) %>%
          arrange(Electrode, Time)
     dfz$Electrode<-as.factor(dfz$Electrode)
     
     Abl@Data<-dfz
     return(Abl)
     
}


abl_summary <- function(Abl){
     
     df<-Abl@Data
     S <- data.frame(AblNum=Abl@AblNum, 
                     Date=Abl@Date, 
                     SW_version=Abl@SW_version,
                     HW_version=Abl@HW_version,
                     MaxDuration=Abl@MaxDuration, 
                     AblDuration=Abl@Data$Time[nrow(Abl@Data)],
                     PreAblTime=Abl@Data$Time[1],
                     Catheter=Abl@Catheter,
                     AblMode=Abl@Mode,
                     StopReason=Abl@StopReason
                     )
     
     S<-S[rep(1,Abl@ElecNum),] 
     
     
     # Mode="character",
     # Catheter="character",
     # 
     # ElecNum="numeric",
     #
     # Data="data.frame"
     
     
}