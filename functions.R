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
              Annotation="character",
              Data="data.frame"
         )
)

read_all_ablations<-function(files){
     
     #files <- list.files(path, full.names = T, pattern=".*ABL.*txt")
     
     dfs<-NULL
     for (file in files){
          print(file)
          Abl<-read_log_file(file)
          df<-Abl@Data
          S <- data.frame(AblNum=as.factor(Abl@AblNum), 
                          Date=Abl@Date, 
                          #SW_version=Abl@SW_version,
                          #HW_version=Abl@HW_version,
                          MaxDuration=Abl@MaxDuration, 
                          #AblDuration=Abl@Data$Time[nrow(Abl@Data)],
                          #PreAblTime=Abl@Data$Time[1],
                          Catheter=Abl@Catheter,
                          AblMode=Abl@Mode,
                          StopReason=Abl@StopReason
                          #Annotation=Abl@Annotation
          )
          
          S<-S[rep(1,nrow(df)),] 
          dfs<-rbind(dfs, cbind(S, df))
     }
     return(dfs)
     
}

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
     
     if (grepl("Ablation", footer) | grepl("Error", footer)){
          body<-lines[(n_header+1):(length(lines)-1)]
          Abl@StopReason<-trimws(str_split(footer,"\t")[[1]][2])
     }else{
          body<-lines[(n_header+1):length(lines)]
          Abl@StopReason<-"Unspecified"
     }
     
     Abl@Annotation=""
     
     s<-header[6]
     n_elec<-str_count(s,"\\|")
     
     Abl@ElecNum<-n_elec
     
     s<-trimws(strsplit(s, "[\\/\\|\t0-9]")[[1]])
     s<-s[s!=""]
     titles<-s[-c(1:3)]
     
     n_params=(length(titles)-1)/n_elec
     titles_single<-titles[1+1:n_params]
     
     isdup<-duplicated(titles_single)
     dup<-unique(titles_single[isdup])
     
     if (any(isdup)){
          for (d in dup){
               ind<-which(titles_single==d)
               for (k in 1:length(ind)){
                    titles_single[ind[k]]<-paste0(titles_single[ind[k]],k)
               }
          }
          
     }
     
     titles<-rep(titles_single,n_elec)
     
     for (k in c(1:n_elec)){
          titles[(k-1)*n_elec+1:n_elec]<-paste(titles[(k-1)*n_elec+1:n_elec],k,sep= "_")
     }
     
     titles<-c("Time",titles)
     
   
     #d<-as.data.frame(setNames(replicate(length(titles),numeric(0), simplify = F), titles))
     #d<-cbind(data.frame(TS=character(), stringsAsFactors = F), d)
     ts<-NULL
     
     
     m<-matrix(NA, nrow=length(body), ncol=length(titles), byrow=TRUE)
     
     for (k in 1:length(body)){
          s<-trimws(str_split(body[k],"[\\t\\/\\|]")[[1]])
          s<-s[s!=""]
          m[k,] <- as.numeric(s[-1])
          ts<-c(ts, s[1])
     }
     
     d<-as.data.frame(m)
     names(d)<-titles
     
     ts<-stri_replace_last_fixed(ts,pattern = ":", replacement = ".")
     op <- options(digits.secs = 3)
     ts<-strptime(ts, "%H:%M:%OS")
     dts<-round(diff(ts),3)
     dts[dts==0]<-0.001
     
     d$Time<-cumsum(c(d$Time[1], dts))
     options(op)
     
     
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


# abl_summary <- function(Abl){
#      
#      df<-Abl@Data
#      S <- data.frame(AblNum=Abl@AblNum, 
#                      Date=Abl@Date, 
#                      SW_version=Abl@SW_version,
#                      HW_version=Abl@HW_version,
#                      MaxDuration=Abl@MaxDuration, 
#                      AblDuration=Abl@Data$Time[nrow(Abl@Data)],
#                      PreAblTime=Abl@Data$Time[1],
#                      Catheter=Abl@Catheter,
#                      AblMode=Abl@Mode,
#                      StopReason=Abl@StopReason,
#                      Annotation=Abl@Annotation
#                      )
#      
#      S<-S[rep(1,nrow(df)),] 
     
 
#}