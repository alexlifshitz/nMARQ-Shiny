library(lubridate)
library(plotly)
library(stringr)
library(stringi)
library(dplyr)
library(tidyr)
library(ggplot2)
library(pracma)
library(mailR)


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

setClass(Class="case",
         representation(
              Name="character",
              Description="character",
              Physician="character",
              Site="character",
              Date="POSIXct",
              SW_version="character",
              HW_version="character",
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
     k<-0
     while(!grepl("\\d+:\\d+:\\d+:\\d+",footer)){
          k<-k+1
          footer<-lines[length(lines)-k]
     }
     
     if (grepl("Ablation", footer) | grepl("Error", footer)){
          start <- n_header+1
          stop <-length(lines)-k-1
          if (start<=stop){
               body<-lines[start:stop]     
          }else{
               body<-NULL
          }
          
          Abl@StopReason<-trimws(str_split(footer,"\t")[[1]][2])
     }else{
          
          start <-n_header+1
          stop <-length(lines)-k
          if (start<=stop){
               body<-lines[start:stop]     
          }else{
               body<-NULL
          }
          Abl@StopReason<-"Unspecified"
     }
     
     
     Abl@Annotation<-""
     
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
          titles[(k-1)*n_params+1:n_params]<-paste(titles[(k-1)*n_params+1:n_params],k,sep= "_")
     }
     
     titles<-c("Time",titles)
     
   
     if(is.null(body)){
          Abl@Annotation<-"No data in the log file"
          Abl@Data<-data.frame(data=NULL)
          return(Abl)
     }
     
     ts<-NULL
     
     
     m<-matrix(NA, nrow=length(body), ncol=length(titles), byrow=TRUE)
     
     
     err<-NULL
     
     for (k in 1:length(body)){
          s<-trimws(str_split(body[k],"[\\t\\/\\|]")[[1]])
          s<-s[s!=""]
          
          if (length(s)!=length(titles)+1){
               err<-c(err,k)
               ts<-c(ts, NA)
               Abl@Annotation<-"Log file contains error"
               next
          }else{
               m[k,] <- as.numeric(s[-1])
               ts<-c(ts, s[1])
          }
     }
     
     d<-as.data.frame(m)
     names(d)<-titles
     
     if (length(err)>0){
          d<-d[-err,]
          ts<-ts[-err]
     }
     
     
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
     
     dfz<-mutate(dfz, Current=sqrt(Pow/Imp), Voltage=sqrt(Pow*Imp))
     
     Abl@Data<-dfz
     return(Abl)
     
}
 

df2Ablation <- function(df) {
  
     Abl<-new("ablation")
     
     Abl@AblNum<-as.numeric(as.character(df$AblNum[1]))
     Abl@Date<-df$Date[1]
     Abl@SW_version<-as.character(df$SW_version[1])
     Abl@HW_version<-as.character(df$HW_version[1])
     Abl@Mode<-as.character(df$AblMode[1])
     Abl@Catheter<-as.character(df$Catheter[1])
     Abl@MaxDuration<-df$MaxDuration[1]
     Abl@ElecNum<-nlevels(df$Electrode)
     Abl@StopReason<-as.character(df$StopReason[1])
     Abl@Annotation<-as.character(df$Annotation[1])
     Abl@Data<-dplyr::select(df, -c(AblNum, 
                                    Date, 
                                    SW_version,
                                    HW_version,
                                    MaxDuration, 
                                    Catheter,
                                    AblMode,
                                    StopReason,
                                    Annotation))
     
     return(Abl)
     
}


low<-function(x) {quantile(x, 0.05, na.rm=T)}
high<-function(x) {quantile(x, 0.95, na.rm=T)}


average_inrange <-function(x, t, range){
     
     if (range[1]>=range[2]){
          return(NA)
     }
     
     start<-which(t>=range[1])[1]
     stop<- tail(which(t<=range[2]),1)
     
     if (is.na(start) | is.na(stop)){
          return(NA)
     }else{
          return(mean(x[start:stop]))
     }
     
}

elec_summary <- function(Abl, Elec, Tmin=0, Tmax=Inf){

     dff<-filter(Abl@Data, Time>=Tmin, Time<=Tmax) %>% 
          group_by(Electrode) %>% 
          summarize_each_(funs("max","high","mean", "median", "low", "min"),c("Pow", "Temp", "Imp")) %>%
          filter(Electrode==Elec) 
     
     dfz<- gather(dff, "variable", "value", -1) %>% 
          separate(variable, c("variable", "Function"), sep = "_") %>%
          spread(variable, value)
     dfz<-dplyr::select(dfz,-Electrode) %>% arrange(desc(Temp))
     return(dfz)
     
}

param_summary <- function(Abl, param,Tmin=0, Tmax=Inf){
     
     
     dff<-filter(Abl@Data, Time>=Tmin, Time<=Tmax) %>% group_by(Electrode) %>% dplyr::select_("Electrode", param) %>%
          summarize_each_(funs("max","high","mean", "median", "low", "min"),param) 
          
     return(dff)
     
}


case_summary<-function(df, Tmin=0, Tmax=Inf){
     if (is.null(df)){
          return(NULL)
     }
     
     # initial<-function (x){
     #      SingleE<-filter(df, Electrode==1)
     #      average_inrange(x, SingleE$Time, c(Tmin-1, Tmin))
     # }
     # 
     dff<-filter(df, Time>=Tmin, Time<=Tmax) %>% group_by(AblNum,Electrode) %>% dplyr::select_("AblNum", "Electrode", "Pow", "Temp", "Imp", "Current", "Voltage") %>%
                    #summarize_each_(funs("max","high","mean", "median", "low", "min"),c("Pow", "Temp", "Imp"))
          summarize_each(funs(max(.,na.rm=T),high,mean(.,na.rm=T), median(.,na.rm=T), low, min(.,na.rm=T), sd(.,na.rm=T)),c(Pow, Temp, Imp, Current, Voltage))
     
     dp<-group_by(df, AblNum,Electrode) %>% summarize(Date=Date[1], 
                                                      MaxDuration=MaxDuration[1],
                                                      Duration_sec=max(Time),
                                                      Catheter=Catheter[1],
                                                      AblMode=AblMode[1],
                                                      StopReason=StopReason[1],
                                                      StopType=factor(ifelse(!grepl("Ablation Stop ",StopReason[1]) & !is.na(StopReason[1]), "Error",
                                                                                 ifelse(grepl("message from generator",StopReason[1]),"Stopped",
                                                                                        ifelse(grepl("reached final duration",StopReason[1]),"FullDuration", "Unspecified"))),
                                                                          levels = c("FullDuration", "Stopped", "Error", "Unspecified")),
                                                      ErrorElec=as.numeric(str_extract(StopReason[1], "[0-9]+")),
                                                      PreAblationTime=as.factor(round(abs(Time[1]))), 
                                                      Pow_target=max(PowT),
                                                      Temp_target=max(TempT), 
                                                      isActive=(Pow_target>0 & max(Time)>0), 
                                                      Pow_risetime=ifelse(isActive & max(Pow)>0.95*Pow_target, 
                                                                            head(Time[Pow>=Pow_target*0.95],1)-head(Time[Pow>=Pow_target*0.05],1),
                                                                            NA), 
                                                      Energy_Joules=trapz(Time, Pow),
                                                   
                                                      Temp_initial=ifelse(max(Pow)>0, Temp[Pow>0][1],Temp[Time>=Tmin][1]),
                                                      Temp_rise=ifelse(isActive, 
                                                                       quantile(Temp[Time>Tmin],0.95)-Temp_initial,
                                                                       NA),
                                                      Imp_initial=average_inrange(Imp,Time,c(max(Tmin-1, min(Time)),Tmin)),
                                                      Imp_drop=ifelse(isActive, 
                                                                      Imp_initial-quantile(Imp[Imp>0],0.05),
                                                                      NA) 
                                                      
                                                      
                                                      )
     
     dft<-group_by(dp, AblNum) %>% summarize(Date=min(Date), Duration=min(Duration_sec)) %>% mutate(TimebetAbl_sec=as.numeric(difftime(lead(Date), Date, units="sec"))-Duration) %>% dplyr::select(-Date, -Duration)
     
     dp<-merge(dp, dft, by="AblNum")
          
     dd<-merge(dp, dff, by=c("AblNum", "Electrode"), all=T)
     
     dd<-arrange(dd, AblNum, Electrode) %>% dplyr::select(-Imp_drop,  Imp_drop)
      
     return(dd)
}


read_all_ablations<-function(path){

     files <- list.files(path, full.names = T, pattern=".*ABL.*txt")

     dfs<-NULL
    
     for (i in seq_along(files)){
          print(files[i])
          
          Abl<-read_log_file(files[i])
          df<-Abl@Data
          if (nrow(df)==0){
               next
          }
          S <- data.frame(AblNum=Abl@AblNum,
                          Date=Abl@Date,
                          SW_version=Abl@SW_version,
                          HW_version=Abl@HW_version,
                          MaxDuration=Abl@MaxDuration,
                          #AblDuration=Abl@Data$Time[nrow(Abl@Data)],
                          #PreAblTime=Abl@Data$Time[1],
                          Catheter=Abl@Catheter,
                          AblMode=Abl@Mode,
                          StopReason=Abl@StopReason,
                          Annotation=Abl@Annotation, 
                          stringsAsFactors = F
          )

          S<-S[rep(1,nrow(df)),]
          dfs<-rbind(dfs, cbind(S, df))
     }
     row.names(dfs)<-NULL
   
     return(dfs)

}


read_carto_force<-function(path_to_zip){
     filesdir <- unzip(path_to_zip, list  = T)
     files <- filesdir$Name[grepl(pattern = "ContactForceInRF.*txt", x = filesdir$Name)]
     
     dff<-NULL
     
     for (i in seq_along(files)){
          r<-str_match(files[i], ".*-.*_([:digit:]+).*")
          AblNum<-as.numeric(r[2])
          df<- read.table(unz(path_to_zip, files[i]), header = T)
          df$AblNum<-AblNum
          dff<-rbind(dff,df)

     }
     
     return(dff)
     
}

force_summary<-function(dff){
     dffs<-group_by(dff, AblNum) %>% dplyr::select(TimeStamp, AblNum, ForceValue, AxialAngle, LateralAngle) %>%
          summarize_each(funs(max(.,na.rm=T),high,mean(.,na.rm=T), median(.,na.rm=T), low, min(.,na.rm=T), sd(., na.rm=T)),c(TimeStamp,ForceValue, AxialAngle, LateralAngle)) %>%
          mutate(Duration_sec=(TimeStamp_max-TimeStamp_min)/1000) %>%
          dplyr::select(-contains("TimeStamp")) %>%
          arrange(AblNum)
     #dffs<-as.data.frame(dffs)
     
}


read_carto_sites<-function(path_to_zip){
     filesdir <- unzip(path_to_zip, list  = T)
     f<-filesdir$Name[grepl(pattern = "VisiTagExport/Sites.txt", x = filesdir$Name)]
     
     df<-NULL
     if (length(f)>0){
          df<- read.table(unz(path_to_zip, f), header = T)
     }
     df[df==-10000]<-NA
     dfs<-dplyr::select(df,Session,ChannelID, X,Y,Z, DurationTime)%>%                #,MaxTemperature, MaxPower, BaseImpedance) %>%
          mutate(Session=Session-Session[1]+1, ChannelID=ChannelID+1) %>% 
          rename(AblNum=Session, Electrode=ChannelID, DurationC=DurationTime) %>%    #Temp_maxC=MaxTemperature, Pow_maxC=MaxPower, Imp_initialC=BaseImpedance) %>% 
          mutate_each(funs(.*DurationC), c(X, Y, Z)) %>%                             #, Temp_maxC, Pow_maxC, Imp_initialC)) %>% 
          group_by(AblNum, Electrode) %>% summarize_each(funs(sum)) %>% mutate_each(funs(./DurationC),c(X, Y, Z)) #, Temp_maxC, Pow_maxC, Imp_initialC))
     return(dfs)
          
}

merge_carto_gen<-function(dfc ,dfg){
     dfcs<-group_by(dfc,AblNum) %>% summarize(DurationC=max(DurationC)) %>% arrange(AblNum)
     dfgs<-group_by(dfg,AblNum) %>% summarize(Duration_sec=max(Duration_sec)) %>% top_n(nrow(dfcs)) %>% arrange(AblNum)
     
     dfcg<-data.frame(AblNumC=dfcs$AblNum, AblNumG=dfgs$AblNum)
     
     dfc<-mutate(dfc,AblNumM=AblNum)
     
     for (n in 1:nrow(dfcg)){
          dfc$AblNum[dfc$AblNumM==dfcg$AblNumC[n]]<-dfcg$AblNumG[n]
     }
     dfc<-dplyr::select(dfc, -AblNumM, -DurationC)
     dfm<-merge(dfg, dfc, by=c("AblNum","Electrode"), all=T)
     return(dfm)
     
}

get_user<-function (){
     Sys.getenv("USERNAME")
}

sender <- "ep.robot.bwi@gmail.com"
recipients <- c("alifshi1@its.jnj.com")
send.mail(from = sender,
          to = recipients,
          subject = "Hello World",
          body = "Hi",
          smtp = list(host.name = "smtp.gmail.com", port = 465,
                      user.name = "ep.robot.bwi@gmail.com",
                      passwd = "eprobotbwi", ssl = TRUE),
          authenticate = TRUE,
          send = TRUE)

     