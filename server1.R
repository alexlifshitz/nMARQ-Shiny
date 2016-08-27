library(shiny)
library(plotly)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)

shinyServer(function(input, output) {
     
     observe({
          ablation_file = input$file
          if (is.null(ablation_file)) {
               return(NULL)
          }
          
          n_header=6
          con <- file(ablation_file$datapath, "r", blocking = FALSE)
          header<-readLines(con,n=n_header) 
          close(con)
          
          r <- str_match(header[1], "number (\\d*?) started")
          AblNum<-as.numeric(r[2])
          
          r <- str_match(header[1], "at (.*?) ")
          Date<-as.Date(r[2],"%d/%m/%Y")
          
          r <- str_match(header[1], "Version Ablator (.*?) ")
          SW_version<-r[2]
          
          a<-read.table(ablation_file$datapath,header = FALSE,skip = n_header,fill=TRUE,stringsAsFactors = FALSE)
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
          
          last_row<-a[nrow(a),-1]
          
          if (last_row[1]=="Ablation"){
               a<-a[-nrow(a),]
               StopReason<-trimws(paste(last_row[!is.na(last_row)], collapse=" "))
          }else{
               StopReason<-NA
          }
          
          if (nrow(a)>0){
               ind<-which(a[1,]=="/" | a[1,]=="|")
               b<-dplyr::select(a,-ind) %>%dplyr::select(-1)
               c<-as.data.frame(sapply(b, gsub, pattern="/", replacement=""), stringsAsFactors = FALSE)
               d <- as.data.frame(sapply( c, as.numeric ))
          }
          
          names(d)<-titles
          
          dfz<- gather(d, "variable", "value", -1) %>% 
               separate(variable, c("variable", "Electrode"), sep = "_") %>%
               spread(variable, value)
          
          dfz$Electrode<-as.numeric(dfz$Electrode)
          
          dfz<-dplyr::select(dfz,match(c("Electrode", "Time", titles_single), names(dfz))) %>%
               arrange(Electrode, Time)
          dfz$Electrode<-as.factor(dfz$Electrode)
       
          output$ParamPlot <- renderPlotly({
               g<-ggplot(dfz, aes_string(x="Time", y=names(dfz)[as.numeric(input$param)], color="Electrode"))+geom_line()
               ggplotly(g)
               #plot_ly(dfz, x = Time, y = Temp,color = as.ordered(Electrode))
          })
     })
     
})