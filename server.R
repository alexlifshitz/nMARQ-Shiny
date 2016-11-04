library(shiny)
library(plotly)
library(stringr)
library(stringi)
library(dplyr)
library(tidyr)
library(gtools)
library(ggplot2)
library(DT)
library(RColorBrewer)

source('functions.R')

Ablation<-NULL
Ablations<-NULL
files<-NULL
SumAblations<-NULL
Annotations<-NULL
Total_files<-0


shinyServer(function(input, output, session) {
     #print(session$user)
     
     dfcc<-readRDS("data/rds/dfcc_qdot.rds")
     mesh<-readRDS("data/rds/mesh_qdot.rds")
     triangles<-readRDS("data/rds/triangles_qdot.rds")
     
     
     user <- data.frame(START = as.POSIXlt(Sys.time(),tz="America/Los_Angeles"))
     session$onSessionEnded(function() {
          user$END <- as.POSIXlt(Sys.time(),tz="America/Los_Angeles")

          text<-paste(paste("StartTime", user$START, sep="\t"), 
                      paste("UsageTime_min", round(as.numeric(difftime(user$END, user$START, units="mins")),2), sep="\t"),  
                      paste("AblNum", Total_files, sep="\t"), sep="\n")
          
          if (get_user()=="shiny"){
               send_email(subject= paste("EP robot helped a human to analyze", Total_files,  "ablations") , body=text)     
          }else{
               print("robot idle")
          }
          
          
          Ablation<<-NULL
          Ablations<<-NULL
          files<<-NULL
          SumAblations<<-NULL
          Annotations<<-NULL
          Total_files<<-0
       
     })
     
     
     # output$hello <- renderText({ 
     #      paste("Hello ", get_user())
     # })
     # 
     
     observeEvent(
          ignoreNULL = T,
          eventExpr = {
               input$Dir
          },
          handlerExpr = {
               if (!is.null(input$Dir)) {
                    # condition prevents handler execution on initial app launch
                    
                    # launch the directory selection dialog with initial path read from the widget
                    ##path = choose.dir(default = readDirectoryInput(session, 'directory'))
                    
                    # update the widget value
                    ##updateDirectoryInput(session, 'directory', value = path)
                    
                    #files <<- list.files(readDirectoryInput(session, 'directory'), full.names = T, pattern=".*ABL.*txt")
                    ##files <<- list.files(path, full.names = T, pattern=".*ABL.*txt")
                    
                    files<<- filter(input$Dir, grepl(".*ABL.*txt", name))
                    files <<- files[mixedorder(files$name),]
                    
                    updateSelectInput(session, "file", choices=files$name)
                    output$load_case <- renderUI({
                         actionButton("action", label = "Process Case Data")
                    })
                    #Ablations<<-read_all_ablations(files)
                    Ablations<<-NULL
                    SumAblations<<-NULL
                    Ablation<<-NULL
                    Annotations<<-NULL
                    
                    Total_files<<-Total_files+nrow(files)
                    
               }
          }
     )
     
     
     observe({
          output$md <- renderUI({
               if (input$password2=="sharktank"){
                    includeMarkdown("data/nMARQ.md")     
               }
               
          })
     })

     
     observeEvent(
          ignoreNULL = T,
          eventExpr = {
               #input$password1
               input$opacity_qdot
               input$color_qdot
          },
          handlerExpr = {
               
               pal<-brewer.pal(9, "Set1")
               
               op<-input$opacity_qdot
               
               
               dfcp<-dplyr::select(dfcc, AblNum, X,Y,Z, Pow_target, Energy_Joules, ForceValue_median, Imp_initial, Imp_drop, Temp_rise) %>%
                    mutate_(col=input$color_qdot)
               
              # names(dfcp)[names(dfcp)==input$color_qdot]<-"col"
               
               if (input$color_qdot=="Pow_target"){
                    cols<-c(pal[2], pal[1])
                    dfcp$col<-as.factor(dfcp$col)
               }else{
                    colfunc <- colorRampPalette(c("blue", "red"))
                    cols<-colfunc(10)
               }
                    
               
               
               
               output$qdot1 <- renderPlotly({
                    # if (input$password1=="sharktank"){
                    #      ....
                    # }else{
                    #      return(NULL)
                    # }
                    plot_ly(data=dfcp, x=~X, y=~Y, z=~Z) %>% 
                         add_trace(data=mesh, x=~X, y=~Y, z=~Z, type="mesh3d", i=~triangles$Vertex0, j=~triangles$Vertex1, k=~triangles$Vertex2,
                                   opacity=~op, showscale=F, hoverinfo="none")%>% 
                         add_markers(data=dfcp, type = 'scatter3d', size = ~Energy_Joules, 
                                     color = ~col, colors = ~cols,
                                     marker = list(opacity = 0.7, sizemode = 'area', sizeref=0.05),hoverinfo="text",
                                     text=~paste("Abl Num: ", AblNum, "<br>","Energy: ", round(Energy_Joules,1), "<br>",  "Force: ",round(ForceValue_median,1),"<br>","Imp: ", round(Imp_initial,1),
                                                 "<br>","Imp Drop: ", round(Imp_drop,1), "<br>","Temp Rise: ", round(Temp_rise,1)))
                    
               })
               output$qdot2 <- renderPlotly({     
               plot_ly(data=dfcp, y = ~col, type = "box", boxpoints = "all", jitter = 0.3,marker=list(showscale=F),
                           pointpos = 0) %>%
                         layout(title = ~input$color_qdot, 
                                xaxis = list(title = ""), 
                                yaxis=  list(title="")
                                
                         )
               })
     })
     
     
     
     observeEvent(
          ignoreNULL = T,
          eventExpr = {
               input$demo
          },
          handlerExpr = {
               if (!is.null(input$demo)) {
                    path <-"data/test"
                    f <- list.files(path, full.names = T, pattern=".*ABL.*txt")
                    files<<-data.frame(name=basename(f), datapath=f, stringsAsFactors = F)
                    
                    updateSelectInput(session, "file", choices=files$name)
                    output$load_case <- renderUI({
                         actionButton("action", label = "Process Case Data")
                    })
                    #Ablations<<-read_all_ablations(files)
                    Ablations<<-NULL
                    SumAblations<<-NULL
                    Ablation<<-NULL
                    Annotations<<-NULL
               }
          }
     )
     
     observeEvent(
          ignoreNULL = T,
          eventExpr = {
               input$annotate
          },
          handlerExpr = {
               
               r <- str_match(input$file, "ABL(\\d*?).txt")
               Num<-as.numeric(r[2])
               Elec<-as.numeric(input$elec)
             
               if (is.null(Annotations)){
                    Annotations<<- data.frame(AblNum=Num, Electrode=Elec, Annotation=input$annotate, stringsAsFactors = F)
               }else if (nrow(filter(Annotations, AblNum==Num, Electrode==Elec))==0){
                    Annotations<<-rbind(Annotations, data.frame(AblNum=Num, Electrode=Elec, Annotation=input$annotate,stringsAsFactors = F))     
                    
               }else{
                    Annotations$Annotation[Annotations$AblNum==Num & Annotations$Electrode==Elec]<<-input$annotate
               }
          }
     )
     
     
     get_ablation_data<-reactive({#eventReactive(input$file, valueExpr = {
               if (input$file > 0) {
                   
                    #ablation_file = files[stri_endswith_fixed(files, input$file)]
                    
                    if (!is.null(Ablations)){
                         r <- str_match(input$file, "ABL(\\d*?).txt")
                         
                         Abl<-df2Ablation(filter(Ablations,AblNum==as.numeric(r[2])))
                         #updateTextInput(session, "annotate", value = Abl@Annotation)
                         
                         
                         
                    }else{
                         ablation_file = files$datapath[input$file==files$name]
                         Abl<-read_log_file(ablation_file)     
                         #updateTextInput(session, "annotate", value = NULL)
                         updateTextInput(session, "date", value = as.Date(min(Abl@Date),format="%Y-%m-%d"))
                    }
                    
                    updateSelectInput(session, "elec", choices=1:Abl@ElecNum)
                    Ablation<<-Abl
                    return(Abl)
               }else{
                    Ablation<<-NULL
                    return(NULL)
               }

          }
     )
     
     
     observeEvent(
          ignoreNULL = T,
          eventExpr = {
               input$action
          },
          handlerExpr = {
     
          if (is.null(Ablations)){
               
               #Ablations<<-read_all_ablations(files)
               
               dfs<-NULL
               withProgress(message = 'Processing ablations files...', value = 0, {
                    
                    for (i in seq_along(files$datapath)){
                         #print(files$name[i])
                         incProgress(1/length(files$datapath), detail = files$name[i])
                         Abl<-read_log_file(files$datapath[i])
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
               })
               row.names(dfs)<-NULL
               Ablations<<-dfs
               
               updateTextInput(session, "date", value = as.Date(min(Ablations$Date),format="%Y-%m-%d"))
               
               SumAblations<<-case_summary(Ablations)      
               
               params<-names(SumAblations)[grepl(pattern = "_",names(SumAblations))]
               params<-as.data.frame(str_split(params,"_",simplify = T))
               names(params)<-c("param", "func")
               
               ch=as.character(params$func[params$param==input$param_case_y])
               updateSelectInput(session, "sumy", choices=ch)     
               
               output$load_case <- renderUI({
                    actionButton("action", label = "Done")
               
               })
               
          }
     })
     
     
     observeEvent(
          ignoreNULL = T,
          eventExpr = {
               input$sumy
               input$param_case_y
               input$active
          },
          handlerExpr = {
               
               
               if (is.null(Ablations)) {
                    SumAblations<<-NULL
                    return(NULL)
               }
               
               if (input$active==T){
                    dfp<-filter(SumAblations, isActive==TRUE)
               }else{
                    dfp<-SumAblations
               }
               
               yaxis<-paste(input$param_case_y,input$sumy,sep="_")
               if (yaxis %in% names(dfp)){
                    dfp<-dplyr::select_(dfp,"AblNum", "Electrode",  yaxis) 
                    names(dfp)[names(dfp)==yaxis]<-"Yaxis"
                 
                    output$CasePlot <- renderPlotly({
                         plot_ly(dfp, x=~AblNum, y=~Yaxis, color = ~Electrode, colors = "Paired",marker=list(alpha=0.8, size=12)) %>%
                              add_markers(hoverinfo="text", text=~paste("Ablation: ", AblNum, "<br>", "Elec: ", Electrode, "<br>", sprintf("%s: ", yaxis), Yaxis)) %>%
                              
                              layout(title = sprintf("Case Summary - %s", yaxis), 
                                     xaxis = list(title = "Ablation Number", ticks = "outside",zeroline=F, showline=T, gridcolor = toRGB("gray80")), 
                                     #yaxis=  list(title=yaxis, range = c(ifelse(min(Yaxis)>0,0,min(Yaxis)*1.05), max(Yaxis)*1.05), autotick = T, tick0 = 0, ticks = "outside",showline=T,zeroline=T,gridcolor = toRGB("gray80"))
                                     yaxis=  list(title=yaxis, autotick = T, tick0 = 0, ticks = "outside",showline=T,zeroline=F,gridcolor = toRGB("gray80"))
                                                  
                                     
                              )
                    })
                    
               }
               
    })
     
     observeEvent(
          ignoreNULL = T,
          eventExpr = {
               input$action
               input$param_t
               input$active
          },
          handlerExpr = {
               
               
               if (is.null(Ablations)) {
                    SumAblations<<-NULL
                    return(NULL)
               }
               
               if (input$active==T){
                    abl_nums<-unique(SumAblations$AblNum[SumAblations$isActive])
               }else{
                    abl_nums<-unique(SumAblations$AblNum)
               }
               
               
               raw_abl<-filter(Ablations, AblNum %in% abl_nums)
               raw_abl<-merge(raw_abl, dplyr::select(SumAblations, c(AblNum, Electrode, Pow_target)), by=c("AblNum", "Electrode"))
                    
               yaxis<-input$param_t
           
               if (yaxis %in% names(raw_abl)){
                    dfp<-dplyr::select_(raw_abl,"Time", "AblNum", "Electrode", "Pow_target", "TempT", yaxis) %>% mutate(AblElec=paste(AblNum, Electrode, sep="_"))
                    names(dfp)[names(dfp)==yaxis]<-"Yaxis"
                    
                    output$TimePlot <- renderPlotly({
                         
                         dfp %>%
                              group_by(AblElec) %>%
                              plot_ly(x=~Time, y=~Yaxis) %>%
                              add_lines(split = ~Pow_target,alpha = 0.35)%>% 
                              layout(title = sprintf("Case Summary - %s", yaxis), 
                                     xaxis = list(title = "Time[sec]", ticks = "outside",zeroline=F, showline=T, gridcolor = toRGB("gray80")), 
                                     yaxis=  list(title=yaxis, autotick = T, tick0 = 0, ticks = "outside",showline=T,zeroline=F,gridcolor = toRGB("gray80"))
                                     
                                     
                              )

                    })
                    
               }
               
          })
     
     observeEvent(
          ignoreNULL = T,
          eventExpr = {
               input$param_case_y
          },
          handlerExpr = {
               if(!is.null(SumAblations)){
                    
                    
                    params<-names(SumAblations)[grepl(pattern = "_",names(SumAblations))]
                    params<-as.data.frame(str_split(params,"_",simplify = T))
                    names(params)<-c("param", "func")
                    
                    ch=as.character(params$func[params$param==input$param_case_y])
                    updateSelectInput(session, "sumy", choices=ch, selected = ch[1])
               }
               
          })
     
     
     get_fname<-reactive({
          
          if (!is.null(Ablations)){
               if (input$case==""){
                    fname<-paste0("Case_", as.Date(min(Ablations$Date),format="%Y_%m_%d"))
               }else{
                    
                    fname<-paste(input$case, as.Date(min(Ablations$Date),format="%Y_%m_%d"), sep="_")
               }
               return(fname)
          }else{
               return(NULL)
          }     
          
     })
     
     
     observe({
          output$All_data <- DT::renderDataTable({
               
               DT::datatable(Ablations, filter = 'top',rownames = FALSE,options = list(orderClasses = TRUE,pageLength = 5, dom = 'tip',autoWidth = F)) 
          })
          
          output$Sum_data <- DT::renderDataTable({
               if (!is.null(Annotations)){
                    SumAblations<<-merge(SumAblations, Annotations, by=c("AblNum", "Electrode"), all=T)
               }
               DT::datatable(SumAblations, filter = 'top',rownames = FALSE,options = list(orderClasses = TRUE,pageLength = 5,dom = 'tip',autoWidth = F)) #pageLength = 10
          })
          
          fname<-get_fname()

          output$downloadReport <- downloadHandler(

               filename = function() { paste(ifelse(input$case=="", "Case", input$case),
                                             "_R_",
                                             as.Date(min(Ablations$Date),format="%Y_%m_%d"),'.rds', sep='') },
               content = function(file) {
                    saveRDS(Ablations,file)
               }
          )
          
          output$downloadRawData <- downloadHandler(
               
               filename = function() { paste(ifelse(input$case=="", "Case", input$case),
                                             "_All_Data_",
                                             as.Date(min(Ablations$Date),format="%Y_%m_%d"),'.csv', sep='') },
               content = function(file) {
                    write.csv(Ablations, file)
               }
          )
          output$downloadSumData <- downloadHandler(
               filename = function() { paste(ifelse(input$case=="", "Case", input$case),
                                             "_Summary_",
                                             as.Date(min(Ablations$Date),format="%Y_%m_%d"),'.csv', sep='') },
               content = function(file) {
                    if (!is.null(Annotations)){
                         if ("Annotation" %in% colnames(SumAblations)){
                              SumAblations<<-dplyr::select(SumAblations, -Annotation)
                         } 
                         SumAblations<<-merge(SumAblations, Annotations, by=c("AblNum", "Electrode"), all=T)
                         
                    }
                    write.csv(SumAblations, file)     

               }
          )
     })
     
     
     observe({
             
          Ablation <<- get_ablation_data()
          if (is.null(Ablation)) {
                return(NULL)
          }
          
          yaxis<-names(Ablation@Data)[as.numeric(input$param)]
          dfa<-dplyr::select_(Ablation@Data, "Time" ,yaxis, "Electrode")
          names(dfa)[names(dfa)==yaxis]<-"Yaxis"
          
          
          output$ParamPlot <- renderPlotly({
               plot_ly(dfa, x=~Time, y=~Yaxis, color=~Electrode) %>% add_lines(colors="Paired") %>%
                    layout(title = sprintf("Ablation #%d %s %s", Ablation@AblNum, Ablation@Catheter, Ablation@Mode), 
                           xaxis = list(title = "Time[sec]", ticks = "outside",zeroline=F, showline=T, gridcolor = toRGB("gray80")), 
                           yaxis=  list(title=yaxis, autotick = T, tick0 = 0, ticks = "outside",showline=T,zeroline=F,gridcolor = toRGB("gray80"))
                           
                           
                    )
          # g<-ggplot(Ablation@Data, aes_string(x="Time", y=names(Ablation@Data)[as.numeric(input$param)], color="Electrode"))+
          #      geom_line()+labs(title = sprintf("Ablation #%d %s %s", Ablation@AblNum, Ablation@Catheter, Ablation@Mode)) + 
          #      xlab("Time [sec]")+ylab(names(Ablation@Data)[as.numeric(input$param)])+xlim(-5, Ablation@MaxDuration)
          # ggplotly(g)
          })
          
          output$table2 <- renderTable(param_summary(Ablation,names(Ablation@Data)[as.numeric(input$param)]),include.rownames=FALSE)
          
     })
     
     
     observe({
          Ablation <<- get_ablation_data()
          if (is.null(Ablation)) {
               return(NULL)
          }
          
          df<-filter(Ablation@Data,Electrode==input$elec)
          a<-list()
          a <- list(
               x = Ablation@MaxDuration,
               y = max(c(df$Temp,df$Pow, df$TempT, df$PowT))*1.03,
               xanchor = "right",
               text = Ablation@StopReason,
               xref = "x",
               yref = "y",
               showarrow = F,
               ax = 0,
               ay = 0
          )
          b <- list(
               x = min(Ablation@Data$Time)+0.5,
               y = max(c(df$Temp,df$Pow, df$TempT, df$PowT))*1.03,
               xanchor = "left",
               text = as.character(Ablation@Date),
               xref = "x",
               yref = "y",
               showarrow = F,
               ax = 0,
               ay = 0
          )
          
          m = list(
               l = 50,
               r = 150,
               b = 100,
               t = 70,
               pad = 4
          )
          output$ElectrodePlot <- renderPlotly({
               
               plot_ly(df, x = ~Time, y = ~Pow, name = "Power", type="scatter", mode = "lines", 
                       line = list(color = toRGB("red")), hoverinfo="text", text=~paste("Time:", Time, "<br>", "Power: ", Pow)) %>%
                    add_lines(x = ~Time, y = ~Temp, name = "Temperature",
                              line = list(color = toRGB("blue")),hoverinfo="text", text=~paste("Time:", Time, "<br>", "Temp: ", Temp)) %>%
                    add_lines(x = ~Time,y = ~Imp, name = "Impedance",yaxis = "y2",
                              line = list(color = toRGB("green")),hoverinfo="text", text=~paste("Time:", Time, "<br>", "Imp: ", Imp)) %>%
                         add_trace(y = ~TempT, name = "TempT", showlegend=F,mode = "lines", line = list(color = toRGB("blue"), dash="5px"), hoverinfo = "none")%>%
                    add_trace(y = ~PowT, name = "PowT", showlegend=F,mode = "lines", line = list(color = toRGB("red"), dash="5px"),hoverinfo = "none") %>%
                    layout(title = sprintf("Ablation #%d: Electrode %s  (Catheter: %s %s)", Ablation@AblNum, input$elec, Ablation@Catheter, Ablation@Mode),
                           yaxis2 = list(title="Impedance [Ohm]",overlaying = "y",side = "right",
                                         #range = c(min(df$Imp)*0.95, max(df$Imp)*1.05), autotick = T, tick0=0, dtick=max(1,ceiling((max(df$Imp)-min(df$Imp))/10)),
                                         ticks = "outside",showgrid=F, showline=T,zeroline=F),
                           xaxis = list(title = "Time [sec]",
                                        #range = c(Ablation@Data$Time[1], Ablation@MaxDuration),
                                        autorange = T,autotick = T,
                                        tick0 = -5, #Ablation@Data$Time[1],
                                        ticks = "outside",zeroline=F, showline=T, gridcolor = toRGB("gray80")),
                           yaxis=list(title="Power [W]/Temperature [deg C]",
                                      #range = c(0, max(c(df$Temp,df$Pow, df$TempT, df$PowT))*1.05),
                                      autotick = T, tick0 = 0, dtick = 5, ticks = "outside",showline=T,zeroline=F,gridcolor = toRGB("gray80")),
                           margin=m, legend=list(x=1.05, orientation="v")
                    ) %>% add_annotations(x=~c(a$x, b$x), y=~c(a$y, b$y),
                                          text=~c(a$text,b$text), xanchor = c("right", "left"), xref = c("x", "x"),yref=c("y", "y"), 
                                          showarrow = c(F,F),ax = c(0,0),ay = c(0,0))
          })
          
          output$table1 <- renderTable(elec_summary(Ablation,input$elec),include.rownames=FALSE)
          
          r <- str_match(input$file, "ABL(\\d*?).txt")
          Num<-as.numeric(r[2])
          Elec<-as.numeric(input$elec)
          
          if (!is.null(Annotations)){
               if (nrow(filter(Annotations, AblNum==Num, Electrode==Elec))==1){
                    updateTextInput(session, "annotate", value = Annotations$Annotation[Annotations$AblNum==Num & Annotations$Electrode==Elec])     
               }else{
                    updateTextInput(session, "annotate", value = "") 
               }
               
          }

     })
     
})