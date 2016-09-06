library(shiny)
library(plotly)
library(stringr)
library(stringi)
library(dplyr)
library(tidyr)
library(gtools)
library(ggplot2)
library(DT)

source('functions.R')

Ablations<-NULL
files<-NULL

shinyServer(function(input, output, session) {
     
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
               }
          }
     )
     
     get_ablation_data<-eventReactive(input$file, valueExpr = {
               if (input$file > 0) {
                    
                    #ablation_file = files[stri_endswith_fixed(files, input$file)]
                    ablation_file = files$datapath[input$file==files$name]
                    Ablation<-read_log_file(ablation_file)
                    updateSelectInput(session, "elec", choices=1:Ablation@ElecNum)
                    return(Ablation)
               }

          }
     )
     
     
     load_case <- eventReactive(input$action, {
          if (is.null(Ablations)){
               
               #Ablations<<-read_all_ablations(files)
               
               dfs<-NULL
               withProgress(message = 'Processing ablations files...', value = 0, {
                    Sys.sleep(0.25)
                    for (i in seq_along(files$datapath)){
                         #print(files$name[i])
                         incProgress(1/length(files$datapath), detail = files$name[i])
                         Abl<-read_log_file(files$datapath[i])
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
               })
               row.names(dfs)<-NULL
               Ablations<<-dfs
               
               output$load_case <- renderUI({
                    actionButton("action", label = "Done")
               })
               
          }
          return(Ablations)
     })
     
    
     
     observe({
          load_case()
          dfs<-dplyr::select(Ablations, AblNum, Electrode, Pow, Temp, Imp) %>% group_by(AblNum, Electrode) %>%
               summarize_each_(funs_("mean"),c("Pow"))
     
          output$CasePlot <- renderPlotly({
               # g<-ggplot(dfs, aes(x=AblNum, y=Pow, color=Electrode))+geom_point(size=3, alpha=0.5)
               # ggplotly(g)  
               plot_ly(dfs, x=AblNum, y=Pow, mode = "markers", color = Electrode, marker = list(opacity = 0.8, size = 12))
          })
          
          
     })
     
     
     observe({
          
               output$All_data <- DT::renderDataTable({
               DT::datatable(Ablations, filter = 'top',rownames = FALSE,options = list(orderClasses = TRUE,pageLength = 5, dom = 'tip',autoWidth = F))
          })
          
          output$downloadData <- downloadHandler(
               filename = function() { paste('All_data', '.csv', sep='') },
               content = function(file) {
                    write.csv(Ablations, file)
               }
          )
     })
     
     
     observe({
             
          Ablation <- get_ablation_data()
          if (is.null(Ablation)) {
                return(NULL)
          }
          
          
          output$ParamPlot <- renderPlotly({
          g<-ggplot(Ablation@Data, aes_string(x="Time", y=names(Ablation@Data)[as.numeric(input$param)], color="Electrode"))+
               geom_line()+labs(title = sprintf("Ablation #%d %s %s", Ablation@AblNum, Ablation@Catheter, Ablation@Mode)) + 
               xlab("Time [sec]")+ylab(names(Ablation@Data)[as.numeric(input$param)])+xlim(-5, Ablation@MaxDuration)
          ggplotly(g)
          })
          
          output$table2 <- renderTable(param_summary(Ablation,names(Ablation@Data)[as.numeric(input$param)]),include.rownames=FALSE)
          
          
          df<-filter(Ablation@Data,Electrode==input$elec)
          
          a <- list(
               x = Ablation@MaxDuration,
               y = max(c(df$Temp,df$Pow))*1.03,
               xanchor = "right",
               text = Ablation@StopReason,
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
               
               plot_ly(df, x = Time, y = Pow, name = "Power", mode = "lines", 
                       line = list(color = toRGB("red")), hoverinfo="text", text=paste("Time:", Time, "<br>", "Power: ", Pow)) %>%
                    add_trace(x = Time, y = Temp, name = "Temperature",mode = "lines", 
                              line = list(color = toRGB("blue")),hoverinfo="text", text=paste("Time:", Time, "<br>", "Temp: ", Temp)) %>%
                    add_trace(x = Time, y = Imp, name = "Impedance",yaxis = "y2",mode = "lines", 
                              line = list(color = toRGB("green")),hoverinfo="text", text=paste("Time:", Time, "<br>", "Imp: ", Imp)) %>%
                    add_trace(x = Time, y = TempT, name = "TempT", showlegend=F,mode = "lines", line = list(color = toRGB("blue"), dash="5px"), hoverinfo = "none")%>% 
                    add_trace(x = Time, y = PowT, name = "PowT", showlegend=F,mode = "lines", line = list(color = toRGB("red"), dash="5px"),hoverinfo = "none") %>%
                    layout(title = sprintf("Ablation #%d: Electrode %s  (Catheter: %s %s)", Ablation@AblNum, input$elec, Ablation@Catheter, Ablation@Mode), 
                           yaxis2 = list(title="Impedance [Ohm]",overlaying = "y",side = "right",
                                         range = c(min(df$Imp)*0.95, max(df$Imp)*1.05), autotick = F, tick0=0, dtick=max(1,ceiling((max(df$Imp)-min(df$Imp))/10)), ticks = "outside",showgrid=F, showline=T,zeroline=F),
                           xaxis = list(title = "Time [sec]", range = c(-5, Ablation@MaxDuration), autorange = F,autotick = F, tick0 = -5, dtick = 5, ticks = "outside",zeroline=F, showline=T, gridcolor = toRGB("gray80")), 
                           yaxis=list(title="Power [W]/Temperature [deg C]", range = c(0, max(c(df$Temp,df$Pow))*1.05), autotick = F, tick0 = 0, dtick = 5, ticks = "outside",showline=T,zeroline=F,gridcolor = toRGB("gray80")),
                           annotations=a, margin=m, legend=list(x=1.05, orientation="v")
                           ) 
          })
          
          output$table1 <- renderTable(elec_summary(Ablation,input$elec),include.rownames=FALSE)
         
     })
     
})