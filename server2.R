library(shiny)
library(plotly)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)

source('functions.R')

shinyServer(function(input, output, session) {
     
     observeEvent(
          ignoreNULL = TRUE,
          eventExpr = {
               input$directory
          },
          handlerExpr = {
               if (input$directory > 0) {
                    # condition prevents handler execution on initial app launch
                    
                    # launch the directory selection dialog with initial path read from the widget
                    path = choose.dir(default = readDirectoryInput(session, 'directory'))
                    
                    # update the widget value
                    updateDirectoryInput(session, 'directory', value = path)
               }
          }
     )
     
     
     output$directory = renderText({
          readDirectoryInput(session, 'directory')
     })
     
     
     observe({
          ablation_file = input$file
          if (is.null(ablation_file)) {
               return(NULL)
          }
          
          Ablation<-read_log_file(ablation_file$datapath)
       
          output$ParamPlot <- renderPlotly({
               g<-ggplot(Ablation@Data, aes_string(x="Time", y=names(Ablation@Data)[as.numeric(input$param)], color="Electrode"))+
                    geom_line()+labs(title = sprintf("Ablation #%d %s %s", Ablation@AblNum, Ablation@Catheter, Ablation@Mode)) + 
                    xlab("Time [sec]")+ylab(names(Ablation@Data)[as.numeric(input$param)])+xlim(-5, Ablation@MaxDuration)
               ggplotly(g)
               
               #plot_ly(dfz, x = Time, y = Temp,color = as.ordered(Electrode))
          })
          
          
          df<-filter(Ablation@Data,Electrode==input$elec)
          a <- list(
               x = -5,
               y = max(c(df$Temp,df$Pow))*1.03,
               xanchor = "left",
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
                    layout(title = sprintf("Ablation #%d %s %s", Ablation@AblNum, Ablation@Catheter, Ablation@Mode), 
                           yaxis2 = list(title="Impedance [Ohm]",overlaying = "y",side = "right",
                                         range = c(0, max(df$Imp)*1.05), autotick = F, tick0=0, dtick=10, ticks = "outside",showgrid=F, showline=T,zeroline=F),
                           xaxis = list(title = "Time [sec]", range = c(-5, Ablation@MaxDuration), autorange = F,autotick = F, tick0 = -5, dtick = 5, ticks = "outside",zeroline=F, showline=T, gridcolor = toRGB("gray80")), 
                           yaxis=list(title="Power [W]/Temperature [deg C]", range = c(0, max(c(df$Temp,df$Pow))*1.05), autotick = F, tick0 = 0, dtick = 5, ticks = "outside",showline=T,zeroline=F,gridcolor = toRGB("gray80")),
                           annotations=a, margin=m, legend=list(x=1.05, orientation="v")
                           ) 
          })
          
          # dfs<-dplyr::select(dfz,c(1,2,3,5,6))
          # output$view <- renderGvis({
          #      Motion=gvisMotionChart(dfs, 
          #                             idvar="Electrode", 
          #                             timevar="Time")
          #      plot(Motion)
          # })
     })
     
})