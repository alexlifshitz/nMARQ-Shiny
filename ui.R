library(shiny)
library(plotly)

shinyUI(fluidPage(
     
     # Application title
     titlePanel("nMARQ Log Analysis Tool"),
     
     # Sidebar with controls to select the random distribution type
     # and number of observations to generate. Note the use of the
     # br() element to introduce extra vertical spacing
     sidebarLayout(
          sidebarPanel(
               # fileInput('file', 'Select Ablation Log file',
               #           accept=c('text/plain','.txt')),
               directoryInput('directory', label = 'Select a folder', 
                              value = 'C:\\Data Science\\nMARQ Shiny\\test'),
               tags$hr(),
               selectInput("file", label = "Ablations",choices=NULL)
               
          ),
          mainPanel(
               tabsetPanel(
                    tabPanel("Electrode View", 
                             tags$hr(),
                             selectInput("elec", 
                                         label = "Electrode", 1:10, selected = 1),
                             plotlyOutput("ElectrodePlot",height = "500px"),
                             textInput("AnnotationE", label=NULL, value = "", width = "80%", placeholder = "Annotate graph...")
                             ),
                    tabPanel("Parameter View", 
                             tags$hr(),
                             selectInput("param", label = "Select Parameter", 
                                         choices = list("Power" = 3, "Temperature" = 6, "Impedance" = 5), 
                                         selected = 1),
                             plotlyOutput("ParamPlot",height = "500px"),
                             textInput("AnnotationP", label=NULL, value = "", width = "80%", placeholder = "Annotate graph...")
                    ),
                    tabPanel("Ablation Summary", htmlOutput("view")
                             )
                    
               )
          )
     ))
)