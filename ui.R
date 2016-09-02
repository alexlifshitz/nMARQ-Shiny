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
               selectInput("file", label = "Ablations",choices=NULL),
               tags$hr(),
               textInput("case", label="Case Name", value = NULL, width = "100%", placeholder = "Case Name"),
               textInput("author", label="Author", value = "", width = "100%", placeholder = "John Smith"),
               #tags$textarea(id="description", rows=3, cols=40, "Default value")
               textInput("description", label="Case Description", value = "", width = "100%", placeholder = "Very important study"),
               tags$hr()
               
          ),
          mainPanel(
               tabsetPanel(
                    tabPanel("Ablation View", 
                             
                             # fluidRow(
                             #      column(3,selectInput("elec", label = "Select Electrode", 1, selected = 1)),
                             #      column(4,actionButton("next", "Next"))),
                             # tags$style(type='text/css', "#next { margin-top: 25px;}"),
                             
                             selectInput("elec", label = "Select Electrode", 1, selected = 1),
                             plotlyOutput("ElectrodePlot",height = "500px"),
                             textInput("AnnotationE", label=NULL, value = "", width = "80%", placeholder = "Annotate graph..."),
                             tags$hr(),
                             selectInput("param", label = "Select Parameter", 
                                         choices = list("Power" = 3, "Temperature" = 6, "Impedance" = 5), 
                                         selected = 1),
                             plotlyOutput("ParamPlot",height = "500px")
                             ),
                    # tabPanel("Parameter View", 
                    #          # tags$hr(),
                    #          # selectInput("param", label = "Select Parameter", 
                    #          #             choices = list("Power" = 3, "Temperature" = 6, "Impedance" = 5), 
                    #          #             selected = 1),
                    #          # plotlyOutput("ParamPlot",height = "500px"),
                    #          # textInput("AnnotationP", label=NULL, value = "", width = "80%", placeholder = "Annotate graph...")
                    # ),
                    tabPanel("Case View", 
                             br(),
                             uiOutput("load_case"),
                             selectInput("param_case", label = "Select Parameter", 
                                         choices = list("Power" = 9, "Temperature" = 12, "Impedance" = 11), 
                                         selected = NULL),
                             plotlyOutput("CasePlot",height = "500px")
                             )
                    
               )
          )
     ))
)