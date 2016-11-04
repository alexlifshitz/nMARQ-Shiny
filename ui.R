library(shiny)
library(plotly)
require(markdown)

shinyUI(fluidPage(
     includeScript("./www/text.js"),
     # Application title
     titlePanel("EP Data Analysis Platform (Beta)"),
     helpText("The platform is at an early development stage, bugs are possible. Use with caution."),
     helpText("Please report bugs or suggestions to ", a("Alexander Lifshitz", href="mailto:alifshi1@its.jnj.com", target="_blank")),
     
     
     # Sidebar with controls to select the random distribution type
     # and number of observations to generate. Note the use of the
     # br() element to introduce extra vertical spacing
     sidebarLayout(
          sidebarPanel(width=2,
               #fileInput('file', 'Select Ablation Log file',accept=c('.zip','.txt')),
               # directoryInput('directory', label = 'Select a folder',
               #                value = 'C:\\Data Science\\nMARQ Shiny\\test'),
               #textOutput("hello"),
               tags$div(class="form-group shiny-input-container",
                        #tags$label("Select a folder"),
                        tags$div(tags$label("Load Generator Case", class="btn btn-primary",
                                            tags$input(id = "Dir", webkitdirectory = TRUE,
                                                       type = "file", accept=".txt", style="display: none;", onchange="pressed()"))),
                        tags$div(id="Dir_progress", class="progress progress-striped active shiny-file-input-progress",
                        tags$label("No folder choosen", id = "noFile"),
                        tags$div(class="progress-bar")
                        )
               ),
               actionButton("demo", "Load Demo"),
               tags$hr(),
               textInput("case", label="Case Name", value = NULL, width = "100%", placeholder = "Case Name"),
               textInput("doctor", label="Physician", value = "", width = "100%", placeholder = "John"),
               textInput("site", label="Site", value = "", width = "100%", placeholder = "Site Name"),
               #tags$textarea(id="description", rows=3, cols=40, "Default value")
               textInput("date", label="Date", value="", width = "100%"),
               textInput("description", label="Case Description", value = "", width = "100%", placeholder = "Demo"),
               tags$hr()
               
          ),
          mainPanel(
               tabsetPanel(
                    tabPanel("Ablation View", 
                             
                             fluidRow(
                                  column(2,selectInput("file", label = "Ablations",choices=NULL)),
                                  column(2,selectInput("elec", label = "Select Electrode", 1, selected = 1))
                             #tags$style(type='text/css', "#next { margin-top: 25px;}"
                             ),
                             fluidRow(
                                  column(10,plotlyOutput("ElectrodePlot",height = "500px")),
                                  column(2,br(),br(),tableOutput('table1'))
                             ),
                                                     
                             textInput("annotate", label=NULL, value = "", width = "80%", placeholder = "Annotate graph..."),
                             tags$hr(),
                             selectInput("param", label = "Select Parameter", 
                                         choices = list("Power" = 3, "Temperature" = 6, "Impedance" = 5), 
                                         selected = 1),
                             fluidRow(
                                  column(10,plotlyOutput("ParamPlot",height = "500px")),
                                  column(2,br(),br(),tableOutput('table2'))
                             )),
                    
                    tabPanel("Case View", 
                             br(),
                             uiOutput("load_case"),
                             fluidRow(
                                  column(2,selectInput("param_case_y", label = "Select Y Axis Parameter", 
                                                       choices = list("Power" = "Pow", "Temperature" = "Temp", "Impedance" = "Imp", "Energy"="Energy",
                                                                      "Current"="Current", "Voltage"="Voltage","Duration"="Duration", "TimebetAbl"="TimebetAbl"), selected = "Pow")
                                         ),
                                  column(2,selectInput("sumy", label = "Summarize by", 
                                                       choices = list("mean" = "mean"), selected="mean")
                                         ),
                                  column(2,checkboxInput("active", "Only active", value = TRUE)
                                         )
                                  
                             ),
                             
                             plotlyOutput("CasePlot",height = "500px"), 
                             tags$hr(),
                             selectInput("param_t", label = "Select Parameter", 
                                         choices = list("Power" = "Pow", "Temperature" = "Temp", "Impedance" = "Imp", 
                                                        "Current"="Current", "Voltage"="Voltage"), selected=NULL),
                             plotlyOutput("TimePlot",height = "500px")
                             ),
                    
                    tabPanel("Export Data", 
                             br(),
                             downloadButton('downloadSumData', 'Export Summary Data'),
                             br(),
                             DT::dataTableOutput('Sum_data'),
                             tags$hr(),
                             downloadButton('downloadRawData', 'Export Raw Data'),
                             br(),
                             DT::dataTableOutput('All_data'),
                             tags$hr(),
                             downloadButton('downloadReport', 'Export Raw Data to R'),
                             tags$hr()
                    ),
                    
                    tabPanel("Carto Analysis Demo", 
                             #passwordInput("password1", "Password:"),
                             h2("Carto visualization demo"),
                             fluidRow(
                                  column(4,
                                         sliderInput("opacity_qdot", "Anatomy Opacity:",
                                                     min = 0, max = 1, value = 1, step= 0.1)
                                         ),
                                  column(2,selectInput("color_qdot", label = "Color", 
                                                       choices = list("Target Power" = "Pow_target", "Energy" = "Energy_Joules", "Median Force"="ForceValue_median","Impedance Drop" = "Imp_drop", 
                                                                      "Temp Rise"="Temp_rise"), selected="Pow_target"))
                             ),
                             
                             fluidRow(
                                  column(8,plotlyOutput("qdot1",height = "700px")),
                                  column(2,plotlyOutput("qdot2",height = "700px"))
                                  ),
                                  
                             tags$hr()
                             # h2("nMARQ catheter visualization demo"),
                             # sliderInput("opacity_nMARQ", "Anatomy Opacity:",
                             #             min = 0, max = 1, value = 1, step= 0.1),
                             # plotlyOutput("nmarq",height = "700px")
                             
                             
                    ),
                             
                    tabPanel("Big Picture Demo", 
                             passwordInput("password2", "Password:"),
                             div(style='height:800px; overflow-y: scroll',
                                 uiOutput("md")
                             )
                             
                    )
               )
          )
     ))
)