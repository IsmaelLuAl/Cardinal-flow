# User interface para Flujo de Analisis

library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(DT)
library(shinycssloaders)
library(waiter)

source("helpers.R")


## Colores para las Tab box del preprocesamiento
js <- '.nav-tabs-custom .nav-tabs li.active {
    border-top-color: #00c0ef;
}"'

## Inicio del dashboard de la app
dashboardPage(skin = "black",
              
              dashboardHeader(
                
                title = "Analysis flow"
                
              ),
              
              dashboardSidebar(
                
                sidebarMenu(
                  
                  menuItem("Load files", tabName = "loadFiles", icon = icon("file-upload")),
                  menuItem("Preprocess", tabName = "preprocess", icon = icon("microchip")),
                  menuItem("Binning", tabName = "binning", icon = icon("cogs")),
                  menuItem("Peak Picking", tabName = "peakPicking", icon = icon("think-peaks")),
                  menuItem("Kmeans", tabName = "kmeans", icon = icon("bullseye"))
                  
                  
                )
              ),
              
              dashboardBody(
                
                useShinyjs(),
                
                tags$style(js),
                
                use_waiter(),
                
                tabItems(
                  
                  ############################ Tab para la pestaña de Load Files ############################
                  tabItem(tabName = "loadFiles",
                          
                          fluidRow(
                            
                            box(
                              title = "Load imzML files", status = "warning", solidHeader = TRUE,
                              
                              h4("Please put the imzMl files in the 'data' folder of the project"),
                              
                              hr(),
                              
                              actionButton("loadButton", label = "Load Files")
                              
                            ),
                            
                            box(id = "filesBox",
                                title = "Loaded imzML files", status = "info", solidHeader = TRUE,
                                
                                hidden(tags$h3(id = "loadedFilesH3", "The uploaded files are: ")),
                                hidden(tags$h3(id = "loadedFilesNames", "")),
                                tags$h3(id = "loadFilesH3", "No file has been uploaded yet"),
                                
                                
                                DT::dataTableOutput("dataNames"), type = 2
                                
                                # footer = textOutput("valueLoadingFile")
                            )
                          )
                          
                  ),
                  
                  ############################ Tab para la pestaña de preprocess ############################
                  tabItem(tabName = "preprocess",
                          
                          fluidRow(
                            
                            
                            ## Caja para las opciones de preprocesamiento
                            box(width = 4,
                                
                                title = "Preprocess", status = "warning", solidHeader = TRUE,
                                
                                h4("The files will be pre-processed with the methods you select below"),
                                
                                hr(),
                                
                                
                                selectInput("methodNormalize", "Normalize method:", width = 120,
                                            
                                            choices = methodsNormalize
                                            
                                ),
                                
                                ## Sección para el método smooth
                                
                                fluidRow(
                                  
                                  
                                  column(6,
                                         
                                         radioButtons("radioButtonSmooth", "Do you want to apply the Smooth method to pre-processing?",
                                                      
                                                      choices = c("No" = "no",
                                                                  "Yes" = "yes"),
                                                      selected = "no"
                                                      
                                         )
                                         
                                  ),
                                  
                                  
                                  hidden(column(id = "SmoothColumn", 6,
                                                
                                                selectInput("smoothMethod", "Smooth method:", width = 120,
                                                            
                                                            choices = methodsSmooth
                                                            
                                                ),
                                                
                                                uiOutput("secondSelectSmooth")
                                                
                                  )
                                  
                                  )),
                                
                                br(),
                                
                                ## Sección para el metodo Reduce baseline
                                
                                fluidRow(
                                  
                                  
                                  column(6,
                                         
                                         radioButtons("radioButtonReduce", "Do you want to apply the Reduce baseline method to pre-processing?",
                                                      
                                                      choices = c("No" = "no",
                                                                  "Yes" = "yes"),
                                                      selected = "no"
                                                      
                                         )
                                         
                                  ),
                                  
                                  
                                  hidden(column(id = "ReduceColumn", 6,
                                                
                                                selectInput("reduceMethod", "Reduce method:", width = 120,
                                                            
                                                            choices = methodsReduceBaseline
                                                            
                                                ),
                                                
                                                uiOutput("secondSelectReduce")
                                                
                                  )
                                  
                                  )),
                                
                                actionButton("preprocessButton", label = "Preprocess Files")
                                
                            ),
                            
                            
                            ## Caja para mostrar los espectros media
                            
                            box(id = "preprocessingBox", width = 8,
                                
                                tabBox(width = 12,
                                       title = "Media Spectrum",
                                       # The id lets us use input$tabset1 on the server to find the current tab
                                       id = "tabsetPreprocess",
                                       
                                       tabPanel("1",
                                                
                                                tags$h3(id = "preprocessedImagesH2_1", 'No file has been preprocessed yet'),
                                                hidden(imageOutput("mediaSpectrumImage1"))
                                                
                                       ),
                                       
                                       tabPanel("2",
                                                
                                                tags$h3(id = "preprocessedImagesH2_2", 'No file has been preprocessed yet'),
                                                hidden(imageOutput("mediaSpectrumImage2"))
                                                
                                       ),
                                       
                                       tabPanel("3",
                                                
                                                tags$h3(id = "preprocessedImagesH2_3", 'No file has been preprocessed yet'),
                                                hidden(imageOutput("mediaSpectrumImage3"))
                                                
                                       ),
                                       
                                       tabPanel("4",
                                                
                                                tags$h3(id = "preprocessedImagesH2_4", 'No file has been preprocessed yet'),
                                                hidden(imageOutput("mediaSpectrumImage4"))
                                                
                                       )
                                )
                            )
                          ),
                          
                          
                          ### Caja para las imagenes por intensidades maximas
                          # hidden(
                            
                            fluidRow(id = "rowIntensities",

                                      box(width = 4,

                                        title = "Max Intensitities", status = "warning", solidHeader = TRUE,

                                        column(6,

                                          h5("Obtain the 3 maximum intensities"),

                                          selectInput("list3Max", "File:", width = 160,

                                                      choices = "No processed files"

                                          ),

                                          sliderInput("plusminusIntensity", "Plusminus:",
                                                      min = 0.05, max = 0.5,
                                                      value = 0.05, step = 0.01),

                                          actionButton("calculate3MaxButton", label = "Calculate")

                                        ),

                                        column(6,

                                          h5("Generate Image"),

                                          selectInput("threeMaxIntensities", "Max intensity:",

                                                      choices = "Uncalculated intensities"),

                                          actionButton("generateImageMaxIntButton", label = "Generate Image")

                                        ),

                                        column(6,

                                          imageOutput("maxIntImage")

                                        )
                                      )
                            )
                          # )
                  ),
                  
                  ######################################### Tab para la pestaña de binning #########################################
                  
                  tabItem(tabName = "binning",
                          
                          fluidRow(
                            
                            box(
                              
                              title = "Binning files", status = "warning", solidHeader = TRUE,
                              
                              h4("The files will be binned with the parameters you select below"),
                              
                              hr(),
                              
                              column(4,
                                
                                numericInput("resolutionBinning", label = "Resolution:", 0.05),
                                
                                selectInput("unitsBinning", "Units:",
                                            
                                            choices = c("mz" = "mz",
                                                        "ppm" = "ppm")),
                                
                                actionButton("binningButton", label = "Binning Files")
                                
                              )
                              
                              
                              
                            ),
                            
                            box(id = "binningBox",
                                
                                title = "Binned imzML files", status = "info", solidHeader = TRUE,
                                
                                hidden(tags$h3(id = "binnedFilesH3", "The uploaded files are: ")),
                                hidden(tags$h3(id = "binnedFilesNames", "")),
                                tags$h3(id = "binningFilesH3", "No file has been binned yet")
                                
                                
                                # DT::dataTableOutput("dataBinning"), type = 2,
                                
                            )
                          )
                  ),
                   
                  
                  ######################################### Tab para la pestaña de peak Picking #########################################
                  
                  tabItem(tabName = "peakPicking",
                          
                          fluidRow(
                            
                            box(
                              
                              title = "Peak picking files", status = "warning", solidHeader = TRUE,
                              
                              h4("The files will be peak picked with the parameters you select below"),
                              
                              hr(),
                              
                              column(6,
                                     
                                     numericInput("SNRPeakPicking", label = "SNR:", 4),
                                     
                                     selectInput("peakPickingMethods", "Methods:",

                                                 choices = c("mad" = "mad",
                                                             "simple" = "simple",
                                                             "adaptative" = "adaptative",
                                                             "limpic" = "limpic")),

                                     actionButton("peakPickingButton", label = "Peak Picking Files")
                                     
                              )
                              
                              
                              
                            ),
                            
                            box(id = "peakPickingBox", 
                                
                                title = "Picked imzML files", status = "info", solidHeader = TRUE,
                                
                                # hidden(tags$h3(id = "pickedFilesH3", "The picked files are: ")),
                                # hidden(tags$h3(id = "pickedFilesNames", "")),
                                tags$h3(id = "pickedFilesH3", "No file has been picked yet")
                                
                            )
                          )
                  ),
                  
                  
                  
                  ######################################### Tab para la pestaña de Kmeans #########################################
                  
                  
                  tabItem(tabName = "kmeans",
                          
                          fluidRow(
                            
                            
                            box(
                              
                              title = "Kmeans cluster", status = "warning", solidHeader = TRUE,
                              
                              h4("A cluster object (Kmeans) shall be created with the following attributes"),
                              
                              hr(),
                              
                              column(6,
                                     
                                     actionButton("loadPeakNames", label = "Load peaked files"),
                                     
                                     br(),
                                     
                                     selectInput("peakName", "File:", width = "75%",

                                                 choices = "Uncalculated peak picking files"

                                                 ),
                                     
                                     selectInput("rNumberKmeans", "R:", width = "75px",
                                                 
                                                 choices = c("1" = "1",
                                                             "2" = "2")),
                                     
                                     numericInput("kNumberKmeans", label = "K:", 10, width = "75px"),
                                     
                                     selectInput("kmeansMethods", "Methods:", width = "150px",
                                                 
                                                 choices = c("Lloyd" = "Lloyd",
                                                             "Forgy" = "Forgy",
                                                             "Hartigan-Wong" = "Hartigan-Wong",
                                                             "MacQueen" = "MacQueen")),
                                     
                                     actionButton("kmeansButton", label = "Kmeans")
                                     
                              )
                              
                              
                              
                            ),
                            
                            box(id = "kmeansBox",
                                
                                imageOutput("imagenPrueba")
                                
                                # title = "Picked imzML files", status = "info", solidHeader = TRUE,
                                # 
                                # hidden(tags$h3(id = "pickedFilesH3", "The picked files are: ")),
                                # hidden(tags$h3(id = "pickedFilesNames", "")),
                                # tags$h3(id = "pickedFilesH3", "No file has been picked yet")

                                
                            )
                          ),
                          
                          fluidRow(
                            
                            box(
                              
                              title = "Virtual dissection", status = "warning", solidHeader = TRUE,
                              
                              h4("A virtual dissection will be carried out with the following parameters"),
                              
                              hr(),
                              
                              actionButton("loadClusters", label = "Load clusters"),
                              
                              br(),
                              
                              selectizeInput(width = "50%",
                                'clustersSelect', 'Selects clusters for dissection', choices = "No clusters yet", multiple = TRUE
                              ),
                              
                              actionButton("dissectionButton", label = "Digital dissection")
                              
                            )
                          )
                  )
                  

                  
                  
                  
                  
                  #################################################################
                  

                  # tabItem(tabName = "peakPicking",
                  #         
                  #         fluidRow(
                  #           
                  #           actionButton("peakPickingButton", label = "Peak Picking")
                  #         )
                  # ),
                  
                  
                )
              )
)



#
# shinyUI(fluidPage(
#
#     shinyjs::useShinyjs(),
#
#     # Application theme
#     theme = shinytheme("yeti"),navbarPage(
#
#         "Work flow",
#         tabPanel("imzML Files",
#
#                  sidebarPanel(
#                      tags$h3("Load imzMl files"),
#                      actionButton("load", label = "Load Files"),
#
#                      box(
#                          title = "Inputs", status = "warning", solidHeader = TRUE,
#                          "Box content here", br(), "More box content",
#                          sliderInput("slider", "Slider input:", 1, 100, 50),
#                          textInput("text", "Text input:")
#                      ),
#
#                  ), # sidebarPanel
#
#                  mainPanel(
#                      h2("Please put the imzMl files in the 'data' folder of the project"),
#                      br(),
#                      hidden(tags$h3(id = "loadedFilesH3", "The uploaded files are: ")),
#                      hidden(tags$h3(id = "loadedFilesNames", "")),
#                      tags$h3(id = "loadFilesH3", "No file has been uploaded yet"),
#
#                  ) # mainPanel
#
#         ), # Navbar 1, tabPanel
#
#         navbarMenu("Preprocess",
#
#                    tabPanel("Preprocessing",
#                             sidebarPanel(
#                                 tags$h3("Preprocessing"),
#                                 actionButton("preprocess", label = "Preprocess"),
#                             )
#                    ),
#                    tabPanel("Mean spectra", "This panel is intentionally left blank"),
#                    tabPanel("3 maximum intensities", "This panel is intentionally left blank")
#         ),
#
#         #Preprocess panel
#
#         tabPanel("Binning", "This panel is intentionally left blank"),
#         tabPanel("Peakpicking", "This panel is intentionally left blank"),
#         tabPanel("Kmeans", "This panel is intentionally left blank")
#
#     ) # navbarPage
#
# ))
