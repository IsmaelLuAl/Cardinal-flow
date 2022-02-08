library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(waiter)

## Ficheros externos
source("helpers.R", local = TRUE)
memory.limit(size = 9999999999999)

shinyServer(function(input, output, session) {
    
    # Allows the use of javascript in the application
    useShinyjs()
    
    tags$head(tags$style(HTML('.box{-border-top: none;}')))
    
    ## Reactive Values
    valuesDataName <- reactiveValues(df = NULL)
    valuesBinning <- reactiveValues(df = NULL)
    
    dataIntensities <- reactiveValues(df = FALSE)
    
    ## Reactive buttons
    loadFilesButton <- reactiveValues(ok = FALSE)
    preProcessButton <- reactiveValues(ok = FALSE)
    maxIntensitiesButton <- reactiveValues(ok = FALSE)
    generateImageInt <- reactiveValues(ok = FALSE)
    binningButton <- reactiveValues(ok = FALSE)
    peakPickingButton <- reactiveValues(ok = FALSE)
    kmeansButton <- reactiveValues(ok = FALSE)

    ## waiter load files
    waiterLoadFiles <- Waiter$new(id = "filesBox", html = spin_folding_cube(), color = "#00c0ef")
    ## Waiter preprocess
    waiterPreprocess <- Waiter$new(id = "preprocessingBox", html = spin_folding_cube(), color = "#00c0ef")
    ## Waiter binning
    waiterBinning <- Waiter$new(id = "binningBox", html = spin_folding_cube(), color = "#00c0ef")
    ## Waiter peakPicking
    waiterPeakPicking <- Waiter$new(id = "peakPickingBox", html = spin_folding_cube(), color = "#00c0ef")
    ## Waiter Kmeans
    waiterKmeans <- Waiter$new(id = "kmeansBox", html = spin_folding_cube(), color = "#00c0ef")
    
    # if (length(all_tejidos_pick_no_combine) >= 1) {
    # 
    #     updateSelectInput(session, "peakName", choices = names(all_tejidos_pick_no_combine))
    # 
    # }
    
    ################################# Pantalla de Load Files #################################
    
    ## Objeto observador del boton de cargar los archivos
    observeEvent(input$loadButton, {
        
        # Deshabilitamos el botón de cargar cuando lo pulsamos y la navegacion entre las tabs
        shinyjs::disable("loadButton")
        shinyjs::disable(selector = '.sidebar-menu a')
        
        loadFilesButton$ok <- FALSE
        
        waiterLoadFiles$show()
        
        
        ## La barra de progresión se muestra al pulsar el botón de cargar los archivos
        withProgress(message = "Progress bar",
                     
                     detail ='Reading files...',
                     value = 0,{
                         
                         # La barra de progresión se llena a la mitad antes de empezar el proceso
                         # de carga de archivos
                         setProgress(0.5)
                         
                         # Se llama a la función de carga y lectura de archivos que se encuentra en helpers
                         my_list <<- loadAndRead()
                         
                         valuesDataName$dataNames <<- data.frame(imzMLFiles = c(names(my_list)))
                         
                         
                         # Se completa el 100% cuando se acaba la llamada a la función
                         setProgress(1)
                     }
        )
        
        if(length(my_list) <= 0) {
            
            ## Notificación no se han contrado los archivos
            showNotification("No files were found", duration = 5, closeButton =  TRUE, type = "error")
            
        }else{
            
            shinyjs::show(id = "loadedFilesH3")
            shinyjs::show(id = "loadedFilesNames")
            shinyjs::hide(id = "loadFilesH3")
            
            ## Muestra un mensaje cuando se completa la carga de archivos
            showNotification("The files were read and uploaded!", duration = 5, closeButton =  TRUE, type = "message")
        }
        
        loadFilesButton$ok <- TRUE
        
        waiterLoadFiles$hide()
        
        shinyjs::enable(selector = '.sidebar-menu a')
        
    })
    
    
    ## Objeto de la tabla de los archivos cargados
    output$dataNames = renderDataTable({
        
        if (loadFilesButton$ok) {
            
            shinyjs::enable("loadButton")
            shinyjs::enable("preprocessButton")
            
            datatable(valuesDataName$dataNames)
            
        }
        
    })
    
    ################################# Pantalla de Preprocess #################################
    
    ## Evento observador para cuando se cambia si se desea aplicar el metodo smooth
    observeEvent(input$radioButtonSmooth, {
        
        if (input$radioButtonSmooth == "yes") {
            
            shinyjs::show(id = "SmoothColumn")
            
        } else {
            
            shinyjs::hide(id = "SmoothColumn")
            
        }
        
    })
    
    ## Evento observador para cuando se cambia si se desea aplicar el metodo reduce baseline
    observeEvent(input$radioButtonReduce, {
        
        if (input$radioButtonReduce == "yes") {
            
            shinyjs::show(id = "ReduceColumn")
            
        } else {
            
            shinyjs::hide(id = "ReduceColumn")
            
        }
        
    })
    
    ## Render del ui tras cambiar el input select smooth
    output$secondSelectSmooth <- renderUI({
        
        switch(input$smoothMethod,
               
               "gaussian" =  numericInput("smoothNumber", "Window:", width = '23%',
                                          value = 5),
               
               "sgolay" =  numericInput("smoothNumber", "Order:",  width = '23%',
                                        value = 5),
               
               "ma" =  numericInput("smoothNumber", "Window:",  width = '23%',
                                    value = 5)
               
        )
        
    })
    
    # Render del ui tras cambiar el input select Reduce
    output$secondSelectReduce <- renderUI({
        
        switch(input$reduceMethod,
               
               "locmin" =  numericInput("reduceNumber", "Window:", width = '23%',
                                        value = 5),
               
               "median" =  numericInput("reduceNumber", "Blocks:",  width = '30%',
                                        value = 500)
        )
        
    })
    
    ## Observador del boton preprocess
    observeEvent(input$preprocessButton, {
        
        waiterPreprocess$show()
        
        ## Loading
        shinyjs::disable(selector = '.sidebar-menu a')
        shinyjs::disable("preprocessButton")
        shinyjs::disable("maxIntensitiesButton")
        # calculate3MaxButton
        # generateImageMaxIntButton
        
        preProcessButton$ok <- FALSE
        
        if (input$radioButtonSmooth == "yes") {
            
            dataSmooth <- list("smoothMethod" = input$smoothMethod, "smoothNumber" = input$smoothNumber)
            
        } else {
            
            dataSmooth <- NULL
        }
        
        if (input$radioButtonReduce == "yes") {
            
            dataReduce <- list("reduceMethod" = input$reduceMethod, "reduceNumber" = input$reduceNumber)
            
        } else {
            
            dataReduce <- NULL
            
        }
        
        
        withProgress(message = 'Preprocessing Files..', value = 0, {
            
            # Each time through the loop, add another row of data. This is
            # a stand-in for a long-running computation.
            list_preprocessed <<- preProcess(
                
                pPalList = my_list,
                methodNormalize = input$methodNormalize,
                dataReduce = dataReduce,
                dataSmooth = dataSmooth)
            
        })
        
        withProgress(message = 'Processing Images from Media Spectra', value = 0, {
            
            nombres_graficas <<- mediaSpectra(preprocess_list = list_preprocessed, names = list_names)
            
        })
        
        
        updateSelectInput(session, "list3Max", choices = names(list_preprocessed))
        
        
        shinyjs::hide(id = "preprocessedImagesH2_1")
        shinyjs::show(id = "mediaSpectrumImage1")
        shinyjs::show(id = "mediaSpectrumImage2")
        shinyjs::show(id = "mediaSpectrumImage3")
        shinyjs::show(id = "mediaSpectrumImage4")
        shinyjs::show(id = "rowIntensities")
        
        preProcessButton$ok <- TRUE
        
        shinyjs::enable("preprocessButton")
        shinyjs::enable("maxIntensitiesButton")
        shinyjs::enable(selector = '.sidebar-menu a')
        
        waiterPreprocess$hide()
        
    })
    
    ## Imagen media spectra 1
    output$mediaSpectrumImage1 <- renderImage({
        
        if (exists("nombres_graficas")) {
            
            path_grafica <- paste("data/Imagenes/Preprocesamiento/Graficas/", nombres_graficas[1],".png", sep = "")
            
            return(list(
                src = path_grafica,
                contentType = "image/png",
                width = "100%",
                height = "100%"
            ))
            
        } else {
            
            showNotification("The files have not been pre-processed correctly", duration = 5, closeButton =  TRUE, type = "error")
            
        }
        
    }, deleteFile = FALSE)
    
    ## Imagen media spectra 2
    output$mediaSpectrumImage2 <- renderImage({
        
        if (exists("nombres_graficas")) {
            
            path_grafica <- paste("data/Imagenes/Preprocesamiento/Graficas/", nombres_graficas[2],".png", sep = "")
            
            return(list(
                src = path_grafica,
                contentType = "image/png",
                width = "100%",
                height = "100%"
            ))
            
        } else {
            
            showNotification("The files have not been pre-processed correctly", duration = 5, closeButton =  TRUE, type = "error")
            
        }
        
    }, deleteFile = FALSE)
    
    ## Imagen media spectra 3
    output$mediaSpectrumImage3 <- renderImage({
        
        if (exists("nombres_graficas")) {
            
            path_grafica <- paste("data/Imagenes/Preprocesamiento/Graficas/", nombres_graficas[3],".png", sep = "")
            
            return(list(
                src = path_grafica,
                contentType = "image/png",
                width = "100%",
                height = "100%"
            ))
            
        } else {
            
            showNotification("The files have not been pre-processed correctly", duration = 5, closeButton =  TRUE, type = "error")
            
        }
        
    }, deleteFile = FALSE)
    
    ## Imagen media spectra 4
    output$mediaSpectrumImage4 <- renderImage({
        
        if (exists("nombres_graficas")) {
            
            path_grafica <- paste("data/Imagenes/Preprocesamiento/Graficas/", nombres_graficas[4],".png", sep = "")
            
            return(list(
                
                src = path_grafica,
                contentType = "image/png",
                width = "100%",
                height = "100%"
                
            ))
            
        } else {
            
            showNotification("The files have not been pre-processed correctly", duration = 5, closeButton =  TRUE, type = "error")
            
        }
        
    }, deleteFile = FALSE)
    
    
    ## Observador del botón 3max intensities
    
    observeEvent(input$calculate3MaxButton, {

        nameSelected <- input$list3Max
        tejidoPrep <<- list_preprocessed[[nameSelected]]

        dataIntensities <<- make3MaxAverageIntensities(tejidoPreprocessed = tejidoPrep)

        itensitiesList <- c(dataIntensities$intensidades_total[["intensidad1"]],
                            dataIntensities$intensidades_total[["intensidad2"]],
                            dataIntensities$intensidades_total[["intensidad3"]])

        updateSelectInput(session, "threeMaxIntensities", choices = itensitiesList)

    })
    
    observeEvent(input$generateImageMaxIntButton, {
        
        my_path <- getwd()
        intensity <- as.numeric(input$threeMaxIntensities)
        plusMinus <- input$plusminusIntensity
        pathTemp <- dataIntensities$path_images
        name_intensity <- format(round(intensity, 2), nsmall = 2)
        final_intensity_name <- gsub("\\.", "_", name_intensity)
        fileName <<- paste("Intensidad_max_", final_intensity_name,"_tejido_", tejidoPrep@metadata$name, ".tiff", sep = "")
        
        setwd(pathTemp)
        
        
        imageIntMax <- image(tejidoPrep, mz = intensity, plusminus = plusMinus)
        
        windows()
        print(imageIntMax)
        
        savePlot(filename = fileName, type = "tiff", device = dev.cur())
        dev.off()
        setwd(my_path)
    })
    
    output$maxIntImage <- renderImage({
        
        if (is.null(input$threeMaxIntensities)) {
            
            return(NULL)
            
        } else {
            
            input$maxIntImage <- as.character()
            
            return(list(
                src = "images/face.png",
                contentType = "image/png",
                alt = "Face"
            ))
        }

        image(tejidoPrep, mz = intensity, plusminus = plusMinus)

    })
    
    # output$tabset1Selected <- renderText({
    #     input$tabset1
    # })
    
    ################################# Pantalla de Binning #################################
    
    observeEvent(input$binningButton, {
        
        waiterBinning$show()
        
        shinyjs::disable(selector = '.sidebar-menu a')
        shinyjs::disable("binningButton")
        
        my_resolution <- input$resolutionBinning
        my_units <- input$unitsBinning
        
        list_binned <<- binning(preprocessed = list_preprocessed, units = my_units, binning_number = my_resolution)
        
        valuesBinning <<- list_binned
        
        shinyjs::enable(selector = '.sidebar-menu a')
        shinyjs::enable("binningButton")
        
        waiterBinning$hide()
        
    })
    
    ################################# Pantalla de Peak picking #################################
    
    observeEvent(input$peakPickingButton, {
        
        waiterPeakPicking$show()
        
        shinyjs::disable(selector = '.sidebar-menu a')
        shinyjs::disable("peakPickingButton")
        
        my_SNR <- input$SNRPeakPicking
        my_peak_method <- input$peakPickingMethods
        
        all_tejidos_pick_no_combine <- bigPeakPicking(binned = list_binned, SNR_Number = my_SNR, my_peak_method = my_peak_method)
        
        assign("all_tejidos_pick_no_combine",
               all_tejidos_pick_no_combine,
               envir = globalenv())
    
        updateSelectInput(session ,inputId = "peakName", choices = names(all_tejidos_pick_no_combine))
        
        shinyjs::enable(selector = '.sidebar-menu a')
        shinyjs::enable("peakPickingButton")
        
        waiterPeakPicking$hide()
        
    })
    
    ################################# Pantalla de kmeans #################################
    
    observeEvent(input$kmeansButton, {
        
        waiterKmeans$show()
        
        shinyjs::disable(selector = '.sidebar-menu a')
        shinyjs::disable(selector = "kmeansButton")
        
        my_R <- as.numeric(input$rNumberKmeans)
        my_K <- input$kNumberKmeans
        my_kmeans_method <- input$kmeansMethods
        my_object_kmeans <- all_tejidos_pick_no_combine[[input$peakName]]
        
        updateSelectizeInput(inputId = "clustersSelect", choices = c(1:my_K), selected = 1)
        
        object_Kmeans <- my_kmeans(object = my_object_kmeans, r_Number = my_R, k_Number = my_K, algorithm_Name = my_kmeans_method)

        object_Kmeans <- spatialKMeans(my_object_kmeans, r = my_R,
                                       k = my_K,
                                       algorithm = my_kmeans_method,
                                       method = 'gaussian',
                                       weights = 1,
                                       iter.max = 10, nstart = 50,
                                       ncomp = 100)
        
        # object_Kmeans <- spatialShrunkenCentroids(my_object_kmeans,
        #                                           r = 1,
        #                                           k = 6,
        #                                           s = 3,
        #                                           method = 'gaussian')
        
        assign("object_kmeans",
               my_kmeans(object = my_object_kmeans, r_Number = my_R, k_Number = my_K, algorithm_Name = my_kmeans_method),
               envir = globalenv())

        shinyjs::enable(selector = '.sidebar-menu a')
        shinyjs::enable(selector = "kmeansButton")
        
        waiterKmeans$hide()
        
    })
    
    observeEvent(input$loadPeakNames, {
        
        if (exists("all_tejidos_pick_no_combine")) {
            
            updateSelectInput(session, "peakName", choices = names(all_tejidos_pick_no_combine))
            
        } else {
            
            ## Poner mensaje emergente no existe archivos con el peak pickig
        }
    })
    
    observeEvent(input$loadClusters, {
        
        my_K <- input$kNumberKmeans    
        updateSelectizeInput(inputId = "clustersSelect", choices = c(1:my_K), selected = 1)
            
    })
    
    
    observeEvent(input$dissectionButton, {

        my_cluster_list <- as.integer(input$clustersSelect)
        file_selected <- my_list[[input$peakName]]
        object_Kmeans <- list_Kmeans[[1]]
        
        assign("dissection",
               my_dissection(object = object_Kmeans,
                             my_file = file_selected,
                             r_number = input$rNumberKmeans,
                             k_number = input$kNumberKmeans,
                             cluster_list = my_cluster_list),
               envir = globalenv())

    })
    
    ## Descomentar cuando se reanude este elemento
    output$imagenPrueba <- renderImage({
        # A temp file to save the output.
        # This file will be removed later by renderImage
        outfile <- tempfile(fileext = '.png')

        # Generate the PNG
        # png(outfile, width = 400, height = 300)
        if (exists(dissection)) {
            image(dissection)
        }

        dev.off()

        # Return a list containing the filename
        list(src = outfile,
             contentType = 'image/png',
             width = 400,
             height = 300,
             alt = "Aun no se han obtenido resultados")
    }, deleteFile = TRUE)
    
    
    # #####################################################
    
    # pData(my_file)$segment <- object@resultData$`r = r_number, k = k_number`$cluster[]
    # 
    # 
    # #para un solo segmento
    # #pulmon119_1_kmeansmorado <- pulmon119_1[,pData(pulmon119_1)$segment == 6]
    # 
    # #para varios segmentos
    # object_clustersel <- my_file[,pData(my_file)$segment %in% c(6, 7, 8, 12, 13, 14)]
    # 
    # image(estudio6y8dpi_replicaunica_3tent_clustersel, mz = 1180.64, contrast.enhance = "histogram",
    #       smooth.image = "gaussian", key = FALSE, strip = FALSE, normalize.image = 'linear',
    #       layout = c(3,5), main = "dpi selected clusters", col.regions=mapavir)
    # 
    # plot(estudio6y8dpi_replicaunica_3tent_clustersel,
    #      pixel=1:ncol(estudio6y8dpi_replicaunica_3tent_clustersel), col="darkred", main="estudio6y8dpi cluster sel", layout=c(1,1))

})
