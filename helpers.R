library(Cardinal)
library(colormap)
library(BiocParallel)
library(zoom)
library(plotly)
library(readr)
library(Factoshiny)
library(gplots)
library(rlist)
library(ggplot2)
library(gridExtra)
library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(DT)
library(shinycssloaders)
library(waiter)

############################################# Constantes ############################################

##SnowPAram = Numero de procesadores
register(SnowParam(12, tasks = 0L, progressbar = TRUE))
register(SerialParam(stop.on.error = TRUE, progressbar = TRUE))

bpparam<-SnowParam(workers = 7, stop.on.error = TRUE, progressbar = TRUE)


my_colors <- c("#003f5c","#2f4b7c","#665191", "#a05195" ,"#d45087", "#f95d6a", "#ff7c43" ,"#ffa600", "#FF2256")
methodsNormalize <- c("tic", "rms", "reference")
methodsReduceBaseline <- c("locmin", "median")
methodsSmooth <- c("gaussian", "sgolay", "ma")
myWD <- getwd()
dataWD <- paste(myWD, "/data", sep = "")
# list_preprocessed <- list()

# Options for Spinner
options(spinner.color = "orange", spinner.color.background = "#ffffff", spinner.size = 1)

############################################# Controlador ############################################

snowWorkers(type = c("SOCK", "MPI", "FORK"))

############################################# Funciones #############################################

## Se crea una lista temporal con el nombre de los archivos y posteriormente se procesan esos archivos para transformarlos en
## MSImagesets y seguidamente almacenarlos en una lista de forma conjunta
## Una vez terminada la lista se borran todas las imagenes almacenadas en el entorno
loadAndRead <- function() {
  
  # input$valueLoadingFile <- "Leyendo ficheros.."
  
  memory.limit(99999999999)
  setwd(dataWD)
  temp = list.files(pattern="*.imzML")
  list2env(my_list <- lapply(setNames(temp, make.names(gsub("*.imzML$", "", temp))), readMSIData), envir = .GlobalEnv)
  list_names <<- names(my_list)
  
  ## Se comprueba si existen los ficheros necearios para guardar las imagenes
  ## Si no existen los crea en el directorio actual de trabajo
  path_graficas  <- "/Imagenes/Preprocesamiento/Graficas"
  path_images <- "/Imagenes/Preprocesamiento/Images"
  my_path_graficas <- paste(dataWD, path_graficas, sep = "")
  my_path_images <- paste(dataWD, path_images, sep = "")
  
  if (file.exists(my_path_graficas)) {
    
    message("El fichero Graficas ya existe")
    
  } else {
    
    ## El atributo recursive sirve para que cree tantos ficheros como sean necesarios y no solo el primero
    dir.create(file.path(dataWD, path_graficas), recursive = TRUE)
    
  }
  
  if (file.exists(my_path_images)) {
    
    message("El fichero Images ya existe")
    
  } else {
    ## El atributo recursive sirve para que cree tantos ficheros como sean necesarios y no solo el primero
    dir.create(file.path(dataWD, path_images), recursive = TRUE)
    
  }
  
  rm(path_images, path_graficas, my_path_images, my_path_graficas, temp)
  
  setwd(myWD)
  
  return(my_list)
}


## Funcion que crea tres bucles que sirven para bufferear las dos operaciones necesarias en preprocesamiento
## en cada una de las imagenes de forma secuencial y con el ultimo bucle ejecutamos el preprocesamiento
## PARAMS: Lista principal que contiene los cortes antes de cualquier operacion
## RETURN: Una lista de todos los cortes una vez ya preprocesados
preProcess <- function(pPalList, methodNormalize, methodReduceBase, dataSmooth = NULL, dataReduce = NULL) {
  
  list_processed <- list()
  
  # Number of times we'll go through the progress bar
  n <- length(pPalList)
  
  for (imagen in pPalList) {
    name <- imagen@metadata$name
    list_processed[[name]] <- normalize(imagen, method = methodNormalize)
  }
  
  ## Se ejecuta el preprocesamiento con el metodo smooth si "dataSmooth" es diferente de NULL
  if (!is.null(dataSmooth)) {
    
    ## Se ejecuta el metodo Smooth pero con las variantes 'gaussian' y 'ma'
    ## El else ejecuta el metodo restante
    if (dataSmooth$smoothMethod == "gaussian" || dataSmooth$smoothMethod == "ma") {
      
      for (imagen in list_processed) {
        
        name <- imagen@metadata$name
        list_processed[[name]] <- smooth(imagen, method = dataSmooth$smoothMethod, window = dataSmooth$smoothNumber)
        
      }
      
    } else {
      
      for (imagen in list_processed) {
        
        name <- imagen@metadata$name
        list_processed[[name]] <- smooth(imagen, method = dataSmooth$smoothMethod, order = dataSmooth$smoothNumber)
        
      }
    }
  }
  
  
  if (!is.null(dataReduce)) {
    
    if (dataReduce$reduceMethod == "locmin") {
      
      for (imagen in list_processed) {
        
        name <- imagen@metadata$name
        list_processed[[name]] <- reduceBaseline(imagen, method = dataReduce$reduceMethod, window = dataReduce$reduceNumber)
        
      }
      
    } else {
      
      for (imagen in list_processed) {
        
        name <- imagen@metadata$name
        list_processed[[name]] <- reduceBaseline(imagen, method = dataReduce$reduceMethod, blocks = dataReduce$reduceNumber)
        
      }
    }
  }
  
  
  for (imagen in list_processed) {
    
    name <- imagen@metadata$name
    list_processed[[name]] <- process(imagen)
    
    # Increment the progress bar, and update the detail text.
    incProgress(1/n, detail = paste("File: ", name, " preprocessed", sep = ""))
    
    message(paste("Tejido ",imagen@metadata$name, " preprocesado ", sep = ""))
    
  }
  
  ## Si la lista de preprocesado es igual que la lista de tejidos se borran las que no son necesarias
  ## Si son diferentes borra todas las listas creadas
  
  if(length(my_list) == length(list_processed)) {
    
    showNotification("The files were preprocessed", duration = 5, closeButton =  TRUE, type = "message")
    
    # gc()
    
  } else {
    
    showNotification("Error, The files could not be pre-processed", duration = 5, closeButton =  TRUE, type = "message")
    
  }
  
  # gc()
  
  is.atomic(list_processed)
  
  return(list_processed)
  
}

## Funcion que recorre la lista con los elementos preprocesados genera una grafica por cada uno de ellos y las va almacenando en 4 listas
## de 9 graficas cada una hasta que se acaban los elementos
## PARAMS: La lista de elementos preprocesados y la lista con los nombres de los tejidos
savePlotsGrids <- function(preprocessed, names) {
  
  list_1_grid <- list()
  list_2_grid <- list()
  list_3_grid <- list()
  list_4_grid <- list()
  
  contador <- 1
  # Number of times we'll go through the progress bar
  n <- length(preprocessed)
  contador_grid_1 <- 1
  contador_grid_2 <- 1
  contador_grid_3 <- 1
  contador_grid_4 <- 1
  
  for (imagen_pre in preprocessed) {
    
    nombre_grafica <- paste("tejido_", names[contador], sep = "")
    
    if (contador <= 9) {
      
      list_1_grid[[nombre_grafica]] <- plot(imagen_pre, col = my_colors[contador_grid_1], main = nombre_grafica)
      contador_grid_1 %+=% 1
      
    } else if(contador > 9  && contador <= 18) {
      
      list_2_grid[[nombre_grafica]] <- plot(imagen_pre, col = my_colors[contador_grid_2], main = nombre_grafica)
      contador_grid_2 %+=% 1
      
    } else if(contador > 18 && contador <= 27) {
      
      list_3_grid[[nombre_grafica]] <- plot(imagen_pre, col = my_colors[contador_grid_3], main = nombre_grafica)
      contador_grid_3 %+=% 1
      
    } else {
      
      list_4_grid[[nombre_grafica]] <- plot(imagen_pre, col = my_colors[contador_grid_4], main = nombre_grafica)
      contador_grid_4 %+=% 1
      
    }
    
    message(paste("Tejido numero ",contador, " de ", length(preprocessed), sep = ""))
    
    # Increment the progress bar, and update the detail text.
    incProgress(1/n, detail = paste("Grafica numero ", contador, " hecha", sep = ""))
    
    contador %+=% 1
  }
  
  grids_list <- list(list_1_grid, list_2_grid, list_3_grid, list_4_grid)
  
  gc()
  return(grids_list)
}

## Funcionx que crea una rejilla con las graficas de las listas que pases
## PARAMS: La lista que contiene los tejidos a los que se les va a realizar la grafica
## RETURN: La rejilla compuesta por las graficas
makePlotsGrid <- function(list_grid) {
  
  my_path <- getwd()
  path_file_images <- paste(my_path, "/data/Imagenes/Preprocesamiento/Graficas", sep = "")
  setwd(path_file_images)
  
  if (length(list_grid) > 0) {
    
    first_name <- list.first(names(list_grid))
    last_name <- list.last(names(list_grid))
    file_name <- paste("Grid_", first_name, "_hasta_" ,last_name, sep = "")
    
    windows(height = 540, width = 960)
    par(mfrow = c(3,3))
    for (plot in list_grid){
      print(plot, layout = FALSE)
    }
    
    savePlot(filename = file_name, type = "png", device = dev.cur())
    
    dev.off()
    
  } else {
    
    message("La lista no contiene ningun elemento")
    
  }
  
  setwd(my_path)
  
  gc()
  
  if (exists("file_name")) {
    
    return(file_name)
    
  }
  
}


## Metodo para las graficas de espectro media

mediaSpectra <- function(preprocess_list, names) {
  
  grids_list <- savePlotsGrids(preprocessed = preprocess_list, names = names)
  
  graphic_names <- list()
  contador <- 0
  
  for (grid in grids_list) {
    
    contador %+=% 1
    name <- paste("grafica", contador, sep = "")
    
    graphic_names[name] <- makePlotsGrid(grid)
    
  }
  
  return(graphic_names)
  
}


## Funcion que obtiene las 3 intensidades medias mas altas del tejido
## PARAMS: La lista que contiene los tejidos a los que se les va a realizar la grafica
## RETURN: La rejilla compuesta por las graficas

make3MaxAverageIntensities <- function(tejidoPreprocessed) {
  
  path_main <- getwd()
  tejido_name <- tejidoPreprocessed@metadata$name
  
  path_images <- paste("/data/Imagenes/Preprocesamiento/Images/", tejido_name, sep = "")
  my_path_images <- paste(path_main, path_images, sep = "")
  
  summarized_tejido <- summarizeFeatures(tejidoPreprocessed, FUN = "mean", as = "DataFrame")
  
  masas_vector<- summarized_tejido@mz
  
  intensidades_vector <- summarized_tejido@listData$mean
  
  dataframe_summarized <- data.frame(masas_vector, intensidades_vector)
  
  dataframe_summarized <- dataframe_summarized[order(-intensidades_vector),]
  
  dataframe_summarized <- dataframe_summarized[1:3,]
  
  intensidad_max_1 <- dataframe_summarized[1,]$masas_vector
  intensidad_max_2 <- dataframe_summarized[2,]$masas_vector
  intensidad_max_3 <- dataframe_summarized[3,]$masas_vector
  
  intensidades_total <- list("intensidad1" = intensidad_max_1, "intensidad2" = intensidad_max_2, "intensidad3" = intensidad_max_3)
  
  if (file.exists(my_path_images)) {

    message(paste("El fichero para el tejido ", tejido_name, " ya existe", sep = ""))

  } else {

    dir.create(file.path(path_main, path_images), recursive = TRUE)

  }
  
  data <- list("intensidades_total" = intensidades_total, "path_images" = my_path_images)
  
  message(paste("Tejido ", tejido_name, " completado", sep = ""))
  
  return(data)
  
  gc()
}


## Funcion para generar la imagen segun la intensidad maxima elegida y el fichero
## PARAMS: los datos obtenidos del fichero seleccionado y el propio fichero preprocesado
## RETURN: La imagen generada

generateImageInt <- function(data, tejidoPrep) {
  
  intensity <- data$intensidades_total$intensidad1
  pathTemp <- data$path_images
  setwd(pathTemp)
  windows()
  image(tejidoPrep, mz = intensity, plusminus = data$plusminus)
  savePlot(filename = paste("Intensidad_max_",intensity,"_tejido_", tejidoPrep@metadata$name, sep = ""), type = "png", device = dev.cur())
  # dev.off(which = dev.cur())
  
}

## Funcion le hace el binning a toda la lista de preprocesados
## PARAMS: La lista que contiene los tejidos preprocesados
## RETURN: Los tejidos binneados
binning <- function(preprocessed, binning_number, units) {
  
  # message("Introduzca el tipo de binning que sea desea aplicar en el proceso")
  # binning_number <- as.numeric(readline(prompt = "Binning: "))
  list_binned <- list()
  contador <- 1
  
  for (tejido in preprocessed) {
    name_tejido <- tejido@metadata$name
    list_binned[[name_tejido]] <- mzBin(tejido, from = 700, to = 2000, resolution = binning_number, units = units)
    
  }
  
  for (tejido in list_binned) {
    name_tejido <- tejido@metadata$name
    
    list_binned[[name_tejido]] <- process(tejido)
    
    message(paste("Tejido numero ",contador, " de ", length(preprocessed), sep = ""))
    contador %+=% 1
  }
  
  gc()
  return(list_binned)
}


## Funcion que realiza el peakpicking al objeto compuesto(combine())
## PARAMS: El objeto compuesto
## RETURN: El nuevo objeto procesado
# peakPickingWithcombine <- function(object_combine, SNR, Methodname) {
#   
#   all_tejidos_temp <- list()
#   all_tejidos_pick <- list()
#   
#   all_tejidos_temp <- peakPick(object_combine, method = Methodname, SNR = SNR)
#   
#   all_tejidos_pick <- process(all_tejidos_temp)
#   
#   gc()
#   return(all_tejidos_pick)
# }

## Funcion que realiza el peakpicking a todos los tejidos por separado
## PARAMS: El tejido preprocesado
## RETURN: El nuevo tejido procesado
peakPickingWithoutcombine <- function(tejido, Methodname, SNR) {
  
  
  tejido_temp <- peakPick(tejido, method = Methodname, SNR = SNR)
  
  tejido_pick <- process(tejido_temp)
  
  gc()
  return(tejido_pick)
}


## Funcion mayor que procesa las entradas de teclado para el SNR y el metodo y llama a la funcion mas pequeña peakPickingWithoutcombine
## PARAMS: La lista de tejidos del binning
## RETURN: La lista de tejidos con el peakpicking hecho
bigPeakPicking <- function(binned, SNR_Number, my_peak_method) {
  
  # message("Introduzca el tipo de SNR que sea desea aplicar en el proceso")
  # SNR_Number <- as.numeric(readline(prompt = "SNR: "))
  
  
  # while (flag == FALSE) {
  #   message("Introduzca el tipo de m�todo que sea desea aplicar en el proceso")
  #   message("Los m�todos disponibles son: 'mad', 'simple', 'adaptative', 'limpic'.")
  #   my_method_name <- as.character(readline(prompt = "M�todo: "))
  #   
  #   if (my_method_name == "mad" || my_method_name == "simple" || my_method_name == "adaptative" || my_method_name == "limpic") {
  #     
  #     flag <- TRUE
  #     
  #   }
  # }
  rm(all_tejidos_pick_no_combine_prev)
  all_tejidos_pick_no_combine_prev <- list()
  SNR_Number <- SNR_Number
  my_peak_method <- my_peak_method
  
  for (tejido in binned) {
    
    name <- tejido@metadata$name
    message(name)
    all_tejidos_pick_no_combine_prev[[name]] <- peakPickingWithoutcombine(tejido = tejido, Methodname = my_peak_method, SNR = SNR_Number)
    
  }
  
  rm(tejido)
  
  return(all_tejidos_pick_no_combine_prev)
}



## Funcion
## PARAMS: El tejido preprocesado
## RETURN: El nuevo tejido procesado
kmeanWithCombine <- function(object_pick) {
  
  #Pedir R, K y algoritmo
  
  #opciones algorithm "Hartigan-Wong", "Lloyd", "Forgy","MacQueen"
  object_kmeans<- spatialKMeans(object_pick, r = 1, k = 15, method = 'gaussian', weights = 1, iter.max = 10, nstart = 50,
                                algorithm = "Lloyd", ncomp = 100)
  
  # image(object_kmeans, model = 1, key = FALSE,
  #       main = "Titulo Arriba", strip = FALSE,
  #       layout = c(3,5), col = mycol)
  
  gc()
  return(object_kmeans)
}


## Funcion para hacerle el kmeans a todos los tejidos por separado
## PARAMS: La lista de tejidos con el peakpicking
## RETURN: La lista de los tejidos con el Kmeans
my_kmeans <- function(object, k_Number, r_Number, algorithm_Name) {
  
  
  object_kmeans <- spatialKMeans(object, r = r_Number,
                                 k = k_Number,
                                 algorithm = algorithm_Name,
                                 method = 'gaussian',
                                 weights = 1,
                                 iter.max = 10, nstart = 50,
                                 ncomp = 100)
  
  gc()
  
  return(object_kmeans)
}



## Funcion
## PARAMS: El tejido preprocesado
## RETURN: El nuevo tejido procesado
summaryPlotsKmeans <- function(dataset, results, model, segment, name, col) {
  ## Imagen de distribuci�n del cluster
  image(results, model = model, key = FALSE, strip = FALSE, column = segment, main = name, layout = c(3,5), col = col)
  
  ## Espectro Media
  plot(results, model = model, key = FALSE, column = segment, main = "Cluster mean spectrum", col = col)
  
  ## Masas mas representativas de ese Cluster
  top <- topLabels(results, n = 1, model = model, filter = list(cluster = segment))
  ## Imagen de las masas mas representativas calculadas anteriormente en el vector top
  image(dataset, mz=top$mz, plusminus=0.1, key=FALSE, strip=TRUE,
        normalize.image="linear", contrast.enhance="histogram",
        smooth.image="adaptive", col.regions=mapavir)
  
  gc()
  return(top)
}

## Funcion para hacer una diseccion digital de los clusters elegidos deaspues de haber realizado el Kmeans
## PARAMS: El objeto kmeans, el fichero inicial del tejido, el radio, el numero de clusters y la lista de los cluster seleccionados
## RETURN: El nuevo fichero con solo los clusters seleccionados
my_dissection <- function(object, my_file, r_number, k_number, cluster_list) {
  
  
  #Aislamiento segmento. Introducir numero de segmento en el pData del set y aislarlos
  pData(my_file)$segment <- object@resultData@listData[[1]][["cluster"]] 
  
  
  object_clustersel <- my_file[,pData(my_file)$segment %in% cluster_list]
  
  return(object_clustersel)
  
}


##Funcion que se utiliza para sumar +1
`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))

withConsoleRedirect <- function(containerId, expr) {
  # Change type="output" to type="message" to catch stderr
  # (messages, warnings, and errors) instead of stdout.
  txt <- capture.output(results <- expr, type = "output")
  if (length(txt) > 0) {
    insertUI(paste0("#", containerId), where = "beforeEnd",
             ui = paste0(txt, "\n", collapse = "")
    )
  }
  results
}




############################################# Codigo Antiguo #############################################

########### Pruebas de preprocesamiento para un bucle en la lista con las 3 operaciones a la vez ###########

## Se crea un solo bucle con el que por cada imagen se har� todas las operaciones necesarias para el preprocesamiento
## y se almacenan en una lista final
# t <- proc.time()
#
# for (imagen in my_list) {
#   contador %+=% 1
#   name <- names(my_list[contador])
#   name_pre <- paste(name, "_pre", sep = "")
#
#   name_pre <- normalize(imagen, method = 'rms')
#   name_pre <- reduceBaseline(name_pre, method='median')
#   name_pre <- process(name_pre, BPPARAM = bpparam)
#   list_preprocessed <- name_pre
# }
#
# proc.time()-t
########### ########### ########### ########### ###########


#Kmeans
# estudio6y8dpi_replicaunica_3tent_kmeans<- spatialKMeans(estudio6y8dpi_replicaunica_3tent_peaks,
#                                                         r = 1, k = 15,
#                                                         method = 'gaussian',
#                                                         weights = 1, iter.max = 10, nstart = 50,
#                                                         algorithm = "Lloyd", #opciones "Hartigan-Wong", "Lloyd", "Forgy","MacQueen"
#                                                         ncomp = 100)
#
# image(estudio6y8dpi_replicaunica_3tent_kmeans, model=1, key=FALSE,
#       main="K=15 iter10ncomp100nstart50", strip=FALSE,
#       layout=c(3,5), col=mycol)
#
# summaryPlots_kmeans <- function(dataset, results, model, segment, name, col) {
#   ## Imagen de distribuci�n del cluster
#   image(results, model=model, key=FALSE, strip = FALSE, column=segment, main=name, layout=c(3,5), col = col)
#
#   ## Espectro Media
#   plot(results, model = model, key = FALSE, column = segment, main = "Cluster mean spectrum", col = col)
#
#   ## Masas mas representativas de ese Cluster
#   top <- topLabels(results, n = 1, model = model, filter = list(cluster = segment))
#   ## Imagen de las masas mas representativas calculadas anteriormente en el vector top
#   image(dataset, mz=top$mz, plusminus=0.1, key=FALSE, strip=TRUE,
#         normalize.image="linear", contrast.enhance="histogram",
#         smooth.image="adaptive", col.regions=mapavir)
# }


########### Imagenes y plot del original ###########
# image(pig206, mz = 944.54, plusminus = 0.1, main = 'pig206')
# plot(pig206, col = "green", main = "data centroided", layout = c(1,1))


# makeIntensitiImages <- function(list_names, preprocessed_list) {
#
#   message("Introduzca el nombre del tejido con el que desea crear las imagenes")
#   nombre_tejido <- as.character(readline(prompt="Tejido: "))
#
#   if (nombre_tejido %in% list_names) {
#
#     tejidoForImages <- preprocessed_list[[nombre_tejido]]
#
#     message("Clase prep antes de enviar")
#     message(class(tejidoForImages))
#
#     data <- makeImagesFrom3MaxAverageIntensities(tejidoPrep = tejidoForImages, tejido_name = nombre_tejido)
#
#     message("AQUI aun no llega")
#
#     setwd(data$path_images)
#     windows()
#     image(tejidoForImages, mz = data$intensidades_total$intensidad1, plusminus = data$plusminus)
#     savePlot(filename = paste("Intensidad_max_1_tejido_", tejidoForImages@metadata$name, sep = ""), type = "png", device = dev.cur())
#     pause(3)
#     dev.off(which = dev.cur())
#
#     windows()
#     image(tejidoForImages, mz = data$intensidades_total$intensidad2, plusminus = data$plusminus)
#     savePlot(filename = paste("Intensidad_max_2_tejido_", tejidoForImages@metadata$name, sep = ""), type = "png", device = dev.cur())
#     pause(3)
#     dev.off(which = dev.cur())
#
#
#     windows()
#     image(tejidoForImages, mz = data$intensidades_total$intensidad3, plusminus = data$plusminus)
#     savePlot(filename = paste("Intensidad_max_3_tejido_", tejidoForImages@metadata$name, sep = ""), type = "png", device = dev.cur())
#     pause(3)
#     dev.off(which = dev.cur())
#
#
#     setwd(path_main)
#
#
#   } else {
#     message("Por favor introduzca bien el nombre del Tejido, los nombres son los siguientes: ")
#     message(paste(list_names, sep="' '", collapse=", "))
#   }
#
# }





# rm(list_temp)
# list_kmeans <- list()
# list_temp <- list_peak
# 
# length1 <- length(list_temp)
# 
# if (length(list_temp) == 1) {
#   
#   tejido <- list_peak[[1]]
#   
#   list_kmeans <- spatialKMeans(tejido, r = r_Number, k = k_Number, method = 'gaussian', weights = 1, iter.max = 10, nstart = 50,
#                                algorithm = algorithm_Name, ncomp = 100)
# } else {
#   
#   for (tejido in list_peak) {
#     
#     name_tejido <- tejido@metadata$name
#     
#     list_kmeans[[name_tejido]] <- spatialKMeans(tejido, r = r_Number, k = k_Number, method = 'gaussian', weights = 1, iter.max = 10, nstart = 50,
#                                                 algorithm = algorithm_Name, ncomp = 100)
#   }
#   
# }