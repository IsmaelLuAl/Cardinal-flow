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

options(Cardinal.verbose=TRUE)
options(Cardinal.progress=TRUE)
RNGkind("Mersenne-Twister")

my_colors <- c("#003f5c","#2f4b7c","#665191", "#a05195" ,"#d45087", "#f95d6a", "#ff7c43" ,"#ffa600", "#FF2256")
methodsNormalize <- c("tic", "rms", "reference")
methodsReduceBaseline <- c("locmin", "median")
methodsSmooth <- c("gaussian", "sgolay", "ma")
myWD <- getwd()
dataWD <- paste(myWD, "/data", sep = "")

######################### Funciones ######################### 


## Funcion que realiza el peakpicking al objeto compuesto(combine())
## PARAMS: El objeto compuesto
## RETURN: El nuevo objeto procesado
peakPickingWithcombine <- function(object_combine, SNR, Methodname) {
  
  all_tejidos_temp <- list()
  all_tejidos_pick <- list()
  
  all_tejidos_temp <- peakPick(object_combine, method = Methodname, SNR = SNR)
  
  all_tejidos_pick <- process(all_tejidos_temp)
  
  gc()
  return(all_tejidos_pick)
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


## Funcion 
## PARAMS: El tejido preprocesado
## RETURN: El nuevo tejido procesado
summaryPlotsKmeans <- function(dataset, results, model, segment, name, col) {
  ## Imagen de distribuci?n del cluster 
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


##Funcion que se utiliza para sumar +1
`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))

## Funcion mayor que procesa las entradas de teclado para el SNR y el metodo y llama a la Funcion mas peque?a peakPickingWithoutcombine
## PARAMS: La lista de tejidos del binning
## RETURN: La lista de tejidos con el peakpicking hecho
bigPeakPicking <- function(binned) {
  
  flag <- FALSE
  
  message("Introduzca el tipo de SNR que sea desea aplicar en el proceso")
  SNR_Number <- as.numeric(readline(prompt = "SNR: "))
  
  
  while (flag == FALSE) {
    message("Introduzca el tipo de metodo que sea desea aplicar en el proceso")
    message("Los metodos disponibles son: 'mad', 'simple', 'adaptative', 'limpic'.")
    my_method_name <- as.character(readline(prompt = "metodo: "))
    
    if (my_method_name == "mad" || my_method_name == "simple" || my_method_name == "adaptative" || my_method_name == "limpic") {
      
      flag <- TRUE
      
    }
  }
  
  for (tejido in binned) {
    
    name <- tejido@metadata$name
    message(name)
    all_tejidos_pick_no_combine[[name]] <- peakPickingWithoutcombine(tejido = tejido, Methodname = my_method_name, SNR = SNR_Number)
    
  }
  
  rm(tejido)
  
  return(all_tejidos_pick_no_combine)
}

## Funcion que realiza el peakpicking a todos los tejidos por separado
## PARAMS: El tejido preprocesado
## RETURN: El nuevo tejido procesado
peakPickingWithoutcombine <- function(tejido, Methodname, SNR) {
  
  
  tejido_temp <- peakPick(tejido, method = Methodname, SNR = SNR)
  
  tejido_pick <- process(tejido_temp)
  
  gc()
  return(tejido_pick)
}


## Funcion para hacerle el kmeans a todos los tejidos pro separado
## PARAMS: La lista de tejidos con el peakpicking
## RETURN: La lista de los tejidos con el Kmeans
kmeanWithOutCombine <- function(list_peak, k_Number, r_Number, algorithm_Name) {
  
  list_kmeans <- list()
  list_temp <- list_peak
  
  flag <- FALSE
  
  message("Introduzca el numero de R que sea desea aplicar en el proceso (1 o 2)")
  r_Number <- as.numeric(readline(prompt = "R: "))
  
  message("Introduzca el numero de K que sea desea aplicar en el proceso (10 - 15)")
  k_Number <- as.numeric(readline(prompt = "K: "))
  
  
  while (flag == FALSE) {
    message("Introduzca el tipo de algoritmo que sea desea aplicar en el proceso")
    message("Los algoritmos disponibles son: 'Hartigan-Wong', 'Lloyd', 'Forgy', 'MacQueen'.")
    algorithm_Name <- as.character(readline(prompt = "metodo: "))
    
    if (algorithm_Name == "Hartigan-Wong" || algorithm_Name == "Lloyd" || algorithm_Name == "Forgy" || algorithm_Name == "MacQueen") {
      
      flag <- TRUE
      
    }
  }
  
  if (length(list_temp) == 1) {
    
    tejido <- list_peak
    
    list_kmeans <- spatialKMeans(tejido, r = r_Number, k = k_Number, method = 'gaussian', weights = 1, iter.max = 10, nstart = 50,
                                 algorithm = algorithm_Name, ncomp = 100)
  } else {
    
    for (tejido in list_peak) {
      
      name_tejido <- tejido@metadata$name
      
      list_kmeans[[name_tejido]] <- spatialKMeans(tejido, r = r_Number, k = k_Number, method = 'gaussian', weights = 1, iter.max = 10, nstart = 50,
                                                  algorithm = algorithm_Name, ncomp = 100)
    }
    
  }
  
  gc()
  return(list_kmeans)
}

snowWorkers(type = c("SOCK", "MPI", "FORK"))

###########################################################

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
########### ########### Preprocesamiento ########### ###########

# list_preprocessed <- preProcess(pPalList = my_list)

list_normalized <- list()
list_reduced <- list()
list_processed <- list()

for (imagen in my_list) {
  name <- imagen@metadata$name
  list_normalized[[name]] <- normalize(imagen, method = 'rms')
}

for (imagen in list_normalized) {
  name <- imagen@metadata$name
  list_reduced[[name]] <- reduceBaseline(imagen, method='median')
}

for (imagen in list_reduced) {
  name <- imagen@metadata$name
  list_processed[[name]] <- process(imagen)
  
}

rm(list_normalized, list_reduced)

########### ########### Binning ########### ###########

message("Introduzca el tipo de binning que sea desea aplicar en el proceso")
my_number_binning <- as.numeric(readline(prompt = "Binning: "))
list_binned <- list()
contador <- 1

for (tejido in list_processed) {
  name_tejido <- tejido@metadata$name
  list_binned[[name_tejido]] <- mzBin(tejido, from = 700, to = 2000, resolution = my_number_binning, units="mz")
  
}

for (tejido in list_binned) {
  name_tejido <- tejido@metadata$name
  
  list_binned[[name_tejido]] <- process(tejido)
  
  message(paste("Tejido n? ",contador, " de ", length(list_processed), sep = ""))
  contador %+=% 1
}

for (tejido in list_binned) {
  
  message(tejido@metadata$name)
  list_binned[[tejido@metadata$name]]@centroided <- TRUE
  message(list_binned[[tejido@metadata$name]]@centroided)
  
}

gc()

########### ########### Peakpicking ########### ###########

### Peakpicking without combine ###

all_tejidos_pick_no_combine <- list()

all_tejidos_pick_no_combine <- bigPeakPicking(binned = list_binned)

########### ########### Kmeans ########### ###########

list_Kmeans <- kmeanWithOutCombine(list_peak = all_tejidos_pick_no_combine)

# objeto_kmeans <- spatialKMeans(all_tejidos_pick_no_combine[[1]], r = 1, k = 4, method = 'gaussian', weights = 1, iter.max = 10, nstart = 50,
#                                algorithm = "Lloyd", ncomp = 100)


########### ########### Digital Dissection ########### ###########

#Aislamiento segmento. Introducir numero de segmento en el pData del set y aislarlos
pData(my_list$tejidoA2cristalA2)$segment <- objeto_kmeans@resultData$`r = 1, k = 4`$cluster[]


object_clustersel <- my_list$tejidoA2cristalA2[,pData(my_list$tejidoA2cristalA2)$segment %in% c(2,3)]
