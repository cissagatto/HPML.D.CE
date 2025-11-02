##############################################################################
# Label Clusters Chains for Multi-Label Classification                       #
# Copyright (C) 2025                                                         #
#                                                                            #
# This code is free software: you can redistribute it and/or modify it under #
# the terms of the GNU General Public License as published by the Free       #
# Software Foundation, either version 3 of the License, or (at your option)  #
# any later version. This code is distributed in the hope that it will be    #
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of     #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General   #
# Public License for more details.                                           #
#                                                                            #
# 1 - Prof Elaine Cecilia Gatto                                              #
# 2 - Prof PhD Ricardo Cerri                                                 #
# 3 - Prof PhD Mauri Ferrandin                                               #
# 4 - Prof PhD Celine Vens                                                   #
# 5 - PhD Felipe Nakano Kenji                                                #
# 6 - Prof PhD Jesse Read                                                    #
#                                                                            #
# 1 = Federal University of São Carlos - UFSCar - https://www2.ufscar.br     #
# Campus São Carlos | Computer Department - DC - https://site.dc.ufscar.br | #
# Post Graduate Program in Computer Science - PPGCC                          # 
# http://ppgcc.dc.ufscar.br | Bioinformatics and Machine Learning Group      #
# BIOMAL - http://www.biomal.ufscar.br                                       # 
#                                                                            # 
# 1 = Federal University of Lavras - UFLA                                    #
#                                                                            # 
# 2 = State University of São Paulo - USP                                    #
#                                                                            # 
# 3 - Federal University of Santa Catarina Campus Blumenau - UFSC            #
# https://ufsc.br/                                                           #
#                                                                            #
# 4 and 5 - Katholieke Universiteit Leuven Campus Kulak Kortrijk Belgium     #
# Medicine Department - https://kulak.kuleuven.be/                           #
# https://kulak.kuleuven.be/nl/over_kulak/faculteiten/geneeskunde            #
#                                                                            #
# 6 - Ecole Polytechnique | Institut Polytechnique de Paris | 1 rue Honoré   #
# d’Estienne d’Orves - 91120 - Palaiseau - FRANCE                            #
#                                                                            #
##############################################################################



# cat("\n################################")
# cat("\n# Set Work Space               #")
# cat("\n###############################\n\n")
# library(here)
# library(stringr)
# FolderRoot <- here::here()
# setwd(FolderRoot)



#########################################################################
# FUNCTION DIRECTORIES                                   
#   Objective:                                           
#      Creates all the necessary folders for the project.
#   Parameters:                                          
#      dataset_name: name of the dataset                 
#      folderResults: path to save process the algorithm. 
#               Example: "/dev/shm/birds", "/scratch/birds", 
#            "/home/usuario/birds", "/C:/Users/usuario/birds"
#   Return:                                                              
#      All path directories                                              
#########################################################################
directories <- function(parameters){
  
  library(here)
  library(stringr)
  FolderRoot <- here::here()
  
  retorno = list()
  
  folderResults = parameters$Config$Folder.Results
  
  #############################################################################
  # RESULTS FOLDER:                                                           #
  # Parameter from command line. This folder will be delete at the end of the #
  # execution. Other folder is used to store definitely the results.          #
  # Example: "/dev/shm/res"                                                   #
  #############################################################################
  if(dir.exists(folderResults) == TRUE){
    setwd(folderResults)
    dir_folderResults = dir(folderResults)
    n_folderResults = length(dir_folderResults)
  } else {
    dir.create(folderResults)
    setwd(folderResults)
    dir_folderResults = dir(folderResults)
    n_folderResults = length(dir_folderResults)
  }
  
  
  #############################################################################
  # UTILS FOLDER:                                                             #
  # Get information about the files within folder utils that already exists   #
  # in the project. It's needed to run CLUS framework and convert CSV files   #
  # in ARFF files correctly.                                                  #
  # "/home/[user]/Partitions-Kohonen/utils"                                   #
  #############################################################################
  folderUtils = paste(FolderRoot, "/Utils", sep="")
  if(dir.exists(folderUtils) == TRUE){
    setwd(folderUtils)
    dir_folderUtils = dir(folderUtils)
    n_folderUtils = length(dir_folderUtils)
  } else {
    dir.create(folderUtils)
    setwd(folderUtils)
    dir_folderUtils = dir(folderUtils)
    n_folderUtils = length(dir_folderUtils)
  }
  
  
  #############################################################################
  #
  #############################################################################
  folderScript = paste(FolderRoot, "/R", sep="")
  if(dir.exists(folderScript) == TRUE){
    setwd(folderScript)
    dir_folderScript = dir(folderScript)
    n_folderScript = length(dir_folderScript)
  } else {
    dir.create(folderScript)
    setwd(folderScript)
    dir_folderScript = dir(folderScript)
    n_folderScript = length(dir_folderScript)
  }
  
  
  #############################################################################
  #
  #############################################################################
  folderReports = paste(FolderRoot, "/Reports", sep="")
  if(dir.exists(folderReports) == TRUE){
    setwd(folderReports)
    dir_folderReports = dir(folderReports)
    n_folderReports = length(dir_folderReports)
  } else {
    dir.create(folderReports)
    setwd(folderReports)
    dir_folderReports = dir(folderReports)
    n_folderReports = length(dir_folderReports)
  }
  
  
  
  #############################################################################
  #
  #############################################################################
  folderTested = paste(folderResults, "/Tested", sep="")
  if(dir.exists(folderTested) == TRUE){
    setwd(folderTested)
    dir_folderTested = dir(folderTested)
    n_folderTested = length(dir_folderTested)
  } else {
    dir.create(folderTested)
    setwd(folderTested)
    dir_folderTested = dir(folderTested)
    n_folderTested = length(dir_folderTested)
  }
  
  
  #############################################################################
  #
  #############################################################################
  folderPython = paste(FolderRoot, "/Python", sep="")
  if(dir.exists(folderPython ) == TRUE){
    setwd(folderPython )
    dir_folderPython  = dir(folderPython )
    n_folderPython  = length(dir_folderPython )
  } else {
    dir.create(folderPython)
    setwd(folderPython )
    dir_folderPython  = dir(folderPython )
    n_folderPython  = length(dir_folderPython )
  }
  
  
  #############################################################################
  # DATASETS FOLDER:                                                          #
  # Get the information within DATASETS folder that already exists in the     #
  # project. This folder store the files from cross-validation and will be    #
  # use to get the label space to modeling the label correlations and         #
  # compute silhouete to choose the best hybrid partition.                    #
  # "/home/[user]/Partitions-Kohonen/datasets"                                #
  #############################################################################
  folderDatasets = paste(folderResults, "/Datasets", sep="")
  if(dir.exists(folderDatasets) == TRUE){
    setwd(folderDatasets)
    dir_folderDatasets = dir(folderDatasets)
    n_folderDatasets = length(dir_folderDatasets)
  } else {
    dir.create(folderDatasets)
    setwd(folderDatasets)
    dir_folderDatasets = dir(folderDatasets)
    n_folderDatasets = length(dir_folderDatasets)
  }
  
  
  #############################################################################
  # SPECIFIC DATASET FOLDER:                                                  #
  # Path to the specific dataset that is runing. Example: with you are        # 
  # running this code for EMOTIONS dataset, then this get the path from it    #
  # "/home/[user]/Partitions-Kohonen/datasets/birds"                          #
  #############################################################################
  folderSpecificDataset = paste(folderDatasets, "/", dataset_name, sep="")
  if(dir.exists(folderSpecificDataset) == TRUE){
    setwd(folderSpecificDataset)
    dir_folderSpecificDataset = dir(folderSpecificDataset)
    n_folderSpecificDataset = length(dir_folderSpecificDataset)
  } else {
    dir.create(folderSpecificDataset)
    setwd(folderSpecificDataset)
    dir_folderSpecificDataset = dir(folderSpecificDataset)
    n_folderSpecificDataset = length(dir_folderSpecificDataset)
  }
  
  
  #############################################################################
  # LABEL SPACE FOLDER:                                                       #
  # Path to the specific label space from the dataset that is runing.         #
  # This folder store the label space for each FOLD from the cross-validation #
  # which was computed in the Cross-Validation Multi-Label code.              #
  # In this way, we don't need to load the entire dataset into the running    #
  # "/home/elaine/Partitions-Kohonen/datasets/birds/LabelSpace"               #
  #############################################################################
  folderLabelSpace = paste(folderSpecificDataset, "/LabelSpace", sep="")
  if(dir.exists(folderLabelSpace) == TRUE){
    setwd(folderLabelSpace)
    dir_folderLabelSpace = dir(folderLabelSpace)
    n_folderLabelSpace = length(dir_folderLabelSpace)
  } else {
    dir.create(folderLabelSpace)
    setwd(folderLabelSpace)
    dir_folderLabelSpace = dir(folderLabelSpace)
    n_folderLabelSpace = length(dir_folderLabelSpace)
  }
  
  
  #############################################################################
  # NAMES LABELS FOLDER:                                                      #
  # Get the names of the labels from this dataset. This will be used in the   #
  # code to create the groups for each partition. Is a way to guarantee the   #
  # use of the correct names labels.                                          #
  # "/home/[user]/Partitions-Kohonen/datasets/birds/NamesLabels"              #
  #############################################################################
  folderNamesLabels = paste(folderSpecificDataset, "/NamesLabels", sep="")
  if(dir.exists(folderNamesLabels) == TRUE){
    setwd(folderNamesLabels)
    dir_folderNamesLabels = dir(folderNamesLabels)
    n_folderNamesLabels = length(dir_folderNamesLabels)
  } else {
    dir.create(folderNamesLabels)
    setwd(folderNamesLabels)
    dir_folderNamesLabels = dir(folderNamesLabels)
    n_folderNamesLabels = length(dir_folderNamesLabels)
  }
  
  
  #############################################################################
  # CROSS VALIDATION FOLDER:                                                  #
  # Path to the folders and files from cross-validation for the specific      # 
  # dataset                                                                   #
  # "/home/[user]/Partitions-Kohonen/datasets/birds/CrossValidation"          #
  #############################################################################
  folderCV = paste(folderSpecificDataset, "/CrossValidation", sep="")
  if(dir.exists(folderCV) == TRUE){
    setwd(folderCV)
    dir_folderCV = dir(folderCV)
    n_folderCV = length(dir_folderCV)
  } else {
    dir.create(folderCV)
    setwd(folderCV)
    dir_folderCV = dir(folderCV)
    n_folderCV = length(dir_folderCV)
  }
  
  
  #############################################################################
  # TRAIN CROSS VALIDATION FOLDER:                                            #
  # Path to the train files from cross-validation for the specific dataset    #                                                                   #
  # "/home/[user]/Partitions-Kohonen/datasets/birds/CrossValidation/Tr"       #
  #############################################################################
  folderCVTR = paste(folderCV, "/Tr", sep="")
  if(dir.exists(folderCVTR) == TRUE){
    setwd(folderCVTR)
    dir_folderCVTR = dir(folderCVTR)
    n_folderCVTR = length(dir_folderCVTR)
  } else {
    dir.create(folderCVTR)
    setwd(folderCVTR)
    dir_folderCVTR = dir(folderCVTR)
    n_folderCVTR = length(dir_folderCVTR)
  }
  
  
  #############################################################################
  # TEST CROSS VALIDATION FOLDER:                                             #
  # Path to the test files from cross-validation for the specific dataset     #                                                                   #
  # "/home/[user]/Partitions-Kohonen/datasets/birds/CrossValidation/Ts"       #
  #############################################################################
  folderCVTS = paste(folderCV, "/Ts", sep="")
  if(dir.exists(folderCVTS) == TRUE){
    setwd(folderCVTS)
    dir_folderCVTS = dir(folderCVTS)
    n_folderCVTS = length(dir_folderCVTS)
  } else {
    dir.create(folderCVTS)
    setwd(folderCVTS)
    dir_folderCVTS = dir(folderCVTS)
    n_folderCVTS = length(dir_folderCVTS)
  }
  
  
  #############################################################################
  # VALIDATION CROSS VALIDATION FOLDER:                                       #
  # Path to the validation files from cross-validation for the specific       #
  # dataset                                                                   #                                                           
  # "/home/[user]/Partitions-Kohonen/datasets/birds/CrossValidation/Vl"       #
  #############################################################################
  folderCVVL = paste(folderCV, "/Vl", sep="")
  if(dir.exists(folderCVVL) == TRUE){
    setwd(folderCVVL)
    dir_folderCVVL = dir(folderCVVL)
    n_folderCVVL = length(dir_folderCVVL)
  } else {
    dir.create(folderCVVL)
    setwd(folderCVVL)
    dir_folderCVVL = dir(folderCVVL)
    n_folderCVVL = length(dir_folderCVVL)
  }
  
  
  #############################################################################
  # RESULTS DATASET FOLDER:                                                   #
  # Path to the results for the specific dataset that is running              #                                                           
  # "/dev/shm/res/birds"                                                      #
  #############################################################################
  folderResultsDataset = paste(folderResults, "/", dataset_name, sep="")
  if(dir.exists(folderResultsDataset) == TRUE){
    setwd(folderResultsDataset)
    dir_folderResultsDataset = dir(folderResultsDataset)
    n_folderResultsDataset = length(dir_folderResultsDataset)
  } else {
    dir.create(folderResultsDataset)
    setwd(folderResultsDataset)
    dir_folderResultsDataset = dir(folderResultsDataset)
    n_folderResultsDataset = length(dir_folderResultsDataset)
  }
  
  
  #############################################################################
  # RESULTS PARTITIONS FOLDER:                                                #
  # Folder to store the results from partitioning the label correlations      #
  # "/dev/shm/res/birds/Partitions"                                           #
  #############################################################################
  folderPartitions = paste(folderResults, "/Partitions", sep="")
  if(dir.exists(folderPartitions) == TRUE){
    setwd(folderPartitions)
    dir_folderPartitions = dir(folderPartitions)
    n_folderPartitions = length(dir_folderPartitions)
  } else {
    dir.create(folderPartitions)
    setwd(folderPartitions)
    dir_folderPartitions = dir(folderPartitions)
    n_folderPartitions = length(dir_folderPartitions)
  }
  
  
  
  #############################################################################
  # RETURN ALL PATHS                                                          #
  #############################################################################
  retorno$folderReports = folderReports
  retorno$folderScript = folderScript
  retorno$folderPython = folderPython
  retorno$folderTested = folderTested
  retorno$folderResults = folderResults
  retorno$folderUtils = folderUtils
  retorno$folderDatasets = folderDatasets
  retorno$folderSpecificDataset = folderSpecificDataset
  retorno$folderLabelSpace = folderLabelSpace
  retorno$folderNamesLabels = folderNamesLabels
  retorno$folderCV = folderCV
  retorno$folderCVTR = folderCVTR
  retorno$folderCVTS = folderCVTS
  retorno$folderCVVL = folderCVVL
  retorno$folderPartitions = folderPartitions
  retorno$folderResultsDataset = folderResultsDataset
  
  
  #############################################################################
  # RETURN ALL DIRS                                                           #
  #############################################################################
  retorno$dir_folderReports = dir_folderReports
  retorno$dir_folderScript = dir_folderScript
  retorno$dir_folderPython = dir_folderPython
  retorno$dir_folderTested = dir_folderTested
  retorno$dir_folderResults = dir_folderResults
  retorno$dir_folderUtils = dir_folderUtils
  retorno$dir_folderDatasets = dir_folderDatasets
  retorno$dir_folderSpecificDataset = dir_folderSpecificDataset
  retorno$dir_folderLabelSpace = dir_folderLabelSpace
  retorno$dir_folderNamesLabels = dir_folderNamesLabels
  retorno$dir_folderCV = dir_folderCV
  retorno$dir_folderCVTR = dir_folderCVTR
  retorno$dir_folderCVTS = dir_folderCVTS
  retorno$dir_folderCVVL = dir_folderCVVL
  retorno$dir_folderPartitions = dir_folderPartitions
  retorno$dir_folderResultsDataset = dir_folderResultsDataset
  
  
  #############################################################################
  # RETURN ALL LENGHTS                                                        #
  #############################################################################
  retorno$n_folderReports = n_folderReports
  retorno$n_folderScript = n_folderScript
  retorno$n_folderPython = n_folderPython
  retorno$n_folderTested = n_folderTested
  retorno$n_folderResults = n_folderResults
  retorno$n_folderUtils = n_folderUtils
  retorno$n_folderDatasets = n_folderDatasets
  retorno$n_folderSpecificDataset = n_folderSpecificDataset
  retorno$n_folderLabelSpace = n_folderLabelSpace
  retorno$n_folderNamesLabels = n_folderNamesLabels
  retorno$n_folderCV = n_folderCV
  retorno$n_folderCVTR = n_folderCVTR
  retorno$n_folderCVTS = n_folderCVTS
  retorno$n_folderCVVL = n_folderCVVL
  retorno$n_folderPartitions = n_folderPartitions
  retorno$n_folderResultsDataset = n_folderResultsDataset
  
  return(retorno)
  gc()
}

#' @title Extract Label Space from Training Folds
#'
#' @description
#' Separates the label space from the rest of the dataset for each training fold.
#' This function reads the cross-validation training files, extracts the label
#' columns (based on the dataset structure), and returns both the label names and
#' the label space for each fold. The extracted label spaces can be used as input
#' for computing label correlations or modeling label dependencies.
#'
#' @param parameters A list containing configuration and directory information,
#' typically returned by the \code{\link{directories}} function. It must include:
#' \itemize{
#'   \item \code{Config$Number.Folds}: Integer. Number of cross-validation folds.
#'   \item \code{Directories$folderCVTR}: Character. Path to the folder containing
#'   the training split CSV files (e.g., \code{"CrossValidation/Tr"}).
#' }
#'
#' @param ds A list containing metadata about the dataset structure, with the following fields:
#' \itemize{
#'   \item \code{LabelStart}: Integer. Index of the first label column in the dataset.
#'   \item \code{LabelEnd}: Integer. Index of the last label column in the dataset.
#' }
#'
#' @param dataset_name Character. Name of the dataset being processed.
#' This name is used to identify and read the corresponding training split files.
#'
#' @details
#' For each fold (from 1 to \code{Number.Folds}), the function reads the file
#' named according to the pattern:
#' \preformatted{
#' [folderCVTR]/[dataset_name]-Split-Tr-[k].csv
#' }
#' where \code{k} represents the fold number.
#'
#' The function then extracts the label columns based on
#' \code{ds$LabelStart} and \code{ds$LabelEnd}, stores them in a list, and
#' retrieves their column names as label identifiers.
#'
#' @return
#' A list with the following elements:
#' \itemize{
#'   \item \code{NamesLabels}: Character vector containing the names of the labels.
#'   \item \code{Classes}: List of data frames, where each element corresponds
#'   to the label space of one fold.
#' }
#'
#' @examples
#' \dontrun{
#' # Example of parameter structure
#' parameters <- list(
#'   Config = list(Number.Folds = 10),
#'   Directories = list(folderCVTR = "/home/user/project/datasets/birds/CrossValidation/Tr")
#' )
#'
#' ds <- list(LabelStart = 5, LabelEnd = 12)
#' dataset_name <- "birds"
#'
#' label_data <- labelSpace(parameters)
#' print(label_data$NamesLabels)
#' }
#'
#' @export
labelSpace <- function(parameters){
  
  retorno = list()
  
  # return all fold label space
  classes = list()
  
  # from the first FOLD to the last
  k = 1
  while(k<=parameters$Config$Number.Folds){
    
    # get the correct fold cross-validation
    nome_arquivo = paste(parameters$Folders$folderCVTR,
                         "/", dataset_name, 
                         "-Split-Tr-", k, ".csv", sep="")
    
    # open the file
    arquivo = data.frame(read.csv(nome_arquivo))
    
    # split label space from input space
    classes[[k]] = arquivo[,ds$LabelStart:ds$LabelEnd]
    
    # get the names labels
    namesLabels = c(colnames(classes[[k]]))
    
    # increment FOLD
    k = k + 1 
    
    # garbage collection
    gc() 
    
  } # End While of the 10-folds
  
  # return results
  retorno$NamesLabels = namesLabels
  retorno$Classes = classes
  return(retorno)
  
  gc()
  cat("\n##########################################################")
  cat("\n# FUNCTION LABEL SPACE: END                              #") 
  cat("\n##########################################################")
  cat("\n\n\n\n")
}






#' @title Retrieve Dataset Metadata
#'
#' @description
#' Extracts all relevant information about a specific dataset from the
#' \code{"datasets-hpmlk.csv"} file.  
#' The function organizes and returns key dataset attributes — such as the
#' number of instances, input features, labels, and label-space properties — in
#' a structured list format.
#'
#' @param dataset A list or data frame row representing a specific dataset,
#' typically read from the file \code{"datasets-hpmlk.csv"}.  
#' It must contain the following named fields:
#' \itemize{
#'   \item \code{ID} — dataset identifier.
#'   \item \code{Name} — dataset name.
#'   \item \code{Instances} — number of instances (samples).
#'   \item \code{Inputs} — number of input (feature) attributes.
#'   \item \code{Labels} — number of label attributes.
#'   \item \code{LabelsSets} — number of unique label combinations.
#'   \item \code{Single} — number of single-label instances.
#'   \item \code{MaxFreq} — frequency of the most common label combination.
#'   \item \code{Card} — label cardinality.
#'   \item \code{Dens} — label density.
#'   \item \code{Mean} — average number of labels per instance.
#'   \item \code{Scumble} — imbalance metric.
#'   \item \code{TCS} — theoretical complexity score.
#'   \item \code{AttStart}, \code{AttEnd} — index range of feature columns.
#'   \item \code{LabelStart}, \code{LabelEnd} — index range of label columns.
#'   \item \code{Distinct} — number of distinct instances.
#'   \item \code{xn}, \code{yn}, \code{gridn} — grid-related parameters
#'   (for visualization or map-based partitioning).
#' }
#'
#' @details
#' This function is typically used after reading dataset metadata from a CSV file
#' (e.g., \code{datasets-hpmlk.csv}) into a data frame. Each row of this CSV
#' represents a dataset and its structural and statistical characteristics.
#' Passing a single row (as a list) to this function extracts and organizes those
#' characteristics for further processing or modeling steps.
#'
#' @return
#' A named list containing dataset metadata, including:
#' \code{
#' id, name, instances, inputs, labels, LabelsSets, single, maxfreq,
#' card, dens, mean, scumble, tcs, attStart, attEnd, labStart, labEnd,
#' distinct, xn, yn, gridn
#' }
#'
#' @examples
#' \dontrun{
#' # Example of using infoDataSet()
#' datasets_info <- read.csv("datasets-hpmlk.csv")
#' birds_info <- datasets_info[datasets_info$Name == "birds", ]
#'
#' ds <- infoDataSet(birds_info)
#' print(ds$name)
#' print(ds$labels)
#' }
#'
#' @export
infoDataSet <- function(dataset){
  
  retorno = list()
  
  retorno$id = dataset$ID
  retorno$name = dataset$Name
  retorno$instances = dataset$Instances
  retorno$inputs = dataset$Inputs
  retorno$labels = dataset$Labels
  retorno$LabelsSets = dataset$LabelsSets
  retorno$single = dataset$Single
  retorno$maxfreq = dataset$MaxFreq
  retorno$card = dataset$Card
  retorno$dens = dataset$Dens
  retorno$mean = dataset$Mean
  retorno$scumble = dataset$Scumble
  retorno$tcs = dataset$TCS
  retorno$attStart = dataset$AttStart
  retorno$attEnd = dataset$AttEnd
  retorno$labStart = dataset$LabelStart
  retorno$labEnd = dataset$LabelEnd
  retorno$distinct = dataset$Distinct
  retorno$xn = dataset$xn
  retorno$yn = dataset$yn
  retorno$gridn = dataset$gridn
  
  return(retorno)
  
  gc()
}

###############################################################################
#' Convert CSV files to ARFF format using a Java converter
#'
#' @description
#' This function calls a Java JAR file (`R_csv_2_arff.jar`) to convert a CSV dataset
#' into an ARFF file format (used by Weka and other machine learning tools).
#' It builds the system command dynamically and executes it from within R.
#'
#' @details
#' The function assumes that the Java JAR converter (`R_csv_2_arff.jar`) is located
#' in the folder specified by `FolderUtils`. The user must have Java properly installed
#' and accessible through the system PATH.
#'
#' @param arg1 Character. The path to the input CSV file to be converted.
#' @param arg2 Character. The path or name of the output ARFF file to be generated.
#' @param arg3 Character. Additional parameters to be passed to the Java converter.
#' @param FolderUtils Character. The directory containing the `R_csv_2_arff.jar` file.
#'
#' @return
#' The function prints the result of the system command execution to the console.
#' It does not return any R object (invisible return of `NULL`).
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' FolderUtils <- "/home/user/utils"
#' input_csv <- "/home/user/data/sample.csv"
#' output_arff <- "/home/user/data/sample.arff"
#'
#' converteArff(input_csv, output_arff, "", FolderUtils)
#' }
#'
#' @author Elaine Cecília Gatto - Cissa
#'
#' @seealso
#' [system()] for executing system commands in R,
#' [paste()] for string concatenation.
#'
#' @note
#' Make sure Java is installed and available in your system environment.
#' The JAR file `R_csv_2_arff.jar` must exist in the specified `FolderUtils` directory.
#'
#' @export
converteArff <- function(arg1, arg2, arg3){
  str = paste("java -jar ", parameters$Directories$folderUtils,
              "/R_csv_2_arff.jar ", arg1, " ", arg2, " ", arg3, sep="")
  system(str)
  cat("\n")
}



#' @title Retrieve and Consolidate All Best Partitions
#'
#' @description
#' Loads and consolidates information about the best partitions of each fold
#' from the partitioning results of a dataset.  
#' The function reads the best silhouette configuration for each fold, retrieves
#' corresponding partition and label files, and compiles them into structured
#' data frames for further analysis or reporting.
#'
#' @param parameters A list containing configuration and directory information,
#' typically created by the \code{\link{directories}} function.  
#' It must include the following components:
#' \itemize{
#'   \item \code{Config$Dataset.Name}: Character. Name of the dataset being processed.
#'   \item \code{Config$Number.Folds}: Integer. Number of folds used in cross-validation.
#'   \item \code{Directories$folderPartitions}: Character. Path to the folder
#'   containing partition result files.
#'   \item \code{Directories$folderTested}: Character. Path to the folder
#'   where consolidated results will be saved.
#' }
#'
#' @details
#' The function performs the following steps for each fold (\code{1:Config$Number.Folds}):
#' \enumerate{
#'   \item Reads the best silhouette results from
#'   \code{"[folderPartitions]/[Dataset.Name]/[Dataset.Name]-Best-Silhouete.csv"}.
#'   \item Identifies the best partition number (\code{num.part}) for that fold.
#'   \item Loads the group information from
#'   \code{"fold-[f]-groups-per-partition.csv"} and extracts the matching partition.
#'   \item Reads the partition assignments (\code{"partition-[num.part].csv"})
#'   and corresponding label sets for each group.
#'   \item Aggregates all folds’ results into summary data frames.
#'   \item Exports the consolidated information as CSV files in
#'   \code{folderTested}:
#'     \itemize{
#'       \item \code{best-part-info.csv} — summary of best partitions per fold.
#'       \item \code{all.partitions.info.csv} — all partition data.
#'       \item \code{all.total.labels.csv} — all label-to-group mappings.
#'     }
#' }
#'
#' @return
#' A list containing:
#' \itemize{
#'   \item \code{best.part.info} — Data frame summarizing the best partition
#'   (fold, partition number, and number of groups) for each fold.
#'   \item \code{all.partitions.info} — Combined data frame with detailed partition
#'   structure across all folds.
#'   \item \code{all.total.labels} — Combined data frame with labels per group
#'   for each fold.
#' }
#'
#' @examples
#' \dontrun{
#' parameters <- list(
#'   Config = list(
#'     Dataset.Name = "birds",
#'     Number.Folds = 10
#'   ),
#'   Directories = list(
#'     folderPartitions = "/home/user/project/results/birds/Partitions",
#'     folderTested = "/home/user/project/results/birds/Tested"
#'   )
#' )
#'
#' partitions_info <- get.all.partitions(parameters)
#' head(partitions_info$best.part.info)
#' }
#'
#' @export
get.all.partitions <- function(parameters){
  
  retorno = list()
  
  pasta.best = paste(parameters$Folders$folderPartitions, 
                     "/", parameters$Config$Dataset.Name, 
                     "/", parameters$Config$Dataset.Name, 
                     "-Best-Silhouete.csv", sep="")
  best = data.frame(read.csv(pasta.best))
  
  
  library(ggplot2)
  
  grafico <- ggplot(best, aes(x = fold, y = valueSilhouete)) +
    geom_line(color = "steelblue", linewidth = 1.2) +
    geom_point(color = "darkred", size = 3) +
    geom_text(aes(label = round(valueSilhouete, 3)),
              vjust = -1, size = 4, color = "black") +
    scale_x_continuous(
      breaks = seq(1, 10, by = 1),        # mostra 1, 2, 3, ..., 10
      limits = c(1, 10)
    ) +
    theme_bw(base_size = 14) +            # tema com fundo branco e grades visíveis
    theme(
      panel.grid.major = element_line(color = "grey80"),  # linhas principais
      panel.grid.minor = element_line(color = "grey90", linetype = "dotted"),
      plot.title = element_text(face = "bold", hjust = 0.5)
    ) +
    labs(
      title = "Silhouette Value per Fold",
      x = "Fold",
      y = "Silhouette"
    )
  
  # Salvar como PDF em alta resolução
  ggsave(
    filename = paste0(parameters$Folders$folderTested, "/silhouette_folds.pdf"),  # nome do arquivo
    plot = grafico,
    device = "pdf",
    width = 12,   # largura em polegadas
    height = 8,   # altura em polegadas
    units = "in"
  )
  
  num.fold = c(0)
  num.part = c(0)
  num.group = c(0)
  best.part.info = data.frame(num.fold, num.part, num.group)
  
  all.partitions.info = data.frame()
  all.total.labels = data.frame()
  
  f = 1
  while(f<=parameters$Config$Number.Folds){
    
    best.fold = best[f,]
    num.fold = best.fold$fold
    num.part = best.fold$part
    
    Pasta = paste(parameters$Folders$folderPartitions, 
                  "/", parameters$Config$Dataset.Name, "/Split-", 
                  f, sep="")
    pasta.groups = paste(Pasta, "/fold-", f, 
                         "-groups-per-partition.csv", sep="")
    clusters = data.frame(read.csv(pasta.groups))
    groups.fold = data.frame(filter(clusters, partition == num.part))
    
    num.group = groups.fold$num.groups
    best.part.info = rbind(best.part.info, 
                           data.frame(num.fold, num.part, num.group))
    
    nome = paste(Pasta, "/Partition-", num.part, 
                 "/partition-", num.part, ".csv", sep="")
    partitions = data.frame(read.csv(nome))
    partitions = data.frame(num.fold, num.part, partitions)
    partitions = arrange(partitions, group)
    
    all.partitions.info = rbind(all.partitions.info, partitions)
    
    nome.2 = paste(Pasta, "/Partition-", num.part,
                   "/fold-", f, "-labels-per-group-partition-", 
                   num.group, ".csv", sep="")
    labels = data.frame(read.csv(nome.2))
    labels = data.frame(num.fold, labels)
    all.total.labels = rbind(all.total.labels , labels)
    
    f = f + 1
    gc()
  } # fim do fold
  
  setwd(parameters$Folders$folderTested)
  write.csv(best, "silhouette.csv", row.names = FALSE)
  write.csv(best.part.info, "best-part-info.csv", row.names = FALSE)
  write.csv(all.partitions.info, "all.partitions.info.csv", row.names = FALSE)
  write.csv(all.total.labels, "all.total.labels.csv", row.names = FALSE)
  
  retorno$silhouette = best
  retorno$best.part.info = best.part.info[-1,]
  retorno$all.partitions.info = all.partitions.info
  retorno$all.total.labels = all.total.labels
  return(retorno)
  
}


#' @title Compute and Export Cluster Label Properties
#'
#' @description
#' Computes a comprehensive set of descriptive statistics, label dependencies,
#' and dataset-level properties for multi-label clusters within a given fold.
#'  
#' The function summarizes label distributions, dependency metrics,
#' labelset frequencies, and dataset measures for the training, testing,
#' validation, and combined (train+val) datasets.  
#' All results are saved to CSV and text files for later analysis.
#'
#' @param nomes.labels.clusters Character vector containing the names of the label columns.
#' @param fold Integer. Identifier of the current cross-validation fold.
#' @param cluster Integer or character. Identifier of the current cluster.
#' @param folderSave Character. Path to the directory where all output files
#' (CSV and TXT summaries) will be stored.
#' @param labels.indices Integer vector. Column indices in the data frames
#' that correspond to the label attributes.
#' @param train Data frame containing the training subset.
#' @param test Data frame containing the testing subset.
#' @param val Data frame containing the validation subset.
#' @param tv Data frame containing the union of training and validation subsets.
#'
#' @details
#' For each dataset subset (train, test, val, and tv), this function:
#' \enumerate{
#'   \item Computes descriptive statistics for each label:
#'     \itemize{
#'       \item Standard deviation, mean, median, sum, min, max, and quantiles.
#'     }
#'     Saved in: \code{summary.csv}.
#'   \item Calculates counts of positive and negative instances per label,
#'     saved in: \code{num-pos-neg.csv}.
#'   \item Extracts labelset frequencies using \code{mldr_from_dataframe()},
#'     saved in: \code{labelsets.csv}.
#'   \item Exports detailed label-level metrics from \code{mldr} objects
#'     (e.g., imbalance ratios, frequencies, etc.) to: \code{labels-info.csv}.
#'   \item Retrieves dataset-level measures from each \code{mldr} object,
#'     such as label cardinality, density, and distinct labelsets.
#'     Saved in: \code{measures.csv}.
#'   \item Computes label dependency scores for each subset using
#'     \code{dependency()} and saves to: \code{dependency.csv}.
#'   \item Prints contingency tables (cross-tabulations of label combinations)
#'     for each subset to text files:
#'       \code{contingency-tr.txt}, \code{contingency-ts.txt},
#'       \code{contingency-vl.txt}, and \code{contingency-tv.txt}.
#' }
#'
#' The function leverages the \pkg{mldr} package to extract multi-label dataset
#' descriptors and statistics, and assumes that binary labels are represented
#' as \code{0/1} values.
#'
#' @return
#' This function does not return an object directly.
#' Instead, it writes multiple summary files to the \code{folderSave} directory:
#' \itemize{
#'   \item \code{summary.csv} — Label-level descriptive statistics.
#'   \item \code{num-pos-neg.csv} — Counts of positive and negative instances.
#'   \item \code{labelsets.csv} — Frequencies of unique label combinations.
#'   \item \code{labels-info.csv} — Label-wise statistics from \pkg{mldr}.
#'   \item \code{measures.csv} — Dataset-level measures from \pkg{mldr}.
#'   \item \code{dependency.csv} — Label dependency coefficients.
#'   \item \code{contingency-*.txt} — Contingency tables for label co-occurrence.
#' }
#'
#' @seealso
#' \code{\link{mldr_from_dataframe}}, \code{\link{dependency}}
#'
#' @examples
#' \dontrun{
#' library(mldr)
#'
#' # Suppose 'train', 'test', 'val', 'tv' are pre-split data frames
#' # with label columns 5:10
#' labels <- colnames(train)[5:10]
#' 
#' properties.clusters(
#'   nomes.labels.clusters = labels,
#'   fold = 1,
#'   cluster = 2,
#'   folderSave = "/home/user/results/cluster-2",
#'   labels.indices = 5:10,
#'   train = train,
#'   test = test,
#'   val = val,
#'   tv = tv
#' )
#' }
#'
#' @export
properties.clusters <- function(nomes.labels.clusters,
                                fold,
                                cluster,
                                folderSave, 
                                labels.indices, 
                                train, 
                                test, 
                                val, 
                                tv){
  
  ##################################################################
  treino.labels = data.frame(train[,labels.indices])
  colnames(treino.labels) = nomes.labels.clusters
  
  teste.labels = data.frame(test[,labels.indices])
  colnames(teste.labels) = nomes.labels.clusters
  
  val.labels = data.frame(val[,labels.indices])
  colnames(val.labels) = nomes.labels.clusters
  
  tv.labels = data.frame(tv[,labels.indices])
  colnames(tv.labels) = nomes.labels.clusters
  
  
  ##########################################################################
  treino.sd = apply(treino.labels , 2, sd)
  treino.mean = apply(treino.labels , 2, mean)
  treino.median = apply(treino.labels , 2, median)
  treino.sum = apply(treino.labels , 2, sum)
  treino.max = apply(treino.labels , 2, max)
  treino.min = apply(treino.labels , 2, min)
  treino.quartis = apply(treino.labels, 2, quantile, 
                         probs = c(0.10, 0.25, 0.50, 0.75, 0.90))
  treino.summary = rbind(sd = treino.sd, mean = treino.mean, 
                         median = treino.median,
                         sum = treino.sum, max = treino.max, 
                         min = treino.min, treino.quartis)
  
  teste.sd = apply(teste.labels , 2, sd)
  teste.mean = apply(teste.labels , 2, mean)
  teste.median = apply(teste.labels , 2, median)
  teste.sum = apply(teste.labels , 2, sum)
  teste.max = apply(teste.labels , 2, max)
  teste.min = apply(teste.labels , 2, min)
  teste.quartis = apply(teste.labels, 2, quantile,
                        probs = c(0.10, 0.25, 0.50, 0.75, 0.90))
  teste.summary = rbind(sd = teste.sd, mean = teste.mean, 
                        median = teste.median,
                        sum = teste.sum, max = teste.max, 
                        min = teste.min, teste.quartis)
  
  val.sd = apply(val.labels , 2, sd)
  val.mean = apply(val.labels , 2, mean)
  val.median = apply(val.labels , 2, median)
  val.sum = apply(val.labels , 2, sum)
  val.max = apply(val.labels , 2, max)
  val.min = apply(val.labels , 2, min)
  val.quartis = apply(val.labels, 2, quantile, 
                      probs = c(0.10, 0.25, 0.50, 0.75, 0.90))
  val.summary = rbind(sd = val.sd, mean = val.mean, 
                      median = val.median,
                      sum = val.sum, max = val.max, 
                      min = val.min, val.quartis)
  
  tv.sd = apply(tv.labels , 2, sd)
  tv.mean = apply(tv.labels , 2, mean)
  tv.median = apply(tv.labels , 2, median)
  tv.sum = apply(tv.labels , 2, sum)
  tv.max = apply(tv.labels , 2, max)
  tv.min = apply(tv.labels , 2, min)
  tv.quartis = apply(tv.labels, 2, quantile, 
                     probs = c(0.10, 0.25, 0.50, 0.75, 0.90))
  tv.summary = rbind(sd = tv.sd, mean = tv.mean, 
                     median = tv.median,
                     sum = tv.sum, max = tv.max, 
                     min = tv.min, tv.quartis)
  
  r1 <- data.frame(fold, cluster, stat = rownames(val.summary), type = "val", val.summary, row.names = NULL)
  r2 <- data.frame(fold, cluster, stat = rownames(teste.summary), type = "test", teste.summary, row.names = NULL)
  r3 <- data.frame(fold, cluster, stat = rownames(treino.summary), type = "train", treino.summary, row.names = NULL)
  r4 <- data.frame(fold, cluster, stat = rownames(tv.summary), type = "tv", tv.summary, row.names = NULL)
  
  sumario <- rbind(r1, r2, r3, r4)
  name = paste(folderSave, "/summary.csv", sep="")
  write.csv(sumario, name, row.names = FALSE)
  
  ##################################################################
  instances.tr <- data.frame(
    label = names(treino.labels),
    negative = colSums(treino.labels == 0),
    positive = colSums(treino.labels == 1)
  )
  rownames(instances.tr) = NULL
  instances.tr = data.frame(fold, cluster, type = "train", instances.tr)
  
  instances.ts <- data.frame(
    label = names(teste.labels),
    negative = colSums(teste.labels == 0),
    positive = colSums(teste.labels == 1)
  )
  rownames(instances.ts) = NULL
  instances.ts = data.frame(fold, cluster, type = "test", instances.ts)
  
  instances.vl <- data.frame(
    label = names(val.labels),
    negative = colSums(val.labels == 0),
    positive = colSums(val.labels == 1)
  )
  rownames(instances.vl) = NULL
  instances.vl = data.frame(fold, cluster, type = "val", instances.vl)
  
  instances.tv <- data.frame(
    label = names(tv.labels),
    negative = colSums(tv.labels == 0),
    positive = colSums(tv.labels == 1)
  )
  rownames(instances.tv) = NULL
  instances.tv = data.frame(fold, cluster, type = "tv", instances.tv)
  
  allposneg = rbind(instances.tr, instances.ts, instances.vl, instances.tv)
  name = paste0(folderSave, "/num-pos-neg.csv")
  write.csv(allposneg , name, row.names = FALSE)
  
  ##########################################################################
  mldr.treino = mldr_from_dataframe(train, labelIndices = labels.indices)
  mldr.teste = mldr_from_dataframe(test, labelIndices = labels.indices)
  mldr.val = mldr_from_dataframe(val, labelIndices = labels.indices)
  mldr.tv = mldr_from_dataframe(tv, labelIndices = labels.indices)
  
  ##########################################################################
  labelsets.train = data.frame(mldr.treino$labelsets)
  names(labelsets.train) = c("labelset", "frequency")
  labelsets.train = data.frame(type = "train", labelsets.train)
  
  labelsets.test = data.frame(mldr.teste$labelsets)
  names(labelsets.test) = c("labelset", "frequency")
  labelsets.test = data.frame(type = "test", labelsets.test)
  
  labelsets.val = data.frame(mldr.val$labelsets)
  names(labelsets.val) = c("labelset", "frequency")
  labelsets.val = data.frame(type = "val", labelsets.val)
  
  labelsets.tv = data.frame(mldr.tv$labelsets)
  names(labelsets.tv) = c("labelset", "frequency")
  labelsets.tv = data.frame(type = "tv", labelsets.tv)
  
  res = rbind(labelsets.train, labelsets.test, labelsets.val, labelsets.tv)
  res = cbind(fold, cluster, res)
  
  name = paste(folderSave, "/labelsets.csv", sep="")
  write.csv(res, name, row.names = FALSE)
  
  ##########################################################################
  labels.train = data.frame(mldr.treino$labels)
  labels.test = data.frame(mldr.teste$labels)
  labels.val = data.frame(mldr.val$labels)
  labels.tv = data.frame(mldr.tv$labels)
  
  r1 <- data.frame(stat = rownames(labels.train), type = "val", labels.train, row.names = NULL)
  r2 <- data.frame(stat = rownames(labels.test), type = "test", labels.test, row.names = NULL)
  r3 <- data.frame(stat = rownames(labels.val), type = "train", labels.val, row.names = NULL)
  r4 <- data.frame(stat = rownames(labels.tv), type = "tv", labels.tv, row.names = NULL)
  
  all.labels <- rbind(r1, r2, r3, r4)
  name = paste(folderSave, "/labels-info.csv", sep="")
  write.csv(all.labels, name, row.names = FALSE)
  
  
  ##########################################################################  
  properties.train = data.frame(mldr.treino$measures)
  properties.train = cbind(fold, cluster, properties.train)
  properties.train = data.frame(type = "train", properties.train)
  
  properties.test = data.frame(mldr.teste$measures)
  properties.test = cbind(fold, cluster, properties.test)
  properties.test = data.frame(type = "test", properties.test)
  
  properties.val = data.frame(mldr.val$measures)
  properties.val = cbind(fold, cluster, properties.val)
  properties.val = data.frame(type = "val", properties.val)
  
  properties.tv = data.frame(mldr.tv$measures)
  properties.tv = cbind(fold, cluster, properties.tv)
  properties.tv = data.frame(type = "tv", properties.tv)
  
  measures = rbind(properties.train, properties.test, properties.val, properties.tv)
  measures = cbind(fold, cluster, measures)
  
  name = paste(folderSave , "/measures.csv", sep="")
  write.csv(measures , name, row.names = FALSE)
  
  ##########################################################################  
  label.space.tr = train[,labels.indices]
  ld.train = dependency(label.space.tr)
  
  label.space.ts = test[,labels.indices]
  ld.test = dependency(label.space.ts)
  
  label.space.vl = val[,labels.indices]
  ld.val = dependency(label.space.vl)
  
  label.space.tv = tv[,labels.indices]
  ld.tv = dependency(label.space.tv)
  
  ld = data.frame(fold, cluster, 
                  train = ld.train$label.dependency, 
                  test = ld.test$label.dependency, 
                  val = ld.val$label.dependency, 
                  tv = ld.tv$label.dependency)
  name = paste(folderSave , "/dependency.csv", sep="")
  write.csv(ld, name, row.names = FALSE)
  
  ##################################################################
  # name = paste(folderSave , "/contingency-tr.txt", sep="")
  # sink(name )
  # print(table(label.space.tr))
  # sink()
  # 
  # name = paste(folderSave , "/contingency-ts.txt", sep="")
  # sink(name )
  # print(table(label.space.ts))
  # sink()
  # 
  # name = paste(folderSave , "/contingency-vl.txt", sep="")
  # sink(name )
  # print(table(label.space.vl))
  # sink()
  # 
  # name = paste(folderSave , "/contingency-tv.txt", sep="")
  # sink(name )
  # print(table(label.space.tv))
  # sink()
  
}

#########################################################################
#
#########################################################################
predictions.information <- function(nomes.rotulos, 
                                    proba, 
                                    preds, 
                                    trues, 
                                    folder){
  
  #####################################################################
  pred.o = paste(colnames(preds), "-pred", sep="")
  names(preds) = pred.o
  
  true.labels = paste(colnames(trues), "-true", sep="")
  names(trues) = true.labels
  
  proba.n = paste(nomes.rotulos, "-proba", sep="")
  names(proba) = proba.n
  
  all.predictions = cbind(proba, preds, trues)
  setwd(folder)
  write.csv(all.predictions, "predictions.csv", row.names = FALSE)
  
  ###############################################
  bipartition = data.frame(trues, preds)
  
  # número de instâncias do conjunto
  num.instancias = nrow(bipartition)
  
  # número de rótulos do conjunto
  num.rotulos = ncol(trues)
  
  # número de instâncias positivas
  num.positive.instances = apply(bipartition, 2, sum)
  
  # número de instâncias negativas
  num.negative.instances = num.instancias - num.positive.instances 
  
  # salvando
  res = rbind(num.positive.instances, num.negative.instances)
  name = paste(folder, "/instances-pn.csv", sep="")
  write.csv(res, name)
  
  # calcular rótulo verdadeiro igual a 1
  true_1 = data.frame(ifelse(trues==1,1,0))
  total_true_1 = apply(true_1, 2, sum)
  
  # calcular rótulo verdadeiro igual a 0
  true_0 = data.frame(ifelse(trues==0,1,0))
  total_true_0 = apply(true_0, 2, sum)
  
  # calcular rótulo predito igual a 1
  pred_1 = data.frame(ifelse(preds==1,1,0))
  total_pred_1 = apply(pred_1, 2, sum)
  
  # calcular rótulo verdadeiro igual a 0
  pred_0 = data.frame(ifelse(preds==0,1,0))
  total_pred_0 = apply(pred_0, 2, sum)
  
  matriz_totais = cbind(total_true_0, total_true_1, total_pred_0, total_pred_1)
  row.names(matriz_totais) = nomes.rotulos
  name = paste(folder, "/trues-preds.csv", sep="")
  write.csv(matriz_totais, name)
  
  # Verdadeiro Positivo: O modelo previu 1 e a resposta correta é 1
  TPi  = data.frame(ifelse((true_1 & true_1),1,0))
  tpi = paste(nomes.rotulos, "-TP", sep="")
  names(TPi) = tpi
  
  # Verdadeiro Negativo: O modelo previu 0 e a resposta correta é 0
  TNi  = data.frame(ifelse((true_0 & pred_0),1,0))
  tni = paste(nomes.rotulos, "-TN", sep="")
  names(TNi) = tni
  
  # Falso Positivo: O modelo previu 1 e a resposta correta é 0
  FPi  = data.frame(ifelse((true_0 & pred_1),1,0))
  fpi = paste(nomes.rotulos, "-FP", sep="")
  names(FPi) = fpi
  
  # Falso Negativo: O modelo previu 0 e a resposta correta é 1
  FNi  = data.frame(ifelse((true_1 & pred_0),1,0))
  fni = paste(nomes.rotulos, "-FN", sep="")
  names(FNi) = fni
  
  fpnt = data.frame(TPi, FPi, FNi, TNi)
  name = paste(folder, "/tfpn.csv", sep="")
  write.csv(fpnt, name, row.names = FALSE)
  
  # total de verdadeiros positivos
  TPl = apply(TPi, 2, sum)
  tpl = paste(nomes.rotulos, "-TP", sep="")
  names(TPl) = tpl
  
  # total de verdadeiros negativos
  TNl = apply(TNi, 2, sum)
  tnl = paste(nomes.rotulos, "-TN", sep="")
  names(TNl) = tnl
  
  # total de falsos negativos
  FNl = apply(FNi, 2, sum)
  fnl = paste(nomes.rotulos, "-FN", sep="")
  names(FNl) = fnl
  
  # total de falsos positivos
  FPl = apply(FPi, 2, sum)
  fpl = paste(nomes.rotulos, "-FP", sep="")
  names(FPl) = fpl
  
  matriz_confusao_por_rotulos = data.frame(TPl, FPl, FNl, TNl)
  colnames(matriz_confusao_por_rotulos) = c("TP","FP", "FN", "TN")
  row.names(matriz_confusao_por_rotulos) = nomes.rotulos
  name = paste(folder, "/matrix-confusion-2.csv", sep="")
  write.csv(matriz_confusao_por_rotulos, name)
  
}

#' @title Generate and Export Multi-Label Confusion Matrices
#'
#' @description
#' Computes detailed confusion matrix components (True Positives, False Positives,
#' False Negatives, and True Negatives) for each label in a multi-label classification task.
#'  
#' It also exports summary CSV files containing total counts of true/predicted labels,
#' positive/negative instance distributions, and label-wise confusion matrices.
#'
#' @param true Data frame or matrix of true binary label values (0 or 1).
#' Each column corresponds to a label.
#' @param pred Data frame or matrix of predicted binary label values (0 or 1).
#' Must have the same dimensions and column order as \code{true}.
#' @param type Character string indicating the dataset type or evaluation stage
#' (e.g., \code{"train"}, \code{"test"}, \code{"val"}). Used in filenames.
#' @param salva Character string. Path to the directory where output files will be saved.
#' @param nomes.rotulos Character vector containing the label names (used for row names and column naming).
#'
#' @details
#' For each label, the function computes:
#' \itemize{
#'   \item \strong{TP (True Positive)} — model predicted 1, and true label is 1.
#'   \item \strong{FP (False Positive)} — model predicted 1, but true label is 0.
#'   \item \strong{FN (False Negative)} — model predicted 0, but true label is 1.
#'   \item \strong{TN (True Negative)} — model predicted 0, and true label is 0.
#' }
#'
#' Additionally, it exports several CSV summaries:
#' \enumerate{
#'   \item \code{<type>-ins-pn.csv} — counts of positive and negative instances per label.
#'   \item \code{<type>-trues-preds.csv} — total number of true and predicted 0s and 1s per label.
#'   \item \code{<type>-tfpn.csv} — element-wise binary indicators for TP, FP, FN, TN across all instances.
#'   \item \code{<type>-matrix-confusion.csv} — aggregated confusion matrix per label (TP, FP, FN, TN totals).
#' }
#'
#' This function assumes binary labels (0/1) and that \code{true} and \code{pred} contain
#' the same number of instances and labels.
#'
#' @return
#' The function does not return a value.
#' Instead, it writes the following files to the directory specified by \code{salva}:
#' \itemize{
#'   \item \code{<type>-ins-pn.csv}
#'   \item \code{<type>-trues-preds.csv}
#'   \item \code{<type>-tfpn.csv}
#'   \item \code{<type>-matrix-confusion.csv}
#' }
#'
#' @examples
#' \dontrun{
#' # Example for a 3-label classification problem
#' true <- data.frame(A = c(1, 0, 1, 1), B = c(0, 0, 1, 1), C = c(1, 1, 0, 0))
#' pred <- data.frame(A = c(1, 1, 1, 0), B = c(0, 0, 1, 0), C = c(1, 0, 0, 1))
#' nomes.rotulos <- c("A", "B", "C")
#'
#' matrix.confusao(true, pred, type = "test",
#'                 salva = "results/",
#'                 nomes.rotulos = nomes.rotulos)
#' }
#'
#' @export
matrix.confusao <- function(true, pred, type, salva, nomes.rotulos){ 
  
  bipartition = data.frame(true, pred)
  
  num.instancias = nrow(bipartition)
  num.rotulos = ncol(true) # número de rótulos do conjunto
  
  num.positive.instances = apply(bipartition, 2, sum) # número de instâncias positivas
  num.negative.instances = num.instancias - num.positive.instances   # número de instâncias negativas  # salvando
  
  res = rbind(num.positive.instances, num.negative.instances)
  #name = paste(salva, "/", type, "-ins-pn.csv", sep="")
  #write.csv(res, name)
  
  true_1 = data.frame(ifelse(true==1,1,0)) # calcular rótulo verdadeiro igual a 1
  total_true_1 = apply(true_1, 2, sum)
  
  true_0 = data.frame(ifelse(true==0,1,0)) # calcular rótulo verdadeiro igual a 0
  total_true_0 = apply(true_0, 2, sum)
  
  pred_1 = data.frame(ifelse(pred==1,1,0)) # calcular rótulo predito igual a 1
  total_pred_1 = apply(pred_1, 2, sum)
  
  pred_0 = data.frame(ifelse(pred==0,1,0)) # calcular rótulo verdadeiro igual a 0
  total_pred_0 = apply(pred_0, 2, sum)
  
  matriz_totais = cbind(total_true_0, total_true_1, total_pred_0, total_pred_1)
  row.names(matriz_totais) = nomes.rotulos
  #name = paste(salva, "/", type, "-trues-preds.csv", sep="")
  #write.csv(matriz_totais, name)
  
  # Verdadeiro Positivo: O modelo previu 1 e a resposta correta é 1
  TPi  = data.frame(ifelse((true_1 & true_1),1,0))
  tpi = paste(nomes.rotulos, "-TP", sep="")
  names(TPi) = tpi
  
  # Verdadeiro Negativo: O modelo previu 0 e a resposta correta é 0
  TNi  = data.frame(ifelse((true_0 & pred_0),1,0))
  tni = paste(nomes.rotulos, "-TN", sep="")
  names(TNi) = tni
  
  # Falso Positivo: O modelo previu 1 e a resposta correta é 0
  FPi  = data.frame(ifelse((true_0 & pred_1),1,0))
  fpi = paste(nomes.rotulos, "-FP", sep="")
  names(FPi) = fpi
  
  # Falso Negativo: O modelo previu 0 e a resposta correta é 1
  FNi  = data.frame(ifelse((true_1 & pred_0),1,0))
  fni = paste(nomes.rotulos, "-FN", sep="")
  names(FNi) = fni
  
  fpnt = data.frame(TPi, FPi, FNi, TNi)
  name = paste(salva, "/", type, "-tfpn.csv", sep="")
  #write.csv(fpnt, name, row.names = FALSE)
  
  # total de verdadeiros positivos
  TPl = apply(TPi, 2, sum)
  tpl = paste(nomes.rotulos, "-TP", sep="")
  names(TPl) = tpl
  
  # total de verdadeiros negativos
  TNl = apply(TNi, 2, sum)
  tnl = paste(nomes.rotulos, "-TN", sep="")
  names(TNl) = tnl
  
  # total de falsos negativos
  FNl = apply(FNi, 2, sum)
  fnl = paste(nomes.rotulos, "-FN", sep="")
  names(FNl) = fnl
  
  # total de falsos positivos
  FPl = apply(FPi, 2, sum)
  fpl = paste(nomes.rotulos, "-FP", sep="")
  names(FPl) = fpl
  
  matriz_confusao_por_rotulos = data.frame(TPl, FPl, FNl, TNl)
  colnames(matriz_confusao_por_rotulos) = c("TP","FP", "FN", "TN")
  row.names(matriz_confusao_por_rotulos) = nomes.rotulos
  name = paste(salva, "/", type, "-matrix-confusion.csv", sep="")
  write.csv(matriz_confusao_por_rotulos, name)
}



###############################################################################
#' Compute and export AUPRC (Precision-Recall) metrics for multilabel classification
#'
#' @description
#' This function computes the AUPRC (Area Under the Precision-Recall Curve)
#' for each label in a multilabel classification problem. It also calculates
#' macro and micro AUPRC scores and exports the results as CSV files.
#' Optional plotting code for PR curves is included (commented out).
#'
#' @details
#' The function evaluates per-label and aggregated AUPRC metrics using
#' the \code{PRROC} package. For each label, a precision-recall curve is
#' generated when possible (skipping labels with only one class present).
#' It writes two CSV outputs:
#' \itemize{
#'   \item \code{r-auprc-per-label.csv}: AUPRC values for each label.
#'   \item A file specified by \code{nome}: macro and micro AUPRC scores.
#' }
#'
#' @param y_true Matrix or data frame. True binary labels (0 or 1) for each class.
#' @param y_proba Matrix or data frame. Predicted probabilities or confidence scores for each class.
#' @param Folder Character. Directory where output CSV files will be saved.
#' @param nome Character. The name of the main output CSV file containing macro and micro AUPRC values.
#'
#' @return
#' Two CSV files are written to disk:
#' \enumerate{
#'   \item \code{r-auprc-per-label.csv}: per-label AUPRC values.
#'   \item The file specified in \code{nome}: overall macro and micro AUPRC values.
#' }
#' The function does not return an R object (invisible \code{NULL}).
#'
#' @examples
#' \dontrun{
#' # Example data
#' y_true <- data.frame(
#'   L1 = c(1, 0, 1, 0),
#'   L2 = c(0, 1, 1, 0)
#' )
#' y_proba <- data.frame(
#'   L1 = c(0.9, 0.2, 0.8, 0.3),
#'   L2 = c(0.1, 0.7, 0.6, 0.4)
#' )
#'
#' # Output directory and filenames
#' Folder <- "results"
#' dir.create(Folder, showWarnings = FALSE)
#'
#' auprc.curve(y_true = y_true, y_proba = y_proba,
#'             Folder = Folder,
#'             nome = paste0(Folder, "/auprc-summary.csv"))
#' }
#'
#' @author Elaine Cecília Gatto - Cissa
#'
#' @seealso
#' [PRROC::pr.curve()] for PR curve and AUPRC computation,
#' [write.csv()] for saving structured metrics.
#'
#' @note
#' This function requires the \code{PRROC} package.
#' Labels with no positive or negative instances are skipped (AUPRC = NA).
#' The commented plotting code can be re-enabled to generate per-label
#' and global PR curve visualizations.
#'
#' @export
auprc.curve <- function(y_true, y_proba, Folder, nome){
  library(PRROC)
  
  # Garantindo que y_true e y_score sejam matrizes
  y_true <- as.matrix(y_true)
  y_score <- as.matrix(y_proba)
  
  auprc_list <- c()
  
  for(i in 1:ncol(y_true)){
    cat("\n", i)
    # Evita erro quando não houver positivos ou negativos
    if(sum(y_true[, i] == 1) == 0 | sum(y_true[, i] == 0) == 0) {
      auprc_list[i] <- NA
      next
    }
    
    pr_obj <- pr.curve(
      scores.class0 = y_score[y_true[, i] == 1, i],
      scores.class1 = y_score[y_true[, i] == 0, i],
      curve = TRUE
    )
    
    auprc_list[i] <- pr_obj$auc.integral
    
    #nome = paste("AUPRC-Label", i, ".pdf", sep="")
    #pdf(file = paste0(Folder, "/", nome), width = 8, height = 6)
    #plot(pr_obj, main = paste("PR Curve label", i))
    #dev.off()
  }
  
  auprc_per_labels = data.frame(t(auprc_list))
  colnames(auprc_per_labels) = colnames(y_true)
  nome1 = paste(Folder, "/r-auprc-per-label.csv", sep="")
  write.csv(auprc_per_labels, nome1, row.names = FALSE)
  
  # Macro AUPRC
  auprc_macro <- mean(auprc_list, na.rm = TRUE)
  
  # Micro AUPRC: achata tudo
  y_true_vec <- as.vector(y_true)
  y_score_vec <- as.vector(y_score)
  pr_micro <- pr.curve(
    scores.class0 = y_score_vec[y_true_vec == 1],
    scores.class1 = y_score_vec[y_true_vec == 0],
    curve = TRUE
  )
  auprc_micro <- pr_micro$auc.integral
  
  auprc = data.frame(auprc_micro, auprc_macro)
  auprc = data.frame(t(auprc))
  Measure = rownames(auprc)
  auprc = data.frame(Measure, auprc)
  rownames(auprc) = NULL
  colnames(auprc) = c("Measure", "Value")
  write.csv(auprc, nome, row.names = FALSE)
  
  # Salvar gráfico
  # pdf("PR_micro.pdf", width = 8, height = 6)
  # plot(pr_micro, main = "Micro-PR Curve (AUPRC Global)")
  # dev.off()
  
}



#########################################################################################################
#' Compute and export ROC curve evaluation for multilabel classification
#'
#' @description
#' This function evaluates the ROC (Receiver Operating Characteristic) metrics
#' for multilabel classification results and exports the computed metrics to a CSV file.
#' Optionally, the function can also plot and save the ROC curve (the plotting code
#' is currently commented out but preserved for reference).
#'
#' @details
#' The function uses \code{mldr_evaluate()} to compute performance metrics and
#' ROC-related statistics for multilabel models. The results are converted into a
#' clean data frame and saved to disk. If the ROC object is available, its AUC
#' (Area Under the Curve) is extracted and appended to the output.
#'
#' @param f Integer or character. Identifier of the fold being evaluated (used in cross-validation).
#' @param y_pred Data frame or list. Predicted label scores or probabilities from the model.
#' @param test Data frame or list. True labels for the test partition.
#' @param Folder Character. Directory path where output files (e.g., plots or CSVs) will be saved.
#' @param nome Character. The name of the output CSV file (including path if needed).
#'
#' @return
#' A CSV file is written to disk containing all evaluation metrics derived from
#' \code{mldr_evaluate()}, including (if available) the ROC AUC value.
#' The function does not return an R object (invisible \code{NULL}).
#'
#' @examples
#' \dontrun{
#' test <- data.frame(L1 = c(1, 0, 1, 0), L2 = c(0, 1, 1, 0))
#' y_pred <- data.frame(L1 = c(0.9, 0.2, 0.8, 0.3),
#'                      L2 = c(0.1, 0.7, 0.6, 0.4))
#' output_dir <- "results"
#' dir.create(output_dir, showWarnings = FALSE)
#'
#' roc.curve(f = 1, y_pred = y_pred, test = test,
#'           Folder = output_dir,
#'           nome = paste0(output_dir, "/fold1_roc.csv"))
#' }
#'
#' @author Elaine Cecília Gatto - Cissa
#'
#' @seealso
#' [mldr_evaluate()] for multilabel evaluation,
#' [plot()] for ROC curve visualization,
#' and [write.csv()] for saving structured metrics.
#'
#' @note
#' Ensure that the \code{mldr} package (or any library providing \code{mldr_evaluate})
#' is loaded in your environment. The commented ROC plotting section can be re-enabled
#' if graphical outputs are required.
#'
#' @export
roc.curve <- function(f, y_pred, test, Folder, nome){
  
  res = mldr_evaluate(test, y_pred)
  
  ###############################################################
  # PLOTANDO ROC CURVE
  #name = paste(Folder, "/roc.pdf", sep="")
  #pdf(name, width = 10, height = 8)
  #print(plot(res$roc, print.thres = 'best', print.auc=TRUE, 
  #            print.thres.cex=0.7, grid = TRUE, identity=TRUE,
  #            axes = TRUE, legacy.axes = TRUE, 
  #            identity.col = "#a91e0e", col = "#1161d5",
  #            main = paste("fold ", f, " ", nome, sep="")))
  #dev.off()
  #cat("\n")
  
  ###############################################################
  # Transformar a lista em data frame, removendo 'roc' para evitar problemas
  df_res <- res
  if("roc" %in% names(df_res)) df_res$roc <- NULL
  
  df_metrics <- data.frame(
    metric = names(df_res),
    value = unlist(df_res)
  )
  
  # Se quiser, também adiciona a AUC do objeto ROC
  if(!is.null(res$roc)) {
    df_metrics <- rbind(df_metrics, data.frame(
      metric = "roc_auc",
      value = res$roc$auc
    ))
  }
  
  colnames(df_metrics) = c("Measure", "Value")
  write.csv(df_metrics, nome, row.names = FALSE)
  
}



#########################################################################################################
#' Evaluate multilabel classification results and save performance metrics
#'
#' @description
#' This function performs multilabel model evaluation by computing confusion matrices
#' and derived performance measures. It saves the main evaluation results to CSV files
#' for further analysis and reporting.
#'
#' @details
#' The function uses `multilabel_confusion_matrix()` to generate per-label confusion
#' matrices from the true and predicted multilabel sets. Then, it computes overall
#' evaluation metrics using `multilabel_evaluate()` and organizes the results in
#' structured tables. Summary information, including true/false positives and negatives,
#' is saved in CSV format.
#'
#' @param f Integer or character. Identifier for the current fold (used in cross-validation).
#' It is appended to column names in the output.
#' @param y_true Data frame or list. Ground truth labels for the multilabel task.
#' Must contain one column per label.
#' @param y_pred Data frame or list. Predicted labels with the same structure as `y_true`.
#' @param salva Character. Directory path where result files will be saved.
#' @param nome Character. Base name used to name the output CSV files.
#'
#' @return
#' This function writes the following files to disk:
#' \itemize{
#'   \item `<nome>.csv` — evaluation metrics for the given fold.
#'   \item `<nome>-utiml.csv` — detailed confusion matrix statistics (optional, currently commented).
#' }
#' The function does not return an object in R (invisible `NULL`).
#'
#' @examples
#' \dontrun{
#' # Example: evaluating multilabel predictions for one fold
#' y_true <- data.frame(L1 = c(1,0,1,0), L2 = c(0,1,1,0))
#' y_pred <- data.frame(L1 = c(1,0,0,0), L2 = c(1,1,0,0))
#' output_dir <- "results"
#' dir.create(output_dir, showWarnings = FALSE)
#'
#' avaliacao(f = 1, y_true = y_true, y_pred = y_pred,
#'           salva = output_dir, nome = "Fold1_results")
#' }
#'
#' @author Elaine Cecília Gatto - Cissa
#'
#' @seealso
#' [multilabel_confusion_matrix()], [multilabel_evaluate()],
#' [write.csv()] for saving structured outputs.
#'
#' @note
#' The helper functions `multilabel_confusion_matrix()` and `multilabel_evaluate()`
#' must be available in the environment or loaded from the appropriate library.
#'
#' @export
avaliacao <- function(f, y_true, y_pred, salva, nome){
  
  #salva.0 = paste(salva, "/", nome, "-conf-mat.txt", sep="")
  #sink(file=salva.0, type="output")
  confmat = multilabel_confusion_matrix(y_true, y_pred)
  #print(confmat)
  #sink()
  
  resConfMat = multilabel_evaluate(confmat)
  resConfMat = data.frame(resConfMat)
  names(resConfMat) = paste("Fold-", f, sep="")
  Measure = rownames(resConfMat)
  resConfMat = data.frame(Measure, resConfMat)
  rownames(resConfMat) = NULL
  salva.1 = paste(salva, "/", nome, ".csv", sep="")
  write.csv(resConfMat, salva.1, row.names = FALSE)
  
  conf.mat = data.frame(confmat$TPl, confmat$FPl,
                        confmat$FNl, confmat$TNl)
  names(conf.mat) = c("TP", "FP", "FN", "TN")
  conf.mat.perc = data.frame(conf.mat/nrow(y_true$dataset))
  names(conf.mat.perc) = c("TP.perc", "FP.perc", "FN.perc", "TN.perc")
  wrong = conf.mat$FP + conf.mat$FN
  wrong.perc = wrong/nrow(y_true$dataset)
  correct = conf.mat$TP + conf.mat$TN
  correct.perc = correct/nrow(y_true$dataset)
  conf.mat.2 = data.frame(conf.mat, conf.mat.perc, wrong, correct, 
                          wrong.perc, correct.perc)
  salva.2 = paste(salva, "/", nome, "-utiml.csv", sep="")
  #write.csv(conf.mat.2, salva.2)
  
  
}





###########################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com            #
# Thank you very much!                                                    #
###########################################################################
