cat("\n\n########################################################")
  cat("\n# RSCRIPT: START EXECUTE CLUSTERS                      #")
  cat("\n########################################################\n\n")


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



cat("\n##############################")
cat("\n# Set Work Space             #")
cat("\n##############################\n\n")
library(here)
library(stringr)
FolderRoot <- here::here()



cat("\n########################################")
cat("\n# R Options Configuration              #")
cat("\n########################################\n\n")
options(java.parameters = "-Xmx64g")  # JAVA
options(show.error.messages = TRUE)   # ERROR MESSAGES
options(scipen=20)                    # number of places after the comma
options(repos = c(CRAN = "https://cloud.r-project.org"))



cat("\n########################################")
cat("\n# Creating parameters list             #")
cat("\n########################################\n\n")
parameters = list()


cat("\n########################################")
cat("\n# Reading Datasets-Original.csv        #")
cat("\n########################################\n\n")
setwd(FolderRoot)
datasets <- data.frame(read.csv("datasets-original.csv"))
parameters$Datasets.List = datasets


cat("\n#####################################")
cat("\n# GET ARGUMENTS FROM COMMAND LINE   #")
cat("\n#####################################\n\n")
args <- commandArgs(TRUE)

config_file <- args[1]


# config_file = "~/HPML.D.CE/config-files/elcc-GnegativeGO-1.csv"


parameters$Config.File$Name = config_file
if(file.exists(config_file)==FALSE){
  cat("\n################################################################")
  cat("\n# Missing Config File! Verify the following path:              #")
  cat("\n################################################################")
  cat("\n --------------->", config_file)
  break
} else {
  cat("\n########################################")
  cat("\n# Properly loaded configuration file!  #")
  cat("\n########################################\n\n")
}


cat("\n########################################")
cat("\n# Config File                          #")
cat("\n########################################\n\n")
config = data.frame(read.csv(config_file))
print(config)



cat("\n##################################################")
cat("\n# HPML.D.CE: Getting Parameters                  #")
cat("\n##################################################\n\n")
FolderScript = toString(config$Value[1])
FolderScript = str_remove(FolderScript, pattern = " ")
parameters$Config$FolderScript = FolderScript

dataset_path = toString(config$Value[2])
dataset_path = str_remove(dataset_path, pattern = " ")
parameters$Config$Dataset.Path = dataset_path

folderResults = toString(config$Value[3])
folderResults = str_remove(folderResults, pattern = " ")
parameters$Config$Folder.Results = folderResults

Partitions_Path = toString(config$Value[4])
Partitions_Path = str_remove(Partitions_Path, pattern = " ")
parameters$Config$Partitions.Path = Partitions_Path

Implementation = toString(config$Value[5])
Implementation = str_remove(Implementation, pattern = " ")
parameters$Config$Implementation = Implementation

similarity = toString(config$Value[6])
similarity = str_remove(similarity, pattern = " ")
parameters$Config$Similarity = similarity

dendrogram = toString(config$Value[7])
dendrogram = str_remove(dendrogram, pattern = " ")
parameters$Config$Dendrogram = dendrogram

criteria = toString(config$Value[8])
criteria = str_remove(criteria, pattern = " ")
parameters$Config$Criteria = criteria 

dataset_name = toString(config$Value[9])
dataset_name = str_remove(dataset_name, pattern = " ")
parameters$Config$Dataset.Name = dataset_name

number_dataset = as.numeric(config$Value[10])
parameters$Config$Number.Dataset = number_dataset

number_folds = as.numeric(config$Value[11])
parameters$Config$Number.Folds = number_folds

number_cores = as.numeric(config$Value[12])
parameters$Config$Number.Cores = number_cores

number_chains = as.numeric(config$Value[13])
parameters$Config$Number.Chains = number_chains

ds = datasets[number_dataset,]
parameters$DatasetInfo = ds



cat("\n########################################")
cat("\n# Loading R Sources                    #")
cat("\n########################################\n\n")
source(file.path(parameters$Config$FolderScript, "libraries.R"))
source(file.path(parameters$Config$FolderScript, "utils.R"))



cat("\n#########################################")
cat("\n# HPML.D: Get directories               #")
cat("\n#########################################\n\n")
if (dir.exists(folderResults) == FALSE) {dir.create(folderResults)}
diretorios <- directories(parameters)
parameters$Folders = diretorios

FolderE = paste(parameters$Folders$folderResults, 
                "/Tested2", sep="")
if(dir.exists(FolderE)==FALSE){dir.create(FolderE)}
parameters$Folders$folderTested2 = FolderE



cat("\n####################################################################")
cat("\n# Checking the dataset tar.gz file                                 #")
cat("\n####################################################################\n\n")
str00 = paste(dataset_path, "/", ds$Name,".tar.gz", sep = "")
str00 = str_remove(str00, pattern = " ")

if(file.exists(str00)==FALSE){
  
  cat("\n######################################################################")
  cat("\n# The tar.gz file for the dataset to be processed does not exist!    #")
  cat("\n# Please pass the path of the tar.gz file in the configuration file! #")
  cat("\n# The path entered was: ", str00, "                                  #")
  cat("\n######################################################################\n\n")
  break
  
} else {
  
  cat("\n####################################################################")
  cat("\n# tar.gz file of the DATASET loaded correctly!                     #")
  cat("\n####################################################################\n\n")
  
  # COPIANDO
  str01 = paste("cp ", str00, " ", parameters$Folders$folderDatasets, sep = "")
  res = system(str01)
  if (res != 0) {
    cat("\nError: ", str01)
    break
  }
  
  # DESCOMPACTANDO
  str02 = paste("tar xzf ", parameters$Folders$folderDatasets, "/", ds$Name,
                ".tar.gz -C ", parameters$Folders$folderDatasets, sep = "")
  res = system(str02)
  if (res != 0) {
    cat("\nError: ", str02)
    break
  }
  
  #APAGANDO
  str03 = paste("rm ", parameters$Folders$folderDatasets, "/", ds$Name,
                ".tar.gz", sep = "")
  res = system(str03)
  if (res != 0) {
    cat("\nError: ", str03)
    break
  }
  
}



cat("\n####################################################################")
cat("\n# Checking the BEST PARTITIONS dataset tar.gz file                 #")
cat("\n####################################################################\n\n")
str00 = paste(Partitions_Path, "/", ds$Name,".tar.gz", sep = "")
str00 = str_remove(str00, pattern = " ")

if(file.exists(str00)==FALSE){
  
  cat("\n######################################################################")
  cat("\n# The tar.gz file for the partition to be processed does not exist!  #")
  cat("\n# Please pass the path of the tar.gz file in the configuration file! #")
  cat("\n# The path entered was: ", str00, "                                  #")
  cat("\n######################################################################\n\n")
  break
  
} else {
  
  cat("\n####################################################################")
  cat("\n# tar.gz file of the PARTITION loaded correctly!                   #")
  cat("\n####################################################################\n\n")
  
  # COPIANDO
  str01 = paste("cp ", str00, " ", diretorios$folderPartitions, sep = "")
  res = system(str01)
  if (res != 0) {
    cat("\nError: ", str01)
    break
  }
  
  # DESCOMPACTANDO
  str02 = paste("tar xzf ", diretorios$folderPartitions, "/", ds$Name,
                ".tar.gz -C ", diretorios$folderPartitions, sep = "")
  res = system(str02)
  if (res != 0) {
    cat("\nError: ", str02)
    break
  }
  
  #APAGANDO
  str03 = paste("rm ", diretorios$folderPartitions, "/", ds$Name,
                ".tar.gz", sep = "")
  res = system(str03)
  if (res != 0) {
    cat("\nError: ", str03)
    break
  }
  
}




if(parameters$Config$Implementation =="clus"){
  # 
  # cat("\n\nRUNNING CLUS\n")  
  # 
  # setwd(FolderScripts)
  # source("run-clus.R")
  # 
  # timeFinal <- system.time(results <- execute.run.clus(parameters))
  # result_set <- t(data.matrix(timeFinal))
  # setwd(parameters$Folders$folderTested)
  # write.csv(result_set, "Runtime.csv")
  # 
  # print(system(paste("rm -r ", diretorios$folderDatasets, sep="")))
  # print(system(paste("rm -r ", diretorios$folderPartitions, sep="")))
  # 
  # cat("\n\nCOPY TO GOOGLE DRIVE")
  # origem = parameters$Folders$folderTested
  # destino = paste("nuvem:Clusters-Chains-HPMLs/",
  #                 parameters$Config$Implementation, "/", 
  #                 parameters$Config$Similarity, "/", 
  #                 parameters$Config$Criteria, "/", 
  #                 parameters$Config$Dataset.Name, sep="")
  # comando1 = paste("rclone -P copy ", origem, " ", destino, sep="")
  # cat("\n", comando1, "\n")
  # a = print(system(comando1))
  # a = as.numeric(a)
  # if(a != 0) {
  #   stop("Erro RCLONE")
  #   quit("yes")
  # }
  # 
  
  # cat("\n####################################################################")
  # cat("\n# Compress folders and files                                       #")
  # cat("\n####################################################################\n\n")
  # str_a <- paste("tar -zcf ", diretorios$folderResults, "/", dataset_name,
  #                "-", similarity, "-results-tbpma.tar.gz ",  
  #                diretorios$folderResults, sep = "")
  # print(system(str_a))
  
  # cat("\n####################################################################")
  # cat("\n# Copy to root folder                                              #")
  # cat("\n####################################################################\n\n")
  # str_b <- paste("cp -r ", diretorios$folderResults, "/", dataset_name,
  #                "-", similarity, "-results-tbpma.tar.gz ", 
  #                diretorios$folderRS, sep = "")
  # print(system(str_b))
 
} else if(parameters$Config$Implementation=="python"){
  
  cat("\n######################################################")
  cat("\n# LCC + ELCC: RUNNING PYTHON                         #")  
  cat("\n######################################################\n\n")
  source(file.path(parameters$Config$FolderScript, "run-python.R"))
  timeFinal <- system.time(results <- execute.run.python(parameters))
  
  
  cat("\n###################################################")
  cat("\n# LCC + ELCC: SAVE RUNTIME                        #")
  cat("\n###################################################\n\n")
  result_set <- t(data.matrix(timeFinal))
  setwd(parameters$Folders$folderResults)
  write.csv(result_set, "runtime-script.csv")
  
  
  system(paste0("rm -r ", parameters$Folders$folderPartitions))
  system(paste0("rm -r ", parameters$Folders$folderDatasets))
  system(paste0("rm -r ", parameters$Folders$folderResults, 
                "/", parameters$DatasetInfo$Name))
  
  
  cat("\n############################################################")
  cat("\n# LCC + ELCC: COMPRESS RESULTS                             #")
  cat("\n############################################################\n\n")
  base_dir <- parameters$Folders$folderResults
  subdirs <- list.dirs(base_dir, full.names = FALSE, recursive = FALSE)
  
  if (length(subdirs) == 0) {
    dirs_to_zip <- "."
  } else {
    dirs_to_zip <- paste(subdirs, collapse = " ")
  }
  
  if(parameters$Config$Number.Chains==1){
    output_tar <- file.path(base_dir, 
                            paste0(parameters$DatasetInfo$Name, 
                                   "-results-lcc.tar.gz"))
  } else {
    output_tar <- file.path(base_dir, 
                            paste0(parameters$DatasetInfo$Name, 
                                   "-results-elcc.tar.gz"))
  }
  
  str_01 <- paste("tar -zcvf", output_tar, "-C", base_dir, dirs_to_zip)
  res <- system(str_01)
  
  if (res != 0) {
    system(paste("rm -r", base_dir))
    print(res)
    stop("\n\n Something went wrong in compressing results files \n\n")
  } else {
    cat("\n✅ Compressão concluída com sucesso!\n")
  }
  
  
  cat("\n#########################################################")
  cat("\n# LCC + ELCC: COPY TO HOME                              #")
  cat("\n#########################################################\n\n")
  str0 = paste0(FolderRoot, "/Reports")
  if(dir.exists(str0)==FALSE){dir.create(str0)}
  
  if(parameters$Config$Number.Chains==1){
    str4 <- paste0(parameters$Folders$folderResults, "/",
                   parameters$DatasetInfo$Name, "-results-lcc.tar.gz")  
  } else {
    str4 <- paste0(parameters$Folders$folderResults, "/",
                   parameters$DatasetInfo$Name, "-results-elcc.tar.gz")  
  }
  
  str5 = paste("cp ", str4, " ", str0, sep="")
  res = system(str5)
  
  if(res!=0){
    system(paste("rm -r ", parameters$Folders$FolderResults, sep=""))
    print(res)
    stop("\n\n Something went wrong in compressing results files \n\n")
  }
  
  # destino = paste("nuvem:Clusters-Chains-HPMLs/",
  #                 parameters$Config$Implementation, "/", 
  #                 parameters$Config$Similarity, "/", 
  #                 parameters$Config$Dendrogram, "/", 
  #                 parameters$Config$Criteria, "/", 
  #                 parameters$Config$Dataset.Name, sep="")
  # comando1 = paste("rclone -P copy ", origem, " ", destino, sep="")
  # cat("\n", comando1, "\n")
  # a = print(system(comando1))
  # a = as.numeric(a)
  # if(a != 0) {
  #   stop("Erro RCLONE")
  #   quit("yes")
  # }
  # 
  
} else if(parameters$Config$Implementation=="mulan"){
  
  cat("\n\nRUNNING MULAN\n")  
  
} else {
  
  cat("\n\nRUNNING UTIML\n")  
  
}



cat("\n####################################################################")
cat("\n# DELETE                                                           #")
cat("\n####################################################################\n\n")
print(system(paste0("rm -r ", parameters$Config$Folder.Results)))
rm(list = ls())

gc()

cat("\n\n############################################################")
  cat("\n# END TEST BEST HYBRID PARTITION                           #")
  cat("\n############################################################\n\n") 
cat("\n\n\n\n") 


#############################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com              #
# Thank you very much!                                                      #
#############################################################################