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



##############################################################################
# FUNCTION BUILD AND TEST SELECTED HYBRID PARTITION                          #
#   Objective                                                                #
#   Parameters                                                               #
##############################################################################
build.python.silho <- function(parameters){
  
  # f = 1
  build.paralel.ecc <- foreach(f = 1:parameters$Config$Number.Folds) %dopar%{
  # while(f<=parameters$Config$Number.Folds){
    
    cat("\n\n\n#===================================================#")
    cat("\n# FOLD [", f, "]                                      #")
    cat("\n#====================================================#\n\n\n")
    
    cat("\n##########################################################")
    cat("\n# Load R Sources                                         #")
    cat("\n##########################################################\n\n")
    source(file.path(parameters$Config$FolderScript, "libraries.R"))
    source(file.path(parameters$Config$FolderScript, "utils.R"))
    
    #cat("\n##########################################################")
    #cat("\n# Loading Python Sources                                 #")
    #cat("\n##########################################################\n\n")
    #str = paste0("python ", parameters$Folders$folderPython, "/check_packages.py")
    #print(system(str))
    
    start <- proc.time()
    
    cat("\n##########################################################")
    cat("\n# Getting information about clusters                     #")
    cat("\n##########################################################\n\n")
    best.part.info = data.frame(parameters$All.Partitions$best.part.info)
    all.partitions.info = data.frame(parameters$All.Partitions$all.partitions.info )
    all.total.labels = data.frame(parameters$All.Partitions$all.total.labels)
    best.part.info.f = data.frame(filter(best.part.info, num.fold==f))
    all.total.labels.f = data.frame(filter(all.total.labels, num.fold==f))
    partition = data.frame(filter(all.partitions.info, num.fold==f))
    
    cat("\n##########################################################")
    cat("\n# Creating Folders from Best Partitions and Splits Tests #")
    cat("\n##########################################################\n\n")
    
    Folder.Best.Partition.Split = paste(parameters$Folders$folderPartitions, 
                                        "/", parameters$DatasetInfo$Name, 
                                        "/Split-", f, sep="")
    
    Folder.Tested.Split = paste(parameters$Folders$folderTested,
                                "/Split-", f, sep="")
    if(dir.create(Folder.Tested.Split)==FALSE){dir.create(Folder.Tested.Split)}
    
    Folder.BP = paste(parameters$Folders$folderPartitions, 
                      "/", parameters$DatasetInfo$Name, sep="")
    
    Folder.BPF = paste(Folder.BP, "/Split-", f, sep="")
    
    Folder.BPGP = paste(Folder.BPF, "/Partition-", 
                        best.part.info.f$num.part, 
                        sep="")
    
    cat("\n##########################################################")
    cat("\n# Opening TRAIN file                                     #")
    cat("\n##########################################################\n\n")
    train.name.file.csv = paste(parameters$Folders$folderCVTR, 
                                "/", parameters$DatasetInfo$Name, 
                                "-Split-Tr-", f, ".csv", sep="")
    train.file = data.frame(read.csv(train.name.file.csv))
    
    cat("\n##########################################################")
    cat("\n# Opening VALIDATION file                                #")
    cat("\n##########################################################\n\n")
    val.name.file.csv = paste(parameters$Folders$folderCVVL, 
                              "/", parameters$DatasetInfo$Name, 
                              "-Split-Vl-", f, ".csv", sep="")
    val.file = data.frame(read.csv(val.name.file.csv))
    
    
    cat("\n##########################################################")
    cat("\n# Opening TEST file                                      #")
    cat("\n##########################################################\n\n")
    test.name.file.csv = paste(parameters$Folders$folderCVTS,
                               "/", parameters$DatasetInfo$Name, 
                               "-Split-Ts-", f, ".csv", sep="")
    test.file = data.frame(read.csv(test.name.file.csv))
    
    cat("\n##########################################################")
    cat("\n Join Train and Validation                               #")
    cat("\n##########################################################\n\n")
    tv = rbind(train.file, val.file)
    
    cat("\n##########################################################")
    cat("\n# Partition                                              #")
    cat("\n##########################################################\n\n")
    partition.csv.name = paste(Folder.BPGP, 
                               "/partition-", best.part.info.f$num.part, 
                               ".csv", sep="")
    
    particoes = data.frame(read.csv(partition.csv.name))
    
    cat("\n##########################################################")
    cat("\n# Execute Python                                         #")
    cat("\n##########################################################\n\n")
    str.execute = paste("python3 ", parameters$Folders$folderPython,
                        "/main.py ",
                        train.name.file.csv, " ",
                        val.name.file.csv,  " ",
                        test.name.file.csv, " ",
                        partition.csv.name, " ",
                        Folder.Tested.Split, " ",
                        Number.Chains = parameters$Config$Number.Chains,
                        #fold = f,
                        sep="")
    
    res = print(system(str.execute))
    if(res!=0){
      break
    }
    
    tempo = data.matrix((proc.time() - start))
    tempo = data.frame(t(tempo))
    write.csv(tempo, paste(Folder.Tested.Split,
                           "/runtime-fold.csv", sep=""),
              row.names = FALSE)
    
    # f = f + 1
    gc()
    cat("\n\n\n\n\n")
  } # fim do for each
  
  gc()
  cat("\n##########################################################")
  cat("\n# TEST: Build and Test Hybrid Partitions END             #")
  cat("\n##########################################################\n\n")
}



##############################################################################
# FUNCTION EVALUATE TESTED HYBRID PARTITIONS                                 #
#   Objective                                                                #
#   Parameters                                                               #
##############################################################################
evaluate.python.silho <- function(parameters){
  
  # f = 1
  avalParal <- foreach(f = 1:parameters$Config$Number.Folds) %dopar%{
  # while(f<=parameters$Config$Number.Folds){
    
    cat("\n\n\n#===================================================#")
    cat("\n# FOLD [", f, "]                                      #")
    cat("\n#====================================================#\n\n\n")
    
    cat("\n##########################################################")
    cat("\n# Load R Sources                                         #")
    cat("\n##########################################################\n\n")
    source(file.path(parameters$Config$FolderScript, "libraries.R"))
    source(file.path(parameters$Config$FolderScript, "utils.R"))
    
    ########################################################################
    cat("\nObtendo informações dos clusters para construir os datasets")
    best.part.info = data.frame(parameters$All.Partitions$best.part.info)
    all.partitions.info = data.frame(parameters$All.Partitions$all.partitions.info )
    all.total.labels = data.frame(parameters$All.Partitions$all.total.labels)
    best.part.info.f = data.frame(filter(best.part.info, num.fold==f))
    all.total.labels.f = data.frame(filter(all.total.labels, num.fold==f))
    partition = data.frame(filter(all.partitions.info, num.fold==f))
    
    ##########################################################################
    # "/dev/shm/ej3-GpositiveGO/Tested/Split-1"
    Folder.Tested.Split = paste(parameters$Folders$folderTested,
                                "/Split-", f, sep="")
    
    
    ##########################################################################
    train.file.name = paste(parameters$Folders$folderCVTR, "/", 
                            parameters$Config$Dataset.Name, 
                            "-Split-Tr-", f , ".csv", sep="")
    
    test.file.name = paste(parameters$Folders$folderCVTS, "/",
                           parameters$Config$Dataset.Name, 
                           "-Split-Ts-", f, ".csv", sep="")
    
    val.file.name = paste(parameters$Folders$folderCVVL, "/", 
                          parameters$Config$Dataset.Name, 
                          "-Split-Vl-", f , ".csv", sep="")
    
    ##########################################################################
    train = data.frame(read.csv(train.file.name))
    test = data.frame(read.csv(test.file.name))
    val = data.frame(read.csv(val.file.name))
    tv = rbind(train, val)
    
    ##########################################################################
    labels.indices = seq(parameters$DatasetInfo$LabelStart, 
                         parameters$DatasetInfo$LabelEnd, by=1)
    
    ##########################################################################
    mldr.treino = mldr_from_dataframe(train, labelIndices = labels.indices)
    mldr.teste = mldr_from_dataframe(test, labelIndices = labels.indices)
    mldr.val = mldr_from_dataframe(val, labelIndices = labels.indices)
    mldr.tv = mldr_from_dataframe(tv, labelIndices = labels.indices)
    
    ###################################################################
    #cat("\nGet the true and predict lables")
    setwd(Folder.Tested.Split)
    y_true = data.frame(read.csv("y_true.csv"))
    y_proba = data.frame(read.csv("y_proba.csv"))
    
    
    ####################################################################################
    y.true.2 = data.frame(sapply(y_true, function(x) as.numeric(as.character(x))))
    y.true.3 = mldr_from_dataframe(y.true.2, 
                                   labelIndices = seq(1,ncol(y.true.2)), 
                                   name = "y.true.2")
    y_proba = sapply(y_proba, function(x) as.numeric(as.character(x)))
    
    
    ########################################################################
    y_threshold_05 <- data.frame(as.matrix(fixed_threshold(y_proba,
                                                           threshold = 0.5)))
    write.csv(y_threshold_05, 
              paste(Folder.Tested.Split, "/y_pred_thr05.csv", sep=""),
              row.names = FALSE)
    
    ########################################################################
    y_threshold_card = lcard_threshold(as.matrix(y_proba), 
                                       mldr.tv$measures$cardinality,
                                       probability = F)
    write.csv(y_threshold_card, 
              paste(Folder.Tested.Split, "/y_pred_thrLC.csv", sep=""),
              row.names = FALSE)
    
    ##########################################################################    
    avaliacao(f = f, y_true = y.true.3, y_pred = y_proba,
              salva = Folder.Tested.Split, nome = "results-utiml")
    
    #avaliacao(f = f, y_true = y.true.3, y_pred = y_threshold_05,
    #          salva = Folder.Tested.Split, nome = "thr-05")
    
    #avaliacao(f = f, y_true = y.true.3, y_pred = y_threshold_card,
    #          salva = Folder.Tested.Split, nome = "thr-lc")
    
    #####################################################################
    #y_threshold_card = data.frame(as.matrix(y_threshold_card))
    
    #####################################################################
    #nome.true = paste(Folder.Tested.Split, "/y_true.csv", sep="")
    #nome.pred.proba = paste(Folder.Tested.Split, "/y_proba.csv", sep="")
    #nome.thr.05 = paste(Folder.Tested.Split, "/y_pred_thr05.csv", sep="")
    #nome.thr.LC = paste(Folder.Tested.Split, "/y_pred_thrLC.csv", sep="")
    
    #save.pred.proba = paste(Folder.Tested.Split, "/pred-proba-auprc.csv", sep="")
    #save.thr05 = paste(Folder.Tested.Split, "/thr-05-auprc.csv", sep="")
    #save.thrLC = paste(Folder.Tested.Split, "/thr-lc-auprc.csv", sep="")
    
    #################################################################
    # str.execute = paste("python3 ",
    #                     parameters$Folders$folderUtils,
    #                     "/Python/auprc.py ",
    #                     nome.true, " ",
    #                     nome.pred.proba, " ",
    #                     save.pred.proba, " ",
    #                     sep="")
    
    # str.execute = paste("/home/cissagatto/Documentos/myenv/bin/python ", 
    #                     parameters$Folders$folderUtils,
    #                     "/Python/auprc.py ",
    #                     nome.true, " ",
    #                     nome.pred.proba, " ",
    #                     save.pred.proba, " ",
    #                     sep="")
    
    # str.execute = paste("~/miniforge3/envs/py310/bin/python", 
    #                     parameters$Folders$folderUtils,
    #                     "/Python/auprc.py ",
    #                     nome.true, " ",
    #                     nome.pred.proba, " ",
    #                     save.pred.proba, " ",
    #                     sep="")
    
    # res = print(system(str.execute))
    # if(res!=0){
    #   break
    # }
    
    ################################################################
    # str.execute = paste("python3 ",
    #                     parameters$Folders$folderUtils,
    #                     "/Python/auprc.py ",
    #                     nome.true, " ",
    #                     nome.thr.05, " ",
    #                     save.thr05, " ",
    #                     sep="")
    
    # str.execute = paste("/home/cissagatto/Documentos/myenv/bin/python ", 
    #                     parameters$Folders$folderUtils,
    #                     "/Python/auprc.py ",
    #                     nome.true, " ",
    #                     nome.thr.05, " ",
    #                     save.thr05, " ",
    #                     sep="")
    
    # str.execute = paste("~/miniforge3/envs/py310/bin/python", 
    #                     parameters$Folders$folderUtils,
    #                     "/Python/auprc.py ",
    #                     nome.true, " ",
    #                     nome.thr.05, " ",
    #                     save.thr05, " ",
    #                     sep="")
    
    
    # res = print(system(str.execute))
    # if(res!=0){
    #   break
    # }
    
    #################################################################
    # str.execute = paste("python3 ",
    #                     parameters$Folders$folderUtils,
    #                     "/Python/auprc.py ",
    #                     nome.true, " ",
    #                     nome.thr.LC, " ",
    #                     save.thrLC, " ",
    #                     sep="")
    
    # str.execute = paste("/home/cissagatto/Documentos/myenv/bin/python ", 
    #                     parameters$Folders$folderUtils,
    #                     "/Python/auprc.py ",
    #                     nome.true, " ",
    #                     nome.thr.LC, " ",
    #                     save.thrLC, " ",
    #                     sep="")
    
    # str.execute = paste("~/miniforge3/envs/py310/bin/python", 
    #                     parameters$Folders$folderUtils,
    #                     "/Python/auprc.py ",
    #                     nome.true, " ",
    #                     nome.thr.LC, " ",
    #                     save.thrLC, " ",
    #                     sep="")
    
    # str.execute = paste("~/miniforge3/envs/py310/bin/python", 
    #                     parameters$Folders$folderUtils,
    #                     "/Python/auprc.py ",
    #                     nome.true, " ",
    #                     nome.thr.LC, " ",
    #                     save.thrLC, " ",
    #                     sep="")
    
    # system(str.execute)
    # 
    # 
    # res = print(system(str.execute))
    # if(res!=0){
    #   break
    # }
    
    ####################################################
    # names = paste(parameters$Config$NamesLabels, "-proba", sep="")
    # y_proba = data.frame(y_proba)
    # names(y_proba) = names
    # rm(names)
    # 
    # names  = paste(parameters$Config$NamesLabels, "-true", sep="")
    # true = data.frame(y_true)
    # names(y_true) = names 
    # rm(names)
    # 
    # names  = paste(parameters$Config$NamesLabels, "-thr-05", sep="")
    # y_threshold_05 = data.frame(y_threshold_05)
    # names(y_threshold_05) = names 
    # rm(names)
    # 
    # names  = paste(parameters$Config$NamesLabels, "-thr-lc", sep="")
    # y_threshold_card = data.frame(as.matrix(y_threshold_card))
    # names(y_threshold_card) = names 
    # rm(names)
    # 
    # all.predictions = cbind(y_true, y_proba,
    #                         y_threshold_05, y_threshold_card)
    # write.csv(all.predictions, 
    #           paste(Folder.Tested.Split, "/folder-predictions.csv", sep=""), 
    #           row.names = FALSE)
    
    
    ##############################################
    # matrix.confusao(true = y_true, pred = y_threshold_05, 
    #                 type = "thr-05", salva = Folder.Tested.Split, 
    #                 nomes.rotulos = parameters$Names.Labels$Labels)
    # 
    # matrix.confusao(true = y_true, pred = y_threshold_card, 
    #                 type = "thr-lc", salva = Folder.Tested.Split, 
    #                 nomes.rotulos = parameters$Names.Labels$Labels)
    # 
    
    #########################################################################    
    #roc.curva(f = f, y_pred = y_proba, test = mldr.teste,
    #          Folder = Folder.Tested.Split, nome = "utiml")
    
    # roc.curva(f = f, y_pred = y_threshold_card, test = mldr.teste,
    #           Folder = Folder.Tested.Split, nome = "thr-lc")
    # 
    # roc.curva(f = f, y_pred = y_threshold_05, test = mldr.teste,
    #           Folder = Folder.Tested.Split, nome = "thr-05")
    
    # f = f + 1
    gc()
  } # fim do for each
  
  system(paste0("rm -r ", parameters$Folders$folderDatasets))
  system(paste0("rm -r ", parameters$Folders$folderPartitions))
  
  gc()
  cat("\n###################################################")
  cat("\n# TEST: Evaluation Folds END                      #")
  cat("\n###################################################")
  cat("\n\n")
}





###########################################################################
#
###########################################################################
gather.eval.python.silho <- function(parameters){
  
  Measures <- c(
    "auprc_macro",
    "auprc_micro",
    "auprc_weighted",
    "auprc_samples",
    "roc_auc_macro",
    "roc_auc_micro",
    "roc_auc_weighted",
    "roc_auc_samples",
    "accuracy",
    "average-precision",
    "clp",
    "coverage",
    "F1",
    "hamming-loss",
    "macro-AUC",
    "macro-F1",
    "macro-precision",
    "macro-recall",
    "margin-loss",
    "micro-AUC",
    "micro-F1",
    "micro-precision",
    "micro-recall",
    "mlp",
    "one-error",
    "precision",
    "ranking-loss",
    "recall",
    "subset-accuracy",
    "wlp"
  )
  
  all.ignored.classes <- data.frame(
    Measure = c(
      "auprc_macro",
      "auprc_micro",
      "auprc_weighted",
      "auprc_samples",
      "roc_auc_macro",
      "roc_auc_micro",
      "roc_auc_weighted",
      "roc_auc_samples"
    ),
    Ignored_Classes = I(rep(list(character(0)), 8)),
    stringsAsFactors = FALSE
  )
  
  all.model.size.from.memory <- data.frame(
    fold = numeric(0),
    size_bytes = numeric(0)
  )
  
  all.model.size.from.model <- data.frame(
    fold = numeric(0),
    chain_index = integer(0),
    chain_model_size = numeric(0),
    total_model_size = numeric(0)
  )
  
  all.model.size.from.model10 <- data.frame(
    fold = numeric(0),
    chain_model_size = numeric(0),
    total_model_size = numeric(0)
  )
  
  all.results.python <- data.frame(
    Measure = character(0),
    Value = numeric(0)
  )
  
  all.results.utiml <- data.frame()
  
  all.performance <- data.frame(
    Measures = rep(NA_character_, 30),
    Values = rep(NA_real_, 30)
  )
  
  all.runtime.fold <- data.frame(
    fold = numeric(0),
    user.self = numeric(0),
    sys.self = numeric(0),
    elapsed = numeric(0),
    user.child = numeric(0),
    sys.child = numeric(0)
  )
  
  all.train.test.from.model <- data.frame(
    fold = numeric(0),
    chain_index = numeric(0),
    train_time = numeric(0),
    train_time_total = numeric(0),
    test_time = numeric(0)
  )
  
  all.train.test.from.model10 <- data.frame(
    fold = numeric(0),
    train_time = numeric(0),
    train_time_total = numeric(0),
    test_time = numeric(0)
  )
  
  all.train.test.from.time <- data.frame(
    fold = numeric(0),
    train = numeric(0),
    test = numeric(0)
  )
  
  
  if(parameters$Config$Number.Chains == 1){
    
    cat("\n ONLY ONE CHAIN")
    
    f = 1
    while(f<=parameters$Config$Number.Folds){
      
      cat("\nFold: ", f)
      
      #########################################################################
      folderSplit = paste(parameters$Folders$folderTested,
                          "/Split-", f, sep="")
      
      #########################################################################
      ignored.classes = data.frame(read.csv(
        paste(folderSplit, "/ignored-classes.csv", sep="")))
      colnames(ignored.classes)[2] = paste0("fold-",f)
      all.ignored.classes = cbind(all.ignored.classes, ignored.classes[2])
      
      
      #########################################################################
      model.size.from.memory = data.frame(read.csv(
        paste(folderSplit, "/model-size-from-memory.csv", sep="")))
      model.size.from.memory = data.frame(fold=f, model.size.from.memory )
      all.model.size.from.memory = rbind(all.model.size.from.memory, model.size.from.memory)
      
      
      #########################################################################
      model.size.from.model = data.frame(read.csv(
        paste(folderSplit, "/model-size-from-model.csv", sep="")))
      model.size.from.model = data.frame(fold=f, model.size.from.model)
      all.model.size.from.model = rbind(all.model.size.from.model, model.size.from.model)
      
      
      #########################################################################
      results.python = data.frame(read.csv(
        paste(folderSplit, "/results-python.csv", sep="")))
      names(results.python) = c("Measures", paste0("Fold",f))
      
      results.utiml = data.frame(read.csv(paste(
        folderSplit, "/results-utiml.csv", sep="")))
      names(results.utiml) = c("Measures", paste0("Fold",f))
      
      resultados = rbind(results.python, results.utiml)
      all.performance = cbind(all.performance, resultados)
      
      
      #########################################################################
      runtime.fold = data.frame(read.csv(paste(
        folderSplit, "/runtime-fold.csv", sep="")))
      runtime.fold = data.frame(fold=f, runtime.fold)
      all.runtime.fold = rbind(all.runtime.fold, runtime.fold)
      
      
      #########################################################################
      train.test.from.model = data.frame(read.csv(paste(
        folderSplit, "/train-test-from-model.csv", sep="")))
      train.test.from.model = data.frame(fold=f, train.test.from.model)
      all.train.test.from.model = rbind(all.train.test.from.model, train.test.from.model)
      
      
      #########################################################################
      train.test.from.time = data.frame(read.csv(paste(
        folderSplit, "/train-test-from-time.csv", sep="")))
      train.test.from.time = data.frame(fold=f, train.test.from.time)
      colnames(train.test.from.time) = c("fold", "train", "test")
      all.train.test.from.time = rbind(all.train.test.from.time, train.test.from.time)
      
      
      #################################
      #system(paste0("rm -r ", folderSplit, "/results-python.csv", sep=""))
      #system(paste0("rm -r ", folderSplit, "/results-utiml.csv", sep=""))
      #system(paste0("rm -r ", folderSplit, "/model-size.csv", sep=""))
      #system(paste0("rm -r ", folderSplit, "/runtime-python-2.csv", sep=""))
      #system(paste0("rm -r ", folderSplit, "/runtime-python.csv", sep=""))
      #system(paste0("rm -r ", folderSplit, "/runtime-fold.csv", sep=""))
      
      f = f + 1
      gc()
    } # fim do while
    
    name = file.path(parameters$Folders$folderTested, "ignored-classes.csv")
    all.ignored.classes = all.ignored.classes[,-2]
    write.csv(all.ignored.classes, name, row.names = FALSE)
    
    name = file.path(parameters$Folders$folderTested, "model-size-from-memory.csv")
    write.csv(all.model.size.from.memory, name, row.names = FALSE)
    
    name = file.path(parameters$Folders$folderTested, "model-size-from-model.csv")
    all.model.size.from.model = all.model.size.from.model[,-2]
    write.csv(all.model.size.from.model, name, row.names = FALSE)
    
    name = file.path(parameters$Folders$folderTested, "performance.csv")
    all.performance = all.performance[, !duplicated(colnames(all.performance))]
    all.performance = all.performance[,c(-1,-2)]
    all.performance = data.frame(Measures, all.performance)
    write.csv(all.performance, name, row.names = FALSE)
    
    name = file.path(parameters$Folders$folderTested, "runtime-fold.csv")
    write.csv(all.runtime.fold, name, row.names = FALSE)
    
    name = file.path(parameters$Folders$folderTested, "train-test-from-model.csv")
    all.train.test.from.model = all.train.test.from.model[,-2]
    write.csv(all.train.test.from.model, name, row.names = FALSE)
    
    name = file.path(parameters$Folders$folderTested, "train-test-from-time.csv")
    write.csv(all.train.test.from.time, name, row.names = FALSE)
    
  } else { # fim do if 
    
    cat("\n MORE THAN ONE CHAIN")
    
    f = 1
    while(f<=parameters$Config$Number.Folds){
      
      cat("\nFold: ", f)
      
      #########################################################################
      folderSplit = paste(parameters$Folders$folderTested,
                          "/Split-", f, sep="")
      
      #########################################################################
      ignored.classes = data.frame(read.csv(
        paste(folderSplit, "/ignored-classes.csv", sep="")))
      colnames(ignored.classes)[2] = paste0("fold-",f)
      all.ignored.classes = cbind(all.ignored.classes, ignored.classes[2])
      
      
      #########################################################################
      model.size.from.memory = data.frame(read.csv(
        paste(folderSplit, "/model-size-from-memory.csv", sep="")))
      model.size.from.memory = data.frame(fold=f, model.size.from.memory )
      all.model.size.from.memory = rbind(all.model.size.from.memory, model.size.from.memory)
      
      
      #########################################################################
      model.size.from.model = data.frame(read.csv(
        paste(folderSplit, "/model-size-from-model.csv", sep="")))
      model.size.from.model = data.frame(fold=f, model.size.from.model)
      res = data.frame(t(apply(model.size.from.model,2,mean)))
      res = res[,-2]
      all.model.size.from.model10 = rbind(all.model.size.from.model10, res)
      
      
      #########################################################################
      results.python = data.frame(read.csv(
        paste(folderSplit, "/results-python.csv", sep="")))
      names(results.python) = c("Measures", paste0("Fold",f))
      
      results.utiml = data.frame(read.csv(paste(
        folderSplit, "/results-utiml.csv", sep="")))
      names(results.utiml) = c("Measures", paste0("Fold",f))
      
      resultados = rbind(results.python, results.utiml)
      all.performance = cbind(all.performance, resultados)
      
      
      #########################################################################
      runtime.fold = data.frame(read.csv(paste(
        folderSplit, "/runtime-fold.csv", sep="")))
      runtime.fold = data.frame(fold=f, runtime.fold)
      all.runtime.fold = rbind(all.runtime.fold, runtime.fold)
      
      
      #########################################################################
      train.test.from.model = data.frame(read.csv(paste(
        folderSplit, "/train-test-from-model.csv", sep="")))
      train.test.from.model = data.frame(fold=f, train.test.from.model)
      res = data.frame(t(apply(train.test.from.model,2,mean)))
      res = res[,-2]
      all.train.test.from.model10 = rbind(all.train.test.from.model10, res)
      
      
      #########################################################################
      train.test.from.time = data.frame(read.csv(paste(
        folderSplit, "/train-test-from-time.csv", sep="")))
      train.test.from.time = data.frame(fold=f, train.test.from.time)
      colnames(train.test.from.time) = c("fold", "train", "test")
      all.train.test.from.time = rbind(all.train.test.from.time, train.test.from.time)
      
      
      #################################
      #system(paste0("rm -r ", folderSplit, "/results-python.csv", sep=""))
      #system(paste0("rm -r ", folderSplit, "/results-utiml.csv", sep=""))
      #system(paste0("rm -r ", folderSplit, "/model-size.csv", sep=""))
      #system(paste0("rm -r ", folderSplit, "/runtime-python-2.csv", sep=""))
      #system(paste0("rm -r ", folderSplit, "/runtime-python.csv", sep=""))
      #system(paste0("rm -r ", folderSplit, "/runtime-fold.csv", sep=""))
      
      f = f + 1
      gc()
    } # fim do while
    
    name = file.path(parameters$Folders$folderTested, "ignored-classes.csv")
    all.ignored.classes = all.ignored.classes[,-2]
    write.csv(all.ignored.classes, name, row.names = FALSE)
    
    name = file.path(parameters$Folders$folderTested, "model-size-from-memory.csv")
    write.csv(all.model.size.from.memory, name, row.names = FALSE)
    
    name = file.path(parameters$Folders$folderTested, "model-size-from-model.csv")
    all.model.size.from.model = all.model.size.from.model[,-2]
    write.csv(all.model.size.from.model10, name, row.names = FALSE)
    
    name = file.path(parameters$Folders$folderTested, "performance.csv")
    all.performance = all.performance[, !duplicated(colnames(all.performance))]
    all.performance = all.performance[,c(-1,-2)]
    all.performance = data.frame(Measures, all.performance)
    write.csv(all.performance, name, row.names = FALSE)
    
    name = file.path(parameters$Folders$folderTested, "runtime-fold.csv")
    write.csv(all.runtime.fold, name, row.names = FALSE)
    
    name = file.path(parameters$Folders$folderTested, "train-test-from-model.csv")
    all.train.test.from.model = all.train.test.from.model[,-2]
    write.csv(all.train.test.from.model10, name, row.names = FALSE)
    
    name = file.path(parameters$Folders$folderTested, "train-test-from-time.csv")
    write.csv(all.train.test.from.time, name, row.names = FALSE)
    
  }
  
  
  gc()
  cat("\n########################################################")
  cat("\n# END EVALUATED                                        #") 
  cat("\n########################################################")
  cat("\n\n\n\n")
} # fim da função



#########################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com
# Thank you very much!
#########################################################################
