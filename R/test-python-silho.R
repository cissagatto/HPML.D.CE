##############################################################################
# Clusters CHAINS HPML                                                         #
# Copyright (C) 2023                                                         #
#                                                                            #
# This code is free software: you can redistribute it and/or modify it under #
# the terms of the GNU General Public License as published by the Free       #
# Software Foundation, either version 3 of the License, or (at your option)  #
# any later version. This code is distributed in the hope that it will be    #
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of     #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General   #
# Public License for more details.                                           #
#                                                                            #
# 1 - PhD Elaine Cecilia Gatto | Prof PhD Ricardo Cerri                      #
# 2 - Prof PhD Mauri Ferrandin                                               #
# 3 - Prof PhD Celine Vens | PhD Felipe Nakano Kenji                         #
# 4 - Prof PhD Jesse Read                                                    #
#                                                                            #
# 1 = Federal University of São Carlos - UFSCar - https://www2.ufscar.br     #
# Campus São Carlos | Computer Department - DC - https://site.dc.ufscar.br | #
# Post Graduate Program in Computer Science - PPGCC                          # 
# http://ppgcc.dc.ufscar.br | Bioinformatics and Machine Learning Group      #
# BIOMAL - http://www.biomal.ufscar.br                                       # 
#                                                                            #
# 2 - Federal University of Santa Catarina Campus Blumenau - UFSC            #
# https://ufsc.br/                                                           #
#                                                                            #
# 3 - Katholieke Universiteit Leuven Campus Kulak Kortrijk Belgium           #
# Medicine Department - https://kulak.kuleuven.be/                           #
# https://kulak.kuleuven.be/nl/over_kulak/faculteiten/geneeskunde            #
#                                                                            #
# 4 - Ecole Polytechnique | Institut Polytechnique de Paris | 1 rue Honoré   #
# d’Estienne d’Orves - 91120 - Palaiseau - FRANCE                            #
#                                                                            #
##############################################################################



###############################################################################
# SET WORKSAPCE                                                               #
###############################################################################
FolderRoot = "/Clusters-Chains-HPML"
FolderScripts = "/Clusters-Chains-HPML/R"



##############################################################################
# FUNCTION BUILD AND TEST SELECTED HYBRID PARTITION                          #
#   Objective                                                                #
#   Parameters                                                               #
##############################################################################
build.python.silho <- function(parameters){
  
  parameters = parameters
  
  f = 1
  build.paralel.ecc <- foreach(f = 1:parameters$Config$Number.Folds) %dopar%{
  # while(f<=parameters$Config$Number.Folds){
    
    
    cat("\n\n\n#===================================================#")
    cat("\n# FOLD [", f, "]                                      #")
    cat("\n#====================================================#\n\n\n")
    
    
    ########################################################################
    cat("\nWorkSpace")
    FolderRoot = "~/Clusters-Chains-HPML"
    FolderScripts = "~/Clusters-Chains-HPML/R"
    
    
    ########################################################################
    cat("\nLoad Scripts")
    setwd(FolderScripts)
    source("utils.R")
    
    setwd(FolderScripts)
    source("libraries.R")
    
    
    ########################################################################
    cat("\nGetting information about clusters")
    best.part.info = data.frame(parameters$All.Partitions$best.part.info)
    all.partitions.info = data.frame(parameters$All.Partitions$all.partitions.info )
    all.total.labels = data.frame(parameters$All.Partitions$all.total.labels)
    
    best.part.info.f = data.frame(filter(best.part.info, num.fold==f))
    all.total.labels.f = data.frame(filter(all.total.labels, num.fold==f))
    # build.datasets.f = data.frame(filter(parameters$Labels.Attr$all.info, num.fold==f))
    
    # partição específica
    partition = data.frame(filter(all.partitions.info, num.fold==f))
    
    ##########################################################################
    cat("\nCreating Folders from Best Partitions and Splits Tests")
    
    Folder.Best.Partition.Split = paste(parameters$Folders$folderPartitions, 
                                        "/", parameters$DatasetInfo$Name, 
                                        "/Split-", f, sep="")
    
    Folder.Tested.Split = paste(parameters$Folders$folderTested,
                                "/Split-", f, sep="")
    if(dir.create(Folder.Tested.Split)==FALSE){dir.create(Folder.Tested.Split)}
    
    Folder.BP = paste(parameters$Folders$folderPartitions, 
                      "/", parameters$DatasetInfo$Name, sep="")
    
    Folder.BPF = paste(Folder.BP, "/Split-", f, sep="")
    
    Folder.BPGP = paste(Folder.BPF, "/Partition-", best.part.info.f$num.part, 
                        sep="")
    
    ########################################################################
    cat("\nOpening TRAIN file")
    train.name.file.csv = paste(parameters$Folders$folderCVTR, 
                                "/", parameters$DatasetInfo$Name, 
                                "-Split-Tr-", f, ".csv", sep="")
    train.file = data.frame(read.csv(train.name.file.csv))
    
    
    #####################################################################
    cat("\nOpening VALIDATION file")
    val.name.file.csv = paste(parameters$Folders$folderCVVL, 
                              "/", parameters$DatasetInfo$Name, 
                              "-Split-Vl-", f, ".csv", sep="")
    val.file = data.frame(read.csv(val.name.file.csv))
    
    
    ########################################################################
    cat("\nOpening TEST file")
    test.name.file.csv = paste(parameters$Folders$folderCVTS,
                               "/", parameters$DatasetInfo$Name, 
                               "-Split-Ts-", f, ".csv", sep="")
    test.file = data.frame(read.csv(test.name.file.csv))
    
    ########################################################################
    cat("\nJoint Train and Validation")
    tv = rbind(train.file, val.file)
    
    ####################
    partition.csv.name = paste(Folder.BPGP, 
                               "/partition-", best.part.info.f$num.part, 
                               ".csv", sep="")
    
    particoes = data.frame(read.csv(partition.csv.name))
    
    ##################################################################
    # EXECUTE ECC PYTHON
    str.execute = paste("python3 ", parameters$Folders$folderUtils, 
                        "/Python/main.py ", 
                        train.name.file.csv, " ",
                        val.name.file.csv,  " ",
                        test.name.file.csv, " ", 
                        partition.csv.name, " ", 
                        Folder.Tested.Split, 
                        sep="")
    
    # EXECUTA
    start <- proc.time()
    res = print(system(str.execute))
    tempo = data.matrix((proc.time() - start))
    tempo = data.frame(t(tempo))
    write.csv(tempo, paste(Folder.Tested.Split, "/runtime-fold.csv", 
                           sep=""))
    
    if(res!=0){
      break
    }
    
  
  # f = f + 1
  gc()
  cat("\n")
} # fim do for each

gc()
cat("\n##################################################")
cat("\n# TEST: Build and Test Hybrid Partitions End     #")
cat("\n##################################################")
cat("\n\n\n\n")
}



##############################################################################
# FUNCTION EVALUATE TESTED HYBRID PARTITIONS                                 #
#   Objective                                                                #
#   Parameters                                                               #
##############################################################################
evaluate.python.silho <- function(parameters){
  
  f = 1
  avalParal <- foreach(f = 1:parameters$Config$Number.Folds) %dopar%{
    # while(f<=parameters$Config$Number.Folds){
    
    cat("\n\n\n#======================================================")
    cat("\n# Fold: ", f)
    cat("\n#======================================================\n\n\n")
    
    ########################################################################
    cat("\nWorkSpace")
    FolderRoot = "~/Clusters-Chains-HPML"
    FolderScripts = "~/Clusters-Chains-HPML/R"
    
    
    ########################################################################
    cat("\nLoad Scripts")
    setwd(FolderScripts)
    source("utils.R")
    
    setwd(FolderScripts)
    source("libraries.R")
    
    
    ########################################################################
    cat("\nObtendo informações dos clusters para construir os datasets")
    best.part.info = data.frame(parameters$All.Partitions$best.part.info)
    all.partitions.info = data.frame(parameters$All.Partitions$all.partitions.info )
    all.total.labels = data.frame(parameters$All.Partitions$all.total.labels)
    
    best.part.info.f = data.frame(filter(best.part.info, num.fold==f))
    all.total.labels.f = data.frame(filter(all.total.labels, num.fold==f))
    # build.datasets.f = data.frame(filter(parameters$Labels.Attr$all.info, num.fold==f))
    
    # partição específica
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
              salva = Folder.Tested.Split, nome = "pred-proba")
    
    avaliacao(f = f, y_true = y.true.3, y_pred = y_threshold_05,
              salva = Folder.Tested.Split, nome = "thr-05")
    
    avaliacao(f = f, y_true = y.true.3, y_pred = y_threshold_card,
              salva = Folder.Tested.Split, nome = "thr-lc")
    
    #####################################################################
    
    y_threshold_card = data.frame(as.matrix(y_threshold_card))
    
    #####################################################################
    nome.true = paste(Folder.Tested.Split, "/y_true.csv", sep="")
    nome.pred.proba = paste(Folder.Tested.Split, "/y_proba.csv", sep="")
    nome.thr.05 = paste(Folder.Tested.Split, "/y_pred_thr05.csv", sep="")
    nome.thr.LC = paste(Folder.Tested.Split, "/y_pred_thrLC.csv", sep="")
    
    save.pred.proba = paste(Folder.Tested.Split, "/pred-proba-auprc.csv", sep="")
    save.thr05 = paste(Folder.Tested.Split, "/thr-05-auprc.csv", sep="")
    save.thrLC = paste(Folder.Tested.Split, "/thr-lc-auprc.csv", sep="")
    
    #################################################################
    str.execute = paste("python3 ", parameters$Folders$folderUtils,
                        "/Python/auprc.py ",
                        nome.true, " ",
                        nome.pred.proba, " ",
                        save.pred.proba, " ",
                        sep="")
    res = print(system(str.execute))
    if(res!=0){
      break
    }
    
    #################################################################
    str.execute = paste("python3 ",  parameters$Folders$folderUtils,
                        "/Python/auprc.py ",
                        nome.true, " ",
                        nome.thr.05, " ",
                        save.thr05, " ",
                        sep="")
    res = print(system(str.execute))
    if(res!=0){
      break
    }
    
    #################################################################
    str.execute = paste("python3 ",  parameters$Folders$folderUtils,
                        "/Python/auprc.py ",
                        nome.true, " ",
                        nome.thr.LC, " ",
                        save.thrLC, " ",
                        sep="")
    res = print(system(str.execute))
    if(res!=0){
      break
    }
    
    ####################################################
    names = paste(parameters$Config$NamesLabels, "-proba", sep="")
    y_proba = data.frame(y_proba)
    names(y_proba) = names
    rm(names)
    
    names  = paste(parameters$Config$NamesLabels, "-true", sep="")
    true = data.frame(y_true)
    names(y_true) = names 
    rm(names)
    
    names  = paste(parameters$Config$NamesLabels, "-thr-05", sep="")
    y_threshold_05 = data.frame(y_threshold_05)
    names(y_threshold_05) = names 
    rm(names)
    
    names  = paste(parameters$Config$NamesLabels, "-thr-lc", sep="")
    y_threshold_card = data.frame(as.matrix(y_threshold_card))
    names(y_threshold_card) = names 
    rm(names)
    
    all.predictions = cbind(y_true, y_proba,
                            y_threshold_05, y_threshold_card)
    write.csv(all.predictions, 
              paste(Folder.Tested.Split, "/folder-predictions.csv", sep=""), 
              row.names = FALSE)
    
    
    ##############################################
    matrix.confusao(true = y_true, pred = y_threshold_05, 
                    type = "thr-05", salva = Folder.Tested.Split, 
                    nomes.rotulos = parameters$Names.Labels$Labels)
    
    matrix.confusao(true = y_true, pred = y_threshold_card, 
                    type = "thr-lc", salva = Folder.Tested.Split, 
                    nomes.rotulos = parameters$Names.Labels$Labels)
    
    
    #########################################################################    
    roc.curva(f = f, y_pred = y_pred_proba, test = mldr.teste,
              Folder = Folder.Tested.Split, nome = "pred-proba")
    
    roc.curva(f = f, y_pred = y_threshold_card, test = mldr.teste,
              Folder = Folder.Tested.Split, nome = "thr-lc")
    
    roc.curva(f = f, y_pred = y_threshold_05, test = mldr.teste,
              Folder = Folder.Tested.Split, nome = "thr-05")
    
    
    
    # f = f + 1
    gc()
  } # fim do for each
  
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
  
  measures = c("accuracy", "average-precision", "clp", "coverage",
               "F1", "hamming-loss", "macro-AUC", "macro-F1", 
               "macro-precision", "macro-recall", "margin-loss", 
               "micro-AUC","micro-F1", "micro-precision",
               "micro-recall", "mlp", "one-error", "precision", 
               "ranking-loss", "recall", "subset-accuracy", "wlp")
  
  folds = c(0)
  
  nomes.preds = c("pred-proba", "thr-05", "thr-lc")
  
  i = 1
  while(i<=length(nomes.preds)){
    
    cat("\n\npredicao: ", i)
    
    final.roc.auc = data.frame()
    final.roc.auc.micro = data.frame()
    final.roc.auc.macro = data.frame()
    
    final.auprc.macro = data.frame(fold = c(0), value=c(0))
    final.auprc.micro = data.frame(fold = c(0), value=c(0))
    
    final.runtime = data.frame()
    final.conf.mat = data.frame(measures)
    
    
    f = 1
    while(f<=parameters$Config$Number.Folds){
      
      cat("\nFold: ", f)
      
      #########################################################################
      folderSplit = paste(parameters$Folders$folderTested,
                          "/Split-", f, sep="")
      
      #########################################################################
      confMat = data.frame(read.csv(paste(folderSplit, "/", nomes.preds[i], 
                                          "-evaluated.csv", sep="")))
      names(confMat) = c("Measures", "Fold")
      
      #########################################################################
      confMat[is.na(confMat)] <- 0
      
      #########################################################################
      final.conf.mat = cbind(final.conf.mat, confMat$Fold) 
      folds[f] = paste("Fold-", f, sep="")
      
      #########################################################################
      roc.auc = data.frame(read.csv(paste(folderSplit, "/", nomes.preds[i], 
                                          "-roc-auc.csv", sep="")))       
      final.roc.auc = rbind(final.roc.auc, roc.auc)
      
      #########################################################################
      roc.micro.auc = data.frame(read.csv(paste(folderSplit, "/", nomes.preds[i], 
                                                "-roc-auc-micro.csv", sep="")))       
      final.roc.auc.micro = rbind(final.roc.auc.micro, roc.micro.auc)
      
      #########################################################################
      roc.macro.auc = data.frame(read.csv(paste(folderSplit, "/", nomes.preds[i], 
                                                "-roc-auc-macro.csv", sep="")))       
      final.roc.auc.macro = rbind(final.roc.auc.macro, roc.macro.auc)
      
      #########################################################################
      auprc = data.frame(read.csv(paste(folderSplit, "/", nomes.preds[i], 
                                        "-auprc.csv", sep="")))       
      final.auprc.macro = rbind(final.auprc.macro, 
                                data.frame(fold = f, value = auprc$Macro.AUPRC))
      final.auprc.micro = rbind(final.auprc.micro, 
                                data.frame(fold = f, value = auprc$Micro.AUPRC))
      
      #################################
      runtime = data.frame(read.csv(paste(folderSplit, "/runtime-fold.csv", sep="")))
      names(runtime) = c("fold", "user.self", "sys.self",
                         "elapsed","user.child","sys.child")
      final.runtime = rbind(final.runtime, runtime)
      
      #################################
      f = f + 1
      gc()
    } 
    
    
    names(final.conf.mat) = c("Measures", folds)
    names(final.roc.auc) = c("Fold", "Value")
    names(final.roc.auc.micro) = c("Fold", "Value")
    names(final.roc.auc.macro) = c("Fold", "Value")
    names(final.auprc.micro) = c("Fold", "Value")
    names(final.auprc.macro) = c("Fold", "Value")
    final.auprc.macro = final.auprc.macro[-1,]
    final.auprc.micro = final.auprc.micro[-1,]
    
    ###########################################
    fold = seq(1, parameters$Config$Number.Folds, by =1)
    
    ###########################################
    names(final.conf.mat) = c("Measures", folds)
    final.conf.mat[is.na(final.conf.mat)] <- 0
    write.csv(final.conf.mat, 
              paste(parameters$Folders$folderTested, "/", nomes.preds[i], 
                    "-Test-Evaluated.csv", sep=""), 
              row.names = FALSE)
    
    #######################
    media = data.frame(apply(final.conf.mat[,-1], 1, mean))
    media = cbind(measures, media)
    names(media) = c("Measures", "Mean10Folds")
    write.csv(media, 
              paste(parameters$Folders$folderTested, "/", 
                    nomes.preds[i], "-Mean10Folds.csv", sep=""), 
              row.names = FALSE)
    
    #######################
    mediana = data.frame(apply(final.conf.mat[,-1], 1, median))
    mediana = cbind(measures, mediana)
    names(mediana) = c("Measures", "Median10Folds")
    write.csv(mediana, 
              paste(parameters$Folders$folderTested, "/", 
                    nomes.preds[i], "-Median10Folds.csv", sep=""), 
              row.names = FALSE)
    
    
    #######################
    desvio = data.frame(apply(final.conf.mat[,-1], 1, sd))
    desvio  = cbind(measures, desvio)
    names(desvio ) = c("Measures", "Deviation10Folds")
    write.csv(desvio , 
              paste(parameters$Folders$folderTested, "/", 
                    nomes.preds[i], "-Deviation10Folds.csv", sep=""), 
              row.names = FALSE)
    
    ###########################################
    write.csv(final.roc.auc, 
              paste(parameters$Folders$folderTested, "/", nomes.preds[i], 
                    "-roc-auc.csv", sep=""), 
              row.names = FALSE)
    
    ###########################################
    write.csv(final.roc.auc.micro, 
              paste(parameters$Folders$folderTested, "/", nomes.preds[i], 
                    "-roc-auc-micro.csv", sep=""), 
              row.names = FALSE)
    
    ###########################################
    write.csv(final.roc.auc.macro, 
              paste(parameters$Folders$folderTested, "/", nomes.preds[i], 
                    "-roc-auc-macro.csv", sep=""), 
              row.names = FALSE)
    
    ###########################################
    write.csv(final.auprc.micro, 
              paste(parameters$Folders$folderTested, "/", nomes.preds[i], 
                    "-roc-auprc-micro.csv", sep=""), 
              row.names = FALSE)
    
    ###########################################
    final.runtime$fold = fold
    write.csv(final.runtime, 
              paste(parameters$Folders$folderTested, 
                    "/runtime-folds.csv", sep=""), 
              row.names = FALSE)
    
    ################
    i = i + 1
    gc()
  }
  
  gc()
  cat("\n########################################################")
  cat("\n# END EVALUATED                                        #") 
  cat("\n########################################################")
  cat("\n\n\n\n")
}






#########################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com
# Thank you very much!
#########################################################################
