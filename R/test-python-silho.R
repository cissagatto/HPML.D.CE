##############################################################################
# CHAINS OF HYBRID PARTITIONS                                                #
# Copyright (C) 2022                                                         #
#                                                                            #
# This code is free software: you can redistribute it and/or modify it under #
# the terms of the GNU General Public License as published by the Free       #
# Software Foundation, either version 3 of the License, or (at your option)  #
# any later version. This code is distributed in the hope that it will be    #
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of     #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General   #
# Public License for more details.                                           #
#                                                                            #
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri           #
# Ferrandin | Prof. Dr. Celine Vens | Dr. Felipe Nakano Kenji                #
#                                                                            #
# Federal University of São Carlos - UFSCar - https://www2.ufscar.br         #
# Campus São Carlos - Computer Department - DC - https://site.dc.ufscar.br   #
# Post Graduate Program in Computer Science - PPGCC                          # 
# http://ppgcc.dc.ufscar.br - Bioinformatics and Machine Learning Group      #
# BIOMAL - http://www.biomal.ufscar.br                                       #
#                                                                            #
# Katholieke Universiteit Leuven Campus Kulak Kortrijk Belgium               #
# Medicine Department - https://kulak.kuleuven.be/                           #
# https://kulak.kuleuven.be/nl/over_kulak/faculteiten/geneeskunde            #
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
    str.execute = paste("python3 ",parameters$Folders$folderUtils, 
                        "/Python/clusters-chains/main.py ", 
                        train.name.file.csv, " ",
                        val.name.file.csv,  " ",
                        test.name.file.csv, " ", 
                        partition.csv.name, " ", 
                        Folder.Tested.Split, 
                        sep="")
    
    # EXECUTA
    res = print(system(str.execute))
    
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
    #cat("\nData frame")
    apagar = c(0)
    confMatPartitions = data.frame(apagar)
    partitions = c()
    
    ###################################################################
    apagar = c(0)
    y_true = data.frame(apagar)
    y_proba = data.frame(apagar)
    y_pred = data.frame(apagar)
    
    ###################################################################
    #cat("\nGet the true and predict lables")
    setwd(Folder.Tested.Split)
    y_true = data.frame(read.csv("y_true.csv"))
    y_proba = data.frame(read.csv("y_proba.csv"))
    
    nomes.rotulos = names(y_proba)
    
    #########################################################################
    cat("\n\nOpen Test file")
    test.name.file = paste(parameters$Folders$folderCVTS, "/",
                           parameters$Config$Dataset.Name, "-Split-Ts-",
                           f, ".csv", sep = "")
    test.dataset = data.frame(read.csv(test.name.file))
    
    labels.indices = seq(parameters$DatasetInfo$LabelStart,
                         parameters$DatasetInfo$LabelEnd, by=1)
    test.mldr = mldr_from_dataframe(test.dataset, labelIndices = labels.indices)
    
    y_pred <- data.frame(as.matrix(fixed_threshold(y_proba, 
                                                   threshold = 0.5)))
    
    setwd(Folder.Tested.Split)
    write.csv(y_pred, "y_pred.csv", row.names = FALSE)
    
    
    #####################################################################
    roc.curva(predictions = y_pred,
              probabilities = y_proba,
              test = test.mldr,
              Folder = Folder.Tested.Split)
    
    
    ##############################################
    cat("\nInformações das predições")
    predictions.information(nomes.rotulos=nomes.rotulos, 
                            proba = y_proba, 
                            preds = y_pred, 
                            trues = y_true, 
                            folder = Folder.Tested.Split)
    
    
    #######################################################################
    pred = paste(Folder.Tested.Split, "/y_proba.csv", sep="" )
    true = paste(Folder.Tested.Split, "/y_true.csv", sep="" )
    str.execute = paste("python3 ",
                        parameters$Folders$folderUtils,
                        "/Python/auprc.py ",
                        true, " ",
                        pred,  " ",
                        Folder.Tested.Split,
                        sep="")
    
    # EXECUTA
    res = print(system(str.execute))
    
    if(res!=0){
      break
    }
    
    #########################################################################
    #cat("\nCompute measures multilabel")
    y.true = data.frame(sapply(y_true, function(x) as.numeric(as.character(x))))
    y_true3 = mldr_from_dataframe(y.true , labelIndices = seq(1,ncol(y.true )), name = "y.true")
    y_pred2 = sapply(y_pred, function(x) as.numeric(as.character(x)))
    
    #cat("\nSave Confusion Matrix")
    setwd(Folder.Tested.Split)
    salva3 = paste("Conf-Mat-Fold-", f, ".txt", sep="")
    sink(file=salva3, type="output")
    confmat = multilabel_confusion_matrix(y_true3, y_pred2)
    print(confmat)
    sink()
    
    #cat("\nCreating a data frame")
    confMatPart = multilabel_evaluate(confmat)
    confMatPart = data.frame(confMatPart)
    names(confMatPart) = paste("Fold-", f, sep="")
    namae = paste("Split-", f, "-Evaluated.csv", sep="")
    setwd(Folder.Tested.Split)
    write.csv(confMatPart, namae)
    
    
    ###############################################################
    conf.mat = data.frame(confmat$TPl, confmat$FPl,
                          confmat$FNl, confmat$TNl)
    names(conf.mat) = c("TP", "FP", "FN", "TN")
    
    
    # porcentagem
    conf.mat.perc = data.frame(conf.mat/nrow(y_true))
    names(conf.mat.perc) = c("TP.perc", "FP.perc", "FN.perc", "TN.perc")
    
    # calculando o total de rótulos classificados errados
    wrong = conf.mat$FP + conf.mat$FN
    
    # calculando a porcentagem de rótulos classificados errados
    wrong.perc = wrong/nrow(y_true)
    
    # calculando o total de rótulos classificados corretamente
    correct = conf.mat$TP + conf.mat$TN
    
    # calculando a porcentagem de rótulos classificados corretamente
    correct.perc = correct/nrow(y_true)
    
    conf.mat = data.frame(conf.mat, conf.mat.perc, wrong, correct, 
                          wrong.perc, correct.perc)
    
    setwd(Folder.Tested.Split)
    write.csv(conf.mat, "utiml-matrix-confusion.csv")
    
    #####################################################################
    cat("\nSave original and pruned predictions")
    pred.o = paste(colnames(y_pred), "-pred", sep="")
    names(y_pred) = pred.o
    
    true.labels = paste(colnames(y_true), "-true", sep="")
    names(y_true) = true.labels
    
    proba = paste(colnames(y_proba), "-proba", sep="")
    names(y_proba) = proba
    
    all.predictions = cbind(y_proba, y_pred, y_true)
    
    setwd(Folder.Tested.Split)
    write.csv(all.predictions, "folder-predictions.csv", row.names = FALSE)
    
    f = f + 1
    gc()
  } # fim do for each
  
  gc()
  cat("\n###################################################")
  cat("\n# TEST: Evaluation Folds END                      #")
  cat("\n###################################################")
  cat("\n\n")
}



##############################################################################
# FUNCTION GATHER EVALUATION                                                 #
#   Objective                                                                #
#   Parameters                                                               #
##############################################################################
gather.eval.python.silho <- function(parameters){
  
  
  ##########################################################################
  final.proba.auc = c(0)
  final.proba.micro.auc = c(0)
  final.proba.macro.auc = c(0)
  final.proba.ma.mi.auc.fold = c(0)
  final.proba.ma.mi.auc.cluster = c(0)
  
  final.bin.micro.auc = c(0)
  final.bin.macro.auc = c(0)
  final.bin.auc = c(0)
  
  apagar = c(0)
  avaliado.final = data.frame(apagar)
  nomes = c("")
  
  # from fold = 1 to index.dataset_folders
  f = 1
  while(f<=parameters$Config$Number.Folds){
    
    cat("\n#======================================================")
    cat("\n# Fold: ", f)
    cat("\n#======================================================\n")
    
    # vector with names
    measures = c("accuracy","average-precision","clp","coverage","F1",
                 "hamming-loss","macro-AUC", "macro-F1","macro-precision",
                 "macro-recall","margin-loss","micro-AUC","micro-F1",
                 "micro-precision","micro-recall","mlp","one-error",
                 "precision","ranking-loss", "recall","subset-accuracy","wlp")
    
    ##########################################################################
    # "/dev/shm/ej3-GpositiveGO/Tested/Split-1"
    Folder.Tested.Split = paste(parameters$Folders$folderTested,
                                "/Split-", f, sep="")
    
    ######################################################################
    setwd(Folder.Tested.Split)
    str = paste("Split-", f, "-Evaluated.csv", sep="")
    avaliado = data.frame(read.csv(str))
    avaliado.final= cbind(avaliado.final, avaliado[,2])
    nomes[f] = paste("Fold-", f, sep="")
    
    ######################################################################
    setwd(Folder.Tested.Split)
    auc = data.frame(read.csv("bin-auc.csv"))
    names(auc) = c("fold", "value")
    final.bin.auc = rbind(final.bin.auc, auc)
    
    micro.auc = data.frame(read.csv("bin-micro-auc.csv"))
    names(micro.auc) = c("fold", "value")
    final.bin.micro.auc = rbind(final.bin.micro.auc, micro.auc)
    
    macro.auc = data.frame(read.csv("bin-macro-auc.csv"))
    names(macro.auc) = c("fold", "value")
    final.bin.macro.auc = rbind(final.bin.macro.auc, macro.auc)
    
    #################################
    setwd(Folder.Tested.Split)
    proba.auc = data.frame(read.csv("proba-auc.csv"))
    names(proba.auc) = c("fold", "value")
    final.proba.auc = rbind(final.proba.auc, proba.auc)
    
    proba.micro.auc = data.frame(read.csv("proba-micro-auc.csv"))
    names(proba.micro.auc) = c("fold", "value")
    final.proba.micro.auc = rbind(final.proba.micro.auc, proba.micro.auc)
    
    proba.macro.auc = data.frame(read.csv("proba-macro-auc.csv"))
    names(proba.macro.auc) = c("fold", "value")
    final.proba.macro.auc = rbind(final.proba.macro.auc, proba.macro.auc)
    
    proba.ma.mi.auc.fold = data.frame(read.csv("folder_proba_ma_mi.csv"))
    final.proba.ma.mi.auc.fold = rbind(final.proba.ma.mi.auc.fold,
                                       proba.ma.mi.auc.fold)
    
    
    f = f + 1
    gc()
    
  } # end folds
  
  
  fold = seq(1, number_folds, by =1)
  
  final.proba.auc = final.proba.auc[-1,]
  final.proba.auc = data.frame(fold, auc = final.proba.auc$value)
  
  final.proba.micro.auc = final.proba.micro.auc[-1,]
  final.proba.micro.auc = data.frame(fold, micro.auc = final.proba.micro.auc$value)
  
  final.proba.macro.auc = final.proba.macro.auc[-1,]
  final.proba.macro.auc = data.frame(fold, macro.auc = final.proba.macro.auc$value)
  
  final.proba.ma.mi.auc.fold = final.proba.ma.mi.auc.fold[-1,]
  final.proba.ma.mi.auc.fold = data.frame(fold, final.proba.ma.mi.auc.fold)
  
  setwd(parameters$Folders$folderTested)
  write.csv(final.proba.auc, "proba-auc.csv", row.names = FALSE)  
  write.csv(final.proba.macro.auc, "proba-macro-auc.csv", row.names = FALSE)  
  write.csv(final.proba.micro.auc, "proba-micro-auc.csv", row.names = FALSE)
  write.csv(final.proba.ma.mi.auc.fold, "proba-ma-mi-auprc-fold.csv", row.names = FALSE)  
  
  #################
  final.bin.auc = final.bin.auc[-1,]
  final.bin.auc = data.frame(fold, auc = final.bin.auc$value)
  
  final.bin.micro.auc = final.bin.micro.auc[-1,]
  final.bin.micro.auc = data.frame(fold, micro.auc = final.bin.micro.auc$value)
  
  final.bin.macro.auc = final.bin.macro.auc[-1,]
  final.bin.macro.auc = data.frame(fold, macro.auc = final.bin.macro.auc$value)
  
  setwd(parameters$Folders$folderTested)
  write.csv(final.bin.auc, "bin-auc.csv", row.names = FALSE)  
  write.csv(final.bin.macro.auc, "bin-macro-auc.csv", row.names = FALSE)  
  write.csv(final.bin.micro.auc, "bin-micro-auc.csv", row.names = FALSE)  
  
  
  avaliado.final = avaliado.final[,-1]
  names(avaliado.final) = nomes
  avaliado.final = cbind(measures, avaliado.final)
  
  setwd(Folder.Tested.Split)
  write.csv(avaliado, paste("Evaluated-Fold-", f, ".csv", sep=""),
            row.names = FALSE)
  
  # calculando a média dos 10 folds para cada medida
  media = data.frame(apply(avaliado.final[,-1], 1, mean))
  media = cbind(measures, media)
  names(media) = c("Measures", "Mean10Folds")
  
  setwd(parameters$Folders$folderTested)
  write.csv(media, "Mean10Folds.csv", row.names = FALSE)
  
  mediana = data.frame(apply(avaliado.final[,-1], 1, median))
  mediana = cbind(measures, mediana)
  names(mediana) = c("Measures", "Median10Folds")
  
  setwd(parameters$Folders$folderTested)
  write.csv(mediana, "Median10Folds.csv", row.names = FALSE)
  
  dp = data.frame(apply(avaliado.final[,-1], 1, sd))
  dp = cbind(measures, dp)
  names(dp) = c("Measures", "SD10Folds")
  
  setwd(parameters$Folders$folderTested)
  write.csv(dp, "desvio-padrão-10-folds.csv", row.names = FALSE)
  
  str = "ESTE É O COMPLETE CHAINS HPMLS"
  write(str, "informacoes.txt")
  
  
  gc()
  cat("\n######################################################")
  cat("\n# TEST: Gather Evaluations End                       #")
  cat("\n######################################################")
  cat("\n\n\n\n")
  
}




#########################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com
# Thank you very much!
#########################################################################
