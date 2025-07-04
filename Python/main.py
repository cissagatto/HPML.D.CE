##############################################################################
# LABEL CLUSTERS CHAINS FOR MULTILABEL CLASSIFICATION                        #
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
# Prof. Elaine Cecilia Gatto - UFLA - Lavras, Minas Gerais, Brazil           #
# Prof. Ricardo Cerri - USP - São Carlos, São Paulo, Brazil                  #
# Prof. Mauri Ferrandin - UFSC - Blumenau, Santa Catarina, Brazil            #
# Prof. Celine Vens - Ku Leuven - Kortrijik, West Flanders, Belgium          #
# PhD Felipe Nakano Kenji - Ku Leuven - Kortrijik, West Flanders, Belgium    #
#                                                                            #
# BIOMAL - http://www.biomal.ufscar.br                                       #
#                                                                            #
##############################################################################


import sys
import platform
import os
import time
import pandas as pd
from sklearn.ensemble import RandomForestClassifier  
import importlib
import lccml
importlib.reload(lccml)
from lccml import LCCML
import evaluation as eval
importlib.reload(eval)
import measures as ms
importlib.reload(ms)


if __name__ == '__main__':    
    
    random_state = 0
    n_estimators = 200
    baseModel = RandomForestClassifier(n_estimators = n_estimators, 
                                       random_state = random_state)

    train = pd.read_csv(sys.argv[1])
    valid = pd.read_csv(sys.argv[2])
    test = pd.read_csv(sys.argv[3])
    partitions = pd.read_csv(sys.argv[4])
    directory = sys.argv[5]
    n_chains = int(sys.argv[6])
    
    # print(" NUMBER CHAINS ", n_chains)
    
    #train = pd.read_csv("/tmp/emotions/Datasets/emotions/CrossValidation/Tr/emotions-Split-Tr-1.csv")
    #test = pd.read_csv("/tmp/emotions/Datasets/emotions/CrossValidation/Ts/emotions-Split-Ts-1.csv")
    #valid = pd.read_csv("/tmp/emotions/Datasets/emotions/CrossValidation/Vl/emotions-Split-Vl-1.csv")
    #partitions = pd.read_csv("/tmp/emotions/Partitions/emotions/Split-1/Partition-2/partition-2.csv")
    #directory = "/tmp/emotions/Tested/Split-1"    

    train = pd.concat([train,valid],axis=0).reset_index(drop=True)

    clusters = partitions.groupby("group")["label"].apply(list)   
    allLabels = partitions["label"].unique()

    x_train = train.drop(allLabels, axis=1)
    y_train = train[allLabels]
    
    x_test = test.drop(allLabels, axis=1)
    y_test = test[allLabels]

    lccml = LCCML(baseModel,n_chains)
    lccml.fit(x_train,y_train,clusters,) 

    start_time = time.time()
    test_predictions = pd.DataFrame(lccml.predict(x_test))
    end_time = time.time()
    predict_time = end_time - start_time
    
    name = (directory + "/predict_time.csv")
    predict_time = pd.DataFrame({'predict_time': [predict_time]})
    predict_time.to_csv(name, index=False)
    
    train_predictions = pd.DataFrame(lccml.predict(x_train))
    
    # test_predictions.columns
    # allLabels
    
    true = (directory + "/y_true.csv")
    pred = (directory + "/y_proba.csv")   
    train = (directory + "/y_train.csv") 
    
    test_predictions.to_csv(pred, index=False)
    test[allLabels].to_csv(true, index=False)

    train_predictions.to_csv(train, index=False)
    train_predictions[allLabels].to_csv(true, index=False)

    y_test.to_csv(true, index=False)     
    y_test[allLabels].to_csv(true, index=False)
    
    res_curves = eval.multilabel_curve_metrics(y_test, test_predictions)
    #res_final = pd.concat([res_curves, res_lp], ignore_index=True)    
    name = (directory + "/results-python.csv") 
    res_curves.to_csv(name, index=False)

    model_sizes = lccml.chain_model_sizes
    total_model_size = lccml.total_model_size

    df_sizes = pd.DataFrame({
        'chain_index': list(range(len(model_sizes))),
        'model_size_bytes': model_sizes
    })

    # Adiciona uma linha para o total
    df_sizes.loc['total'] = ['-', total_model_size]
    
    name = (directory + "/model-size.csv")     
    df_sizes.to_csv(name, index=False)

    train_times = lccml.chain_train_times
    train_time_total = lccml.train_time_total
    test_time_total = lccml.test_time_total

    df_times = pd.DataFrame({
        'chain_index': list(range(len(train_times))),
        'train_time': train_times
    })

    # Adiciona uma linha para o total
    df_times.loc['total'] = ['-', train_time_total]
    df_times['test_time_total'] = test_time_total  # repete o total para cada linha, ou ajuste como quiser

    name = (directory + "/runtime-python.csv")     
    df_times.to_csv(name, index=False)
  
    



