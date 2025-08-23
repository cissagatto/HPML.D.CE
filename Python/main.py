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
import pickle

import lccml
importlib.reload(lccml)
from lccml import LCCML
import evaluation as eval
importlib.reload(eval)
import measures as ms
importlib.reload(ms)


if __name__ == '__main__':            
    
    # =========== READING DATA ===========
    train = pd.read_csv(sys.argv[1])
    valid = pd.read_csv(sys.argv[2])
    test = pd.read_csv(sys.argv[3])
    partitions = pd.read_csv(sys.argv[4])
    directory = sys.argv[5]
    n_chains = int(sys.argv[6])
    # fold = int(sys.argv[7])
    # print(" NUMBER CHAINS ", n_chains)
  
    #train = pd.read_csv("/tmp/lcc-GpositiveGO/Datasets/GpositiveGO/CrossValidation/Tr/GpositiveGO-Split-Tr-1.csv")
    #test = pd.read_csv("/tmp/lcc-GpositiveGO/Datasets/GpositiveGO/CrossValidation/Ts/GpositiveGO-Split-Ts-1.csv")
    #valid = pd.read_csv("/tmp/lcc-GpositiveGO/Datasets/GpositiveGO/CrossValidation/Vl/GpositiveGO-Split-Vl-1.csv")
    #partitions = pd.read_csv("/tmp/lcc-GpositiveGO/Partitions/GpositiveGO/Split-1/Partition-2/partition-2.csv")
    #directory = "/tmp/lcc-GpositiveGO/Tested/Split-1"    
    #n_chains = 10
    #fold = 1

    #print("\n\n%==============================================%")
    #print("train: ", sys.argv[1])
    #print("valid: ", sys.argv[2])
    #print("test: ", sys.argv[3])
    #print("partitions: ", sys.argv[4])
    #print("directory: ", sys.argv[5])
    #print("n_chains: ", sys.argv[6])
    #print("FOLD: ", sys.argv[7])
    #print("%==============================================%\n\n")

    
    train = pd.concat([train,valid],axis=0).reset_index(drop=True)

    clusters = partitions.groupby("group")["label"].apply(list)   
    allLabels = partitions["label"].unique()

    x_train = train.drop(allLabels, axis=1)
    y_train = train[allLabels]
    
    x_test = test.drop(allLabels, axis=1)
    y_test = test[allLabels]

    # =========== INITIALIZE MODEL ===========
    random_state = 0
    n_estimators = 200
    baseModel = RandomForestClassifier(n_estimators = n_estimators, 
                                       random_state = random_state)
    modelo = LCCML(baseModel,n_chains)


    # =========== FIT ===========            
    start_time_train = time.time()
    modelo.fit(x_train,y_train,clusters,) 
    end_time_train = time.time()
    train_duration = end_time_train - start_time_train


    # =========== PREDICT ===========
    #start_time_test_bin = time.time()
    #bin = pd.DataFrame(modelo.predict(x_test))
    #end_time_test_bin = time.time()
    #test_duration_bin = end_time_test_bin - start_time_test_bin        


    # =========== PREDICT PROBA ===========
    start_time_test_proba = time.time()    
    proba = lccml.safe_predict_proba(modelo, x_test, y_train)
    end_time_test_proba = time.time()
    test_duration_proba = end_time_test_proba - start_time_test_proba               


    # =========== SAVE TIME PREDICT ===========
    times_df = pd.DataFrame({        
        'train_duration': [train_duration],
        'test_duration_proba': [test_duration_proba],
        #'test_duration_bin': [test_duration_bin]
    })
    times_path = os.path.join(directory, "train-test-from-time.csv")
    times_df.to_csv(times_path, index=False)  

    
    # =========== SAVE RUNTIME ===========   
    df_times = pd.DataFrame({
        'chain_index': list(range(len(modelo.chain_train_times))),
        'train_time': modelo.chain_train_times,
        'train_time_total': modelo.train_time_total,
        'test_time': modelo.test_time_total
    })
    name = (directory + "/train-test-from-model.csv")     
    df_times.to_csv(name, index=False)  


    # =========== SAVE PREDICTIONS ===========   
    # probas_df = pd.DataFrame(proba, columns=labels_y_test)
    probas_path = os.path.join(directory, "y_proba.csv")
    proba.to_csv(probas_path, index=False)   
    
    #bin_path = os.path.join(directory, "y_bin.csv")
    #bin.to_csv(bin_path, index=False)   

    y_test.to_csv(os.path.join(directory, 'y_true.csv'), index=False)


    # =========== SAVE MEASURES ===========   
    metrics_df, ignored_df = eval.multilabel_curve_metrics(y_test, proba)    
    name = (directory + "/results-python.csv") 
    metrics_df.to_csv(name, index=False)  
    name = (directory + "/ignored-classes.csv") 
    ignored_df.to_csv(name, index=False)         

    
    # =========== SAVE MODEL SIZE ===========   
    df_sizes = pd.DataFrame({
        'chain_index': list(range(len(modelo.chain_model_sizes))),
        'chain_model_size': modelo.chain_model_sizes,
        'total_model_size': modelo.total_model_size
    })        
    name = (directory + "/model-size-from-model.csv")     
    df_sizes.to_csv(name, index=False)

    size = len(pickle.dumps(modelo))
    df_size = pd.DataFrame({        
        "size_bytes": [size]
    })
    name = (directory + "/model-size-from-memory.csv")     
    df_size.to_csv(name, index=False)