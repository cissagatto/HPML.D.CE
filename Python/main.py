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
import io
import platform
import os
import time
import pandas as pd
import importlib
import pickle

from sklearn.ensemble import RandomForestClassifier  

import lccml
importlib.reload(lccml)
from lccml import LCCML

import evaluation as eval
importlib.reload(eval)

import measures as ms
importlib.reload(ms)

import ml_auprc_roc as curve
importlib.reload(curve)


if __name__ == '__main__':            
    
    # =========== ARGUMENTS ===========    
    """
    train = pd.read_csv("/tmp/d-GnegativeGO/Datasets/GnegativeGO/CrossValidation/Tr/GnegativeGO-Split-Tr-1.csv")
    valid = pd.read_csv("/tmp/d-GnegativeGO/Datasets/GnegativeGO/CrossValidation/Vl/GnegativeGO-Split-Vl-1.csv")
    test = pd.read_csv("/tmp/d-GnegativeGO/Datasets/GnegativeGO/CrossValidation/Ts/GnegativeGO-Split-Ts-1.csv")
    partitions = "/tmp/d-GnegativeGO/Partitions/GnegativeGO/Split-1/Partition-7/partition-7.csv"
    directory = "/tmp/d-GnegativeGO/Tested/Split-1"            
    start = 1717    
    n_chains = 10    
    """
    
    # =========== READING DATA ===========
    train = pd.read_csv(sys.argv[1])
    valid = pd.read_csv(sys.argv[2])
    test = pd.read_csv(sys.argv[3])
    partitions = pd.read_csv(sys.argv[4])
    directory = sys.argv[5]
    n_chains = int(sys.argv[6])    

    train = pd.concat([train,valid],axis=0).reset_index(drop=True)


    print("\n\n%==============================================%")
    #print("train: ", sys.argv[1])
    #print("valid: ", sys.argv[2])
    #print("test: ", sys.argv[3])
    #print("partitions: ", sys.argv[4])
    print("directory: ", sys.argv[5])
    print("n_chains: ", sys.argv[6])    
    print("%==============================================%\n\n")    
    

    clusters = partitions.groupby("group")["label"].apply(list)   
    allLabels = partitions["label"].unique()


    # attributes and labels
    x_train = train.drop(allLabels, axis=1)
    y_train = train[allLabels]    
    x_test = test.drop(allLabels, axis=1)
    y_test = test[allLabels]

    # Labels and attributes names
    labels_y_train = list(y_train.columns)
    labels_y_test = list(y_test.columns)
    attr_x_train = list(x_train.columns)
    attr_x_test = list(x_test.columns)


    # =========== INITIALIZE MODEL ===========
    random_state = 0
    n_estimators = 200
    baseModel = RandomForestClassifier(n_estimators = n_estimators, 
                                       random_state = random_state)
    model = LCCML(baseModel,n_chains)


    # =========== FIT ===========            
    start_time_train = time.time()
    model.fit(x_train,y_train,clusters,) 
    end_time_train = time.time()
    training = end_time_train - start_time_train
    

    # =========== PREDICT ===========
    start_time_test_bin = time.time()
    predicts = pd.DataFrame(model.predict2(x_test))
    end_time_test_bin = time.time()
    testing = end_time_test_bin - start_time_test_bin   


     # =========== file names ===========        
    true_name = os.path.join(directory, "y_true.csv")
    binary_name = os.path.join(directory, "y_pred_bin.csv")
    proba_name = os.path.join(directory, "y_pred_proba.csv")   
    proba_original = os.path.join(directory, "y_proba_original.csv")   

    
    # ======= SALVANDO OS CSVS =======            
    y_test.to_csv(true_name, index=False)
    predicts.to_csv(proba_name, index=False)
    true = y_test
    pred = predicts


    # ======= SAVE TIME =======    
    df_timing = pd.DataFrame([[        
        training,     
        testing
    ]], columns=["training", "testing"])
    df_timing.to_csv(os.path.join(directory, "runtime-python.csv"), index=False)


    # =========== SAVE MODEL SIZE EM BYTES ===========
    model_buffer = io.BytesIO()
    pickle.dump(model, model_buffer)
    model_size_bytes = model_buffer.tell()
    model_size_df = pd.DataFrame({
        'model_size_bytes': [model_size_bytes]
    })
    model_size_df.to_csv(os.path.join(directory, "model-size.csv"), index=False)   
    
    
