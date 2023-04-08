##############################################################################
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


import sys
import numpy as np
import pandas as pd
from sklearn.base import clone

class ECC:
    random_seed = 1234
    rng = np.random.default_rng(random_seed)
    def __init__(self,
                 model,
                 n_chains = 10,
                 ):
       self.model = model

       self.n_chains = n_chains
       self.orders = None
       self.chains = []
       
    def fit(self,
            x, ## dataframe
            y,
            clusters
            ):
        self.__generateOrders(len(clusters))
        self.clusters = self.__preprocessClustersName(clusters,
                                    y)
        for i in range(self.n_chains):
            self.__fitChain(self.orders[i], 
                           x, 
                           y)
    def predict(self,
                x):
        predictions = pd.concat([self.__predictChain(x, i) for i in range(self.n_chains)],axis=0)
        return predictions.groupby(predictions.index).apply(np.mean)

    def __generateOrders(self,
                        n_labels
                        ):
        self.orders = [self.rng.permutation(n_labels) for _ in range(self.n_chains)]
    
    def __fitChain(self,
                  order,
                  x,
                  y,
                  ):
        chain_x = x.copy()
        chain = []
        self.orderLabelsDataset = y.columns
        for i in order:
            chainModel = self.__getModel()            
            chain_y = pd.DataFrame(y[y.columns[self.clusters[i]]])
            chainModel.labelName_ = y.columns[self.clusters[i]]
            if chain_y.shape[1] == 1:
                chainModel.fit(chain_x, chain_y.values.ravel())
            else:
                chainModel.fit(chain_x, chain_y)
            chain_x = pd.concat([chain_x, chain_y],axis=1)
            chain.append(chainModel)
        self.chains.append(chain)
        
    def __predictChain(self,
                       x,
                       chainIndex):
        chain_x = x.copy()
        predictions = pd.DataFrame([])
        for model in self.chains[chainIndex]:
#            predictionsChain = pd.DataFrame(model.predict(chain_x), columns = model.labelName_ )
            predChain = model.predict_proba(chain_x)
            if type(predChain) is list:
                predChain = np.concatenate([probs[:,1].reshape(-1,1) for probs in predChain],axis=1)
            else:
                predChain = predChain[:,1].reshape(-1,1)
            predChain = pd.DataFrame(predChain, columns = model.labelName_ )
            predictions[model.labelName_] = predChain

            chain_x = pd.concat([chain_x, predChain], axis=1)
        predictions = predictions[self.orderLabelsDataset]
        return predictions      
      
    def __getModel(self):
        return clone(self.model)    
      
    def __preprocessClustersName(self,
            clusters,
            y):       ### transform clusters names to integers
        clustersIndexes = [[y.columns.get_loc(l) for l in labels] for labels in clusters ]
        return clustersIndexes
