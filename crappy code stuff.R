prostate =get(load("prostate2.rdata"))

<<<<<<< HEAD


forward_stepwise <- function(data){
  i_vector = c()
  lm_vector =c()
  for (j in 1:length(names(data))-1){
    rsq = 0
    best_predictor = NULL
    i_oneloop = 0
    for (i in 2:length(names(data))){
      if (!is.element(i,i_vector)){
        #Mki <- lm(Cscore~data[,i], data)
        Mki <- lm(as.formula(paste("Cscore~", paste(i_vector, collapse = "+", paste("+", paste(i))))))
        new_rsq = summary(Mki)$r.squared
        if (new_rsq > rsq){
          rsq = new_rsq
          best_predictor = Mki
          i_oneloop = i
        }
      }
    }
    append(i_vector, i_oneloop)
  }
=======
bestforward = function (data){
  response = data[1]
  bestModelList = vector(mode="list")
  p = length(names(data))
  k = 0:(p-1)
  nullmod = lm(as.matrix(data[1])~ 1, data = data)
  usedUpVariables = c()
  predictorData = data
  predictorData[,1] = NULL
  bestModelList[[1]] = nullmod
  constructionData = predictorData
  for (i in k){ #iterate over all variables, starting with null model 0 and to p-1
    
    # now we need to iterate as many times as there are unused variables to construct the candidate model matrix
    modellist = vector(mode="list")
    for (z in 1:length(names(constructionData))){
      itermodel = update(bestModelList[[i + 1]], paste("~ . +", names(constructionData)[z]))
      modellist[[z]] = itermodel
      remove(itermodel)
    }
    
    # select the best model out of this Mk+1 subset
    rSquaredList = vector(mode="list")
    for (h in 1:length(modellist)){
      rSquaredList[h] = summary(modellist[[h]])$r.squared
    }
    bestModelNumber = which.max(rSquaredList)
    bestModel = modellist[[bestModelNumber]]
    bestModelList[[i+2]] = bestModel
    #discard the variable used in the model from further use via constructedData
    toBeDiscarded = names(bestModel$coefficients)[-1]
    
    # TODO: fix this so it selects the variables chosen and removes them correctly
    for (j in 1:length(toBeDiscarded)){
      if (!is.na(-(match(toBeDiscarded, colnames(constructionData))[j])){
        constructionData = constructionData[,-(match(toBeDiscarded, colnames(constructionData))[j])]
      }
    }
  }
  
  #now that we have the best models of all Mk+1 subsets, pick the best best model
  BICforModels = vector(mode="list")
  for (h in 1:length(bestModelList)){
    BICforModels[h] = BIC(bestModelList[[h]])
  }
  bestBICModelNumber = which.max(BICforModels)
  return(bestModelList[[bestBICModelNumber]])
>>>>>>> ac1e6cfda3ca4a9b3b025e3e37a3743c57766adc
}

result = bestforward(prostate)

