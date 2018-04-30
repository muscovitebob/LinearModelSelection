prostate =get(load("prostate2.rdata"))

bestforward = function (data){
  response = data[1]
  bestModelList = vector(mode="list")
  p = length(names(data))
  k = 0:(p-1)
  null = lm(data$Cscore~1, data = data)
  usedUpVariables = c()
  predictorData = data
  predictorData[,1] = NULL
  for (i in k){ #iterate over all variables, starting with null model 0 and to p-1
    # now we need to pick a subset of variables that have not yet been used
    if (length(usedUpVariables != 0)){
      constructionData = predictorData[,,!usedUpVariables]
    } else {
      constructionData = predictorData
    }
    # now we need to iterate as many times as there are unused variables to construct the models
    modellist = vector(mode="list")
    for (j in 1:length(constructionData)){ #iterate over all Mk+1
      provFormulaString = paste(names(response), "~")
      for (z in 1:j){#for the current Mk+1, create the model string
        if (z == 1){
          provFormulaString = paste(provFormulaString, names(constructionData)[z])
        } else {
          provFormulaString = paste(provFormulaString, "+", names(constructionData)[z])
        }
      }
      itermodel = lm(as.formula(paste(provFormulaString)), data = data) #assigns the current Mk+1 this model
      vars = variable.names(itermodel)
      vars = vars[-1]
      modellist[[j]] = itermodel #add this model to the candidate model list
      UsedUpVariables = append(usedUpVariables, vars) #add used variable to discard pile
    }
    
    # select the best model out of this Mk+1 subset
    rSquaredList = vector(mode="list")
    for (h in 1:length(modellist)){
      rSquaredList[[h]] = summary(modellist[[h]])$r.squared
    }
    bestModelNumber = which.max(rSquaredList)
    bestModel = modellist[bestModelNumber]
    bestModelList[[i+1]] = bestModel
  }
  
  #now that we have the best models of all Mk+1 subsets, pick the best best model
  BICforModels = vector(mode="list")
  for (h in 1:length(bestModelList)){
    BICforModels[[h]] = BIC(bestModelList[[h]][[1]])
  }
  bestBICModelNumber = which.max(BICforModels)
  return(bestModelList[[bestBICModelNumber]])
}

result = bestforward(prostate)

