prostate =get(load("prostate2.rdata"))

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
  for (i in k){ #iterate over all variables, starting with null model 0 and to p-1
    # now we need to pick a subset of variables that have not yet been used
    if (length(usedUpVariables) > 0 && usedUpVariables == names(data)){
      break
    }
    
    if (length(usedUpVariables != 0)){
      constructionData = predictorData[,-which(names(predictorData) %in% usedUpVariables)]
    } else {
      constructionData = predictorData
    }
    
    # now we need to iterate as many times as there are unused variables to construct the candidate model matrix
    modellist = vector(mode="list")
    for (z in 1:length(names(constructionData))){
      itermodel = update(bestModelList[[i + 1]], ~. + as.matrix(constructionData[z]))
      modellist[[z]] = itermodel
      remove(itermodel)
    }
    
    # select the best model out of this Mk+1 subset
    rSquaredList = vector(mode="list")
    for (h in 1:length(modellist)){
      rSquaredList[h] = summary(modellist[[h]])$r.squared
    }
    bestModelNumber = which.max(rSquaredList)
    bestModel = modellist[bestModelNumber]
    bestModelList[i+2] = bestModel
    vars = variable.names(bestModel)
    vars = vars[-1]
    usedUpVariables = append(usedUpVariables, vars) #add used variable to discard pile
  }
  
  #now that we have the best models of all Mk+1 subsets, pick the best best model
  BICforModels = vector(mode="list")
  for (h in 1:length(bestModelList)){
    BICforModels[h] = BIC(bestModelList[[h]])
  }
  bestBICModelNumber = which.max(BICforModels)
  return(bestModelList[[bestBICModelNumber]])
}

result = bestforward(prostate)

