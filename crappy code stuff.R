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
>>>>>>> ac1e6cfda3ca4a9b3b025e3e37a3743c57766adc
}

result = bestforward(prostate)

