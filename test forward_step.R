prostate = get(load("prostate2.rdata"))

forward_stepwise <- function(data){
  data_vars = names(data)
  #a list for all the predictors that have been used
  predictor_list = c()
  #the names of all the best models of each round of stepwise selection
  models <- paste("Mk", 1:7, sep = "")
  for (j in 1:(length(data_vars)-1)){ # -1 because Cscore is not a predictor
    rsq = 0
    best_predictor = NULL
    best_model = NULL
    name = paste("Mk",j, sep = "") 
    
    for (i in 2:length(data_vars)){ # the loop starts from 2, because Cscore, the response variable, is 1    
      if (!is.element(data_vars[i],predictor_list)){
        #paste the function Cscore~(all the variables from the previous round) + this variable
        Mki <- lm(paste(data_vars[1], "~", paste(predictor_list, collapse = "+", "+",paste(data_vars[i]))), data = data)
        new_rsq = summary(Mki)$r.squared
        #if this model is better(higher r square) than the previously selected model this round, 
        #make this the selected variable
        if (new_rsq > rsq){
          rsq = new_rsq
          best_predictor = data_vars[i]
          assign(name, Mki)
        }
      }
    }
    #append the selected variable of this round to the list of used predictor variables
    predictor_list <- c(predictor_list, best_predictor)
  }
  # based on the list of all the model names, get all the forward stepwise models in a list
  model_list <- lapply(models, function(x)get(x))
  
  BIC_models = c()
  #get a BIC value for every forward stepwise model
  for (h in 1:length(model_list)){
    BIC_models = c(BIC_models,BIC(model_list[[h]]))
  }
  best_model_nr = which.min(BIC_models)
  #return the model with the lowest BIC
  return(model_list[[best_model_nr]])
}

result = forward_stepwise(prostate)
result
