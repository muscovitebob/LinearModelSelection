prostate = get(load("prostate2.rdata"))

forward_stepwise <- function(data){
  data_vars = names(data)
  predictor_list = c()
  models <- paste("Mk", 1:6, sep = "")
  for (j in 1:(length(data_vars)-1)){ # -1 because Cscore is not a predictor
    rsq = 0
    best_predictor = NULL
    best_model = NULL
    name = paste("Mk",j, sep = "")
    
    for (i in 2:length(data_vars)){ #Cscore is 1, the response variable
      if (!is.element(data_vars[i],predictor_list)){
        Mki <- lm(paste(data_vars[1], "~", paste(predictor_list, collapse = "+", "+",paste(data_vars[i]))), data = data)
        new_rsq = summary(Mki)$r.squared
        if (new_rsq > rsq){
          rsq = new_rsq
          best_predictor = data_vars[i]
          assign(name, Mki)
        }
      }
    }
    predictor_list <- c(predictor_list, best_predictor)
  }
  model_list <- lapply(models, function(x)get(x))
  
  BIC_models = c()
  for (h in 1:length(model_list)){
    BIC_models = c(BIC_models,BIC(model_list[[h]]))
  }
  best_model_nr = which.min(BIC_models)
  return(model_list[[best_model_nr]])
}

result = forward_stepwise(prostate)
result
