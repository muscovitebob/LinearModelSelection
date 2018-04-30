prostate =get(load("prostate2.rdata"))



forward_stepwise <- function(data){
  rsq = 0
  best_predictor = NULL
  for (i in 2:length(names(data))){
    a <- lm(Cscore~names(data)[i])
    new_rsq = summary(a)$r.squared
    if (new_rsq > rsq){
      rsq = new_rsq
      best_predictor = a
    }
  }
  return(best_predictor)
}

forward_stepwise(prostate)


