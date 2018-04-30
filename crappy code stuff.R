prostate =get(load("prostate2.rdata"))



forward_stepwise <- function(dataset){
  rsq = 0
  best_predictor = NULL
  for (i in 2:length(names(dataset))){
    a <- lm(Cscore~data[,i])
    new_rsq = summary(a)$r.squared
    if (new_rsq > rsq){
      rsq = new_rsq
      best_predictor = a
    }
  }
  return(best_predictor)
}

forward_stepwise(prostate)


