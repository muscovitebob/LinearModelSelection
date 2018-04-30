prostate =get(load("prostate2.rdata"))



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
}

forward_stepwise(prostate)


