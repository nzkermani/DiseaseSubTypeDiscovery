checkCollinearity<- function(Known_AF,key_)
{
  #Coliniearity spearman
  #exclude hyper tension because it is continious variable
  #Known_AF = Known_AF[,-6]
  temp = Known_AF[,dim(Known_AF)[2]]
  numeric_Known_af = sapply(temp, as.numeric)
  cor_matrix = cor(numeric_Known_af)
  library(corrplot)
  corrplot(cor_matrix, method="circle")
  heatmap(cor_matrix)
  w=''
  wind = ''
  for(i in 1:dim(cor_matrix)[1]){
    w[i] = max(abs(cor_matrix[i,-i]))
    wind[i] = which(abs(cor_matrix[i,-i]) ==max(abs(cor_matrix[i,-i])) )
  }
  # # 14 and 16 are highly correlated [14] "HTN.Q1I" "HLP.Q1J" 
  # # upon client advide we retain HTN.Q1I
  # Known_AF= Known_AF[,-c(15)]
  if(key_==1)
  {
  Known_AF = Known_AF[,-c(6,22,27)]
  Known_AF$Label = as.numeric(Known_AF$Label)
  }
  else
  {
    Known_AF = Known_AF[,-c(6,20,27)]
  }
  # no colinearity
  return(Known_AF)
}