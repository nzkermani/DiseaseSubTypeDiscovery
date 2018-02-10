CleanData<-function(data)
{
  nVar = dim(data)[2]
  for(i in 1:(nVar-1))
  {
    if(sum(unique(data[,i]))==1)
      data[,i] = as.factor(data[,i])
    
  }
  
  return(data)
}