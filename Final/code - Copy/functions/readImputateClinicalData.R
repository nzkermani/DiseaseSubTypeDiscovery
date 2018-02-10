readImputateClinicalData<-function()
{
clinical <- read.table("C:/Users/nz1413/Desktop/dataTAC/clinical_620Modified.csv",  header = TRUE, sep = ",")
data <- t(read.table("C:/Users/nz1413/Desktop/dataTAC/sputum_508genes.txt",  header = TRUE))
dataScaled = scale(data)

library(mclust)
mbc<-Mclust(dataScaled)
summary(mbc)
# find the clinical vatiable for data

temp = match(rownames(dataScaled) , as.character(clinical$Patient))
clinicalData = clinical[temp,]


# Data imputation
clinicalData$Label = mbc$classification
indout = rep(1,dim(clinicalData)[2])
for(i in 4:dim(clinicalData)[2])
{
  if(is.numeric(clinicalData[,i]))
  {
    if(length(which(is.na(clinicalData[,i])))/length(clinicalData$Label) < 0.2)
      indout[i]=0
    
  }
}

forPlot = clinicalData[,which(indout==0)]
library(missForest)
forplot.imp = missForest(forPlot, maxiter = 100, ntree = 10000, verbose = TRUE )
imputatedClinicalData = forplot.imp$ximp

return(imputatedClinicalData)
}

