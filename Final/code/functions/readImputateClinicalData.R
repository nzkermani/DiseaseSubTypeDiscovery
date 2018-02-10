readImputateClinicalData<-function()
{
  clinical <- read.table("C:/Users/nz1413/Desktop/dataTAC/clinical_620Modified.csv",  header = TRUE, sep = ",")
  data <- t(read.table("C:/Users/nz1413/Desktop/dataTAC/sputum_508genes.txt",  header = TRUE))
  dataScaled = scale(data)
  
  TAClabels <- t(read.table("C:/Users/nz1413/Desktop/dataTAC/correspondanceTAC.txt",  header = FALSE))
  inds = match(TAClabels[2,], rownames(data))
  tacLabels <- as.numeric(TAClabels[1,])
  dataScaled = dataScaled[inds,]
  
  library(mclust)
  mbc<-Mclust(dataScaled)
  summary(mbc)
  temp  = factor(mbc$classification)
  levels(temp)=c("TAC1", "TAC3a", "TAC2", "TAC3b")
  mbc$classification = temp
  
  # find the clinical vatiable for data
  temp = match(rownames(dataScaled) , as.character(clinical$Patient))
  clinicalData = clinical[temp,]
  # Data imputation
  clinicalData$gp = mbc$classification
  indout = rep(1,dim(clinicalData)[2])
  for(i in 4:dim(clinicalData)[2])
  {
    if(is.numeric(clinicalData[,i]))
    {
      if(length(which(is.na(clinicalData[,i])))/length(clinicalData$gp) < 0.2)
        indout[i]=0
      
    }
  }
  
  forPlot = clinicalData[,which(indout==0)]
  library(missForest)
  forplot.imp = missForest(forPlot, maxiter = 100, ntree = 10000, verbose = TRUE )
  imputatedClinicalData = forplot.imp$ximp
  require(reshape2)
  
  
  Label = mbc$classification
  imputatedClinicalData$Label = factor(Label) 
return(imputatedClinicalData)
}

