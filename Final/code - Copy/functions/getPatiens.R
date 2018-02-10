getPatiens<-function()
{
clinical <- read.table("C:/Users/nz1413/Desktop/dataTAC/clinical_620Modified.csv",  header = TRUE, sep = ",")
data <- t(read.table("C:/Users/nz1413/Desktop/dataTAC/sputum_508genes.txt",  header = TRUE))
dataScaled = scale(data)

library(mclust)
mbc<-Mclust(dataScaled)
summary(mbc)
# find the clinical vatiable for data
temp = match(rownames(dataScaled) , as.character(clinical$Patient))
clinicalDataTEMP = clinical[temp,]
patients = clinicalDataTEMP$Patient

return(patients)
}