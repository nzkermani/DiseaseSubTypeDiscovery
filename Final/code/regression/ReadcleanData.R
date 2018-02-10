 readcleanData = function(){
   

# read the data
data <- read.delim("D:/mohsen AF study/data/data.txt")

#0-delete race, AAA, CAD.Fam.history.Q14B
data = data[,-c(3,5, 10,20)]
#TO DO
#0.1 - cigarets
#per day 1=0, 2=5, 3=15, 4=30, 5=40
#year 1=0, 2=5, 3=12.5, 4=32.5, 5=40
#multiply

temp = as.numeric(as.character(data$Years.Q10))
temp[which(temp==1)]=0
temp[which(temp==2)]=5
temp[which(temp==3)]=12.5
temp[which(temp==4)]=32.5
temp[which(temp==5)]=40
temp[which(is.na(as.numeric(as.character(data$Years.Q10))) == TRUE)]=0

temp2 = as.numeric(as.character(data$Cigarettes.day.Q9))
temp2[which(temp2==1)]=0
temp2[which(temp2==2)]=5
temp2[which(temp2==3)]=15
temp2[which(temp2==4)]=30
temp2[which(temp2==5)]=40
temp2[which(is.na(as.numeric(as.character(data$Cigarettes.day.Q9))) == TRUE)]=0
# 0 reference 1-9,10-19, 20-39, 40+
#cigarettes_total = (temp*temp2)/20 
cigarettes_total = temp+temp2
Temp = cigarettes_total
Temp[cigarettes_total == 0 ] = 0
Temp[cigarettes_total > 0  ] = 1
# Temp[cigarettes_total == 0 ] = 0
# Temp[cigarettes_total > 0 & cigarettes_total < 10 ] = 1
# Temp[cigarettes_total >= 10 & cigarettes_total < 20] = 2
# Temp[cigarettes_total >= 20 & cigarettes_total < 40] = 3
# Temp[cigarettes_total >= 40] = 4

data$cigarettes_total  = factor(Temp) 

data = data[,-c(18,19)]


#1- sampling
# delaling with im/unnalanced data 
# reference :  "Instance sampling in credit scoring: An empirical study of sample size and balancing."

#2- covariate sanity check
#leave out covariate with 10% missing values

#2.1 looking for the missing values 
#leave out covariate with 10% missing values


Known_AF = data[(which(data$AF != 'NULL')),];
#age
Known_AF = Known_AF[which(Known_AF$AGE != '#VALUE!'),]
Known_AF = Known_AF[which(Known_AF$AGE != ""),]
Known_AF = Known_AF[which(as.double(as.character(Known_AF$AGE)) < 95),]

Known_AF = Known_AF[which(Known_AF$HR != ""),]

# remove samples with missing values
new_AF = Known_AF[,4]
Known_AF = Known_AF[,-4]
noMissingVlueSample = apply(Known_AF, 1,  function(x) !sum(x=='NULL')) 
Known_AF = Known_AF[noMissingVlueSample,]

# remove all the nulls from the levels
temp = lapply(Known_AF, function(x) as.character(x))
data = as.data.frame(lapply(temp, as.factor))
data$HR = as.numeric(data$HR)
Known_AF = data
return(Known_AF)
}