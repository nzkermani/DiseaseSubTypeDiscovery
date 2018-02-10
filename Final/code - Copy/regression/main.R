setwd('C:/Users/nz1413/Desktop/AnalysisTag/Final/code/regression/')
source("dummyVariableAndFeatureEnginearing.R")
source("checkCollinearity.R")
Known_AF0 = readcleanData()
Known_AF1 =dummyVariableAndFeatureEnginearing(Known_AF0)
Known_AF2 =checkCollinearity(Known_AF1)



fitted_model_all_intercept = glm(AF~., data=Known_AF2,  family="binomial")
summary(fitted_model_all_intercept)
backward_model = step(fitted_model_all_intercept,  direction="backward")
#forward_model = step(fitted_model_all_intercept,  direction="forward")


significant_cov = Known_AF2[, colnames(backward_model$model)]
fitted_model_best = glm(AF~., data=significant_cov,  family="binomial")

# obsolete for logestic reg on bionimial distributions
# # assumptio testing
# # colinearity pvalue > 0.05, test stat > 2
# library(car)
# dwt(fitted_model_best)
# 
# car::vif(fitted_model_best)
# library(lmtest)
# bptest(fitted_model_best)
# 
# library(nortest)
# ad.test(fitted_model_best$residuals)
# 
# 
# qqplot(fitted_model_best)

temp = lapply(Known_AF2, as.numeric)
data = as.data.frame(lapply(temp, as.factor))
data$HR = as.numeric(data$HR)
data$cigarettes_total = as.numeric(data$cigarettes_total)





# CMA

library(CMA)
# #generate learning sets
# d_ = Known_AF[,c("AF","AGE" , "Sex" , "HR", "Carotid.Left", "PAD.Left" , "Stroke.TIA.Q1D",
#                           "HLP.Q1J" , "HTN.Med.Q3A", "HLP.Med.Q3B" , "Cigarettes.day.Q9",
#                           "Self.reported.AF..Have.you.ever.been.diagnosed.or.told.by.a.GP.that.you.have.any.of.the.following.Q1F")]
data =data.matrix(significant_cov)

y = data[,1]
x = data[,-1]


# undersampling
all = list()
missClassification_mean = vector()
missClassification_sd = vector()

set.seed(123)
for(i in 1:1000)
  # undersample the control class, wrire a peace of code for that
{
  
  ratio_ = 2
  ind_nonAF_cv = sample(which(y==1), ratio_*length(which(y==2)))
  minix = x[c(which(y==2), ind_nonAF_cv ),];
  miniy = y[c(which(y==2), ind_nonAF_cv )]
  
  fiveCVdat <- GenerateLearningsets(y=miniy, method = "CV",
                                    fold = 5, strat = TRUE) 
  
  resultlr <- classification(X=minix, y=miniy, learningsets = fiveCVdat, classifier =pls_lrCMA)
  missClass <- evaluation(resultlr)
  missClassification_mean[i] = mean(missClass@score)
  missClassification_sd[i] = sd(missClass@score)
  
  
  all <-c(all , missClass)
}
#classification performance
accuracy = 100*(1 - mean(unlist(lapply(all, function(x) mean(x@score)))))
sd_accuracy = 100*sd(unlist(lapply(all, function(x) mean(x@score))))

# exp logistic regression coef
coefs = exp(backward_model$coefficients)
# retrieve confidence intervals
confIntervals = exp(confint(backward_model))




# 
# 1.	This is excellent and further confirms the value of database. Can we please have a table with prevalence in each group? In addition, 
# we should have a similar one for males and another one for females in different age groups if statistically significant. Below is an example:
#1.sex(1)male
data = Known_AF2
maleStudyData = data[which(data$Sex == "1"),]
# prevelance
ftable(Known_AF2$Sex[which(Known_AF2$AF == 'Y')])
#FSD
#remove sex
maleStudyData = maleStudyData [,-2]

fitted_model_all_intercept_FSD = glm(AF~AGE, data=maleStudyData,  family="binomial")

sex1 = exp(fitted_model_all_intercept_FSD$coefficients)
sex1Confint = confint(fitted_model_all_intercept_FSD)

#1.sex(2)female
data = Known_AF2
FemaileStudyData = data[which(data$Sex == "2"),]
#FSD
#remove sex
FemaileStudyData = FemaileStudyData [,-2]

set.seed(12458) #For reproducibility
SubFemaleStudy = FemaileStudyData[c(sample(which(FemaileStudyData$AF=="N"),80),which(FemaileStudyData$AF=="Y")),]

fitted_model_all_intercept_mSD = glm(AF~AGE, data=FemaileStudyData,  family="binomial")

sex2 = exp(fitted_model_all_intercept_mSD$coefficients)
sex2Confint = confint(fitted_model_all_intercept_mSD)

b = barplot(sex2, ylim=range(c(sex2Confint)))

# prevalence of new AF af=y selfR = no(2)
ftable(Known_AF2[which(Known_AF2$AF == "Y" & Known_AF2$Self.reported.AF..Have.you.ever.been.diagnosed.or.told.by.a.GP.that.you.have.any.of.the.following.Q1F == 2),1])

sum(ftable(Known_AF2[which(Known_AF2$AF == "Y" & Known_AF2$Self.reported.AF..Have.you.ever.been.diagnosed.or.told.by.a.GP.that.you.have.any.of.the.following.Q1F == 2),1]))/dim(data)[1]*100
ftable(Known_AF2[which(Known_AF2$AF == "Y" & Known_AF2$Self.reported.AF..Have.you.ever.been.diagnosed.or.told.by.a.GP.that.you.have.any.of.the.following.Q1F == 2),1])/sum(ftable(Known_AF2[which(Known_AF2$AF == "Y" & Known_AF2$Self.reported.AF..Have.you.ever.been.diagnosed.or.told.by.a.GP.that.you.have.any.of.the.following.Q1F == 2),1]))
# HR between AF and non AF

> mean(Known_AF2[which(Known_AF2$AF == "Y" ),6])
[1] 68.26129
> mean(Known_AF2[which(Known_AF2$AF == "N" ),6])
[1] 66.62621
> sd(Known_AF2[which(Known_AF2$AF == "N" ),6])
[1] 10.99361
> sd(Known_AF2[which(Known_AF2$AF == "Y" ),6])
26.64484
# make more data -c(3,9,22)]


significant_cov_FSD = Known_AF[, colnames(backward_model_FSD$model)]
fitted_model_best_FSD = glm(AF~., data=significant_cov_FSD,  family="binomial")




numeric_fmls = sapply(FemaileStudyData, as.numeric)
cor_mat_fmls = cor(numeric_fmls)
