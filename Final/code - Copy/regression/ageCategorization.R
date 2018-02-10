dummyVariableAndFeatureEnginearing = function(Known_AF){
  
  ageTemp = as.double(as.character(Known_AF$AGE))
  ageTemp[as.double(as.character(Known_AF$AGE)) < 45 ] = 1
  ageTemp[as.double(as.character(Known_AF$AGE)) >= 45 & as.double(as.character(Known_AF$AGE)) < 55] = 2
  ageTemp[as.double(as.character(Known_AF$AGE)) >= 55 & as.double(as.character(Known_AF$AGE)) < 65] = 3
  ageTemp[as.double(as.character(Known_AF$AGE)) >= 65 & as.double(as.character(Known_AF$AGE)) < 75] = 4
  ageTemp[as.double(as.character(Known_AF$AGE)) >= 75 ] = 5
  
  
  # if a continious varuable is represented as factor 
  #it has to be replaced with a vector and vice versa
  Known_AF$AGE = as.factor(ageTemp)
  #Known_AF$HR = as.double(as.character(Known_AF$HR))
  
  # bivariate analysis
  # pearson's chi squared coefficient has a problem with small propotion of subclasses
  # neglected out if laziness
  ##library(MASS)
  ##lapply(Known_AF, function(x) chisq.test(x, Known_AF$AF)) 
  
  # data revision putin mohesn
  # formula for cigarets
  data = Known_AF
  

  return(data)
}