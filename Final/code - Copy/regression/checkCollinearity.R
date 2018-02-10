checkCollinearity<- function(Known_AF)
{
#Coliniearity spearman
#exclude hyper tension because it is continious variable
#Known_AF = Known_AF[,-6]
temp = Known_AF[,-3]
numeric_Known_af = sapply(temp, as.numeric)
cor_matrix = cor(numeric_Known_af)

heatmap(cor_matrix)
w=''
for(i in 1:dim(cor_matrix)[1])
  w[i] = max(abs(cor_matrix[i,-i]))

# # 14 and 16 are highly correlated [14] "HTN.Q1I" "HLP.Q1J" 
# # upon client advide we retain HTN.Q1I
# Known_AF= Known_AF[,-c(15)]

# no colinearity
return(Known_AF)
}