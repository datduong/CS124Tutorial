
## read file
fin = read.table('example_data_1.txt',header=F,stringsAsFactors=F) ## is the same data in your class project
dim(fin)

fin = t(fin) ## people x Snp
num_people = nrow(fin)

## let's create a phenotype y
random_noise = rnorm(num_people,1)
y = fin [,1] * 10 + random_noise ## use 1st snp to create the phenotype y.

## fit y = ag + b + epsilon
model = lm (y ~  fin [,1] ) ## fit 1st snp on the phenotype
summary (model)

## remove intercept so we fit y = ag + epsilon
model = lm (y ~  fin [,1] -1 ) ## fit 1st snp on the phenotype
summary (model)

## fit 1st 10 SNPs
model = lm (y ~  fin [,1:10] -1 ) ## fit 1st snp on the phenotype
summary (model) ## notice NA value
## look at the position it is all NA , this is the 6th SNPs.
fin[,6] ## all 0
## but 5th SNP is all 2. and did not get NA
fin[,5]
## SNPs with correlation (here the 5th and 6th have cor=-1) --> one of them will have NA values

## we have 50 people ... so what happen if we fit more than 50 SNPs ?
model = lm (y ~  fin [,1:50] -1 ) ## fit 1st snp on the phenotype
summary (model)

## 500 snps
model = lm (y ~  fin [,1:500] -1 ) ## fit 1st snp on the phenotype
summary (model) ## model fails ... bunch of NAs

## fit lasso or ridge 
install.packages('glmnet')
library('glmnet')

## lasso alpha=1 is the lasso penalty
model = glmnet (x=fin,y=y,alpha=1,family="gaussian",intercept=F) # https://www.rdocumentation.org/packages/glmnet/versions/3.0-2/topics/glmnet

coef1 = coef(model, s = 0.01)  # extract coefficients at a single value of lambda, here it is 0.01
head (coef1) ## do we see that SNP1 has same values as simple linear regression ? 

coef1 = coef(model, s = 0.1)  
head (coef1) ## not much difference for the 2 choices of lambda

## ridge
model = glmnet (x=fin,y=y,alpha=0,family="gaussian",intercept=F) # https://www.rdocumentation.org/packages/glmnet/versions/3.0-2/topics/glmnet
coef1 = coef(model, s = 0.01)  # extract coefficients at a single value of lambda
head (coef1) ## do we see that SNP1 has same values as simple linear regression ? 

## fit both lasso and ridge together 
model = glmnet (x=fin,y=y,alpha=0.5,family="gaussian",intercept=F) # https://www.rdocumentation.org/packages/glmnet/versions/3.0-2/topics/glmnet
coef1 = coef(model, s = 0.01)  # extract coefficients at a single value of lambda
head (coef1) ## do we see that SNP1 has same values as simple linear regression ? 


## let's look at how related are the 50 people.
K = fin %*% t(fin) ## notice, this is matrix mult. is just doing simple dot product for everyone in the dataset
heatmap(K,scale='none') ## looks be about 2 ancestral populations here.


