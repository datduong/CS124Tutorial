


## read file
fin = read.table('example_data_1.txt',header=F,stringsAsFactors=F) ## is the same data in your class project
dim(fin)

fin = t(fin) ## people x Snp
num_people = nrow(fin)

snp = fin[,1:100]
LD = cor(snp)
WhereIndependentOfSnp1 = which ( (abs(LD[1,])<0.1) == TRUE )

SnpUsedToMakeY = c( 1, 4, 35, 55, 70, 86 )
# snp_effect = c(5,5,5,5,5)
y = rowSums (snp[,SnpUsedToMakeY] * 5 )

## let's create a phenotype y
random_noise = rnorm(num_people,1)
y = y + random_noise ## use 1st snp to create the phenotype y.

##
data_out = data.frame( y,snp )
colnames(data_out) = c('phenotype',paste0('snp',1:100))
# rownames(data_out) = paste0('person',1:50)
write.csv(data_out, file='Cs124Homework3Data.csv',col.names=T,row.names=F)

## fit lasso or ridge
install.packages('glmnet')
library('glmnet')

## lasso alpha=1 is the lasso penalty
model = glmnet (x=snp,y=y,alpha=1,family="gaussian",intercept=T) # https://www.rdocumentation.org/packages/glmnet/versions/3.0-2/topics/glmnet

coef1 = coef(model, s = 0.01)  # extract coefficients at a single value of lambda, here it is 0.01

plot(coef1[-1]) ## ignore intercept by using x[-1]

