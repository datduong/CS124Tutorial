

library('ggfortify')


#### generic car models example

mtcars
## some cars similar 
## some variables are similar 
plot(mtcars$mpg, mtcars$wt,pch=16,cex=1)
text(mtcars$mpg, mtcars$wt, rownames(mtcars), pos = 4)

## it is enough to know mpg, and not have to worry about knowing what is wt. 
## but... what do we keep? do we keep only mpg or only wt. ?
## this is where PCA comes in. PCA allows us to linearly combine mpg and wt into 1 single variable 

# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/prcomp

## scale --> make variance=1
## center --> make the mean=0

pca_mod = prcomp(mtcars[,c('mpg','wt')],retx=T,scale=T,center=T) # scale=T,center=T
summary(pca_mod)

par(mfrow=c(1,2))
biplot(pca_mod)
plot(mtcars$mpg, mtcars$wt,pch=16,cex=1)
text(mtcars$mpg, mtcars$wt, rownames(mtcars), pos = 4)

pca_mod$x ## the transformed matrix
pca_mod$rotation ## column is an eigenvectors 


#### SNP example

fin = read.table("example_data_2.txt",header=F,stringsAsFactors=F) ## num_snp x people 
fin = t(fin) ## people x num_snp 

# let's look at the 5 SNPs
# what is covariance here? covariance is the D metric. 
cov ( fin[,1:5] ) 

cor ( fin[,1:5] ) ## correlation -->LD

dim (fin) 

# the key question for PCA: can we use few SNPs than 75503

# notice that SNP 1 and 4 are highly correlated 
# what do we keep? do we keep SNP 1 only? it looks like keeping only SNP1 is enough 
# so you can represent the 50 people with 1 less SNP 75502

# what about SNP 2 and 5 ? they are quite strongly correlated, but not strong enough. 
# not easy to decide what SNPs to keep in this case. 
# PCA now comes in. it allows us to decide how to linearly combines the SNPs 

# principal component 

# each SNP goes from to 0-2 so they are on the same scale already. 

pca_mod = prcomp(fin,retx=T) # notice, we don't need to scale or center? scale=T,center=T
plot(pca_mod$x[,1],pca_mod$x[,2])


# ways to cluster
K-mean clustering 
hierachical clustering 
