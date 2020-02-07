

p00 = .35
p01 = .25 ##
p10 = .15
p11 = .25 ##

true_llh = function (p01){
	p11 = .5 - p01 # suppose we fix everything else at 0.25
	return (log(p00*p11 + p01*p10))
}

compute_alpha = function(p00,p01,p10,p11){
	alpha1 = p00*p11
	alpha2 = p01*p10
	scale_factor = alpha1+alpha2
	alpha1 = alpha1/scale_factor
	alpha2 = alpha2/scale_factor
	return(list(alpha1,alpha2)) 
}
	
approx_llh = function(p01_inside) {
	## approximate true log likelihood with jensen inequality, then apply tightest bound
	## suppose we fix everything else at 0.25
	alphas = compute_alpha(p00,p01,p10,p00) 
	alpha1 = alphas[[1]]
	alpha2 = alphas[[2]]
	p11_inside = .5 - p01_inside ## we will optimize only the inside
	## lower bound looks slightly different from lecture
	return ( alpha1*log(p00*p11_inside/alpha1) + alpha2*log(p01_inside*p10/alpha2)) 
}

## step 1, solve for best p01 in lower bound
all_possible_value = seq(0.001,.499,by=0.01)
plot(all_possible_value , sapply(all_possible_value,true_llh), type='l') ## easily see solution by grid search
points(all_possible_value , sapply(all_possible_value,approx_llh), type='l',col='red')
legend('topright',c('true likelihood','approx bound'),col=c('black','red'),lty=1)
abline(v=p01)

