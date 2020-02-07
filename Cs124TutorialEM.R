

## Let's say we have 1 person, with genotypes 11. Then, there are two cases for this person's haplotype
## case 1, it is 00 and 11
## case 2, it is 01 and 10
## the likelihood will be log(p00 p11 + p01 p10)
## all 4 probabilities are unknown. but for the sake of learning, let's assume p00 and p10 are given to us.
## below, we use EM to solve for p01 and p11.

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

approx_llh = function(p01_inside,alphas) {
	## approximate true log likelihood with jensen inequality, then apply tightest bound
	## suppose we fix everything else at 0.25
	alpha1 = alphas[[1]]
	alpha2 = alphas[[2]]
	p11_inside = .5 - p01_inside ## we will optimize only the inside
	## lower bound looks slightly different from lecture
	return ( alpha1*log(p00*p11_inside/alpha1) + alpha2*log(p01_inside*p10/alpha2))
}

all_possible_value = seq(0.001,.499,by=0.01)
plot(all_possible_value , sapply(all_possible_value,true_llh), type='l', xlab='input value',ylab='objective func value') ## easily see solution by grid search

#### start with some guesses
p00 = .35
p01 = .25 ## want to solve
p10 = .15
p11 = .25 ## want to solve

#### step 1, solve for best p01 in lower bound
alphas = compute_alpha(p00,p01,p10,p00)
points(all_possible_value , sapply(all_possible_value,approx_llh,alphas), type='l',col='red',lwd=2)
legend('topright',c('true likelihood','approx bound'),col=c('black','red'),lty=1)
abline(v=p01)
p01 = .1 ## new best, here, we just eye-ball
p11 = .4 ## new best

#### step 2, update alphas
alphas = compute_alpha(p00,p01,p10,p00)
points(all_possible_value , sapply(all_possible_value,approx_llh,alphas), type='l',col='blue',lwd=2)
# legend('topright',c('true likelihood','approx bound'),col=c('black','blue'),lty=1)
abline(v=p01)
p01 = .05 ## new best, here, we just eye-ball
p11 = .45 ## new best

#### repeat step 1, and then 2 again.
alphas = compute_alpha(p00,p01,p10,p00)
points(all_possible_value , sapply(all_possible_value,approx_llh,alphas), type='l',col='darkviolet',lwd=2)
# legend('topright',c('true likelihood','approx bound'),col=c('black','blue'),lty=1)
abline(v=p01)
p01 = .025 ## new best, here, we just eye-ball
p11 = .475 ## new best
