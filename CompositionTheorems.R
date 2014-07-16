#This code contains three functions to compute the three different composition theorems. 


# Supposedly the optimal composition possible (Theorem 3.5, [Oh, Viswanath, 2013]). Works for 
# different settings of epsilons and deltas. 

optimal_composition <- function(params, del, print = FALSE){
	#Compute the optimal composition theorem for the given epsilon_i and delta_i pairs
	# 
	# Args:
	#	params: a kx2 matrix where the first column corresponds to epsilon values and the second 
	# 			corresponds to delta values. 
	#	del: global delta value
	#   
	# Returns:
	#	global epsilon value
	#
	
	# Compute different components of the theorem:
	
	k <- length(params[ , 1])
	

	
	del_bar <- 1 - sqrt(1 - del)
	del_i <- 1 - (1 - del)^(1/(2*k))   # Set del_i's to this in params if you're spreading evenly
	del_product <- del_i*k
	
	sum_of_squares <- 0
	for(i in 1:k){
		sum_of_squares <- sum_of_squares + params[i,1]^2
	}
	
	fun <- function(x){
		return(((exp(x) - 1)*x)/(exp(x) + 1))		
	}
	
	first_term <- sum(sapply(params[,1], FUN=fun))
	
	# a, b, and c will correspond to the first, second, and third expressions in the 
	# optimal composition theorem. The minimum is returned.
	
	a <- sum(params[,1])
	b <- first_term + sqrt((2*log(exp(1)+ (sqrt(sum_of_squares)/del_bar)))*sum_of_squares)
	c <- first_term + sqrt(2*log(1/del_bar)*sum_of_squares)
	
	# For testing purposes
	if(print){
	cat("\nFirst term: ", a)
	cat("\nSecond term: ",b)
	cat("\nThird term: ",c)
	cat("\nFinal result: ", min(a,b,c))
	}
	
	return(min(a,b,c))
	
}



thm_3.4 <- function(params, del, print = FALSE){
	# The optimal composition theorem when all epsilons are the same and 
	# all deltas are the same and in the "high privacy regime" when eps_global <= .9
	# (homogenous case) (Theorem 3.4, [Oh, Viswanath, 2013]). 
	# 
	# Args:
	#	params: a kx2 matrix where the first column corresponds to epsilon values and the second 
	# 			corresponds to delta values. Note: this function really only needs one epsilon, delta
	#			pair since they will all be equal anyway. However for ease of use and testing, I wanted all
	#			of the composition functions to have the same signature.
	#	del: global delta value
	#   
	# Returns:
	#	global epsilon value
	#
	
	k <- length(params[ , 1])
	
	del_bar <- 1 - sqrt(1 - del) # set del_i's equal to 1 - (1 - del)^(-2k)
	
	eps_i <- params[1,1]
	
	a <- k*eps_i
	term <- a*eps_i
	
	b <- term + eps_i*sqrt(2*k*log(exp(1) + (sqrt(term)/del_bar)))
	c <- term + eps_i*sqrt(2*k*log(1/del_bar))
	
	# For testing purposes	
	if(print){
	cat("\nFirst term: ", a)
	cat("\nSecond term: ",b)
	cat("\nThird term: ",c)
	cat("\nFinal result: ", min(a,b,c))
	}
	
	return(min(a,b,c))
}


advanced_composition <- function(params, del){
	# Compute the advanced composition theorem for the homogenous case. [Dwork, Rothblum, Vadhan, 2010]
	# 
	# Args:
	#	params: a kx2 matrix where the first column corresponds to epsilon values and the second 
	# 			corresponds to delta values. 
	#	del: global delta value
	#
	# Returns:
	#	global epsilon value
	#
	
	k <- length(params[,1])
	del_bar <- del/2   # Set individual deltas equal to del/2k
	eps_i <- params[1,1]
	
	term1 <- k*eps_i*(exp(eps_i) -1)
	term2 <- eps_i*sqrt(2*k*log(1/del_bar))
	return(term1 + term2)
	
}
