# Unit tests for composition theorems


#Global Epsilon by Advanced Composition Theorem:  0.07650509

source("CompositionTheorems.R")

# Fix initial settings for epsilons and deltas, homogeneous case. 

delta_global <- 2^-20
eps_list <- rep(.002, times = 50)
del_list <- rep(delta_global/100, times = 50)

params <- cbind(eps_list, del_list)

# Run all three composition theorems
cat("Global Epsilon by Advanced Composition Theorem: ", advanced_composition(params, delta_global))
cat("\n\nGlobal Epsilon for Theorem 3.4: ")
thm_3.4(params, del, print=TRUE)
cat("\n\nGlobal Epsilon for Optimal Composition Theorem: ")
optimal_composition(params, del, print=TRUE)

# Fix settings for heterogenous case. Uniform random sample of 20 numbers between 0 and .1.

eps_list <- c(0.001399624, 0.001919920, 0.009980242, 0.002975300, 0.009726829, 0.009986694, 0.007891922, 0.006054512, 0.004091431, 0.009703885, 0.003287346, 0.002464207, 0.003008644, 0.007272591, 0.003387047, 0.002694358, 0.002561664, 0.003824803, 0.003911608, 0.006459682)

del_list <- rep(1 - (1 - del)^(1/(2*k)), times = 20)

params <- cbind(eps_list, del_list)

# Run Optimal Composition Theorem
cat("\n\nGlobal Epsilon for Optimal Composition Theorem, Heterogenous Case:")
optimal_composition(params, delta_global, print=TRUE)



########################################
#  Expected output:
# Global Epsilon by Advanced Composition Theorem:  0.07650509
#
# Global Epsilon for Theorem 3.4: 
# First term:  0.1
# Second term:  0.0643797
# Third term:  0.07650489
# Final result:  0.0643797
#
# Global Epsilon for Optimal Composition Theorem: 
# First term:  0.1
# Second term:  0.0642797
# Third term:  0.07640489
# Final result:  0.0642797
#
# Global Epsilon for Optimal Composition Theorem, Heterogenous Case:
# First term:  0.1026023
# Second term:  0.1234645
# Third term:  0.142494
# Final result:  0.1026023
#
##########################################




