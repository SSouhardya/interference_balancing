#THIS FILE CANNOT RUN UNLESS YOU HAVE LOADED THE SOURCE CODE


#bunch of possible stochastic interventions
stoc_int<-function(a,X, sd = 1){
	x = as.vector(t(X))
	#prob = pnorm(sqrt(length(X))*mean(X)/sd(X))
	prob = pnorm(mean(x)/sd(x), sd = sd)
	temp = (prob^(sum(a)))*((1-prob)^(length(a) - sum(a)))
	return(temp)	
}

stoc_int1<-function(a,X,sd = 1){
	x = as.vector(t(X))
	prob = pnorm(mean(x)/sd(x), sd = sd)
	propensity = (prob^(sum(a)))*((1-prob)^(length(a) - sum(a)))
	return(propensity)
}

stoc_int2<-function(a,X,sd = 1){
	prob = 0.3
	propensity = (prob^(sum(a)))*((1-prob)^(length(a) - sum(a)))
	return(propensity)
}

stoc_int3<-function(a,X,l,c){
	x = as.vector(t(X))
	prob = pnorm(sum(l*x) + c)
	propensity = (prob^(sum(a)))*((1-prob)^(length(a) - sum(a)))
	return(propensity)
}

stoc_int4<-function(a,X,c){	#note that this does not take the entire list X as input but just the data-frame corresponding to the cluster.
	d = ncol(X)
	n = nrow(X)
	common = sum(apply(X,2,mean))/sqrt(d)
	x = as.vector(t(X)) #this is the shared parameter
	probs = sapply(1:n, function(i){ return(pnorm(common + c*mean(X[i,]))) })
	#prob = pnorm(mean(x)/sd(x) + c)
	propensity = (probs^a)*((1-probs)^(1-a))
	#propensity = (prob^(sum(a)))*((1-prob)^(length(a) - sum(a)))

	return(prod(propensity))
}


generate_trt_pattern1<-function(k,X,sd=1){	#this is one way of generating treatment patterns
	T = list()
	for(i in 1:length(X)){
		x = as.vector(t(X[[i]]))
		prob = pnorm(mean(x)/sd(x), sd = sd)
		T[[i]] = rbinom(k[i],1,prob)	
	}
	return(T)
}

generate_trt_pattern2<-function(k,X,sd=1){	  #this way doesn't take into account the covariate
	prob = 0.3
	T = list()
	for(i in 1:length(k)){
		T[[i]] = rbinom(k[i],1,prob)	
	}
	return(T)
}

generate_trt_pattern3<-function(k,X,l,c){	
	x = as.vector(t(X))
	prob = pnorm(sum(l*x) + c)
	T = list(length = length(k))
	for(i in 1:length(k)){
		T[[i]] = rbinom(k[i],1,prob)	
	}
	return(T)
}

generate_trt_pattern4<-function(k,X,c){	
	T = list()
	d = ncol(X[[1]])
	for(i in 1:length(k)){
		common = sum(apply(X[[i]],2,mean))/sqrt(d)
		probs = sapply(1:k[i], function(j){ return(pnorm(common + c*mean(X[[i]][j,]))) })
		u = runif(k[i])
		T[[i]] = as.numeric(u<probs)
	}
	return(T)
}

g<-function(v){
    return(v/sqrt(sum(v^2))  )
}


k_max = max(k_range[[1]]) #this has to set equal to the maximum value of a possible cluster size.
#k_max is only used if additive == TRUE.

est_sample = 600

f_sample_times = 1000

#k_try = c(1,3,5,7,10)
k_try = c(1,3,5,7)
k_generate = 5
k_hypo = 5

#these will be used only when indiv = TRUE
alpha_try = c(0.1,0.25,0.5,0.75,0.9)
alpha_generate = 0.5
alpha_hypo = 0.5

par_alpha = alpha_hypo
par_prob = 0.3
special_type = 1 #setting this to 0 will disregard the special type of the low-rank structure, and the algorithm will run the entire loop

times = 1

cluster = TRUE #always set this to true when running on the cluster.
only_counterfactual = FALSE

n_max = 1000

set.seed(1)

#gamma_master will be used under FIXED case.
#gamma_generator will be used under INDIV case.

if(graph_experiment){
	gamma_length = (2^k_generate)*p_cov
	if(special_type == 0){
		special_type = NULL
	} else{
		special_type = 'knn'
	}
	gamma_master = sqrt(0.004370974)*(1:(2^k_generate))%x%rep(1,p_cov) 
	gamma_generator<-function(k_generate){
		return(0.011*(1:(2^k_generate))%x%rep(1,p_cov))  #the multiplier was initially 0.05
	}
}

if(stratified_experiment){
	gamma_length = (k_generate+1)*p_cov
	if(special_type == 0){
		special_type = NULL
	} else{
		special_type = 'stratified'
	}
	gamma_master = sqrt(0.1291146)*(1:(k_generate+1))%x%rep(1,p_cov)
	gamma_generator<-function(k_generate){
		return(0.22*(1:(1+k_generate))%x%rep(1,p_cov)) 
	}
}

if(additive_experiment){
	gamma_length = ((2^k_generate) + 2*(k_max - k_generate))*p_cov

	if(special_type == 0){
		special_type = NULL
	} else{
		special_type = 'additive'
	}
	gamma_master = sqrt(0.001071825)*c((1:k_max),rep(0,k_max))%x%rep(1,p_cov)
	gamma_generator<-function(k_generate){
		return(0.02*c((1:k_max),rep(0,k_max))%x%rep(1,p_cov))	
	}
}

#slurm_ind = as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))

#if(cluster){
#	set.seed(slurm_ind)
#	times = 1
#	if(slurm_ind<=500){
#		only_counterfactual = TRUE
#		par_number = 1
#		n_range[1] = n_max
#	} else{
#		only_counterfactual = FALSE
#	}
#} else{
#	set.seed(2024)
#	#set.seed(507)
#}

#if(only_counterfactual){
#	par_number = 1
#	n_range[1] = n_max
#}

if(!exists('SLURM_IND_OVERRIDE')){
     slurm_ind = as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))
} else if(is.null(SLURM_IND_OVERRIDE)){
    slurm_ind = as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))
} else{
    slurm_ind = SLURM_IND_OVERRIDE
}

if(cluster){
	set.seed(slurm_ind)
	times = 1
	if(slurm_ind<=500){
		only_counterfactual = TRUE
		n_range = rep(n_max,par_number)
	} else{
		only_counterfactual = FALSE
	}
} else{
	set.seed(2024)
	#set.seed(507)
}

if(only_counterfactual){
	n_range = rep(n_max,par_number)
}



if(!ci_experiment){
	bias_bal = vector(length = par_number)
	bias_bal_sd = vector(length = par_number)
	sd_bal = vector(length = par_number)
	bias_bal.ipw = vector(length = par_number)
	bias_bal.ipw_sd = vector(length = par_number)
	sd_bal.ipw = vector(length = par_number)
	bias_ipw = vector(length = par_number)
	bias_ipw_sd = vector(length = par_number)
	sd_ipw = vector(length = par_number)
	bias_ME = vector(length = par_number)
	bias_ME_sd = vector(length = par_number)
	sd_ME = vector(length = par_number)
	success = vector(length = par_number)
	counterfactual_est = vector(length = par_number)
	weights_norm.ipw_bal = vector(length = par_number)
	weights_norm.bal = vector(length = par_number)
	weights_norm.ipw = vector(length = par_number)
	weights_norm.ME = vector(length = par_number)
	weighted_signal.ipw_bal = vector(length = par_number)
	weighted_signal.bal = vector(length = par_number)
	weighted_signal.ipw = vector(length = par_number)
	weighted_signal.ME = vector(length = par_number)

	epsilon_list = list()
	ipw_weights_list = list()
	weights_list = list()

	stats_list.bal = list()
	stats_list.ipw_bal = list()


	tic = Sys.time()
	for(i in 1:par_number){
		n = n_range[i]
		#p = p_range[i]
		p_cov = p_cov_range[i]
		k = k_range[[i]]
		sd = sd_range[i] #standard deviation of the error term
		rho = rho_range[i]
		c = c_range[i]
		gamma_scale = gamma_scale_range[i]
		if(length(k) == 1){
			k_vec = rep(k,n)
		} else{
			k_vec = sample(k,n,replace = TRUE)
		}
		print(paste('n = ',n))

		trt_generate<-function(k,X){
			return(generate_trt_pattern4(k,X,0))
		}
		propensity<-function(a,x){
			return(stoc_int4(a,x,0))
		}

		if(graph_experiment){
			row_block_true<-function(v,X,i,j, univariate = indiv){
				return(row_block_knn(v,X,k_generate,i,j, univariate = univariate))
			}
			row_block_hypo<-function(v,X,i,j, univariate = indiv){
				return(row_block_knn(v,X,k_hypo,i,j, univariate = univariate))
			}
			row_block_flexi = row_block_knn
			gamma_ind = NULL
		}

		if(stratified_experiment){
			row_block_true<-function(v,X,i,j, univariate = indiv){
				return(row_block_stratified_knn(v,X,k_generate,i,j, univariate = univariate))
			}
			row_block_hypo<-function(v,X,i,j, univariate = indiv){
				return(row_block_stratified_knn(v,X,k_hypo,i,j, univariate = univariate))
			}
			row_block_flexi = row_block_stratified_knn
			gamma_ind = NULL
		}


		if(additive_experiment){
			row_block_hypo<-function(v,X,i,j,univariate = indiv){
				return(row_block_additive_adapt(v,X,k_hypo,i,j,k_max = k_max, univariate = univariate))
			}
			row_block_true<-function(v,X,i,j, univariate = indiv){
				return(row_block_additive_adapt(v,X,k_generate,i,j,k_max = k_max, univariate = univariate))
			}
			row_block_flexi<-function(v,X,k,i,j,univariate){
				return(row_block_additive_adapt(v,X,k,i,j,k_max,univariate))
			}
			gamma_ind = NULL
		}

		stoc_int<-function(a,x){
			return(stoc_int4(a,x,c))
		}

		f_sampler<-function(a,x){
			return(generate_trt_pattern4(a,x,c))
		}


		if(is.null(gamma_ind)){
		    #gamma = abs(rnorm(gamma_length[i], mean = 2))   
		    #gamma = (1:gamma_length[i])
		    gamma = gamma_scale*(gamma_master[1:gamma_length]) #we want to make its norm exactly equal to scale.
		}

		if(indiv){
			gamma_to_send = gamma_generator
		} else{
			gamma_to_send = gamma
		}

		#if(misspecify){
		#	ww = run_expt(n = n,p_cov = p_cov,k_vec = k_vec,times = times,gamma = gamma_to_send, sd = sd, imbalance = imbalance, row_block_true = row_block_true, row_block_hypo = row_block_hypo, trt_generate = trt_generate, propensity = propensity, stoc_int = stoc_int, print_time = FALSE, only_ipw = FALSE, indiv = indiv, only_counterfactual = only_counterfactual, row_block_flexi = row_block_flexi,k_try = k_try, ADAPT = ADAPT, f_sampler = f_sampler, f_sample_times = f_sample_times, alpha_generate = alpha_generate, alpha_hypo = alpha_hypo,est_bal = est_bal, est_sample = est_sample, special_type =  special_type,par_prob = par_prob,par_alpha = par_alpha)
		#} else{
		#	ww = run_expt(n = n,p_cov = p_cov, k_vec = k_vec,times = times,gamma = gamma_to_send, sd = sd, imbalance = imbalance, row_block_true = row_block_hypo, row_block_hypo = row_block_hypo, trt_generate = trt_generate,propensity = propensity, stoc_int = stoc_int, print_time = FALSE, only_ipw = FALSE, indiv = indiv, only_counterfactual = only_counterfactual, row_block_flexi = row_block_flexi,k_try = k_try, ADAPT = ADAPT, f_sampler = f_sampler, f_sample_times = f_sample_times, alpha_generate = alpha_generate, alpha_hypo = alpha_hypo, est_bal = est_bal, est_sample = est_sample, special_type =  special_type,par_prob = par_prob,par_alpha = par_alpha)
		#}

		ww = run_expt(n = n,p_cov = p_cov,rho = rho, k_vec = k_vec,times = times,gamma = gamma_to_send, sd = sd, imbalance = imbalance, row_block_true = row_block_true, row_block_hypo = row_block_hypo, trt_generate = trt_generate, propensity = propensity, stoc_int = stoc_int, print_time = FALSE, only_ipw = FALSE, indiv = indiv, only_counterfactual = only_counterfactual, row_block_flexi = row_block_flexi,k_try = k_try, ADAPT = ADAPT, f_sampler = f_sampler, f_sample_times = f_sample_times, alpha_generate = alpha_generate, alpha_hypo = alpha_hypo,est_bal = est_bal, est_sample = est_sample, special_type =  special_type,par_prob = par_prob,par_alpha = par_alpha)



		if(only_counterfactual){
			counterfactual_est[i] = ww
		} else{
			bias_bal[i] = mean(ww[[1]][which(!is.na(ww[[1]]))])#/sd(ww[[3]][,1])
			bias_bal_sd[i] = sd(ww[[1]][which(!is.na(ww[[1]]))])#/sd(ww[[3]][,1])
			sd_bal[i] = sd(ww[[1]][which(!is.na(ww[[1]]))] + ww[[4]][which(!is.na(ww[[1]]))])

			bias_bal.ipw[i] = mean(ww[[2]][which(!is.na(ww[[2]]))])#/sd(ww[[3]][,1])
			bias_bal.ipw_sd[i] = sd(ww[[2]][which(!is.na(ww[[2]]))])#/sd(ww[[3]][,1])
			sd_bal.ipw[i] = sd(ww[[2]][which(!is.na(ww[[2]]))] + ww[[4]][which(!is.na(ww[[2]]))])

			bias_ipw[i] = mean(ww[[3]])#/sd(ww[[3]][,1])
			bias_ipw_sd[i] = sd(ww[[3]])#/sd(ww[[3]][,1])
			sd_ipw[i] = sd(ww[[3]] + ww[[4]])

			bias_ME[i] = mean(ww[[14]])#/sd(ww[[3]][,1])
			bias_ME_sd[i] = sd(ww[[14]])#/sd(ww[[3]][,1])
			sd_ME[i] = sd(ww[[14]] + ww[[4]])


			success[i] = mean(ww[[1]]!=-1)

			if(ADAPT){
				stats_list.bal[[i]] = ww[[5]]
				stats_list.ipw_bal[[i]] = ww[[6]]
			} else{
				stats_list.bal[[i]] = NA
				stats_list.ipw_bal[[i]] = NA

				weights_norm.bal[i] = mean(ww[[5]][which(!is.na(ww[[5]]))])
				weights_norm.ipw_bal[i] = mean(ww[[6]])
				weights_norm.ipw[i] = mean(ww[[7]])
				weights_norm.ME[i] = mean(ww[[15]])
				weighted_signal.bal[i] = mean(ww[[8]][which(!is.na(ww[8]))])
				weighted_signal.ipw_bal[i] =  mean(ww[[9]])
				weighted_signal.ipw[i] =  mean(ww[[10]])
				weighted_signal.ME[i] =  mean(ww[[16]])
			}
			epsilon_list[[i]] = ww[[11]]
			ipw_weights_list[[i]] = ww[[12]]
			weights_list[[i]] = ww[[13]]
		}
	}


	if(!cluster){
		bias_bal_sd = bias_bal_sd/sqrt(success*times)
		bias_bal.ipw_sd = bias_bal.ipw_sd/sqrt(times)
		bias_ipw_sd = bias_ipw_sd/sqrt(times)
	}

	if(only_counterfactual){
		bundle = counterfactual_est
	} else{
		bundle = list(bias_bal, bias_bal_sd, sd_bal, bias_bal.ipw, bias_bal.ipw_sd, sd_bal.ipw, bias_ipw, bias_ipw_sd, sd_ipw, stats_list.bal, stats_list.ipw_bal, weights_norm.bal, weights_norm.ipw_bal, weights_norm.ipw , weighted_signal.bal, weighted_signal.ipw_bal, weighted_signal.ipw, epsilon_list, ipw_weights_list, weights_list,  bias_ME, bias_ME_sd, sd_ME, weights_norm.ME, weighted_signal.ME)
		names(bundle) = c('bias_bal', 'bias_bal_sd', 'sd_bal', 'bias_bal.ipw', 'bias_bal.ipw_sd', 'sd_bal.ipw', 'bias_ipw', 'bias_ipw_sd', 'sd_ipw', 'stats_list.bal', 'stats_list.ipw_bal', 'weights_norm.bal', 'weights_norm.ipw_bal', 'weights_norm.ipw', 'weighted_signal.bal', 'weighted_signal.ipw_bal', 'weighted_signal.ipw', 'epsilon_list', 'ipw_weights_list', 'weights_list', 'bias_ME', 'bias_ME_sd', 'sd_ME', 'weights_norm.ME', 'weighted_signal.ME')	
	}

	if(cluster){
		saveRDS(bundle, paste0('run_',slurm_ind,'.list'))
	}

	toc = Sys.time()
	print(toc - tic)
}

if(ci_experiment){

	coverage = 0.95

	CI_or_estimates = list() #this would either contain the CI or the counterfactuals depending on the argument only_counterfactual.

	tic = Sys.time()
	for(i in 1:par_number){
		n = n_range[i]
		p_cov = p_cov_range[i]
		k = k_range[[i]]
		sd = sd_range[i] #standard deviation of the error term
		rho = rho_range[i]
		c = c_range[i]
		gamma_scale = gamma_scale_range[i]
		if(length(k) == 1){
			k_vec = rep(k,n)
		} else{
			k_vec = sample(k,n,replace = TRUE)
		}
		print(paste('n = ',n))

		trt_generate<-function(k,X){
			return(generate_trt_pattern4(k,X,0))
		}
		propensity<-function(a,x){
			return(stoc_int4(a,x,0))
		}

		if(graph_experiment){
			row_block_true<-function(v,X,i,j, univariate = indiv){
				return(row_block_knn(v,X,k_generate,i,j, univariate = univariate))
			}
			row_block_hypo<-function(v,X,i,j, univariate = indiv){
				return(row_block_knn(v,X,k_hypo,i,j, univariate = univariate))
			}
			row_block_flexi = row_block_knn
			gamma_ind = NULL
		}

		if(stratified_experiment){
			row_block_true<-function(v,X,i,j, univariate = indiv){
				return(row_block_stratified_knn(v,X,k_generate,i,j, univariate = univariate))
			}
			row_block_hypo<-function(v,X,i,j, univariate = indiv){
				return(row_block_stratified_knn(v,X,k_hypo,i,j, univariate = univariate))
			}
			row_block_flexi = row_block_stratified_knn
			gamma_ind = NULL
		}
 

		if(additive_experiment){
			row_block_hypo<-function(v,X,i,j,univariate = indiv){
				return(row_block_additive_adapt(v,X,k_hypo,i,j,k_max = k_max, univariate = univariate))
			}
			row_block_true<-function(v,X,i,j, univariate = indiv){
				return(row_block_additive_adapt(v,X,k_generate,i,j,k_max = k_max, univariate = univariate))
			}
			row_block_flexi<-function(v,X,k,i,j,univariate){
				return(row_block_additive_adapt(v,X,k,i,j,k_max,univariate))
			}
			gamma_ind = NULL
		}

		stoc_int<-function(a,x){
			return(stoc_int4(a,x,c))
		}
		f_sampler<-function(a,x){
			return(generate_trt_pattern4(a,x,c))
		}
		if(is.null(gamma_ind)){
		    gamma = gamma_scale*(gamma_master[1:gamma_length]) #we want to make its norm exactly equal to scale.
		}

		if(indiv){
			gamma_to_send = gamma_generator
		} else{
			gamma_to_send = gamma
		}
	    #print('a')
		#I am not going to specify a mis-specify argument here. I will always assume that row_block_true is used to generate the treatment while row_block_hypo is the assumed one.
		#if(misspecify){
		#	CI = run_expt_var_est(n = n,p_cov = p_cov,k_vec = k_vec,times = times,gamma = gamma_to_send, sd = sd, imbalance = imbalance,row_block_true = row_block_true,row_block_hypo = row_block_hypo,trt_generate = trt_generate,propensity = propensity,stoc_int = stoc_int,coverage = coverage, print_time = FALSE, indiv = indiv, only_counterfactual = only_counterfactual, row_block_flexi = row_block_flexi,k_try = k_try, ADAPT = ADAPT, f_sampler = f_sampler, f_sample_times = f_sample_times, alpha_generate = alpha_generate, alpha_hypo = alpha_hypo, est_bal = est_bal, est_sample = est_sample, special_type = special_type,par_prob = par_prob, par_alpha = par_alpha)
		#} else{
		#	CI = run_expt_var_est(n = n,p_cov = p_cov,k_vec = k_vec,times = times,gamma = gamma_to_send, sd = sd, imbalance = imbalance,row_block_true = row_block_hypo,row_block_hypo = row_block_hypo,trt_generate = trt_generate,propensity = propensity,stoc_int = stoc_int,coverage = coverage, print_time = FALSE, indiv = indiv, only_counterfactual = only_counterfactual, row_block_flexi = row_block_flexi,k_try = k_try, ADAPT = ADAPT, f_sampler = f_sampler, f_sample_times = f_sample_times, alpha_generate = alpha_generate, alpha_hypo = alpha_hypo, est_bal = est_bal, est_sample = est_sample, special_type = special_type,par_prob = par_prob, par_alpha = par_alpha)
		#}

		CI = run_expt_var_est(n = n,p_cov = p_cov,rho = rho, k_vec = k_vec,times = times,gamma = gamma_to_send, sd = sd, imbalance = imbalance,row_block_true = row_block_true,row_block_hypo = row_block_hypo,trt_generate = trt_generate,propensity = propensity,stoc_int = stoc_int,coverage = coverage, print_time = FALSE, indiv = indiv, only_counterfactual = only_counterfactual, row_block_flexi = row_block_flexi,k_try = k_try, ADAPT = ADAPT, f_sampler = f_sampler, f_sample_times = f_sample_times, alpha_generate = alpha_generate, alpha_hypo = alpha_hypo, est_bal = est_bal, est_sample = est_sample, special_type = special_type,par_prob = par_prob, par_alpha = par_alpha)

		#print('b')
		CI_or_estimates[[i]] = CI
	}

	if(cluster){
		saveRDS(CI_or_estimates, paste0('run_',slurm_ind,'.list'))
	}

	toc = Sys.time()
	print(toc - tic)
}
