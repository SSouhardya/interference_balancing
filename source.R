require(quadprog)
require(binaryLogic)
library(corpcor)
library(MASS)
library(Matrix)
library(expm)
library(lme4)



row_block_additive<-function(v,X, i = NULL, j = NULL, k_max = 10, univariate = FALSE){	#this is the additive low-rank structure assumption and is the same for all i and j for this particular low-rank assumption.
	#X is a list, as per the convention we are following.
	cov_length = length(X[[i]][j,])
	clust_size = nrow(X[[i]])
	u = c(v, rep(0,k_max-clust_size))
	u_c = c(1-v, rep(0,k_max-clust_size))
	if(!univariate){
		return(t(c(u,u_c))%x%diag(cov_length))
	} else{
		return(t(c(u,u_c)))
	}
}

row_block_additive_adapt<-function(v,X, k = 3, i = NULL, j = NULL, k_max = 10, univariate = FALSE){	#this is the additive low-rank structure assumption and is the same for all i and j for this particular low-rank assumption.
	k = 1 #I AM EXPLICITLY SETTING K = 1.
	if(k == 1){
		return(row_block_additive(v,X,i,j, k_max = k_max, univariate = univariate))
	}
	cov_length = length(X[[i]][j,])
	clust_size = nrow(X[[i]])
	v_id = v[1:k]
	v_rest = v[-(1:k)]
	u_id = row_block_id(v_id,X,i = i, j = j, univariate = TRUE)
	u = c(v_rest, rep(0,k_max-clust_size))
	u_c = c(1-v_rest, rep(0,k_max-clust_size))
	if(!univariate){
		return(t(c(u_id,u,u_c))%x%diag(cov_length))
	} else{
		return(t(c(u_id,u,u_c)))
	}
}

row_block_additive_flexible<-function(v,X, m = 1, i = NULL, j = NULL){
	if(m == length(v)){
		return(row_block_id(v,X,i,j))
	}
	if(m == 1){
		return(row_block_additive(v,X,i,j))
	}
	v1 = v[1:m]
	v2 = v[(m+1): length(v)]
	row_mat = cbind(row_block_id(v1,X,i,j), row_block_additive(v2,X,i,j))
	return(row_mat)
}

row_block_id<-function(v, X, i = NULL, j = NULL, univariate = FALSE){
	cov_length = length(X[[i]][j,])
	v_dec = sum((2^((length(v)-1):0 ))*v) + 1
	vec = rep(0,2^length(v))
	vec[v_dec] = 1
	if(!univariate){
		return(t(vec)%x%diag(cov_length))
	} else{
		return(t(vec))
	}
}

row_block_knn<-function(v,X,knn = 3, i = 1, j = 1, univariate = FALSE){
	#cov_length is the length of each of the covariates of each of the units, by default is 1
	cov_length = length(X[[i]][j,])
	X_covariates = X[[i]]
	dist_mat = as.matrix(dist(X_covariates))
	dist_row = dist_mat[j,]
	v_star = v[order(dist_row)[1:knn]]
	v_dec = sum((2^((length(v_star)-1):0 ))*v_star) + 1
	vec = rep(0,2^length(v_star))
	vec[v_dec] = 1
	if(!univariate){
		return(t(vec)%x%diag(cov_length))
	} else{
		return(t(vec))
	}
}

row_block_stratified_knn<-function(v,X,knn = 3, i = 1, j = 1, univariate = FALSE){
	#cov_length is the length of each of the covariates of each of the units, by default is 1
	cov_length = length(X[[i]][j,])
	X_covariates = X[[i]]
	dist_mat = as.matrix(dist(X_covariates))
	dist_row = dist_mat[j,]
	v_sum = sum(v[order(dist_row)[1:knn]])
	vec = rep(0,knn+1)
	vec[v_sum+1] = 1
	if(!univariate){
		return(t(vec)%x%diag(cov_length))
	} else{
		return(t(vec))
	}
}



#utility functions
bin_pattern<-function(a,k){
	#a should range from 0 to 2^k-1
	vec = as.binary(a)
	vec = c(rep(0, k - length(vec)), vec)
	return(vec)
}

 

# Mixed effects model for estimating the propensity score
mixed_effects_estimation_old<-function(X,T,stoc_int){
	X_grand = do.call(rbind,X)
	T_grand = do.call(base::c,T)
	k_vec = sapply(T, length)
	k_vec_rep = do.call(base::c,lapply(k_vec,function(k){return(rep(k,k))}))
	cluster_identity = do.call(base::c,lapply(1:length(k_vec),function(i){return(rep(i,k_vec[i]))}))
	cluster_identity = as.factor(cluster_identity)
	dat = data.frame(y = T_grand, X = X_grand, cluster_identity = cluster_identity)
	#fit = glmer(y~.-cluster_identity-1+(1|cluster_identity), data = dat, family = binomial(link = 'probit'))
	fit = glmer(y~.-cluster_identity-1, data = dat, family = binomial(link = 'probit'))
	p_hat = predict(fit, type = "response")
	hat.propensity = T_grand*p_hat + (1-T_grand)*(1-p_hat)
	stoc_int.scores = vector(length = length(X))
	for(i in 1:length(X)){
		stoc_int.scores[i] = stoc_int(T[[i]], X[[i]])
	}
	stoc_int.scores_repeated = do.call(base::c,lapply(1:length(X),function(i){return(rep(stoc_int.scores[i],k_vec[i]))}))
	fitted_weights = unname(stoc_int.scores_repeated/(hat.propensity*k_vec_rep))
	#fitted_weights = unname(stoc_int.scores_repeated/(hat.propensity))
	return(fitted_weights)
}


mixed_effects_estimation<-function(X,T,stoc_int, modify_cluster_cov = TRUE){
	X_original = X
	if(modify_cluster_cov){
		for(i in 1:length(X)){
			dat_temp = X_original[[i]]
			common_part = apply(dat_temp,2,mean)
			specific_part = apply(dat_temp,1,mean)
			X[[i]] = as.data.frame(cbind(as.vector(rep(1,nrow(X[[i]])))%*%t(as.vector(common_part)), specific_part))
		}
	}
	X_grand = do.call(rbind,X)
	T_grand = do.call(base::c,T)
	k_vec = sapply(T, length)
	k_vec_rep = do.call(base::c,lapply(k_vec,function(k){return(rep(k,k))}))
	cluster_identity = do.call(base::c,lapply(1:length(k_vec),function(i){return(rep(i,k_vec[i]))}))
	cluster_identity = as.factor(cluster_identity)
	dat = data.frame(y = T_grand, X = X_grand, cluster_identity = cluster_identity)
	fit = glmer(y~.-cluster_identity-1+(1|cluster_identity), data = dat, family = binomial(link = 'probit'))
	p_hat = predict(fit, type = "response")
	hat.propensity = T_grand*p_hat + (1-T_grand)*(1-p_hat)
	cluster_propensity = sapply(1:length(X),function(i){
		pp = hat.propensity[which(as.numeric(cluster_identity) == i)]
		return(prod(pp))
		})
	stoc_int.scores = vector(length = length(X))
	for(i in 1:length(X_original)){
		stoc_int.scores[i] = stoc_int(T[[i]], as.matrix(X_original[[i]]))
	}
	ME.scores_repeated = do.call(base::c,lapply(1:length(X),function(i){return(rep(stoc_int.scores[i]/cluster_propensity[i],k_vec[i]))}))
	fitted_weights = unname(ME.scores_repeated/(k_vec_rep))
	#fitted_weights = unname(stoc_int.scores_repeated/(hat.propensity))
	return(fitted_weights)
}





#----------------- CODES FOR ESTIMATING THE CAUSAL EFFECT ----------------------------------#
#-------------------------------------------------------------------------------------------#

#this is the master function and returns the fitted weights
balance_low_rank_lm<-function(X,T,row_block,stoc_int,imbalance = 0,digits = 10, k = NULL, known_ipw = FALSE, ipw_weights = NULL, display_time = FALSE, tol = 10e-9, est_bal = FALSE, est_sample = 100, propensity = NULL, sampler = NULL, return_Z = FALSE){
	#X and T are lists
	#row_block is a function that outputs the \Lambda_{ij}(\bm a) matrix
	#stoc_int is the target stochastic intervention

	tic = Sys.time()
	n = length(X)	#the total number of clusters
	if(is.null(k)){
		k = sapply(T,length)	#a vector of all cluster sizes (NOT covariate sizes)
	}
	if(is.null(ipw_weights)){
		known_ipw = FALSE
		warning('IPW weights not specified. Will only return the balancing weights')
	}

	if(display_time){
		print('Creating Z matrix:')
	}
	Z = NULL
	for(i in 1:length(X)){
		for(j in 1:k[i]){
			print(paste0('Count: (',i,',',j,')'))
			temp = t(row_block(T[[i]],X,i,j))%*%as.numeric(X[[i]][j,])
			Z = cbind(Z,temp)
		}
	}
	Z = t(Z) #this is the intendent Z matrix
	EX <<- X
	LAMBDA<<-Z
	if(known_ipw){
		#print(1)
		if(is.null(ipw_weights)){
			stop('Must specify the IPW weights if known_ipw is TRUE')
		}
		weights.ipw = as.vector(Z%*%pseudoinverse(Z)%*%ipw_weights) #remember weights.ipw incorporates the low-rank structural information.
	} else{
		weights.ipw = NA
	}

	if(!est_bal){
		inner_sums.bal = list()
		sum_outer = 0
		tic2 = Sys.time()
		for(i in 1:n){
			Lambda_dim = dim(t(stoc_int(bin_pattern(1-1,k[i]), X[[i]])*row_block(bin_pattern(1-1,k[i]), X,i,1)/k[i]))
			sum_inner = 0
			for(j in 1:k[i]){
				for(a in 1:2^(k[i])){
					sum_inner = sum_inner + as.vector(stoc_int(bin_pattern(a-1,k[i]), X[[i]])*(rep(row_block(bin_pattern(a-1,k[i]), X,i,j, univariate = TRUE), each = p_cov)*rep(X[[i]][j,], times = Lambda_dim[1]/p_cov) )/k[i])
				}
			}
			inner_sums.bal[[i]] = sum_inner
			sum_outer = sum_outer + sum_inner
		}
		sum_outer = as.vector(sum_outer)
		toc2 = Sys.time()
		if(display_time){
		    print(toc2 - tic2)
		}
	}

	if(est_bal){
		inner_sums.bal = list()
		sum_outer = 0
		sum_outer2 = 0

		if(display_time){
			print('Evaluating balancing components: ')
		}
		tic2 = Sys.time()
		for(i in 1:n){
			X_reps = replicate(est_sample,X[[i]],simplify = FALSE)
			trt_pat = sampler(rep(k[i],est_sample), X_reps)
			Lambda_dim = dim(t(stoc_int(bin_pattern(1-1,k[i]), X[[i]])*row_block(bin_pattern(1-1,k[i]), X,i,1)/k[i]))
			sum_inner = 0
			sum_inner2 = 0
			for(j in 1:k[i]){
				if(display_time){
					#print(paste0('Count: (',i,',',j,')'))
				}
				sum_inner_sampler = 0
				sum_inner_sampler2 = 0
				for(iter in 1:est_sample){
					temp = as.vector((rep(row_block(trt_pat[[iter]], X,i,j, univariate = TRUE), each = p_cov)*rep(as.numeric(X[[i]][j,]), times = Lambda_dim[1]/p_cov) )/(1*k[i]) )

					sum_inner_sampler = sum_inner_sampler + temp
					sum_inner_sampler2 = sum_inner_sampler2 + temp^2  #this is not used anywhere and can be removed
				}
				sum_inner = sum_inner + sum_inner_sampler/est_sample
				sum_inner2 = sum_inner2 + sum_inner_sampler2/est_sample #this is not used anywhere and can be removed
			}
			inner_sums.bal[[i]] = sum_inner
			sum_outer = sum_outer + sum_inner
			sum_outer2 = sum_outer2 + sum_inner^2
		}
		sum_outer = as.vector(sum_outer)
		sum_outer2 = as.vector(sum_outer2)
		toc2 = Sys.time()
		if(display_time){
		    print(toc2 - tic2)
		}
	}

	target = as.vector(sum_outer)
	target2 = as.vector(sum_outer2)

	if(display_time){
		print('Obtaining pseudoinverse: ')
	}
	tic5 = Sys.time()
	weights.bal = as.vector(pseudoinverse(t(Z))%*%target)
	toc5 = Sys.time()
	if(display_time){
	    print(toc5 - tic5)
	}
	weights.bal_pseudoinv = weights.bal
	weights.bal1 = NULL

	if(sum(abs(target - t(Z)%*%weights.bal))>tol){	#shows that balance couldn't be achieved
		weights.bal = NA
		warning('Could not achieve balance')
	}
	toc = Sys.time()
	if(display_time){
		print(toc - tic)
	}

	weights_ipw.out <<- ipw_weights
	weights.out <<- weights.ipw

	#calculate the mixed effect weights
	weights.ME = mixed_effects_estimation(X,T,stoc_int)

	if(return_Z){
		Z_toret = Z
		target_toret = as.vector(target)
		target2_toret = as.vector(target2)
	} else{
		Z_toret = NA
		target_toret = NA
		target2_toret = NA
	}
	return_bundle = list(as.vector(weights.ipw), as.vector(weights.bal), as.vector(weights.bal1), inner_sums.bal, as.vector(weights.bal_pseudoinv), Z_toret, target_toret, target2_toret, as.vector(weights.ME))
	names(return_bundle) = c('weights.ipw', 'weights.bal', 'weights.bal1', 'inner_sums.bal', 'weights.bal_pseudoinv', 'Z_toret', 'target_toret', 'target2_toret', 'weights.ME')
	return(return_bundle)
}


trt_effect_lm<-function(y_obs,X,T,row_block,stoc_int,imbalance=0,digits = 10, k = NULL, known_ipw = TRUE, ipw_weights = NULL, display_time = FALSE, est_bal = FALSE, est_sample=100, propensity = NULL, sampler=NULL){
	#the y vector needs to sorted in lexicographic order in terms of (i,j)'s. This is intended to be the observed y's instead of all possible potential outcomes.
	n = length(X)
	if(is.null(k)){
		k = as.vector(sapply(T,length))
	}
	weights = balance_low_rank_lm(X,T,row_block,stoc_int,imbalance=imbalance,digits = digits, k = k, known_ipw = known_ipw, ipw_weights = ipw_weights, display_time = display_time, est_bal = est_bal, est_sample = est_sample, propensity = propensity, sampler = sampler)
	te_est.ipw = sum(weights[[1]]*y_obs)/n	#the estimated treatment effect under the stochastic interference
	te_est.bal = sum(weights[[2]]*y_obs)/n
	te_est.ME = sum(weights[[9]]*y_obs)/n

	#print('weights:')
	#print(weights)
	#print('y_obs:')
	#print(y_obs)

	y_out <<- y_obs

	return(list(c(te_est.ipw, te_est.bal, te_est.ME), weights))
}



#----------------- CODES FOR GENERATING ASYMPTOTIC CI ----------------------------------#
#---------------------------------------------------------------------------------------#

y_weighted_global = NULL

asymp_CI_lm<-function(y_observed,X,T,row_block,stoc_int, propensity = NULL,coverage = 0.95, indiv = FALSE, return_weights = FALSE, est_bal = FALSE, est_sample = 100, sampler = NULL,alpha_hypo = NULL, special_type = NULL, par_prob = NULL, par_alpha = NULL){ #this will return both the confidence intervals if ipw weights are supplied
	#remember the ipw_weights you feed in here should not be normalized by n
	#first get the weights, the first component is the IPW projection weight and the second one is the balancing weight.
	n = length(T)
	k_vec = sapply(T, length)
	quantile = qnorm((1-coverage)/2, lower.tail = FALSE)

	if(is.null(propensity)){
		ipw_weights = NULL
	} else{
		ipw = vector(length = n)
		for(i in 1:n){
			ipw[i] = stoc_int(T[[i]],X[[i]])/(propensity(T[[i]], X[[i]]))
		}
		ipw_weights = NULL
		for(i in 1:n){
			ipw_weights = c(ipw_weights, rep(ipw[i],k_vec[i])/k_vec[i])
		}
	}

	w = balance_low_rank_lm(X,T,row_block,stoc_int,known_ipw = TRUE, ipw_weights = ipw_weights, est_bal = est_bal, est_sample = est_sample, propensity = propensity, sampler = sampler)
	# JUST FOR REFERENCE, DELETE: balance_low_rank_lm<-function(X,T,row_block,stoc_int,imbalance = 0,digits = 10, k = NULL, known_ipw = FALSE, ipw_weights = NULL, display_time = FALSE, tol = 10e-9, est_bal = FALSE, est_sample = 100, propensity = NULL, sampler = NULL){

	Z = NULL
	for(i in 1:length(X)){
		for(j in 1:k_vec[i]){
			temp = t(row_block(T[[i]],X,i,j))%*%X[[i]][j,] #we are assuming that the outcome regression is a linear model.
			Z = cbind(Z,temp)
		}
	}
	Z = t(Z)
	L = c(as.vector(pseudoinverse(Z)%*%y_observed), -1) #this is the L-vector

	func<-function(w, type = 'bal',inner_sums.bal = NULL){
		eta = matrix(0, nrow = n, ncol = length(L))
		for(i in 1:n){
			if(i == 1){
				indices = 1:k_vec[1]
			} else{
				indices = (sum(k_vec[1:(i-1)])+1):sum(k_vec[1:i])#indices of the units corresponding to the current cluster
			}
			w_c = w[indices]
			Lambda = NULL
			for(j in 1:k_vec[i]){
				temp = t(row_block(T[[i]],X,i,j))%*%X[[i]][j,]
				Lambda = cbind(Lambda,temp)
			}
			Lambda = t(Lambda)
			y_c = y_observed[indices]
			if(type == 'bal'){
			sum_inner = 0
				eta[i,] = c(as.vector(t(Lambda)%*%w_c - inner_sums.bal[[i]]), sum(y_c*w_c))
			} else{
				w.ipw_c = ipw_weights[indices]
				eta[i,] = c(as.vector(t(Lambda)%*%w_c - t(Lambda)%*%w.ipw_c), sum(y_c*w_c))
			}
		}
		#eta[,3] = eta[,3] - mean(eta[,3])
		#the above line was probably wrong. I am changinh it now.
		eta[,ncol(eta)] = eta[,ncol(eta)] - mean(eta[,ncol(eta)])
		var.est = mean((eta%*%L)^2)
		return(var.est)
	}
	if(sum(is.na(w[[2]]))>0){
		est.bal = NA
		var_est.bal = NA
		ci.bal = c(NA, NA)
	} else{
		est.bal = sum(w[[2]]*y_observed)/n
		var_est.bal = func(w[[2]], type = 'bal',inner_sums.bal = w[[4]])
		ci.bal = est.bal + c(-1,1)*(quantile*sqrt(var_est.bal)/sqrt(n))
	}
	if(!is.null(ipw_weights)){
		est.ipw_bal = sum(w[[1]]*y_observed)/n
		var_est.ipw_bal = func(w[[1]], type = 'ipw.bal')
		ci.ipw_bal = est.ipw_bal + c(-1,1)*(quantile*sqrt(var_est.ipw_bal)/sqrt(n))
	} else{
		est.ipw_bal = NA
		var_est.ipw_bal = NA
		ci.ipw_bal = c(NA,NA)
	}
	#now obtain the CI using the mixed effects model estimated weights
	y_weighted = y_observed*w[[9]]
	y_weighted_global <<- y_weighted
	#print('y_weighted_sum')
	#print(summary(y_weighted))
	y_cluster = vector(length = length(k_vec))
	for(i in 1:length(k_vec)){
		if(i==1){
			current_indices = 1:k_vec[i]
		} else{
			current_indices = (sum(k_vec[1:(i-1)])+1):sum(k_vec[1:i])
		}
		y_cluster[i] = sum(y_weighted[current_indices])
	}

	ci.ME = mean(y_cluster) +c(-1,1)*quantile*sd(y_cluster)/sqrt(n) #I use the sample variance as a proxy for the population variance. Remember, sd() divides by n and not n-1.
	if(return_weights){
		bundle = list(c(ci.bal, est.bal, var_est.bal), c(ci.ipw_bal, est.ipw_bal, var_est.ipw_bal), ci.ME, w[[2]], w[[1]], w[[9]])
	} else{
		bundle = list(c(ci.bal, est.bal, var_est.bal), c(ci.ipw_bal, est.ipw_bal, var_est.ipw_bal), ci.ME)
	}
	return(bundle)
}



#--------------------------------CODE FOR CHOOSING A LOW-RANK STRUCTURE--------------------------#
#------------------------------------------------------------------------------------------------#

asymp_CI_lm_adapt<-function(y_observed,X,T,row_block_flexi,stoc_int, k_try, propensity=NULL,coverage = 0.95, indiv = FALSE, chi_alpha = 0.99, est_bal = FALSE, est_sample = 100, sampler = NULL){
	quantile = qnorm((1-coverage)/2, lower.tail = FALSE)
	k_try = sort(k_try)
	n = length(T)
	univariate = indiv
	bundle = list()
	mse.bal = vector(length = length(k_try))
	mse.ipw_bal = mse.bal
	bias.bal = mse.bal 
	bias.ipw_bal = mse.bal
	var.bal = mse.bal
	var.ipw_bal = mse.bal

	for(counter in 1:length(k_try)){
		k = k_try[counter]
		row_block<-function(v,X,i=1,j=1,univariate = indiv){
			return(row_block_flexi(v,X,k, i, j, univariate))
		}
		bundle[[counter]] = asymp_CI_lm(y_observed,X,T,row_block,stoc_int, propensity = propensity,coverage = coverage, indiv = indiv, return_weights = TRUE, est_bal = est_bal, est_sample = est_sample, sampler = sampler)
	}

	bundle_max = bundle[[length(k_try)]]

	for(counter in 1:length(k_try)){
		bundle_current = bundle[[counter]]
		bias.bal[counter] = (bundle_current[[1]][3] - bundle_max[[1]][3])
		var.bal[counter] = bundle_current[[1]][4]/n
		bias.ipw_bal[counter] = (bundle_current[[2]][3] - bundle_max[[2]][3]) 
		var.ipw_bal[counter] = bundle_current[[2]][4]/n
	}

	if(!indiv){
		k = sapply(T,length)
		#print(1)
		row_block<-function(v,X,i=1,j=1,univariate = indiv){
			return(row_block_flexi(v,X,max(k_try)[1], i, j, univariate))
		}
		#print(2)
		Z = NULL
		for(i in 1:length(X)){
			for(j in 1:k[i]){
				temp = t(row_block(T[[i]],X,i,j))%*%X[[i]][j,]
				Z = cbind(Z,temp)
			}
		}
		Z = t(Z) #shall use this to get unbiased estimate of the standard error

		obj = lm(y_observed~Z-1)
		#print('obj')
		#print(summary(obj))
		#sigma.estimate = sqrt(sum(obj$residiuals^2)/obj$df.residual)
		sigma.estimate = summary(obj)[[6]]
	} else{
		sigma.estimate = 1 #THIS WILL CHANGE ONCE I FIGURE OUT VARIANCE ESTIMATION IN INDIV = TRUE CASE.
	}

	stats.bal = vector(length = length(k_try))
	stats.ipw_bal = vector(length = length(k_try))

	weights_denom.bal = stats.bal
	weights_denom.ipw_bal = stats.ipw_bal

	w.bal_final = bundle[[length(k_try)]][[3]]
	w.ipw_bal_final = bundle[[length(k_try)]][[4]]
	for(counter in 1:(length(k_try)-1)){
		w.bal = bundle[[counter]][[3]]
		w.ipw_bal = bundle[[counter]][[4]]
		stats.bal[counter] = (sqrt(n)*(bias.bal[counter]))/sqrt( (1/n)*(sum((w.bal - w.bal_final)^2))*sigma.estimate^2)
		stats.ipw_bal[counter] = (sqrt(n)*(bias.ipw_bal[counter]))/sqrt( (1/n)*(sum((w.ipw_bal - w.ipw_bal_final)^2))*sigma.estimate^2)

		weights_denom.bal[counter] =  (1/n)*(sum((w.bal - w.bal_final)^2))
		weights_denom.ipw_bal[counter] = (1/n)*(sum((w.ipw_bal - w.ipw_bal_final)^2))
	}
	stats.bal[length(k_try)] = 0
	stats.ipw_bal[length(k_try)] = 0

	cutoff = qchisq(chi_alpha,1)

	mse.bal = bias.bal^2 + var.bal
	mse.ipw_bal = bias.ipw_bal^2 + var.ipw_bal

	bal.counter = min(which(stats.bal^2<cutoff))
	ipw_bal.counter = min(which(stats.ipw_bal^2<cutoff))

	if(sum(is.na(stats.bal))>0){
		bundle_opt = list(c(NA,NA,NA,NA), bundle[[ipw_bal.counter]][[2]], bal.counter, ipw_bal.counter, stats.bal, stats.ipw_bal)
	}else{
		bundle_opt = list(bundle[[bal.counter]][[1]], bundle[[ipw_bal.counter]][[2]], bal.counter,ipw_bal.counter, stats.bal, stats.ipw_bal)
	}

	return(bundle_opt)
}


#------------------------- CODE FOR RUNNING THE EXPERIMENTS -------------------------------------#
#------------------------------------------------------------------------------------------------#


IPW_WEIGHTS = NULL
GAMMA = NULL

X_global = NULL
T_global = NULL
y_global = NULL

run_expt<-function(n,p_cov,rho,k_vec,times,gamma,sd,imbalance,row_block_true,row_block_hypo,trt_generate,propensity,stoc_int, print_time = FALSE, only_ipw = FALSE, indiv = FALSE, only_counterfactual = FALSE, row_block_flexi = NULL,k_try = NULL, ADAPT = FALSE, f_sampler = NULL, f_sample_times = NULL, alpha_generate = NULL, alpha_hypo = NULL, est_bal = FALSE, est_sample = 100, special_type = NULL,par_prob = NULL, par_alpha = NULL){	#this runs the experiment of comparing the ipw and balancing estimator for a particular paramteric setting
	#I will generate one set of beta for the entire simulations

	coverage = 0.95 #will not matter here
	if(ADAPT){
		if(is.null(row_block_flexi)|is.null(k_try)|indiv){
			stop('Must supply row_block_flexi and k_try with ADAPT = TRUE')
		}
	}
	status = rep(0,times)
	sq_bias = rep(0, times)
	sq_bias_ipw = rep(0, times)
	sq_bias.bal = rep(0, times)
	sq_bias.ipw_bal = rep(0, times)
	sq_bias.ME = rep(0,times)
	weights_norm.ipw_bal = rep(0, times) #diagnostic tool
	weights_norm.bal = rep(0, times)
	weights_norm.ipw = rep(0, times)
	weights_norm.ME = rep(0,times)
	weighted_signal.ipw = rep(0, times)
	weighted_signal.ipw_bal = rep(0,times)
	weighted_signal.bal = rep(0, times)
	weighted_signal.ME = rep(0,times)
	estimates = vector(length = times)
	#names(estimates) = c('target', 'balance_est', 'ipw_est')

	if(only_counterfactual){
		tic = Sys.time()
		X = list()
		cov_matrix_mult = expm::sqrtm(toeplitz(rho^(0: (p_cov - 1) )))
		for(i in 1:n){
			#p = p_cov*k_vec[i]
			X[[i]] = apply(matrix(rnorm(p_cov*k_vec[i]), ncol = p_cov)%*%cov_matrix_mult,2,g)
		}
		X_modified = X
		if(indiv){ #only modify if indiv = TRUE
			for(i in 1:n){
				X_modified[[i]][,p_cov] = rep(mean(X_modified[[i]][,p_cov]), k_vec[i])
				for(j in 1:p_cov){
					X_modified[[i]][,j] = X_modified[[i]][,j]^j
				}
			}
		}
		counterfactual = 0
		for(counter in 1:f_sample_times){ # we will estimate the treatment effect via empirical averages.
			print(paste0('Sample: ',counter))
			tic11 = Sys.time()
			counterfactual_each_sample = 0
			T = f_sampler(k_vec, X)
			for(i in 1:n){
				temp_sum = 0
				for(j in 1:k_vec[i]){
					if(indiv){
						k_generate = ceiling(k_vec[i]*alpha_generate)
						gamma_temp = as.vector(gamma(k_generate))
						gamma_to_use = gamma_temp + rnorm(length(gamma_temp))
						#gamma_to_use = as.vector(mvrnorm(1,mu = as.vector(gamma_temp), Sigma = diag(length( as.vector(gamma_temp) ))))
						temp = row_block_flexi(T[[i]],X,k_generate,i,j, univariate = FALSE)
					} else{
						gamma_to_use = gamma
						temp = row_block_true(T[[i]],X,i,j, univariate = FALSE)
					}
					gamma_to_use = gamma_to_use[1:ncol(temp)] #this is probably only going to be used when complete unstructure is used.
					beta_current = as.vector(temp%*%gamma_to_use)
					y_current = as.vector(sum(X_modified[[i]][j,]*beta_current) + rnorm(1,sd = sd))
					temp_sum = temp_sum + y_current/k_vec[i]
				}
				counterfactual_each_sample = counterfactual_each_sample + temp_sum
			}
			toc11 = Sys.time()
			if(print_time){
			    print(toc11 - tic11)
			}
			counterfactual_each_sample = counterfactual_each_sample/n
			counterfactual = counterfactual + counterfactual_each_sample
		}
		counterfactual = counterfactual/f_sample_times

		#print(paste0('counterfactual: ',counterfactual))

		return(counterfactual) #so if the program passes this if-clause, it returns from here on
	}


	for(iter in 1:times){
		if(print_time){
			print(paste('Iteration: ',iter))
		}
		tic = Sys.time()

		full_beta = list() 
		#print(1)
		X = list()
		cov_matrix_mult = expm::sqrtm(toeplitz(rho^(0: (p_cov - 1) )))
		for(i in 1:n){
			#p = p_cov*k_vec[i]
			X[[i]] = apply(matrix(rnorm(p_cov*k_vec[i]), ncol = p_cov)%*%cov_matrix_mult,2,g)
		}
		#print(2)
		T = trt_generate(k_vec, X)
		X_modified = X
		if(indiv){ #only modify if indiv = TRUE
			for(i in 1:n){
				X_modified[[i]][,p_cov] = rep(mean(X_modified[[i]][,p_cov]), k_vec[i])
				for(j in 1:p_cov){
					X_modified[[i]][,j] = X_modified[[i]][,j]^j
				}
			}
		}

		#print(3)
		
		counterfactual = 0 #this is not the true counterfactual but only helps in the subsequent code.

		y_observed = NULL
		y_observed_noiseless = NULL
		
		epsilon = list()
		for(i in 1:n){
			epsilon_temp_vec = NULL
		    epsilon[[i]] = NULL
		    #print(3.1)
		    if(indiv){
		    	k_generate = ceiling(k_vec[i]*alpha_generate)
				gamma_temp = gamma(k_generate)
		    }
			for(j in 1:k_vec[i]){
				#print(3.2)
				if(indiv){
					gamma_to_use =  gamma_temp + rnorm(length(as.vector(gamma_temp)))  #as.vector(mvrnorm(1,mu = as.vector(gamma_temp), Sigma = diag(length( as.vector(gamma_temp) ))))
					temp = row_block_flexi(T[[i]],X,k_generate,i,j, univariate = FALSE)
				} else{
					temp = row_block_true(T[[i]],X,i,j, univariate = FALSE)
					gamma_to_use = gamma
				}

				gamma_to_use = gamma_to_use[1:ncol(temp)]
				beta_curr = as.vector(temp%*%gamma_to_use)
				y_curr_noiseless = as.vector(X_modified[[i]][j,]%*%beta_curr)
				err_lm =  rnorm(1,sd = sd)
				epsilon_temp_vec = c(epsilon_temp_vec, err_lm)
				y_curr = as.vector(X_modified[[i]][j,]%*%beta_curr +err_lm)
				y_observed = c(y_observed, y_curr)	
				y_observed_noiseless = c(y_observed_noiseless, y_curr_noiseless)
			}
			epsilon[[i]] = epsilon_temp_vec
		}
	

		ipw = vector(length = n)
		#probas = vector(length = n)
		for(i in 1:n){
			ipw[i] = stoc_int(T[[i]],X[[i]])/(propensity(T[[i]], X[[i]]))
		}
		ipw.weights = NULL
		for(i in 1:n){
			ipw.weights = c(ipw.weights, rep(ipw[i],k_vec[i])/k_vec[i])
		}
		
		IPW_WEIGHTS <<- ipw.weights
		
		te.ipw = sum(ipw.weights*y_observed)/n

		#print(5)
		if(only_ipw){
			sq_bias_ipw[iter] = (te.ipw - counterfactual) #because counterfactual is 0, this sq bias is equal to the estimate.
			sq_bias[iter] = 0
		}
		else{
			if(ADAPT){
				temp = asymp_CI_lm_adapt(y_observed,X,T,row_block_flexi,stoc_int, k_try, propensity=propensity,coverage = coverage, indiv = indiv)
				te = c(temp[[2]][3], temp[[1]][3])
			} else{
				if(indiv){
					te = trt_effect_indiv(y_observed,X,T,row_block_flexi,stoc_int,propensity = propensity, imbalance=imbalance, alpha_hypo = alpha_hypo, special_type = special_type,par_prob = par_prob,par_alpha = par_alpha)
				} else{
					te = trt_effect_lm(y_observed,X,T,row_block_hypo,stoc_int,imbalance=imbalance, digits = 10, known_ipw = TRUE, ipw_weights = ipw.weights, display_time = FALSE, est_bal = est_bal, est_sample = est_sample, propensity = propensity, sampler = f_sampler)
				}
			}
			#print('te:')
			#print(te[[1]])
			#print(10)
			if(is.na(te[[1]][1])){
				sq_bias.ipw_bal[iter] = NA
			} else{
				sq_bias.ipw_bal[iter] = (te[[1]][1] - counterfactual)
				weights_norm.ipw_bal[iter] = sum((te[[2]][[1]])^2)/n
				weighted_signal.ipw_bal[iter] = sum(te[[2]][[1]]*y_observed_noiseless)/n
			}

			if(is.na(te[[1]][2])){
				sq_bias.bal[iter] = NA
				weights_norm.bal[iter] = NA
				weighted_signal.bal[iter] = NA
			} else{
				sq_bias.bal[iter] = (te[[1]][2] - counterfactual)
				weights_norm.bal[iter] = sum((te[[2]][[2]])^2)/n
				weighted_signal.bal[iter] = sum(te[[2]][[2]]*y_observed_noiseless)/n

			}
			sq_bias_ipw[iter] = (te.ipw - counterfactual)
			weights_norm.ipw[iter] = sum((ipw.weights)^2)/n
			weighted_signal.ipw[iter] = sum(ipw.weights*y_observed_noiseless)/n

			#for mixed effects estimated weights
			sq_bias.ME[iter] = te[[1]][3] - counterfactual
			weights_norm.ME[iter] = sum((te[[2]][[9]])^2)/n #recall the ME weights are in the 9th position
			weighted_signal.ME[iter] = sum(te[[2]][[9]]*y_observed_noiseless)/n
		}
		estimates[iter] = counterfactual
		toc = Sys.time()
		if(print_time){
			print(toc - tic)
		}
	}
	if(ADAPT){
		return(list(sq_bias.bal, sq_bias.ipw_bal, sq_bias_ipw, estimates, temp[[5]], temp[[6]]))
	} else{
		return(list(sq_bias.bal, sq_bias.ipw_bal, sq_bias_ipw, estimates, weights_norm.bal, weights_norm.ipw_bal, weights_norm.ipw, weighted_signal.bal, weighted_signal.ipw_bal, weighted_signal.ipw, epsilon, ipw.weights, te[[2]], sq_bias.ME, weights_norm.ME, weighted_signal.ME)) #te[[2]] is the balancing, projected and ME estimated ipw weights
	}
}


run_expt_var_est<-function(n,p_cov,rho,k_vec,times,gamma, sd,imbalance,row_block_true,row_block_hypo,trt_generate,propensity,stoc_int,coverage = 0.95, print_time = FALSE, indiv = FALSE, only_counterfactual = FALSE, row_block_flexi = NULL,k_try = NULL, ADAPT = FALSE, f_sampler = NULL, f_sample_times = NULL, alpha_generate = NULL, alpha_hypo = NULL, est_bal = FALSE, est_sample = 100, special_type = NULL,par_prob = NULL, par_alpha = NULL){	#this runs the experiment of comparing the ipw and balancing estimator for a particular paramteric setting
	#if only_counterfactual is true, then this only returns the causal estimand conditioned on X.
	#I will generate one set of beta for the entire simulations

	if(only_counterfactual){
		tic = Sys.time()
		X = list()
		cov_matrix_mult = expm::sqrtm(toeplitz(rho^(0: (p_cov - 1) )))
		for(i in 1:n){
			#p = p_cov*k_vec[i]
			X[[i]] = apply(matrix(rnorm(p_cov*k_vec[i]), ncol = p_cov)%*%cov_matrix_mult,2,g)
		}
		X_modified = X
		if(indiv){ #only modify if indiv = TRUE
			for(i in 1:n){
				X_modified[[i]][,p_cov] = rep(mean(X_modified[[i]][,p_cov]), k_vec[i])
				for(j in 1:p_cov){
					X_modified[[i]][,j] = X_modified[[i]][,j]^j
				}
			}
		}
		counterfactual = 0
		for(counter in 1:f_sample_times){ # we will estimate the treatment effect via empirical averages.
			print(paste0('Sample: ',counter))
			tic11 = Sys.time()
			counterfactual_each_sample = 0
			T = f_sampler(k_vec, X)
			for(i in 1:n){
				temp_sum = 0
				for(j in 1:k_vec[i]){
					if(indiv){
						k_generate = ceiling(k_vec[i]*alpha_generate)
						gamma_temp = as.vector(gamma(k_generate))
						gamma_to_use = gamma_temp + rnorm(length(gamma_temp))
						#gamma_to_use = as.vector(mvrnorm(1,mu = as.vector(gamma_temp), Sigma = diag(length( as.vector(gamma_temp) ))))
						temp = row_block_flexi(T[[i]],X,k_generate,i,j, univariate = FALSE)
					} else{
						gamma_to_use = gamma
						temp = row_block_true(T[[i]],X,i,j, univariate = FALSE)
					}
					gamma_to_use = gamma_to_use[1:ncol(temp)] #this is probably only going to be used when complete unstructure is used.
					beta_current = as.vector(temp%*%gamma_to_use)
					y_current = as.vector(sum(X_modified[[i]][j,]*beta_current) + rnorm(1,sd = sd))
					temp_sum = temp_sum + y_current/k_vec[i]
				}
				counterfactual_each_sample = counterfactual_each_sample + temp_sum
			}
			toc11 = Sys.time()
			if(print_time){
			    print(toc11 - tic11)
			}
			counterfactual_each_sample = counterfactual_each_sample/n
			counterfactual = counterfactual + counterfactual_each_sample
		}
		counterfactual = counterfactual/f_sample_times

		#BLOCK FOR DEBUGGING PURPOSES ONLY
		counterfactual.ipw = 0
		T = trt_generate(k_vec, X)
		gamma_to_use = gamma
			for(i in 1:n){
				sum.ipw = 0
				for(j in 1:k_vec[i]){
					temp = row_block_true(T[[i]],X,i,j, univariate = FALSE)
					gamma_to_use = gamma_to_use[1:ncol(temp)]
					beta_current = as.vector(temp%*%gamma_to_use)
					y_current = as.vector(sum(X_modified[[i]][j,]*beta_current) + rnorm(1,sd = sd))
					sum.ipw = sum.ipw + y_current*stoc_int(T[[i]],X[[i]])/propensity(T[[i]],X[[i]])
				}
				counterfactual.ipw = counterfactual.ipw + sum.ipw/k_vec[i]
			}
			counterfactual.ipw = as.numeric(counterfactual.ipw/n)

		#print(paste0('counterfactual: ',counterfactual))
		#print(paste0('counterfactual.ipw: ',counterfactual.ipw))

		return(list(counterfactual, counterfactual.ipw)) #so if the program passes this if-clause, it returns from here on
	}


	CIs = list(list(),list())

	for(iter in 1:times){
		if(print_time){
			print(paste('Iteration: ',iter))
		}
		tic = Sys.time()

		X = list()
		cov_matrix_mult = expm::sqrtm(toeplitz(rho^(0: (p_cov - 1) )))
		for(i in 1:n){
			#p = p_cov*k_vec[i]
			X[[i]] = apply(matrix(rnorm(p_cov*k_vec[i]), ncol = p_cov)%*%cov_matrix_mult,2,g)
		}
		T = trt_generate(k_vec, X)
		X_modified = X
		if(indiv){ #only modify if indiv = TRUE
			for(i in 1:n){
				X_modified[[i]][,p_cov] = rep(mean(X_modified[[i]][,p_cov]), k_vec[i])
				for(j in 1:p_cov){
					X_modified[[i]][,j] = X_modified[[i]][,j]^j
				}
			}
		}

		
		counterfactual = 0 #this is not the true counterfactual but only helps in the subsequent code.

		y_observed = NULL
		#y_observed_noiseless = NULL
		
		for(i in 1:n){
		    if(indiv){
		    	k_generate = ceiling(k_vec[i]*alpha_generate)
				gamma_temp = gamma(k_generate)
		    }
			for(j in 1:k_vec[i]){
				if(indiv){
					gamma_to_use =  gamma_temp + rnorm(length(as.vector(gamma_temp)))  #as.vector(mvrnorm(1,mu = as.vector(gamma_temp), Sigma = diag(length( as.vector(gamma_temp) ))))
					temp = row_block_flexi(T[[i]],X,k_generate,i,j, univariate = FALSE)
				} else{
					temp = row_block_true(T[[i]],X,i,j, univariate = FALSE)
					gamma_to_use = gamma
				}
				gamma_to_use = gamma_to_use[1:ncol(temp)]
				beta_curr = as.vector(temp%*%gamma_to_use)
				err_lm =  rnorm(1,sd = sd)
				y_curr = as.vector(X_modified[[i]][j,]%*%beta_curr + err_lm)
				y_observed = c(y_observed, y_curr)	
			}
		}

	
		ipw = vector(length = n)
		for(i in 1:n){
			ipw[i] = stoc_int(T[[i]],X[[i]])/(propensity(T[[i]], X[[i]]))
		}
		ipw.weights = NULL
		for(i in 1:n){
			ipw.weights = c(ipw.weights, rep(ipw[i],k_vec[i])/k_vec[i])
		}

		ipw_est = vector(length = n)
		for(i in 1:n){
			if(i == 1){
				indices = 1:k_vec[1]
			} else{
				indices = (sum(k_vec[1:(i-1)])+1):sum(k_vec[1:i])
			}
			ipw_est[i] = sum(ipw.weights[indices]*y_observed[indices])
		}

		quantile = qnorm((1-coverage)/2, lower.tail = FALSE)
		mid = mean(ipw_est)
		ipw.sd = sd(ipw_est)
		ci.ipw = mid + c(-1,1)*quantile*ipw.sd/sqrt(n)

		X_global <<- X
		T_global <<- T
		y_global <<- y_observed

		if(ADAPT){
			CI = asymp_CI_lm_adapt(y_observed,X,T,row_block_flexi,stoc_int, k_try, propensity = propensity,coverage = coverage, indiv = indiv, est_bal = est_bal, est_sample = est_sample, sampler = f_sampler)
		} else{
			CI = asymp_CI_lm(y_observed,X,T,row_block_hypo,stoc_int, propensity = propensity, coverage = coverage, indiv = indiv, est_bal = est_bal, est_sample = est_sample, sampler = f_sampler, alpha_hypo = alpha_hypo, special_type = special_type, par_prob = par_prob, par_alpha = par_alpha) 
		}

		#print('CI: ')
		#print(CI)

		#print('CI.ipw: ')
		#print(ci.ipw)
		
		CIs[[1]][[iter]] = CI
		CIs[[2]][[iter]] = ci.ipw
		toc = Sys.time()
		if(print_time){
			print(toc - tic)
		}
	}
	return(CIs)
}