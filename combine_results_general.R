args_temp<-commandArgs(TRUE)
ci_indicator = as.numeric(args_temp[[1]])
st = args_temp[[2]]
save_name = paste0(strsplit(st,'/')[[1]][2],'_fixed.list')
#1 will trigger `CI COMBINE RESULTS'
#0 will trigger 'EST COMBINE RESULTS'

print('CI indicator: ')
print(ci_indicator)

args = args_temp[-c(1)]

if(!ci_indicator){
    tic = Sys.time()
    times = 1000
    times_counterfactual = 500

    for(j in 1:length(args)){
        setwd(paste0('~/spillover_balance/',args[j]))
        
    
        #first get the counterfactual
    
        counterfactual = 0
        for(i in 1:times_counterfactual){
            uu = try(readRDS(paste0('run_',i,'.list')), silent = TRUE)
            counterfactual = counterfactual + uu
        }
        counterfactual = counterfactual/times_counterfactual
    
        #counterfactual =  readRDS('summary2.vec')[[8]]
        
        n = length(counterfactual)
        ll_bias = list()
        for(k in 1:n){
            ll_bias[[k]] = list()
            ll_bias[[k]][[1]] = vector(length = 0)
            ll_bias[[k]][[2]] = vector(length = 0)
            ll_bias[[k]][[3]] = vector(length = 0)
            ll_bias[[k]][[4]] = vector(length = 0)
        }
    
        bias.bal = 0
        bias.ipw.bal = 0
        bias.ipw = 0
        bias.ME = 0
    
        sd.bal = 0
        sd.ipw.bal = 0
        sd.ipw = 0
        sd.ME = 0
    
        no_balance = 0
    
        counter = 0

        for(i in times_counterfactual + (1:times) ){
            uu = try(readRDS(paste0('run_',i,'.list')), silent = TRUE)
            if(inherits(uu, "try-error")){
                print(i)
                counter = counter + 1
                next
            }
            temp = uu[[1]]
            temp_for_list = uu[[1]]
            no_balance = no_balance + 1*is.nan(temp)
            temp[which(is.nan(temp))] = 0
            temp_for_list[which(is.nan(temp))] = NA
            
      
            bias.bal = bias.bal + temp
            bias.ipw.bal = bias.ipw.bal + uu[[4]]
            bias.ipw = bias.ipw + uu[[7]]
            bias.ME = bias.ME + uu$bias_ME

            sd.bal = sd.bal + temp^2
            sd.ipw.bal = sd.ipw.bal + uu[[4]]^2
            sd.ipw = sd.ipw + uu[[7]]^2
            sd.ME = sd.ME + (uu$bias_ME)^2
            
            for(k in 1:n){
                ll_bias[[k]][[1]] = c( ll_bias[[k]][[1]], temp_for_list[k] - counterfactual[k])
                ll_bias[[k]][[2]] = c( ll_bias[[k]][[2]], uu[[4]][k] - counterfactual[k])
                ll_bias[[k]][[3]] = c( ll_bias[[k]][[3]], uu[[7]][k] - counterfactual[k])
                ll_bias[[k]][[4]] = c( ll_bias[[k]][[4]], uu$bias_ME[k] - counterfactual[k])
            }
        }
        times_adjusted = times - counter
    
        n.bal = rep(times-counter, length(bias.ipw))
        n.ipw.bal = n.bal
        n.ipw = n.bal
        n.ME = n.bal
    
        n.bal = n.bal - no_balance
    
        bias.bal = bias.bal/n.bal - counterfactual
        bias.ipw.bal = bias.ipw.bal/n.ipw - counterfactual
        bias.ipw = bias.ipw/n.ipw - counterfactual
        bias.ME = bias.ME/n.ME - counterfactual
    
        sd.bal= sqrt(sd.bal/n.bal - (bias.bal + counterfactual)^2)/sqrt(n.bal)
        sd.ipw.bal= sqrt(sd.ipw.bal/n.ipw.bal - (bias.ipw.bal+counterfactual)^2)/sqrt(n.ipw.bal)
        sd.ipw= sqrt(sd.ipw/n.ipw - (bias.ipw+counterfactual)^2)/sqrt(n.ipw)
        sd.ME = sqrt(sd.ME/n.ME - (bias.ME+counterfactual)^2)/sqrt(n.ME)
    


        summary = list(bias.bal, sd.bal, bias.ipw.bal, sd.ipw.bal, bias.ipw, sd.ipw, n.bal, counterfactual, bias.ME, sd.ME)
        names(summary) = c('bias.bal', 'sd.bal', 'bias.ipw.bal', 'sd.ipw.bal', 'bias.ipw', 'sd.ipw', 'n.bal', 'counterfactual', 'bias.ME', 'sd.ME')
        #saveRDS(summary, 'summary.list')
        setwd('~/spillover_balance/output_files')
        saveRDS(list(summary, ll_bias), paste0(save_name))
        toc = Sys.time()
        print(toc - tic)
    }
} else{
    
    times = 500
    times_sum_count = 1000

for(j in 1:length(args)){
    setwd(paste0('~/spillover_balance/',args[j]))
    #setwd('~/spillover_balance/var_est/fixed/additive/well_specified')
    #setwd('~/spillover_balance/var_est/fixed/graph/graph3_3')
    
    counterfactual_vec = NULL

    for(i in 1:times){
      uu_temp = NULL
      temp =  readRDS(paste0('run_',i,'.list'))
      for(iter in 1:length(temp)){
          uu_temp = c(uu_temp, temp[[iter]][[1]])
      }
      #uu = readRDS(paste0('run_',i,'.list'))[[1]][[2]]
      uu = uu_temp
      counterfactual_vec = rbind(uu,  counterfactual_vec)
    }
    
    counterfactual_vec = apply(counterfactual_vec,2,mean)
    
    uu = readRDS(paste0('run_',times+1,'.list'))
    n =  length(uu)
    
    
    
    BAL_CI = list() #this will not impact the final output, but would just store the confidence intervals.
    IPW.BAL_CI = list()
    IPW_CI = list()
    n_probe = n #the index of par number where for which you want to store the confidence intervals.
    
    counter = vector(length = n)
    length.bal = vector(length = n)
    length.bal_ipw = vector(length = n)
    length.bal.sd = vector(length = n)
    length.bal_ipw.sd = vector(length = n)
    length.ipw = vector(length = n)
    length.ipw.sd = vector(length = n)
    length.ME = vector(length = n)
    length.ME.sd = vector(length = n)
    
    coverage.bal = vector(length = n)
    coverage.bal_ipw = vector(length = n)
    coverage.bal.sd = vector(length = n)
    coverage.bal_ipw.sd = vector(length = n)
    coverage.ipw = vector(length = n)
    coverage.ipw.sd = vector(length = n)
    coverage.ME = vector(length = n)
    coverage.ME_sd = vector(length = n)
    
    variance.bal = vector(length = n)
    variance.bal_ipw = vector(length = n)
    variance.ipw = vector(length = n)
    variance.ME = vector(length = n)
    
    
    mean.bal = vector(length = n)
    mean.bal_ipw = vector(length = n)
    mean.ipw = vector(length = n)
    mean.ME = vector(length = n)
    
    
    counting = 0
    
    #print('a')
    ll_length = list()
    for(k in 1:n){
         ll_length[[k]] = list()
         ll_length[[k]][[1]] = vector(length = 0)
         ll_length[[k]][[2]] = vector(length = 0)
         ll_length[[k]][[3]] = vector(length = 0)
         ll_length[[k]][[4]] = vector(length = 0)
    }
    #print('b')
    
    for(i in times+(1:times_sum_count)){
         uu = readRDS(paste0('run_',i,'.list'))
         for(k in 1:n){
             ci.bal = uu[[k]][[1]][[1]][[1]][1:2]
             #print(ci.bal)
             ci.bal_ipw =  uu[[k]][[1]][[1]][[2]][1:2]
             ci.ME = uu[[k]][[1]][[1]][[3]][1:2]
             ci.ipw =  uu[[k]][[2]][[1]][1:2]
             
             counterfactual = counterfactual_vec[k]
             
             
             if(k == n_probe){
                 counting = counting + 1
                 #print('a')
                 #print(ci.bal)
                 BAL_CI[[counting]] = ci.bal
                   #print('b')
                 IPW.BAL_CI[[counting]] = ci.bal_ipw
                   #print('c')
                 IPW_CI[[counting]] = ci.ipw 
                 #print('d')
             }
             if(is.na(ci.bal[1])){
                 counter[k] = counter[k] + 1
             } else{
                 coverage.bal[k] = as.numeric((ci.bal[1]<=counterfactual) & (counterfactual <= ci.bal[2])) + coverage.bal[k]
                 length.bal[k] = length.bal[k] + ci.bal[2] - ci.bal[1]
                 length.bal.sd[k] = length.bal.sd[k] +  (ci.bal[2] - ci.bal[1])^2
                 mean.bal[k] =  mean.bal[k] + mean(ci.bal)
                 variance.bal[k] = variance.bal[k] + (mean(ci.bal))^2
                 ll_length[[k]][[1]] = c(ll_length[[k]][[1]], ci.bal[2] - ci.bal[1])
             }
                coverage.bal_ipw[k] = as.numeric((ci.bal_ipw[1]<=counterfactual) & (counterfactual <= ci.bal_ipw[2])) + coverage.bal_ipw[k]
                length.bal_ipw[k] = length.bal_ipw[k] + (ci.bal_ipw[2] - ci.bal_ipw[1])
                length.bal_ipw.sd[k] = length.bal_ipw.sd[k] + (ci.bal_ipw[2] - ci.bal_ipw[1])^2
                mean.bal_ipw[k] =  mean.bal_ipw[k] + mean(ci.bal_ipw)
                variance.bal_ipw[k] = variance.bal_ipw[k] + (mean(ci.bal_ipw))^2
                ll_length[[k]][[2]] = c(ll_length[[k]][[2]], ci.bal_ipw[2] - ci.bal_ipw[1])
                
                coverage.ipw[k] = as.numeric((ci.ipw[1]<=counterfactual) & (counterfactual <= ci.ipw[2])) + coverage.ipw[k]
                length.ipw[k] = length.ipw[k] + (ci.ipw[2] - ci.ipw[1])
                length.ipw.sd[k] = length.ipw.sd[k] + (ci.ipw[2] - ci.ipw[1])^2
                mean.ipw[k] =  mean.ipw[k] + mean(ci.ipw)
                variance.ipw[k] = variance.ipw[k] + (mean(ci.ipw))^2
                ll_length[[k]][[3]] = c(ll_length[[k]][[3]], ci.ipw[2] - ci.ipw[1])
                
                coverage.ME[k] = as.numeric((ci.ME[1]<=counterfactual) & (counterfactual <= ci.ME[2])) + coverage.ME[k]
                length.ME[k] = length.ME[k] + (ci.ME[2] - ci.ME[1])
                length.ME.sd[k] = length.ME.sd[k] + (ci.ME[2] - ci.ME[1])^2
                mean.ME[k] =  mean.ME[k] + mean(ci.ME)
                variance.ME[k] = variance.ME[k] + (mean(ci.ME))^2
                ll_length[[k]][[4]] = c(ll_length[[k]][[4]], ci.ME[2] - ci.ME[1])
         }
    }
    
    coverage.bal = coverage.bal/(times_sum_count - counter)
    length.bal = length.bal/(times_sum_count - counter)
    
    length.bal.sd =  sqrt(length.bal.sd/(times_sum_count - counter) - length.bal^2)
    coverage.bal.sd = sqrt(coverage.bal*(1-coverage.bal)/(times_sum_count - counter))
    
    mean.bal = mean.bal/(times_sum_count - counter)
    variance.bal = sqrt(variance.bal/(times_sum_count - counter) - mean.bal^2)
    
    coverage.bal_ipw = coverage.bal_ipw/times_sum_count
    length.bal_ipw = length.bal_ipw/times_sum_count
    
    length.bal_ipw.sd =  sqrt(length.bal_ipw.sd/times_sum_count - length.bal_ipw^2)
    coverage.bal_ipw.sd = sqrt(coverage.bal_ipw*(1-coverage.bal_ipw)/times_sum_count)
    
    mean.bal_ipw = mean.bal_ipw/(times_sum_count)
    variance.bal_ipw = sqrt(variance.bal_ipw/(times_sum_count) - mean.bal_ipw^2)
    
    coverage.ipw = coverage.ipw/times_sum_count
    length.ipw = length.ipw/times_sum_count
    
    length.ipw.sd =  sqrt(length.ipw.sd/times_sum_count - length.ipw^2)
    coverage.ipw.sd = sqrt(coverage.ipw*(1-coverage.ipw)/times_sum_count)
    
    mean.ipw = mean.ipw/(times_sum_count)
    variance.ipw = sqrt(variance.ipw/(times_sum_count) - mean.ipw^2)
    
    coverage.ME = coverage.ME/times_sum_count
    length.ME = length.ME/times_sum_count
    
    length.ME.sd =  sqrt(length.ME.sd/times_sum_count - length.ME^2)
    coverage.ME.sd = sqrt(coverage.ME*(1-coverage.ME)/times_sum_count)
    
    mean.ME = mean.ipw/(times_sum_count)
    variance.ME = sqrt(variance.ME/(times_sum_count) - mean.ME^2)
    

    #summary = list(bias.bal, sd.bal, bias.ipw.bal, sd.ipw.bal, bias.ipw, sd.ipw, n.bal)
    summary = list(coverage.bal, coverage.bal.sd, length.bal, length.bal.sd, coverage.bal_ipw, coverage.bal_ipw.sd, length.bal_ipw, length.bal_ipw.sd, coverage.ipw, coverage.ipw.sd, length.ipw, length.ipw.sd, coverage.ME, coverage.ME.sd, length.ME, length.ME.sd, times_sum_count - counter, counterfactual_vec, variance.bal, variance.bal_ipw, variance.ipw, variance.ME)
    
    names(summary) = c('coverage.bal', 'coverage.bal.sd', 'length.bal', 'length.bal.sd', 'coverage.bal_ipw', 'coverage.bal_ipw.sd', 'length.bal_ipw', 'length.bal_ipw.sd', 'coverage.ipw', 'coverage.ipw.sd', 'length.ipw', 'length.ipw.sd', 'coverage.ME', 'coverage.ME.sd', 'length.ME', 'length.ME.sd', 'times_sum_count_minus_counter', 'counterfactual_vec', 'variance.bal', 'variance.bal_ipw', 'variance.ipw', 'variance.ME')


    #saveRDS(summary, 'summary.list')
    setwd('~/spillover_balance/output_files')
    saveRDS(list(summary,ll_length), paste0(save_name))
}
    
}

