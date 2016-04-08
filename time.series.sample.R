################################################
##finding parameters of ARIMA model using ABC
################################################
abc.time.series.init<-function(N){
sum1 = c();
sum2 = c();
sum3 = c();
param1 = runif(N,0,10);
param2 = runif(N, -1, 1);
param3 = sample(x = c(1,2,3), N, replace = T, prob = rep(1/3, 3));

for (i in 1:N){
ts.seq = arima.sim(list(order = c(1, param3[i], 0), ar = param2[i]), sd = param1[i], n = 20);
#plot(ts.seq);
af = acf(ts.seq, plot=F);
paf = pacf(ts.seq, plot=F);
sum1 = c(sum1, mean(af$acf[2:6]));
sum2 = c(sum2, mean(paf$acf[1:5]));
sum3 = c(sum3,  max(ts.seq)-min(ts.seq));
}
sum1 = (sum1 - mean(sum1))/sd(sum1);
sum2 = (sum2 - mean(sum2))/sd(sum2);
sum3 = (sum3 - mean(sum3))/sd(sum3);
#have observation as first row in stats matrix
sum_stat = cbind(sum1, sum2, sum3, sum4);
sum_obs = sum_stat[1,];
param_list = cbind(param1, param2, param3);
list(params = param_list, sum_obs = sum_obs, sum_stat = sum_stat);
}

abc.time.series<-function(x, tol){
sum_obs = x[[2]];
param_list = x[[1]];
sum_stat = x[[3]];

abc1 = abc(sum_obs, param_list, sum_stat, tol, method = "rejection");
post.samples = abc1$unadj.values;

print (length(post.samples));
print (param_list[1,]);
post.samples;
}

