library("abc")
subset.example<-function(){
tol = 0.005;
N = 100000;
sum1 = c();
r.obs=1.117;
AR1.obs=-0.608;
d.obs=2;
 

r = runif(N,0,10);
ar1 = runif(N, -1, 1);
d = sample(x = c(1,2,3), N, replace = T, prob = rep(1/3, 3));
for (i in 1:N){
ts.seq = arima.sim(list(order = c(1, d[i], 0), ar = ar1[i]), sd = r[i], n = 20);
#plot(ts.seq);
tsx = 1 : length(ts.seq)
#fit a crude linear model
lmm = lm(ts.seq ~ tsx);
sum1 = c(sum1, summary(lmm)$sigma);
}
sum1 = (sum1 - mean(sum1))/sd(sum1);
#have observation as first row in stats matrix
sum_stat = as.matrix(sum1);
sum_obs = sum_stat[1,];
param_list = cbind(r, ar1, d);
#
abc1 = abc(sum_obs, param_list, sum_stat, tol, method = "rejection");
post.samples = abc1$unadj.values;
post.samples;
}
