########################################################################
##Simple example of using ABC to infer parameters for logistic regression
########################################################################
library("abc")
##
abc.rej.init<-function(N){
beta1 = runif(N, -1,1);
beta2 = runif(N, -1,1);
sum1 = c();
sum2 = c();
x1 = seq(0, 1, 0.01);
for (i in 1:N){
z =  beta1[i] + beta2[i]*x1;
pr = 1/(1+exp(-z));
y = rbinom(length(x1),1,pr);
##construct summary statistics
sum2 = c(sum2, mean(x1[y==1]) - mean(x1[y==0]));
sum1 = c(sum1, mean(pr[1 : length(floor(x1/4))]));
}
sum1 = (sum1 - mean(sum1))/sd(sum1);
sum2 = (sum2 - mean(sum2))/sd(sum2);
sum_stat = cbind(sum1, sum2);
#have observation as first row in stats matrix
sum_obs = sum_stat[1, ];
param_list = cbind(beta1, beta2);
list( sum_obs, sum_stat, param_list);
}
##
abc.rej<-function(ss, tol){
abc1 = abc(ss[[1]], ss[[3]], ss[[2]], tol, method = "rejection");
post.samples = abc1$unadj.values;
print(length(post.samples));
print(ss[[3]][1,]);
post.samples
}
##############################
##############################
x = abc.rej.init(100000);
res = abc.rej(x,0.01);
hist(res[,1])
hist(res[,2])
################################
############################### 



