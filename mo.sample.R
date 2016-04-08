#############################
##mo sample
#############################
##use   source("C:/Users/User/Desktop/UDESK/MO_ABC/mo.sample.R", echo=T)
##to run automatically
############################
library("abc")
library("entropy")
##must enter the following to switch on the R server so that clojure can talk to it
##library("Rserve")
##Rserve()
###############################################
init.data.and.stats<-function(N, tol, test.param){
r = runif(N,0,10);
ar1 = runif(N, -1, 1);
d = sample(x = c(1,2,3), N, replace = T, prob = rep(1/3, 3));

stats.all = c();
params.all = c();
stats.all.std = c();


for (i in 1:N){

ts.seq = arima.sim(list(order = c(1, d[i], 0), ar = ar1[i]), sd = r[i], n = 20);
tsx = 1 : length(ts.seq)
#fit a crude linear model
lmm = lm(ts.seq ~ tsx);
stat2 = lmm$coefficients[1];
stat3 = lmm$coefficients[2];
stat4 = summary(lmm)$sigma;
stat1 = mean(ts.seq);

#acf and pcf
af = acf(ts.seq, plot=F);
paf = pacf(ts.seq, plot=F);
stat5 = af$acf[2];
stat6 = af$acf[3];
stat7 = af$acf[4];
stat8 = af$acf[5];
stat9 = mean(af$acf);
stat10 = sd(af$acf);
stat11 = paf$acf[1];
stat12 = paf$acf[2];
stat13 = paf$acf[3];
stat14 = paf$acf[4];
stat15 = mean(paf$acf);
stat16 = sd(paf$acf)
stat17 =  max(ts.seq)-min(ts.seq);
stat18 = runif(1, 0, 10); ###just add random noise here to simulate the accumulation of noise
###with longer vectors for studying and callibrating
##
##
stats.row = c(stat1, stat2, stat3, stat4, stat5, stat6, stat7, stat8, stat9, stat10, stat11, stat12, stat13, stat14, stat15, stat16, stat17); #dont use stat18 for now 
stats.all = rbind(stats.all, stats.row);
params.all = rbind(params.all, c(r[i], ar1[i], d[i]));
}
rownames(stats.all) = NULL;
rownames(params.all) = NULL; 
for (ff in 1:dim(stats.all)[2]){
stats.all.std = cbind(stats.all.std, 
(stats.all[,ff]-mean(stats.all[,ff]))/sd(stats.all[,ff])
);
}
stats.all.std = as.matrix(stats.all.std);
params.all = as.matrix(params.all);
list(params.all,  stats.all.std, N, tol, test.param);
}
###############################################
#need the stats and param globally for later
#choose to measure entropy on r
gInfo = init.data.and.stats(10000, 0.01, 3);  ##test.param =1 r, 2 AR1 and 3 d
###############################################
std.entropy<-function(x){
#finds the emirical entropy
#note that longer x are more likely to contain extreme info and
#so reduce the entropy, when comparing entropies need to ensure
#that x is the same length
#this is OK as we are always selecting the top 1% so the
#sample size would be the same each time

z = (x - mean(x))/sd(x);
hst = hist(z, breaks = seq(min(z)-1, max(z)+1, 0.1), plot=F);
entropy.empirical(hst$counts);
}
#################################################
std.entropy1<-function(x){
hst = hist(x, breaks = seq(min(x)-1, max(x)+1, 0.01), plot=F);
f = hst$density;
dd = f!=0;
f = f[dd];
w = diff(hst$breaks);
w = w[dd];
entr = -sum( f*log(f/w) );
if (is.na(entr)) entr = 20;
return (entr)
}
###############################################
mo.score<-function(stats.subset, num.samples)
{
df = read.table("C:/Users/User/Desktop/UDESK/MO_ABC/counter.txt", header=FALSE)
df[[1]] = df[[1]] + 1;
write.table(df,"C:/Users/User/Desktop/UDESK/MO_ABC/counter.txt", col.names=F, row.names=F, quote = F);

if (length(stats.subset)==0) return(10000);
#called by GP ie clojure code
#enters a vector of stats (1,3,4)
#performs abc with that choice and returns the entropy
sum.stats = as.matrix(gInfo[[2]][,stats.subset]);
param.list = gInfo[[1]];
emp.entropy = rep(NA, num.samples);
for (f in 1:num.samples){
##repeat num.samplesx to build up an average entropy
sum.obs = sum.stats[sample(1:gInfo[[3]], 1, replace=T), ];
####print(sum.obs);
abc.result = abc(sum.obs, param.list, sum.stats, gInfo[[4]], method = "rejection");
post.samples = abc.result$unadj.values;
emp.entropy[f] = std.entropy1(post.samples[ ,gInfo[[5]]]);

}
####print(emp.entropy);
mean(emp.entropy);
}
################################################