setwd('C:/Users/Allen/Dropbox/activities/Buxton Challenge')
da = read.csv('CTL Data - Mod.csv')
dim(da)
### Choose "URBAN" or "SUBRURBAN" (all caps)
MODEL = "URBAN"

#################
### Scrubbing ###
#################
desc = read.csv('Descriptions - Mod.csv')
desc = subset(desc, !(Variable %in% c('NONCALI_RES_GRAV','NONCALI_WRK_GRAV','Region_AvgSales')))
temp0 = da[,as.character(desc$Variable[which(desc$Not_Zero==1)])]
da[,as.character(desc$Variable[which(desc$Not_Zero==1)])][temp0==0] = NA
# write.csv(da, file='CTL Data - Mod_2.csv', row.names=F)
# da = subset(da, select=as.character(desc$Variable[which(desc$Is_PCT!=1)]))
dim(da)

##################
### Subsetting ###
##################
if (MODEL == "URBAN"){
  density.types = c('URBAN', 'METRO')
} else {
  density.types = c('SUBURBAN','IN-TOWN')
}
da1 = subset(da, (DENSITY_DESC %in% density.types & !(STORE_STATUS %in% c('CLOSED','OPEN AFTER MSR'))), select=-c(STORE_STATUS,DENSITY_CLASS,OPEN_DATE,CLOSE_DATE,Region_Count))
if (MODEL=="URBAN"){
  dropcols = grepl('8TO',colnames(da1))
} else {
  dropcols = (grepl('1RO',colnames(da1)) | grepl('0_5RO',colnames(da1)))
}
da1 = da1[,!dropcols]
dim(da1)



####################################
### Subsetting and Standardizing ###
####################################
detach(da1)
attach(da1)
library(glmnet)
da1 = subset(da1, select=-SQFT)
X = model.matrix(SALES_2013~., da1) # drops rows with NA
test = which(X[,'PRELIMINARY.SAMPLEPRELIM']==0)
if (MODEL == "URBAN"){
  dropcols = match(c('NUMBER','PRELIMINARY.SAMPLEPRELIM','DENSITY_DESCSUPER-URBAN','DENSITY_DESCRURAL','DENSITY_DESCSUBURBAN','UNIV_DORMS_0_5RO'),colnames(X))
} else {
  dropcols = match(c('NUMBER','PRELIMINARY.SAMPLEPRELIM','DENSITY_DESCSUPER-URBAN','DENSITY_DESCRURAL','DENSITY_DESCURBAN','DENSITY_DESCMETRO'),colnames(X))
}
X = X[,-dropcols]
if (MODEL=="URBAN"){
  X[,-c(1:7)] = scale(X[,-c(1:7)]) # don't scale indicators
} else {
  X[,-c(1:6)] = scale(X[,-c(1:6)])
}
y = da$SALES_2013[as.integer(rownames(X))]
y.mean = mean(y); y.sd = sd(y)
y = scale(y)
if (MODEL=="URBAN"){
  X.test = X[test,]
} else {
  X.test = t(matrix(X[test,]))
}



######################################
### Cross-validation + Grid search ###
######################################
X = X[-test,]
y = y[-test]
alpha = c(0.25,0.5,0.75,1)
# pf = seq(1,length.out=ncol(X)); pf[1:7] = 0
lambda = glmnet(X,y,standardize=F)$lambda
nFolds = 10
q = round(seq(0,nFolds)*nrow(X)/nFolds)
rmse.mat = matrix(nrow=length(alpha),ncol=100) # default 100 lambdas
mae.mat = matrix(nrow=length(alpha),ncol=100)
mape.mat = matrix(nrow=length(alpha),ncol=100)
for (i in 1:length(alpha)){
  s = sample(nrow(X),nrow(X),replace=F)
  rmse.alpha.mat = matrix(nrow=nFolds,ncol=100)
  mae.alpha.mat = matrix(nrow=nFolds,ncol=100)
  mape.alpha.mat = matrix(nrow=nFolds,ncol=100)
  for (j in 1:nFolds){
    cvFold = s[(q[j]+1):q[j+1]]
    X.train = X[-cvFold,]; X.cv = X[cvFold,]
    y.train = y[-cvFold]; y.cv = y[cvFold]
    da1.enet = glmnet(X.train,y.train,alpha=alpha[i],lambda=lambda,standardize=F)
    predictions = predict(da1.enet, newx=X.cv)*y.sd + y.mean
    y.cv.rep = replicate(100,y.cv)*y.sd + y.mean
    rmse.alpha.mat[j,] = sqrt(apply((y.cv.rep-predictions)^2, 2, mean))
    mae.alpha.mat[j,] = apply(abs(y.cv.rep-predictions), 2, mean)
    mape.alpha.mat[j,] = apply(abs((y.cv.rep-predictions)/y.cv.rep), 2, mean)
  }
  rmse.mat[i,] = apply(rmse.alpha.mat, 2, mean)
  mae.mat[i,] = apply(mae.alpha.mat, 2, mean)
  mape.mat[i,] = apply(mape.alpha.mat,2,mean)
}

### Print Evaluation Metrics
min(rmse.mat)
min(mae.mat)
min(mape.mat)

### Plot RMSE Curves
matplot(log(lambda),t(rmse.mat),type='l',las=1,cex.axis=.8,xlab=expression(paste('Log ',lambda)),ylab='',main='RMSE Curves')



################################
### Best model & predictions ###
################################
best.params = which(mape.mat == min(mape.mat), arr.ind=T)
best.alpha = alpha[best.params[1]]; best.lambda = lambda[best.params[2]]
da1.enet = glmnet(X,y,alpha=best.alpha,standardize=F)
coefs = da1.enet$beta[,best.params[2]]
# Number of variables selected
da1.enet.coefs = coefs[which(coefs!=0)]
da1.enet.coefs = da1.enet.coefs[order(abs(da1.enet.coefs),decreasing=T)]
View(da1.enet.coefs)

# Fitted values with residual plot
da1.enet.fit = predict(da1.enet, s=best.lambda, newx=X)*y.sd + y.mean
z = da$SALES_2013[as.integer(rownames(X))]
# View(cbind(z,da1.enet.fit))
write.csv(cbind(z,da1.enet.fit),file='Fits-ENet.csv')
plot((da1.enet.fit-z)/z,type='p',las=1,cex.axis=.8,main='Residual Plot',ylab='Error')
abline(0,0)

# Predictions for potential stores
predict(da1.enet, s=best.lambda, newx=X.test)*y.sd + y.mean



