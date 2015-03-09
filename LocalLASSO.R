setwd('C:/Users/Allen/Dropbox/activities/Buxton Challenge')
MODEL = "URBAN"
KERNEL = T
k = 5
if (MODEL=="URBAN"){
  X = read.csv('da-Urban.csv',header=T,sep=',')
} else {
  X = read.csv('da-Suburban.csv',header=T,sep=',')
}
rownames(X) = X$X; X=X[,-1]
if (KERNEL){
  # drop demographic-distributional features
  X = X[,!grepl('XC',colnames(X))]
}

#####################################
### Create Kernel (if KERNEL = T) ###
#####################################
if (KERNEL){
  bhatt.mats = c('BhattAgeDist.csv','BhattIncomeDist.csv','BhattEducationDist.csv','BhattFamilyDist.csv','BhattHouseholdDist.csv') 
  if (MODEL=='URBAN'){
    gamma = c(0.2, 0.4, 0.2, 0.1, 0.1)
  } else {
    gamma = c(0.4, 0.2, 0.1, 0.4, 0.1)
  }
  gamma = gamma/sum(gamma) # sum to 1
  w = 0
  for (i in 1:length(bhatt.mats)){
    bhatt.mat = read.csv(bhatt.mats[i],header=T,sep=',')
    w = w + gamma[i]*exp(-bhatt.mat)
  }
  rm(bhatt.mat)
}
w = as.matrix(w)
diag(w) = pmax(diag(w),1)
w = pmax(1-4*(1-w), 0) # Stretch distances!
w = data.frame(w,row.names=1:314)
w = subset(w, subset=(rownames(w) %in% rownames(X)), select=(colnames(w) %in% paste0("V",rownames(X))))

####################################
### Subsetting and Standardizing ###
####################################
test = which(X[,'PRELIMINARY.SAMPLEPRELIM']==0)
w.train = w[-test,-test]
# Find k-Most-Similar Existing Stores for each test store
kSS.test = list()
for (i in 1:length(test)){
  kSS.test[[i]] = list()
  kSS.test[[i]][[1]] = rownames(w)[test[i]]
  kSS.test[[i]][[2]] = rownames(w[-test,])[order(w[-test,][,test[i]], decreasing=T)[2:(k+1)]]
}
if (MODEL == "URBAN"){
  dropcols = match(c('NUMBER','PRELIMINARY.SAMPLEPRELIM','DENSITY_DESCSUPER.URBAN','DENSITY_DESCRURAL','DENSITY_DESCSUBURBAN','UNIV_DORMS_0_5RO'),colnames(X))
} else {
  dropcols = match(c('NUMBER','PRELIMINARY.SAMPLEPRELIM','DENSITY_DESCSUPER.URBAN','DENSITY_DESCRURAL','DENSITY_DESCURBAN','DENSITY_DESCMETRO'),colnames(X))
}
X = as.matrix(X[,-dropcols])
if (MODEL=="URBAN"){
  X[,-c(1:7)] = scale(X[,-c(1:7)]) # don't scale indicators
} else {
  X[,-c(1:6)] = scale(X[,-c(1:6)])
}
y = read.csv('CTL Data - Mod.csv',header=T,sep=',')$SALES_2013
y = y[as.integer(rownames(X))]
y.mean = mean(y); y.sd = sd(y)
y.copy = y # For computing prediction error
y = scale(y)
if (MODEL=="URBAN"){
  X.test = X[test,]
} else {
  X.test = t(matrix(X[test,]))
}



######################
### Run LASSO Fits ###
######################
X = X[-test,]
y = y[-test]
library(glmnet)
Lassos = list()
for (i in 1:nrow(X)){
  Lassos[[i]] = list()
  Lassos[[i]][[1]] = rownames(X)[i]
  Lassos[[i]][[2]] = cv.glmnet(X,y,weights=w.train[,i], standardize=F, nfolds=5)
  print(i)
}

# Find k-Most-Similar Stores for each training store
kSS.train = list()
for (i in 1:nrow(X)){
  kSS.train[[i]] = list()
  kSS.train[[i]][[1]] = rownames(w.train)[i]
  kSS.train[[i]][[2]] = rownames(w.train)[order(w.train[,i], decreasing=T)[2:(k+1)]]
}

predictions = vector(length=nrow(X))
for (i in 1:nrow(X)){
  temp = vector(length=k)
  for (j in 1:k){
    jth.sim.store = kSS.train[[i]][[2]][j] # (a string)
    jth.sim.store.model = Lassos[[which(rownames(X)==jth.sim.store)]][[2]] # cv.glmnet object
    temp[j] = predict(jth.sim.store.model, newx=t(matrix(X[i,])))*y.sd + y.mean
  }
  weights = w[which(rownames(w) %in% kSS.train[[i]][[2]]),i] # same order???
  predictions[i] = sum(temp*weights)/sum(weights)
}

mae = mean(abs(y.copy[-test]-predictions))
mape = mean(abs(y.copy[-test]-predictions)/y.copy[-test])

#print
mae; mape


###################################
### Predictions for Test Stores ###
###################################
test.predictions = vector(length=nrow(X.test))
for (i in 1:length(test)){
  temp = vector(length=k)
  for (j in 1:k){
    jth.sim.store = kSS.test[[i]][[2]][j] # (a string)
    jth.sim.store.model = Lassos[[which(rownames(X)==jth.sim.store)]][[2]] # cv.glmnet object
    temp[j] = predict(jth.sim.store.model, newx=t(matrix(X.test[i,])))*y.sd + y.mean
  }
  weights = w[which(rownames(w) %in% kSS.test[[i]][[2]]),test[i]] # same order???
  test.predictions[i] = sum(temp*weights)/sum(weights)
}

# print
test.predictions



