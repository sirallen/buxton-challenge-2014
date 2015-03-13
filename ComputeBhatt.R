setwd('C:/Users/Allen/Dropbox/activities/Buxton Challenge')
da = read.csv('CTL Data - Mod.csv')
dim(da)

desc = read.csv('Descriptions - Mod.csv')
desc = subset(desc, !(Variable %in% c('NONCALI_RES_GRAV','NONCALI_WRK_GRAV','Region_AvgSales')))

# Substitute NA for mislabeled 0's where appropriate
temp0 = da[,as.character(desc$Variable[which(desc$Not_Zero==1)])]
da[,as.character(desc$Variable[which(desc$Not_Zero==1)])][temp0==0] = NA
rm(temp0)

# Demographic categories
demog.cats = c('Age','Family','Household','Income','Education')
demog.ind.cols = paste0('Is_', demog.cats, '_PCT')

for (d in 1:length(demog.ind.cols)) {
  demog.col = desc[,demog.ind.cols[d]]
  da2 = subset(da, select=as.character(desc$Variable[which(demog.col==1)]))
  dropcols = (grepl('8TO',colnames(da2)) | grepl('1RO',colnames(da2)))
  da2 = as.matrix(da2[,!dropcols]/100)
  
  # Compute pairwise Bhattacharyya distances
  Bhatt.mat = pmax(-log(sqrt(da2) %*% sqrt(t(da2))), 0)
  write.csv(Bhatt.mat,file=paste0('Bhatt',demog.cats[d],'Dist.csv'))
}



