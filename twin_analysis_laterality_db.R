#MZ vs DZ correlations, 
#based on models_lat measures by diagnosis
#NB change in code line 26 to correct 'sex' to 'female' as variable name had changed

require(yarrr)
require(lmerTest)
require(tidyverse)

mydir<-"/Users/dorothybishop/Dropbox/ERCadvanced/project twin kids/Project_files/"

file<-"TwinsData_DATA_2017-12-13_1033.csv"
data<-read.csv(paste0(mydir,"Data/",file))

#-------------------------------------------------------
# Create a variable that is 1 if L language and strong R on QHP and EHP and 0 otherwise
#-------------------------------------------------------
data$lat3<-data$lateralised_category
w<-which(data$qhp_freq_r<14)
data$lat3[w]<-0
w<-which(data$ehp_handedness<8)
data$lat3[w]<-0
w<-which(data$lateralised_category==-1)
data$lat3[w]<-0
w<-which(colnames(data)=='include')
colnames(data)[w]<-'myinclude' #to avoid problems with having 'include' as column name
#----------------------------------------------------------
# Produce data frame of selected columns 
#----------------------------------------------------------
data.short<-filter(data,zygosity<9)
data.short<-dplyr::select(data,record_id,fam_id,age_at_test,female,zygosity,twin,n_trials,laterality_index,qhp_freq_r,ehp_right,ehp_either,lang_probs,myinclude,lat3)

#-------------------------------------------------------
# Create double entry file with twin 1 and 2 aligned
#-------------------------------------------------------
nrec<-nrow(data.short)
ncol<-ncol(data.short)
nuorder2<-c(seq(from=2,to=nrec,by=2),seq(from=1,to=nrec,by=2))
nuorder1<-c(seq(from=1,to=nrec,by=2),seq(from=2,to=nrec,by=2))
doubledata<-cbind(data.short[nuorder1,],data.short[nuorder2,])
colnames(doubledata)[15:28]<-paste0(colnames(doubledata)[15:28],2)
#check all aligned
check<-sum(doubledata$fam_id-doubledata$fam_id2)
if(check>0) {print('Twins not aligned!!!')}


#-------------------------------------------------------
# Exclude those with unusable data (NB do this after double entry file created)
#-------------------------------------------------------

Nexcluded<-rep(0,3)#records numbers excluded at each step: 1) exclusions based on diagnosis and hearing, etc
#2) by lack of useable fTCD data, 3) by extreme LI, defined as +/-10
doubledata<-filter(doubledata,myinclude>0,myinclude2>0)
Nexcluded[1]<-nrec-nrow(doubledata) 
doubledata<-filter(doubledata,n_trials> 11,n_trials2>11)
Nexcluded[2]<-nrec-nrow(doubledata)-Nexcluded[1]
doubledata<-filter(doubledata,abs(laterality_index)<10,abs(laterality_index2)<10)
Nexcluded[3]<-nrec-nrow(doubledata)-Nexcluded[1]-Nexcluded[2]

#unit of analysis is twin pair excluded, so need to divide by 2 as one row per twin
Nexcluded<-Nexcluded/2

#remove unwanted columns
doubledata<-dplyr::select(doubledata,fam_id,age_at_test,female,zygosity,twin,n_trials,laterality_index,qhp_freq_r,
                          ehp_right,ehp_either,lang_probs,lat3,n_trials2,laterality_index2,qhp_freq_r2,ehp_right2,ehp_either2,
                          lang_probs2,lat32)

#scatterplot and correlation by zygosity
# for (i in 1:3){
#   tempdat <-filter(doubledata,zygosity==i)
#   #correlation from double entry gives ICC
#   rlat<-cor(tempdat$laterality_index,tempdat$laterality_index2)
#   tempdat1<-filter(tempdat,twin==1)
#   plot(tempdat1$laterality_index,tempdat1$laterality_index2)
# }

#scatterplot and correlation by zygosity as MZ/DZ
png(filename = "zygolat.png",
    width = 750, height = 400, units = "px", res=80,
    bg = "white")
par(mfrow=c(1,2))
doubledata$MZDZ<-2
w<-which(doubledata$zygosity==1)
doubledata$MZDZ[w]<-1
for (i in 1:2){
  mymain='MZ: '
  if (i==2){mymain='DZ: '}
  tempdat <-filter(doubledata,MZDZ==i)
  #correlation from double entry gives ICC
  rlat<-cor(tempdat$laterality_index,tempdat$laterality_index2)
  mysub<-paste0('r = ',toString(round(rlat,3)))
  tempdat1<-filter(tempdat,twin==1)
  mymain=paste0(mymain,'N = ',nrow(tempdat1),' pairs')
  plot(tempdat1$laterality_index,tempdat1$laterality_index2,xlim=c(-6,9),ylim=c(-6,9),
       xlab='LI twin 1',ylab='LI twin 2',main=mymain,cex.lab=1.5,cex.main=1.6)
  text(-4,8,mysub,col='red',font=2,cex=1.4)
  abline(h=0,col=4,lty=2)
  abline(v=0,col=4,lty=2)
}
dev.off()

#to look at correlation for extreme vs inconsistent - need to do for each zygo
#so far not promising!
cor(tempdat$lat3,tempdat$lat32)

