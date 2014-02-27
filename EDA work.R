d<-read.csv('/home/landon/Documents/workspace/DataCompete/Student Data Competition/MCAS.csv',header=T)
# require(foreign)
# a<-read.spss('/home/landon/Documents/workspace/DataCompete/Student Data Competition/missingDataCoded.sav',use.value.labels=T,to.data.frame=T,use.missings=T)
colnames(d)<-tolower(colnames(d))
d$id<-rownames(d)
d$id<-factor(d$id)

codive <- function(x) {ifelse(x=='+',1,ifelse(x==' ',NA,0))}

poi<-as.data.frame(codive(d[sapply(d,is.factor)]))
poi$gender<-poi$race_off<-NULL
poi[,111:135]<-list(NULL)


d$firstlanguage<-factor(d$firstlanguage)
d$grade<-factor(d$grade)
d$sex<-d$gender
d$eth<-d$race_off

d$race_off<-NULL
d$gender<-NULL

# unique(sort(d$firstlanguage))
# cor(d[sapply(d,is.numeric)],use='pairwise.complete.obs')
require(homals)
homals(poi[,1:110],level='n',verbose=3)

