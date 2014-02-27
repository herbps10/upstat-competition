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
require(psych)
<<<<<<< HEAD
require(GPArotation)
# homals(poi[,1:110],level='n',ndim=3,rank=3)
poe<-poi[,1:36]
pom<-poi[,37:68]
pos<-poi[,69:110]

fe<-fa.poly(poe,nfactors=3,n.iter=100,rotate='oblimin',fm='ml',oblique.scores=T)
fm<-fa.poly(pom,nfactors=3,n.iter=100,rotate='oblimin',fm='ml',oblique.scores=T)
fs<-fa.poly(pos,nfactors=3,n.iter=100,rotate='oblimin',fm='ml',oblique.scores=T)

fa.diagram(poe)
fa.diagram(pom)
fa.diagram(pos)
=======
require(polychor)
# homals(poi[,1:110],level='n',ndim=3,rank=3)
dich<-homals(poi[,1:110], ndim = 3, rank = 3, level = "nominal", sets = 0, active = T, eps = 1e-06, itermax = 1000, verbose = 3)

m.tet<-tetrachoric(poi,correct=T,smooth=T)
fa(m.tet)
fa.poly(m.tet,n.iter=100,rotate='oblimin',fm='ml',missing=T,symmetric=T)
>>>>>>> 22e2dec70ae4aab5796ed9210c1f5f0a48438359
