d<-read.csv('~/Documents/workspace/DataCompete/Student Data Competition/MCAS.csv',header=T)
# require(foreign)
# a<-read.spss('~/Documents/workspace/DataCompete/Student Data Competition/missingDataCoded.sav',use.value.labels=T,to.data.frame=T,use.missings=T)
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
require(psych)
require(GPArotation)
# homals(poi[,1:110],level='n',ndim=3,rank=3)
poe<-poi[,1:36]
pom<-poi[,37:68]
pos<-poi[,69:110]

#composes correlated factors from domains' multiple choice questions
fe<-fa.poly(poe,nfactors=2,n.iter=100,rotate='oblimin',fm='ml',oblique.scores=T)
fm<-fa.poly(pom,nfactors=5,n.iter=100,rotate='oblimin',fm='ml',oblique.scores=T)
fs<-fa.poly(pos,nfactors=3,n.iter=100,rotate='oblimin',fm='ml',oblique.scores=T)

fa.diagram(fe)
fa.diagram(fm)
fa.diagram(fs)
#composes correlated factors from each domain's questions, including numeric

# codive <- function(x) {ifelse(is.integer(x),x,ifelse(x=='+',1,ifelse(x==' ',NA,0)))}

# sapply(eng,is.numeric)
# (eng[,sapply(eng,is.numeric)]) gives all numeric
eng<-d[,3:42]
eng<-data.frame(codive(eng))
eng$eitem9<-d$eitem9
eng$eitem18<-d$eitem18
eng$eitem27<-d$eitem27
eng$eitem35<-d$eitem35

mat<-d[,49:90]
mat<-data.frame(codive(mat))
mat$mitem15<-d$mitem15
mat$mitem16<-d$mitem16
mat$mitem17<-d$mitem17
mat$mitem18<-d$mitem18
mat$mitem19<-d$mitem19
mat$mitem20<-d$mitem20
mat$mitem21<-d$mitem21
mat$mitem31<-d$mitem31
mat$mitem41<-d$mitem41
mat$mitem42<-d$mitem42

sci<-d[,96:140]
sci<-data.frame(codive(sci))
sci$sitem23<-d$sitem23
sci$sitem44<-d$sitem44
sci$sitem45<-d$sitem45


feng<-fa.poly(eng,nfactors=3,n.iter=1000,rotate='oblimin',fm='ml',oblique.scores=T)
fa.diagram(feng)
fmat<-fa.poly(mat,nfactors=5,n.iter=1000,rotate='oblimin',fm='ml',oblique.scores=T)

# don't use this one yet. We can't solve this for solutions before disentangling the two tests.
# fsci<-fa.poly(sci,nfactors=2,n.iter=1,rotate='oblimin',fm='ml',oblique.scores=T)

##below is just scratchwork, I'll remove it when we've got a solution.

require(poLCA)
# codive <- function(x) {ifelse(x=='+',2,ifelse(x==' ',NA,1))}

# sci2<-d[,96:140]
# sci2<-as.data.frame(codive(sci2))
# sci2$sitem23<-NULL
# sci2$sitem44<-NULL
# sci2$sitem45<-NULL

#Latent class analysis, ultimately pretty useless.
poLCA(cbind(sitem1,sitem2,sitem3,sitem4,sitem5,sitem6,sitem7,sitem8,sitem9,sitem10,sitem11,sitem12,sitem13,sitem14,sitem15,sitem16,sitem17,sitem18,sitem19,sitem20,sitem21,sitem22,sitem24,sitem25,sitem26,sitem27,sitem28,sitem29,sitem30,sitem31,sitem32,sitem33,sitem34,sitem35,sitem36,sitem37,sitem38,sitem39,sitem40,sitem41,sitem42,sitem43)~1,sci2,nclass=2,maxiter=5000,nrep=2500,verbose=3)

##Below gives us two of the three domains to work with.
two<-d[,3:90]
two[,43:48]<-list(NULL)
codive <- function(x) {ifelse(x=='+',1,ifelse(x==' ',NA,0))}
two<-data.frame(codive(two))
two$eitem9<-d$eitem9
two$eitem18<-d$eitem18
two$eitem27<-d$eitem27
two$eitem35<-d$eitem35
two$mitem15<-d$mitem15
two$mitem16<-d$mitem16
two$mitem17<-d$mitem17
two$mitem18<-d$mitem18
two$mitem19<-d$mitem19
two$mitem20<-d$mitem20
two$mitem21<-d$mitem21
two$mitem31<-d$mitem31
two$mitem41<-d$mitem41
two$mitem42<-d$mitem42

f.two<-fa.poly(two,nfactors=5,n.iter=1000,rotate='bifactor',fm='minres',oblique.scores=T)
fa.diagram(f.two)
fa.parallel.poly(eng,1000,SMC=T,fm='ml',T)

<<<<<<< HEAD
f.htwo<-omega(two,nfactors=9,fm="ml",n.iter=1,poly=T,flip=TRUE,digits=3,
      title="English and Math Sections",sl=F,
      plot=TRUE,n.obs=NA,rotate="oblimin")

require(gdata)
require(car)
demo<-d[,145:172]
demo$id<-NULL
demo<-demo[demo$eth!='M' & demo$eth!='P' & demo$eth!='N' & demo$quest3!='E',]
demo$eth<-drop.levels(demo$eth,reorder=T)
demo$quest10<-recode(demo$quest10,"' '=NA")
demo$quest10<-as.numeric(demo$quest10)
fa.poly(demo,nfactors=5,n.iter=1,rotate='oblimin',fm='ml',global=F)
=======
omega(two,nfactors=10,fm="ml",n.iter=1,poly=T,flip=TRUE,digits=3,
      title="Omega",sl=F,labels=NULL,
      plot=TRUE,n.obs=NA,rotate="oblimin")
>>>>>>> fce11648fca816fd07345425fac464279a7fd3dc
