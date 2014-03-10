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


codive2<-function(x){ifelse(x==' ',NA,as.numeric(x))}
require(gdata)
sci<-d[,96:140]
sci$scitry<-d$scitry
sci1<-sci[sci$scitry==1,]
da<-sci1
# lapply(sci1,table)
sci1<-drop.levels(sci1)
da<-drop.levels(da)
sci1<-as.data.frame(codive(sci1[sapply(sci1,is.factor)]))
sci1$sitem12<-codive2(da$sitem12)
sci1$sitem23<-codive2(da$sitem23)
sci1$sitem32<-codive2(da$sitem32)
sci1$sitem44<-codive2(da$sitem44)
sci1$sitem45<-codive2(da$sitem45)
sci1$scitry<-NULL

sci2<-sci[sci$scitry==2,]
sci2$scitry<-NULL
# lapply(sci2,table)
db<-sci[sci$scitry==2,]

sci2<-drop.levels(sci2)
db<-drop.levels(db)
sci2<-as.data.frame(codive(sci2[sapply(sci2,is.factor)]))
sci2$sitem11<-codive2(db$sitem11)-1
sci2$sitem23<-codive2(db$sitem23)
sci2$sitem33<-codive2(db$sitem33)-1
sci2$sitem44<-codive2(db$sitem44)
sci2$sitem45<-codive2(db$sitem45)


sci3<-sci[sci$scitry==3,]
sci3$scitry<-NULL
dc<-sci3
dc<-drop.levels(dc)
sci3<-drop.levels(sci3)
# lapply(sci3,table)

sci3<-as.data.frame(codive(sci3[sapply(sci3,is.factor)]))
sci3$sitem12<-codive2(dc$sitem12)
sci3$sitem23<-codive2(dc$sitem23)+1
sci3$sitem32<-codive2(dc$sitem32)
sci3$sitem44<-codive2(dc$sitem44)+1
sci3$sitem45<-codive2(dc$sitem45)+1


sci4<-sci[sci$scitry==4,]
sci4$scitry<-NULL
sci4<-drop.levels(sci4)
dd<-sci[sci$scitry==4,]
dd<-drop.levels(dd)
# lapply(sci4,table)

sci4<-as.data.frame(codive(sci4[sapply(sci4,is.factor)]))

sci4$sitem11<-codive2(dd$sitem11)
sci4$sitem23<-codive2(dd$sitem23)
sci4$sitem33<-codive2(dd$sitem33)-1
sci4$sitem44<-codive2(dd$sitem44)
sci4$sitem45<-codive2(dd$sitem45)



feng<-fa.poly(eng,nfactors=3,n.iter=1000,rotate='oblimin',fm='ml',oblique.scores=T)
fengh<-omega(poe,nfactors=3,n.iter=1000,rotate='oblimin',fm='ml',sl=F)
fa.diagram(feng)
fmat<-fa.poly(mat,nfactors=5,n.iter=1,rotate='oblimin',fm='ml',oblique.scores=T)

# x<-read.csv('~/Documents/workspace/DataCompete/Student Data Competition/bio.txt')
# y<-read.csv('~/Documents/workspace/DataCompete/Student Data Competition/phys.txt')
# This just pulls information that I built using pdftotext and the two MC guides.

fsci1<-fa.poly(sci1,nfactors=4,n.iter=1,rotate='oblimin',fm='ml',oblique.scores=T)
fsci2<-fa.poly(sci2,nfactors=4,n.iter=1,rotate='oblimin',fm='pa',oblique.scores=T,na.rm=T)

fsci3<-fa.poly(sci3,nfactors=4,n.iter=5,rotate='oblimin',fm='minres',oblique.scores=T)
fsci4<-fa.poly(sci4,nfactors=4,n.iter=1,rotate='oblimin',fm='ml',oblique.scores=T)


# require(poLCA)
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


f.htwo<-omega(two,nfactors=7,fm="ml",n.iter=1,poly=T,flip=TRUE,digits=3,
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

require(semTools)
require(lavaan)
require(mice)

d2<-d[,3:90]
# d2[,44:46]<-list(NULL)
codive <- function(x) {ifelse(x=='+',1,ifelse(x==' ',NA,0))}
d3<-data.frame(codive(d2))
d3<-as.data.frame(lapply(d3,factor))
d3$eitem9<-d2$eitem9
d3$eitem18<-d$eitem18
d3$eitem27<-d$eitem27
d3$eitem35<-d$eitem35
d3$mitem15<-d$mitem15
d3$mitem16<-d$mitem16
d3$mitem17<-d$mitem17
d3$mitem18<-d$mitem18
d3$mitem19<-d$mitem19
d3$mitem20<-d$mitem20
d3$mitem21<-d$mitem21
d3$mitem31<-d$mitem31
d3$mitem41<-d$mitem41
d3$mitem42<-d$mitem42
d3$erawsc<-d$erawsc
d3$mrawsc<-d$mrawsc
d3$eitem41<-NULL
d3$eitem42<-NULL

#d3<-d3[d3$eth!='M' & d3$eth!='P' & d3$eth!='N' & d3$quest3!='E',]
d3<-d3[d3$eth!='M' & d3$eth!='P' & d3$eth!='N' & d3$quest3!='E',]
d3$eth<-drop.levels(d3$eth,reorder=T)


invariance="f1=~eitem8+eitem13+eitem22+eitem26+eitem28+eitem30+eitem31
            f2=~eitem38+eitem39+eitem40
            f4=~mitem40+mitem42+mitem35+mitem12+mitem11+mitem5
            f5=~mitem6+mitem10+mitem17+mitem39
            f6=~mitem22+mitem28+mitem34+mitem38+mitem31
            f7=~mitem18+mitem20+mitem21+mitem31+mitem41
            erawsc~f1+f2
            mrawsc~f4+f5+f6+f7"

MI.model<-measurementInvariance(invariance,data=d3,group='eth',missing='fiml',bootstrap=75)

sci_inv<-'f3=~sitem1+sitem2+sitem13+sitem14+sitem15+sitem22+sitem27+sitem30+sitem33+sitem34+sitem37+sitem40+sitem41
f2=~sitem3+sitem5+sitem18+sitem19+sitem28+sitem31+sitem35+sitem43+sitem45
f1=~sitem8+sitem10+sitem11+sitem12+sitem24+sitem23
f4=~sitem4+sitem29+sitem32+sitem44
srawsc~f1+f2+f3+f4'

sci<-d[,96:140]
sci$srawsc<-d$srawsc
sci$scitry<-d$scitry
sci$eth<-d$eth
sci1<-sci[sci$scitry==1,]
da<-sci1
# lapply(sci1,table)
sci1<-drop.levels(sci1)
da<-drop.levels(da)
sci1<-as.data.frame(codive(sci1[sapply(sci1,is.factor)]))
sci1$sitem12<-codive2(da$sitem12)
sci1$sitem23<-codive2(da$sitem23)
sci1$sitem32<-codive2(da$sitem32)
sci1$sitem44<-codive2(da$sitem44)
sci1$sitem45<-codive2(da$sitem45)
sci1$scitry<-NULL
sci1$srawsc<-da$srawsc
sci1$eth<-da$eth
sci1<-sci1[sci1$eth !='M' & sci1$eth !='P' & sci1$eth !='N',]
sci1$eth<-drop.levels(sci1$eth,reorder=T)
MI.model2<-measurementInvariance(sci_inv,data=sci1,group='eth',missing='fiml',bootstrap=75)
