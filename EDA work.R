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
require(psych)
require(GPArotation)
# homals(poi[,1:110],level='n',ndim=3,rank=3)
poe<-poi[,1:36]
pom<-poi[,37:68]
pos<-poi[,69:110]

#composes correlated factors from domains' multiple choice questions
fe<-fa.poly(poe,nfactors=2,n.iter=1,rotate='oblimin',fm='ml',oblique.scores=T)
fm<-fa.poly(pom,nfactors=5,n.iter=1,rotate='oblimin',fm='ml',oblique.scores=T)
fs<-fa.poly(pos,nfactors=3,n.iter=1,rotate='oblimin',fm='ml',oblique.scores=T)

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


feng<-fa.poly(eng,nfactors=3,n.iter=1,rotate='oblimin',fm='ml',oblique.scores=T)
fmat<-fa.poly(mat,nfactors=6,n.iter=1,rotate='oblimin',fm='ml',oblique.scores=T)
# don't use this one yet. We can't solve this for solutions before disentangling the two tests.
# fsci<-fa.poly(sci,nfactors=2,n.iter=1,rotate='oblimin',fm='ml',oblique.scores=T)



