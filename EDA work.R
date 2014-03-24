d<-read.csv('~/Documents/workspace/DataCompete/Student Data Competition/MCAS.csv',header=T)
# require(foreign)
# a<-read.spss('~/Documents/workspace/DataCompete/Student Data Competition/missingDataCoded.sav',use.value.labels=T,to.data.frame=T,use.missings=T)
colnames(d)<-tolower(colnames(d))
d$id<-rownames(d)
d$id<-as.integer(d$id)

d$firstlanguage<-factor(d$firstlanguage)
d$grade<-factor(d$grade)
d$sex<-d$gender
d$eth<-d$race_off
d$race_off<-d$gender<-NULL
a1<-as.data.frame(codive(a[sapply(a,is.factor)]))
codive <- function(x) {ifelse(x=='+',1,ifelse(x==' ',NA,0))}

poi<-as.data.frame(cbind(codive(d[sapply(d,is.factor)]),d$id,d$scitry))
poi$sex<-poi$eth<-poi$grade<-poi$firstlanguage<-NULL
poi$id<-poi$V140
poi$scitry<-poi$V141
poi$V140<-poi$V141<-NULL
poi[111:135]<-list(NULL)


# sapply(eng,is.numeric)
# (eng[,sapply(eng,is.numeric)]) gives all numeric
eng<-poi[,c(1:36,111)]
engx<-d[c(170,11,20,29,37)]

eng<-merge(eng,engx,by='id')

mat<-poi[,c(37:68,111)]
matx<-d[,c(170,63:69,79,89,90)]
mat<-merge(mat,matx,by='id')

sci1<-d[d$scitry==1,c(95:140,170)]
require(gdata)
sci1<-as.data.frame(lapply(sci1,drop.levels))
scix<-sci1[,c(13,24,33,45,46,47)]
sci1$sitem12<-sci1$sitem32<-NULL
sci1<-as.data.frame(cbind(codive(sci1[sapply(sci1,is.factor)]),sci1$id))
sci1$id<-sci1$V41
sci1$V41<-NULL

scix[scix==' ']<-NA
scix<-as.data.frame(lapply(scix,as.integer))
sci1<-merge(sci1,scix,by='id')

sci2<-d[d$scitry==2,c(95:140,170)]
sci2<-as.data.frame(lapply(sci2,drop.levels))
scixx<-sci2[,c(12,24,34,45,46,47)]
sci2<-as.data.frame(cbind(codive(sci2[sapply(sci2,is.factor)]),sci2$id))
sci2$id<-sci2$V43
sci2$V43<-NULL
scixx[scixx==' ']<-NA
scixx<-as.data.frame(lapply(scixx,as.integer))
sci2<-merge(sci2,scixx,by='id')

sci3<-d[d$scitry==3,c(95:140,170)]
sci3<-as.data.frame(lapply(sci3,drop.levels))
scixxx<-sci3[,c(13,24,33,45,46,47)]
sci3<-as.data.frame(cbind(codive(sci3[sapply(sci3,is.factor)]),sci3$id))
sci3$id<-sci3$V43
sci3$V43<-NULL

scixxx[scixxx==' ']<-NA
scixxx<-as.data.frame(lapply(scixxx,as.integer))
sci3<-merge(sci3,scixxx,by='id')

sci4<-d[d$scitry==4,c(95:140,170)]
sci4<-as.data.frame(lapply(sci4,drop.levels))
scixxxx<-sci4[,c(12,24,34,45,46,47)]
sci4<-as.data.frame(cbind(codive(sci4[sapply(sci4,is.factor)]),sci4$id))
sci4$id<-sci4$V43
sci4$V43<-NULL

scixxxx[scixxxx==' ']<-NA
scixxxx<-as.data.frame(lapply(scixxxx,as.integer))
sci4<-merge(sci4,scixxxx,by='id')

require(psych)
feng<-fa.poly(eng[,c(2:41)],nfactors=3,n.iter=10,rotate='oblimin',fm='ml',oblique.scores=T)
# fengh<-omega(poe,nfactors=3,n.iter=1000,rotate='oblimin',fm='ml',sl=F)
fa.diagram(feng)
fmat<-fa.poly(mat,nfactors=5,n.iter=1000,rotate='oblimin',fm='ml',oblique.scores=T)

fsci1<-fa.poly(sci1,nfactors=4,n.iter=100,rotate='oblimin',fm='ml',oblique.scores=T)

##Below gives us two of the three domains to work with.
two<-merge(eng,mat,by='id')
f.two<-fa.poly(two,nfactors=5,n.iter=100,rotate='oblimin',fm='ml',oblique.scores=T)
fa.diagram(f.two)
fa.parallel.poly(two,1000,SMC=T,fm='ml',T)


# f.htwo<-omega(two,nfactors=7,fm="ml",n.iter=1,poly=T,flip=TRUE,digits=3,
#       title="English and Math Sections",sl=F,
#       plot=TRUE,n.obs=NA,rotate="oblimin")

require(semTools)
require(lavaan)
require(mice)

two<-merge(eng,mat,by='id')
d1<-d[,c(170:172,45,91,141)]
two<-merge(two,d1,by='id')
three<-merge(two,sci1,by='id')
d3<-three[three$eth!='M' & three$eth!='P' & three$eth!='N',]
d2<-two[two$eth!='M' & two$eth!='P' & two$eth!='N',]

require(gdata)
d3$eth<-drop.levels(d3$eth,reorder=T)
d2$eth<-drop.levels(d2$eth,reorder=T)

engl<-'
e1=~eitem8+eitem10+eitem11+eitem12+eitem13+eitem16+eitem17+eitem20+eitem21+eitem22+eitem23+eitem24+eitem25+eitem26+eitem28+eitem29+eitem30+eitem31+eitem32+eitem33+eitem34+eitem36+eitem37
e2=~eitem27+eitem35+eitem9+eitem18
e3=~eitem38+eitem39+eitem40
erawsc~e1+e2+e3'
maths<-'m1=~mitem11+mitem12+mitem14+mitem25+mitem32+mitem35+mitem37+mitem40
m2=~mitem6+mitem10+mitem17+mitem24+mitem39
m3=~mitem22+mitem28+mitem34+mitem38+mitem41
m4=~mitem18+mitem20+mitem21+mitem31+mitem42
mrawsc~m1+m2+m3+m4'
scien<-'s1=~sitem1+sitem2+sitem13+sitem14+sitem15+sitem22+sitem27+sitem30+sitem33+sitem34+sitem37+sitem40+sitem41
s2=~sitem3+sitem5+sitem18+sitem19+sitem28+sitem31+sitem35+sitem43+sitem45
s3=~sitem8+sitem10+sitem11+sitem12+sitem23+sitem24+sitem25
s4=~sitem4+sitem23+sitem29+sitem32+sitem44
srawsc~s1+s2+s3+s4'
  
  
MI.eng<-measurementInvariance(engl,d3,group='eth',missing='fiml',bootstrap=150,strict=T)
MI.maths<-measurementInvariance(maths,d3,group='eth',missing='fiml',bootstrap=150,strict=T)
MI.scien<-measurementInvariance(scien,d3,group='eth',missing='fiml',bootstrap=150,strict=T)

fitMeasures(MI.eng$fit.configural, c('chisq','tli', 'cfi','rmsea','aic'))
fitMeasures(MI.eng$fit.loadings, c('chisq','tli', 'cfi','rmsea','aic'))
fitMeasures(MI.eng$fit.intercepts, c('chisq','tli', 'cfi','rmsea','aic'))
fitMeasures(MI.eng$fit.residuals, c('chisq','tli', 'cfi','rmsea','aic'))
fitMeasures(MI.eng$fit.means, c('chisq','tli', 'cfi','rmsea','aic'))

fitMeasures(MI.maths$fit.configural, c('chisq','tli', 'cfi','rmsea','aic'))
fitMeasures(MI.maths$fit.loadings, c('chisq','tli', 'cfi','rmsea','aic'))
fitMeasures(MI.maths$fit.intercepts, c('chisq','tli', 'cfi','rmsea','aic'))
fitMeasures(MI.maths$fit.residuals, c('chisq','tli', 'cfi','rmsea','aic'))
fitMeasures(MI.maths$fit.means, c('chisq','tli', 'cfi','rmsea','aic'))

fitMeasures(MI.scien$fit.configural, c('chisq','tli', 'cfi','rmsea','aic'))
fitMeasures(MI.scien$fit.loadings, c('chisq','tli', 'cfi','rmsea','aic'))
fitMeasures(MI.scien$fit.intercepts, c('chisq','tli', 'cfi','rmsea','aic'))
fitMeasures(MI.scien$fit.residuals, c('chisq','tli', 'cfi','rmsea','aic'))
fitMeasures(MI.scien$fit.means, c('chisq','tli', 'cfi','rmsea','aic'))


d4<-d[,c(145:172,45,91,141,95,2)]
require(mvpart)
fit<-mvpart(cbind(mrawsc,erawsc,srawsc)~.,d4)

require(foreign)
a<-read.spss('/home/landon/Documents/workspace/DataCompete/Student Data Competition/missingDataCoded.sav',use.value.labels=T,to.data.frame=T,use.missings=T)

d5<-a[,c(147:171,47,93,143,97,2,3)]
colnames(d5)<-tolower(colnames(d5))
d5$eth<-d5$race_off
d5$race_off<-NULL


require(mice)
d6<-mice(d5,m=13,maxit=15,defaultMethod = c("norm","logreg","polyreg"))
summary(aggr(d5[,c(1,7:31)]))

d5.fixed = subset(d5, select=-c(quest2a, quest2b, quest2c, quest2d, quest2e))

summary(manova(cbind(mrawsc,erawsc,srawsc)~.,d5.fixed)) #this is on the data with missing 
summary(lm(cbind(mrawsc,erawsc,srawsc)~.,d5.fixed)) #this is on the data with missing 

#d6.fixed = subset(d6$imp, select=-c(quest2a, quest2b, quest2c, quest2d, quest2e))

summary(with(d6,manova(cbind(mrawsc,erawsc,srawsc)~., d6)))
summary(with(d6,lm(cbind(mrawsc,erawsc,srawsc)~., d6)))

# require(VIM)
# a$erawsc<-a$emcpts<-a$eorpts<-a$ecpi<-a$mrawsc<-a$mmcpts<-a$morpts<-a$mcpi<-a$srawsc<-a$smcpts<-a$sorpts<-a$scpi<-a$eitem41<-a$eitem42<-a$scitry<-NULL
# summary(aggr(a))
