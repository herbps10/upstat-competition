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
sci1<-as.data.frame(cbind(codive(sci1[sapply(sci1,is.factor)]),sci1$id))
sci1$id<-sci1$V43
sci1$V43<-NULL

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
feng<-fa.poly(eng,nfactors=3,n.iter=1000,rotate='oblimin',fm='ml',oblique.scores=T)
# fengh<-omega(poe,nfactors=3,n.iter=1000,rotate='oblimin',fm='ml',sl=F)
fa.diagram(feng)
fmat<-fa.poly(mat,nfactors=5,n.iter=1000,rotate='oblimin',fm='ml',oblique.scores=T)

fsci1<-fa.poly(sci1,nfactors=4,n.iter=1,rotate='oblimin',fm='ml',oblique.scores=T)

##Below gives us two of the three domains to work with.
two<-merge(eng,mat,by='id')
f.two<-fa.poly(two,nfactors=5,n.iter=1000,rotate='oblimin',fm='ml',oblique.scores=T)
fa.diagram(f.two)
fa.parallel.poly(two,1000,SMC=T,fm='ml',T)


f.htwo<-omega(two,nfactors=7,fm="ml",n.iter=1,poly=T,flip=TRUE,digits=3,
      title="English and Math Sections",sl=F,
      plot=TRUE,n.obs=NA,rotate="oblimin")

require(semTools)
require(lavaan)
require(mice)

two<-merge(eng,mat,by='id')
d1<-d[,c(170:172)]
two<-merge(two,d1,by='id')
#d3<-d3[d3$eth!='M' & d3$eth!='P' & d3$eth!='N',]
d3$eth<-d$eth
d3<-d3[d3$eth!='M' & d3$eth!='P' & d3$eth!='N',]
require(gdata)
d3$eth<-drop.levels(d3$eth,reorder=T)

invariance="f1=~eitem8+eitem13+eitem22+eitem26+eitem28+eitem30+eitem31
            f2=~eitem38+eitem39+eitem40
            f4=~mitem40+mitem42+mitem35+mitem12+mitem11+mitem5
            f5=~mitem6+mitem10+mitem17+mitem39
            f6=~mitem22+mitem28+mitem34+mitem38+mitem31
            f7=~mitem18+mitem20+mitem21+mitem31+mitem41
            erawsc~f1+f2
            mrawsc~f4+f5+f6+f7"

MI.model<-measurementInvariance(invariance,data=d3,group='eth',missing='fiml',bootstrap=1000)


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


d4<-d[,c(147:171,93,47,143)]
require(mvpart)
fit1<-mvpart(cbind(d4$mrawsc,d4$erawsc,d4$srawsc)~.,data=d4)

summary(manova(cbind(d4$mrawsc,d4$erawsc,d4$srawsc)~.,d4))
summary(lm(cbind(d4$mrawsc,d4$erawsc,d4$srawsc)~.,d4))
require(VIM)
a$erawsc<-a$emcpts<-a$eorpts<-a$ecpi<-a$mrawsc<-a$mmcpts<-a$morpts<-a$mcpi<-a$srawsc<-a$smcpts<-a$sorpts<-a$scpi<-a$eitem41<-a$eitem42<-a$scitry<-NULL
summary(aggr(a))
