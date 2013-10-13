library(GPArotation)
library(psych)

# R<-input$dataset
# n.obs<-n()
# k<-input$k
# p<-p()

R<-cognitive
n.obs<-n.coginitive
k<-5
p<-p.cognitive


A <- factanal(covmat=R,n.obs=n.obs,factors=k,maxit=1000,rotation="none")



str(A)
FPM<-A$loadings[1:p,]
F<-cbind(F,matrix(numeric(0),p(),p()-input$k))
colnames(F)<-paste0("F",1:ncol(R))




oblimin(L, Tmat=diag(ncol(L)), gam=0,            normalize=FALSE, eps=1e-5, maxit=1000)
quartimin(L, Tmat=diag(ncol(L)),                 normalize=FALSE, eps=1e-5, maxit=1000)
targetT(L, Tmat=diag(ncol(L)),      Target=NULL, normalize=FALSE, eps=1e-5, maxit=1000)
targetQ(L, Tmat=diag(ncol(L)),      Target=NULL, normalize=FALSE, eps=1e-5, maxit=1000)
pstT(L, Tmat=diag(ncol(L)), W=NULL, Target=NULL, normalize=FALSE, eps=1e-5, maxit=1000)
pstQ(L, Tmat=diag(ncol(L)), W=NULL, Target=NULL, normalize=FALSE, eps=1e-5, maxit=1000)
oblimax(L, Tmat=diag(ncol(L)),                   normalize=FALSE, eps=1e-5, maxit=1000)
entropy(L, Tmat=diag(ncol(L)),                   normalize=FALSE, eps=1e-5, maxit=1000)
quartimax(L, Tmat=diag(ncol(L)),                 normalize=FALSE, eps=1e-5, maxit=1000)
Varimax(L, Tmat=diag(ncol(L)),                   normalize=FALSE, eps=1e-5, maxit=1000)
simplimax(L, Tmat=diag(ncol(L)),      k=nrow(L), normalize=FALSE, eps=1e-5, maxit=1000)
bentlerT(L, Tmat=diag(ncol(L)),                  normalize=FALSE, eps=1e-5, maxit=1000)
bentlerQ(L, Tmat=diag(ncol(L)),                  normalize=FALSE, eps=1e-5, maxit=1000)
tandemI(L, Tmat=diag(ncol(L)),                   normalize=FALSE, eps=1e-5, maxit=1000)
tandemII(L, Tmat=diag(ncol(L)),                  normalize=FALSE, eps=1e-5, maxit=1000)
geominT(L, Tmat=diag(ncol(L)),        delta=.01, normalize=FALSE, eps=1e-5, maxit=1000)
geominQ(L, Tmat=diag(ncol(L)),        delta=.01, normalize=FALSE, eps=1e-5, maxit=1000)
cfT(L, Tmat=diag(ncol(L)),              kappa=0, normalize=FALSE, eps=1e-5, maxit=1000)
cfQ(L, Tmat=diag(ncol(L)),              kappa=0, normalize=FALSE, eps=1e-5, maxit=1000)
infomaxT(L, Tmat=diag(ncol(L)),                  normalize=FALSE, eps=1e-5, maxit=1000)
infomaxQ(L, Tmat=diag(ncol(L)),                  normalize=FALSE, eps=1e-5, maxit=1000)
mccammon(L, Tmat=diag(ncol(L)),                  normalize=FALSE, eps=1e-5, maxit=1000)
bifactorT(L, Tmat=diag(ncol(L)),                 normalize=FALSE, eps=1e-5, maxit=1000)
bifactorQ(L, Tmat=diag(ncol(L)),                 normalize=FALSE, eps=1e-5, maxit=1000)
