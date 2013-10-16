{
  if( input$rotation=="svd" ) {
    V <- base::svd(R)$v
    FPM <- V[, 1:k] # FPM - Factor Pattern Matrix
    FPM <- cbind(FPM, matrix(numeric(0), p, p-k)) # appends empty columns to have p columns
    rownames(FPM) <- rownames(datasetInput())
    colnames(FPM) <- paste0("V", 1:p) #Andrey, should this be 'F' instead of 'V'?
    FPM # THE OUTPUT
    Phi <- diag(k)
    colnames(Phi) <- paste0("F", 1:k)
    rownames(Phi) <- paste0("F", 1:k)    
    Phi
  } 
  else if( input$rotation=="promax" ) { 
    A <- stats::factanal(factors = k, covmat=R, rotation="none", control=list(rotate=list(normalize=TRUE)))
#     FPM <- stats::promax(A$loadings, m=3)$loadings    
    Atemp <- GPromax(A$loadings, pow=3)
    FPM <- Atemp$Lh # FPM - Factor Pattern Matrix
    FPM <- cbind(FPM, matrix(numeric(0), p, p-k)) # appends empty columns to have p columns
    colnames(FPM) <- paste0("F", 1:p) # renames for better presentation in tables and graphs
    FPM # THE OUTPUT
    Phi <- Atemp$Phi
    if( is.null(Phi)) {Phi<-diag(k)} else{Phi}
    colnames(Phi) <- paste0("F", 1:k)
    rownames(Phi) <- paste0("F", 1:k)    
    Phi    
  } 
  else if( input$rotation=="none" ) { 
    A <- stats::factanal(factors = k, covmat=R, rotation="none", control=list(rotate=list(normalize=TRUE)))
    Atemp <- A
    FPM <- Atemp$loadings # FPM - Factor Pattern Matrix
    FPM <- cbind(FPM, matrix(numeric(0), p, p-k)) # appends empty columns to have p columns
    colnames(FPM) <- paste0("F", 1:p) # renames for better presentation in tables and graphs
    FPM  # THE OUTPUT
    Phi <- Atemp$Phi
    if( is.null(Phi)) {Phi<-diag(k)} else{Phi}
    colnames(Phi) <- paste0("F", 1:k)
    rownames(Phi) <- paste0("F", 1:k)    
    Phi   
  } 
  else if( input$rotation %in% c("cfT","cfQ") ) { 
    A <- stats::factanal(factors = k, covmat=R, rotation="none", control=list(rotate=list(normalize=TRUE)))
    L <- A$loadings
    Atemp <- eval(parse(text=
                        paste0(rotationInput(),"(L,Tmat=diag(ncol(L)),kappa=input$kappa,normalize=FALSE, eps=1e-5, maxit=1000)")))
    FPM <- Atemp$loadings # FPM - Factor Pattern Matrix
    FPM <- cbind(FPM,matrix(numeric(0), p, p-k)) # appends empty columns to have p columns
    colnames(FPM) <- paste0("F", 1:p) # renames for better presentation in tables and graphs
    FPM  # THE OUTPUT
    Phi <- Atemp$Phi
    if( is.null(Phi)) {Phi<-diag(k)} else{Phi}
    colnames(Phi) <- paste0("F", 1:k)
    rownames(Phi) <- paste0("F", 1:k)    
    Phi
  } 
  else if( input$rotation==rotationInput() ) { 
    A <- stats::factanal(factors = k, covmat=R, rotation="none", control=list(rotate=list(normalize=TRUE)))
    L <- A$loadings
    Atemp <- eval(parse(text=paste0(rotationInput(),"(L, Tmat=diag(ncol(L)), normalize=FALSE, eps=1e-5, maxit=1000)")))
    FPM <- Atemp$loadings # FPM - Factor Pattern Matrix
    FPM <- cbind(FPM, matrix(numeric(0), p, p-k)) # appends empty columns to have p columns
    colnames(FPM) <- paste0("F", 1:p) # renames for better presentation in tables and graphs
    FPM  # THE OUTPUT
    Phi <- Atemp$Phi
    if( is.null(Phi)) {Phi <- diag(k)} else{Phi}
    colnames(Phi) <- paste0("F", 1:k)
    rownames(Phi) <- paste0("F", 1:k)    
    Phi
  }
}
