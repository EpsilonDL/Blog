
Modelo<- function(datos, umbral){
    if(length(unique(datos))<4){
        Ncluster<- NA
    }
    else{
        kmedias<- kmeans(datos,3)
        Ncluster<- ConteoCluster(datos,kmedias,umbral)
    }
    return(Ncluster)
}

ConteoCluster<- function(datos, kmedias, umbral){
    distancias<- numeric(length = 3)
    for(i in 1:(length(datos)-1)){
        for(j in i:length(datos)){
            distancias[kmedias$cluster[i]]<- distancias[kmedias$cluster[i]] +
                abs(datos[i]-datos[j])
        }
    }
    Ndatos<- tapply(kmedias$cluster,kmedias$cluster,length)
    return(3 - length(which(distancias/Ndatos < umbral)))
}

GeneradorDatos<- function(Ndatos){
    Nmedias<- runif(3,min = -10, max = 10)
    Nsd<- runif(3,min = 0.5, max = 5)
    datos<- numeric()
    for(i in 1:3){
        if(Ndatos[i]!= 0) 
            datos<- c(datos,rnorm(Ndatos[i],Nmedias[i],Nsd[i]))
    }
    datos<- sample(datos,sum(Ndatos))
    return(list(Datos=datos,Nprocesos=sum(Ndatos!=0)))
}

aux.sim<- function(Nsim,umbral,Ndatos,Matrix){
    if(Matrix){
        salida<- matrix(rep(0,12),nrow = 4,ncol = 3)
        row.names(salida)<- c("1","2","3","NA"); colnames(salida)<- c("1","2","3")
    }
    Prob<- 0
    for(i in 1:Nsim){
        if(is.null(Ndatos)) N<- ceiling(runif(3,min = -0.5,max = 3.2))
        else{
            N<- numeric(length = 3)
            for(i in 1:Ndatos){
                aux<- ceiling(runif(1, max = 3))
                N[aux]<- N[aux] + 1
            }
        }
        datos<- GeneradorDatos(N)
        Ncluster<- Modelo(datos$Datos,umbral)
        if(is.na(Ncluster)){
            if(Matrix) salida[4,datos$Nprocesos]<- salida[4,datos$Nprocesos] + 1
            next
        }
        if(Matrix) 
            salida[Ncluster,datos$Nprocesos]<- salida[Ncluster,datos$Nprocesos] + 1
        if(Ncluster == datos$Nprocesos) Prob<- Prob + 1
    }
    if(Matrix) return(list(Efectividad=Prob/Nsim,Matrix=salida/Nsim))
    return(Prob/Nsim)
}

simulacion<- function(Nsim=10000,umbral=1,ProbCond=FALSE,Matrix=TRUE){
    if(ProbCond){
        salida<- list()
        for(i in 1:10){
            salida[[i]]<- aux.sim(Nsim/10,umbral,i,Matrix)
        }
        names(salida)<- paste("Numdatos",1:10,sep = "")
        return(salida)
    }
    return(aux.sim(Nsim,umbral,NULL,Matrix))
}