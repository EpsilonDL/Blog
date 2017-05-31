
Modelo<- function(datos, umbral){
    if(length(unique(datos))<3){
        Ncluster<- NA
    }
    else{
        kmedias<- kmeans(datos,3)
        Ncluster<- ConteoCluster(datos,kmedias,umbral)
    }
    return(Ncluster)
}

ConteoCluster<- function(datos, kmedias, umbral){
    distancias<- distancias(length(3))
    for(i in 1:(length(datos)-1)){
        for(j in i:length(datos)){
            distancias[kmedias$cluster[i]]<- distancias[kmedias$cluster[i]] +
                abs(datos[i]-datos[j])
        }
    }
    Ndatos<- tapply(kmedias$cluster,kmedias$cluster,length)
    distancias<- distancias/Ndatos
    
}

GeneradorDatos<- function(){
    Nmedias<- runif(3,min = -10, max = 10)
    Nsd<- runif(3,min = 0.5, max = 5)
    Ndatos<- ceiling(runif(3,min = -0.5,max = 3.2))
    datos<- numeric()
    for(i in 1:3){
        if(Ndatos[i]!= 0) 
            datos<- c(datos,rnorm(Ndatos[i],Nmedias[i],Nsd[i]))
    }
    datos<- sample(datos,sum(Ndatos))
    return(list(Datos=datos,Nprocesos=sum(Ndatos!=0)))
}
