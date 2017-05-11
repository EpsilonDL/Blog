mazo<- data.frame(Valor=rep(1:13,4),Pinta=c(rep("C",13),rep("CN",13),
                                            rep("T",13),rep("D",13)))
ExtraerCartas<- function(){
    mano<- sample(52,5,replace = FALSE)
    if(sum(mazo$Valor[mano] %in% c(1,10:13))==5 & 
       length(unique(mazo$Pinta[mano]))==1) return("Escalera real")
    if(sum(diff(sort(mano))==1) & 
       length(unique(mazo$Pinta[mano]))==1) return("Escalera de color")
    if(sum(mazo$Valor[mano]==1)==4) return("Poker")
    aux<- sort(tapply(mazo$Valor[mano],mazo$Valor[mano],length))
    if(aux[1]==2 & aux[2]==3) return("Full")
    if(length(unique(mazo$Pinta[mano]))==1) return("Color")
    if(sum(diff(sort(mazo$Valor[mano]))==1)==4) return("Escalera")
    if(aux[length(aux)]==3) return("Trio")
    if(sum(aux==2)==2) return("Doble pareja")
    if(max(aux)==2) return("Pareja")
    return("Nada")
}

CalcularProbabilidad<- function(N){
    cantos<- data.frame(Cantos=c("Escalera real","Escalera de color",
                                 "Poker","Full","Color","Escalera","Trio",
                                 "Doble pareja","Pareja","Nada"),
                        Probabilidad=numeric(length = 10))
    for(i in 1:N){
        aux<- ExtraerCartas()
        cantos$Probabilidad[cantos$Cantos==aux]<- cantos$Probabilidad[cantos$Cantos==aux] + 1
    }
    cantos$Probabilidad<- cantos$Probabilidad/N
    return(cantos)
}
