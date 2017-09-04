give<- function(player, players){
    aux<- 1:players
    aux<- aux[-player]
    return(sample(aux,1))
}

game<- function(players,initial.cap,rounds){
    capital<- rep(initial.cap,players)
    cap.history<- matrix(0,nrow = players,ncol = rounds)
    for(i in 1:rounds){
        who.play<- which(capital > 0)
        capital[who.play]<- capital[who.play] - 1
        for(j in who.play){
            togive<- give(j,players)
            capital[togive]<- capital[togive] + 1
        }
        cap.history[,i]<- capital
    }
    return(cap.history)
}
cap<- data.frame(cap)
orden<- order(cap$v1000)
plot_ly(data = cap, x = 1:50, y = ~sort(v1000), type = "scatter", mode = "line", name = "Ganancia Jugadores") %>%
    add_trace(x = rep(which(orden==30),2), y = c(0,200), name = "Jugador fijo") %>%
    layout(title = "Distribución del dinero - Ronda 1000", 
           xaxis = list(title = "Jugadores"),
           yaxis = list(title = "Dolares"))
orden<- order(cap$v2000)
plot_ly(data = cap, x = 1:50, y = ~sort(v2000), type = "scatter", mode = "line", name = "Ganancia Jugadores") %>%
    add_trace(x = rep(which(orden==30),2), y = c(0,200), name = "Jugador fijo") %>%
    layout(title = "Distribución del dinero - Ronda 2000", 
           xaxis = list(title = "Jugadores"),
           yaxis = list(title = "Dolares"))
orden<- order(cap$v3000)
plot_ly(data = cap, x = 1:50, y = ~sort(v3000), type = "scatter", mode = "line", name = "Ganancia Jugadores") %>%
    add_trace(x = rep(which(orden==30),2), y = c(0,200), name = "Jugador fijo") %>%
    layout(title = "Distribución del dinero - Ronda 3000", 
           xaxis = list(title = "Jugadores"),
           yaxis = list(title = "Dolares"))
orden<- order(cap$v4000)
plot_ly(data = cap, x = 1:50, y = ~sort(v4000), type = "scatter", mode = "line", name = "Ganancia Jugadores") %>%
    add_trace(x = rep(which(orden==30),2), y = c(0,200), name = "Jugador fijo") %>%
    layout(title = "Distribución del dinero - Ronda 4000", 
           xaxis = list(title = "Jugadores"),
           yaxis = list(title = "Dolares"))
orden<- order(cap$v5000)
plot_ly(data = cap, x = 1:50, y = ~sort(v5000), type = "scatter", mode = "line", name = "Ganancia Jugadores") %>%
    add_trace(x = rep(which(orden==30),2), y = c(0,200), name = "Jugador fijo") %>%
    layout(title = "Distribución del dinero - Ronda 5000", 
           xaxis = list(title = "Jugadores"),
           yaxis = list(title = "Dolares"))