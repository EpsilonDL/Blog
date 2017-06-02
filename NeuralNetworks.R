

SimulatePoints<- function(N1=100,N2=100,center1=c(-1,-1),center2=c(1,1),
                          sd1=c(1,1),sd2=c(1,1),Train=0.75){
    aux<- round(Train*(N1+N2))
    Points<- data.frame(X=c(rnorm(N1,center1[1],sd1[1]),rnorm(N2,center2[1],sd2[1])),
                        Y=c(rnorm(N1,center1[2],sd1[2]),rnorm(N2,center2[2],sd2[2])),
                        Class=c(rep(-1,N1),rep(1,N2)))
    Points<- Points[sample(1:(N1+N2),(N1+N2)),]
    Points$Set<- c(rep("Train",aux),rep("Test",(N1+N2)-aux))
    return(Points)
}

GaussianKernel<- function(points){
    points<- exp(-(norm(points$X,points$Y)^2)/(2*sd()))
}

SVM<- function(points,Lambda=0.1){
    # Queremos minimizar la norma de W sujeto a Class(W*X - b) >= 1 
    W<- rnorm(2);b<- rnorm(1)
    
}



#--------------- Graphics -----------------

points<- SimulatePoints()
plot(points[,-c(3,4)],col = "white", xlab = "X", ylab = "Y")
grid()
points(points$X[points$Class==-1],points$Y[points$Class==-1], pch = 19, col = "blue")
points(points$X[points$Class==1],points$Y[points$Class==1], pch = 19, col = "green")
points(points$X[points$Set=="Train"],points$Y[points$Set=="Train"], col = "red")

