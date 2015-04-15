library(ggplot2)

conTable <- function(pointCount) {
    pc <- function(index) {
        floor(pointCount*randoms[index]/randomTotal)
    }
    
    randoms = runif(4,min=0,max=1)
    randomTotal = sum(randoms)
    a = pc(1)
    b = pc(2)
    c = pc(3)
    d = pointCount-(a+b+c)
    c(a,b,c,d)
}

calcError <- function(conVector) {
    (conVector[2]+conVector[3]+2*conVector[4]) / (2*sum(conVector))
}

calcKappa <- function(conVector) {
    a = conVector[1]
    b = conVector[2]
    c = conVector[3]
    d = conVector[4]
    (2*(a*d - b*c)) / ((a+b)*(b+d) + (a+c)*(c+d))
}

drawFig1 <- function(allConVecs,fileName="fig1.png") {
    kappa = apply(allConVecs,1,calcKappa)
    error = apply(allConVecs,1,calcError)
    data = data.frame(kappa,error)
    g = ggplot(data,aes(kappa,error))
    
    png(fileName,units="px")
    print(g+geom_point(color="green",size=1,alpha=1/2))
    dev.off()
}


prepareFig2 <- function(ensembleCount=1000,pointCount=500,fileName="fig2.png") {
    conVecs = t(replicate(ensembleCount,conTableFor3(pointCount)))
    drawFig2(conVecs,fileName)
}

conTableFor3 <- function(pointCount) {
    pc <- function(x) {
        floor(pointCount*x/randomTotal)
    }
    
    randoms = runif(8,min=0,max=1)
    randomTotal = sum(randoms)
    
    conTable = sapply(randoms,pc)
    conTable[8] = pointCount - sum(conTable[1:7])
    conTable
}

drawFig2 <-  function(conVecs,fileName="fig2.png") {
    k12 = apply(t(apply(conVecs,1,conVector12)),1,calcKappa)
    e12 = apply(t(apply(conVecs,1,conVector12)),1,calcError)
    k13 = apply(t(apply(conVecs,1,conVector13)),1,calcKappa)
    e13 = apply(t(apply(conVecs,1,conVector13)),1,calcError)
    k23 = apply(t(apply(conVecs,1,conVector23)),1,calcKappa)
    e23 = apply(t(apply(conVecs,1,conVector23)),1,calcError)
    
    errorMid = (e12+e13+e23)/3
    kappaMid = (k12+k13+k23)/3
    accuracy = apply(conVecs,1,majorAcc)
    
    data2 = data.frame(k12,k13,k23,e12,e13,e23,accuracy,errorMid,kappaMid)
    
    g = ggplot(data2)+ylab("Error")+xlab("Kappa")
    
    gpoints = g+ geom_point(aes(k12,e12),colour="green")+ geom_point(aes(k13,e13),colour="green")+geom_point(aes(k23,e23),colour="green")
    gpointslines = gpoints+geom_segment(aes(xend=k13,yend=e13,x=k12,y=e12),colour="green")+geom_segment(aes(xend=k23,yend=e23,x=k12,y=e12),colour="green")+geom_segment(aes(xend=k23,yend=e23,x=k13,y=e13),colour="green")
    finalg = gpointslines+geom_point(aes(kappaMid,errorMid,size=accuracy))
    
    png(fileName,units="px")
    print(finalg)
    dev.off()
}

# 000,001,010,011,100,101,110,111
conVector12 <- function(conVec) {
    c(conVec[8]+conVec[7],conVec[6]+conVec[5],conVec[4]+conVec[3],conVec[2]+conVec[1])
}

conVector13 <- function(conVec) {
    c(conVec[8]+conVec[6],conVec[7]+conVec[5],conVec[4]+conVec[2],conVec[3]+conVec[1])
}

conVector23 <- function(conVec) {
    c(conVec[8]+conVec[4],conVec[7]+conVec[3],conVec[6]+conVec[2],conVec[5]+conVec[1])
}

majorAcc <- function(conVec) {
    (sum(conVec[6:8])+conVec[4])/sum(conVec)
}