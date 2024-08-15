v1<-sample(1:100, 70)
v2<-sample(1:100, 70)

common<-intersect(v1,v2)

v1.genome<-rep(0,100)
v2.genome<-rep(0,100)

fitness.common<-rnorm(length(common))

v1.genome[v1]<-rnorm(length(v1))
v2.genome[v2]<-rnorm(length(v2))

v1.genome[common]<-v2.genome[common]<-rnorm(length(common))

v1.genome[c(60:80)]<-v1.genome[c(60:80)]+0.4
v2.genome[c(60:80)]<-v2.genome[c(60:80)]+0.6
v1.genome[common[which(common>=60 & common <= 80)]]<-v2.genome[common[which(common>=60 & common <= 80)]]<-rnorm(length(common[which(common>=60 & common <= 80)]))+0.5


matrix2v<-rbind(v1.genome, v2.genome)
matrix.world<- matrix2v[c(rep(1, 80), rep(2, 20)),]

matrix.world <- matrix.world[,-which(apply(matrix.world,2, sum)==0)]

noise<-list()
fitness.gene<-list()
for (i in 1:100) {
  noise<- c(noise,list(apply(matrix.world, 2, function(x) as.numeric(table(x)[which(table(x)==max(table(x)))]/sum(table(x))) ))  )
  index.rec <- which(runif(nrow(matrix.world)) < 0.01 )
  
  #Recombination
  if(length(index.rec)>0){
   match.rec<-  sample(c(1:nrow(matrix.world)), length(index.rec))
   
   for (j in 1:length(index.rec)) {
    breakpoint<-sample(c(2:(ncol(matrix.world)-1)),1)
    matrix.world[index.rec[j], c(1:breakpoint)] <- matrix.world[match.rec[j],c(1:breakpoint)]
    matrix.world[match.rec[j], c((breakpoint+1):ncol(matrix.world))] <- matrix.world[index.rec[j], c((breakpoint+1):ncol(matrix.world))]  
   }
   
   fitness<-round(apply(matrix.world, 1, sum))
   
   #Survival
   superworld<-matrix.world[rep(c(1:nrow(matrix.world)), fitness),]
   matrix.world<-superworld[sample(c(1:nrow(superworld)), nrow(matrix.world)),]
   fitness.gene<-c(fitness.gene, list(round(apply(matrix.world, 2, sum))/nrow(matrix.world)))
   
   
  }
  
}

plot(noise[[2]])

plot(fitness.gene[[5]])
