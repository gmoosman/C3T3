advertising = c(117.913, 120.112, 125.828, 115.354, 177.090, 141.647, 137.892,   0.000,   0.000,   0.000,   0.000, 
                0.000,   0.000,   0.000,   0.000,   0.000,   0.000, 158.511, 109.385,  91.084,  79.253, 102.706, 
                78.494, 135.114, 114.549,  87.337, 107.829, 125.020,  82.956,  60.813,  83.149,   0.000,   0.000, 
                0.000,   0.000,   0.000,   0.000, 129.515, 105.486, 111.494, 107.099,   0.000,   0.000,   0.000, 
                0.000,   0.000,   0.000,   0.000,   0.000,   0.000,   0.000,   0.000)

adstock <-function(data_vector, decay, period, pool_vector=0){  
#create second version of data to apply decay
  data2<-data_vector
  l<-length(data_vector)
#if no pool apply zero to vector
if(length(pool_vector)==1)pool_vector<-rep(0,l)
#outer loop: extract data to decay from observation i
  for( i in 1:l){
    x<-data_vector[i]
#inner loop: apply decay onto following observations after i
    for(j in 1:min(period,l)){
      #constrain decay to same pool (if data is pooled)
      if( pool_vector[i]==pool_vector[min(i+j,l)]){data2[(i+j)]<- data2[(i+j)]+(x*(1-decay)^j)}
    }
  }
#reduce lenth of edited data to equal lenth of innitial data
data2<-data2[1:l]
  return(data2)
}

pool<-rep(1,52)
pool[26:52]<-2

lines( advertising,type="l")
lines( adstock(advertising,.8,5,pool),col="red")
plot( adstock(advertising,.8,5),type="l",col="blue")
setwd ("C:\\Users\\Gabriel.Moosman\\Desktop\\WIP\\R course\\C3T3")
getwd
