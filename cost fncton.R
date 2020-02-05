plot(iris$Petal.Length, iris$Petal.Width, pch=21, bg=c("red","green","blue")
     [unclass(iris$Species)],main="Iris Data")



df1=iris
df1

iris[,1:4]


n=150
d1=0
loss=c(0,0,0,0,0)
k_values=c(1,2,3,4,5)
for (k in k_values){
  d11=0
  iriscluster=kmeans(iris[,1:4], centers=k)
  for (j in 1:k){
    c=iriscluster$centers[j,]
    for (i in 1:n){
      if (iriscluster$cluster[i]==j){
        d1= (dist(rbind(c,iris[,1:4][i,])))^2
        d11= d11+d1
      }
    }
    
  loss[k]= d11/n  
    
  }
}

plot(k_values,loss,type="l")

loss

