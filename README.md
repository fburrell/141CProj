# 141CProj
```
#below are some helpful examples about using the package 

library(blblm)
#non parallel lm 
l<-blblm(wt~mpg,mtcars, 1000)
coeff.blb(l)
se.blb(l)
predicts.blb(l, data.frame(wt = c(2.5, 3), mpg=c(20,30)), confidence=TRUE)
confints.blb(l, level=0.9)


#parallel lm
l<-parr.blblm(wt~mpg,mtcars,1000,4)
coeff.blb(l)
se.blb(l)
predicts.blb(l, data.frame(wt = c(2.5, 3), mpg=c(20,30)), confidence=TRUE)
confints.blb(l, level=0.9)

#non parallel glm
df<-data.frame(Titanic)
l<-blbglm(Survived~Class+Sex, binomial, df,1000)
coeff.blb(l)
se.blb(l)
confints.blb(l, level=0.9)


#parallel glm
l<-parr.blbglm(Survived~Class+Sex, binomial, df,1000, 4)
coeff.blb(l)
se.blb(l)
confints.blb(l, level=0.9)
```
