l<-blblm(wt~mpg,mtcars, 10)
l<-parr.blblm(wt~mpg,mtcars,10,4)

df<-data.frame(Titanic)
l<-blbglm(Survived~Class+Sex, binomial, df,10)
l<-parr.blbglm(Survived~Class+Sex, binomial, df,10, 4)

coeff.blb(l)
se.blb(l)
predicts.blb(l, data.frame(wt = c(2.5, 3), mpg=c(20,30)), confidence=TRUE)
confints.blb(l, level=0.9)
