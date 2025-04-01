#Lecture 13, factor analysis example
library(psych)
library(factoextra)
library(FactoMineR)

mypath<-"D:\\Teaching\\Survmeth686\\Winter 2024\\Lectures\\13 Factor Analysis\\"

#The data are consumer ratings of variables as important or not in deciding whether to buy a type of beer
beer <- read.csv(paste (mypath,"beer.csv",sep=""))
dim(beer)
beer[1:4,]
summary(beer)

scaled.beer<-scale(beer)
plot(beer)


fit1 <- princomp(scaled.beer)
(eig.val <- get_eigenvalue(fit1))

summary(fit1) # print variance accounted for
loadings(fit1) # pc loadings
plot(fit1,type="lines") # scree plot
fviz_eig(fit1) #alternative scree plot
fit1$scores # the principal components
plot(fit1$scores)
biplot(fit1) #Variables are projected from the origin and show the influence on each component
#Left to right, and up and down in this two-dimensional case

#Scree plot
plot(fit1$sdev^2, 
     xlab = "Component number",
     ylab = "Component variance", 
     type = "l", 
     main = "Scree diagram")

fit2 <- principal(beer, nfactors=2, rotate="varimax")
summary(fit2) # print variance accounted for
loadings(fit2) # loadings

#Testing the number of factors
fit3 <- fa(beer, nfactors=2, rotate="oblimin",fm="ml") # varimax is an orthogonal rotation
summary(fit3) # print variance accounted for
loadings(fit3) # factor loadings

fa.diagram(fit3,digits=2,main="Factor Analysis: Beer Attributes")

colnames(fit3$loadings) <- c("Quality", "Value")
fa.diagram(fit3,digits=2,errors=TRUE)