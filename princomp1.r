#Example 1 in Lecture 13
library(car)
library(factoextra)
library(FactoMineR)

mypath<-"D:\\Teaching\\Survmeth686\\Winter 2024\\Lectures\\13 Factor Analysis\\"

temperature <- read.csv(paste (mypath,"temperature.csv",sep=""))
dim(temperature)
temperature[1:4,]
summary(temperature)
attach(temperature)
table(Cityid,useNA = "ifany")
table(City,useNA = "ifany")

#Extracting temperature data
temps<-temperature[,3:4]
dim(temps)
temps[1:4,]


scaled.temps<-scale(temps)
plot(temps$July,temps$January)


fit <- princomp(scaled.temps)
(eig.val <- get_eigenvalue(fit))

summary(fit) # print variance accounted for
loadings(fit) # pc loadings
plot(fit,type="lines") # scree plot
fviz_eig(fit) #alternative scree plot
fit$scores # the principal components
plot(fit$scores)
biplot(fit) #Variables are projected from the origin and show the influence on each component
#Left to right, and up and down in this two-dimensional case

#confidence ellipses
library(devtools)
devtools::install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot(fit, obs.scale = 1, var.scale = 1,  
         groups=1,
         ellipse = TRUE, 
         labels=temperature$Cityid,
         ellipse.prob = 0.95)
