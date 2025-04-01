library(MVA)#applied multivariate analysis

bc <- c(
  +  0.290,           
  +  0.202,  0.415,       
  + -0.055,  0.285,  0.419,       
  + -0.105, -0.376, -0.521, -0.877,      
  + -0.252, -0.349, -0.441, -0.076,  0.206,
  + -0.229, -0.164, -0.145,  0.023,  0.034,  0.192,
  +  0.058, -0.129, -0.076, -0.131,  0.151,  0.077,  0.423)

blood_sd <- c(rblood = 0.371, plate = 41.253,  wblood = 1.935,
                               neut = 0.077, lymph = 0.071, bilir = 4.037,
                               sodium = 2.732, potass = 0.297)

blood_corr <- diag(length(blood_sd)) / 2
blood_corr[upper.tri(blood_corr)] <- bc     
blood_corr <- blood_corr + t(blood_corr)
blood_cov <- blood_corr * outer(blood_sd, blood_sd, "*")


#Using covariance matrix
blood_pcacov <- princomp(covmat = blood_cov)
summary(blood_pcacov, loadings = TRUE)

#Using correlation matrix
blood_pcacor <- princomp(covmat = blood_corr)
summary(blood_pcacor, loadings = TRUE)

#Scree plot
 plot(blood_pcacor$sdev^2, 
      xlab = "Component number",
      ylab = "Component variance", 
      type = "l", 
      main = "Scree diagram")

 plot(blood_pcacor)
 plot(blood_pcacor,type="lines")