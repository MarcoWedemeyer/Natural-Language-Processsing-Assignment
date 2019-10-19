### NLP Project Code

rm(list = ls(all = TRUE))

library(mice)
library(MASS)
library(MLmetrics)
library(ggplot2)
source("studentFunctions.R")
source("studentSubroutines.R")
set.seed(235711)


###--------------------------------------------------------------------------###

dat1 <- read.csv("../data/Complete_df.csv")
dat2 <- read.csv("../data/Statcheck results.csv")
dat3 <- read.csv("../data/combined_reduced.csv")


##-Analysis run on all 1104 Articles------------------------------------------##
#--Dataframe showing levels of significance for the estimates per index--------#

out <- lm(dat1[,3] ~ dat1$error_binary)
sOut <- summary(out)
beta <- round(sOut$coefficients[2, "Estimate"],4)
pvalue <- round(sOut$coefficients[2, "Pr(>|t|)"],4)
if (pvalue <= 0.05) {
  sig <- "Yes"
} else {
  sig <- "No"
}
titles <- c("Index","Estimate","P-value", "Significance (alpha = 0.05)")
firstrow <- c(colnames(dat1)[3], beta, pvalue, sig)

df <- data.frame(titles, firstrow)

for (i in 4:(ncol(dat1)-2)){
  out <- lm(dat1[,i] ~ dat1$error_binary)
  sOut <- summary(out)
  beta <- round(sOut$coefficients[2, "Estimate"],4)
  pvalue <- round(sOut$coefficients[2, "Pr(>|t|)"],4)
  if (pvalue <= 0.05) {
    sig <- "Yes"
  } else {
    sig <- "No"
  }
  x <- c(colnames(dat1)[i], beta, pvalue, sig)
  df <- cbind(df, x)
}


#--Visualization of KDEs per index---------------------------------------------#

dev.off()
par("mar" = c(2,2,2,1))
par(mfrow=c(3,3))

for (i in 3:(ncol(dat1)-2)){
  d1 = density(dat1[dat1$error_binary == 1,i])
  d2 = density(dat1[dat1$error_binary == 0,i])
  f = max(d1$y, d2$y)
  plot(density(dat1[dat1$error_binary == 1,i]), 
       col='red', 
       main=colnames(dat1)[i],
       ylim=c(0,f))
  lines(density(dat1[dat1$error_binary == 0,i]), 
        col='black')
}




##-Analysis run only on the 129 Articles where statcheck identified p values--##
#--Dataframe showing levels of significance for the estimates per index--------#

out <- lm(dat3[,5] ~ dat3$error_binary)
sOut <- summary(out)
beta <- round(sOut$coefficients[2, "Estimate"],4)
pvalue <- round(sOut$coefficients[2, "Pr(>|t|)"],4)
if (pvalue <= 0.05) {
  sig <- "Yes"
} else {
  sig <- "No"
}
titles <- c("Index","Estimate","P-value", "Significance (alpha = 0.05)")
firstrow <- c(colnames(dat3)[5], beta, pvalue, sig)

df2 <- data.frame(titles, firstrow)

for (i in 6:(ncol(dat3))){
  out <- lm(dat3[,i] ~ dat3$error_binary)
  sOut <- summary(out)
  beta <- round(sOut$coefficients[2, "Estimate"],4)
  pvalue <- round(sOut$coefficients[2, "Pr(>|t|)"],4)
  if (pvalue <= 0.05) {
    sig <- "Yes"
  } else {
    sig <- "No"
  }
  x <- c(colnames(dat3)[i], beta, pvalue, sig)
  df2 <- cbind(df2, x)
}

#--Visualization of KDEs per index---------------------------------------------#

dev.off()
par("mar" = c(2,2,2,1))
par(mfrow=c(3,3))

for (i in 5:(ncol(dat3))){
  d1 = density(dat3[dat3$error_binary == 1,i])
  d2 = density(dat3[dat3$error_binary == 0,i])
  f = max(d1$y, d2$y)
  plot(density(dat3[dat3$error_binary == 1,i]), 
       col='red', 
       main=colnames(dat3)[i], 
       ylim=c(0,f))
  lines(density(dat3[dat3$error_binary == 0,i]), 
        col='black')
}

###--------------------------------------------------------------------------###