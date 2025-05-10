#For Shapiro-Wilk test and SEM
dir.create("./SEM_result", showWarnings = TRUE, recursive = FALSE, mode = "0777")

library(MVN)
input <- read.csv("FCP_SEM.csv")
mvn(input)

mvn_data=mvn(input,univariateTest="SW")

sink('./SEM_result/mvn_data.txt', append = TRUE)
print (mvn_data)
sink()


#For SEM

createModel10 <- function() {
  +     # variable ∼ dependent variable
    +     # + dependent variable
    return ("
    Comp ~ Lactobacillus
    AIB ~ Lactobacillus + Comp
    AIB  ~ Butyrate # please check the other components
       ")
}

model.l10 <- createModel10()

library(lavaan)

# Select robust estimation (estimator = "MLR") based on the statistical result (p<0.05) using Shapiro-Wilk test

res.l10 <- lavaan(model.l10, data = input, estimator = "MLR", auto.var = TRUE)
summary(res.l10, fit.measure=TRUE,　standardized = TRUE)
fitMeasures(res.l10)

p0=model.l10
sink('./SEM_result/lavaanstaticsmodel.l10.txt', append = TRUE)
print (p0)
sink()

p1=summary(res.l10, fit.measure=TRUE,　standardized = TRUE)
sink('./SEM_result/lavaanstaticssummary.txt', append = TRUE)
print (p1)
sink()

p2=fitMeasures(res.l10)
sink('./SEM_result/lavaanstaticsfitMeasures.txt', append = TRUE)
print (p2)
sink()

#optinal evaluation:|SR|<2.58
sr <- residuals(res.l10, type = "standardized")
sink('./SEM_result/residuals.txt', append = TRUE)
print (sr)
sink()


# bootstrap based on ML

res.l12 <- lavaan(model.l10, data = input, auto.var = TRUE,se="bootstrap",bootstrap=1000)
P4=summary(res.l12, fit.measure=TRUE,　standardized = TRUE)
fitMeasures(res.l12)

sink('./SEM_result/lavaanstaticsmodel.lbootres12.txt', append = TRUE)
print (P4)
sink()

#Visualize the SEM
library(OpenMx)
library(psych)
library(semPlot)

semPaths(res.l10, what="stand", layout="tree", style="lisrel",
         shapeMan="rectangle", shapeLat="ellipse",
         sizeMan=6, residScale=5, posCol="darkgreen",
         negCol="violet", fade=FALSE, edge.label.cex=0.8) #Type1

semPaths(res.l10, what="stand", layout="tree", style="lisrel",
         shapeMan="rectangle", shapeLat="ellipse",
         sizeMan=8, residScale=5, posCol="darkgreen",
         negCol="violet", fade=FALSE, edge.label.cex=1.0) #Type2

semPaths(res.l10, what="stand", layout="tree", style="lisrel",
         shapeMan="rectangle", shapeLat="ellipse",
         sizeMan=10, residScale=5, posCol="darkgreen",
         negCol="violet", fade=FALSE, edge.label.cex=1.0) #gType3
