
#Install Package
library(ggplot2)


Data <- read.csv("~/Desktop/Taiwan/3rd Semester/GLM/Canada_Fuel_Consumption_Rating.csv")
#removing "NA" data
Data <- na.omit(Data)

#Train_Test_Dataset
i <- nrow(Data) - 20
Data_Train <- Data[1: i, ]     #Extract first i obs
Data_Test <- tail(Data, n = 20)   #Extract last 20 obs
rm(i)

#Extract Variable
Engine <- Data_Train$ENGINE_SIZE
Cylinders <- Data_Train$CYLINDERS
Fuel <- Data_Train$FUEL_TYPE
City <- Data_Train$CITY
HWY  <- Data_Train$HWY
COMB <- Data_Train$COMB
COMB_mpg   <- Data_Train$COMB_mpg
CO2_Emission   <- Data_Train$CO2_EMISSIONS


#Pre-processing 
   #Outliers
func <- function(x){
  quantiles <- quantile( x, c(.05, .95 ) )
  b <- boxplot(x, plot = FALSE)
  x[ x %in% b$out]  <- quantiles[2]
  x
}

par(mfrow = c(3,3))  
boxplot(Engine, main = "Engine", col = "steelblue3")
boxplot(City, main = "C.City", col = "steelblue3")
boxplot(CO2_Emission, main = "CO2_Emission", col = "steelblue3")
boxplot(COMB,main = "C.COMB", col = "steelblue3")
boxplot(Cylinders, main = "Cylinders", col = "steelblue3")
boxplot(COMB_mpg, main = "C.COMB_mpg", col = "steelblue3")
boxplot(HWY, main = "C.HWY", col = "steelblue3")

City <- func(City)
CO2_Emission <- func(CO2_Emission)
COMB <- func(COMB)
Cylinders <- func(Cylinders)
COMB_mpg <- func(COMB_mpg)
HWY <- func(HWY)


par(mfrow = c(3,3))  
boxplot(Engine, main = "Engine", col = "steelblue3")
boxplot(City, main = "C.City", col = "steelblue3")
boxplot(CO2_Emission, main = "CO2_Emission", col = "steelblue3")
boxplot(COMB, main = "C.COMB", col = "steelblue3")
boxplot(Cylinders, main = "Cylinders", col = "steelblue3")
boxplot(COMB_mpg, main = "C.COMB_mpg", col = "steelblue3")
boxplot(HWY, main = "C.HWY", col = "steelblue3")


#Regression non-orthogonal polynomial
non <- lm( CO2_Emission ~ Engine + I(Engine^2) + I(Engine^3) + I(Engine^4), data=Data_Train)
 summary(non, correlation=TRUE)$correlation

#Regression orthogonal polynomial
Op.m1 <- lm( CO2_Emission ~ poly(Engine , 1), data=Data_Train) # Linear
Op.m2 <- lm( CO2_Emission ~ poly(Engine, degree = 2), data=Data_Train) # Quadratic

Op.m3 <- lm( CO2_Emission ~ poly(Engine, 3), data=Data_Train) # Cubic
Op.m4 <- lm( CO2_Emission ~ poly(Engine, 4), data=Data_Train) # Quartic



#Estimated Result
printCoefmat(coef(summary(Op.m1)))
printCoefmat(coef(summary(Op.m2)))
printCoefmat(coef(summary(Op.m3)))
printCoefmat(coef(summary(Op.m4)))

#Goodness of Fit
sse <- sum( (Data_Train$CO2_EMISSIONS - fitted(Op.m2))^2)
sse

#find ssr
ssr <- sum((fitted(Op.m2) - mean(Data_Train$CO2_EMISSIONS))^2)
ssr

# find SST
sst <- sse + ssr
sst


#Diagnostic
par(mfrow = c(1,2))
plot( rstandard(Op.m2) ~ fitted(Op.m2), las=1,
      ylab="Standardized residuals", xlab="Fitted values", col = "palevioletred3" )


plot( rstandard(Op.m2) ~ Data_Train$CO2_EMISSIONS, las=1,
      ylab="Standardized residuals", xlab="CO2_Emission", col = "steelblue3" ) 

par(mfrow = c(1,1))
qqnorm( rstandard( Op.m2 ), las=1, col = "skyblue4" ); qqline( rstandard( Op.m2 ) )

plot( cooks.distance(Op.m2), type="h", las=1, col = "skyblue4")



#Accurancy

h = c(Data_Test$ENGINE_SIZE)
Prediction <- predict(Op.m2, newdata = list(Engine = h)); predict


#Export Excel File
k <- matrix(cbind(Prediction, Data_Test$CO2_EMISSIONS), ncol = 2 )
write.table(k, file = "Prediction.f.xls",  sep='\t', dec = ",") 

#Graphical
#Prediction vs Actual Data
dev.off()
matplot(k, type = c("b"),pch=1,col = 2:4, ylab="CO2 Emission", xlab="Index",) #plot
legend("topleft", legend = c("Orthogonal Polynomial ","Raw Data"), col=2:4, pch=1)


#Fitted Value vs Actual Value

n <- matrix(cbind(fitted(Op.m2),Data_Train$CO2_EMISSIONS ), ncol = 2)
matplot(n, type = c("b"),pch=1,col = 6:7) #plot
legend("topleft", legend = c("Fitted Value ","Actual Value"), col=6:7, pch=1)


 
