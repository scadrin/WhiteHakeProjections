# white hake recruitment rate (r/ssb) change-point analysis
# Steve Cadrin June 2023
library("changepoint")
#FALL SURVEY
#read data from csv, convert to numeric and specify year range
fall <- read.csv("C:/Users/scadrin/Desktop/all_work/white_hake_RPS_fall_index.csv")
fall.num <- as.numeric(unlist(fall))

#detection of single change-point
fall.amoc <- cpt.mean(fall.num, method = "AMOC")
plot(fall.amoc, type = "l", cpt.col = "blue", xlab = "Year", ylab = "Fall Index (Ln r/ssb)", cpt.width = 4)
legend("topright", legend = "AMOC Mean(s)", col = "blue", lwd = 2)
cpts(fall.amoc)

#Two methods for detection of multiple change-points
fall.pelt <- cpt.mean(fall.num, method = "PELT")
plot(fall.pelt, type = "l", cpt.col = "red", xlab = "Year", ylab = "Fall Index (Ln r/ssb)", cpt.width = 4)
legend("topright", legend = "PELT Mean(s)", col = "red", lwd = 2)
cpts(fall.pelt)

fall.binseg <- cpt.mean(fall.num, method = "BinSeg")
plot(fall.binseg, type = "l", cpt.col = "purple", xlab = "Year", ylab = "Fall Index (Ln r/ssb)", cpt.width = 4)
legend("topright", legend = "BinSeg Mean(s)", col = "purple", lwd = 2)
cpts(fall.binseg)

#number of change-points, means, likelihood, , by method
m.pm <- cpt.mean(fall.num, penalty = "Manual", pen.value = "1.5 * log(n)", method = "PELT")
plot(m.pm, type = "l", cpt.col = "blue", xlab = "Year", ylab = "Fall Index (Ln r/ssb)", cpt.width = 4)
cpts(m.pm)

m.bsm <- cpt.mean(fall.num, "Manual", pen.value = "1.5 * log(n)", method = "BinSeg")
cpts(m.bsm)

fall.point <- cpt.mean(fall.num, method = "PELT")
plot(fall.point, pch = 20, col = "grey", cpt.col = "black", type = "p", xlab = "Year", ylab = "Fall Index (Ln r/ssb)")
cpts(fall.point)
coef(fall.point)

#SPRING SURVEY
#read data from csv, convert to numeric and specify year range
spring <- read.csv("C:/Users/scadrin/Desktop/all_work/white_hake_RPS_spring_index.csv")
spring.num <- as.numeric(unlist(spring))

#detection of single change-point
spring.amoc <- cpt.mean(spring.num, method = "AMOC")
plot(spring.amoc, type = "l", cpt.col = "blue", xlab = "Year", ylab = "Spring Index (Ln r/ssb)", cpt.width = 4)
legend("topright", legend = "AMOC Mean(s)", col = "blue", lwd = 2)
cpts(spring.amoc)

#Two methods for detection of multiple change-points
spring.pelt <- cpt.mean(spring.num, method = "PELT")
plot(spring.pelt, type = "l", cpt.col = "red", xlab = "Year", ylab = "Spring Index (Ln r/ssb)", cpt.width = 4)
legend("topright", legend = "PELT Mean(s)", col = "red", lwd = 2)
cpts(spring.pelt)

spring.binseg <- cpt.mean(spring.num, method = "BinSeg")
plot(spring.binseg, type = "l", cpt.col = "purple", xlab = "Year", ylab = "Spring Index (Ln r/ssb)", cpt.width = 4)
legend("topright", legend = "BinSeg Mean(s)", col = "purple", lwd = 2)
cpts(spring.binseg)

#number of change-points, means, likelihood, , by method
m.pm <- cpt.mean(spring.num, penalty = "Manual", pen.value = "1.5 * log(n)", method = "PELT")
plot(m.pm, type = "l", cpt.col = "blue", xlab = "Year", ylab = "Spring Index (Ln r/ssb)", cpt.width = 4)
cpts(m.pm)

m.bsm <- cpt.mean(spring.num, "Manual", pen.value = "1.5 * log(n)", method = "BinSeg")
cpts(m.bsm)

spring.point <- cpt.mean(spring.num, method = "PELT")
plot(spring.point, pch = 20, col = "grey", cpt.col = "black", type = "p", xlab = "Year", ylab = "Spring Index (Ln r/ssb)")
cpts(spring.point)
coef(spring.point)

#SHRIMP SURVEY
#read data from csv, convert to numeric and specify year range
shrimp <- read.csv("C:/Users/scadrin/Desktop/all_work/white_hake_RPS_shrimp_index.csv")
shrimp.num <- as.numeric(unlist(shrimp))

#detection of single change-point
shrimp.amoc <- cpt.mean(shrimp.num, method = "AMOC")
plot(shrimp.amoc, type = "l", cpt.col = "blue", xlab = "Year", ylab = "Shrimp Index (Ln r/ssb)", cpt.width = 4)
legend("topright", legend = "AMOC Mean(s)", col = "blue", lwd = 2)
cpts(shrimp.amoc)

#Two methods for detection of multiple change-points
shrimp.pelt <- cpt.mean(shrimp.num, method = "PELT")
plot(shrimp.pelt, type = "l", cpt.col = "red", xlab = "Year", ylab = "Shrimp Index (Ln r/ssb)", cpt.width = 4)
legend("topright", legend = "PELT Mean(s)", col = "red", lwd = 2)
cpts(shrimp.pelt)

shrimp.binseg <- cpt.mean(shrimp.num, method = "BinSeg")
plot(shrimp.binseg, type = "l", cpt.col = "purple", xlab = "Year", ylab = "Shrimp Index (Ln r/ssb)", cpt.width = 4)
legend("topright", legend = "BinSeg Mean(s)", col = "purple", lwd = 2)
cpts(shrimp.binseg)

#COMBINED SURVEY INDEX
#read data from csv, convert to numeric and specify year range
comb <- read.csv("C:/Users/scadrin/Desktop/all_work/white_hake_RPS_combined_index.csv")
comb.num <- as.numeric(unlist(comb))

#detection of single change-point
comb.amoc <- cpt.mean(comb.num, method = "AMOC")
plot(comb.amoc, type = "l", cpt.col = "blue", xlab = "Year", ylab = "Combined Index (Ln r/ssb)", cpt.width = 4)
legend("topright", legend = "AMOC Mean(s)", col = "blue", lwd = 2)
cpts(comb.amoc)

#Two methods for detection of multiple change-points
comb.pelt <- cpt.mean(comb.num, method = "PELT")
plot(comb.pelt, type = "l", cpt.col = "red", xlab = "Year", ylab = "Combined Index (Ln r/ssb)", cpt.width = 4)
legend("topright", legend = "PELT Mean(s)", col = "red", lwd = 2)
cpts(comb.pelt)

comb.binseg <- cpt.mean(comb.num, method = "BinSeg")
plot(comb.binseg, type = "l", cpt.col = "purple", xlab = "Year", ylab = "Combined Index (Ln r/ssb)", cpt.width = 4)
legend("topright", legend = "BinSeg Mean(s)", col = "purple", lwd = 2)
cpts(comb.binseg)

#number of change-points, means, likelihood, , by method
m.pm <- cpt.mean(comb.num, penalty = "Manual", pen.value = "1.5 * log(n)", method = "PELT")
plot(m.pm, type = "l", cpt.col = "blue", xlab = "Year", ylab = "Combined Index (Ln r/ssb)", cpt.width = 4)
cpts(m.pm)

m.bsm <- cpt.mean(comb.num, "Manual", pen.value = "1.5 * log(n)", method = "BinSeg")
cpts(m.bsm)

comb.point <- cpt.mean(comb.num, method = "PELT")
plot(comb.point, pch = 20, col = "grey", cpt.col = "black", type = "p", xlab = "Year", ylab = "comb Index (Ln r/ssb)")
cpts(comb.point)
coef(comb.point)

