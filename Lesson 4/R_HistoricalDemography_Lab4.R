
# Install the Demography package

install.packages("demography", dependencies = TRUE)

library(demography)

#Importing data from the Human Mortality Database

italy <- hmd.mx("ITA", "account", "password", "Italy")

#Importing other country data

sweden <- hmd.mx("SWE", "account", "password", "sweden")
france <- hmd.mx("FRATNP", "account", "password", "France")

italy
sweden
france

# Plotting the age-specific death rates
plot(italy) 

plot(italy, series=names(italy$rate)[1], year=1900:2000, main="Italy-Female, 1900-2000")
plot(italy, series=names(italy$rate)[2], year=1900:2000, main="Italy-Male, 1900-2000")
plot(italy, series=names(italy$rate)[3], year=1900:2000, main="Italy-Total, 1900-2000")

# The life expectancy series

italy0 <- hmd.e0("ITA", "account", "password")


italy0


plot(italy0)

# We can add main title and show a red dashed line
plot(italy0, main="Italy, Life Expectancy at birth", col = "red", lty = 2)



sweden0 <- hmd.e0("SWE", "account", "password")

plot(sweden0)


# Lifetables from mortality rates

italyf.lt <- lifetable(italy, series = names(italy$rate)[1], years = 1872:2019, ages = italy$age, max.age = 99, type = c("period"))
italym.lt <- lifetable(italy, series = names(italy$rate)[2], years = 1872:2019, ages = italy$age, max.age = 99, type = c("period"))
italyt.lt <- lifetable(italy, series = names(italy$rate)[3], years = 1872:2019, ages = italy$age, max.age = 99, type = c("period"))


lifetable_fem <- print(italyf.lt)
lifetable_mal <- print(italym.lt)
lifetable_tot <- print(italyt.lt)


italyt_2001.lt <- lifetable(italy, series = names(italy$rate)[3], years = 2001, ages = italy$age, max.age = 99, type = c("period"))
print(italyt_2001.lt)


#Export life table object
#sink("C:/Users/Francesco/sink-tav2001.txt")

#tav2001 <- print(italyt_2001.lt)

#write.table(tav2001, "C:/Users/Francesco/tavola2001.txt")

#Plotting ex curves

plot(italyf.lt, main="Female Life Expectancy at Age x, Italy")

plot(italym.lt, main="Male Life Expectancy at Age x, Italy")



#Plotting lx curves

italyt_2001.lt <- lifetable(italy, series = names(italy$rate)[3], years = 2001, ages = italy$age, max.age = 99, type = c("period"))
lx_2001 <-italyt_2001.lt$lx

plot(lx_2001, col="blue", xlab="age", ylab="lx", type="l", lty=1, main = "Survivors lx. Italy - 2001")


#Plotting lx curves at 1872, 1931 and 2001

italyt_1872.lt <- lifetable(italy, series = names(italy$rate)[3], years = 1872, ages = italy$age, max.age = 99, type = c("period"))
lx_1872 <-italyt_1872.lt$lx

italyt_1931.lt <- lifetable(italy, series = names(italy$rate)[3], years = 1931, ages = italy$age, max.age = 99, type = c("period"))
lx_1931 <-italyt_1931.lt$lx

italyt_2001.lt <- lifetable(italy, series = names(italy$rate)[3], years = 2001, ages = italy$age, max.age = 99, type = c("period"))
lx_2001 <-italyt_2001.lt$lx

plot(lx_1872, col="blue", xlab="age", ylab="lx", type="l", lty=1, main = "Survivors lx. Italy - 1872, 1931, 2001")
lines(lx_1931, col="red", lty=2)
lines(lx_2001, col="black", lty=3)
legend("bottomleft",c("1872", "1931", "2001"),col=c("blue","red","black"), lty=1:3)


#Plotting dx curves

italyt_2009.lt <- lifetable(italy, series = names(italy$rate)[3], years = 2009, ages = italy$age, max.age = 110, type = c("period"))
dx_2009 <-italyt_2009.lt$dx


plot(dx_2009, ylim=c(0, 0.05), col="blue", xlab="age", ylab="dx", type="l", lty=1, main = "Deths dx. Italy - 2009")


# Plotting 3D mortality surfaces
age <- italyt.lt$age
year <- italyt.lt$year

persp(age,year,italyt.lt$lx, theta = 40, zlab = "lx", main = "lx. Italy 1872-2009")

persp(age,year,italyt.lt$dx, theta = 40, zlab = "dx", main = "dx. Italy 1872-2009")


#Life expectancy e0
e0_f <- life.expectancy(italy, series = names(italy$rate)[1],  years = 1872:2019, type = c("period"))
e0_m <- life.expectancy(italy, series = names(italy$rate)[2],  years = 1872:2019, type = c("period"))
e0_t <- life.expectancy(italy, series = names(italy$rate)[3],  years = 1872:2019, type = c("period"))

e0_f
e0_m
e0_t

#Evolution of life expectancy at 0, 60 e 80
e_0 <- life.expectancy(italy, series = names(italy$rate)[3],  years = 1872:2019, type = c("period"))
e_60 <- life.expectancy(italy, series = names(italy$rate)[3],  years = 1872:2019, type = c("period"), age=60)
e_80 <- life.expectancy(italy, series = names(italy$rate)[3],  years = 1872:2019, type = c("period"), age=80)

e_80


plot(e_60, ylim=c(0, 25), col="blue", xlab="year", ylab="expectancy", main = "Evolution of e60 and e80")
lines(e_80, col="red", lty=2)
legend("topleft",c("e 60", "e 80"),col=c("blue","red"), lty=1:2)


# extract some ages from the demogdata object

italy_0_3 <- extract.ages(italy, 0:3, combine.upper=FALSE)

plot(italy_0_3, series=names(italy$rate)[1], year=1872, type="l", col="blue", lty=1, main="Ln(mx) ages 0-3. Italy, 1872")
lines(italy_0_3, series=names(italy$rate)[2], year=1872, type="l", col="red", lty=2,)
legend("topright",c("Female", "Male"),col=c("blue","red"), lty=1:2)



# Analyzing fertility data

aus.fert

plot(aus.fert)

tfr <- tfr(aus.fert)

tfr

plot(tfr(aus.fert))


# H index in only 3 years (1872, 1931, 2001)

italyt_1872.lt <- lifetable(italy, series = names(italy$rate)[3], years = 1872, ages = italy$age, max.age = min(100, max(italy$age)), type = c("period"))
lx_1872 <-italyt_1872.lt$lx

italyt_1931.lt <- lifetable(italy, series = names(italy$rate)[3], years = 1931, ages = italy$age, max.age = min(100, max(italy$age)), type = c("period"))
lx_1931 <-italyt_1931.lt$lx

italyt_2001.lt <- lifetable(italy, series = names(italy$rate)[3], years = 2001, ages = italy$age, max.age = min(100, max(italy$age)), type = c("period"))
lx_2001 <-italyt_2001.lt$lx

plot(lx_1872, col="blue", xlab="age", ylab="lx", type="l", lty=1, main = "Survivors lx. Italy - 1872, 1931, 2001")
lines(lx_1931, col="red", lty=2)
lines(lx_2001, col="green", lty=3)
legend("topright",c("1872", "1931", "2001"),col=c("blue","red", "green"), lty=1:3)




#lx <- lx_1872

ln_lx_1872 <- log(lx_1872)

ln_lx_1872

product_1872 <- ln_lx_1872 * lx_1872

product_1872

sum_product_1872 <- sum(product_1872)

sum_product_1872

sum_dem_1872 <- (lx_1872)

sum_dem_1872

sum_dem_1872 <- sum(lx_1872)

sum_dem_1872

H_1872 <- - (sum_product_1872 / sum_dem_1872)

H_1872




ln_lx_1931 <- log(lx_1931)

ln_lx_1931

product_1931 <- ln_lx_1931 * lx_1931

product_1931

sum_product_1931 <- sum(product_1931)

sum_product_1931

sum_dem_1931 <- (lx_1931)

sum_dem_1931

sum_dem_1931 <- sum(lx_1931)

sum_dem_1931

H_1931 <- - (sum_product_1931 / sum_dem_1931)

H_1931





ln_lx_2001 <- log(lx_2001)

ln_lx_2001

product_2001 <- ln_lx_2001 * lx_2001

product_2001

sum_product_2001 <- sum(product_2001)

sum_product_2001

sum_dem_2001 <- (lx_2001)

sum_dem_2001

sum_dem_2001 <- sum(lx_2001)

sum_dem_2001

H_2001 <- - (sum_product_2001 / sum_dem_2001)

H_2001


H_3 <- c(H_1872, H_1931, H_2001)

H_3



# H index from 1872 to 2009. Italy

H <- rep(1,138)
Anno <- seq(1872,2009,1)

for (index in 1:138) {
  
  italyt_year.lt <- lifetable(italy, series = names(italy$rate)[3], years = italy$year[index], ages = italy$age, 100, type = c("period"))
  
  lx_year <-italyt_year.lt$lx
  
  ln_lx_year <- log(lx_year)
  
  ln_lx_year
  
  product_year <- ln_lx_year * lx_year
  
  product_year
  
  sum_product_year <- sum(product_year)
  
  sum_product_year
  
  sum_dem_year <- (lx_year)
  
  sum_dem_year
  
  sum_dem_year <- sum(lx_year)
  
  sum_dem_year
  
  H_year <- - (sum_product_year / sum_dem_year)
  
  H_year
  
  H[index] = H_year
  
  
}

H

serie <- cbind(H, Anno)

serie

plot(Anno, H, type = "l", main="Rectangularization. H index in Italy, 1872-2009")



# Plotting multiple survival curves using a For Loop


lx_matrix <- matrix(data = seq(1,15318,1), nrow=111, ncol=138)
for (index in 1:138) {
  lx_matrix[,index] <- ((lifetable(italy, years = italy$year[index], ages = italy$age, max.age = 110, type = c("period")))$lx)
}

lx_matrix


years <- seq(1872,2009,5)

plot(lx_matrix[,1], xlab="age", ylab="lx", type="l", lty=1)
for (index in seq(1, 138, 5)) {
  lines(lx_matrix[,index], col=rainbow(138)[index], lty=1)
}

legend("bottomright", legend=years, col=rainbow(24), lty=1, cex = 0.5, bty ="n") 





# Comparison of Male and Female Survival: temporal advantage of women in Italy


# Italy Male 2009

italy_M_2009.lt <- lifetable(italy, series = names(italy$rate)[2], years = 2009, ages = italy$age, max.age = 110, type = c("period"))
lx_M_2009 <-italy_M_2009.lt$lx

e_0_M_2009 <- life.expectancy(italy, series = names(italy$rate)[2], years = 2009, type = c("period"))


e_0_M_2009


# Italy Femmale 1985

italy_F_1986.lt <- lifetable(italy, series = names(italy$rate)[1], years = 1986, ages = italy$age, max.age = 110, type = c("period"))
lx_F_1986 <-italy_F_1986.lt$lx

e_0_F_1986 <- life.expectancy(italy, series = names(italy$rate)[1], years = 1986, type = c("period"))


e_0_F_1986




confronto_Italia <- matrix(c(e_0_F_1986, e_0_M_2009), nrow = 1, ncol = 2, byrow = TRUE,
                           dimnames = list(c("e0"), c("Female 1986", "Male 2009")))


confronto_Italia


plot(lx_F_1986, col="red", xlab="age", ylab="lx", type="l", lty=1, main = "Survivors lx by Sex. Italy, Males 2009 and Females 1986")
lines(lx_M_2009, col="blue", lty=2)
legend("topright",c("Female", "Male"),col=c("red","blue"), lty=1:2)


# Comparing probability of death in reproductive ages. Italy 1881


# Italy 1881

italy_F_1881.lt <- lifetable(italy, series = names(italy$rate)[1], years = 1881, ages = italy$age, max.age = 100, type = c("period"))

italy_M_1881.lt <- lifetable(italy, series = names(italy$rate)[2], years = 1881, ages = italy$age, max.age = 100, type = c("period"))



qx_F_1881_15_44 <- italy_F_1881.lt$qx[25:44]
qx_M_1881_15_44 <- italy_M_1881.lt$qx[25:44]

age <- seq(25,44)


plot( age, qx_F_1881_15_44, ylim=c(0, 0.03), col="red", xlab="age", ylab="qx", type="l", lty=1, main = "Probability of death (qx) by sex from age 25 to 44. Italy 1881")
lines( age, qx_M_1881_15_44, col="blue", lty=2)
legend("topright",c("Female", "Male"),col=c("red","blue"), lty=1:2)


# Computing sex ratios from mortality rates


par(mfrow=c(1,2))
plot(sex.ratio(italy))
plot(sex.ratio(extract.ages(italy, 0:99, combine.upper=FALSE)))

dev.off()

# Smoothing Demographic Data in R

italyt_1900.lt <- lifetable(italy, series = names(italy$rate)[3], years = 2009, max.age = 110)
dx_1900 <-italyt_1900.lt$dx

dx_1900_0.spl <- smooth.spline(dx_1900, spar=0)
dx_1900_03.spl <- smooth.spline(dx_1900, spar=0.8)
dx_1900_1.spl <- smooth.spline(dx_1900, spar=1)
dx_1900_6.spl <- smooth.spline(dx_1900, spar=2)

plot(dx_1900, type="l", col="blue", lty=1, main = "Deaths dx. Italy 1900")
lines(dx_1900_0.spl, col="red", lty=2)
lines(dx_1900_03.spl, col="green", lty=3)
lines(dx_1900_1.spl, col="yellow", lty=4)
lines(dx_1900_6.spl, col="black", lty=5)
legend("topleft",c("No smoothing", "Smooth parameter = 0", "Smooth parameter = 0.3", "smooth parameter = 1", "Smooth parameter = 6"), col=c("blue", "red", "green", "yellow", "black"), lty=1:5)


#Importing data from text files

italy2 <- read.demogdata("C:/Users/Francesco/Mx_1x1.txt", "C:/Users/Francesco/Exposures_1x1.txt", type="mortality", label="Italy", skip=2)


# Importing data matrices from external source (Istat website)

data <- read.table(file="C:/Users/Francesco/Documents/Didattica/mx_italia_1992_2012.txt", header=TRUE, sep="\t", dec=".")

pop <- read.table(file="C:/Users/Francesco/Documents/Didattica/pop_italia_1992_2012.txt", header=TRUE, sep="\t", dec=".")

ages <- (0:100)

years <- (1992:2012)

italy_istat <- demogdata(data, pop, ages, years, "mortality", "ITALIA", "male")

plot(italy_istat)


plot(lifetable(italy_istat))

plot(life.expectancy(italy_istat))

plot((lifetable(italy_istat, years = 2012, ages = italy_istat$age, max.age = 100, type = c("period")))$lx)


