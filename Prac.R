
####################################################
#                    Practical 
####################################################

# Intro -------------------------------------------------------------------
# Probability Distribution function

df_sample <- data.frame(x = c(
  x = rnorm(10000, mean = 0, sd = 3),
  y = rnorm(10000, mean = 0, sd = 10),
  z = rnorm(10000, mean = 0, sd = 2),
  q = rnorm(10000, mean = 1, sd = 1.5)),
  g = gl(4, 10000))

# Probability density function
ggplot(df_sample, aes(x, colour = g)) + geom_density(adjust = 5)

# Cumulative Distribution function
ggplot(df_sample, aes(x, colour = g)) + stat_ecdf()

# Copulas -----------------------------------------------------------------
####################################################
#   THANKS TO THOMAS NAGLER's VineCopula package  #
####################################################
#https://www.r-bloggers.com/how-to-fit-a-copula-model-in-r-heavily-revised-part-1-basic-tools/
  
###############################
#     Fitting Copulas         #
###############################  

# Copula generator
library(copula)
library(VineCopula)
library(magrittr)
# VineCopula is the shizz nizz for copula flow
# EDA > Inference > Properties

# Fancy 3D plain scatterplots
library(scatterplot3d)
library(rgl)
# ggplot2
library(ggplot2)
# Useful package to set ggplot plots one next to the other
library(grid)
set.seed(235)

#Elliptical and Archimedean copulas have nice mathematical proprieties, shape and formulas. They are a good choice for the initial warm up. Assuming you already know the parameters, this is how you would generate a bivariate normal and a t copula

# Generate a bivariate normal copula with rho = 0.7
normal <- tCopula(par = 0.8, dim =2)
# Generate a bivariate t-copula with rho = 0.8 and df = 2
stc <- tCopula(par = 0.8, dim = 2, df = 1)

# This fact leads to a great simplyfication when trying to model the joint behaviour complex phenomena, mainly because the marginals are easier to model than the joint distribution. Furthermore, if you had to model the joint behaviour of, say more than 20 objects (for instance the life of 20+ capacitors in an electrical component), then you would probably have a hard time by going straight into modelling the joint behaviour while the marginal distributions may be easier to estimate.

# Generate random samples
norm_d <- rCopula(2000, normal)
t_d <- rCopula(2000, stc)

# Plot the samples
p1 <- qplot(norm_d[,1], norm_d[,2], colour = norm_d[,1], main="Norm with rho = 0.8", xlab = "u", ylab = "v")

p1

p2 <- qplot(t_d[,1], t_d[,2], colour = t_d[,1], main="Student t with rho = 0.8 and df = 1", xlab = "u", ylab = "v") 

p2
# Once the copula has been fitted, you can easily generate random numbers by using rCopula method for the single copula or rMvdc for the multivariate distribution as below

BiCopKDE(pobs(norm_d)[ ,1], pobs(norm_d)[ ,2])
BiCopKDE(pobs(t_d)[ ,1], pobs(t_d)[ ,2])

# The general form "name" + "Copula()" can be used to build Archimedean copulas as well. Archimedean copulas take only a single parameter $theta$
# Build a Frank, a Gumbel and a Clayton copula

frank <- BiCop(5, tau = 0.5) %>% 
  BiCopSim(10000 , .)
gumbel <- BiCop(4, tau = 0.5) %>% 
  BiCopSim(10000 , .)
clayton <- BiCop(3, tau = 0.5) %>% 
  BiCopSim(10000 , .)

# Print information on the Frank copula
print(BiCop(5, tau = 0.5))

# Plot the samples
p1 <- qplot(frank[,1], frank[,2], colour = frank[,1], main="Frank copula random samples", xlab = "u", ylab = "v")
p2 <- qplot(gumbel[,1], gumbel[,2], colour = gumbel[,1], main="Gumbel copula random samples", xlab = "u", ylab = "v") 
p3 <- qplot(clayton[,1], clayton[,2], colour = clayton[,1], main="Clayton copula random samples", xlab = "u", ylab = "v")

# Define grid layout to locate plots and print each graph^(1)
x11()
pushViewport(viewport(layout = grid.layout(1, 3)))
print(p1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(p3, vp = viewport(layout.pos.row = 1, layout.pos.col = 3))

# Contours
contour(BiCop(5, tau = 0.5))
BiCopKDE(frank[ ,1], frank[ ,2])

contour(BiCop(4, tau = 0.5))
BiCopKDE(gumbel[ ,1], gumbel[ ,2])

contour(BiCop(3, tau = 0.5))
BiCopKDE(clayton[ ,1],clayton[ ,2])

# Density plot
plot(BiCop(5, tau = 0.5),  main ="Frank copula density")
plot(BiCop(4, tau = 0.5),  main ="Gumbel copula density")
plot(BiCop(3, tau = 0.5),  main ="Clayton copula density")

# Changes in dependence measure
t1 <- BiCop(2, tau = 0.2, par2 = 3) %>%
   BiCopSim(10000 , .) 
 
t2 <- BiCop(2, tau = 0.5, par2 = 3) %>%
   BiCopSim(10000 , .) 
 
t3 <- BiCop(2, tau = 0.8, par2 = 3) %>%
   BiCopSim(10000 , .) 

p1 <- qplot(t1[,1], t1[,2], colour = t1[,1], 
            main= expression(paste("Student T  where ", tau,"= 0.2")),
            xlab = "u", ylab = "v")

p2 <- qplot(t2[,1], t2[,2], colour = t2[,1], 
            main= expression(paste("Student T  where ", tau,"= 0.5")),
            xlab = "u", ylab = "v")

p3 <- qplot(t3[,1], t3[,2], colour = t3[,1], 
            main= expression(paste("Student T  where ", tau,"= 0.8")),
            xlab = "u", ylab = "v")

x11()
pushViewport(viewport(layout = grid.layout(1, 3)))
print(p1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(p3, vp = viewport(layout.pos.row = 1, layout.pos.col = 3))

# Changes in par2 measure
t1 <- BiCop(2, tau = 0.5, par2 = 3) %>%
  BiCopSim(10000 , .) 

t2 <- BiCop(2, tau = 0.5, par2 = 10) %>%
  BiCopSim(10000 , .) 

t3 <- BiCop(2, tau = 0.5, par2 = 10e7) %>%
  BiCopSim(10000 , .) 

p1 <- qplot(t1[,1], t1[,2], colour = t1[,1], 
            main= expression(paste("Student T  where ", upsilon,"= 3")),
            xlab = "u", ylab = "v")

p2 <- qplot(t2[,1], t2[,2], colour = t2[,1], 
            main= expression(paste("Student T  where ", upsilon,"= 10")),
            xlab = "u", ylab = "v")

p3 <- qplot(t3[,1], t3[,2], colour = t3[,1], 
            main= expression(paste("Student T  where ", upsilon,"= 10e7")),
            xlab = "u", ylab = "v")

x11()
pushViewport(viewport(layout = grid.layout(1, 3)))
print(p1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(p3, vp = viewport(layout.pos.row = 1, layout.pos.col = 3))



###############################
#     Fitting Copulas         #
###############################

# Estimation
fit1 <- BiCopEst(clayton[, 1], clayton[, 2], family = 5)
summary(fit1)
plot(fit1)


# White's information matrix equality (White 1982) as introduced by Huang and Prokhorov (2011)
BiCopGofTest(clayton[, 1], clayton[, 2], fit1)

BiCopKDE(clayton[, 1], clayton[, 2]) 

contour(fit1, col = 2, add = TRUE, drawlabels = FALSE)


# Best fit estimation
fit2 <- BiCopSelect(clayton[, 1], clayton[, 2])
contour(fit2, col = 3, add = TRUE, drawlabels = FALSE)

BiCopGofTest(clayton[, 1], clayton[, 2], fit2)

copsim <- BiCopSim(10000, BiCop(1, tau = 0.2))

x <- copsim[,2] 
y <- copsim[,1] 
CDF <- BiCopCDF(copsim[,1],
         copsim[,2], 
         BiCop(1, tau = 0.2))
PDF <- BiCopPDF(copsim[,1], 
         copsim[,2], 
         BiCop(1, tau = 0.2))

kde <- kde2d(x, y, n = 100)

col2 <- heat.colors(length(CDF))[rank(CDF)]
persp3d(x=kde, col = 4)
plot(BiCop(1, tau = 0.2))
# Lets have a look
plot3d(x, y, PDF, 
       col = 4)

scatterplot3d(copsim[,1], 
              copsim[,2], 
              BiCopPDF(copsim[,1], 
                         copsim[,2], 
                         fit2), 
              color="red", main="Density", xlab = "u1", ylab="u2", zlab="pMvdc",pch=".")
plot(fit2)

plot3d(x, y, CDF, 
       col = 4)

scatterplot3d(copsim[,1], 
              copsim[,2], 
              BiCopCDF(copsim[,1], 
                       copsim[,2], 
                       fit2), 
              color="red", main="Density", xlab = "u1", ylab="u2", zlab="pMvdc",pch=".")

# Vine copula -------------------------------------------------------------
cree <- read.csv('Data/cree_r.csv',header=F)$V2
yahoo <- read.csv('Data/yahoo_r.csv',header=F)$V2

data(daxreturns)
u <- daxreturns[, 1:4]
pairs.copuladata(u)

vinefit <- RVineStructureSelect(u)
summary(vinefit)

plot(vinefit)

contour(vinefit)
