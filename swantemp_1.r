#Contosta script from previous work. This script will serve as a template for discussion of modeling approach, but will likely not be used in its present form.

#setwd("C:\\Users\\LocalUser\\Documents\\Alix\\UNH\\PhD\\HF Warming and Nitrogen\\Data\\Raw\\2006-2012 Annual Flux")
setwd("Documents/R/annualflux")
#resp <- read.table("Season.csv", head = TRUE, sep = ",")

#import file containing averaged daily plot temperatures from 2006 to 2015 (or year of interest)
dtemp <- read.csv("cleaned_swntemp_06-16_daily_avg.csv", header=TRUE)

#import file containing respiration and temperature data
resp <- read.csv("SWN_annualflux.csv", header=TRUE)

#Log daily flux values for running linear model based on Alixs method 
resp$lnflux = log(resp$dflux)
#select rows for starting regression
#first fit regression for each plot for each year

#open containers for starting rows
ystart.rows <- c()
yend.rows <- c()

#select rows for beginning the regression
for (i in seq(1,(length(resp[,1])))) {

  ystart.rows[i] = ifelse((resp[i,2] == 1) == 'TRUE', i, NA)
  
  yend.rows[i] = ifelse((resp[i,3] == 1) == 'TRUE', i, NA)


  }
                                            
#remove NAs                                
ystart.rows <- na.omit(ystart.rows)
yend.rows <- na.omit(yend.rows)

#open containers for variables to be included post-regression
year = c()
Plt = c()
trt = c()

lin.int = c()
lin.slope = c()
lin.r2 = c()
#Loop to start all the below line code, can ignore error if it pops up 
for (i in seq(1,length(ystart.rows))) {
    year[i] = resp$year[ystart.rows[i]]
    Plt[i] = resp$Plot[ystart.rows[i]]
    trt[i] = resp$Trt[ystart.rows[i]]
 
    start = ystart.rows[i]    
    end = yend.rows[i]
    linear = lm(resp$lnflux[start:end] ~ resp$temp[start:end])
    lin.int = summary(linear) [[4]][1]
    lin.slope = summary(linear) [[4]][2]
    lin.r2 = summary(linear) [[8]] 

resp.calc <- data.frame(cbind(year, Plt, trt, lin.int, lin.slope, lin.r2))
     
}


################################################################################
################################################################################

m1 <- lm(lnflux ~ temp, data=subset(resp, Plot == 1) ) #Subset by plot
m2 <- lm(lnflux ~ temp, data=subset(resp, Plot == 2) )
m3 <- lm(lnflux ~ temp, data=subset(resp, Plot == 3) )
m4 <- lm(lnflux ~ temp, data=subset(resp, Plot == 4) )
m5 <- lm(lnflux ~ temp, data=subset(resp, Plot == 5) )
m6 <- lm(lnflux ~ temp, data=subset(resp, Plot == 6) )
m7 <- lm(lnflux ~ temp, data=subset(resp, Plot == 7) )
m8 <- lm(lnflux ~ temp, data=subset(resp, Plot == 8) )
m9 <- lm(lnflux ~ temp, data=subset(resp, Plot == 9) )
m10 <- lm(lnflux ~ temp, data=subset(resp, Plot == 10) )
m11 <- lm(lnflux ~ temp, data=subset(resp, Plot == 11) )
m12 <- lm(lnflux ~ temp, data=subset(resp, Plot == 12) )
m13 <- lm(lnflux ~ temp, data=subset(resp, Plot == 13) )
m14 <- lm(lnflux ~ temp, data=subset(resp, Plot == 14) )
m15 <- lm(lnflux ~ temp, data=subset(resp, Plot == 15) )
m16 <- lm(lnflux ~ temp, data=subset(resp, Plot == 16) )
m17 <- lm(lnflux ~ temp, data=subset(resp, Plot == 17) )
m18 <- lm(lnflux ~ temp, data=subset(resp, Plot == 18) )
m19 <- lm(lnflux ~ temp, data=subset(resp, Plot == 19) )
m20 <- lm(lnflux ~ temp, data=subset(resp, Plot == 20) )
m21 <- lm(lnflux ~ temp, data=subset(resp, Plot == 21) )
m22 <- lm(lnflux ~ temp, data=subset(resp, Plot == 22) )
m23 <- lm(lnflux ~ temp, data=subset(resp, Plot == 23) )
m24 <- lm(lnflux ~ temp, data=subset(resp, Plot == 24) )


#estimate daily fluxes from plot-average daily soil temperature values

#first create a data frame for each plot

t1 <- data.frame(cbind("year" = dtemp$year, "month" = dtemp$month, "doy" = dtemp$doy, "dtemp" = dtemp$C1)) 
t2 <- data.frame(cbind("year" = dtemp$year, "month" = dtemp$month, "doy" = dtemp$doy, "dtemp" = dtemp$H2))
t3 <- data.frame(cbind("year" = dtemp$year, "month" = dtemp$month, "doy" = dtemp$doy, "dtemp" = dtemp$HN3))
t4 <- data.frame(cbind("year" = dtemp$year, "month" = dtemp$month, "doy" = dtemp$doy, "dtemp" = dtemp$HN4))
t5 <- data.frame(cbind("year" = dtemp$year, "month" = dtemp$month, "doy" = dtemp$doy, "dtemp" = dtemp$H5))
t6 <- data.frame(cbind("year" = dtemp$year, "month" = dtemp$month, "doy" = dtemp$doy, "dtemp" = dtemp$N6))
t7 <- data.frame(cbind("year" = dtemp$year, "month" = dtemp$month, "doy" = dtemp$doy, "dtemp" = dtemp$HN7))
t8 <- data.frame(cbind("year" = dtemp$year, "month" = dtemp$month, "doy" = dtemp$doy, "dtemp" = dtemp$H8))
t9 <- data.frame(cbind("year" = dtemp$year, "month" = dtemp$month, "doy" = dtemp$doy, "dtemp" = dtemp$N9))
t10 <- data.frame(cbind("year" = dtemp$year, "month" = dtemp$month, "doy" = dtemp$doy, "dtemp" = dtemp$HN10))
t11 <- data.frame(cbind("year" = dtemp$year, "month" = dtemp$month, "doy" = dtemp$doy, "dtemp" = ((dtemp$N6 + dtemp$N21)/2)))
t12 <- data.frame(cbind("year" = dtemp$year, "month" = dtemp$month, "doy" = dtemp$doy, "dtemp" = dtemp$C12))
t13 <- data.frame(cbind("year" = dtemp$year, "month" = dtemp$month, "doy" = dtemp$doy, "dtemp" = dtemp$H13))
t14 <- data.frame(cbind("year" = dtemp$year, "month" = dtemp$month, "doy" = dtemp$doy, "dtemp" = dtemp$C14))
t15 <- data.frame(cbind("year" = dtemp$year, "month" = dtemp$month, "doy" = dtemp$doy, "dtemp" = dtemp$HN15))
t16 <- data.frame(cbind("year" = dtemp$year, "month" = dtemp$month, "doy" = dtemp$doy, "dtemp" = ((dtemp$N6 + dtemp$N21)/2)))
t17<- data.frame(cbind("year" = dtemp$year, "month" = dtemp$month, "doy" = dtemp$doy, "dtemp" = dtemp$HN17))
t18 <- data.frame(cbind("year" = dtemp$year, "month" = dtemp$month, "doy" = dtemp$doy, "dtemp" = dtemp$H18))
t19 <- data.frame(cbind("year" = dtemp$year, "month" = dtemp$month, "doy" = dtemp$doy, "dtemp" = dtemp$C19))
t20 <- data.frame(cbind("year" = dtemp$year, "month" = dtemp$month, "doy" = dtemp$doy, "dtemp" = dtemp$C20))
t21 <- data.frame(cbind("year" = dtemp$year, "month" = dtemp$month, "doy" = dtemp$doy, "dtemp" = dtemp$N21))
t22 <- data.frame(cbind("year" = dtemp$year, "month" = dtemp$month, "doy" = dtemp$doy, "dtemp" = dtemp$H22))
t23 <- data.frame(cbind("year" = dtemp$year, "month" = dtemp$month, "doy" = dtemp$doy, "dtemp" = ((dtemp$N6 + dtemp$N21)/2)))
t24 <- data.frame(cbind("year" = dtemp$year, "month" = dtemp$month, "doy" = dtemp$doy, "dtemp" = dtemp$C24))

################################################################################

#Calculate annual fluxes
#The exponential values are calculated to get the non transformed value of y of the line equation where the dflux value was 
#initially transformed as natural log of the dflux and outputed here for each plot
#[4]][1] is the intercept (or b of y=mx + b
#[4]][2] is the slope (or m of y=mx + b
est.1 <- data.frame(cbind(t1, estflux = exp(summary(m1)[[4]][1] + (t1$dtemp * summary(m1)[[4]][2]))))
est.2 <- data.frame(cbind(t2, estflux = exp(summary(m2)[[4]][1] + (t2$dtemp * summary(m2)[[4]][2]))))
est.3 <- data.frame(cbind(t3, estflux = exp(summary(m3)[[4]][1] + (t3$dtemp * summary(m3)[[4]][2]))))
est.4 <- data.frame(cbind(t4, estflux = exp(summary(m4)[[4]][1] + (t4$dtemp * summary(m4)[[4]][2]))))
est.5 <- data.frame(cbind(t5, estflux = exp(summary(m5)[[4]][1] + (t5$dtemp * summary(m5)[[4]][2]))))
est.6 <- data.frame(cbind(t6, estflux = exp(summary(m6)[[4]][1] + (t6$dtemp * summary(m6)[[4]][2]))))
est.7 <- data.frame(cbind(t7, estflux = exp(summary(m7)[[4]][1] + (t7$dtemp * summary(m7)[[4]][2]))))
est.8 <- data.frame(cbind(t8, estflux = exp(summary(m8)[[4]][1] + (t8$dtemp * summary(m8)[[4]][2]))))
est.9 <- data.frame(cbind(t9, estflux = exp(summary(m9)[[4]][1] + (t9$dtemp * summary(m9)[[4]][2]))))
est.10 <- data.frame(cbind(t10, estflux = exp(summary(m10)[[4]][1] + (t10$dtemp * summary(m10)[[4]][2]))))
est.11 <- data.frame(cbind(t11, estflux = exp(summary(m11)[[4]][1] + (t11$dtemp * summary(m11)[[4]][2]))))
est.12 <- data.frame(cbind(t12, estflux = exp(summary(m12)[[4]][1] + (t12$dtemp * summary(m12)[[4]][2]))))
est.13 <- data.frame(cbind(t13, estflux = exp(summary(m13)[[4]][1] + (t13$dtemp * summary(m13)[[4]][2]))))
est.14 <- data.frame(cbind(t14, estflux = exp(summary(m14)[[4]][1] + (t14$dtemp * summary(m14)[[4]][2]))))
est.15 <- data.frame(cbind(t15, estflux = exp(summary(m15)[[4]][1] + (t15$dtemp * summary(m15)[[4]][2]))))
est.16 <- data.frame(cbind(t16, estflux = exp(summary(m16)[[4]][1] + (t16$dtemp * summary(m16)[[4]][2]))))
est.17 <- data.frame(cbind(t17, estflux = exp(summary(m17)[[4]][1] + (t17$dtemp * summary(m17)[[4]][2]))))
est.18 <- data.frame(cbind(t18, estflux = exp(summary(m18)[[4]][1] + (t18$dtemp * summary(m18)[[4]][2]))))
est.19 <- data.frame(cbind(t19, estflux = exp(summary(m19)[[4]][1] + (t19$dtemp * summary(m19)[[4]][2]))))
est.20 <- data.frame(cbind(t20, estflux = exp(summary(m20)[[4]][1] + (t20$dtemp * summary(m20)[[4]][2]))))
est.21 <- data.frame(cbind(t21, estflux = exp(summary(m21)[[4]][1] + (t21$dtemp * summary(m21)[[4]][2]))))
#est.22 <- data.frame(cbind(t22, estflux = exp(summary(m22)[[4]][1] + (t22$dtemp * summary(m22)[[4]][2]))))
est.23 <- data.frame(cbind(t23, estflux = exp(summary(m23)[[4]][1] + (t23$dtemp * summary(m23)[[4]][2]))))
est.24 <- data.frame(cbind(t24, estflux = exp(summary(m24)[[4]][1] + (t24$dtemp * summary(m24)[[4]][2]))))

#add estimated fluxes within plots and within years

an.1 <- aggregate(est.1$estflux, by=list(est.1$year), sum)
names(an.1) <- c("year", "anflux")
Plt <- 1
trt <- "C"
all.1 <- cbind(an.1, Plt, trt)

an.2 <- aggregate(est.2$estflux, by=list(est.2$year), sum)
names(an.2) <- c("year", "anflux")
Plt <- 2
trt <- "H"
all.2 <- cbind(an.2, Plt, trt)

an.3 <- aggregate(est.3$estflux, by=list(est.3$year), sum)
names(an.3) <- c("year", "anflux")
Plt <- 3
trt <- "HN"
all.3 <- cbind(an.3, Plt, trt)

an.4 <- aggregate(est.4$estflux, by=list(est.4$year), sum)
names(an.4) <- c("year", "anflux")
Plt <- 4
trt <- "HN"
all.4 <- cbind(an.4, Plt, trt)

an.5 <- aggregate(est.5$estflux, by=list(est.5$year), sum)
names(an.5) <- c("year", "anflux")
Plt <- 5
trt <- "H"
all.5 <- cbind(an.5, Plt, trt)

an.6 <- aggregate(est.6$estflux, by=list(est.6$year), sum)
names(an.6) <- c("year", "anflux")
Plt <- 6
trt <- "N"
all.6 <- cbind(an.6, Plt, trt)

an.7 <- aggregate(est.7$estflux, by=list(est.7$year), sum)
names(an.7) <- c("year", "anflux")
Plt <- 7
trt <- "HN"
all.7 <- cbind(an.7, Plt, trt)

an.8 <- aggregate(est.8$estflux, by=list(est.8$year), sum)
names(an.8) <- c("year", "anflux")
Plt <- 8
trt <- "H"
all.8 <- cbind(an.8, Plt, trt)

an.9 <- aggregate(est.9$estflux, by=list(est.9$year), sum, na.rm = T)
names(an.9) <- c("year", "anflux")
Plt <- 9
trt <- "N"
all.9 <- cbind(an.9, Plt, trt)

an.10 <- aggregate(est.10$estflux, by=list(est.10$year), sum)
names(an.10) <- c("year", "anflux")
Plt <- 10
trt <- "HN"
all.10 <- cbind(an.10, Plt, trt)

an.11 <- aggregate(est.11$estflux, by=list(est.11$year), sum)
names(an.11) <- c("year", "anflux")
Plt <- 11
trt <- "N"
all.11 <- cbind(an.11, Plt, trt)

an.12 <- aggregate(est.12$estflux, by=list(est.12$year), sum)
names(an.12) <- c("year", "anflux")
Plt <- 12
trt <- "C"
all.12 <- cbind(an.12, Plt, trt)

an.13 <- aggregate(est.13$estflux, by=list(est.13$year), sum)
names(an.13) <- c("year", "anflux")
Plt <- 13
trt <- "H"
all.13 <- cbind(an.13, Plt, trt)

an.14 <- aggregate(est.14$estflux, by=list(est.14$year), sum)
names(an.14) <- c("year", "anflux")
Plt <- 14
trt <- "C"
all.14 <- cbind(an.14, Plt, trt)

an.15 <- aggregate(est.15$estflux, by=list(est.15$year), sum)
names(an.15) <- c("year", "anflux")
Plt <- 15
trt <- "HN"
all.15 <- cbind(an.15, Plt, trt)

an.16 <- aggregate(est.16$estflux, by=list(est.16$year), sum)
names(an.16) <- c("year", "anflux")
Plt <- 16
trt <- "N"
all.16 <- cbind(an.16, Plt, trt)

an.17 <- aggregate(est.17$estflux, by=list(est.17$year), sum)
names(an.17) <- c("year", "anflux")
Plt <- 17
trt <- "HN"
all.17 <- cbind(an.17, Plt, trt)

an.18 <- aggregate(est.18$estflux, by=list(est.18$year), sum)
names(an.18) <- c("year", "anflux")
Plt <- 18
trt <- "H"
all.18 <- cbind(an.18, Plt, trt)

an.19 <- aggregate(est.19$estflux, by=list(est.19$year), sum)
names(an.19) <- c("year", "anflux")
Plt <- 19
trt <- "C"
all.19 <- cbind(an.19, Plt, trt)

an.20 <- aggregate(est.20$estflux, by=list(est.20$year), sum)
names(an.20) <- c("year", "anflux")
Plt <- 20
trt <- "C"
all.20 <- cbind(an.20, Plt, trt)

an.21 <- aggregate(est.21$estflux, by=list(est.21$year), sum)
names(an.21) <- c("year", "anflux")
Plt <- 21
trt <- "N"
all.21 <- cbind(an.21, Plt, trt)

#an.22 <- aggregate(est.22$estflux, by=list(est.22$year), sum)
#names(an.22) <- c("year", "anflux")
#Plt <- 22
#trt <- "H"
#all.22 <- cbind(an.22, Plt, trt)

an.23 <- aggregate(est.23$estflux, by=list(est.23$year), sum)
names(an.23) <- c("year", "anflux")
Plt <- 23
trt <- "N"
all.23 <- cbind(an.23, Plt, trt)

an.24 <- aggregate(est.24$estflux, by=list(est.24$year), sum)
names(an.24) <- c("year", "anflux")
Plt <- 24
trt <- "C"
all.24 <- cbind(an.24, Plt, trt)

#combine estimated annual fluxes into a single dataframe

allannual <- data.frame(rbind(all.1, all.2, all.3, all.4, all.5, all.6, all.7, all.8, all.9, all.10, all.11, all.12, all.13, all.14, all.15,
                        all.16, all.17, all.18, all.19, all.20, all.21, #all.22,
                        all.23, all.24))

#write file
write.table(allannual, file=paste("swnannual_flux.csv"), sep = ",", na="NA", append=FALSE, col.names=TRUE)

################################################################################
#conduct statistical analysis of annual fluxes

#impoty annual flux data
allannual <- read.table("swnannual_flux.csv", head=TRUE, sep=",")

#create a subset that contains just the heated treatment
annual <- allannual#[allannual$trt == "C" | allannual$trt == "H", ]

#make year a categorical variable
annual$year <- factor(annual$year)

require(nlme)
#fit a gls and a mixed models and compare model fits
m1 <- gls(anflux ~ trt * year, data=annual)
m2 <- lme(fixed = anflux ~ trt * year, random = ~1|Plt, data=annual)
m3 <- lme(fixed = anflux ~ trt * year, random = ~ trt|Plt, data=annual)

#compare model fits
anova(m1, m2, m3)

anova(m2, type = "marginal")

m5 <- lme(fixed = anflux ~ trt * year, random = ~1|Plt, data=annual, method="ML")
m6 <- lme(fixed = anflux ~ trt, random = ~1|Plt, data=annual, method="ML")

anova(m5, m6)

#model fits best with trt * year interaction

#summary(m2)

m7 <- lme(fixed = anflux ~ -1 + (trt + year + trt:year), random = ~1|Plt, data=annual)

anova(m7, type = "marginal")

summary (m7)

annual$trt.year = interaction(annual$trt, annual$year)

m7int = lme(fixed = anflux ~ -1 + (trt.year), random = ~1|Plt, data=annual)
 
summary(glht(m7int, linfct = mcp(trt.year = "Tukey")))
