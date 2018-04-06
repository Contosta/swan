require(plyr)
require(dplyr)
require(ggplot2)
lmp <- function (modelobject) {
    if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
    f <- summary(modelobject)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
}


swn_resp = read.csv("swan_data/SWN_annualflux07-17.csv")
swn_resp$julian_day = as.POSIXlt(ISOdate(swn_resp$year, swn_resp$month, swn_resp$doy))$yday
swn_resp$date = as.POSIXlt(ISOdate(swn_resp$year, swn_resp$month, swn_resp$doy))


#By julian day and plot

swn_resp.p1 = data.frame(filter(swn_resp, Plot == 1))

basic.mod = lm(log(dflux) ~ temp, data = swn_resp.p1)
harmonic.mod = lm(log(dflux) ~ temp + sin(2*pi*month/12) + cos(2*pi*month/12), data = swn_resp.p1)

comparison_results = as.data.frame(matrix(NA, ncol = 9, nrow = 24))
colnames(comparison_results) = c("Plot", "basic.r2", "basic.temp.p", "harmonic.r2", "harmonic.temp.p", "basic.RSS", "harmonic.RSS", "comp.p", "sig")




for(i in 1:24)
{
    if(i == 22){comparison_results$Plot[i] = i; next}
    
    swn_resp.tmp = data.frame(filter(swn_resp, Plot == i))
    
    basic.mod = lm(log(dflux) ~ temp, data = swn_resp.tmp)
    harmonic.mod = lm(log(dflux) ~ temp + sin(2*pi*julian_day/365) + cos(2*pi*julian_day/365), data = swn_resp.tmp)
    mod.comparison = anova(basic.mod, harmonic.mod)
    
    comparison_results$Plot[i] = i
    comparison_results$basic.temp.p[i] = summary(basic.mod)[[4]][2,4]
    comparison_results$basic.r2[i] = summary(basic.mod)[[8]]
    comparison_results$harmonic.temp.p[i] = summary(harmonic.mod)[[4]][2,4]
    comparison_results$harmonic.r2[i] = summary(harmonic.mod)[[8]]
    comparison_results$basic.RSS[i] = mod.comparison[1,2]
    comparison_results$harmonic.RSS[i] = mod.comparison[2,2]
    comparison_results$comp.p[i] = mod.comparison[2,6]
    
    if(mod.comparison[2,6] < 0.05)
    {
        comparison_results$sig[i] = "*"
    }else
    {
        comparison_results$sig[i] = "ns"
    }
}


#By julian day and aggregated treatments
swn_resp_trts = levels(swn_resp$Trt)

comparison_results_trt = as.data.frame(matrix(NA, ncol = 9, nrow = 4))
colnames(comparison_results_trt) = c("Trt", "basic.r2", "basic.temp.p", "harmonic.r2", "harmonic.temp.p", "basic.RSS", "harmonic.RSS", "comp.p", "sig")

for(i in 1:length(swn_resp_trts))
{
    swn_resp.tmp = data.frame(filter(swn_resp, Trt == swn_resp_trts[i]))
    comparison_results_trt$Trt[i] = swn_resp_trts[i]
    basic.mod = lm(log(dflux) ~ temp, data = swn_resp.tmp)
    harmonic.mod = lm(log(dflux) ~ temp + sin(2*pi*julian_day/365) + cos(2*pi*julian_day/365), data = swn_resp.tmp)
    mod.comparison = anova(basic.mod, harmonic.mod)
    
    comparison_results_trt$basic.temp.p[i] = summary(basic.mod)[[4]][2,4]
    comparison_results_trt$basic.r2[i] = summary(basic.mod)[[8]]
    comparison_results_trt$harmonic.temp.p[i] = summary(harmonic.mod)[[4]][2,4]
    comparison_results_trt$harmonic.r2[i] = summary(harmonic.mod)[[8]]
    comparison_results_trt$basic.RSS[i] = mod.comparison[1,2]
    comparison_results_trt$harmonic.RSS[i] = mod.comparison[2,2]
    comparison_results_trt$comp.p[i] = mod.comparison[2,6]
    
    if(mod.comparison[2,6] < 0.05)
    {
        comparison_results_trt$sig[i] = "*"
    }else
    {
        comparison_results_trt$sig[i] = "ns"
    }
}



#By julian day and aggregated treatments; adding interaction between temp and year
swn_resp_trts = levels(swn_resp$Trt)

comparison_results_trt_int = as.data.frame(matrix(NA, ncol = 11, nrow = 4))
colnames(comparison_results_trt_int) = c("Trt", "harmonic.r2", "harmonic.temp.p", "inxn.mod.r2", "inxn.mod.temp.p", "inxn.mod.temp.yr.p", "inxn.mod.temp.yr.slope", "harmonic.RSS", "inxn.mod.RSS", "comp.p", "sig")

for(i in 1:length(swn_resp_trts))
{
    swn_resp.tmp = data.frame(filter(swn_resp, Trt == swn_resp_trts[i]))
    
    comparison_results_trt_int$Trt[i] = swn_resp_trts[i]
    harmonic.mod = lm(log(dflux) ~ temp + sin(2*pi*julian_day/365) + cos(2*pi*julian_day/365), data = swn_resp.tmp)
    int.mod = lm(log(dflux) ~ temp + sin(2*pi*julian_day/365) + cos(2*pi*julian_day/365) + temp:year, data = swn_resp.tmp)
    mod.comparison = anova(harmonic.mod, int.mod)
    
    comparison_results_trt_int$harmonic.temp.p[i] = summary(harmonic.mod)[[4]][2,4]
    comparison_results_trt_int$harmonic.r2[i] = summary(harmonic.mod)[[8]]
    comparison_results_trt_int$inxn.mod.temp.p[i] = summary(int.mod)[[4]][2,4]
    comparison_results_trt_int$inxn.mod.temp.yr.p[i] = summary(int.mod)[[4]][5,4]
    comparison_results_trt_int$inxn.mod.temp.yr.slope[i] = summary(int.mod)[[4]][5,1]
    comparison_results_trt_int$inxn.mod.r2[i] = summary(int.mod)[[8]]
    comparison_results_trt_int$harmonic.RSS[i] = mod.comparison[1,2]
    comparison_results_trt_int$inxn.mod.RSS[i] = mod.comparison[2,2]
    comparison_results_trt_int$comp.p[i] = mod.comparison[2,6]
    
    if(mod.comparison[2,6] < 0.05)
    {
        comparison_results_trt_int$sig[i] = "*"
    }else
    {
        comparison_results_trt_int$sig[i] = "ns"
    }
}

pdf("swan_data/ln_flux_by_julian_day_by_trt.pdf", width = 8, height = 6)
ggplot(swn_resp, aes(x = julian_day, y = log(dflux), color = Trt)) +
geom_point() +
geom_smooth(method = lm, formula = y ~ x + sin(2*pi*x/365) + cos(2*pi*x/365)) +
scale_color_manual(values = c("N" = "#009E73", "H" = "#D55E00", "HN" = "#CC79A7", "C" = "#0072B2")) +
my_gg_theme
dev.off()

pdf("swan_data/ln_flux_by_julian_day_by_trtby_yr_no_err.pdf", width = 12, height = 10)
ggplot(swn_resp, aes(x = julian_day, y = log(dflux), color = Trt)) +
geom_point() +
facet_wrap(~year, scales = "free") +
geom_smooth(method = lm, se = F, formula = y ~ x + sin(2*pi*x/365) + cos(2*pi*x/365)) +
scale_color_manual(values = c("N" = "#009E73", "H" = "#D55E00", "HN" = "#CC79A7", "C" = "#0072B2")) +
my_gg_theme
dev.off()

pdf("swan_data/ln_flux_by_temp_by_yr_by_trt.pdf", width = 12, height = 8)
ggplot(swn_resp, aes(x = temp, y = log(dflux), color = Trt)) +
geom_point(size = 2) +
facet_wrap(~year, scales = "free") +
geom_smooth(method = lm) +
scale_color_manual(values = c("N" = "#009E73", "H" = "#D55E00", "HN" = "#CC79A7", "C" = "#0072B2")) +
my_gg_theme
dev.off()

pdf("swan_data/ln_flux_by_temp_by_yr_by_trt_no_err.pdf", width = 12, height = 8)
ggplot(swn_resp, aes(x = temp, y = log(dflux), color = Trt)) +
geom_point(size = 2) +
facet_wrap(~year, scales = "free") +
geom_smooth(method = lm, se = F) +
scale_color_manual(values = c("N" = "#009E73", "H" = "#D55E00", "HN" = "#CC79A7", "C" = "#0072B2")) +
my_gg_theme
dev.off()

pdf("swan_data/ln_flux_by_temp_by_trt.pdf", width = 8, height = 6)
ggplot(swn_resp, aes(x = temp, y = log(dflux), color = Trt)) +
geom_point(size = 2) +
geom_smooth(method = lm) +
scale_color_manual(values = c("N" = "#009E73", "H" = "#D55E00", "HN" = "#CC79A7", "C" = "#0072B2")) +
my_gg_theme
dev.off()

