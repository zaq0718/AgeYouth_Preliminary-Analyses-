library(readxl)
library(metafor)
library(psych)
library(ggplot2)
library(patchwork)

theme_update(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(color="black"), axis.text.y = element_text(color="black"))


#### DATA AND TRANSFORMATIONS ####

dat <- read_xlsx("Development of narcissism - Data.xlsx")


#### Transformations ####

dat$artid <- as.factor(dat$artid)
dat$author <- as.factor(dat$author)
dat$pubyear <- as.numeric(dat$pubyear)
dat$pubtype <- as.factor(dat$pubtype)
dat$inclusion <- as.factor(dat$inclusion)
dat$exclreas <- as.factor(dat$exclreas)
dat$sampid <- as.factor(dat$sampid)
dat$sampname <- as.factor(dat$sampname)
dat$samptype <- as.factor(dat$samptype)
dat$studyname <- as.factor(dat$studyname)
dat$otherref <- as.factor(dat$otherref)
dat$n <- as.numeric(dat$n)
dat$country <- as.factor(dat$country)
dat$female <- as.numeric(dat$female)
dat$ethn <- as.factor(dat$ethn)
dat$measure <- as.factor(dat$measure)
dat$outcome <- as.factor(dat$outcome)
dat$outcome2 <- as.factor(dat$outcome2)
dat$factor <- as.factor(dat$factor)
dat$factor2 <- as.factor(dat$factor2)
dat$reverse <- as.factor(dat$reverse)
dat$meastype <- as.factor(dat$meastype)
dat$rel <- as.numeric(dat$rel)
dat$t1year <- as.numeric(dat$t1year)
dat$agem1 <- as.numeric(dat$agem1)
dat$agesd1 <- as.numeric(dat$agesd1)
dat$interval <- as.factor(dat$interval)
dat$agem1i <- as.numeric(dat$agem1i)
dat$outm1 <- as.numeric(dat$outm1)
dat$outsd1 <- as.numeric(dat$outsd1)
dat$outm2 <- as.numeric(dat$outm2)
dat$outsd2 <- as.numeric(dat$outsd2)
dat$timelag <- as.numeric(dat$timelag)
dat$corr <- as.numeric(dat$corr)
dat$pubstatus <- as.factor(dat$pubstatus)

dat$caseid <- 1:nrow(dat)
dat$clinical <- factor(ifelse((dat$samptype=="1"|dat$samptype=="2"|dat$samptype=="5"), "no", "yes"))
dat <- transform(dat, yob = t1year-agem1)
dat <- transform(dat, agem1ic1 = (agem1i-23.13)/10)
dat <- transform(dat, agem1ic2 = agem1ic1^2)
dat <- transform(dat, agem1ic3 = agem1ic1^3)
dat <- transform(dat, timelagc = timelag-11.42)
dat <- transform(dat, agem2i = agem1i+timelag)
dat <- transform(dat, female = female/100)


#### Computation of Effect Sizes of Mean-Level Change ####

dat$corrmean <- mean(dat$corr, na.rm = TRUE)
dat$corrx <- ifelse(is.na(dat$corr), dat$corrmean, dat$corr) #replace missing data (for computation of mean-level change ES only)
dat <- transform(dat, es_d = (outm2-outm1)/outsd1)
dat <- transform(dat, es_dyear = es_d/timelag)
dat <- transform(dat, v_d = ((2*(1-corrx)/n)+(es_d*es_d/(2*n))))
dat <- transform(dat, v_dyear = v_d/(timelag^2))


#### Computation of Effect Sizes of Rank-Order Stability ####

dat$corrdis <- dat$corr/dat$rel #compute disattenuated test-retest correlation
dat$corrdis <- ifelse(dat$corrdis >= 0.99, 0.99, dat$corrdis) #if corrdis >= 0.99, set to 0.99
dat$corrdis_z <- fisherz(dat$corrdis)
dat$v_corr <- (1/(dat$n-3))
dat$v_corrdis <- dat$v_corr/(dat$rel*dat$rel) #sample variance of disattenuated test-retest correlation


#### Subsamples ####

dat.artid <- dat[!duplicated(dat$artid),]
dat.sample <- dat[!duplicated(dat$sampid),]

dat.agentic <- subset(dat, subset = factor == "Agentic")
dat.antagonistic <- subset(dat, subset = factor == "Antagonistic")
dat.neurotic <- subset(dat, subset = factor == "Neurotic")
dat.agentic.es_dyear <- subset(dat.agentic, subset = es_dyear != "NA")
dat.antagonistic.es_dyear <- subset(dat.antagonistic, subset = es_dyear != "NA")
dat.neurotic.es_dyear <- subset(dat.neurotic, subset = es_dyear != "NA")
dat.agentic.es_dyear.sample <- dat.agentic.es_dyear[!duplicated(dat.agentic.es_dyear$sampid),]
dat.antagonistic.es_dyear.sample <- dat.antagonistic.es_dyear[!duplicated(dat.antagonistic.es_dyear$sampid),]
dat.neurotic.es_dyear.sample <- dat.neurotic.es_dyear[!duplicated(dat.neurotic.es_dyear$sampid),]

dat.agentic.nonclin <- subset(dat.agentic, subset = clinical == "no")
dat.antagonistic.nonclin <- subset(dat.antagonistic, subset = clinical == "no")
dat.neurotic.nonclin <- subset(dat.neurotic, subset = clinical == "no")
dat.agentic.nonclin.es_dyear <- subset(dat.agentic.nonclin, subset = es_dyear != "NA")
dat.antagonistic.nonclin.es_dyear <- subset(dat.antagonistic.nonclin, subset = es_dyear != "NA")
dat.neurotic.nonclin.es_dyear <- subset(dat.neurotic.nonclin, subset = es_dyear != "NA")
dat.agentic.nonclin.es_dyear.sample <- dat.agentic.nonclin.es_dyear[!duplicated(dat.agentic.nonclin.es_dyear$sampid),]
dat.antagonistic.nonclin.es_dyear.sample <- dat.antagonistic.nonclin.es_dyear[!duplicated(dat.antagonistic.nonclin.es_dyear$sampid),]
dat.neurotic.nonclin.es_dyear.sample <- dat.neurotic.nonclin.es_dyear[!duplicated(dat.neurotic.nonclin.es_dyear$sampid),]

dat.nonclin <- subset(dat, subset = clinical == "no")
dat.nonclin.es_dyear <- subset(dat.nonclin, subset = es_dyear != "NA")

dat.agentic2 <- subset(dat, subset = factor2 == "Agentic")
dat.antagonistic2 <- subset(dat, subset = factor2 == "Antagonistic")
dat.neurotic2 <- subset(dat, subset = factor2 == "Neurotic")
dat.agentic2.es_dyear <- subset(dat.agentic2, subset = es_dyear != "NA")
dat.antagonistic2.es_dyear <- subset(dat.antagonistic2, subset = es_dyear != "NA")
dat.neurotic2.es_dyear <- subset(dat.neurotic2, subset = es_dyear != "NA")
dat.agentic2.es_dyear.sample <- dat.agentic2.es_dyear[!duplicated(dat.agentic2.es_dyear$sampid),]
dat.antagonistic2.es_dyear.sample <- dat.antagonistic2.es_dyear[!duplicated(dat.antagonistic2.es_dyear$sampid),]
dat.neurotic2.es_dyear.sample <- dat.neurotic2.es_dyear[!duplicated(dat.neurotic2.es_dyear$sampid),]

dat.agentic2.nonclin <- subset(dat.agentic2, subset = clinical == "no")
dat.antagonistic2.nonclin <- subset(dat.antagonistic2, subset = clinical == "no")
dat.neurotic2.nonclin <- subset(dat.neurotic2, subset = clinical == "no")
dat.agentic2.nonclin.es_dyear <- subset(dat.agentic2.nonclin, subset = es_dyear != "NA")
dat.antagonistic2.nonclin.es_dyear <- subset(dat.antagonistic2.nonclin, subset = es_dyear != "NA")
dat.neurotic2.nonclin.es_dyear <- subset(dat.neurotic2.nonclin, subset = es_dyear != "NA")
dat.agentic2.nonclin.es_dyear.sample <- dat.agentic2.nonclin.es_dyear[!duplicated(dat.agentic2.nonclin.es_dyear$sampid),]
dat.antagonistic2.nonclin.es_dyear.sample <- dat.antagonistic2.nonclin.es_dyear[!duplicated(dat.antagonistic2.nonclin.es_dyear$sampid),]
dat.neurotic2.nonclin.es_dyear.sample <- dat.neurotic2.nonclin.es_dyear[!duplicated(dat.neurotic2.nonclin.es_dyear$sampid),]

dat.agentic.corr <- subset(dat.agentic, subset = corr != "NA")
dat.antagonistic.corr <- subset(dat.antagonistic, subset = corr != "NA")
dat.neurotic.corr <- subset(dat.neurotic, subset = corr != "NA")
dat.agentic.corr.sample <- dat.agentic.corr[!duplicated(dat.agentic.corr$sampid),]
dat.antagonistic.corr.sample <- dat.antagonistic.corr[!duplicated(dat.antagonistic.corr$sampid),]
dat.neurotic.corr.sample <- dat.neurotic.corr[!duplicated(dat.neurotic.corr$sampid),]

dat.agentic2.corr <- subset(dat.agentic2, subset = corr != "NA")
dat.antagonistic2.corr <- subset(dat.antagonistic2, subset = corr != "NA")
dat.neurotic2.corr <- subset(dat.neurotic2, subset = corr != "NA")
dat.agentic2.corr.sample <- dat.agentic2.corr[!duplicated(dat.agentic2.corr$sampid),]
dat.antagonistic2.corr.sample <- dat.antagonistic2.corr[!duplicated(dat.antagonistic2.corr$sampid),]
dat.neurotic2.corr.sample <- dat.neurotic2.corr[!duplicated(dat.neurotic2.corr$sampid),]


#### Sample Characteristics ####

min(dat.sample$n)
max(dat.sample$n)
mean(dat.sample$n)
sd(dat.sample$n)
median(dat.sample$n)
sum(dat.sample$n)

table(dat.sample$samptype)

mean(dat.sample$female, na.rm = TRUE)
min(dat.sample$female, na.rm = TRUE)
max(dat.sample$female, na.rm = TRUE)
sd(dat.sample$female, na.rm = TRUE)
median(dat.sample$female, na.rm = TRUE)

table(dat.sample$country)
table(dat.sample$ethn)

min(dat.sample$agem1)
max(dat.sample$agem1)
mean(dat.sample$agem1)
sd(dat.sample$agem1)

max(dat$agem1i)
max(dat$agem2i)

min(dat.sample$t1year)
max(dat.sample$t1year)
mean(dat.sample$t1year)
sd(dat.sample$t1year)

min(dat.sample$yob)
max(dat.sample$yob)
mean(dat.sample$yob)
sd(dat.sample$yob)

describe(dat$timelag, skew=TRUE)


#### MEAN-LEVEL CHANGE ####

#### Effect Size Analyses ####

res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.agentic)
print(res, digits=3)
sum(dat.agentic.es_dyear.sample$n)

res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.antagonistic)
print(res, digits=3)
sum(dat.antagonistic.es_dyear.sample$n)

res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.neurotic)
print(res, digits=3)
sum(dat.neurotic.es_dyear.sample$n)


#### Age Effects ####

res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.agentic, mods = cbind(agem1ic1))
print(res, digits=4)

res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.agentic, mods = cbind(agem1ic1, agem1ic2))
print(res, digits=4)

res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.agentic, mods = cbind(agem1ic1, agem1ic2, agem1ic3))
print(res, digits=4)

res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.antagonistic, mods = cbind(agem1ic1))
print(res, digits=4)

res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.antagonistic, mods = cbind(agem1ic1, agem1ic2))
print(res, digits=4)

res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.antagonistic, mods = cbind(agem1ic1, agem1ic2, agem1ic3))
print(res, digits=4)

res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.neurotic, mods = cbind(agem1ic1))
print(res, digits=4)

res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.neurotic, mods = cbind(agem1ic1, agem1ic2))
print(res, digits=4)

res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.neurotic, mods = cbind(agem1ic1, agem1ic2, agem1ic3))
print(res, digits=4)


#### Moderator Analyses ####

dat.sample$clinicalnum <- as.numeric(dat.sample$clinicalnum <- dat.sample$clinical)
cor.test(dat.sample$female, dat.sample$yob)
cor.test(dat.sample$female, dat.sample$clinicalnum)
cor.test(dat.sample$yob, dat.sample$clinicalnum)

res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.agentic, mods = cbind(female, yob, clinical))
print(res, digits=4)

res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.antagonistic, mods = cbind(female, yob, clinical))
print(res, digits=4)

res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.neurotic, mods = cbind(female, yob, clinical))
print(res, digits=4)


#### Effect Size Analyses (Nonclinical Samples) ####

res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.agentic.nonclin)
print(res, digits=3)
sum(dat.agentic.nonclin.es_dyear.sample$n)

res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.antagonistic.nonclin)
print(res, digits=3)
sum(dat.antagonistic.nonclin.es_dyear.sample$n)

res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.neurotic.nonclin)
print(res, digits=3)
sum(dat.neurotic.nonclin.es_dyear.sample$n)

min(dat.nonclin.es_dyear$agem1i)
max(dat.nonclin.es_dyear$agem2i)
dat.mlchange <- data.frame(x = c(8, 77))
ggplot(dat.mlchange, aes(x, y)) + labs(x = "Age", y = "Cumulative d value") +
  scale_x_continuous(breaks = seq(10, 70, 10), lim = c(8, 77), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(-0.8, 0.2, 0.2), lim = c(-0.8, 0.2), expand = c(0,0)) +
  geom_hline(yintercept= 0, linetype="dashed", color = "black", linewidth = 1) +
  geom_abline(intercept= -0.004*-8, slope = -0.004, linetype="solid", color = "black", linewidth = 1) +
  geom_abline(intercept= -0.006*-8, slope = -0.006, linetype="solid", color = "black", linewidth = 1) +
  geom_abline(intercept= -0.008*-8, slope = -0.008, linetype="solid", color = "black", linewidth = 1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
  axis.line = element_line(colour = "black")) + 
  annotate("text", x=70, y=-0.2, label="Agentic") + 
  annotate("text", x=69, y=-0.3, label="Antagonistic") + 
  annotate("text", x=66, y=-0.4, label="Neurotic")
ggsave(file="fig1.eps", width=5, height=3.75)


#### Effect Size Analyses (Clinical Samples) #### 

dat.agentic.clin <- subset(dat.agentic, subset = clinical == "yes")
dat.antagonistic.clin <- subset(dat.antagonistic, subset = clinical == "yes")
dat.neurotic.clin <- subset(dat.neurotic, subset = clinical == "yes")
dat.agentic.clin.es_dyear <- subset(dat.agentic.clin, subset = es_dyear != "NA")
dat.antagonistic.clin.es_dyear <- subset(dat.antagonistic.clin, subset = es_dyear != "NA")
dat.neurotic.clin.es_dyear <- subset(dat.neurotic.clin, subset = es_dyear != "NA")
dat.agentic.clin.es_dyear.sample <- dat.agentic.clin.es_dyear[!duplicated(dat.agentic.clin.es_dyear$sampid),]
dat.antagonistic.clin.es_dyear.sample <- dat.antagonistic.clin.es_dyear[!duplicated(dat.antagonistic.clin.es_dyear$sampid),]
dat.neurotic.clin.es_dyear.sample <- dat.neurotic.clin.es_dyear[!duplicated(dat.neurotic.clin.es_dyear$sampid),]

res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.agentic.clin)
print(res, digits=3)
sum(dat.agentic.clin.es_dyear.sample$n)

res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.antagonistic.clin)
print(res, digits=3)
sum(dat.antagonistic.clin.es_dyear.sample$n)

res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.neurotic.clin)
print(res, digits=3)
sum(dat.neurotic.clin.es_dyear.sample$n)


#### Replication of Effect Size Analyses with Reduced Set of Measures ####

res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.agentic2)
print(res, digits=3)
sum(dat.agentic2.es_dyear.sample$n)

res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.antagonistic2)
print(res, digits=3)
sum(dat.antagonistic2.es_dyear.sample$n)

res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.neurotic2)
print(res, digits=3)
sum(dat.neurotic2.es_dyear.sample$n)

res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.agentic2.nonclin)
print(res, digits=3)
sum(dat.agentic2.nonclin.es_dyear.sample$n)

res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.antagonistic2.nonclin)
print(res, digits=3)
sum(dat.antagonistic2.nonclin.es_dyear.sample$n)

res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.neurotic2.nonclin)
print(res, digits=3)
sum(dat.neurotic2.nonclin.es_dyear.sample$n)


#### Tests of Publication Bias ####

res <- rma(yi=es_dyear, vi=v_dyear, data=dat.agentic)
print(res, digits=3)
regtest(res)
res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.agentic, mods = cbind(pubstatus))
print(res, digits=3)
table(dat.agentic.es_dyear$pubstatus)

res <- rma(yi=es_dyear, vi=v_dyear, data=dat.antagonistic)
print(res, digits=3)
regtest(res)
res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.antagonistic, mods = cbind(pubstatus))
print(res, digits=3)
table(dat.antagonistic.es_dyear$pubstatus)

res <- rma(yi=es_dyear, vi=v_dyear, data=dat.neurotic)
print(res, digits=3)
regtest(res)
res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.neurotic, mods = cbind(pubstatus))
print(res, digits=3)
table(dat.neurotic.es_dyear$pubstatus)

par(mfrow = c(3, 1))

res <- rma(yi=es_dyear, vi=v_dyear, data=dat.agentic.nonclin)
print(res, digits=3)
regtest(res)
funnel(res, xlab="Effect Size (d per year)", ylab="Standard Error", main="Agentic Narcissism") +
  mtext("A", side = 3, line = 2.4, adj = -0.16, cex = 1.0)
res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.agentic.nonclin, mods = cbind(pubstatus))
print(res, digits=3)
table(dat.agentic.nonclin.es_dyear$pubstatus)

res <- rma(yi=es_dyear, vi=v_dyear, data=dat.antagonistic.nonclin)
print(res, digits=3)
regtest(res)
funnel(res, xlab="Effect Size (d per year)", ylab="Standard Error", main="Antagonistic Narcissism") +
  mtext("B", side = 3, line = 2.4, adj = -0.16, cex = 1.0)
res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.antagonistic.nonclin, mods = cbind(pubstatus))
print(res, digits=3)
table(dat.antagonistic.nonclin.es_dyear$pubstatus)

res <- rma(yi=es_dyear, vi=v_dyear, data=dat.neurotic.nonclin)
print(res, digits=3)
regtest(res)
funnel(res, xlab="Effect Size (d per year)", ylab="Standard Error", main="Neurotic Narcissism") +
  mtext("C", side = 3, line = 2.4, adj = -0.16, cex = 1.0)
res <- rma.mv(yi=es_dyear, V=v_dyear, random = ~ 1 | sampid/caseid, data=dat.neurotic.nonclin, mods = cbind(pubstatus))
print(res, digits=3)
table(dat.neurotic.nonclin.es_dyear$pubstatus)

fig2 <- recordPlot()
postscript(file='fig2.eps', width=4, height=9, paper="special", horizontal=F)
fig2
dev.off()


#### RANK-ORDER STABILITY ####

#### Effect Size Analyses ####

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.agentic)
print(res, digits=3)
sum(dat.agentic.corr.sample$n)
fisherz2r(0.997)
fisherz2r(0.875)
fisherz2r(1.119)

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.antagonistic)
print(res, digits=3)
sum(dat.antagonistic.corr.sample$n)
fisherz2r(0.842)
fisherz2r(0.646)
fisherz2r(1.039)

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.neurotic)
print(res, digits=3)
sum(dat.neurotic.corr.sample$n)
fisherz2r(0.695)
fisherz2r(0.463)
fisherz2r(0.926)


#### Age Effects ####

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.agentic, mods = cbind(agem1ic1))
print(res, digits=4)

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.agentic, mods = cbind(agem1ic1, agem1ic2))
print(res, digits=4)

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.agentic, mods = cbind(agem1ic1, agem1ic2, agem1ic3))
print(res, digits=4)

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.antagonistic, mods = cbind(agem1ic1))
print(res, digits=4)

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.antagonistic, mods = cbind(agem1ic1, agem1ic2))
print(res, digits=4)

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.antagonistic, mods = cbind(agem1ic1, agem1ic2, agem1ic3))
print(res, digits=4)

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.neurotic, mods = cbind(agem1ic1))
print(res, digits=4)

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.neurotic, mods = cbind(agem1ic1, agem1ic2))
print(res, digits=4)

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.neurotic, mods = cbind(agem1ic1, agem1ic2, agem1ic3))
print(res, digits=4)


#### Moderator Analyses ####

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.agentic, mods = cbind(female, yob, clinical, timelagc))
print(res, digits=4)

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.antagonistic, mods = cbind(female, yob, clinical, timelagc))
print(res, digits=4)

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.neurotic, mods = cbind(female, yob, clinical, timelagc))
print(res, digits=4)


#### Effect Size Analyses Controlled for Time Lag ####

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.agentic, mods = cbind(timelagc))
print(res, digits=3)
sum(dat.agentic.corr.sample$n)
fisherz2r(0.935)
fisherz2r(0.805)
fisherz2r(1.064)

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.antagonistic, mods = cbind(timelagc))
print(res, digits=3)
sum(dat.antagonistic.corr.sample$n)
fisherz2r(0.827)
fisherz2r(0.632)
fisherz2r(1.022)

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.neurotic, mods = cbind(timelagc))
print(res, digits=3)
sum(dat.neurotic.corr.sample$n)
fisherz2r(0.699)
fisherz2r(0.512)
fisherz2r(0.886)


#### Age Effects Controlled for Time Lag ####

cor.test(dat$agem1i, dat$timelag)

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.agentic, mods = cbind(agem1ic1, timelagc))
print(res, digits=4)

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.agentic, mods = cbind(agem1ic1, agem1ic2, timelagc))
print(res, digits=4)

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.agentic, mods = cbind(agem1ic1, agem1ic2, agem1ic3, timelagc))
print(res, digits=4)

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.antagonistic, mods = cbind(agem1ic1, timelagc))
print(res, digits=4)

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.antagonistic, mods = cbind(agem1ic1, agem1ic2, timelagc))
print(res, digits=4)

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.antagonistic, mods = cbind(agem1ic1, agem1ic2, agem1ic3, timelagc))
print(res, digits=4)

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.neurotic, mods = cbind(agem1ic1, timelagc))
print(res, digits=4)

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.neurotic, mods = cbind(agem1ic1, agem1ic2, timelagc))
print(res, digits=4)

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.neurotic, mods = cbind(agem1ic1, agem1ic2, agem1ic3, timelagc))
print(res, digits=4)


#### Replication of Effect Size Analyses with Reduced Set of Measures ####

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.agentic2)
print(res, digits=3)
sum(dat.agentic2.corr.sample$n)
fisherz2r(0.850)
fisherz2r(0.776)
fisherz2r(0.925)

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.antagonistic2)
print(res, digits=3)
sum(dat.antagonistic2.corr.sample$n)
fisherz2r(0.809)
fisherz2r(0.627)
fisherz2r(0.990)

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.neurotic2)
print(res, digits=3)
sum(dat.neurotic2.corr.sample$n)
fisherz2r(1.040)
fisherz2r(0.854)
fisherz2r(1.226)

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.agentic2, mods = cbind(timelagc))
print(res, digits=3)
sum(dat.agentic2.corr.sample$n)
fisherz2r(0.828)
fisherz2r(0.748)
fisherz2r(0.907)

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.antagonistic2, mods = cbind(timelagc))
print(res, digits=3)
sum(dat.antagonistic2.corr.sample$n)
fisherz2r(0.795)
fisherz2r(0.625)
fisherz2r(0.966)

res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.neurotic2, mods = cbind(timelagc))
print(res, digits=3)
sum(dat.neurotic2.corr.sample$n)
fisherz2r(1.040)
fisherz2r(0.854)
fisherz2r(1.226)


#### Nonlinear Regression ####

nls <- nls(corrdis~a+(1-a)*exp(-b*timelag), data=dat.agentic, start=list(a=0.5, b=1.0), trace = TRUE)
summary(nls)
datnls <- subset(dat.agentic, subset = corrdis != "NA")
datnls$prednls = predict(nls)
fig3a <- ggplot(datnls, aes(x = timelag, y = corrdis)) + geom_point() + geom_line(aes(y = prednls), linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 70, 10), lim = c(0, 70), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), lim = c(0, 1), expand = c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
  axis.line = element_line(colour = "black")) + labs(tag = "A", title = "Agentic Narcissism", x = "Time Lag", y = "Rank-Order Stability")

nls <- nls(corrdis~a+(1-a)*exp(-b*timelag), data=dat.antagonistic, start=list(a=0.5, b=1.0), trace = TRUE)
summary(nls)
datnls <- subset(dat.antagonistic, subset = corrdis != "NA")
datnls$prednls = predict(nls)
fig3b <- ggplot(datnls, aes(x = timelag, y = corrdis)) + geom_point() + geom_line(aes(y = prednls), linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 70, 10), lim = c(0, 70), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), lim = c(0, 1), expand = c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
  axis.line = element_line(colour = "black")) + labs(tag = "B", title = "Antagonistic Narcissism", x = "Time Lag", y = "Rank-Order Stability")

nls <- nls(corrdis~a+(1-a)*exp(-b*timelag), data=dat.neurotic, start=list(a=0.5, b=1.0), trace = TRUE)
summary(nls)
datnls <- subset(dat.neurotic, subset = corrdis != "NA")
datnls$prednls = predict(nls)
fig3c <- ggplot(datnls, aes(x = timelag, y = corrdis)) + geom_point() + geom_line(aes(y = prednls), linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 70, 10), lim = c(0, 70), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), lim = c(0, 1), expand = c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
  axis.line = element_line(colour = "black")) + labs(tag = "C", title = "Neurotic Narcissism", x = "Time Lag", y = "Rank-Order Stability")

postscript(file='fig3.eps', width=4.4, height=10, paper="special", horizontal=F)
fig3a + fig3b + fig3c + plot_layout(ncol = 1)
dev.off()


#### Tests of Publication Bias ####

par(mfrow = c(3, 1))

res <- rma(yi=corrdis_z, vi=v_corrdis, data=dat.agentic)
print(res, digits=3)
regtest(res)
funnel(res, xlab="Effect Size (Fisher's z Value)", ylab="Standard Error", main="Agentic Narcissism") +
  mtext("A", side = 3, line = 2.4, adj = -0.16, cex = 1.0)
res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.agentic, mods = cbind(pubstatus))
print(res, digits=3)
table(dat.agentic.corr$pubstatus)

res <- rma(yi=corrdis_z, vi=v_corrdis, data=dat.antagonistic)
print(res, digits=3)
regtest(res)
funnel(res, xlab="Effect Size (Fisher's z Value)", ylab="Standard Error", main="Antagonistic Narcissism") +
  mtext("B", side = 3, line = 2.4, adj = -0.16, cex = 1.0)
res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.antagonistic, mods = cbind(pubstatus))
print(res, digits=3)
table(dat.antagonistic.corr$pubstatus)

res <- rma(yi=corrdis_z, vi=v_corrdis, data=dat.neurotic)
print(res, digits=3)
regtest(res)
funnel(res, xlab="Effect Size (Fisher's z Value)", ylab="Standard Error", main="Neurotic Narcissism") +
  mtext("C", side = 3, line = 2.4, adj = -0.16, cex = 1.0)
res <- rma.mv(yi=corrdis_z, V=v_corrdis, random = ~ 1 | sampid/caseid, data=dat.neurotic, mods = cbind(pubstatus))
print(res, digits=3)
table(dat.neurotic.corr$pubstatus)

fig4 <- recordPlot()
postscript(file='fig4.eps', width=4, height=9, paper="special", horizontal=F)
fig4
dev.off()