
setwd("C:/Users/jakub/Desktop/phd/badanie Izrael")

install.packages("readxl")
install.packages("Hmisc")

######################## preparing the real-life HELPING QUESTION ######################## 

library(readxl)
odpIZR <- read_excel("beh.xlsx")
odpIZR
hist(odpIZR$beh)
hist(odpIZR$beh_max)

odpIZR_beh <- read_excel("beh_row.xlsx")
odpIZR_beh
hist(odpIZR_beh$behavioural)

library(ggplot2)
library(reshape2)
library(MASS)
library(Hmisc)
library(foreign)

head(odpIZR_beh)
lapply(odpIZR_beh[, c("behavioural")], table)
table(odpIZR_beh$behavioural)

odpIZR_beh$behavioural
levels(odpIZR_beh$behavioural)
install.packages("dplyr")
library(dplyr)
odpIZR_beh %>% count(behavioural)


######################## DATA ANALYSIS ######################## 

library(readxl)
prereg <- read_excel("preregistration.xlsx")

library(haven)
prereg
write_sav(prereg, "prereg.sav", compress = FALSE)

library(foreign)

######## the results of the outliers

person19 <- prereg[19, c("HE_comp", "NEU_comp", "HE_neg", "NEU_neg", "CLS", "IRI_EC", "hypothetical", "iwah_humanity", "Vladimirs", "beh_sum", 
                         "beh_lower", "beh_upper", "conf_nations_neigh", "conf_isr")]
person19
write_xlsx(person19,"C:/Users/jakub/Desktop/phd/badanie Izrael\\person19.xlsx")

######## the results without the person #19

prereg <- prereg[-19,]
prereg[19,"HE_comp"]

########  create a new column with substracted value

prereg$neg <- (prereg$HE_neg - prereg$NEU_neg)

######## checking for normality across the results:

shapiro.test(prereg$HE_comp)
shapiro.test(prereg$HE_neg)
shapiro.test(prereg$NEU_comp)
shapiro.test(prereg$NEU_neg)
shapiro.test(prereg$neg_emo)

shapiro.test(prereg$IRI_FS)
shapiro.test(prereg$IRI_PT)
shapiro.test(prereg$IRI_PD)
shapiro.test(prereg$IRI_EC)

shapiro.test(prereg$conf_isr)
shapiro.test(prereg$conf_nations_neigh)
shapiro.test(prereg$conlict)

shapiro.test(prereg$hypothetical)
shapiro.test(prereg$CLS)

shapiro.test(prereg$iwah_community)
shapiro.test(prereg$iwah_israelis)
shapiro.test(prereg$iwah_humanity)

shapiro.test(prereg$beh_contin)

######## histograms

ggplot(prereg, aes(x=HE_comp)) + geom_histogram()
ggplot(prereg, aes(x=HE_neg)) + geom_histogram()
ggplot(prereg, aes(x=NEU_comp)) + geom_histogram()
ggplot(prereg, aes(x=NEU_neg)) + geom_histogram()

ggplot(prereg, aes(x=IRI_FS)) + geom_histogram()
ggplot(prereg, aes(x=IRI_PT)) + geom_histogram()
ggplot(prereg, aes(x=IRI_PD)) + geom_histogram()
ggplot(prereg, aes(x=IRI_EC)) + geom_histogram()

ggplot(prereg, aes(x=conf_isr)) + geom_histogram()
ggplot(prereg, aes(x=conf_nations_neigh)) + geom_histogram()
ggplot(prereg, aes(x=conlict)) + geom_histogram()

ggplot(prereg, aes(x=hypothetical)) + geom_histogram()
ggplot(prereg, aes(x=CLS)) + geom_histogram()

ggplot(prereg, aes(x=iwah_community)) + geom_histogram()
ggplot(prereg, aes(x=iwah_israelis)) + geom_histogram()
ggplot(prereg, aes(x=iwah_humanity)) + geom_histogram()

ggplot(prereg, aes(x=beh_sum)) + geom_histogram()
ggplot(prereg, aes(x=beh_upper)) + geom_histogram()
ggplot(prereg, aes(x=beh_lower)) + geom_histogram()

ggplot(prereg, aes(x=neg_emo)) + geom_histogram()


######################### H1 t test ################################

t.test(x=prereg$HE_comp, y=prereg$NEU_comp, alternative="two.sided", paired=T)
t.test(x=prereg$HE_neg, y=prereg$NEU_neg, alternative="two.sided", paired=T)


######################### H 2 SoVT negative emotions vs. PD ######################### 

library(Hmisc)

#create a new column with substracted value
prereg$neg_emo <- (prereg$HE_neg - prereg$NEU_neg)
shapiro.test(prereg$neg_emo)
macierz_H2 <- prereg[, c("neg_emo", "IRI_PD")]
macierz_H2$neg_emo <- as.numeric(macierz_H2$neg_emo)
macierz_H2$IRI_PD <- as.numeric(macierz_H2$IRI_PD)

cor(macierz_H2$neg_emo, macierz_H2$IRI_PD, use ="pairwise.complete.obs", method="pearson")

macierz_H2.cor = cor(macierz_H2, method = c("pearson"))
macierz_H2.cor

macierz_H2.rcorr = rcorr(as.matrix(macierz_H2))
macierz_H2.rcorr


############################ H3 SoVT & compassion $ helping beh ######################### 

prereg$comp <- (prereg$HE_comp - prereg$NEU_comp)

shapiro.test(prereg$comp)

macierz_H3 <- prereg[, c("HE_comp", "CLS", "IRI_EC", "hypothetical", "beh_sum", "beh_upper", "beh_lower", "beh_weight", 
                         "beh_binary", "beh_contin")]


cor(macierz_H3$comp, macierz_H3$IRI_EC, use ="pairwise.complete.obs", method="pearson")

macierz_H3.cor = cor(macierz_H3, method = c("spearman"))
macierz_H3.cor

macierz_H3.rcorr = rcorr(as.matrix(macierz_H3))
macierz_H3.rcorr


### compassion rating as compassion in emotional condition of SoVT only

macierz_H3b <- prereg[, c("HE_comp", "CLS", "IRI_EC", "hypothetical", "iwah_humanity", "Vladimirs", "beh_sum", "beh_lower", "beh_upper", "conf_nations_neigh", "conf_isr")]


cor(macierz_H3b$HE_comp, macierz_H3b$IRI_EC, use ="pairwise.complete.obs", method="spearman")

macierz_H3b.cor = cor(macierz_H3b, method = c("spearman"))
macierz_H3b.cor

macierz_H3b.rcorr = rcorr(as.matrix(macierz_H3b))
macierz_H3b.rcorr


######################### HYPOTHESIS 4: SoVT COMPASSION and intergroup measures: ######################### 

macierz_H4 <- prereg[, c("HE_comp", "CLS", "IRI_EC", "conf_nations_neigh", "conf_isr", "conlict", "iwah_humanity",
                         "iwah_israelis", "iwah_community", "Vladimirs")]
macierz_H4.cor = cor(macierz_H4, method = c("spearman"))
macierz_H4.cor

macierz_H4.rcorr = rcorr(as.matrix(macierz_H4))
macierz_H4.rcorr 

mcor<-round(rcorr(macierz_H4),2)
lower<-mcor
lower[lower.tri(mcor, diag=TRUE)]<-""
lower<-as.data.frame(lower)
lower
library(xtable)
mcor <- as.data.frame(mcor)

######################### HYPOTHESIS 4: SoVT COMPASSION and conflict scale (only the Israeli-Jewish participants) ######################### 

macierz_H4conf <- prereg[which(prereg$nationality=="ISR"), c("HE_comp", "CLS", "IRI_EC", "conf_nations_neigh", "conf_isr", "conlict", "iwah_humanity",
                                                             "iwah_israelis", "iwah_community", "Vladimirs")]
macierz_H4conf.rcorr = rcorr(as.matrix(macierz_H4conf))
macierz_H4conf.rcorr 

##### correlations with beh questions

cor(macierz_H3b$HE_comp, macierz_H3b$beh_sum, use ="pairwise.complete.obs", method="spearman")
cor(macierz_H3b$HE_comp, macierz_H3b$beh_upper, use ="pairwise.complete.obs", method="spearman")
cor(macierz_H3b$HE_comp, macierz_H3b$beh_lower, use ="pairwise.complete.obs", method="spearman")

####### scatter plots for the correlations:

library(ggplot2)
# Basic scatter plot
ggplot(macierz_H3b, aes(x=HE_comp, y=iwah_humanity)) + geom_point() +geom_smooth()

####### descriptive stats:

summary(prereg$beh_contin)
sd(prereg$beh_contin)
res_sd = na.omit(prereg$beh_contin)
sd(res_sd)


######################### HHYPOTHESIS 5a: regressions ######################### 

###################### Hypothetical helping questions
############## one predictor model

onepred <- lm(hypothetical ~ IRI_EC, prereg)
summary(onepred)

############# two predictors: IRI_EC and compassion (SoVT)

twopred <- lm(hypothetical ~ IRI_EC + HE_comp, prereg)
summary(twopred)

########## compare the models
anova(onepred)
anova(twopred)

############# three predictors: CLS, IRI_EC and compassion (SoVT)

threepred <- lm(hypothetical ~ IRI_EC + CLS + HE_comp, prereg)
summary(threepred)
anova(threepred)

############ checking the assumptions of linear regression:

######## homoscedasticity
par(mfrow=c(2,2))
plot(threepred)

library(car)
ncvTest(threepred)

####### independent errors:
library(lmtest)
dwtest(threepred)

####### normally distributed errors:
threepred.stdres = rstandard(threepred)
qqnorm(threepred.stdres)
qqline(threepred.stdres)

######## homoscedasticity for the two predictors model
par(mfrow=c(2,2))
plot(twopred)
ncvTest(twopred)

######## independent errors:
dwtest(twopred)

######################################## ordinal log regression

require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)

prereg$beh_upperf <- as.factor(prereg$beh_upper)

m <- polr(beh_upperf ~ IRI_EC, data = prereg, Hess=TRUE)
summary(m)
summary_table <- coef(summary(m))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table

############## ordinal log regression with 2 predictors:
m <- polr(beh_upperf ~ IRI_EC + HE_comp, data = prereg, Hess=TRUE)
summary(m)
summary_table <- coef(summary(m))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table

###########calculating the cumulative odds:
########I need to take away the person #14 (because the variable lengths differ)

prereg <- prereg[-14,]

sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 0)),
    'Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)),
    'Y>=4' = qlogis(mean(y >= 4)))
}

s <- with(prereg, summary(as.numeric(beh_upperf) ~ na.omit(IRI_EC) + na.omit(HE_comp), fun=sf))
s
xtabs(~ beh_upperf + HE_comp, data=prereg)

######################### HHYPOTHESIS 5b: regressions & PERSONAL DISTRESS (EMPATHY) ######################### 
###################### Hypothetical helping questions
############## one predictor model

onepred <- lm(hypothetical ~ IRI_PD, prereg)
summary(onepred)

############# two predictors: IRI_EC and compassion (SoVT)

twopred <- lm(hypothetical ~ IRI_PD + neg, prereg)
summary(twopred)

########## compare the models
anova(onepred)
anova(twopred)


############ checking the assumptions of linear regression:

######## homoscedasticity
par(mfrow=c(2,2))
plot(onepred)
plot(twopred)

library(car)
ncvTest(onepred)
ncvTest(twopred)

####### independent errors:
library(lmtest)
dwtest(onepred)
dwtest(twopred)

####### normally distributed errors:
onepred.stdres = rstandard(onepred)
qqnorm(onepred.stdres)
qqline(onepred.stdres)

twopred.stdres = rstandard(twopred)
qqnorm(twopred.stdres)
qqline(twopred.stdres)

######## homoscedasticity for the two predictors model
par(mfrow=c(2,2))
plot(twopred)
ncvTest(twopred)

######## independent errors:
dwtest(twopred)

######################################## ordinal log regression

require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)

prereg$beh_upperf <- as.factor(prereg$beh_upper)

m <- polr(beh_upperf ~ neg, data = prereg, Hess=TRUE)
summary(m)
summary_table <- coef(summary(m))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table

############## ordinal log regression with 2 predictors:
m <- polr(beh_upperf ~ IRI_PD + neg, data = prereg, Hess=TRUE)
summary(m)
summary_table <- coef(summary(m))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table

########### calculating the cumulative odds:

sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 0)),
    'Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)),
    'Y>=4' = qlogis(mean(y >= 4)))
}

s <- with(prereg, summary(as.numeric(beh_upperf) ~ na.omit(IRI_PD) + na.omit(neg), fun=sf))
s
xtabs(~ beh_upperf + HE_comp, data=prereg)

