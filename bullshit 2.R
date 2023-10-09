#############################################################
# study 2 - bullshit receptivity (postmodernist statements)
#############################################################


# packages list
pckgs <- c(
  "lme4",
  "lmerTest",
  "ggpubr",
  "effsize",
  "car",
  # "reghelper",
  "see",
  # "EMAtools",
  "GGally",
  "ggcorrplot",
  "interactions",
  "Rmisc",
  "emmeans",
  "effectsize",
  "performance",
  # "patchwork",
  "tidyverse"
)

#check installation and load
for (pckg in pckgs) {
  #check installation
  if (
    !(pckg %in% installed.packages())
  ) {
    install.packages(pckg)
  }
  #load packages
  lapply(pckg, library, character.only = T)
}

#load data
bs2Data <- read.csv("data files/bs fluency 2 (pomo).csv")

#get demographics
mean(bs2Data$age, na.rm = T); sd(bs2Data$age, na.rm = T)

sex <- bs2Data %>%
  select(sex) %>%
  plyr::count() %>%
  mutate(prop = 100 * freq/sum(freq)) %>%
  mutate(sex = case_when(
    sex == 1 ~ "male",
    sex == 2 ~ "female", 
    sex == 3 ~ "other",
    TRUE ~ "did not identify"
  ))
sex

race <- bs2Data %>%
  select(race) %>%
  plyr::count() %>%
  mutate(prop = 100 * freq/sum(freq)) %>%
  mutate(race = case_when(
    race == 1 ~ "asian",
    race == 2 ~ "black",
    race == 3 ~ "latino/a",
    race == 4 ~ "white",
    race == 5 ~ "native american",
    race == 6 ~ "pacific islander",
    race == 7 ~ "other",
    TRUE ~ "did not identify"
  ))
race

device <- bs2Data %>%
  select(device) %>%
  plyr::count() %>%
  mutate(prop = 100 * freq/sum(freq)) %>%
  mutate(device = case_when(
    device == 1 ~ "laptop",
    device == 2 ~ "tablet",
    device == 3 ~ "phone",
    device == 4 ~ "desktop",
    device == 5 ~ "other",
    TRUE ~ "did not identify"
  ))
device


#clean up CRT responses
bs2Data$CRT1 <- with(bs2Data,
                     case_when(
                       CRT1 == .05 ~ 1,
                       CRT1 == 5 ~ 1,
                       CRT1 == "5 cents" ~ 1,
                       TRUE ~ 0
                     ))

bs2Data$CRT2 <- with(bs2Data,
                     case_when(
                       CRT2 == 5 ~ 1,
                       CRT2 == "5 minutes" ~ 1,
                       CRT2 == "5minutes" ~ 1,
                       TRUE ~ 0
                     ))

bs2Data$CRT3 <- with(bs2Data,
                     case_when(
                       CRT3 == 47 ~ 1,
                       CRT3 == "47 days" ~ 1,
                       TRUE ~ 0
                     ))

bs2Data$CRT4 <- with(bs2Data,
                     case_when(
                       CRT4 == 29 ~ 1,
                       TRUE ~ 0
                     ))


#reverse code NFC responses (numbers 3,4,5,7,8,9,12,16,17)
bs2Data$NFC03 <- 6 - bs2Data$NFC03
bs2Data$NFC04 <- 6 - bs2Data$NFC04
bs2Data$NFC05 <- 6 - bs2Data$NFC05
bs2Data$NFC07 <- 6 - bs2Data$NFC07
bs2Data$NFC08 <- 6 - bs2Data$NFC08
bs2Data$NFC09 <- 6 - bs2Data$NFC09
bs2Data$NFC12 <- 6 - bs2Data$NFC12
bs2Data$NFC16 <- 6 - bs2Data$NFC16
bs2Data$NFC17 <- 6 - bs2Data$NFC17

#get alpha values of scales
psych::alpha(bs2Data[,c(170:173)])
psych::alpha(bs2Data[,c(174:191)])
psych::alpha(bs2Data[,c(192:202)])
psych::alpha(bs2Data[,c(203:227)])

#average responses together
bs2Data$crtMean <- rowMeans(bs2Data[,c(170:173)], na.rm = T)
bs2Data$nfcMean <- rowMeans(bs2Data[,c(174:191)], na.rm = T)
bs2Data$fiMean <- rowMeans(bs2Data[,c(192:202)], na.rm = T)
bs2Data$sbqMean <- rowMeans(bs2Data[,c(203:227)], na.rm = T)

#check duplicated IP addresses and IDs
bs2Data$dupIP <- duplicated(bs2Data$IPAddress); plyr::count(bs2Data$dupIP)
bs2Data$dupID <- duplicated(bs2Data$ResponseId); plyr::count(bs2Data$dupID)

#remove
bs2Data <- filter(bs2Data, dupIP == FALSE & dupID == FALSE)

#add subject number
bs2Data$subj <- as.factor(rep(1:nrow(bs2Data)))
bs2Data$block <- as.factor(bs2Data$block)

#split individual difference measures to check correlations
invDiffs <- bs2Data[,c(238,232:235)]

Hmisc::rcorr(as.matrix(invDiffs[,c(2:5)]))

#reshape
b1 <- bs2Data[,c(238,9,232:235,10:89)] %>%
  filter(block == 1) %>%
  gather(key = "stimID", value = "rating",
         "stim1profflu":"stim20belvdis") %>%
  separate(col = "stimID", into = c("stimID", "fluency"), sep = -3) %>%
  separate(col = "stimID", into = c("stimID", "rateCat"), sep = -4) %>%
  pivot_wider(names_from = "rateCat", values_from = "rating") %>%
  mutate(fluency = case_when(
    fluency == "flu" ~ "Fluent",
    fluency == "dis" ~ "Disfluent"
  )) %>%
  mutate(fluC = case_when(
    fluency == "Fluent" ~ 1,
    fluency == "Disfluent" ~ -1
  )) %>%
  mutate(crtC = scale(crtMean, scale = F)) %>%
  mutate(nfcC = scale(nfcMean, scale = F)) %>%
  mutate(fiC = scale(fiMean, scale = F)) %>%
  mutate(sbqC = scale(sbqMean, scale = F)) 

b2 <- bs2Data[,c(238,9,232:235,90:169)] %>%
  filter(block == 2) %>%
  gather(key = "stimID", value = "rating",
         "stim11profflu":"stim10belvdis") %>%
  separate(col = "stimID", into = c("stimID", "fluency"), sep = -3) %>%
  separate(col = "stimID", into = c("stimID", "rateCat"), sep = -4) %>%
  pivot_wider(names_from = "rateCat", values_from = "rating") %>%
  mutate(fluency = case_when(
    fluency == "flu" ~ "Fluent",
    fluency == "dis" ~ "Disfluent"
  )) %>%
  mutate(fluC = case_when(
    fluency == "Fluent" ~ 1,
    fluency == "Disfluent" ~ -1
  )) %>%
  mutate(crtC = scale(crtMean, scale = F)) %>%
  mutate(nfcC = scale(nfcMean, scale = F)) %>%
  mutate(fiC = scale(fiMean, scale = F)) %>%
  mutate(sbqC = scale(sbqMean, scale = F))

allData <- rbind(b1, b2)

names(allData)[9:12] <- c("profound", "meaningful", "understandable", "believable")


#check for response nonvariance for all ratings
varCheckResp <- allData %>%
  dplyr::group_by(subj) %>%
  dplyr::summarize(
    sdPro = sd(profound, na.rm = T),
    sdMean = sd(meaningful, na.rm =T),
    sdUnder = sd(understandable, na.rm = T),
    sdBelv = sd(believable, na.rm = T)
  ) %>%
  as.data.frame

filter(varCheckResp, sdPro == 0 | sdMean == 0 | sdUnder == 0 | sdBelv == 0)

allBS <- filter(allData, subj != 66)


#correlate ratings
bsRates <- allBS %>%
  dplyr::group_by(subj) %>%
  dplyr::summarize(
    mProf = mean(profound, na.rm = T),
    mMean = mean(meaningful, na.rm = T),
    mUnder = mean(understandable, na.rm = T),
    mBelv = mean(believable, na.rm = T),
    
    mCrt = mean(crtC),
    mNfc = mean(nfcC),
    mFi = mean(fiC),
    mSbq = mean(sbqC)
    
  ) %>%
  as.data.frame

bsCors <- Hmisc::rcorr(as.matrix(bsRates[,2:5]))
bsCors

ggcorrplot(bsCors[[1]], 
           type = "lower",
           lab = T,
           p.mat = bsCors[[3]])

psych::alpha(bsRates[,2:5])

ggpairs(bsRates[,2:5]) + theme_classic()


### explore the correlations at differing levels of fluency
corSums2 <- allBS %>%
  dplyr::group_by(subj, fluency) %>%
  dplyr::summarize(
    mProfound = mean(profound, na.rm = T),
    mMeaning = mean(meaningful, na.rm = T),
    mUnderstand = mean(understandable, na.rm = T),
    mBelieve = mean(believable, na.rm = T),
    mCrt = mean(crtC),
    mNfc = mean(nfcC),
    mFi = mean(fiC),
    mSbq = mean(sbqC)
  ) %>%
  as.data.frame

corSumsFlu <- filter(corSums2, fluency == "Fluent")
corsFlu <- Hmisc::rcorr(as.matrix(corSumsFlu[3:10]))
corsFlu

corSumsDis <- filter(corSums2, fluency == "Disfluent")
corsDis <- Hmisc::rcorr(as.matrix(corSumsDis[3:10]))
corsDis


#correlate individual differences
invCors <- Hmisc::rcorr(as.matrix(bsRates[,6:9]))
invCors

ggcorrplot(invCors[[1]],
           type = "lower",
           lab = T,
           p.mat = invCors[[3]])

ggpairs(bsRates[,6:9]) + theme_classic()


#include all DVs & individual differences
allCors <- Hmisc::rcorr(as.matrix(bsRates[,2:9]))
allCors

ggcorrplot(allCors[[1]],
           type = "lower",
           lab = T,
           p.mat = allCors[[3]])

ggpairs(bsRates[,2:9]) + theme_classic()


#get alpha of the four items
psych::alpha(allBS[,9:12])

#average bs ratings into index
allBS$bsr <- rowMeans(allBS[,9:12], na.rm = T)


#model fluency alone
mod1 <- lmer(bsr ~ fluC + (fluC|subj) + (fluC|stimID), data = allBS)
(sm1 <- summary(mod1))
t_to_eta2(t = sm1$coefficients[7:8], df_error = sm1$coefficients[5:6], paired = T)
t_to_d(t = sm1$coefficients[7:8], df_error = sm1$coefficients[5:6], paired = T)
icc(mod1)
r2(mod1)


#model fluency with individual differences
mod2 <- lmer(bsr ~ fluC + crtC + nfcC + fiC + sbqC + (fluC|subj) + (0 + fluC|stimID), data = allBS)
(sm2 <- summary(mod2))
t_to_eta2(t = sm2$coefficients[19:24], df_error = sm2$coefficients[13:18], paired = T)
t_to_d(t = sm2$coefficients[19:24], df_error = sm2$coefficients[13:18], paired = T)
icc(mod2)
r2(mod2)


#model fluency interactions
mod3 <- lmer(bsr ~ fluC + crtC + nfcC + fiC + sbqC + fluC:crtC + fluC:nfcC + fluC:fiC + fluC:sbqC + (fluC|subj) + (0 + fluC|stimID), data = allBS)
(sm3 <- summary(mod3))
#estimate confidence intervals
se3 <- sqrt(diag(vcov(mod3)))
(m3CI <- cbind(est = fixef(mod3), ll = fixef(mod3) - (1.96*se3), ul = fixef(mod3) + (1.96*se3)))
#estimate effect sizes (d and eta^2)
t_to_d(t = sm3$coefficients[31:40], df_error = sm3$coefficients[21:30], paired = T)
t_to_eta2(t = sm3$coefficients[31:40], df_error = sm3$coefficients[21:30], paired = T)
#model fit indices
icc(mod3)
r2(mod3)

#compare models
anova(mod1, mod2, mod3)

#check model fit
check_model(mod3)


#interaction plot
(crtInt <- interact_plot(mod3, 
                         pred = "fluC", 
                         modx = "crtC", 
                         data = allBS,
                         interval = T,
                         int.type = "confidence",
                         colors = "red", 
                         y.label = "Bullshit Receptivity", 
                         x.label = "", 
                         legend.main = "CRT Tertiles",
                         # main.title = "CRT x Fluency",
                         pred.labels = c("Disfluent", "Fluent")) + 
    theme_classic(base_size = 15) + 
    theme(legend.position = "bottom"))

# ggsave("crt interaction - study 2.jpg", device = "jpeg", path = "plots", units = "cm")

#simple slopes

#crt:fluency
ssCrt <- simple_slopes(mod3, levels = "crtC")
ssCrt
#estimate cis
ciSSCrt <- cbind(Est = ssCrt$`Test Estimate`, LL = ssCrt$`Test Estimate` - (1.96*ssCrt$`Std. Error`), UL = ssCrt$`Test Estimate` + (1.96*ssCrt$`Std. Error`))
ciSSCrt
#effect sizes
t_to_d(t = ssCrt$`t value`, df_error = ssCrt$df, paired = T)
t_to_eta2(t = ssCrt$`t value`, df_error = ssCrt$df, paired = T)

johnson_neyman(mod3, pred = "fluC", modx = "crtC")


#summarize the effect of fluency
fluencySum <- Rmisc::summarySE(allBS,
                               measurevar = "bsr",
                               groupvars = "fluency",
                               na.rm = T)
fluencySum

fluencyP <- Rmisc::summarySE(allBS,
                             measurevar = "bsr",
                             groupvars = c("subj", "fluency"),
                             na.rm = T)

#bar plot
ggplot(fluencySum, aes(fluency, bsr)) +
  geom_bar(stat = "identity", fill = "turquoise4", color = "black", alpha = .8) +
  geom_errorbar(aes(ymin = bsr - ci, ymax = bsr + ci), 
                width = .25, alpha = .6) +
  geom_point(data = fluencyP, aes(fluency, bsr),
             alpha = .25, color = "turquoise4", size = .75, position = position_jitter(.15, .15)) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = c(0, 1, 2, 3, 4, 5, 6, 7)) +
  scale_x_discrete(labels = c("Disfluent Statements", "Fluent Statements")) + 
  expand_limits(y = 5.7) +
  theme_classic(base_size = 15) + 
  xlab("") + 
  ylab("Bullshit Receptivity") + 
  geom_segment(x = 1, xend = 2, y = 5.4, yend = 5.4) + 
  geom_segment(x = 1, xend = 1, y = 5.4, yend = 5.3) +
  geom_segment(x = 2, xend = 2, y = 5.4, yend = 5.3) +
  annotate(geom = "text", x = 1.5, y = 5.5, label = "***", size = 5)

# ggsave("study 2 - fluency plot.jpg", device = "jpeg", path = "plots", units = "cm")


#plot for 6cc talk
ggplot(fluencyP, aes(fluency, bsr)) +
  geom_violin(fill = 'turquoise4', color = 'black', alpha = .4) +
  geom_point(color = 'turquoise4', alpha = .2, position = position_jitter(.2, .05), shape = 7) +
  geom_point(data = fluencySum, aes(fluency, bsr),
             color = 'gray20', position = position_dodge(.9), size = 2) +
  geom_errorbar(data = fluencySum, aes(fluency, bsr, ymin = bsr - ci, ymax = bsr + ci),
                width = .1, alpha = .5, position = position_dodge(.9)) +
  labs(x = '',
       y = 'Bullshit Receptivity') +
  scale_x_discrete(labels = c('Disfluent\nStatements', 'Fluent\nStatements')) +
  scale_y_continuous(limits = c(1,7),
                     breaks = seq(1,7,1)) +
  geom_segment(x = 1, xend = 2, y = 5.4, yend = 5.4) + 
  geom_segment(x = 1, xend = 1, y = 5.4, yend = 5.3) +
  geom_segment(x = 2, xend = 2, y = 5.4, yend = 5.3) +
  annotate(geom = "text", x = 1.5, y = 5.5, label = "***", size = 5) +
  theme_classic(base_size = 15)

# ggsave('study 2 fluency (6cc talk).jpg', device = 'jpeg', units = 'cm', path = 'plots')


# sumBS <- Rmisc::summarySE(data = allBS,
#                           measurevar = "bsr",
#                           groupvar = "fluency",
#                           na.rm = T)
# 
# #raincloud plot
# ggplot(allBS, aes(fluency, bsr)) +
#   geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .7, color = "black", fill = "aquamarine2") +
#   geom_point(position = position_jitter(width = .15, height = .15), size = .5,  alpha = .7, color = "aquamarine2") +
#   geom_boxplot(width = .1, outlier.shape = NA, alpha = .5, color = "black", fill = "aquamarine2") +
#   geom_point(data = sumBS, aes(x = fluency, y = bsr), size = 2, color = "black") +
#   geom_errorbar(data = sumBS, aes(x = fluency, y = bsr, ymin = bsr - (1.96*se), ymax = bsr + (1.96*se)), width = .15, alpha = .6) +
#   expand_limits(x = 3, y = 7.7) + theme_bw() + xlab(NULL) + ylab("Bullshit Receptivity") +
#   theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
#   geom_segment(x = 1, xend = 2, y = 7.4, yend = 7.4) + 
#   geom_segment(x = 1, xend = 1, y = 7.4, yend = 7.3) +
#   geom_segment(x = 2, xend = 2, y = 7.4, yend = 7.3) +
#   annotate(geom = "text", x = 1.5, y = 7.6, label = "***") +
#   coord_flip()



### profundity alone ###

#look for fluency effect alone
mod1.1 <- lmer(profound ~ fluC + (fluC|subj) + (0 + fluC|stimID), data = bs3Long)
(sm1.1 <- summary(mod1.1))
t_to_eta2(t = sm1.1$coefficients[7:8], df_error = sm1.1$coefficients[5:6], paired = T)
t_to_d(t = sm1.1$coefficients[7:8], df_error = sm1.1$coefficients[5:6], paired = T)
icc(mod1.1)
r2(mod1.1)


#include individual differences as predictors
mod2.1 <- lmer(profound ~ fluC + crtC + nfcC + fiC + sbqC + (fluC|subj) + (0 + fluC|stimID), 
               control = lmerControl(optCtrl = list(maxfun = 2e6)), data = bs3Long)
(sm2.1 <- summary(mod2.1))
t_to_eta2(t = sm2.1$coefficients[19:24], df_error = sm2.1$coefficients[13:18], paired = T)
t_to_d(t = sm2.1$coefficients[19:24], df_error = sm2.1$coefficients[13:18], paired = T)
icc(mod2.1)
r2(mod2.1)


#test for interactions with fluency
mod3.1 <- lmer(profound ~ fluC + fluC:crtC + fluC:nfcC + fluC:fiC + fluC:sbqC + crtC + nfcC + fiC + sbqC + (fluC|subj) + (0 + fluC|stimID),
               control = lmerControl(optCtrl = list(maxfun = 2e6)), data = bs3Long)
(sm3.1 <- summary(mod3.1))
se3.1 <- sqrt(diag(vcov(mod3.1)))
(ci3.1 <- cbind(est = fixef(mod3.1), ll = fixef(mod3.1) - (1.96*se3.1), ul = fixef(mod3.1) + (1.96*se3.1)))
t_to_d(t = sm3.1$coefficients[31:40], df_error = sm3.1$coefficients[21:30], paired = T)
t_to_eta2(t = sm3.1$coefficients[31:40], df_error = sm3.1$coefficients[21:30], paired = T)
icc(mod3.1)
r2(mod3.1)


#compare mod2 and mod3
anova(mod2.1, mod3.1)

#look at model results
check_model(mod3.1)