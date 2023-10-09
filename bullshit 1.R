#############################################################
# study 1 - bullshit receptivity (pseudoprofound statements)
#############################################################


library(devtools)
url <- 'https://github.com/ryanetracy/misc_functions/blob/main/misc_functions.R?raw=TRUE'
devtools::source_url(url)

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

package_loader(pckgs)

#load data
bs1Data <- read.csv("data files/bs fluency 1 (pseudo).csv")

#check for duplicated IPs and IDs
bs1Data$dupIP <- duplicated(bs1Data$IPAddress); plyr::count(bs1Data$dupIP)
bs1Data$dupID <- duplicated(bs1Data$ResponseId); plyr::count(bs1Data$dupID)

#remove
bs1Data <- filter(bs1Data, dupIP == "FALSE" & dupID == "FALSE")

#demographics
mean(bs1Data$age, na.rm = T); sd(bs1Data$age, na.rm = T)

sex <- bs1Data %>%
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

race <- bs1Data %>%
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

device <- bs1Data %>%
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


#check CRT responses
#CRT1
plyr::count(bs1Data$CRT1)
#recode (5 or .05 == 1, else == 0)
bs1Data$CRT1 <- with(bs1Data, ifelse(CRT1 == .05 | CRT1 == 5, 1, 0))

#CRT2
plyr::count(bs1Data$CRT2)
#recode (5 == 1, else == 0)
bs1Data$CRT2 <- with(bs1Data, ifelse(CRT2 == 5, 1, 0))

#CRT3
plyr::count(bs1Data$CRT3)
#recode (47 == 1, else == 0)
bs1Data$CRT3 <- with(bs1Data, ifelse(CRT3 == 47, 1, 0))

#CRT4
plyr::count(bs1Data$CRT4)
#recode (29 == 1, else == 0)
bs1Data$CRT4 <- with(bs1Data, ifelse(CRT4 == 29, 1, 0))

#check alpha
psych::alpha(bs1Data[,488:491])
#now average CRT responses
bs1Data$crtMean <- rowMeans(bs1Data[,c(488:491)])

#reverse code NFC responses (numbers 3,4,5,7,8,9,12,16,17)
bs1Data$NFC03 <- 6 - bs1Data$NFC03
bs1Data$NFC04 <- 6 - bs1Data$NFC04
bs1Data$NFC05 <- 6 - bs1Data$NFC05
bs1Data$NFC07 <- 6 - bs1Data$NFC07
bs1Data$NFC08 <- 6 - bs1Data$NFC08
bs1Data$NFC09 <- 6 - bs1Data$NFC09
bs1Data$NFC12 <- 6 - bs1Data$NFC12
bs1Data$NFC16 <- 6 - bs1Data$NFC16
bs1Data$NFC17 <- 6 - bs1Data$NFC17

#no reverse scored items for FI or SBQ scales

#check remaining alphas
#nfc
psych::alpha(bs1Data[,492:509])
#fi
psych::alpha(bs1Data[,510:520])
#sbq
psych::alpha(bs1Data[,521:545])

#average other individual difference measures together
bs1Data$nfcMean <- rowMeans(bs1Data[,c(492:509)], na.rm = T)
bs1Data$fiMean <- rowMeans(bs1Data[,c(510:520)], na.rm = T)
bs1Data$sbqMean <- rowMeans(bs1Data[,c(521:545)], na.rm = T)

#add subject number column
bs1Data$subj <- as.factor(rep(1:nrow(bs1Data)))
bs1Data$block <- factor(bs1Data$block)


#reshape
bsB1 <- bs1Data[,c(557,7,8:247,553:556)] %>%
  filter(block == 1) %>%
  gather(key = "stimID", value = "rating",
         "stim1profdis":"stim60belvflu") %>%
  separate(col = "stimID", into = c("stimID", "fluency"), sep = -3) %>%
  separate(col = "stimID", into = c("stimID", "ratingCat"), sep = -4) %>%
  pivot_wider(names_from = "ratingCat", values_from = "rating") %>%
  mutate(fluency = case_when(
    fluency == "dis" ~ "Disfluent",
    fluency == "flu" ~ "Fluent"
  )) %>%
  mutate(fluC = case_when(
    fluency == "Disfluent" ~ -1,
    fluency == "Fluent" ~ 1
  )) %>%
  mutate(crtC = scale(crtMean, scale = F)) %>%
  mutate(nfcC = scale(nfcMean, scale = F)) %>%
  mutate(fiC = scale(fiMean, scale = F)) %>%
  mutate(sbqC = scale(sbqMean, scale = F))

bsB2 <- bs1Data[,c(557,7,248:487,553:556)] %>%
  filter(block == 2) %>%
  gather(key = "stimID", value = "rating",
         "stim1profflu":"stim60belvdis") %>%
  separate(col = "stimID", into = c("stimID", "fluency"), sep = -3) %>%
  separate(col = "stimID", into = c("stimID", "ratingCat"), sep = -4) %>%
  pivot_wider(names_from = "ratingCat", values_from = "rating") %>%
  mutate(fluency = case_when(
    fluency == "dis" ~ "Disfluent",
    fluency == "flu" ~ "Fluent"
  )) %>%
  mutate(fluC = case_when(
    fluency == "Disfluent" ~ -1,
    fluency == "Fluent" ~ 1
  )) %>%
  mutate(crtC = scale(crtMean, scale = F)) %>%
  mutate(nfcC = scale(nfcMean, scale = F)) %>%
  mutate(fiC = scale(fiMean, scale = F)) %>%
  mutate(sbqC = scale(sbqMean, scale = F))

#merge
bsData <- rbind(bsB1, bsB2)

names(bsData)[9:12] <- c("profound", "meaningful", "understandable", "believable")

#check for response nonvariance for all ratings
varCheckResp <- bsData %>%
  dplyr::group_by(subj) %>%
  dplyr::summarize(
    sdPro = sd(profound, na.rm = T),
    sdMean = sd(meaningful, na.rm =T),
    sdUnder = sd(understandable, na.rm = T),
    sdBelv = sd(believable, na.rm = T)
  ) %>%
  as.data.frame

filter(varCheckResp, sdPro == 0 | sdMean == 0 | sdUnder == 0 | sdBelv == 0)
#response variance doesn't seem to be an issue for the ratings


#get correlations of individual ratings
bsRates <- bsData %>%
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
corSums2 <- bsData %>%
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


#average bs ratings into index
allData <- bsData
allData$bsr <- rowMeans(allData[,c(9:12)], na.rm = T)


#model for fluency effect per block
mod0 <- lmer(bsr ~ block * fluC + (1|subj) + (1|stimID), data = allData)
summary(mod0)


#collapse across block and look for fluency effect alone
mod1 <- lmer(bsr ~ fluC + (fluC|subj) + (fluC|stimID), data = allData)
(sm1 <- summary(mod1))
t_to_eta2(t = sm1$coefficients[7:8], df_error = sm1$coefficients[5:6], paired = T)
t_to_d(t = sm1$coefficients[7:8], df_error = sm1$coefficients[5:6], paired = T)
icc(mod1)
r2(mod1)


#include individual differences as predictors
mod2 <- lmer(bsr ~ fluC + crtC + nfcC + fiC + sbqC + (fluC|subj) + (fluC|stimID), data = allData)
(sm2 <- summary(mod2))
t_to_eta2(t = sm2$coefficients[19:24], df_error = sm2$coefficients[13:18], paired = T)
t_to_d(t = sm2$coefficients[19:24], df_error = sm2$coefficients[13:18], paired = T)
icc(mod2)
r2(mod2)


#test for interactions with fluency
mod3 <- lmer(bsr ~ fluC + fluC:crtC + fluC:nfcC + fluC:fiC + fluC:sbqC + crtC + nfcC + fiC + sbqC + (fluC|subj) + (fluC|stimID),
             control = lmerControl(optCtrl = list(maxfun = 2e6)), data = allData)
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

#look at model results
check_model(mod3)



#summarize the effect of fluency
fluencySum <- Rmisc::summarySE(allData,
                               measurevar = "bsr",
                               groupvars = "fluency",
                               na.rm = T)
fluencySum

fluencyP <- Rmisc::summarySE(allData,
                             measurevar = "bsr",
                             groupvars = c("subj", "fluency"),
                             na.rm = T)

#bar plot
ggplot(fluencySum, aes(fluency, bsr)) +
  geom_bar(stat = "identity", fill = "turquoise4", color = "black", alpha = .8) +
  geom_errorbar(aes(ymin = bsr - ci, ymax = bsr + ci), 
                width = .25, alpha = .75) +
  geom_point(data = fluencyP, aes(fluency, bsr),
             alpha = .25, color = "turquoise4", size = .75, position = position_jitter(.15, .15)) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = c(0, 1, 2, 3, 4, 5, 6, 7)) +
  scale_x_discrete(labels = c("Disfluent Statements", "Fluent Statements")) +
  expand_limits(y = 7) +
  theme_classic(base_size = 15) + 
  xlab("") + 
  ylab("Bullshit Receptivity") + 
  geom_segment(x = 1, xend = 2, y = 5.4, yend = 5.4) + 
  geom_segment(x = 1, xend = 1, y = 5.4, yend = 5.3) +
  geom_segment(x = 2, xend = 2, y = 5.4, yend = 5.3) +
  annotate(geom = "text", x = 1.5, y = 5.5, label = "***", size = 5)

# ggsave("study 1 - fluency plot.jpg", device = "jpeg", path = "plots", units = "cm")


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

# ggsave('study 1 fluency (6cc talk).jpg', device = 'jpeg', units = 'cm', path = 'plots')

# sumBS <- Rmisc::summarySE(data = allData, 
#                           measurevar = "bsr", 
#                           groupvars = "fluency", 
#                           na.rm = T)
# 
# #raincloud plot
# ggplot(allData, aes(fluency, bsr)) +
#   geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .7, color = "black", fill = "turquoise2") +
#   geom_point(position = position_jitter(width = .15, height = .15), size = .5,  alpha = .7, color = "turquoise2") +
#   geom_boxplot(width = .1, alpha = .5, color = "black", fill = "turquoise2") +
#   geom_point(data = sumBS, aes(x = fluency, y = bsr), color = "black", size = 2) +
#   geom_errorbar(data = sumBS, aes(x = fluency, y = bsr, ymin = bsr - (1.96*se), ymax = bsr + (1.96*se)), width = .15, alpha = .6) +
#   expand_limits(x = 3, y = 7.75) + theme_bw() + xlab(NULL) + ylab("Bullshit Receptivity") +
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
mod3.1 <- lmer(profound ~ fluC + fluC:crtC + fluC:nfcC + fluC:fiC + fluC:sbqC + crtC + nfcC + fiC + sbqC + (fluC|subj) + (fluC|stimID),
             control = lmerControl(optCtrl = list(maxfun = 2e6)), data = allData)
(sm3.1 <- summary(mod3.1))
se3.1 <- sqrt(diag(vcov(mod3.1)))
(ci3.1 <- cbind(est = fixef(mod3.1), ll = fixef(mod3.1) - (1.96*se3.1), ul = fixef(mod3.1) + (1.96*se3.1)))
t_to_eta2(t = sm3.1$coefficients[31:40], df_error = sm3.1$coefficients[21:30], paired = T)
t_to_d(t = sm3.1$coefficients[31:40], df_error = sm3.1$coefficients[21:30], paired = T)
icc(mod3.1)
r2(mod3.1)


#compare mod2 and mod3
anova(mod2.1, mod3.1)

#look at model results
check_model(mod3.1)