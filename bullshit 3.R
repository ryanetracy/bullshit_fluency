#############################################################
# study 3 - bullshit receptivity (bs statements with RT DV)
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
  "EMAtools",
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


#data
bs3Data <- read.csv("data files/bs fluency 3 (pseudo with timing).csv")

#remove data from participants with < 96% completion
bs3Data <- filter(bs3Data, Progress >= 96)

#status != 1
bs3Data <- filter(bs3Data, Status == 0)

#check duplicated IPs and IDs
bs3Data$dupID <- duplicated(bs3Data$ResponseId)
bs3Data$dupIP <- duplicated(bs3Data$IPAddress)

bs3Data <- filter(bs3Data, dupID == FALSE & dupIP == FALSE)

#check botcheck question responses
bots <- plyr::count(bs3Data$botCheck)


#demographics
mean(bs3Data$age, na.rm = T); sd(bs3Data$age, na.rm = T)

sex <- bs3Data %>%
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

race <- bs3Data %>%
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

device <- bs3Data %>%
  select(deviceUsed) %>%
  plyr::count() %>%
  mutate(prop = 100 * freq/sum(freq)) %>%
  mutate(deviceUsed = case_when(
    deviceUsed == 1 ~ "laptop",
    deviceUsed == 2 ~ "tablet",
    deviceUsed == 3 ~ "phone",
    deviceUsed == 4 ~ "desktop",
    deviceUsed == 5 ~ "other",
    TRUE ~ "did not identify"
  ))
device

#change 'responseid' to 'subj'
names(bs3Data)[9] <- 'subj'


#clean up crt responses
names(bs3Data)[1460:1463] <- c("crt1", "crt2", "crt3", "crt4")
plyr::count(bs3Data$crt1)
bs3Data$crt01 <- with(bs3Data,
                      case_when(
                        crt1 == "$0.05" ~ 1,
                        crt1 == "0.05" ~ 1,
                        crt1 == "5" ~ 1,
                        crt1 == "5 cents" ~ 1,
                        TRUE ~ 0
                      ))

plyr::count(bs3Data$crt2)
bs3Data$crt02 <- with(bs3Data,
                      case_when(
                        crt2 == "5" ~ 1,
                        crt2 == "5 min" ~ 1,
                        crt2 == "5 mins" ~ 1,
                        crt2 == "5 minutes" ~ 1,
                        crt2 == "5 MINUTES" ~ 1,
                        crt2 == "Five minutes, because one widget takes five minutes to make." ~ 1,
                        TRUE ~ 0
                      ))

plyr::count(bs3Data$crt3)
bs3Data$crt03 <- with(bs3Data,
                      case_when(
                        crt3 == "47" ~ 1,
                        crt3 == "47 days" ~ 1,
                        crt3 == "47days" ~ 1,
                        TRUE ~ 0
                      ))

plyr::count(bs3Data$crt4)
bs3Data$crt04 <- with(bs3Data,
                      case_when(
                        crt4 == "29" ~ 1,
                        crt4 == "29 students" ~ 1,
                        TRUE ~ 0
                      ))

#get alpha values
psych::alpha(bs3Data[,1529:1532])
#average into index
bs3Data$crt <- rowMeans(bs3Data[,1529:1532])

#nfc responses
names(bs3Data)[1464:1481] <- paste0(
  rep("nfc"),
  rep(1:18)
)

#reverse-scored items
bs3Data$nfc3 <- 6 - bs3Data$nfc3
bs3Data$nfc4 <- 6 - bs3Data$nfc4
bs3Data$nfc5 <- 6 - bs3Data$nfc5
bs3Data$nfc7 <- 6 - bs3Data$nfc7
bs3Data$nfc8 <- 6 - bs3Data$nfc8
bs3Data$nfc9 <- 6 - bs3Data$nfc9
bs3Data$nfc12 <- 6 - bs3Data$nfc12
bs3Data$nfc16 <- 6 - bs3Data$nfc16
bs3Data$nfc17 <- 6 - bs3Data$nfc17

#get remaining alphas
#nfc
psych::alpha(bs3Data[,1464:1481])
#fi
psych::alpha(bs3Data[,1483:1493])
#sbq
psych::alpha(bs3Data[,1495:1519])

#average nfc items
bs3Data$nfc <- rowMeans(bs3Data[,1464:1481], na.rm = T)

#average fi and sbq items
bs3Data$fi <- rowMeans(bs3Data[,1483:1493], na.rm = T)
bs3Data$sbq <- rowMeans(bs3Data[,1495:1519], na.rm = T)


#rename the randomization column for blocks
names(bs3Data)[1525] <- "block"

#remove unneeded columns 
bs3 <- bs3Data %>%
  select(-contains(c("Date", "Status", "IPAddress", "Progress", "Duration", "Finished", "Recipient", "Reference", "Location", "Channel", "UserLanguage", "Q1", "_DO", "Q17")))

#split by block
bs3b1 <- bs3[,c(1, 2:601, 1272:1275, 1265)] %>%
  filter(block == "StatementsBlock1")
bs3b2 <- bs3[,c(1, 602:1201, 1272:1275, 1265)] %>%
  filter(block == "StatementsBlock2")

#verify attention check responses (1 = 'respond 5'; 2 = 'respond 7' but in numeric responses)
bs3b1 <- filter(bs3b1, X30_b1AttnCheck1 == 1 & X60_b1AttnCheck2 == 2)
bs3b2 <- filter(bs3b2, X69_b2AttnCheck1 == 1 & X99_b2AttnCheck2 == 2)

#remove unneeded columns (all but ratings responses and page submit timers)
bs3b1 <- bs3b1 %>%
  select(-contains(c("First.Click", "Last.Click", "Click.Count", "AttnCheck", "block")))
bs3b2 <- bs3b2 %>%
  select(-contains(c("First.Click", "Last.Click", "Click.Count", "AttnCheck", "block")))


#rename columns
b1names <- paste0(
  rep("bs"),
  rep(1:60, each = 5),
  rep(c("dis", "flu"), each = 150),
  rep(c("prof", "mean", "undr", "blvb", "time"))
)

b2names <- paste0(
  rep("bs"),
  rep(1:60, each = 5),
  rep(c("flu", "dis"), each = 150),
  rep(c("prof", "mean", "undr", "blvb", "time"))
)

names(bs3b1)[2:301] <- b1names
names(bs3b2)[2:301] <- b2names


#reshape to long
b1Long <- bs3b1 %>%
  gather(key = "stimID", value = "rating",
         bs1disprof:bs60flutime) %>%
  separate(col = "stimID", into = c("stimID", "rateCat"), sep = -4) %>%
  separate(col = "stimID", into = c("stimID", "fluency"), sep = -3) %>%
  pivot_wider(names_from = "rateCat", values_from = "rating") %>%
  mutate(fluency = case_when(
    fluency == "dis" ~ "Disfluent",
    fluency == "flu" ~ "Fluent"
  )) %>%
  mutate(fluC = case_when(
    fluency == "Disfluent" ~ -1,
    fluency == "Fluent" ~ 1
  )) %>%
  mutate(crtC = scale(crt, scale = F)) %>%
  mutate(nfcC = scale(nfc, scale = F)) %>%
  mutate(fiC = scale(fi, scale = F)) %>%
  mutate(sbqC = scale(sbq, scale = F))
names(b1Long)[8:12] <- c("profound", "meaningful", "understandable", "believable", "timing")
  
b2Long <- bs3b2 %>%
  gather(key = "stimID", value = "rating",
         bs1fluprof:bs60distime) %>%
  separate(col = "stimID", into = c("stimID", "rateCat"), sep = -4) %>%
  separate(col = "stimID", into = c("stimID", "fluency"), sep = -3) %>%
  pivot_wider(names_from = "rateCat", values_from = "rating") %>%
  mutate(fluency = case_when(
    fluency == "dis" ~ "Disfluent",
    fluency == "flu" ~ "Fluent"
  )) %>%
  mutate(fluC = case_when(
    fluency == "Disfluent" ~ -.5,
    fluency == "Fluent" ~ .5
  )) %>%
  mutate(crtC = scale(crt, scale = F)) %>%
  mutate(nfcC = scale(nfc, scale = F)) %>%
  mutate(fiC = scale(fi, scale = F)) %>%
  mutate(sbqC = scale(sbq, scale = F))
names(b2Long)[8:12] <- c("profound", "meaningful", "understandable", "believable", "timing")

#merge
bs3Long <- rbind(b1Long, b2Long)

#check response variance
varCheck <- bs3Long %>%
  dplyr::group_by(subj) %>%
  dplyr::summarize(
    sdProfound = sd(profound, na.rm = T),
    sdMeaning = sd(meaningful, na.rm = T),
    sdUnderstand = sd(understandable, na.rm = T),
    sdBelieve = sd(believable, na.rm = T)
  ) %>% 
  as.data.frame

badPs <- filter(varCheck, sdProfound == 0 | sdMeaning == 0 | sdUnderstand == 0 | sdBelieve == 0)
badPs #analyses ran with p's included and excluded, no differences


## test correlations among the variables
corSums <- bs3Long %>%
  dplyr::group_by(subj) %>%
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

### explore the correlations at differing levels of fluency
corSums2 <- bs3Long %>%
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


#just the 4 dvs
cors1 <- Hmisc::rcorr(as.matrix(corSums[2:5]))
cors1

ggcorrplot(cors1[[1]], 
           type = "lower",
           p.mat = cors1[[3]],
           lab = T)

#include all measures
cors2 <- Hmisc::rcorr(as.matrix(corSums[2:9]))
cors2

ggcorrplot(cors2[[1]],
           type = "lower",
           p.mat = cors2[[3]],
           lab = T)

ggpairs(corSums[2:9]) + theme_classic()


#check alpha for bsr items
psych::alpha(corSums[2:5])

#create bsr index
bs3Long$bsr <- rowMeans(bs3Long[,8:11])


#correlate bsr with the individual difference measures
corSumsBSR <- bs3Long %>%
  dplyr::group_by(subj) %>%
  dplyr::summarize(
    mBsr = mean(bsr),
    mCrt = mean(crtC),
    mNfc = mean(nfcC),
    mFi = mean(fiC),
    mSbq = mean(sbqC)
  ) %>% as.data.frame

cors3 <- Hmisc::rcorr(as.matrix(corSumsBSR[2:6]))
cors3

ggcorrplot(cors3[[1]], 
           type = "lower",
           p.mat = cors3[[3]],
           lab = T)

ggpairs(corSumsBSR[2:6]) + theme_classic()


## model responses for bullshit receptivity 

#model only bsr ~ fluency first
mod1 <- lmer(bsr ~ fluC + (fluC|subj) + (0 + fluC|stimID), data = bs3Long)
(sm1 <- summary(mod1))
t_to_eta2(t = sm1$coefficients[7:8], df_error = sm1$coefficients[5:6], paired = T)
t_to_d(t = sm1$coefficients[7:8], df_error = sm1$coefficients[5:6], paired = T)
icc(mod1)
r2(mod1)


#add in controls for individual difference measures
mod2 <- lmer(bsr ~ fluC + crtC + nfcC + fiC + sbqC + (fluC|subj) + (0 + fluC|stimID), data = bs3Long)
(sm2 <- summary(mod2))
t_to_eta2(t = sm2$coefficients[19:24], df_error = sm2$coefficients[13:18], paired = T)
t_to_d(t = sm2$coefficients[19:24], df_error = sm2$coefficients[13:18], paired = T)
icc(mod2)
r2(mod2)


#add in interaction terms
mod3 <- lmer(bsr ~ fluC + crtC + nfcC + fiC + sbqC + fluC:crtC + fluC:nfcC + fluC:fiC + fluC:sbqC + (fluC|subj) + (0 + fluC|stimID), data = bs3Long)
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
anova(mod2, mod3)

check_model(mod3)

## explore response time predicted by fluency

#check timing distribution
ggplot(bs3Long, aes(timing)) +
  geom_histogram(bins = 100, fill = "purple2", alpha = .8, color = "black") +
  theme_classic()

psych::describe(bs3Long$timing)

#apply transformation
msToLog <- function(x) {
  #convert seconds to ms
  toMS <- x * 1000
  #apply log transformation to ms times
  toLog <- log(toMS)
  return(toLog)
}

bs3Long$logTime <- msToLog(bs3Long$timing)

ggplot(bs3Long, aes(logTime)) +
  geom_histogram(bins = 25, fill = "purple2", alpha = .8, color = "black") +
  theme_classic()

#model transformed RT
fMod1 <- lmer(logTime ~ fluC + (fluC|subj) + (0 + fluC|stimID), data = bs3Long)
(sfm1 <- summary(fMod1)) 
t_to_eta2(t = sfm1$coefficients[7:8], df_error = sfm1$coefficients[5:6], paired = T)
t_to_d(t = sfm1$coefficients[7:8], df_error = sfm1$coefficients[5:6], paired = T)
r2(fMod1)
icc(fMod1)

#adjust for individual differences
fMod2 <- lmer(logTime ~ fluC + crtC + nfcC + fiC + sbqC + (fluC|subj) + (0 + fluC|stimID), data = bs3Long)
(sfm2 <- summary(fMod2))
t_to_eta2(t = sfm2$coefficients[19:24], df_error = sfm2$coefficients[13:18], paired = T)
t_to_d(t = sfm2$coefficients[19:24], df_error = sfm2$coefficients[13:18], paired = T)
r2(fMod1)
icc(fMod1)

#add interaction terms
fMod3 <- lmer(logTime ~ fluC + crtC + nfcC + fiC + sbqC + fluC:crtC + fluC:nfcC + fluC:fiC + fluC:sbqC + (fluC|subj) + (0 + fluC|stimID), data = bs3Long)
(sfm3 <- summary(fMod3))
sef3 <- sqrt(diag(vcov(fMod3)))
(fm3CI <- cbind(est = fixef(fMod3), ll = fixef(fMod3) - (1.96*sef3), ul = fixef(fMod3) + (1.96*sef3)))
t_to_d(t = sfm3$coefficients[31:40], df_error = sfm3$coefficients[21:30], paired = T)
t_to_eta2(t = sfm3$coefficients[31:40], df_error = sfm3$coefficients[21:30], paired = T)
r2(fMod3)
icc(fMod3)

#check model
check_model(fMod3)


#include two final models (leave this here just in case someone asks about this [especially a reviewer])

#1: predicting bsr from reading time (interaction with fluency)
fMod4.1 <- lmer(bsr ~ fluC * logTime + crtC + nfcC + fiC + sbqC + fluC:crtC + fluC:nfcC + fluC:fiC + fluC:sbqC + (fluC|subj) + (1|stimID), data = bs3Long)
(sfm4 <- summary(fMod4.1))
sef4 <- sqrt(diag(vcov(fMod4.1)))
(fm4CI <- cbind(est = fixef(fMod4.1), ll = fixef(fMod4.1) - (1.96*sef4), ul = fixef(fMod4.1) + (1.96*sef4)))
t_to_d(t = sfm4$coefficients[37:48], df_error = sfm4$coefficients[25:36], paired = T)
t_to_eta2(t = sfm4$coefficients[37:48], df_error = sfm4$coefficients[25:36], paired = T)
r2(fMod4.1)
icc(fMod4.1)

sim_slopes(fMod4.1,
           pred = 'fluC',
           modx = 'logTime')

interact_plot(fMod4.1,
              pred = 'fluC',
              modx = 'logTime')

# ssfm4.1 <- simple_slopes(fMod4.1)
# ssfm4.1
# cissfm4.1 <- cbind(est = ssfm4.1$`Test Estimate`, ll = ssfm4.1$`Test Estimate` - (1.96*ssfm4.1$`Std. Error`), ul = ssfm4.1$`Test Estimate` + (1.96*ssfm4.1$`Std. Error`))
# cissfm4.1


#2: predicting reading time from bsr (interaction with fluency)
fMod4.2 <- lmer(logTime ~ fluC * bsr + crtC + nfcC + fiC + sbqC + fluC:crtC + fluC:nfcC + fluC:fiC + fluC:sbqC + (fluC|subj) + (1|stimID), data = bs3Long)
(sfm4 <- summary(fMod4.2))
sef4 <- sqrt(diag(vcov(fMod4.2)))
(fm4CI <- cbind(est = fixef(fMod4.2), ll = fixef(fMod4.2) - (1.96*sef4), ul = fixef(fMod4.2) + (1.96*sef4)))
t_to_d(t = sfm4$coefficients[37:48], df_error = sfm4$coefficients[25:36], paired = T)
t_to_eta2(t = sfm4$coefficients[37:48], df_error = sfm4$coefficients[25:36], paired = T)
r2(fMod4.2)
icc(fMod4.2)


#explore the fluency x bsr interaction
sim_slopes(fMod4.2,
           pred = 'fluC',
           modx = 'bsr')

# ssfm4 <- simple_slopes(fMod4.2)
# ssfm4
# cissfm4 <- cbind(est = ssfm4$`Test Estimate`, ll = ssfm4$`Test Estimate` - (1.96*ssfm4$`Std. Error`), ul = ssfm4$`Test Estimate` + (1.96*ssfm4$`Std. Error`))
# cissfm4

#visualize
interact_plot(fMod4.2,
              pred = "fluC",
              modx = "bsr",
              data = bs3Long,
              interval = F,
              int.type = "confidence",
              colors = "purple", 
              y.label = "Log-transformed RTs", 
              x.label = "", 
              legend.main = "BSR Tertiles", 
              main.title = "BSR x Fluency",
              pred.labels = c("Disfluent", "Fluent")) + 
  theme_classic() + 
  theme(legend.position = "bottom")



## model non-transformed RT data using glmm + inverse identity link function from gamma distribution (Lo & Andrews, 2015)

#random intercepts only
fMod1.1 <- glmer(timing ~ fluC + (1|subj) + (1|stimID), family = "Gamma"(link = "identity"), data = bs3Long)
summary(fMod1.1)

#add random slope for fluency for subj intercept
fMod1.2 <- glmer(timing ~ fluC + (fluC|subj) + (1|stimID), family = "Gamma"(link = "identity"), data = bs3Long)
summary(fMod1.2)

#add random slopes to both intercepts
fMod1.3 <- glmer(timing ~ fluC + (fluC|subj) + (0 + fluC|stimID), family = "Gamma"(link = "identity"), data = bs3Long)
summary(fMod1.3)
r2(fMod1.3)
icc(fMod1.3)

#compare models
anova(fMod1.1, fMod1.2, fMod1.3)



#plot two outcomes from fluency: bsr and timing
sumBsr <- Rmisc::summarySE(bs3Long,
                           measurevar = "bsr",
                           groupvars = "fluency",
                           na.rm = T)
sumBsr

bsrParts <- Rmisc::summarySE(bs3Long,
                             measurevar = "bsr",
                             groupvars = c("subj", "fluency"),
                             na.rm = T)

bsrPlot <- ggplot(sumBsr, aes(fluency, bsr)) +
  geom_bar(stat = "identity", alpha = .8, color = "black", fill = "turquoise4", position = position_dodge(.9)) +
  geom_errorbar(aes(ymin = bsr - ci, ymax = bsr + ci),
                width = .25, alpha = .7, position = position_dodge(.9)) +
  geom_point(data = bsrParts, aes(fluency, bsr),
             position = position_jitter(.15, .15), alpha = .3, color = "turquoise4") +
  theme_classic(base_size = 15) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = c(0,1,2,3,4,5,6,7)) +
  scale_x_discrete(labels = c("Disfluent \nStatements", "Fluent \nStatements")) + 
  expand_limits(y = 7) +
  xlab("") +
  ylab("Mean Bullshit Receptivity") +
  # theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
  geom_segment(x = 1, xend = 2, y = 4.1, yend = 4.1) +
  geom_segment(x = 1, xend = 1, y = 4.1, yend = 4.0) +
  geom_segment(x = 2, xend = 2, y = 4.1, yend = 4.0) +
  annotate(geom = "text", x = 1.5, y = 4.2, label = "***")
bsrPlot

# ggsave("study 3 - bsr plot.jpg", device = "jpeg", path = "plots", units = "cm")


sumTime <- Rmisc::summarySE(bs3Long,
                            measurevar = "logTime",
                            groupvars = "fluency",
                            na.rm = T)
sumTime

timeParts <- Rmisc::summarySE(bs3Long,
                              measurevar = "logTime",
                              groupvars = c("subj", "fluency"),
                              na.rm = T)

timePlot <- ggplot(sumTime, aes(fluency, logTime)) +
  geom_bar(stat = "identity", alpha = .8, color = "black", fill = "turquoise4", position = position_dodge(.9)) +
  geom_errorbar(aes(ymin = logTime - ci, ymax = logTime + ci),
                width = .25, alpha = .7, position = position_dodge(.9)) +
  geom_point(data = timeParts, aes(fluency, logTime),
             position = position_jitter(.15, .15), alpha = .3, color = "turquoise4") +
  theme_classic(base_size = 15) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(labels = c("Disfluent \nStatements", "Fluent \nStatements")) + 
  expand_limits(y = 12) +
  xlab("") +
  ylab("Reading Time (log-transformed ms)") +
  geom_segment(x = 1, xend = 2, y = 10.9, yend = 10.9) +
  geom_segment(x = 1, xend = 1, y = 10.9, yend = 10.8) +
  geom_segment(x = 2, xend = 2, y = 10.9, yend = 10.8) +
  annotate(geom = "text", x = 1.5, y = 11.0, label = "***")
timePlot

# ggsave("study 3 - response time plot.jpg", device = "jpeg", path = "plots", units = "cm")

#arrange both plots into a single figure
ggarrange(bsrPlot, timePlot, labels = c("A", "B"), ncol = 2, nrow = 1)

# ggsave("study 3 plots - bsr and timing.jpg", device = "jpeg", path = "plots", units = "cm")


#plot for 6cc talk
bsrPlot2 <- ggplot(bsrParts, aes(fluency, bsr)) +
  geom_violin(fill = 'turquoise4', color = 'black', alpha = .4) +
  geom_point(color = 'turquoise4', alpha = .2, position = position_jitter(.2, .05), shape = 7) +
  geom_point(data = sumBsr, aes(fluency, bsr),
             color = 'gray20', position = position_dodge(.9), size = 2) +
  geom_errorbar(data = sumBsr, aes(fluency, bsr, ymin = bsr - ci, ymax = bsr + ci),
                width = .1, alpha = .5, position = position_dodge(.9)) +
  labs(x = '',
       y = 'Bullshit Receptivity') +
  scale_x_discrete(labels = c('Disfluent\nStatements', 'Fluent\nStatements')) +
  scale_y_continuous(limits = c(1,7),
                     breaks = seq(1,7,1)) +
  geom_segment(x = 1, xend = 2, y = 6.9, yend = 6.9) +
  geom_segment(x = 1, xend = 1, y = 6.9, yend = 6.8) +
  geom_segment(x = 2, xend = 2, y = 6.9, yend = 6.8) +
  annotate(geom = "text", x = 1.5, y = 7, label = "***") +
  theme_classic(base_size = 15)
bsrPlot2

# ggsave('study 3 fluency (6cc talk).jpg', device = 'jpeg', units = 'cm', path = 'plots')


#plot for 6cc talk
timePlot2 <- ggplot(timeParts, aes(fluency, logTime)) +
  geom_violin(fill = 'turquoise4', color = 'black', alpha = .4) +
  geom_point(color = 'turquoise4', alpha = .2, position = position_jitter(.2, .05), shape = 7) +
  geom_point(data = sumTime, aes(fluency, logTime),
             color = 'gray20', position = position_dodge(.9), size = 2) +
  geom_errorbar(data = sumTime, aes(fluency, logTime, ymin = logTime - ci, ymax = logTime + ci),
                width = .1, alpha = .5, position = position_dodge(.9)) +
  labs(x = '',
       y = 'Reading Time (log-transformed ms)') +
  scale_x_discrete(labels = c('Disfluent\nStatements', 'Fluent\nStatements')) +
  scale_y_continuous(limits = c(7.5,11),
                     breaks = seq(7.5,11,.5)) +
  geom_segment(x = 1, xend = 2, y = 10.9, yend = 10.9) +
  geom_segment(x = 1, xend = 1, y = 10.9, yend = 10.8) +
  geom_segment(x = 2, xend = 2, y = 10.9, yend = 10.8) +
  annotate(geom = "text", x = 1.5, y = 11.0, label = "***") +
  theme_classic(base_size = 15)
timePlot2

# ggsave('study 3 reading time (6cc talk).jpg', device = 'jpeg', units = 'cm', path = 'plots')


#arrange into a single figure
ggarrange(bsrPlot2, timePlot2, ncol = 2, labels = c('A', 'B'))

# ggsave('study 3 plots (6cc talk).jpg', device = 'jpeg', units = 'cm', path = 'plots')




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


#explore simple effects of nfc x fluency interaction
#need to re-run the model with the order flipped to get the desired interaction term.... :/
mod3.11 <- lmer(profound ~ fluC + fluC:nfcC + fluC:crtC + fluC:fiC + fluC:sbqC + crtC + nfcC + fiC + sbqC + (fluC|subj) + (0 + fluC|stimID),
               control = lmerControl(optCtrl = list(maxfun = 2e6)), data = bs3Long)
#now do the simple slopes...
ssNfc <- simple_slopes(mod3.11)
ssNfc
ciSSNfc <- cbind(Est = ssNfc$`Test Estimate`, LL = ssNfc$`Test Estimate` - (1.96*ssNfc$`Std. Error`), UL = ssNfc$`Test Estimate` + (1.96*ssNfc$`Std. Error`))
ciSSNfc
t_to_d(t = ssNfc$`t value`, df_error = ssNfc$df, paired = T)
t_to_eta2(t = ssNfc$`t value`, df_error = ssNfc$df, paired = T)

sim_slopes(mod3.1, pred = "fluC", modx = "nfcC", confint = T)
johnson_neyman(mod3.1, pred = "fluC", modx = "nfcC")

(nfcIntPro <- interact_plot(mod3.1,
                            pred = "fluC",
                            modx = "nfcC",
                            data = bs3Long,
                            interval = T,
                            int.type = "confidence",
                            colors = "red", 
                            y.label = "Bullshit Receptivity", 
                            x.label = "", 
                            legend.main = "NFC Tertiles", 
                            main.title = "NFC x Fluency",
                            pred.labels = c("Disfluent", "Fluent")) + 
    theme_classic() + 
    theme(legend.position = "bottom"))

# ggsave("study 3 - nfc x fluency interaction plot (profundity ratings).jpg", device = "jpeg", path = "plots", units = "cm")
