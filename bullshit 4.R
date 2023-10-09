#############################################################
# study 4 - bullshit receptivity (bullshit vs non-bullshit)
# pseudoprofound vs inspirational statements
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


# load the data
s4_df <- read.csv('data files/bs fluency 4 (pseudoprofound vs inspirational).csv')

s4_df <- s4_df %>%
  filter(Progress >= 87)

bots <- s4_df %>% select(bot_check)


# demographics
mean(s4_df$age, na.rm = T); sd(s4_df$age, na.rm = T)

sex <- s4_df %>%
  count(sex) %>%
  mutate(prop = round(100 * (n / sum(n)), 2)) %>%
  mutate(sex = case_when(
    sex == 1 ~ 'male',
    sex == 2 ~ 'female',
    sex == 3 ~ 'nonbinary',
    sex == 4 ~ 'not listed',
    TRUE ~ 'no response'
  ))
sex

race <- s4_df %>%
  count(race) %>%
  mutate(prop = round(100 * (n / sum(n)), 2)) %>%
  mutate(race = case_when(
    race == 1 ~ 'asian',
    race == 2 ~ 'black',
    race == 3 ~ 'latino/a',
    race == 4 ~ 'middle eastern',
    race == 5 ~ 'pacific islander',
    race == 6 ~ 'native american',
    race == 7 ~ 'white',
    race == 8 ~ 'bi/multi racial',
    race == 9 ~ 'other/not listed',
    TRUE ~ 'no response'
  ))
race

names(s4_df)[9] <- 'subj'

# prep individual difference stims
plyr::count(s4_df$X1_crt)
good_crt01 <- c(
  'The ball costs $0.05.',
  'The ball cost $0.05',
  '5 cents',
  '5 cent',
  '0.05 Cents',
  '0.05 cents',
  '0.05',
  '$0.05'
)

plyr::count(s4_df$X2_crt)
good_crt02 <- c(
  'Take 5 minutes for 100 machines to make 100 widgets. ',
  'It will take 100 machines 5 minutes to make 100 widgets.',
  'It should take around 5 minutes for 100 machines to make 100 widgets',
  '5minutes',
  '5min',
  '5m',
  '5 minutes ',
  '5 minutes',
  '5 mins',
  '5 min',
  '5',
  '100 machines can make 100 widgets in 5 minutes.'
)

plyr::count(s4_df$X3_crt)
good_crt03 <- c(
  '47',
  '47 days',
  '47 Days',
  '47 days ',
  '47days',
  'If the patch doubles in size every day, then if it takes 48 days to cover the entire lake, it must take 47 days to cover half the lake.',
  'It should take 47 days as the patch doubles every day',
  'It would take 47 days for the patch to cover half of the lake.'
)

plyr::count(s4_df$X4_crt)
good_crt04 <- c(
  'There should be 29 students in the class',
  '29 students ',
  '29 students',
  '29'
)

s4_df <- s4_df %>% mutate(X1_crt = ifelse(X1_crt %in% good_crt01, 1, 0),
                          X2_crt = ifelse(X2_crt %in% good_crt02, 1, 0),
                          X3_crt = ifelse(X3_crt %in% good_crt03, 1, 0),
                          X4_crt = ifelse(X4_crt %in% good_crt04, 1, 0))


# get alpha values
psych::alpha(s4_df[, c('X1_crt', 'X2_crt', 'X3_crt', 'X4_crt')])
# average together
s4_df$crt <- rowMeans(s4_df[, c('X1_crt', 'X2_crt', 'X3_crt', 'X4_crt')])


# nfc
#reverse-scored items
s4_df$nfc_3 <- 6 - s4_df$nfc_3
s4_df$nfc_4 <- 6 - s4_df$nfc_4
s4_df$nfc_5 <- 6 - s4_df$nfc_5
s4_df$nfc_7 <- 6 - s4_df$nfc_7
s4_df$nfc_8 <- 6 - s4_df$nfc_8
s4_df$nfc_9 <- 6 - s4_df$nfc_9
s4_df$nfc_12 <- 6 - s4_df$nfc_12
s4_df$nfc_16 <- 6 - s4_df$nfc_16
s4_df$nfc_17 <- 6 - s4_df$nfc_17


# get remaining alphas
# nfc
psych::alpha(s4_df[,383:400])
# fi
psych::alpha(s4_df[,402:413])
# sbq
psych::alpha(s4_df[,415:439])

# average together
s4_df$nfc <- rowMeans(s4_df[,383:400], na.rm = T)
s4_df$fi <- rowMeans(s4_df[,402:413], na.rm = T)
s4_df$sbq <- rowMeans(s4_df[,415:439], na.rm = T)

s4_df %>%
  ggplot(aes(crt)) +
  geom_histogram(color = 'black', fill = 'turquoise4') +
  theme_bw()

s4_df %>%
  ggplot(aes(nfc)) +
  geom_histogram(color = 'black', fill = 'turquoise4') +
  theme_bw()

s4_df %>%
  ggplot(aes(fi)) +
  geom_histogram(color = 'black', fill = 'turquoise4') +
  theme_bw()

s4_df %>%
  ggplot(aes(sbq)) +
  geom_histogram(color = 'black', fill = 'turquoise4') +
  theme_bw()

# rename counterbalance block column
names(s4_df)[448] <- 'block'
s4_clean <- s4_df %>%
  select(-contains(c('Date', 'Status', 'IPAddress', 'Progress', 'Duration', 'Finished',
                     'Recipient', 'External', 'Location', 'Distribution', 'User', 'consent',
                     'First.Click', 'Last.Click', 'Click.Count', '_DO', 'dupID', 'dupIP', '_TEXT')))



# create vectors for stim names
ns_bs_1 <- paste0(
  rep('stim'),
  rep(1:20, each = 5),
  rep('_'),
  rep(c('Fluent', 'Disfluent'), each = 25),
  rep('_'),
  rep(c('Bullshit', 'Inspirational'), each = 50),
  rep('_'),
  rep(c('Profound', 'Understandable', 'Believable', 'Meaningful', 'Time'))
)

ns_bs_2 <- paste0(
  rep('stim'),
  rep(1:20, each = 5),
  rep('_'),
  rep(c('Disfluent', 'Fluent'), each = 25),
  rep('_'),
  rep(c('Bullshit', 'Inspirational'), each = 50),
  rep('_'),
  rep(c('Profound', 'Understandable', 'Believable', 'Meaningful', 'Time'))
)


# apply these
names(s4_clean)[2:101] <- ns_bs_1
names(s4_clean)[102:201] <- ns_bs_2

b1 <- s4_clean %>%
  select(subj,
         stim1_Fluent_Bullshit_Profound:stim20_Disfluent_Inspirational_Time,
         block,
         crt:sbq) %>%
  filter(block == 'counterbalance_1')

b2 <- s4_clean %>%
  select(subj,
         stim1_Disfluent_Bullshit_Profound:stim20_Fluent_Inspirational_Time,
         block,
         crt:sbq) %>%
  filter(block == 'counterbalance_2')


# reshape to long
b1_long <- b1 %>%
  pivot_longer(cols = stim1_Fluent_Bullshit_Profound:stim20_Disfluent_Inspirational_Time,
               names_to = c('stimID', 'fluency', 'statement_type', 'rating'),
               values_to = 'response',
               names_sep = '_') %>%
  mutate(flu_c = ifelse(fluency == 'Disfluent', -1, 1),
         type_c = ifelse(statement_type == 'Inspirational', -1, 1)) %>%
  pivot_wider(names_from = 'rating', values_from = 'response')

b2_long <- b2 %>%
  pivot_longer(cols = stim1_Disfluent_Bullshit_Profound:stim20_Fluent_Inspirational_Time,
               names_to = c('stimID', 'fluency', 'statement_type', 'rating'),
               values_to = 'response',
               names_sep = '_') %>%
  mutate(flu_c = ifelse(fluency == 'Disfluent', -1, 1),
         type_c = ifelse(statement_type == 'Inspirational', -1, 1)) %>%
  pivot_wider(names_from = 'rating', values_from = 'response')


# merge
s4_long <- rbind(b1_long, b2_long)

# check invariance
s4_invar <- s4_long %>%
  group_by(subj) %>%
  summarize(
    profound = sd(Profound, na.rm = T),
    understandable = sd(Understandable, na.rm = T),
    meaningful = sd(Meaningful, na.rm = T),
    believable = sd(Believable, na.rm = T)
  ) %>%
  as.data.frame

bad_ps <- s4_invar %>%
  # filter(profound == 0 | understandable == 0 | meaningful == 0 | believable == 0) # just in case
  filter(profound == 0 & understandable == 0 & meaningful == 0 & believable == 0)

# remove these people
s4_long <- s4_long %>% filter(!(subj %in% bad_ps$subj))


# get item correlations
s4_corrs <- s4_long %>%
  group_by(subj, statement_type) %>%
  summarize(
    profound = mean(Profound, na.rm = T),
    meaningful = mean(Meaningful, na.rm = T),
    understandable = mean(Understandable, na.rm = T),
    believable = mean(Believable, na.rm = T),
    crt = mean(crt),
    nfc = mean(nfc),
    fi = mean(fi),
    sbq = mean(sbq)
  ) %>%
  as.data.frame

bs_corrs <- s4_corrs %>% filter(statement_type == 'Bullshit')
insp_corrs <- s4_corrs %>% filter(statement_type == 'Inspirational')

(corr_test_bs <- Hmisc::rcorr(as.matrix(bs_corrs[,3:10])))

ggcorrplot(corr_test_bs$r,
           type = 'lower',
           lab = T,
           p.mat = corr_test_bs$P)

(corr_test_insp <- Hmisc::rcorr(as.matrix(insp_corrs[,3:10])))

ggcorrplot(corr_test_insp$r,
           type = 'lower',
           lab = T,
           p.mat = corr_test_insp$P)

psych::alpha(s4_corrs[,4:7])

# compute centered individual difference variables
s4_long <- s4_long %>%
  mutate(crtC = scale(crt, scale = F),
         nfcC = scale(nfc, scale = F),
         fiC = scale(fi, scale = F),
         sbqC = scale(sbq, scale = F)) 



## main bsr analyses

# compute bsr index
s4_long$bsr <- rowMeans(
  s4_long[, c('Profound', 'Understandable', 'Meaningful', 'Believable')],
  na.rm = T
  )

mod1 <- lmer(bsr ~ flu_c * type_c + (0+flu_c|subj) + (1|stimID), data = s4_long)
summary(mod1)

# summary(lmer(Profound ~ flu_c * type_c + (0 + flu_c|subj) + (1|stimID), data = s4_long))
# summary(lmer(Understandable ~ flu_c * type_c + (0 + flu_c|subj) + (1|stimID), data = s4_long))
# summary(lmer(Meaningful ~ flu_c * type_c + (0 + flu_c|subj) + (1|stimID), data = s4_long))
# summary(lmer(Believable ~ flu_c * type_c + (0 + flu_c|subj) + (1|stimID), data = s4_long))


# add in individual differences
mod2 <- lmer(bsr ~ flu_c * type_c + crtC + nfcC + fiC + sbqC + (0 + flu_c|subj) + (1|stimID), data = s4_long)
summary(mod2)

# interactive effects
mod3 <- lmer(bsr ~ flu_c * type_c 
             + crtC * flu_c
             + nfcC * flu_c
             + fiC * flu_c
             + sbqC * flu_c
             + crtC * type_c
             + nfcC * type_c
             + fiC * type_c
             + sbqC * type_c
             + (0 + flu_c|subj) + (1|stimID), data = s4_long)
(sm3 <- summary(mod3))
confint(mod3)
t_to_d(t = sm3$coefficients[49:64],
       df_error = sm3$coefficients[33:48],
       paired = T)
t_to_eta2(t = sm3$coefficients[49:64],
           df_error = sm3$coefficients[33:48],
           paired = T)
icc(mod3)
r2(mod3)

sim_slopes(mod3,
           pred = 'nfcC',
           modx = 'type_c',
           confint = T)

sim_slopes(mod3,
           pred = 'sbqC',
           modx = 'type_c',
           confint = T)


s4_long %>%
  group_by(fluency) %>%
  rstatix::get_summary_stats(bsr, type = 'mean_sd')

s4_long %>%
  group_by(statement_type) %>%
  rstatix::get_summary_stats(bsr, type = 'mean_sd')

s4_long %>%
  group_by(fluency, statement_type) %>%
  rstatix::get_summary_stats(bsr, type = 'mean_sd')


plot_means <- s4_long %>%
  group_by(fluency, statement_type) %>%
  rstatix::get_summary_stats(bsr, type = 'full')

p1 <- s4_long %>%
  ggplot(aes(fluency, bsr, fill = statement_type)) +
  geom_violin(alpha = .7, color = 'black', position = position_dodge(.9)) +
  geom_point(size = .5,
             alpha = .25,
             position = position_jitterdodge(.15, .05, .9),
             shape = 4) +
  geom_point(data = plot_means,
             aes(fluency, mean),
             position = position_dodge(.9),
             shape = 7,
             size = 3,
             color = 'white') +
  geom_errorbar(data = plot_means,
                aes(fluency, mean, ymin = mean - ci, ymax = mean + ci),
                width = .15,
                position = position_dodge(.9),
                color = 'white') +
  theme_classic() +
  scale_fill_manual(values = c('turquoise4', 'gray70'),
                    labels = c(
                      'Pseudoprofound\nBullshit', 'Inspirational\nQuotes'
                      )) +
  scale_x_discrete(labels = c('Disfluent\nStatements', 'Fluent\nStatements')) +
  scale_y_continuous(breaks = seq(1, 7, 1)) +
  labs(x = '',
       y = 'Statement Receptivity',
       fill = '') +
  theme(legend.position = 'bottom')

p1


# ggsave('study 4 - pseudoprofound vs inspirational (profoundness ratings).jpg',
#        device = 'jpeg',
#        units = 'cm',
#        path = 'plots')



## reading time analyses
s4_long %>%
  ggplot(aes(Time)) +
  geom_histogram(fill = 'turquoise4', binwidth = 12, alpha = .75, color = 'black') +
  theme_classic()

msToLog <- function(x) {
  #convert seconds to ms
  toMS <- x * 1000
  #apply log transformation to ms times
  toLog <- log(toMS)
  return(toLog)
}

s4_long$log_time <- msToLog(s4_long$Time)

s4_long %>%
  ggplot(aes(log_time)) +
  geom_histogram(fill = 'turquoise4', binwidth = .5, alpha = .75, color = 'black') +
  theme_classic()

rt_mod1 <- lmer(log_time ~ flu_c * type_c + (flu_c|subj) + (1|stimID), data = s4_long)
(sm_rt1 <- summary(rt_mod1))
confint(rt_mod1)
t_to_d(t = sm_rt1$coefficients[13:16],
       df_error = sm_rt1$coefficients[9:12],
       paired = T)
t_to_eta2(t = sm_rt1$coefficients[13:16],
          df_error = sm_rt1$coefficients[9:12],
          paired = T)

s4_long %>%
  group_by(fluency) %>%
  get_summary_stats(log_time, type = 'mean_sd')

s4_long %>%
  group_by(statement_type) %>%
  get_summary_stats(log_time, type = 'mean_sd')

s4_long %>%
  group_by(fluency, statement_type) %>%
  get_summary_stats(log_time, type = 'mean_sd')


rt_mod_2 <- lmer(log_time ~ flu_c * type_c
                 + nfcC * flu_c
                 + crtC * flu_c
                 + fiC * flu_c
                 + sbqC * flu_c
                 + nfcC * type_c
                 + crtC * type_c
                 + fiC * type_c
                 + sbqC * type_c
                 + (flu_c|subj) + (1|stimID), data = s4_long)
(sm_rt2 <- summary(rt_mod_2))
confint(rt_mod_2)
t_to_d(t = sm_rt2$coefficients[49:64],
       df_error = sm_rt2$coefficients[33:48],
       paired = T)
t_to_eta2(t = sm_rt2$coefficients[49:64],
          df_error = sm_rt2$coefficients[33:48],
          paired = T)

plot_means_rt <- s4_long %>%
  group_by(fluency, statement_type) %>%
  rstatix::get_summary_stats(log_time, type = 'full')


p2 <- s4_long %>%
  ggplot(aes(fluency, log_time, fill = statement_type)) +
  geom_violin(alpha = .7, color = 'black', position = position_dodge(.9)) +
  geom_point(size = .5,
             alpha = .25,
             position = position_jitterdodge(.15, .05, .9),
             shape = 4) +
  geom_point(data = plot_means_rt,
             aes(fluency, mean),
             position = position_dodge(.9),
             shape = 7,
             size = 3,
             color = 'white') +
  geom_errorbar(data = plot_means_rt,
                aes(fluency, mean, ymin = mean - ci, ymax = mean + ci),
                width = .15,
                position = position_dodge(.9),
                color = 'white') +
  theme_classic() +
  scale_fill_manual(values = c('turquoise4', 'gray70'),
                    labels = c(
                      'Pseudoprofound\nBullshit', 'Inspirational\nQuotes'
                      )) +
  scale_x_discrete(labels = c('Disfluent\nStatements', 'Fluent\nStatements')) +
  scale_y_continuous(breaks = seq(1, 16, 1)) +
  labs(x = '',
       y = 'Reading Time (log-transformed ms)',
       fill = '') +
  theme(legend.position = 'bottom')

p2

# ggsave('study 4 - pseudoprofound vs inspirational (RT ratings).jpg',
#        device = 'jpeg',
#        units = 'cm',
#        path = 'plots')
# 
# 
# ggarrange(p1, p2,
#           common.legend = T,
#           labels = c('A', 'B'))

# ggsave('study 4 - combined plot.jpg',
#        device = 'jpeg',
#        units = 'cm',
#        path = 'plots')


## effect of fluency when controlling for reading time
mod3.1 <- lmer(bsr ~ flu_c * type_c * log_time + (1|subj) + (1|stimID), data = s4_long)
(sm3.1 <- summary(mod3.1))
confint(mod3.1)
t_to_d(t = sm3.1$coefficients[25:32],
       df_error = sm3.1$coefficients[17:24],
       paired = T)
t_to_eta2(t = sm3.1$coefficients[25:32],
          df_error = sm3.1$coefficients[17:24],
          paired = T)

# sim_slopes(mod3.1,
#            pred = 'flu_c',
#            modx = 'type_c')
# 
# sim_slopes(mod3.1,
#            pred = 'log_time',
#            modx = 'flu_c')
# 
# sim_slopes(mod3.1,
#            pred = 'log_time',
#            modx = 'type_c')


sim_slopes(mod3.1,
           pred = 'log_time',
           modx = 'type_c',
           mod2 = 'flu_c',
           confint = T)

interact_plot(mod3.1,
              pred = 'log_time',
              modx = 'type_c',
              mod2 = 'flu_c',
              interval = T,
              int.type = 'confidence',
              modx.labels = c('Inspirational\nQuotes', 'Pseudoprofound\nBullshit'),
              mod2.labels = c('Disfluent\nStatements', 'Fluent\nStatements'),
              x.label = 'Reading Time (log-transformed ms)',
              y.label = 'Statement Receptivity',
              legend.main = '',
              colors = 'blue') +
  scale_y_continuous(limits = c(1, 6),
                     breaks = seq(1, 6, 1)) +
  # scale_x_continuous(limits = c(.5, 8),
  #                    breaks = seq(.5, 8, 1.5)) +
  theme_classic(base_size = 15) +
  theme(legend.position = 'bottom') 

# ggsave('study 4 - time x fluency x type.jpg',
#        device = 'jpeg',
#        units = 'cm',
#        path = 'plots')


mod3.2 <- lmer(bsr ~ flu_c * type_c * log_time 
               + crtC * flu_c
               + nfcC * flu_c
               + fiC * flu_c
               + sbqC * flu_c
               + crtC * type_c
               + nfcC * type_c
               + fiC * type_c
               + sbqC * type_c
               + (1|subj) + (1|stimID), data = s4_long)
summary(mod3.2)

sim_slopes(mod3.2,
           pred = 'flu_c',
           modx = 'type_c',
           confint = T)

sim_slopes(mod3.2,
           pred = 'log_time',
           modx = 'flu_c',
           confint = T)

sim_slopes(mod3.2,
           pred = 'log_time',
           modx = 'type_c',
           confint = T)




## bullshit sensitivity analyses

s4_scales <- s4_clean %>%
  select(subj,
         crt,
         nfc,
         fi,
         sbq) %>%
  mutate(
    crtC = scale(crt, scale = F),
    nfcC = scale(nfc, scale = F),
    fiC = scale(fi, scale = F),
    sbqC = scale(sbq, scale = F)
  )


s4_bs_sensitivity <- s4_long %>%
  group_by(subj, fluency, statement_type) %>%
  summarize(
    profound = mean(Profound, na.rm = T),
    meaningful = mean(Meaningful, na.rm = T),
    understandable = mean(Understandable, na.rm = T),
    believable = mean(Believable, na.rm = T)
  ) %>%
  pivot_wider(
    names_from = 'statement_type',
    values_from = c('profound',
                    'meaningful',
                    'understandable',
                    'believable')
  ) %>%
  mutate(
    profound_sensitivity = profound_Inspirational - profound_Bullshit,
    meaning_sensitivity = meaningful_Inspirational - meaningful_Bullshit,
    understand_sensitivity = understandable_Inspirational - understandable_Bullshit,
    believe_sensitivity = believable_Inspirational - believable_Bullshit,
    
    fl_dummy = ifelse(fluency == 'Fluent', 1, -1)
  ) %>%
  select(
    subj,
    fluency,
    fl_dummy,
    profound_sensitivity,
    meaning_sensitivity,
    understand_sensitivity,
    believe_sensitivity,
  ) %>%
  rowwise() %>%
  mutate(
    bs_sensitivity = rowMeans(across(profound_sensitivity:believe_sensitivity))
    ) %>%
  left_join(s4_scales, by = c('subj' = 'subj'))


mod4 <- lmer(bs_sensitivity ~ fl_dummy
             + crtC * fl_dummy
             + nfcC * fl_dummy
             + fiC * fl_dummy
             + sbqC * fl_dummy
             + (1|subj), data = s4_bs_sensitivity)
summary(mod4)


summary(
  lmer(profound_sensitivity ~ fl_dummy
       + crtC * fl_dummy
       + nfcC * fl_dummy
       + fiC * fl_dummy
       + sbqC * fl_dummy
       + (1|subj), data = s4_bs_sensitivity)
)

summary(
  lmer(profound_sensitivity ~ fl_dummy
       + crtC * fl_dummy
       + nfcC * fl_dummy
       + fiC * fl_dummy
       + sbqC * fl_dummy
       + (1|subj), data = s4_bs_sensitivity)
)

summary(
  lmer(profound_sensitivity ~ fl_dummy
       + crtC * fl_dummy
       + nfcC * fl_dummy
       + fiC * fl_dummy
       + sbqC * fl_dummy
       + (1|subj), data = s4_bs_sensitivity)
)

summary(
  lmer(profound_sensitivity ~ fl_dummy
       + crtC * fl_dummy
       + nfcC * fl_dummy
       + fiC * fl_dummy
       + sbqC * fl_dummy
       + (1|subj), data = s4_bs_sensitivity)
)


