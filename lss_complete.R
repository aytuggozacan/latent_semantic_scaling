library(LSX)
library(quanteda)
library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(ggpubr)

################################################################################
#Data Sources

data <- fread("data/category_I.csv")
data <- data %>% filter(Date > '2014-12-31')

dataII <- fread("data/category_II.csv")
dataII <- dataII %>% filter(Date > '2014-12-31')

lexicon <- fread("data/lexicon_tr.csv")
lexicon <- lexicon[, POLARITY := NULL]

################################################################################
#LSS model common part - CatI

toks_sent <- data %>% 
  corpus(text_field = "Entry")  %>% 
  corpus_reshape(to =  "sentences")  %>%  
  tokens(remove_punct = TRUE, remove_numbers = TRUE, 
         remove_symbols = TRUE, remove_url = TRUE) %>%
  tokens_tolower() %>%  
  tokens_remove(pattern = stopwords("tr", source = "stopwords-iso")) %>%
  tokens_remove(c("bkz","başlık", "bi", "amk", "lan", "mi", "bok", "ada",
                  "a", "entry"))  %>%
  tokens_wordstem(language = "turkish")

dfmat_sent <- toks_sent %>% 
  dfm() %>% 
  dfm_remove(pattern = "") %>% 
  dfm_trim(min_termfreq = 5)

seed <- as.list(lexicon)
seed <- as.seedwords(seed)

###############################################################################
#Economic Sentiment - CatI

eco_econI <- char_context(toks_sent, pattern = "ekonom*", p = 0.05)

tmod_lss_econI <- textmodel_lss(dfmat_sent, seeds = seed,
                          terms = eco_econI, k = 300, cache = TRUE)

dfmat_doc <- dfm_group(dfmat_sent)
dat_econI <- docvars(dfmat_doc)
dat_econI$fit <- predict(tmod_lss_econI, newdata = dfmat_doc)
dat_econI <- dat_econI %>% mutate(date=as.Date(Date, format = "%Y-%m-%d"))

dat_smooth_econI <- smooth_lss(dat_econI, engine = "locfit")

econI_graph <- ggplot(dat_econI, aes(date)) +
  geom_point(aes(y = fit), col = rgb(0, 0, 0, 0.010), pch = 16) +
  geom_line( data = dat_smooth_econI, aes(y = fit), col = "darkred") +
  ylim(c(-0.4,0.4)) + 
  geom_hline(yintercept = 0) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2016-07-15")), 
                 color = "Coup Attempt"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2016-07-20")), 
                 color = "State of Emergency")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2018-01-19")), 
                 color = "Economic Crisis")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2018-07-18")),
                 color = "State o fEmergency")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2018-06-24")), 
                 color = "Election"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2023-05-28")), 
                 color = "Election"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2020-03-11")), 
                 color = "Pandemic")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2022-03-03")), 
                 color = "Pandemic")) +
  scale_color_manual(name = "Breakpoints", 
                     values = c('Coup Attempt' = "red", 'State of Emergency' = "darkolivegreen", 
                                Election = "darkorange2", Pandemic = "darkorchid",
                                'Economic Crisis' = "deepskyblue4")) +
  labs(x = "Time", y = "Cat_I - Economic sentiment") +
  theme_classic()
################################################################################
#Education Sentiment - CatI 

eco_eduI <- char_context(toks_sent, pattern = "eğit*", p = 0.05)

tmod_lss_eduI <- textmodel_lss(dfmat_sent, seeds = seed,
                          terms = eco_eduI, k = 300, cache = TRUE)

dfmat_doc <- dfm_group(dfmat_sent)
dat_eduI <- docvars(dfmat_doc)
dat_eduI$fit <- predict(tmod_lss_eduI, newdata = dfmat_doc)
dat_eduI <- dat_eduI %>% mutate(date=as.Date(Date, format = "%Y-%m-%d"))

dat_smooth_eduI <- smooth_lss(dat_eduI, engine = "locfit")

eduI_graph <- ggplot(dat_eduI, aes(date)) +
  geom_point(aes(y = fit), col = rgb(0, 0, 0, 0.010), pch = 16) +
  geom_line( data = dat_smooth_eduI, aes(y = fit), col = "darkred") +
  ylim(c(-0.4,0.4)) + 
  geom_hline(yintercept = 0) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2016-07-15")), 
                 color = "Coup Attempt"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2016-07-20")), 
                 color = "State of Emergency")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2018-01-19")), 
                 color = "Economic Crisis")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2018-07-18")),
                 color = "State o fEmergency")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2018-06-24")), 
                 color = "Election"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2023-05-28")), 
                 color = "Election"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2020-03-11")), 
                 color = "Pandemic")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2022-03-03")), 
                 color = "Pandemic")) +
  scale_color_manual(name = "Breakpoints", 
                     values = c('Coup Attempt' = "red", 'State of Emergency' = "darkolivegreen", 
                                Election = "darkorange2", Pandemic = "darkorchid",
                                'Economic Crisis' = "deepskyblue4")) +
  labs(x = "Time", y = "Cat_I - Education Sentiment") +
  theme_classic()

################################################################################
#Social Sentiment - CatI

eco_socI <- char_context(toks_sent, pattern = "sosy*", p = 0.05)

tmod_lss_socI <- textmodel_lss(dfmat_sent, seeds = seed,
                          terms = eco_socI, k = 300, cache = TRUE)

dfmat_doc <- dfm_group(dfmat_sent)
dat_socI <- docvars(dfmat_doc)
dat_socI$fit <- predict(tmod_lss_socI, newdata = dfmat_doc)
dat_socI <- dat_socI %>% mutate(date=as.Date(Date, format = "%Y-%m-%d"))

dat_smooth_socI <- smooth_lss(dat_socI, engine = "locfit")

socI_graph <- ggplot(dat_socI, aes(date)) +
  geom_point(aes(y = fit), col = rgb(0, 0, 0, 0.010), pch = 16) +
  geom_line( data = dat_smooth_socI, aes(y = fit), col = "darkred") +
  ylim(c(-0.4,0.4)) + 
  geom_hline(yintercept = 0) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2016-07-15")), 
                 color = "Coup Attempt"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2016-07-20")), 
                 color = "State of Emergency")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2018-01-19")), 
                 color = "Economic Crisis")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2018-07-18")),
                 color = "State o fEmergency")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2018-06-24")), 
                 color = "Election"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2023-05-28")), 
                 color = "Election"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2020-03-11")), 
                 color = "Pandemic")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2022-03-03")), 
                 color = "Pandemic")) +
  scale_color_manual(name = "Breakpoints", 
                     values = c('Coup Attempt' = "red", 'State of Emergency' = "darkolivegreen", 
                                Election = "darkorange2", Pandemic = "darkorchid",
                                'Economic Crisis' = "deepskyblue4")) +
  labs(x = "Time", y = "Cat_I - Social Sentiment") + 
  theme_classic()

################################################################################
#Working Life Sentiment -CatI

eco_workI <- char_context(toks_sent, pattern = "çalış*", p = 0.05)

tmod_lss_workI <- textmodel_lss(dfmat_sent, seeds = seed,
                          terms = eco_workI, k = 300, cache = TRUE)

dfmat_doc <- dfm_group(dfmat_sent)
dat_workI <- docvars(dfmat_doc)
dat_workI$fit <- predict(tmod_lss_workI, newdata = dfmat_doc)
dat_workI <- dat_workI %>% mutate(date=as.Date(Date, format = "%Y-%m-%d"))

dat_smooth_workI <- smooth_lss(dat_workI, engine = "locfit")

workI_graph <- ggplot(dat_workI, aes(date)) +
  geom_point(aes(y = fit), col = rgb(0, 0, 0, 0.010), pch = 16) +
  geom_line( data = dat_smooth_workI, aes(y = fit), col = "darkred") +
  ylim(c(-0.4,0.4)) + 
  geom_hline(yintercept = 0) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2016-07-15")), 
                 color = "Coup Attempt"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2016-07-20")), 
                 color = "State of Emergency")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2018-01-19")), 
                 color = "Economic Crisis")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2018-07-18")),
                 color = "State o fEmergency")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2018-06-24")), 
                 color = "Election"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2023-05-28")), 
                 color = "Election"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2020-03-11")), 
                 color = "Pandemic")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2022-03-03")), 
                 color = "Pandemic")) +
  scale_color_manual(name = "Breakpoints", 
                     values = c('Coup Attempt' = "red", 'State of Emergency' = "darkolivegreen", 
                                Election = "darkorange2", Pandemic = "darkorchid",
                                'Economic Crisis' = "deepskyblue4")) +
  labs(x = "Time", y = "Cat_I - Work Sentiment") +  
  theme_classic()

################################################################################
#Graph CatI

(bluecard_graph + emigration_graph +
  econI_graph + eduI_graph + socI_graph + workI_graph) +
  plot_layout(guides = "collect", ncol = 2) & theme(legend.position = 'bottom')

(econI_graph + eduI_graph + socI_graph + workI_graph) + 
  plot_layout(guides = "collect", ncol = 2) & theme(legend.position = 'bottom')

################################################################################
################################################################################
#LSS model common part CatII

toks_sentII <- dataII %>% 
  corpus(text_field = "Entry")  %>% 
  corpus_reshape(to =  "sentences")  %>%  
  tokens(remove_punct = TRUE, remove_numbers = TRUE, 
         remove_symbols = TRUE, remove_url = TRUE) %>%
  tokens_tolower() %>%  
  tokens_remove(pattern = stopwords("tr", source = "stopwords-iso")) %>%
  tokens_remove(c("bkz","başlık", "bi", "amk", "lan", "mi", "bok", "ada",
                  "a", "entry"))  %>%
  tokens_wordstem(language = "turkish")

dfmat_sentII <- toks_sentII %>% 
  dfm() %>% 
  dfm_remove(pattern = "") %>% 
  dfm_trim(min_termfreq = 5)

seed <- as.list(lexicon)
seed <- as.seedwords(seed)

###############################################################################
#Economic Sentiment - CatII

eco_econII <- char_context(toks_sentII, pattern = "ekonom*", p = 0.05)

tmod_lss_econII <- textmodel_lss(dfmat_sentII, seeds = seed,
                                terms = eco_econII, k = 300, cache = TRUE)

dfmat_docII <- dfm_group(dfmat_sentII)
dat_econII <- docvars(dfmat_docII)
dat_econII$fit <- predict(tmod_lss_econII, newdata = dfmat_docII)
dat_econII <- dat_econII %>% mutate(date=as.Date(Date, format = "%Y-%m-%d"))

dat_smooth_econII <- smooth_lss(dat_econII, engine = "locfit")

econII_graph <- ggplot(dat_econII, aes(date)) +
  geom_point(aes(y = fit), col = rgb(0, 0, 0, 0.010), pch = 16) +
  geom_line( data = dat_smooth_econII, aes(y = fit), col = "darkred") +
  ylim(c(-0.4,0.4)) + 
  geom_hline(yintercept = 0) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2016-07-15")), 
                 color = "Coup Attempt"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2016-07-20")), 
                 color = "State of Emergency")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2018-01-19")), 
                 color = "Economic Crisis")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2018-07-18")),
                 color = "State o fEmergency")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2018-06-24")), 
                 color = "Election"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2023-05-28")), 
                 color = "Election"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2020-03-11")), 
                 color = "Pandemic")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2022-03-03")), 
                 color = "Pandemic")) +
  scale_color_manual(name = "Breakpoints", 
                     values = c('Coup Attempt' = "red", 'State of Emergency' = "darkolivegreen", 
                                Election = "darkorange2", Pandemic = "darkorchid",
                                'Economic Crisis' = "deepskyblue4")) +
  labs(x = "Time", y = "Cat_II - Economic sentiment") +  
  theme_classic()

################################################################################
#Education Sentiment - CatII

eco_eduII <- char_context(toks_sentII, pattern = "eğit*", p = 0.05)

tmod_lss_eduII <- textmodel_lss(dfmat_sentII, seeds = seed,
                               terms = eco_eduII, k = 300, cache = TRUE)

dfmat_docII <- dfm_group(dfmat_sentII)
dat_eduII <- docvars(dfmat_docII)
dat_eduII$fit <- predict(tmod_lss_eduII, newdata = dfmat_docII)
dat_eduII <- dat_eduII %>% mutate(date=as.Date(Date, format = "%Y-%m-%d"))

dat_smooth_eduII <- smooth_lss(dat_eduII, engine = "locfit")

eduII_graph <- ggplot(dat_eduII, aes(date)) +
  geom_point(aes(y = fit), col = rgb(0, 0, 0, 0.010), pch = 16) +
  geom_line( data = dat_smooth_eduII, aes(y = fit), col = "darkred") +
  ylim(c(-0.4,0.4)) + 
  geom_hline(yintercept = 0) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2016-07-15")), 
                 color = "Coup Attempt"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2016-07-20")), 
                 color = "State of Emergency")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2018-01-19")), 
                 color = "Economic Crisis")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2018-07-18")),
                 color = "State o fEmergency")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2018-06-24")), 
                 color = "Election"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2023-05-28")), 
                 color = "Election"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2020-03-11")), 
                 color = "Pandemic")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2022-03-03")), 
                 color = "Pandemic")) +
  scale_color_manual(name = "Breakpoints", 
                     values = c('Coup Attempt' = "red", 'State of Emergency' = "darkolivegreen", 
                                Election = "darkorange2", Pandemic = "darkorchid",
                                'Economic Crisis' = "deepskyblue4")) +
  labs(x = "Time", y = "Cat_II - Education Sentiment") +  
  theme_classic() 

################################################################################
#Social Sentiment - CatII

eco_socII <- char_context(toks_sentII, pattern = "sosy*", p = 0.05)

tmod_lss_socII <- textmodel_lss(dfmat_sentII, seeds = seed,
                               terms = eco_socII, k = 300, cache = TRUE)

dfmat_docII <- dfm_group(dfmat_sentII)
dat_socII <- docvars(dfmat_docII)
dat_socII$fit <- predict(tmod_lss_socII, newdata = dfmat_docII)
dat_socII <- dat_socII %>% mutate(date=as.Date(Date, format = "%Y-%m-%d"))

dat_smooth_socII <- smooth_lss(dat_socII, engine = "locfit")

socII_graph <- ggplot(dat_socII, aes(date)) +
  geom_point(aes(y = fit), col = rgb(0, 0, 0, 0.010), pch = 16) +
  geom_line( data = dat_smooth_socII, aes(y = fit), col = "darkred") +
  ylim(c(-0.4,0.4)) + 
  geom_hline(yintercept = 0) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2016-07-15")), 
                 color = "Coup Attempt"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2016-07-20")), 
                 color = "State of Emergency")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2018-01-19")), 
                 color = "Economic Crisis")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2018-07-18")),
                 color = "State o fEmergency")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2018-06-24")), 
                 color = "Election"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2023-05-28")), 
                 color = "Election"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2020-03-11")), 
                 color = "Pandemic")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2022-03-03")), 
                 color = "Pandemic")) +
  scale_color_manual(name = "Breakpoints", 
                     values = c('Coup Attempt' = "red", 'State of Emergency' = "darkolivegreen", 
                                Election = "darkorange2", Pandemic = "darkorchid",
                                'Economic Crisis' = "deepskyblue4")) +
  labs(x = "Time", y = "Cat_II - Social Sentiment") +  
  theme_classic()

################################################################################
#Working Life Sentiment 

eco_workII <- char_context(toks_sentII, pattern = "çalış*", p = 0.05)

tmod_lss_workII <- textmodel_lss(dfmat_sentII, seeds = seed,
                                terms = eco_workII, k = 300, cache = TRUE)

dfmat_docII <- dfm_group(dfmat_sentII)
dat_workII <- docvars(dfmat_docII)
dat_workII$fit <- predict(tmod_lss_workII, newdata = dfmat_docII)
dat_workII <- dat_workII %>% mutate(date=as.Date(Date, format = "%Y-%m-%d"))

dat_smooth_workII <- smooth_lss(dat_workII, engine = "locfit")

workII_graph <- ggplot(dat_workII, aes(date)) +
  geom_point(aes(y = fit), col = rgb(0, 0, 0, 0.010), pch = 16) +
  geom_line( data = dat_smooth_workII, aes(y = fit), col = "darkred") +
  ylim(c(-0.4,0.4)) + 
  geom_hline(yintercept = 0) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2016-07-15")), 
                 color = "Coup Attempt"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2016-07-20")), 
                 color = "State of Emergency")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2018-01-19")), 
                 color = "Economic Crisis")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2018-07-18")),
                 color = "State o fEmergency")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2018-06-24")), 
                 color = "Election"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2023-05-28")), 
                 color = "Election"), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2020-03-11")), 
                 color = "Pandemic")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2022-03-03")), 
                 color = "Pandemic")) +
  scale_color_manual(name = "Breakpoints", 
                     values = c('Coup Attempt' = "red", 'State of Emergency' = "darkolivegreen", 
                                Election = "darkorange2", Pandemic = "darkorchid",
                                'Economic Crisis' = "deepskyblue4")) +
  labs(x = "Time", y = "Cat_II - Work Sentiment") +  
  theme_classic()

################################################################################
#Graph CatII

(bluecard_graph + emigration_graph +
   econII_graph + eduII_graph + socII_graph + workII_graph) +
  plot_layout(guides = "collect", ncol = 2) & theme(legend.position = 'bottom')

(econII_graph + eduII_graph + socII_graph + workII_graph) + 
  plot_layout(guides = "collect", ncol = 2) & theme(legend.position = 'bottom')

