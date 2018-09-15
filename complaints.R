library(quanteda); library(tidyverse); library(lubridate)

banks <- c("BANK OF AMERICA, NATIONAL ASSOCIATION",
           "CITIBANK, N.A.",
           "JPMORGAN CHASE & CO.",
           "WELLS FARGO & COMPANY")

df <- read_csv("./data/Consumer_Complaints.csv") %>%
  filter(Company %in% banks) %>%
  rename(text = `Consumer complaint narrative`) %>%
  filter(!is.na(text)) %>%
  mutate(Bank = case_when(
    str_detect(Company, "BANK OF") ~ "BAC",
    str_detect(Company, "CITIBANK") ~ "CITI",
    str_detect(Company, "JPMORGAN") ~ "JPM",
    str_detect(Company, "WELLS") ~ "WFC"
  )) %>%
  mutate(Date = as.Date(`Date received`, format = "%m/%d/%Y")) %>%
  mutate(Date = year(Date)*100 + month(Date)) %>%
  filter(Date != 201809)

df$Product2 <- case_when(
    df$Product %in% c("Consumer Loan","Credit card","Credit card or prepaid card","General-purpose credit card or charge card",
                   "Payday loan","Payday loan, title loan, or personal loan",
                   "Prepaid card", "Student loan", "Vehicle loan or lease") ~ "Card/Personal Loan",
    df$Product %in% c("Bank account or service", "Checking or savings account", "Money transfers",
                   "Money transfer, virtual currency, or money service") ~ "Deposit",
    df$Product %in% c("Credit reporting", "Debt collection", "Other financial service",
                   "Credit reporting, credit repair services, or other personal consumer reports") ~ "Collection/Credit Reporting",
    df$Product == "Mortgage" ~ "Mortgage"
  )

# clean up
df$text <- df$text %>%
  str_replace_all(" n't", "n't") %>%
  str_replace_all(" 's", "'s") %>%
  str_replace_all(" 've", "'ve") %>%
  str_replace_all(" 're", "'re") %>%
  str_replace_all(" 'm", "'m") %>%
  str_replace_all("XXXX", " ")


# topic modeling
myCorpus <- corpus(df$text)
docvars(myCorpus, field = "Bank") <- df$Bank
docvars(myCorpus, field = "Month") <- as.integer(as.factor(df$Date))
docvars(myCorpus, field = "Product") <- df$Product2

stopWords <- c("XX","xxxx","also","said","xxxxxxxxxxxx","account","accounts",
               "bank","of","america","citi","citigroup","citibank",
               "wells","fargo","jp","morgan","chase","jpmorgan",
               "cit","wfc","wf","bofa","boa","bac","jpm")

dfm <- dfm(myCorpus,
           remove = c(stopwords("english"), stopWords),
           ngrams= 1L,
           stem = F,
           remove_numbers = TRUE, 
           remove_punct = TRUE,
           remove_symbols = TRUE)

# EDA
numWords <- 50

topfeatures(dfm, n = numWords)

textplot_wordcloud(dfm,  scale=c(3.5, .75), colors=RColorBrewer::brewer.pal(8, "Dark2"), 
                   random.order = F, rot.per=0.1, max.words=250)

textplot_wordcloud(tfidf(dfm),  scale=c(3.5, .75), colors=RColorBrewer::brewer.pal(8, "Dark2"), 
                   random.order = F, rot.per=0.1, max.words=250)

## cluster
wordDfm <- dfm_sort(dfm_tfidf(dfm))
wordDfm <- t(wordDfm)[1:numWords,]  # keep the top numWords words
wordDistMat <- dist(wordDfm)
wordCluster <- hclust(wordDistMat)
ggdendro::ggdendrogram(wordCluster, rotate = TRUE, size = 2) 

library(stm)

# use quanteda converter to convert our Dfm
stmdfm <- convert(dfm, to = "stm", docvars = docvars(myCorpus))

out <- prepDocuments(stmdfm$documents, stmdfm$vocab, stmdfm$meta, lower.thresh = 5)

k <- 20

stmFit <- stm(out$documents, out$vocab, K = k, 
              prevalence =~ s(Month) + Product + Bank,
              max.em.its = 150, data = out$meta, init.type = "Spectral", seed = 300)

library(tidytext)

# summary

library(ggthemes)

td_gamma <- tidy(stmFit, matrix = "gamma") # no document_names

top_terms <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

# determine topic 

bankLevel <- tibble(document = 1:nrow(out$meta),
                    Bank = out$meta$Bank)

zz <- td_gamma %>%
  inner_join(bankLevel, by = "document") %>%
  group_by(topic, Bank) %>%
  summarise(gamma = mean(gamma)) %>%
  spread("Bank", "gamma")


topics <- tibble(
  TopicNumber = 1:k,
  topic = as.factor(paste0("Topic ", 1:k)),
  Bank = colnames(zz[,2:5])[apply(zz[,2:5],1,which.max)],
  TopicProportions = colMeans(stmFit$theta))

colors <- c("#DC1431", # bac
            "#003FA9", # citi
            "#777777", # jpm
            "#FFCC11" # wfc
            ) # jpm

gamma_terms %>%
  inner_join(topics, by = "topic") %>%
  top_n(20, gamma) %>%
  ggplot(aes(forcats::fct_reorder(topic, gamma), gamma,
             label = terms)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 3,
            family = "IBMPlexSans") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.2),
                     labels = scales::percent_format()) +
  theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
  theme(plot.title = element_text(size = 16,
                                  family="IBMPlexSans-Bold"),
        plot.subtitle = element_text(size = 13)) +
  labs(x = NULL, y = "Expected Topic Proportion",
       title = "Top 20 topics by prevalence in the CFPB Complaints",
       subtitle = "With the top words that contribute to each topic")


## topics

td_beta <- tidy(stmFit, matrix = "beta")

# helper functions (from David Robinson's R Package)
scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

# Examine the topics
td_beta %>%
  group_by(topic) %>%
  top_n(8, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = topic)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 4, scales = "free_y") +
  theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
  scale_x_reordered() +
  theme_bw() +
  coord_flip() +
  labs(x = "",
       y = "")

# get topic 18

td_beta %>%
  filter(topic == 18) %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = topic)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 4, scales = "free_y") +
  theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_reordered() +
  theme_bw() +
  coord_flip() +
  labs(x = "",
       y = "Likelihood of Word given Topic")


## Prep

prep <- estimateEffect(1:k ~ Product + s(Month) + Bank, stmFit, 
                       meta = out$meta, 
                       uncertainty = "Global")

source("functions.R")

# do time series

timeDf %>%
  ggplot() +
  # geom_rect(data=data.frame(xmin=ymd_hm("2016-09-20 18:00", tz = "EST"),
  #                           xmax=ymd_hm("2016-09-21 02:00", tz = "EST"),
  #                           ymin=-Inf,
  #                           ymax=Inf),
  #           aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
  #           fill="red",alpha=0.2) +
  geom_line(aes(x = time, y = adjValue)) + 
  geom_ribbon(aes(x = time, ymin=adjlcl,ymax=adjucl),alpha=0.3) +
  facet_wrap(~Topic) +
  theme_bw() +
  #theme_tufte(base_family = "IBMPlexSans", ticks = FALSE) +
  #facet_wrap(~Topic, scales = "free_y") +
  labs(x = " ", y = " ", title = "Expected Complaints per Topic", 
       subtitle = "Topic Probability * Total Complaints") +
  theme(axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))

# do bank

t <- purrr::map_df(c("JPM","CITI","WFC"), getDifference)


top_terms2 <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest() %>%
  mutate(terms = paste0(terms, " (",topic,")"))

ord <- c("fraud, claim, fraudulent, charges, card (10)", 
         "property, mortgage, court, note, documents (8)", 
         "fees, fee, charged, balance, overdraft (14)",  # bac
        "told, called, call, back, asked (3)", 
        "letter, received, complaint, information, request (4)", 
        "interest, balance, rate, amount, statement (13)", # even
        "insurance, car, company, paid, vehicle (6)", 
        "payment, payments, late, due, pay (12)", 
        "us, years, husband, house, wife (15)") # competitor

t %>%
  inner_join(top_terms2, by = c("Topic" = "topic")) %>%
  mutate(Topic = paste0("Topic ",Topic)) %>%
  filter(terms %in% ord) %>% 
  mutate(terms = factor(terms, levels = ord)) %>%
  ggplot() +
  geom_pointrange(aes(x = Bank, y = Mean, ymin = LCL, ymax = UCL),
                  fatten = 3) +
  coord_flip() +
  labs(x = " ", y = " ", title = "Effect of Bank (Relative to BAC) of Topic Size",
       subtitle = "With 99% C.I. estimates") +
  geom_hline(yintercept = 0, color = "darkgrey", linetype = 2) +
  annotate("text", x = 0.6, y = -0.018, label = "More like BAC", size = 3, color = "#DC1431") +
  annotate("text", x = 0.6, y = 0.016, label = "More like Competitor", size = 3) +
  facet_wrap(~terms) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "none") +
  ylim(-0.03,0.03)


# total

monthCounts <- prep$data %>% 
  count(Month, Bank) %>%
  ungroup() %>%
  mutate(newTime = ymd("2015-03-01", tz = "EST") + months(Month))

monthCounts %>%
  ggplot(aes(x = newTime, y = n, color = Bank)) +
  geom_line(size = 2) +
  geom_point(color = "white", size = 0.5) +
  theme_bw() +
  labs(x = " ", y = " ", 
       title = "CFPB Complaint Counts by Bank",
       subtitle = "Excludes Complaints without a Narrative") +
  facet_wrap(~Bank) +
  scale_color_manual(values = colors) +
  theme(legend.position = "none") +
  theme(text = element_text(size=16))


getReps <- function(t, n){
  reps <- td_gamma %>%
    filter(topic == t) %>%
    arrange(desc(gamma)) %>%
    head(n=n)
  
  z <- df[c(-795,-13205,-29309),]
  z <- z[reps$document,]
  
  z <- tibble(
    Date = z$`Date received`,
    Bank = z$Bank,
    `Topic Prob` = round(reps$gamma[1:n],3),
    Narrative = paste0(trimws(substr(z$text,1,70))," ... ")
  )
  return(z)
}

# Get Representative (change 18)
write_rds(getReps(18, n = 100), "./data/reps.rds")

# save image

save.image("./data/stmRun.Rdata")
