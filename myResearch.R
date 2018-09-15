library(quanteda)

df <- readr::read_csv("./data/abstracts.csv")

myCorpus <- corpus(df$Abstract)
docvars(myCorpus, field = "Theme") <- df$Theme
#myCorpus <- corpus_reshape(myCorpus, to = "sentences")

stopWords <- c("can","use","uses","used","using","study","studies","first","second","third","also","across","results",
               "result","resulted","may","however","one","two","three","four","five","among","well","within","many","related",
               "i.e","e.g","e.g.","i.e.","find","finding","finds","found","increase","increases","increasing","increased","decreased",
               "decrease","decreases","decreasing","propose","proposal","proposals","proposes","proposed","new","old",
               "differ","differs","different","difference","differences","positive","negative","findings","reports",
               "report","reported","state","states","article","articles","examines","examine","suggest","research",
               "researches","researchers","need","needs","show","shows","association","associations","associated",
               "discuss","discusses","discussed","will","likely","unlikely","paper","method","methods",
               "compared","specifically","approach","impact","impacts","examine","examined","examines","includes",
               "include","included","including","measure","measures","measured","analysis","analyze","analyses","complete",
               "completes","completed","indicate","indicated","indicates","high","higher","low","lower","follow","follows",
               "following","significant","significance","approach","approaches","approached","demonstrate",
               "demonstrated","demonstrates","yet","best","worst","better","large","small","larger","smaller","several","few",
               "much","less","given","via","long","short","often","years","along","whether","potential","significantly",
               "influence","influenced","influences","develop","develops","developed","good","bad","based","p","group",
               "groups","effect","affect","affects","effects","sample","samples","relationship","relationships","change","changes",
               "m","k","conclusion","conclusions","present","presents","presented","describe","regarding",
               "novel","reveal","unclear","designed")

dfm <- dfm(myCorpus,
           groups = "Theme",
           remove = c(stopwords("english"),stopWords),
           ngrams= 1L,
           stem = F,
           remove_numbers = TRUE, 
           remove_punct = TRUE,
           remove_symbols = TRUE)

quanteda::textplot_wordcloud(dfm, comparison = TRUE, min_size = 0.8, title.size = 2,
                             colors = RColorBrewer::brewer.pal(3,"Dark2"))

toptag <- names(topfeatures(dfm, 50))
tag_fcm <- fcm(dfm)
topgat_fcm <- fcm_select(tag_fcm, toptag)
textplot_network(topgat_fcm, min_freq = 0.4, edge_alpha = 0.2, edge_size = 3,
                 vertext_color = "#4D4D4D", edge_color = "#1F78B4")

library(stm)

# use quanteda converter to convert our Dfm
stmdfm <- convert(dfm, to = "stm")

out <- prepDocuments(stmdfm$documents, stmdfm$vocab, stmdfm$meta, lower.thresh = 2)

stmFit <- stm(out$documents, out$vocab, K = 10, max.em.its = 250, data = out$meta, init.type = "Spectral", seed = 300)

plot(stmFit, 
     type = "summary", 
     xlim = c(0,.35), 
     n = 6, 
     labeltype = "prob",
     main = "Ryan Topics", 
     text.cex = 0.8)
