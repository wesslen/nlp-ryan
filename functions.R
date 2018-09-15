
getDifference <- function(level, k = 20){
  z <- plot(prep, method = "difference", covariate = "Bank",
            cov.value1 = level, cov.value2 = "BAC", ci.level = 0.999)
  
  cis <- data.frame(matrix(unlist(z$cis), nrow=k, byrow=T))
  
  t <- tibble(
    Topic = 1:k,
    Bank = level,
    Mean = unlist(z$means),
    LCL = cis$X1,
    UCL = cis$X2
  )
}

# run code to get ggplot2

x <-  plot(prep, "Month", method = "continuous", 
           printlegend = FALSE, ylab = "Exp. Topic Prob", 
           ylim = c(0,0.1),
           ci.level = 0.99,
           npoints = 42
)


topicTime <- t(do.call(rbind.data.frame, x$means)) %>%
  as.tibble()

colnames(topicTime) <- paste0("Topic ",1:20)
topicTime$time <- ymd("2015-03-01", tz = "EST") + months(0:41)

topicCI <- t(do.call(rbind.data.frame, x$ci)) %>%
  as.tibble() %>%
  mutate(time = ymd("2015-03-01", tz = "EST") + months(0:41))

topicCI <- topicCI %>%
  gather("ID","Value", -time) 

topicCI$ID <- gsub("0.5%", "lcl-", topicCI$ID)
topicCI$ID <- gsub("99.5%", "ucl-", topicCI$ID)

topicCI$ID[topicCI$ID=="ucl-"] <- "ucl-0"
topicCI$ID[topicCI$ID=="lcl-"] <- "lcl-0"

topicCI <- separate(topicCI, "ID", into = c("ci","Topic"), sep = "-")
topicCI$Topic <- paste0("Topic ",as.integer(topicCI$Topic) + 1)

topicCI <- spread(topicCI, ci, Value, fill = 0)


monthCounts <- prep$data %>% 
  count(Month) %>%
  ungroup() %>%
  mutate(newTime = ymd("2015-03-01", tz = "EST") + months(Month))


#colnames(topicTime) <- c(shortNames$shortNames, "time")

order <- topics %>%  
  mutate(TopicNumber = paste0("Topic ",TopicNumber)) %>%
  arrange(desc(TopicProportions)) %>%
  select(TopicNumber)


# three word topic names
temp <- sageLabels(stmFit, n = 2)

shortNames <- sapply(1:20, function(x) paste0(temp$marginal$prob[x,], collapse = " + "))
shortNames <- tibble(shortNames = paste0(shortNames, " (",1:20,")"),
                     TopicNumber = paste0("Topic ",1:20))

order <- left_join(order, shortNames, by = "TopicNumber")

timeDf <- topicTime %>%
  gather("Topic", "Value", -time) %>%
  inner_join(topicCI, by = c("time" = "time", "Topic" = "Topic")) %>%
  mutate(Topic = factor(Topic, levels = order$TopicNumber, labels = order$shortNames))

timeDf$adjValue <- timeDf$Value * rep(monthCounts$n, 20)
timeDf$adjucl <- timeDf$ucl * rep(monthCounts$n, 20)
timeDf$adjlcl <- timeDf$lcl * rep(monthCounts$n, 20)
