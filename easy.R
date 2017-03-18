##Anant Gowadiya
##Project : Sentometrics

#Easy task Solution
require("NLP")
require("tm")
require("tm.lexicon.GeneralInquirer")
require("ggplot2")
#Creating a Function to calculate sentiment of a set of text document
sentimet_plot <- function(doc, n , sentiment = "positive") {
  #calculating sentiment score
  positive_score  <- tm_term_score(
    TermDocumentMatrix(doc[1:n],
                       control = list(removePunctuation = TRUE)),
    terms_in_General_Inquirer_categories("Positiv")
  )
  negative_score  <- tm_term_score(
    TermDocumentMatrix(doc[1:n],
                       control = list(removePunctuation = TRUE)),
    terms_in_General_Inquirer_categories("Negativ")
  )
  #getting the time dimension i.e. when the document was created
  FLAG <- F
  for (i in 1:n) {
    if (FLAG == F) {
      t <- doc[[i]]$meta$datetimestamp
      FLAG <- T
    }
    else{
      t <- c(t, (doc[[i]]$meta$datetimestamp))
    }
  }
  #plotting the sentiment score with time
  if (sentiment == "positive")
  {
    p1 <-
      ggplot(aes(x = t, y = positive_score), data = data.frame(t, positive_score)) + geom_point(colour = "blue") +
      labs(list(title = "Sentiment Plot", y = "Positive Sentiment", x = "Time")) +
      theme(axis.line = element_line(
        colour = "darkblue",
        size = 1,
        linetype = "solid"
      ))
    p1
  }
  else if (sentiment == "negative") {
    p2 <-
      ggplot(aes(x = t, y = negative_score), data = data.frame(t, negative_score)) + geom_point(colour = "red") +
      labs(list(title = "Sentiment Plot", y = "Negative Sentiment", x = "Time")) +
      theme(axis.line = element_line(
        colour = "darkred",
        size = 1,
        linetype = "solid"
      ))
    
    p2
  }
}
#testing the sentiment_plot function
data("acq")
sentimet_plot(acq, n = 17)
data("crude")
sentimet_plot(crude, n = 20, "negative")
