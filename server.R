##########################
#
# Word Prediction WebApp - server.R
# --------------
#
# Author: Thomas Frohwein (thfr@)
#
##########################

#load the libraries
library(shiny)
library(SnowballC)

#load the data in the form of the prepared ngrams
#unigrams001_stem <- read.csv("unigrams_.001_stem.csv", stringsAsFactors = FALSE)
#bigrams001_stem <- read.csv("bigrams_.001_stem.csv", stringsAsFactors = FALSE)
#trigrams001_stem <- read.csv("trigrams_.001_stem.csv", stringsAsFactors = FALSE)
#tetragrams001_stem <- read.csv("tetragrams_.001_stem.csv", stringsAsFactors = FALSE)
#pentagrams001_stem <- read.csv("pentagrams_.001_stem.csv", stringsAsFactors = FALSE)
#unigrams001_nostem <- read.csv("unigrams_.001_nostem.csv", stringsAsFactors = FALSE)
#bigrams001_nostem <- read.csv("bigrams_.001_nostem.csv", stringsAsFactors = FALSE)
#trigrams001_nostem <- read.csv("trigrams_.001_nostem.csv", stringsAsFactors = FALSE)
#tetragrams001_nostem <- read.csv("tetragrams_.001_nostem.csv", stringsAsFactors = FALSE)
#pentagrams001_nostem <- read.csv("pentagrams_.001_nostem.csv", stringsAsFactors = FALSE)
#unigrams01_stem <- read.csv("unigrams_.01_stem.csv", stringsAsFactors = FALSE)
#bigrams01_stem <- read.csv("bigrams_.01_stem.csv", stringsAsFactors = FALSE)
#trigrams01_stem <- read.csv("trigrams_.01_stem.csv", stringsAsFactors = FALSE)
#tetragrams01_stem <- read.csv("tetragrams_.01_stem.csv", stringsAsFactors = FALSE)
#pentagrams01_stem <- read.csv("pentagrams_.01_stem.csv", stringsAsFactors = FALSE)
unigrams01_nostem <- read.csv("unigrams_.01_nostem.csv", stringsAsFactors = FALSE)
bigrams01_nostem <- read.csv("bigrams_.01_nostem.csv", stringsAsFactors = FALSE)
trigrams01_nostem <- read.csv("trigrams_.01_nostem.csv", stringsAsFactors = FALSE)
tetragrams01_nostem <- read.csv("tetragrams_.01_nostem.csv", stringsAsFactors = FALSE)
pentagrams01_nostem <- read.csv("pentagrams_.01_nostem.csv", stringsAsFactors = FALSE)

#arrange the data for processing
#unigrams001_stem <- unigrams001_stem[,2:3]
#bigrams001_stem <- bigrams001_stem[,2:3]
#trigrams001_stem <- trigrams001_stem[,2:3]
#tetragrams001_stem <- tetragrams001_stem[,2:3]
#pentagrams001_stem <- pentagrams001_stem[,2:3]
#unigrams001_nostem <- unigrams001_nostem[,2:3]
#bigrams001_nostem <- bigrams001_nostem[,2:3]
#trigrams001_nostem <- trigrams001_nostem[,2:3]
#tetragrams001_nostem <- tetragrams001_nostem[,2:3]
#pentagrams001_nostem <- pentagrams001_nostem[,2:3]
#unigrams01_stem <- unigrams01_stem[,2:3]
#bigrams01_stem <- bigrams01_stem[,2:3]
#trigrams01_stem <- trigrams01_stem[,2:3]
#tetragrams01_stem <- tetragrams01_stem[,2:3]
#pentagrams01_stem <- pentagrams01_stem[,2:3]
unigrams01_nostem <- unigrams01_nostem[,2:3]
bigrams01_nostem <- bigrams01_nostem[,2:3]
trigrams01_nostem <- trigrams01_nostem[,2:3]
tetragrams01_nostem <- tetragrams01_nostem[,2:3]
pentagrams01_nostem <- pentagrams01_nostem[,2:3]

#assign the default ngram set
unigrams <- unigrams01_nostem
bigrams <- bigrams01_nostem
trigrams <- trigrams01_nostem
tetragrams <- tetragrams01_nostem
pentagrams <- pentagrams01_nostem

#load functions that are necessary for the prediction

clean <- function(x) {
        x <- gsub(" & ", " and ", x)
        x <- gsub("â€™", "'", x)
        x <- gsub("([A-Za-z])'([^A-Za-z])", "\\1\\2", x)
        x <- gsub("(^|\\s)'([A-Za-z])", "\\1\\2", x)
        x <- gsub("(\\s)-", "\\1", x)
        x <- gsub("(^|\\s)u(\\s)", "\\1you\\2", x)
        x <- gsub("([#$^%\\*â€œÃ©]|-|<|>|Œ|º|™|`|ï|¿|½|ð|Ÿ|¦|&|/|\\{|\\}|–|;|@|_|~|—|,|\\(|\\)|\\[|\\]|”|\002L|ˆ|‚|ž|\\||¡|¤|¬|…|¥|˜|¨|±|®|†|‰|¹|²|³|ƒ|Š|‘|’|‹|›|°|š|¢|„|\\+|=|§|‡|¸|«|´|¼|Â|¯|“|´|•|£|»)", " ", x)
        x <- gsub("\"", " ", x)
        x <- gsub("https?://([A-Za-z0-9\\./?#])*", " ", x, ignore.case = TRUE)
        x <- gsub("www\\.[A-Za-z0-9\\./?#]*", " ", x, ignore.case = TRUE)
        x <- gsub("([0-9])\\.([0-9])", "\\1\\2", x)
        x <- gsub("([A-Z])\\.", "\\1", x)
        x <- gsub("(^|\\s)[Ee]\\.g\\.", "\\1eg", x)
        x <- gsub("(^|\\s)[Ii]\\.e\\.", "\\1ie", x)
        x <- gsub("(^|\\s)Ph\\.D\\.", "\\1PhD", x)
        x <- gsub("(^|\\s)(etc)\\.", "\\1\\2", x, ignore.case = TRUE)
        x <- gsub("[0-9]+([.,][0-9]+)*", " <NUM> ", x)
        x <- gsub("[0-9]+(st|th|nd)(\\s|\\t|\\.|!|\\?|:)", " <NUM> \\2", x)
        x <- gsub("(^|\\s)(ain[^a-z]?t|isn[^a-z]?t|aren[^a-z]?t)", "\\1is not", x, ignore.case = TRUE)
        x <- gsub("(^|\\s)[Dd]on[^a-z]?t", "\\1do not", x, ignore.case = TRUE)
        x <- gsub("(^|\\s)[Dd]oesn[^a-z]?t", "\\1does not", x, ignore.case = TRUE)
        x <- gsub("(^|\\s)[Dd]idn[^a-z]?t", "\\1did not", x, ignore.case = TRUE)
        x <- gsub("(^|\\s)[Ii][^a-z]ll", "\\1I will", x, ignore.case = TRUE)
        x <- gsub("(^|\\s)[Ii][^a-z]m(\\s)", "\\1I am\\2", x, ignore.case = TRUE)
        x <- gsub("(^|\\s)[Yy]ou[^a-z]re(\\s)", "\\1you are\\2", x, ignore.case = TRUE)
        x <- gsub("(^|\\s)[Hh]e[^a-z]s(\\s)", "\\1he is\\2", x, ignore.case = TRUE)
        x <- gsub("(^|\\s)[Ss]he[^a-z]s(\\s)", "\\1she is\\2", x, ignore.case = TRUE)
        x <- gsub("(^|\\s)[Ii]t[^a-z]s(\\s)", "\\1it is\\2", x, ignore.case = TRUE)
        x <- gsub("(^|\\s)[Ww]e[^a-z]re(\\s)", "\\1we are\\2", x, ignore.case = TRUE)
        x <- gsub("(^|\\s)[Tt]hey[^a-z]re(\\s)", "\\1they are\\2", x, ignore.case = TRUE)
        x <- gsub("(^|\\s)[Cc]an[^a-z]t(\\s)", "\\1can not\\2", x, ignore.case = TRUE)
        x <- gsub("(^|\\s)[Ww]on[^a-z]t(\\s)", "\\1will not\\2", x, ignore.case = TRUE)
        x <- gsub("(^|\\s)[Tt]here[^a-z]s(\\s)", "\\1there is\\2", x, ignore.case = TRUE)
        x <- gsub("(^|\\s)[Ii][^a-z]d(\\s)", "\\1I would\\2", x, ignore.case = TRUE)
        return(x)
}

sentencer <- function(x) {
        # This function will mark beginning and/or end of sentences with <STOP>, replacing the function of the punctuation. Especially the period can be very ambiguous.
        # First, insert <STOP> at the beginning and end of every element of the corpus.
        x <- gsub("([A-Za-z0-9])\\.([A-Z][a-z])", "\\1 <STOP> \\2", x)
        x <- gsub("([a-z]{2,})\\.([A-Z]\\s)", "\\1 <STOP> \\2", x)
        x <- gsub("([A-Za-z0-9])\\.([A-Za-z]{5,})", "\\1 <STOP> \\2", x, ignore.case = TRUE)
        x <- gsub("[\\.!?:]*[!?:][\\.!?:]*", " <STOP> ", x)
        x <- gsub("\\t", " <STOP> ", x)
        x <- gsub("\\.( +[A-Z])", " <STOP>\\1", x)
        x <- gsub("([|)|}|\"|'])\\.", "\\1 <STOP> ", x)
        x <- gsub("\\.([\"|'])", " <STOP> \\1", x)
        x <- gsub("\\.{2,}", " <STOP> ", x)
        x <- gsub("^(.)", "<STOP> \\1", x)
        x <- gsub("(.)(\\.|\\?|!|\\s)*$", "\\1 <STOP>", x)
        return(x)
}

cleanTokens <- function(x) {
        # remove empty elements
        x <- x[nchar(x) > 0]
        # remove elements without alphanumeric characters
        x <- x[grepl("[A-Za-z0-9]", x)]
        # remove excessive symbol use
        x <- gsub("('|-)+", "\\1", x)
        # this removes periods in 3 or more letter abreviations (U.S.A., M.A.S.H.)
        x <- gsub("([^A-Za-z0-9]|^)([A-Za-z0-9])\\.([A-Za-z0-9])([^A-Za-z0-9]|$)", "\\2\\3", x)
        # remove symbols and whitespace at the beginnings and ends (except the markers <STOP> and <NUM>)
        x <- gsub("^('|-|\\.|\\s| )+", "", x)
        x <- gsub("^[^A-Za-z0-9<]([A-Za-z0-9])", "\\1", x)
        x <- gsub("[^A-Za-z0-9>]$", "", x)
        # remove dashes at the end of tokens
        x <- gsub("(-|\\.)+$", "", x)
        x <- gsub("^(a|p)\\.m$", "\\1m", x, ignore.case = TRUE)
        # in web addresses remove the "." and replace it with "dot"
        x <- gsub("\\.(com|net|org|de)(\\s|$)", "dot\\1\\2", x, ignore.case = TRUE)
        x <- gsub("\\.(co)\\.(uk)(\\s|$)", "dot\\1dot\\2\\3", x, ignore.case = TRUE)
        # Remove . and after completely if only one lower-case letter
        x <- gsub("([A-Za-z0-9]{2,})\\.[a-z]", "\\1", x)
        # reduce multiple <STOP> in a row to only one
        removeStop <- logical(length = length(x))
        for(i in seq_along(x)) {
                if(i > 1 && x[i] == "<STOP>" && x[i-1] == "<STOP>"){
                        removeStop[i] <- TRUE
                }
        }
        x <- x[!removeStop]
        return(x)
}

normalize <- function(x, stem = TRUE) {
        # Commonly used abbreviations can be resolved into their full words in order to analyze them together with the unabreviated occurrences
        x <- gsub("(^|\\s)dr(\\.|$)", "\\1doctor", x, ignore.case = TRUE)
        x <- gsub("(^|\\s)ms(\\.|$)", "\\1Miss", x, ignore.case = TRUE)
        x <- gsub("(^|\\s)mr(\\.|$)", "\\1Mister", x, ignore.case = TRUE)
        x <- gsub("(^|\\s)mrs(\\.|$)", "\\1Misses", x, ignore.case = TRUE)
        if(stem == TRUE) {
                x <- gsub("^(am|are|is|was|were)$", "be", x, ignore.case = TRUE)
                x <- gsub("^(have|has|had)$", "hav", x, ignore.case = TRUE)
                x <- gsub("^(does|did)$", "do", x, ignore.case = TRUE)
                # because of some overzealousness of wordStem, a list of words to exclude from the stemming
                exclu <- c("as")
                for(i in seq_along(x)) {
                        if(!is.element(x[i], exclu)) {
                                x[i] <- wordStem(x[i])
                        }
                }
        }
        return(x)
}

preprocess <- function(x, stem = TRUE) {
        x <- clean(x) # takes < 2 minutes with a character of 58 Mb (330,000 elements)
        x <- sentencer(x) # takes < 2 minutes with a character of 58 Mb (330,000 elements)
        # tokenize the training set using strsplit (a lot faster than the tokenize function in the package tau)
        x <- unlist(lapply(x, function(y){strsplit(y, split = " ")})) # takes < 1 minute
        
        # clean up after tokenization - remove "", " ", other whitespace-only tokens, and reduce series of <STOP> to only one, to make it easier for the computer
        x <- cleanTokens(x)
        # all periods and ' that are left need to be removed and a this point will just be replaced by whitespace and then tokenization and token cleaning need to be applied again because of the new whitespace
        x <- gsub("(\\.|')", " ", x)
        x <- unlist(lapply(x, function(y){strsplit(y, split = " ")}))
        x <- cleanTokens(x)
        # normalize the words by transformation of common inflections and stemming
        x <- normalize(x, stem)
        # remove any remaining empty elements
        x <- x[nchar(x) > 0]
        # collapse several <NUM> in a row into one
        removeNum <- logical(length = length(x))
        for(i in seq_along(x)) {
                if(i > 1 && x[i] == "<NUM>" && x[i-1] == "<NUM>"){
                        removeNum[i] <- TRUE
                }
        }
        x <- x[!removeNum]
        return(x)
}

# this is the continuation probability function for BIGRAMS
p_continuation <- function(term) {
        p <- sum(grepl(paste0("\\.", term, "$"), bigrams[,1])) / dim(bigrams)[1]
        return(p)
}

# discount constant for smoothing
d <- 0.75

# pkn = Kneser-Ney probability
pkn_bigram <- function(term1, term2) {
        denom <- sum(unigrams[grepl(term1, unigrams[,1]), 2])
        string <- paste0(term1, "\\.", term2)
        lambda <- (d / denom) * sum(grepl(paste0("^", term1, "\\."), bigrams[,1]))
        pkn <- ( max((bigrams[grepl(string, bigrams[,1]), 2] - d), 0) / denom ) + lambda * p_continuation(term2)
        return(pkn)
}

pkn_trigram <- function(term1, term2, term3) {
        pkn <- ( max((trigrams[grepl(paste0(term1, "\\.", term2, "\\.", term3), trigrams[,1]), 2] - d), 0) / sum(bigrams[grepl(paste0(term1, "\\.", term2), bigrams[,1]), 2]) ) + ( (d / sum(bigrams[grepl(paste0(term1, "\\.", term2), bigrams[,1]), 2])) * sum(grepl(paste0("^", term1, "\\.", term2, "\\."), trigrams[,1])) ) * pkn_bigram(term2, term3)
        return(pkn)
}

pkn_tetragram <- function(term1, term2, term3, term4) {
        pkn <- ( max((tetragrams[grepl(paste0(term1, "\\.", term2, "\\.", term3, "\\.", term4), tetragrams[,1]), 2] - d), 0) / sum(trigrams[grepl(paste0(term1, "\\.", term2, "\\.", term3), trigrams[,1]), 2]) ) + ( (d/sum(trigrams[grepl(paste0(term1, "\\.", term2, "\\.", term3), trigrams[,1]), 2])) * sum(grepl(paste0("^", term1, "\\.", term2, "\\.", term3, "\\."), tetragrams[,1])) ) * pkn_trigram(term2, term3, term4)
}

pkn_pentagram <- function(term1, term2, term3, term4, term5) {
        pkn <- ( max((pentagrams[grepl(paste0(term1, "\\.", term2, "\\.", term3, "\\", term4, "\\.", term5), pentagrams[,1]), 2] -d), 0) / sum(tetragrams[grepl(paste0(term1, "\\.", term2, "\\.", term3, "\\.", term4), tetragrams[,1]), 2]) ) + ( (d/sum(tetragrams[grepl(paste0(term1, "\\.", term2, "\\.", term3, "\\.", term4), trigrams[,1]), 2])) * sum(grepl(paste0("^", term1, "\\.", term2, "\\.", term3, "\\.", term4, "\\."), pentagrams[,1])) ) * pkn_tetragram(term2, term3, term4, term5)
}

predictString <- function(string, quick_bigram = FALSE, stem = TRUE) {
        string <- tolower(preprocess(string, stem))
        if(length(string) < 3) stop("no prediction was possible with your entry")
        # remove the <STOP> at the beginning and the end of the string
        string <- string[2:(length(string) - 1)]
        if(length(string) > 3) {
                string <- string[(length(string) -3):(length(string))]
                rxstring <- paste0(string, collapse = "\\.")
                rxstring <- paste0("^", rxstring, "\\.", "([^\\.]*)$")
                words <- pentagrams[,1][grepl(rxstring, pentagrams[,1])]
                words <- gsub(rxstring, "\\1", words)
                if(length(words) == 0) {
                        string <- string[2:length(string)]
                } else {
                        prob <- sort(sapply(words, function(x) pkn_pentagram(string[1], string[2], string[3], string[4], x)), decreasing = TRUE)
                }
        }
        if(length(string) == 3) {
                rxstring <- paste0(string, collapse = "\\.")
                rxstring <- paste0("^", rxstring, "\\.", "([^\\.]*)$")
                words <- tetragrams[,1][grepl(rxstring, tetragrams[,1])]
                words <- gsub(rxstring, "\\1", words)
                if(length(words) == 0) {
                        string <- string[2:length(string)]
                } else {
                        prob <- sort(sapply(words, function(x) pkn_tetragram(string[1], string[2], string[3], x)), decreasing = TRUE)
                }
        }
        if(length(string) == 2) {
                rxstring <- paste0(string, collapse = "\\.")
                rxstring <- paste0("^", rxstring, "\\.", "([^\\.]*)$")
                words <- trigrams[,1][grepl(rxstring, trigrams[,1])]
                words <- gsub(rxstring, "\\1", words)
                if(length(words) == 0) {
                        string <- string[2:length(string)]
                } else {
                        prob <- sort(sapply(words, function(x) pkn_trigram(string[1], string[2], x)), decreasing = TRUE)
                }
        }
        if(length(string) == 1) {
                rxstring <- paste0("^", string, "\\.", "([^\\.]*)$")
                #this is a feature to speed up the prediction with only one given word by discarding all bigrams that occurred only once
                bigrams_in_use <- bigrams
                if(quick_bigram == TRUE) {
                        bigrams_in_use <- bigrams[bigrams[,2] > 1, ]
                }
                words <- bigrams_in_use[,1][grepl(rxstring, bigrams_in_use[,1])]
                words <- gsub(rxstring, "\\1", words)
                if(length(words) == 0) {
                        stop("no prediction was possible with your entry")
                } else {
                        prob <- sort(sapply(words, function(x) pkn_bigram(string, x)), decreasing = TRUE)
                }
        }
        if(length(prob) == 0) stop("no prediction was possible with your entry")
        return(prob)
}

nextWord <- function(string, n = 1, quick_bigram = FALSE, stem = TRUE) {
        return(names(predictString(string, quick_bigram = quick_bigram, stem = stem)[1:n]))
}

######################################################################

# Start the shiny server which contains the reactive functions that process the input
# input$* and create the output (output$*).

shinyServer(function(input, output) {

	output$output_text <- renderText({
		as.character(nextWord(input$input_text, n = 1, quick_bigram = TRUE, stem = FALSE))
	        #hist(data_sr[,7], col = 'darkgray', border = 'white', breaks = 20, xlab = "Number of Cases", ylab = "", yaxt='n', xlim=c(0, 800), main = "CABG procedures in 2011 (Histogram)")
		#abline(v = data_sr[data_sr$Hospital.Name == input$hosp, 7], col = "red", lwd = 3)
	})
})
