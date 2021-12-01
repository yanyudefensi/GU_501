####### Twitter in R
## Gates
##
## For DCR 2019


## GET A TWITTER DEV ACCOUNT
## RE:
## https://drive.google.com/file/d/10uwqGiGLQmi8r3LcK4GlnbDYvvf5xYyr/view?usp=sharing


#########################################
#  Consumer API keys
#  Access token & access token secret

## I have created a text file that contains the
## consumerKey, the comsumerSecret, the access_Token, and the access_Secret
## In Twitter - these have other names and things change a lot in Twitter.
## In Twitter:
## consumerKey  is now called the  API key
## comsumerSecret  is now called the    API key secret
## access_Token  is now called the whatever it is called now...its the first hidden one
## access_Secret  is now called the whatever it is called now...its the second hidden one


## IT DOES NOT MATTER WHAT YOU NAME THEM :) As long as the order is the same


## They are comma separated.
## The name of my file is TwitterConKey_ConSec_AccTok_AccSec.txt
#Insert your consumerKey and consumerSecret below


############################
## GET A TWITTER ACCOUNT
##
##  https://developer.twitter.com/en
##  Choose DEVELOPER PORTAL
##  https://developer.twitter.com/en/dashboard
##
###########################################
#setwd("C:/Users/profa/Documents/R/RStudioFolder_1/DCR2019Networks/")
#C:\Users\profa\Documents\RStudioFolder_1\DCR2019Networks

#TwitterCodesFile="C:/users/profa/desktop/TwitterCodesFileCopy.txt"
# TwitterCodesFile="C:/Users/profa/Documents/RStudioFolder_1/DCR2019Networks/TwitterConKey_ConSec_AccTok_AccSec_10_10_21.txt"
# (tokens<-read.csv(TwitterCodesFile, header=T, sep=","))

tokens_consumerKey <- 'jh2Qd23IZiVtwkgwI3IQH7SHM'
tokens_consumerSecret <- 'F2U3558faqCHNKofqBZjO1eFDnS4hvBv5L5MPFXgEOfcIOHxo4'
tokens_access_Token <- '1434201553552957445-xQ6G9ljDsi94tnUuY6QIbvI5oCgNvh'
tokens_access_Secret <- 'ob04bLUhi41u1gSQoo2B55URRj4ILavd92haxaoX06jeX'

(consumerKey=as.character(tokens_consumerKey))
(consumerSecret=as.character(tokens_consumerSecret))
(access_Token=as.character(tokens_access_Token))
(access_Secret=as.character(tokens_access_Secret))


requestURL='https://api.twitter.com/oauth/request_token'
accessURL='https://api.twitter.com/oauth/access_token'
authURL='https://api.twitter.com/oauth/authorize'


########################
## I have commented out many things
## LEAVE them commented out unless you have
## issues. Then, you can try to see what
## will solve the issue.
###################################################
#oauth_endpoint(authorize = "https://api.twitter.com/oauth",
#               access = "https://api.twitter.com/oauth/access_token")

#connect to API
#download.file(url ='http://curl.haxx.se/ca/cacert.pem', destfile ='cacert.pem')


### NOTES: rtweet is another excellent option
## https://mkearney.github.io/blog/2017/06/01/intro-to-rtweet/
### https://rtweet.info/

#install.packages("devtools")
#install.packages("rlang")
library(rlang)
library(usethis)
library(devtools)
#install.packages("base64enc")
library(base64enc)
#install.packages("RCurl")
library(RCurl)

#devtools::install_version("httr", version="0.6.0", repos="http://cran.us.r-project.org")
#devtools::install_version("twitteR", version="1.1.8", repos="http://cran.us.r-project.org")
#devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0")

library(httr)
library(twitteR)

### Install the needed packages...
#install.packages("twitteR")
#install.packages("ROAuth")
# install.packages("rtweet")
library(ROAuth)

library(networkD3)
## If trouble use detach and then install and
## do library
library(arules)
library(rtweet)


library(jsonlite)
#install.packages("streamR")
library(streamR)
#install.packages("rjson")
library(rjson)
#install.packages("tokenizers")
library(tokenizers)
library(tidyverse)
library(plyr)
library(dplyr)
library(ggplot2)
#install.packages("syuzhet")  ## sentiment analysis
library(syuzhet)
library(stringr)
library(arulesViz)
library(igraph)

library(httpuv)
library(openssl)

##############  Using twittR ##########################################################
## NOTE - One day - out of no where - the following line of code stopped working
## It worked for 4 years and then stopped :)
## I Google-Searched the error and found some suggestions.
## One was to install base64enc
## This solution did not work...
## The next option was to run
## devtools::install_version("httr", version="0.6.0", repos="http://cran.us.r-project.org")
## and then - that failed - so I tried...
## # Installing the devtools package
##install.packages("devtools")
## AND then
# Using devtools to install an older version
## devtools::install_version("httr", version="0.6.0", repos="http://cran.us.r-project.org")
## and
## devtools::install_version("twitteR", version="1.1.8", repos="http://cran.us.r-project.org")
## This all took 1.5 hours! Just FYI
############################################---> this happens in open source languages...

# my_oauth <- OAuthFactory$new(consumerKey = consumerKey,
#                              consumerSecret = consumerSecret,
#                              requestURL = requestURL,
#                              accessURL = accessURL,
#                              authURL = authURL)
#


#install.packages("base64enc")

#Search

setup_twitter_oauth(consumerKey,consumerSecret,access_Token, access_Secret)

(Search<-twitteR::searchTwitter("#2016Election", n=50, lang="en"))


##https://www.rdocumentation.org/packages/twitteR/versions/1.1.9/topics/searchTwitter
# (Search2 <- searchTwitter("football",
#                          n = 5,
#                          lang = "en"
#                          #since='2021-03-01', ## need special account
#                          #until='2021-03-02'  ## need special account
#                          ) )
#



(Search_DF <- twitteR::twListToDF(Search))

TransactionTweetsFile = "TweetResults.csv"
(Search_DF$text[3])

#make data frame
## do.call is a list that holds all arguments in a function
## https://www.stat.berkeley.edu/~s133/Docall.html
##(Search2_DF <- do.call("rbind", lapply(Search2, as.data.frame)))
## OR
#tokenize_tweets(x, lowercase = TRUE, stopwords = NULL, strip_punct = TRUE,
#                 strip_url = FALSE, simplify = FALSE)

#tokenize_tweets(Search2_DF$text[1],stopwords = stopwords::stopwords("en"),
#               lowercase = TRUE,  strip_punct = TRUE,
#               strip_url = TRUE, simplify = TRUE)

## Start the file
Trans <- file(TransactionTweetsFile)
## Tokenize to words
Tokens<-tokenizers::tokenize_words(
  Search_DF$text[1],stopwords = stopwords::stopwords("en"),
  lowercase = T,  strip_punct = T, strip_numeric = T,
  simplify = T)

## Write tokens
cat(unlist(Tokens), "\n", file=Trans, sep=",")
close(Trans)

## Append remaining lists of tokens into file
## Recall - a list of tokens is the set of words from a Tweet
Trans <- file(TransactionTweetsFile, open = "a")
for(i in 2:nrow(Search_DF)){
  Tokens<-tokenize_words(Search_DF$text[i],
                         stopwords = stopwords::stopwords("en"),
                         lowercase = T,
                         strip_punct = T,
                         simplify = T)

  cat(unlist(Tokens), "\n", file=Trans, sep=",")
  cat(unlist(Tokens))
}

close(Trans)


######### Read in the tweet transactions
# TweetTrans <- read.transactions(TransactionTweetsFile,
#                                 rm.duplicates = FALSE,
#                                 format = "basket",
#                                 sep=","
#                                 ## cols =
# )
#inspect(TweetTrans)
## See the words that occur the most
#Sample_Trans <- sample(TweetTrans, 3)
#summary(Sample_Trans)

## Read the transactions data into a dataframe
TweetDF <- read.csv(TransactionTweetsFile,
                    header = FALSE, sep = ",")
head(TweetDF)
(str(TweetDF))

## Convert all columns to char
TweetDF<-TweetDF %>%
  mutate_all(as.character)
(str(TweetDF))
# We can now remove certain words
TweetDF[TweetDF == "t.co"] <- ""
TweetDF[TweetDF == "rt"] <- ""
TweetDF[TweetDF == "http"] <- ""
TweetDF[TweetDF == "https"] <- ""

## Check it so far....
TweetDF

## Clean with grepl - every row in each column
MyDF<-NULL
MyDF2<-NULL
for (i in 1:ncol(TweetDF)){
  MyList=c()
  MyList2=c() # each list is a column of logicals ...
  MyList=c(MyList,grepl("[[:digit:]]", TweetDF[[i]]))
  MyDF<-cbind(MyDF,MyList)  ## create a logical DF
  MyList2=c(MyList2,(nchar(TweetDF[[i]])<4 | nchar(TweetDF[[i]])>11))
  MyDF2<-cbind(MyDF2,MyList2)
  ## TRUE is when a cell has a word that contains digits
}
## For all TRUE, replace with blank
TweetDF[MyDF] <- ""
TweetDF[MyDF2] <- ""
(head(TweetDF,10))



# Now we save the dataframe using the write table command
write.table(TweetDF, file = "UpdatedTweetFile.csv", col.names = FALSE,
            row.names = FALSE, sep = ",")
TweetTrans <- read.transactions("UpdatedTweetFile.csv", sep =",",
                                format("basket"),  rm.duplicates = TRUE)
#inspect(TweetTrans)

### Clean up the datafile

## For this ERror
## Error in length(obj) : Method length not implemented for class rules
## DO THIS: (1) detach("package:arulesViz", unload=TRUE)
## (2) detach("package:arules", unload=TRUE)
## (3) library(arules)

## optional after TweetTrans
## Takes LONG time to run
###gsub("[{}]", "", as.character(inspect(TweetTrans_rules@lhs)$items))
#TweetTrans_rules@rhs <- gsub("[{}]", "", as.character(inspect(TweetTrans_rules@rhs)$items))

############ Create the Rules  - Relationships ###########
TweetTrans_rules = arules::apriori(TweetTrans,
        parameter = list(support=.06, conf=1, minlen=2))
        #maxlen
        #appearance = list (default="lhs",rhs="milk")
inspect(TweetTrans_rules[1:20])
##  SOrt by Conf
SortedRules_conf <- sort(TweetTrans_rules, by="confidence", decreasing=TRUE)
inspect(SortedRules_conf[1:20])
## Sort by Sup
SortedRules_sup <- sort(TweetTrans_rules, by="support", decreasing=TRUE)
inspect(SortedRules_sup[1:20])
## Sort by Lift
SortedRules_lift <- sort(TweetTrans_rules, by="lift", decreasing=TRUE)

####################################################
### HERE - you can affect which rules are used
###  - the top for conf, or sup, or lift...
####################################################
TweetTrans_rules<-SortedRules_lift[1:50]
inspect(TweetTrans_rules)

####################################################
## IF ERROR
## RUN THIS CODE
## detach("package:arulesViz", unload=TRUE)
## detach("package:arules", unload=TRUE)
## library(arules)
## library(arulesViz) ## After arules works
#####################################################

#(SortedRules_sup[1:30])
#plot(SortedRules_sup[1:30],method="graph",engine='interactive', shading="confidence")
#plot(SortedRules_conf[1:50],method="graph",engine='interactive',shading="confidence")

#######################################################
########  Using NetworkD3 To View Results   ###########
#######################################################

## Build node and egdes properly formatted data files
## Build the edgeList which will have SourceName, TargetName
##                                    Weight, SourceID, and
##                                    TargetID

#Rules_DF<-as(TweetTrans_rules, "data.frame")
#(head(Rules_DF))

## Convert the RULES to a DATAFRAME
Rules_DF2<-DATAFRAME(TweetTrans_rules, separate = TRUE)
(head(Rules_DF2))
str(Rules_DF2)
## Convert to char
Rules_DF2$LHS<-as.character(Rules_DF2$LHS)
Rules_DF2$RHS<-as.character(Rules_DF2$RHS)

## Remove all {}
Rules_DF2[] <- lapply(Rules_DF2, gsub, pattern='[{]', replacement='')
Rules_DF2[] <- lapply(Rules_DF2, gsub, pattern='[}]', replacement='')

Rules_DF2

## Other options for the following
#Rules_Lift<-Rules_DF2[c(1,2,5)]
#Rules_Conf<-Rules_DF2[c(1,2,4)]
#names(Rules_Lift) <- c("SourceName", "TargetName", "Weight")
#names(Rules_Conf) <- c("SourceName", "TargetName", "Weight")
#head(Rules_Lift)
#head(Rules_Conf)

###########################################
###### Do for SUp, Conf, and Lift   #######
###########################################
## Remove the sup, conf, and count
## USING LIFT
Rules_L<-Rules_DF2[c(1,2,5)]
names(Rules_L) <- c("SourceName", "TargetName", "Weight")
head(Rules_L,30)

## USING SUP
Rules_S<-Rules_DF2[c(1,2,3)]
names(Rules_S) <- c("SourceName", "TargetName", "Weight")
head(Rules_S,30)

## USING CONF
Rules_C<-Rules_DF2[c(1,2,4)]
names(Rules_C) <- c("SourceName", "TargetName", "Weight")
head(Rules_C,30)

## CHoose and set
#Rules_Sup<-Rules_C
Rules_Sup<-Rules_L
#Rules_Sup<-Rules_S

###########################################################################
#############       Build a NetworkD3 edgeList and nodeList    ############
###########################################################################

#edgeList<-Rules_Sup
# Create a graph. Use simplyfy to ensure that there are no duplicated edges or self loops
#MyGraph <- igraph::simplify(igraph::graph.data.frame(edgeList, directed=TRUE))
#plot(MyGraph)

############################### BUILD THE NODES & EDGES ####################################
(edgeList<-Rules_Sup)
(MyGraph <- igraph::simplify(igraph::graph.data.frame(edgeList, directed=TRUE)))

nodeList <- data.frame(ID = c(0:(igraph::vcount(MyGraph) - 1)),
                       # because networkD3 library requires IDs to start at 0
                       nName = igraph::V(MyGraph)$name)
## Node Degree
(nodeList <- cbind(nodeList, nodeDegree=igraph::degree(MyGraph,
                    v = igraph::V(MyGraph), mode = "all")))

## Betweenness
BetweenNess <- igraph::betweenness(MyGraph,
      v = igraph::V(MyGraph),
      directed = TRUE)

(nodeList <- cbind(nodeList, nodeBetweenness=BetweenNess))

## This can change the BetweenNess value if needed
#BetweenNess<-BetweenNess/100



## For scaling...divide by
## RE:https://en.wikipedia.org/wiki/Betweenness_centrality
##/ ((igraph::vcount(MyGraph) - 1) * (igraph::vcount(MyGraph)-2))
## For undirected / 2)
## Min-Max Normalization
##BetweenNess.norm <- (BetweenNess - min(BetweenNess))/(max(BetweenNess) - min(BetweenNess))


## Node Degree


###################################################################################
########## BUILD THE EDGES #####################################################
#############################################################
# Recall that ...
# edgeList<-Rules_Sup
getNodeID <- function(x){
  which(x == igraph::V(MyGraph)$name) - 1  #IDs start at 0
}
## UPDATE THIS !! depending on # choice
(getNodeID("salary"))

edgeList <- plyr::ddply(
  Rules_Sup, .variables = c("SourceName", "TargetName" , "Weight"),
  function (x) data.frame(SourceID = getNodeID(x$SourceName),
                          TargetID = getNodeID(x$TargetName)))

head(edgeList)
nrow(edgeList)

########################################################################
##############  Dice Sim ################################################
###########################################################################
#Calculate Dice similarities between all pairs of nodes
#The Dice similarity coefficient of two vertices is twice
#the number of common neighbors divided by the sum of the degrees
#of the vertices. Method dice calculates the pairwise Dice similarities
#for some (or all) of the vertices.
DiceSim <- igraph::similarity.dice(MyGraph, vids = igraph::V(MyGraph), mode = "all")
head(DiceSim)

#Create  data frame that contains the Dice similarity between any two vertices
F1 <- function(x) {data.frame(diceSim = DiceSim[x$SourceID +1, x$TargetID + 1])}
#Place a new column in edgeList with the Dice Sim
head(edgeList)
edgeList <- plyr::ddply(edgeList,
                        .variables=c("SourceName", "TargetName", "Weight",
                                               "SourceID", "TargetID"),
                        function(x) data.frame(F1(x)))
head(edgeList)

##################################################################################
##################   color #################################################
######################################################
# COLOR_P <- colorRampPalette(c("#00FF00", "#FF0000"),
#                             bias = nrow(edgeList), space = "rgb",
#                             interpolate = "linear")
# COLOR_P
# (colCodes <- COLOR_P(length(unique(edgeList$diceSim))))
# edges_col <- sapply(edgeList$diceSim,
#                     function(x) colCodes[which(sort(unique(edgeList$diceSim)) == x)])
# nrow(edges_col)

## NetworkD3 Object
#https://www.rdocumentation.org/packages/networkD3/versions/0.4/topics/forceNetwork

D3_network_Tweets <- networkD3::forceNetwork(
  Links = edgeList, # data frame that contains info about edges
  Nodes = nodeList, # data frame that contains info about nodes
  Source = "SourceID", # ID of source node
  Target = "TargetID", # ID of target node
  Value = "Weight", # value from the edge list (data frame) that will be used to value/weight relationship amongst nodes
  NodeID = "nName", # value from the node list (data frame) that contains node description we want to use (e.g., node name)
  Nodesize = "nodeBetweenness",  # value from the node list (data frame) that contains value we want to use for a node size
  Group = "nodeDegree",  # value from the node list (data frame) that contains value we want to use for node color
  height = 700, # Size of the plot (vertical)
  width = 900,  # Size of the plot (horizontal)
  fontSize = 20, # Font size
  linkDistance = networkD3::JS("function(d) { return d.value*1000; }"), # Function to determine distance between any two nodes, uses variables already defined in forceNetwork function (not variables from a data frame)
  linkWidth = networkD3::JS("function(d) { return d.value*5; }"),# Function to determine link/edge thickness, uses variables already defined in forceNetwork function (not variables from a data frame)
  opacity = 5, # opacity
  zoom = TRUE, # ability to zoom when click on the node
  opacityNoHover = 5, # opacity of labels when static
  linkColour = "red"   ###"edges_col"red"# edge colors
)

# Plot network
#D3_network_Tweets

# Save network as html file
networkD3::saveNetwork(D3_network_Tweets,
                       "NetD3_DCR2019_worldNewsL_2021.html", selfcontained = TRUE)