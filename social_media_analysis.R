
# #############################################################

# PACKAGES LADEN
install.packages("plyr", dependencies=TRUE)
library(plyr)
install.packages("vosonSML", dependencies=TRUE)
library(vosonSML)
install.packages("magrittr", dependencies=TRUE)
library(magrittr)
install.packages("stringr", dependencies=TRUE)
library(stringr)

# YOUTUBE DEVELOPER ZUGANG
my_apiKeyYoutube<-"HIER APPLICATION PROGRAMMING INTERFACE EINTRAGEN"
apiKeyYoutube<-Authenticate("youtube", apiKey=my_apiKeyYoutube)

# #############################################################

# VIDEOS AUSWÄHLEN
videoIDs=c("HIER KENNUNG DES YOUTUBE-VIDEOS EINTRAGEN")

# #############################################################

# AUSWAHL ALS OBJEKT SPEICHERN
myYoutubeData<-Collect(apiKeyYoutube,videoIDs, writeToFile=TRUE,verbose=TRUE,maxComments=200)

# OBJEKT ANSEHEN
str(myYoutubeData)

# #############################################################

# DATEN VEREINFACHEN
easy_dataset <- data.frame(myYoutubeData$AuthorDisplayName, myYoutubeData$Comment, myYoutubeData$ReplyCount, myYoutubeData$LikeCount)
colnames(easy_dataset) <- c("author", "words", "replies", "likse")

# #############################################################

# DATEN NUMERISCH HINTERLEGEN
easy_dataset$likes <- as.numeric(as.character(easy_dataset$likes))
easy_dataset$replies <- as.numeric(as.character(easy_dataset$replies))
easy_dataset$words <- as.character(easy_dataset$words)
easy_dataset$author <- as.character(easy_dataset$author)

# #############################################################

# NEUE VARIABLE ZUR ANZAHL AN KOMMENTAREN ANLEGEN

comments <- count(easy_dataset$author)
comments <- comments$freq

# #############################################################

# WÖRTER ZÄHLEN
temp <- strsplit(easy_dataset$words, split=" ")
easy_dataset$words <- sapply(temp, length)

# #############################################################

# DOPPELTE FÄLLE AGGREGIEREN

easy_dataset <- ddply(easy_dataset,"easy_dataset$author",numcolwise(sum))

# #############################################################

# ANZAHL AN KOMMENTAREN ALS NEUE VARIABLE HINZUFÜGEN
easy_dataset$comments <- comments
colnames(easy_dataset) <- c("author", "replies", "likes", "words", "comments")

# #############################################################

summary(easy_dataset)

# #############################################################

# NUTZERAUSWAHL NACH DESKRIPTIVEN MERKMALEN
identification <- subset(easy_dataset, replies>="5" & likes>="2" & words>="2" & comments>="2")

# RANKING NACH AKTIVITÄT
most_replies <- easy_dataset[order(-easy_dataset$replies),]
head(most_replies)
most_likes <- easy_dataset[order(-easy_dataset$likes),]
head(most_likes)
most_words <- easy_dataset[order(-easy_dataset$words),]
head(most_words)
most_comments <- easy_dataset[order(-easy_dataset$comments),]
head(most_comments)

# MANUELLE TEXTÜBERPRÜFUNG
subset(myYoutubeData[c(1:2)], AuthorDisplayName=="HIER ZU ANALYSIERENDE PERSON EINTRAGEN")

# #############################################################

# BINÄRE ZIELVARIABLE ERSTELLEN
easy_dataset$relevant <- ifelse(easy_dataset$replies>=5 & easy_dataset$likes>=1 & easy_dataset$words>=1 & easy_dataset$comments>=2, 1,0)

# RESTLICHEN DATENSATZ DICHOTIMISIEREN

easy_dataset$replies <- ifelse(easy_dataset$replies>=5,1,0)
easy_dataset$likes <- ifelse(easy_dataset$likes>=1,1,0)
easy_dataset$words <- ifelse(easy_dataset$words>=1,1,0)
easy_dataset$comments <- ifelse(easy_dataset$comments>2,1,0)

# #############################################################

glm(data=easy_dataset, relevant~replies+likes+words+comments, family=binomial())

# #############################################################

# NETZWERK ZUR VISUALISIERUNG VORBEREITEN
activityNetwork <- myYoutubeData %>% Create("activity") %>% AddText(myYoutubeData)
activityGraph <- activityNetwork %>% Graph(writeToFile = TRUE)
plot(activityGraph, vertex.label="", vertex.size=4, edge.arrow.size=0.5)

# #############################################################

# PACKAGE LADEN
library(tm)
library(wordcloud)
library(RColorBrewer)

# USER AUSWÄHLEN
user_data <- myYoutubeData[myYoutubeData$AuthorDisplayName == "HIER ZU ANALYSIERENDE PERSON EINTRAGEN",]
user_data$doc_id <- user_data$AuthorDisplayName
user_data$text <- user_data$Comment

# CORPUS FÜR WORD-CLOUD ERSTELLEN / AUTOMATISCH SATZZEICHEN UND UNBEDEUTENDE WÖRTER ENTFERNEN
ap.corpus = Corpus(DataframeSource(data.frame(user_data)))
ap.corpus = tm_map(ap.corpus, removePunctuation)
ap.corpus = tm_map(ap.corpus, content_transformer(tolower))
ap.corpus = tm_map(ap.corpus, function(x) removeWords(x, c(stopwords("german"),stopwords("en"),stopwords("SMART"),"sowie","dass","bzw","nie","bitte","hast","immer","wurde","schon","wer")))

# HÄUFIGKEITSÜBERSICHT DER WÖRTER
ap.tdm = TermDocumentMatrix(ap.corpus)
ap.m = as.matrix(ap.tdm)
ap.v = sort(rowSums(ap.m),decreasing=TRUE)
ap.d = data.frame(word = names(ap.v),freq=ap.v)
ap.d$word = toupper(ap.d$word)

# PLOT WORD-CLOUD
pal2 = brewer.pal(5,"Dark2")
png("wordcloud_user.png", width=1024,height=768)
wordcloud(ap.d$word,ap.d$freq, scale=c(8,.2),min.freq=2, max.words=30, random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()
