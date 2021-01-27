library(twitteR)
library(ROAuth)
reqURL <- "http://api.twitter.com/oath/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
CUSTOMER_KEY <- "q4Yccg2KIno1Pff0dOsKbA2RB"
ACCESS_TOKEN <- "1471672230-7TKNN3cuuFG1G8JlyRO0bwy3schnnxu50z4GuCB"
CUSTOMER_SECRET <- "0PJgaCurPorQXkSclIQcFmcE0bBNprGzwqo4W4e6EuIzQf4CV7"
ACCESS_secret <- "MivFNO8rsUlqnown3D5yYBgHqlSZOhssVHIhZL7rFHoDT"
setup_twitter_oauth(CUSTOMER_KEY, CUSTOMER_SECRET, ACCESS_TOKEN, ACCESS_secret)


wfh_tweets_en = searchTwitter("work from home", n=1000, lang="en")


#cleaning text from meta informations, URLs, #hashtags, punctuation marks, numbers, unnecessary spaces, retweets (RTs)
#gunakan code blocks berikut:

wfhTweetsen <- sapply(wfh_tweets_en, function(x) x$getText())

catch.error = function(x)
{
  #buat missing value untuk tujuan tes
  y = NA
  #test untuk mengecek error (NA) yang telah kita dibuat
  catch_error = tryCatch(tolower(x), error=function(e) e)
  #if not an error
  if (!inherits(catch_error, "error"))
    y = tolower(x)
  #check result if error exists, otherwise the function works fine
  return(y)
}

cleanTweets <- function(tweet) {
  #bersihkan tweet untuk sentiment analysis
  #remove html links:
  tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
  #remove retweet entities:
  tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  #remove #hashtags:
  tweet = gsub("#\\w+", " ", tweet)
  #remove all "@people":
  tweet = gsub("@\\w+", " ", tweet)
  #remove all punctuations:
  tweet = gsub("[[:punct:]]", " ", tweet)
  #remove numbers, kita hanya butuh teks untuk analytics
  tweet = gsub("[[:digit:]]", " ", tweet)
  #remove unnecessary spaces (white spaces, tabs, etc)
  tweet = gsub("[ \t]{2,}", " ", tweet)
  tweet = gsub("^\\s+|\\s+$", "", tweet)
  #remove amp
  tweet = gsub("&amp", " ", tweet)
  #remove crazy character
  tweet = gsub("[^a-zA-Z0-9]", " ", tweet)
  #remove alphanumeric
  tweet = gsub("[^[:alnum:]]", " ", tweet)
  #jika ada lagi yang dirasa ingin dihilangkan, bisa. Contohnya, slang words/bahasa gaul dapat dihilangkan dengan cara serupa di atas.
  #ubah semua kata menjadi lowercase:
  tweet = catch.error(tweet)
  tweet
}

cleanTweetsAndRemoveNAs <- function(Tweets) {
  TweetsCleaned = sapply(Tweets, cleanTweets)
  #remove "NA" tweets:
  TweetsCleaned = TweetsCleaned[!is.na(TweetsCleaned)]
  names(TweetsCleaned) = NULL
  #remove repetitive tweets:
  TweetsCleaned = unique(TweetsCleaned)
  TweetsCleaned
}

#eksekusi pembersihan tweet

twfhenCleaned = cleanTweetsAndRemoveNAs(wfhTweetsen)

#export data ke csv

write.csv(twfhenCleaned, file = "D:/KULIAH/SEMESTER 5/DATA SCIENCE/projek/twfhenCleaned.csv")

#import lexicon

opinion.lexicon.posen = scan("D:/KULIAH/SEMESTER 5/DATA SCIENCE/projek/positive_words_en.txt", what = "character", comment.char = ";")
opinion.lexicon.negen = scan("D:/KULIAH/SEMESTER 5/DATA SCIENCE/projek/negative_words_en.txt", what = "character", comment.char = ";")


posen.words = c(opinion.lexicon.posen)
negen.words = c(opinion.lexicon.negen)


#membuat fungsi score.sentiment(), yang bisa menghitung hasil sentimen mentah berdasarkan algoritma pencocokan sederhana:
getSentimentScoreen = function(sentences, posen.words, negen.words, .progress = "none")
{
  require(plyr)
  require(stringr)
  scores = laply(sentences, function(sentence, posen.words, negen.words) {
    #remove digit, punctuation, dan special/control character:
    sentence = gsub("[[:cntrl:]]", "", gsub("[[:punct:]]", "", gsub("\\d+", "", sentence)))
    #convert semua teks menjadi lowercase:
    sentence = tolower(sentence)
    #pisahkan setiap kalimat menggunakan spasi (space delimiter):
    words = unlist(str_split(sentence, "\\s+"))
    #lakukan boolean match dari setiap kata-kata menggunakan pos &amp;amp;amp; neg opinion-lexicon:
    posen.matches = !is.na(match(words, posen.words))
    negen.matches = !is.na(match(words, negen.words))
    #score sentimen = total positive sentiment - total negative:
    score = sum(posen.matches) - sum(negen.matches)
    return(score)
  }, posen.words, negen.words, .progress=.progress)
  #return data frame berisi kalimat beserta sentimennya:
  return(data.frame(text = sentences, score = scores))
}

#terapkan ke data tweet yang telah kita bersihkan:
wfhenResult = getSentimentScoreen(twfhenCleaned, posen.words, negen.words)

head(wfhenResult)
# melakukan labeling pada nilai yang kurang dari 0 sebagai negatif dan lebih dari = 0 adalah positif
wfhenResult$klasifikasi<- ifelse(wfhenResult$score<0, "Negatif","Positif")
wfhenResult$klasifikasi
View(wfhenResult)

#Tukar Row
data <- wfhenResult[c(3,1,2)]
View(data)
write.csv(data, file = "D:/KULIAH/SEMESTER 5/DATA SCIENCE/projek/dataterlabeli.csv")
#Memisahkan dan menyimpan twit positif dan negatif
data.pos <- wfhenResult[wfhenResult$score>0,]
View(data.pos)
write.csv(data.pos, file = "D:/KULIAH/SEMESTER 5/DATA SCIENCE/projek/data-pos.csv")
#Memisahkan dan meyimpan twit positif dan negatif
data.neg <- wfhenResult[wfhenResult$score<0,]
View(data.neg)
write.csv(data.neg, file = "D:/KULIAH/SEMESTER 5/DATA SCIENCE/projek/data-neg.csv")