#load packages ----
library(tidyverse)
library(here)
library(readxl)
library(skimr)
library(janitor)
library(tm)
library(syuzhet)
library(SentimentAnalysis)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(RCurl)
library(ggplot2)
library(lubridate)
library(plotly)
library(reshape2)
library(tidytext)
library(stringr)
library(data.table)
library(zoo)

#read in data ----
reddit <- read_csv(here("reddit_student_data.csv"))
View(reddit)
#Rename the date column to an easier to reference name
reddit <- reddit %>% rename(date = created_utc)

#Step 2: begin cleaning the data and descriptive stats ----

#load the data into a corpus
corpus <- SimpleCorpus(VectorSource(reddit$title))

#clean the text
# 1. Stripping any extra white space:
dfCorpus <- tm_map(corpus, stripWhitespace)
# 2. Transforming everything to lowercase
dfCorpus <- tm_map(dfCorpus, tolower)
# 3. Removing punctuation
dfCorpus <- tm_map(dfCorpus, removePunctuation)
dfCopy <- dfCorpus
# 4. Removing stop words
dfCorpus <- tm_map(dfCorpus, removeWords, stopwords("en"))
# 5. combine common words
for (j in seq(dfCorpus))
{
  dfCorpus[[j]] <- gsub("corona virus", "coronavirus", dfCorpus[[j]])
  dfCorpus[[j]] <- gsub("covid 19", "covid19", dfCorpus[[j]])
  dfCorpus[[j]] <- gsub("unemployment rate", "unemploymentrate", dfCorpus[[j]])
  dfCorpus[[j]] <- gsub("cruise ship", "cruiseship", dfCorpus[[j]])
  dfCorpus[[j]] <- gsub("testing rate", "testingrate", dfCorpus[[j]])
  dfCorpus[[j]] <- gsub("new case", "newcase", dfCorpus[[j]])
  dfCorpus[[j]] <- gsub("confirmed cases", "confirmedcases", dfCorpus[[j]])
  dfCorpus[[j]] <- gsub("white house", "whitehouse", dfCorpus[[j]])
  dfCorpus[[j]] <- gsub("united states", "unitedstates", dfCorpus[[j]])
  dfCorpus[[j]] <- gsub("stock up", "stockup", dfCorpus[[j]])
  dfCorpus[[j]] <- gsub("stock market", "stockmarket", dfCorpus[[j]])
}
# 6. stem the text back to root words
dfCorpusTemp <- tm_map(dfCorpus, stemDocument)
writeLines(as.character(dfCorpusTemp[2])) # Check to see if it worked.
dfCorpus <- dfCorpusTemp
# 7. remove numbers
dfCorpus <- tm_map(dfCorpus, removeNumbers)
# 8. remove common words
#this was done as an iteration because it is redundant the data would include them
#as it is already on the coronavirus thread, therefore surrounding the topic
dfCorpus <- tm_map(dfCorpus, removeWords, "coronavirus")
dfCorpus <- tm_map(dfCorpus, removeWords, "covid")

#create document term matrix to be able to analyze the data now that it is cleaned
dtm <- DocumentTermMatrix(dfCorpus)
dtm

#Descriptive Stats ----
#Sum the number of occurrences of each word
sums <- as.data.frame(colSums(as.matrix(dtm)))
sums<-rownames_to_column(sums)
colnames(sums) <- c("term", "count")
sums <- arrange(sums, desc(count))
head <- sums[1:500,]

#plot wordcloud
wordcloud(words = head$term, freq = head$count, min.freq = 10,
          max.words=5000, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

freq <- colSums(as.matrix(dtm))
head(table(freq), 20)
tail(table(freq), 20)

# This makes a matrix that is 20% empty space, maximum.
#Prep the frequency plot for words to decrease in frequency
freq <- colSums(as.matrix(dtm))   
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 14) 
findFreqTerms(dtm, lowfreq=50)
wf <- data.frame(word=names(freq), freq=freq)

#Plot frequencies
p <- ggplot(subset(wf, freq>100), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=90, hjust=1))
p   

#export csv ----
m <- as.matrix(dtm)
write.csv(m, file="Document_Term_Matrix.csv") 
#some stuff was easier to confirm as correct from Excel and debug so thats what 
#the excel doc was used for

#Step 3: Work with matrix to get a data frame with every word and its frequency per day----
n <- as.data.frame(cbind(as.Date(reddit$date),m))
n <- n %>% rename(dateX = V1)


#using melt and cast functions, take the per post data frame and transition it to a
# per day data fram that sums the posts (frequencies of each word) from 00:00 to 
# 23:59 on the respective day
df_melt <- melt(n, id = "dateX")
df_cast <- as.data.frame(dcast(df_melt, dateX~ variable, sum))
rowSum <- as.data.frame(rowSums(df_cast[,c(-1)]))
df_temp <- cbind(rowSum,df_cast)
df_temp <- df_temp %>%
  rename(sumOfAllWords = `rowSums(df_cast[, c(-1)])`)
df_cast <- df_temp

#Get hourly data frame too - cause regression didn't work well with daily data and hourly
# will avoid massive co-variance.
date_hour <- as.data.frame(ymd_hms(reddit$date))
date_hour <- as.data.frame(floor_date(reddit$date, unit = "hour"))

hourlyData <- as.data.frame(cbind(date_hour,m))
hourlyData <- hourlyData %>% rename(dateX = `floor_date(reddit$date, unit = "hour")`)

hourly_melt <- melt(hourlyData, id = "dateX")
hourly_cast <- as.data.frame(dcast(hourly_melt, dateX ~ variable, sum))
rowSumHour <- as.data.frame(rowSums(hourly_cast[,c(-1)]))
df_temp <- cbind(rowSumHour,hourly_cast)
df_temp <- df_temp %>%
  rename(sumOfAllWords = `rowSums(hourly_cast[, c(-1)])`)
hourly_cast <- df_temp

#Sentiment Analysis----
#Load the document term matrix into sentiment analyzer
sent <- analyzeSentiment(dtm, language = "en")

#Select the Harvard-IV dictionary results ..  
sent <- sent[,1:4]

#Organizing it as a dataframe
sent <- as.data.frame(sent)

#Take a look at what these sentiment values look like. 
head(sent)
summary(sent$SentimentGI)

#Attach the authors of the posts to the sentiment data
final <- cbind(reddit, sent)
final %>% group_by(author, domain, title) %>% 
  summarise(sent = mean(SentimentGI)) %>%
  arrange(sent) %>%
  tail(n = 10)

#emotion lexicon----
#create object to study responses before pandemic:
emotion_before <- reddit %>% 
  mutate(date = ymd_hms(date)) %>%
  filter(date <= as.Date("2020-03-10"))

#create object to study responses after pandemic:
emotion_after <- reddit %>% 
  mutate(date = ymd_hms(date)) %>%
  filter(date >= as.Date("2020-03-12"))


#place that into the NRC sentiment analyzer to analyze the 8 given emotions, ----
#create 2 variables, one measuring before and one measuring after
sent2 <- get_nrc_sentiment(emotion_before$title)
sent3 <- get_nrc_sentiment(emotion_after$title)
sent4 <- as.data.frame(colSums(sent2))
sent5 <- as.data.frame(colSums(sent3))
sent4 <- rownames_to_column(sent4)
sent5 <- rownames_to_column(sent5)
colnames(sent5) <- c("emotion", "countAfter")
sent4 <- sent4 %>% mutate(countAfter = sent5$countAfter)
colnames(sent4) <- c("emotion", "countBefore", "countAfter")

#compare the means of before and after to prove difference:
t.test(sent2$anger,sent3$anger)
t.test(sent2$anticipation,sent3$anticipation)
t.test(sent2$disgust,sent3$disgust)
t.test(sent2$joy,sent3$joy)
t.test(sent2$sadness,sent3$sadness)
t.test(sent2$surprise,sent3$surprise)
t.test(sent2$trust,sent3$trust)
t.test(sent2$negative,sent3$negative)
t.test(sent2$positive,sent3$positive)


#weight each count to their own level of emotion and create df
totalBefore = sum(sent4$countBefore)
totalAfter = sum(sent4$countAfter)
sent_BA <- sent4 %>%
  mutate(weightedBefore = sent4[,2]/totalBefore) %>%
  mutate(weightedAfter = sent4[,3]/totalAfter)
sent_BA <- sent_BA[,-which(names(sent_BA) %in% c("countBefore","countAfter"))]

meltBACount<-melt(sent4)
meltBAWeighted <- melt(sent_BA)

#plot the total count for each sentiment
p <- ggplot(meltBACount, aes(emotion,value,fill=variable)) +
  geom_bar(stat="identity",position="dodge")+
  theme_minimal() + 
  theme(legend.position="right") +
  labs( x = "Emotion", y = "Weighted Count") +
  ggtitle("Sentiment of COVID Posts Before and After Global Pandemic (March 11)") + 
  theme(plot.title = element_text(hjust=0.5))
p

#plot the sentiment weighted to the amount of total sentiment count in that period
q <- ggplot(meltBAWeighted, aes(emotion,value,fill=variable)) +
  geom_bar(stat="identity",position="dodge")+
  theme_minimal() + 
  theme(legend.position="right") +
  labs( x = "Emotion", y = "Weighted Count") +
  ggtitle("Sentiment of COVID Posts Before and After Global Pandemic (March 11)") + 
  theme(plot.title = element_text(hjust=0.5))
q

#Fear Daily Study----
reddit <- reddit %>% 
  mutate(date = ymd_hms(date))

#Frequency of posts on each day
ggplot(reddit, aes(x = date)) +
  geom_histogram(position = "identity",bins = 21, show.legend = FALSE)
  

#Sentiment Indicator - by Tick----
#create object to study responses on each date
emotionDaily <- reddit %>% 
  mutate(date = ymd_hms(date))
  
#Create another NRC sentiment table but this time don't sum the columns
nrc_dailyTemp <- get_nrc_sentiment(emotionDaily$title)

#make it temporary so I don't have to keep running the above line that takes 
#FOREVER
nrc_dailyTemp2 <- cbind(reddit$date,nrc_dailyTemp)
nrc_tick <- nrc_dailyTemp2
nrc_dailyTemp2 <- 0
nrc_tick <- nrc_tick %>% 
  rename(date = `reddit$date`)
nrc_tick$date <- ymd_hms(nrc_tick$date)

#Create the sentiment indicator by subtracting negative by positive
nrc_tick <- nrc_tick %>%
  mutate(nrc_tick[,10]*-1+nrc_tick[, 11])
nrc_tick <- nrc_tick %>% rename(sentimentIndicator = `nrc_tick[, 10] * -1 + nrc_tick[, 11]`)

#Make it rolling by using a for loop and coding it with the previous line
nrc_tick <- nrc_tick %>% 
  mutate(rollingSentiment = sentimentIndicator)
for(i in 2:5000)
{
  nrc_tick[i,13] = nrc_tick[i,13]+nrc_tick[i-1,13]
}

#toss everything back together and rename
redditSentiment <- as.data.frame(cbind(reddit$date,nrc_tick$sentimentIndicator,reddit$num_comments,reddit$title,reddit$domain))
redditSentiment <- redditSentiment %>%
  rename(numComments = V3) %>%
  rename(sentimentIndicator = V2) %>%
  rename(date = V1) %>%
  rename(title = V4) %>%
  rename(domain = V5)
redditSentiment$numComments <- as.numeric(redditSentiment$numComments)
redditSentiment$sentimentIndicator <- as.numeric(redditSentiment$sentimentIndicator)

head(redditSentiment)

plot(redditSentiment$numComments,redditSentiment$sentimentIndicator)
cor.test(redditSentiment$numComments,redditSentiment$sentimentIndicator)

#paint the pretty picture

ggplot(nrc_tick, aes(x = date, y = rollingSentiment)) + 
  geom_line()+
  ggtitle("Rolling Sentiment of Posts")+
  scale_x_datetime(date_breaks = "days", date_labels = "%d-%b")

#Develop Daily Data for Time Series----
#take the 'by post' object and group it by daily
nrc_daily <- nrc_tick %>%
  group_by(as.Date(date)) %>%
  summarise(anger = sum(anger),
            anticipation = sum(anticipation),
            disgust = sum(disgust),
            fear=sum(fear),
            joy = sum(joy),
            sadness =sum(sadness),
            surprise = sum(surprise),
            trust = sum(trust),
            negative = sum(negative),
            positive = sum(positive)) 
nrc_daily <- nrc_daily %>% rename(date = `as.Date(date)`)

#gather it into a plot-able format
df5 <- nrc_daily %>%
  gather(key = "variable", value = "value", -date)
df5$date <- as.Date(df$date)

#plot the 8 emotions together as a line time series
ggplot(df5, aes(x = date, y = value)) + 
  geom_line(aes(color = variable, linetype = variable))+
  ggtitle("Daily Emotion Value")+
  scale_x_date(date_breaks = "days", date_labels = "%d-%b")

#Develop hourly data for time series for regression input----
#Upsample the by post data to hourly using the lubridate library (floor_date)
nrc_hourly <- nrc_tick
date_hour <- as.data.frame(ymd_hms(nrc_hourly$date))
date_hour <- as.data.frame(floor_date(nrc_hourly$date, unit = "hour"))

#clean it and make it ready to use later in regression
hourlyData <- as.data.frame(cbind(date_hour,nrc_hourly))
hourlyData <- hourlyData %>% rename(dateX = `floor_date(nrc_hourly$date, unit = "hour")`)

hourly_melt_sentiment <- melt(hourlyData, id = "dateX")
hourly_cast_sentiment <- as.data.frame(dcast(hourly_melt_sentiment, dateX ~ variable, sum))
rowSumHour_sentiment <- as.data.frame(rowSums(hourly_cast_sentiment[,c(-1)]))
df_temp <- cbind(rowSumHour_sentiment,hourly_cast_sentiment)
df_temp <- df_temp %>%
  rename(sumOfAllWords = `rowSums(hourly_cast_sentiment[, c(-1)])`)
hourly_cast_sentiment <- df_temp


#Develop Daily changes in emotion for Time Series----
#Create a variable that will hold the change data
nrc_dailyChange <- nrc_daily[]

for(i in 2:20)
{
  for(j in 2:11)
  {
    nrc_dailyChange[i,j] <- (nrc_daily[i,j]-nrc_daily[i-1,j])
  }
}
nrc_dailyChange <- nrc_dailyChange[-c(1),]

#gather it into plotting form
df6 <- nrc_dailyChange %>%
  gather(key = "variable", value = "value", -date)
df6$date <- as.Date(df6$date)

#plot the change
ggplot(df6, aes(x = date, y = value)) + 
  geom_line(aes(color = variable, linetype = variable)) +
  ggtitle("Daily Change in Varying Emotions")
 
#Barplot of different emotions by daily change ----
nrc_dailyChangeMelt <- melt(nrc_dailyChange,id.vars = c("date"))

#Just to make sure, we decided to get a barplot of each daily change f emotion 
#to see if we can find anything unusual - wasn't much
ggplot(nrc_dailyChangeMelt, aes(date, value, fill=variable)) +
  geom_bar(stat='Identity',position=position_dodge())+  
  ggtitle("Daily Change")+
  scale_x_date(date_breaks = "days", date_labels = "%d-%b")+
  ylab("Sentiment Change")
  # scale_y_continuous(labels = scales::percent_format(accuracy = 1))

#ANGER
ggplot(nrc_dailyChange, aes(x=date, y=anger)) +
  geom_bar(stat="identity")+
  ggtitle("Daily Change in Anger")+
  scale_x_date(date_breaks = "days", date_labels = "%d-%b")+
  ylab("Anger Score")

#ANTICIPATION
ggplot(nrc_dailyChange, aes(x=date, y=anticipation)) +
  geom_bar(stat="identity")+
  ggtitle("Daily Change in Anticipation")+
  scale_x_date(date_breaks = "days", date_labels = "%d-%b")+
  ylab("Anticipation Score")

#DISGUST
ggplot(nrc_dailyChange, aes(x=date, y=disgust)) +
  geom_bar(stat="identity")+
  ggtitle("Daily Change in Disgust")+
  scale_x_date(date_breaks = "days", date_labels = "%d-%b")+
  ylab("Disgust Score")

#FEAR
ggplot(nrc_dailyChange, aes(x=date, y=fear)) +
  geom_bar(stat="identity")+
  ggtitle("Daily Change in Fear")+
  scale_x_date(date_breaks = "days", date_labels = "%d-%b")+
  ylab("Fear Score")

#JOY
ggplot(nrc_dailyChange, aes(x=date, y=joy)) +
  geom_bar(stat="identity")+
  ggtitle("Daily Change in Joy")+
  scale_x_date(date_breaks = "days", date_labels = "%d-%b")+
  ylab("Joy Score")

#SADNESS
ggplot(nrc_dailyChange, aes(x=date, y=sadness)) +
  geom_bar(stat="identity")+
  ggtitle("Daily Change in Sadness")+
  scale_x_date(date_breaks = "days", date_labels = "%d-%b")+
  ylab("Sadness Score")

#SURPRISE
ggplot(nrc_dailyChange, aes(x=date, y=surprise)) +
  geom_bar(stat="identity")+
  ggtitle("Daily Change in Surprise")+
  scale_x_date(date_breaks = "days", date_labels = "%d-%b")+
  ylab("Surprise Score")

#TRUST
ggplot(nrc_dailyChange, aes(x=date, y=trust)) +
  geom_bar(stat="identity")+
  ggtitle("Daily Change in Trust")+
  scale_x_date(date_breaks = "days", date_labels = "%d-%b")+
  ylab("Trust Score")

#Step 4: develop recommendations around data----
#Figure out what words are leading the different sentiment
#look first at the first 10 days in the data set
negPeriodMelt <- df_melt %>%
  filter(dateX <= 18332)
negPeriod <- dcast(negPeriodMelt, dateX ~ variable, sum)
negPeriod <- as.data.frame(colSums(negPeriod))

#do the same thing with the second half
posPeriodMelt <- df_melt %>%
  filter(dateX >= 18333)
posPeriod <- dcast(posPeriodMelt, dateX ~ variable, sum)
posPeriod <- as.data.frame(colSums(posPeriod))

#smush together
posvneg <- cbind(negPeriod,posPeriod)

#clean it up and filter out the non-frequent words
posvneg <- posvneg %>%
  rename(negativePeriod = `colSums(negPeriod)`) %>%
  rename(positivePeriod = `colSums(posPeriod)`) 
posvnegFiltered <- posvneg %>%
  filter(positivePeriod >= 2) %>%
  filter(negativePeriod >= 10)

#i honestly don't know why i made a for loop and a 
#second data fram for this - could have just mutated - oops
chg <- as.data.frame(posPeriod[FALSE,])

for(i in 2:350)
{
  chg[i,1] = (posvnegFiltered[i,2]-posvnegFiltered[i,1])
}

posvnegFiltered <- cbind(posvnegFiltered,chg)
posvnegFiltered <- posvnegFiltered %>%
  rename(chg = `posPeriod[FALSE, ]`)
posvnegFiltered2 <-posvnegFiltered %>%
  filter(chg >= 55 | chg <= -19)

#melt it into a table that can be plotted
setDT(posvnegFiltered2, keep.rownames = "keyword")
posvnegFiltered2 <- posvnegFiltered2 %>%
  arrange(desc(chg))
melt_posvneg <- melt(posvnegFiltered2,id.vars = "keyword")
melt_posvneg <- melt_posvneg[-c(27:39),]

#plot it
ggplot(melt_posvneg, aes(keyword,value, fill=variable)) +
  geom_bar(stat='Identity',position=position_dodge())+  
  ggtitle("Biggest Movers in Frequency Between the First 10 Days and Second 10 Days of Data")
# scale_y_continuous(labels = scales::percent_format(accuracy = 1))

#Keep this table to see the words relating to actions taken or not taken 
#to illustrate the effectiveness of the government's policies
actionWords <- posvnegFiltered %>%
  subset(rownames(posvnegFiltered) %in% c("will","can","wait","say","plan","think","discuss","consid","question","announc"))

#Test correlation of the top words with the sentiment----
#test the word 'close' and its relation to sentiment
#ran these tests to find inconclusive results on the IMMEDIATE effect of each word
#and the post it was within. Realized it was probably better to look at future posts
#instead
close <- redditSentiment %>% 
  filter(str_detect(title, "clos"))

sentimentClose = sum(close$sentimentIndicator)

#'quarantine'

quarantine <- redditSentiment %>% 
  filter(str_detect(title, "quarantine"))

sentimentQuarantine = sum(quarantine$sentimentIndicator)

#'restrict'
restrict <- redditSentiment %>% 
  filter(str_detect(title, "restrict"))

sentimentRestrict = sum(restrict$sentimentIndicator)

#'case'
case <- redditSentiment %>% 
  filter(str_detect(title, "case"))

sentimentCase = sum(case$sentimentIndicator)

#'will'
will <- redditSentiment %>% 
  filter(str_detect(title, "will"))

sentimentWill = sum(will$sentimentIndicator)

#'school'

school <- redditSentiment %>% 
  filter(str_detect(title, "school"))

sentimentSchool = sum(school$sentimentIndicator)

#'confirmedcase'

confirm <- redditSentiment %>% 
  filter(str_detect(title, "confirmed case"))

sentimentConfirm = sum(confirm$sentimentIndicator)

#'trump'
trump <- redditSentiment %>% 
  filter(str_detect(title, "trump"))

sentimentTrump = sum(trump$sentimentIndicator)

#'cuomo'
cuomo <- redditSentiment %>% 
  filter(str_detect(title, "cuomo"))

sentimentCuomo = sum(cuomo$sentimentIndicator)

#Filter data for policy related words----
#tried to see if there was a theme with certain words and how the sentiment lined up
 filteredDFPolicy <- subset(df_melt, value >= 0,
                           select=c("whitehouse","trump",
                                    "restrictions","restriction","law","laws",
                                    "Obama","policy","regulations","governing",
                                    "govern","government","administration","authority",
                                    "governance","jurisdiction","regime","rules","strategy","plans",
                                    "procedure","program","blueprint","direction","method","intention",
                                    "affairs","institution","liberal","democracy","politics","state",
                                    "public","election","parliment","nation","economy","civil","president",
                                    "debate","leader"))


filteredDFPolicy <- df_cast[,c("trump","restrictions","restriction","law","laws",
                                "obama","policy","regulations","regulation",
                                "governing","govern","governor","government","unitedstates","usa",
                                "administration","authority","rules","rule","strategy",
                                "plans","plan","procedure","procedures","program",
                                "programs","method","intention","affairs","institution",
                                "institutions","liberal","democracy","politics",
                                "political","state","states","public",
                                "election","elections","parliament","nation","nations",
                                "national","economy","civil","president","presidential",
                                "debate","debates","leader","leaders","leadership",
                                "authority","authorities","pence","legal",
                                "republican","congress","congressional",
                                "justice","military","federal","rights","freedom","approach",
                                "cuomo","senator","senators","senate","mayor","mayors",
                                "stimulus","governor","governors","mandatory","employment",
                                "unemployment")]




  
#Bar charts of bad words ----
#tried to see if there were any other relationships with other themes of words
#there wasn't
filteredHoardingRelated <- df_cast[,c("sumOfAllWords","dateX","hoard","hoarding","hoarders",
                                      "buy","stockpile","stockpiling","shortage",
                                      "shortages","supermarket","grocery","toilet","sanitizer")]
# df_cast[,c("sumOfAllWords","dateX","market","selloff",
#            "stock","short","sell","nasdaq")]
filteredHoardingRelated$row_sum <- rowSums(filteredHoardingRelated[,-c(1:2)])
filteredHoardingRelated <- filteredHoardingRelated[,c("dateX","row_sum","sumOfAllWords")]
filteredHoardingRelated$weighted <- filteredHoardingRelated$row_sum * 100 / filteredHoardingRelated$sumOfAllWords
filteredHoardingRelated$dateX <- as.Date(filteredHoardingRelated$dateX,origin = "1970-01-01")

ggplot(filteredHoardingRelated, aes(x=dateX, y=weighted)) +
  geom_bar(stat="identity")+
  ggtitle("Hoarding-related words")+
  scale_x_date(date_breaks = "days", date_labels = "%d-%b")+
  ylab("Number of words * 100 / total words that day")

#regression time ----
#make sure to add to end of list:
regressionSetUp <- as.data.frame(cbind(date = hourly_cast_sentiment$dateX,fear = hourly_cast_sentiment$fear,trust = hourly_cast_sentiment$trust,
                                       positive = hourly_cast_sentiment$positive,negative = hourly_cast_sentiment$negative,
                                       case = hourly_cast$case,will = hourly_cast$will,school = hourly_cast$school,
                                       close = hourly_cast$close,pandemic = hourly_cast$pandem,
                                       confirm = hourly_cast$confirm,lockdown = hourly_cast$lockdown,
                                       quarantine = hourly_cast$quarantin,trump = hourly_cast$trump, infect = hourly_cast$infect,
                                       say = hourly_cast$say,vaccine = hourly_cast$vaccin,spread = hourly_cast$spread,
                                       travel = hourly_cast$travel, panic = hourly_cast$panic, death = hourly_cast$death,
                                       restaurant = hourly_cast$restaur,question = hourly_cast$question, test = hourly_cast$test,
                                       people = hourly_cast$peopl, work = hourly_cast$work, home = hourly_cast$home),col.names = TRUE)

#Wordbank:
#  case + will + school + close + pandemic + confirm + lockdown + quarantine + trump + infect + say + vaccine +
#  spread + travel + panic + death + restaurant + question + test + people + work + home

#create a correlation matrix, will be used as part of the regression
corMatrix <- round(cor(regressionSetUp, method = "pearson", use = "pairwise.complete.obs"),2)
#eliminate the covariant variables, in this case, only 'confirm' and 'close':
cleanCor <- regressionSetUp[-c(9,11)]
corMatrix2 <- round(cor(cleanCor, method = "pearson", use = "pairwise.complete.obs"),2)

#create responding variable of sum of next 10 hours of sentiment:
regressionSetUp <- regressionSetUp %>%
  mutate(sentiIndicator =(rollsum(lead(positive-negative,default = 0), 10, fill = c(0), align = "left")))

#run regression first time
initReg <- lm(sentiIndicator ~ case + will + school + pandemic + lockdown +
                quarantine + trump + infect + say + vaccine + spread + travel + panic +
                death + restaurant + question + test + people + work + home,
              data = regressionSetUp)
summary(initReg)

#run regression multiple time deleting vars each time
reg2 <- lm(sentiIndicator ~ will + school + lockdown +
             infect + say + vaccine + travel,
           data = regressionSetUp)
summary(reg2)

#Overall, the R squared is a meager 0.22. not great, but the words left make sense from sentiment standpoint.
#We only want words in the wordbank that can be controlled indirectly or directly in their frequency by government.
#so stuff like school can be controlled by shutting down schools or generally caring about school but a word like
#Italy can't be influenced by government actions

#will run same words against trust indicator:
#create 10 day Average of subsequent trust:
regressionSetUp <- regressionSetUp %>%
  mutate(trustIndicator =(rollmean(lead(trust,default = 0), 10, fill = c(0), align = "left")))

#run regression first time
initReg <- lm(trustIndicator ~ case + will + school + pandemic + lockdown +
                quarantine + trump + infect + say + vaccine + spread + travel +
                death + restaurant + question + test + people + work + home,
              data = regressionSetUp)
summary(initReg)

#run regression subsequent times after deleting words: 
reg2 <- lm(trustIndicator ~ school + pandemic + lockdown +
             say + vaccine + travel +
             restaurant + people + work,
           data = regressionSetUp)
summary(reg2)

#trust plot with the words 'school' 'vaccine' and 'spread' as a control

workTrust <- regressionSetUp %>%
  mutate(avgTrust = mean(hourly_cast_sentiment$trust)) %>%
  mutate(stdMinTrust = avgTrust - sd(hourly_cast_sentiment$trust),stdMaxTrust = avgTrust + sd(hourly_cast_sentiment$trust)) %>%
  group_by(date,work,avgTrust,stdMaxTrust,stdMinTrust,trustIndicator) %>%
  filter(work >= 1) %>% 
  summarise(date = as_datetime(date))

#gather it into plotting form
df7 <- workTrust %>%
  gather(key = "variable", value = "value", -c(date,work))


#plot the change
ggplot(df7, aes(x = date, y = value)) + 
  geom_line(aes(color = variable, linetype = variable)) +
  ggtitle("Hourly response when the word 'work' is used")

#overall, the words left can be influenced by government action towards the pandemic with restaurant and 
# vaccine having the largest influence on trust the following days. School has smallest std dev so it is 
# more trustworthy to give a 0.39 incr. to value of trust in the subsequent days. 
#Happy with results. We definitely circled the drain with the initial search of what we were 
#going to do with the dataset - the predictive regression was a great idea (thanks Dhyey)