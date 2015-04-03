#/* OKCupid Datamining project | Omar Akhtar | Jie Wen */

mydata = read.csv("desktop/profiles.20120630.csv",header = TRUE)  # read csv file
summary(mydata)

#/* Cleaning Data */
ageNameError<-"#NAME?" 
mydata$username = gsub("#NAME?",NA,mydata$username)
mydata$username = gsub("--","",mydata$username)
mydata$username = gsub("-","",mydata$username)

library(ggplot2)

baseGG <- ggplot(mydata, aes(factor(age)))
baseGG +geom_bar(width=.5)

/* Remove outlier ages and income*/
mydata$age[mydata$age>80] <- NA

baseGG <- ggplot(mydata, aes(factor(income)))
baseGG +geom_bar(width=.5)

baseGG <- ggplot(mydata[!is.na(mydata$income), ], aes(factor(income)))
baseGG +geom_bar(width=.5)

mydata$income[mydata$income<0] <- NA

ggplot(mydata[!is.na(mydata$income), ], aes(factor(income))) + geom_bar() +
  facet_wrap(~ sex) # How salary of men vs women vary

table(mydata$age,mydata$income) # table to see age vs income | Lier detection

#clean essay dirty words

clean.text = function(x)
{
  # remove <br />
  x = gsub("<br />", "", x)
  #remove strong
  x = gsub("<strong>", "", x)
  # tolower
  x = tolower(x)
  # remove punctuation
  x = gsub("[[:punct:]]", "", x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove links http
  x = gsub("http\\w+", "", x)
  # remove tabs
  x = gsub("[ |\t]{2,}", "", x)
  # remove blank spaces at the beginning
  x = gsub("^ ", "", x)
  # remove blank spaces at the end
  x = gsub(" $", "", x)
  return(x)
}

# clean essay
mydata$essay0 = clean.text(mydata$essay0)
mydata$essay1 = clean.text(mydata$essay1)
mydata$essay2 = clean.text(mydata$essay2)
mydata$essay3 = clean.text(mydata$essay3)
mydata$essay4 = clean.text(mydata$essay4)
mydata$essay5 = clean.text(mydata$essay5)
mydata$essay6 = clean.text(mydata$essay6)
mydata$essay7 = clean.text(mydata$essay7)
mydata$essay8 = clean.text(mydata$essay8)
mydata$essay9 = clean.text(mydata$essay9)

#Combine all essays into new varible for text analysis
mydata$cEssay <- paste(mydata$essay0,mydata$essay1,mydata$essay2, mydata$essay3,mydata$essay4,mydata$essay5,mydata$essay6,mydata$essay7,mydata$essay8,mydata$essay9,sep=" ")


#Jie wen

#Remove everything except for signs in mydata$sign
mydata$sign=gsub("capricorn.*", "capricorn", mydata$sign)
mydata$sign=gsub("aquarius.*", "aquarius", mydata$sign)
mydata$sign=gsub('pisces.*', 'pisces', mydata$sign)
mydata$sign=gsub('aries.*', 'aries', mydata$sign)
mydata$sign=gsub('taurus.*', 'taurus', mydata$sign)
mydata$sign=gsub('gemini.*', 'gemini', mydata$sign)
mydata$sign=gsub('cancer.*', 'cancer', mydata$sign)
mydata$sign=gsub('leo.*', 'leo', mydata$sign)
mydata$sign=gsub('virgo.*', 'virgo', mydata$sign)
mydata$sign=gsub('libra.*', 'libra', mydata$sign)
mydata$sign=gsub('scorpio.*', 'scorpio', mydata$sign)
mydata$sign=gsub('sagittarius.*', 'sagittarius', mydata$sign)

#generate a word cloud of the cEssay
library(tm)
library(RColorBrewer)
library(wordcloud)
library(SnowballC)
write.table(mydata$cEssay,file='desktop/cEssay.txt')
lords <- Corpus(DirSource("desktop/Temp/"))
lords <- tm_map(lords, stripWhitespace)
lords <- tm_map(lords, PlainTextDocument)
lords <- tm_map(lords, removeWords, stopwords("english"))
lords <- tm_map(lords, stemDocument)
#this step will take more than 4 hours
wordcloud(lords, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
