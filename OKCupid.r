#/* OKCupid Datamining project | Omar Akhtar | Jie Wen */
#SETL-ZFPFKQ-TD8K

mydata = read.csv("C:\\Users\\Omar\\Documents\\GitHub\\Data-Mining-Project\\OKCupid.csv",header = TRUE)  # read csv file
summary(mydata)

#/* Cleaning Data */
mydata$username = gsub("#NAME?",NA,mydata$username)
mydata$username = gsub("--","",mydata$username)
mydata$username = gsub("-","",mydata$username)

mydata[mydata==""] <- NA
mydata[is.na(mydata)] <- NA

install.packages('ggplot2')
library(ggplot2)

baseGG <- ggplot(mydata, aes(factor(age)))
baseGG +geom_bar(width=.5)

#<<<<<<< Updated upstream
#/* Remove outlier ages and income*/
mydata$age[mydata$age>80] <- NA
#=======
#/* Remove outlier ages and income*/
mydata$age[mydata$age>80] <- NA
#>>>>>>> Stashed changes

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
#<<<<<<< Updated upstream
mydata$cEssay <- paste(mydata$essay0,mydata$essay1,mydata$essay2, mydata$essay3,mydata$essay4,mydata$essay5,mydata$essay6,mydata$essay7,mydata$essay8,mydata$essay9,sep=" ")
mydata$cEssay = gsub("[\r\n]", " ", mydata$cEssay)
mydata$cEssay[6:7]
#=======
mydata$Description <- paste(mydata$essay0,mydata$essay1,mydata$essay2, mydata$essay3,mydata$essay4,mydata$essay5,mydata$essay6,mydata$essay7,mydata$essay8,mydata$essay9,sep=" ")

#Jie wen
# Delete essay responses to reduce data frame size
mydata <- mydata[, -which(substr(names(mydata), 1, 5) == "essay")]
#>>>>>>> Stashed changes

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

#<<<<<<< Updated upstream
install.packages("tm")
#=======
#pre-process the Description
clean.text(mydata$Description)

#generate a word cloud of the Description
library(NLP)
#>>>>>>> Stashed changes
library(tm)
dim(mydata)
myCorpus <- Corpus(VectorSource(mydata$cEssay))
myCorpus <- tm_map(myCorpus, tolower)
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
# remove stopwords
myStopwords <- c(stopwords('english'))
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

dictCorpus <- myCorpus

install.packages("SnowballC")
library(SnowballC)
#<<<<<<< Updated upstream

myCorpus <- tm_map(myCorpus, stemDocument)
myCorpus <- tm_map(myCorpus, PlainTextDocument)
myCorpus <- tm_map(myCorpus, stripWhitespace)
inspect(myCorpus[7:10])
myDtm <- DocumentTermMatrix(myCorpus, control=list(bounds = list(global = c(50,Inf)))) #long time
inspect(myDtm[1646:1659,1131:1147])
install.packages("wordcloud")
library(wordcloud)


findFreqTerms(myDtm, lowfreq=1000)
findAssocs(myDtm, 'friends', 0.30)
findAssocs(myDtm, 'love', 0.30)

table(mydata$age,mydata$income) #total entries 59946, NA=48443 ... majority empty


counts <- table(mydata$income)
barplot(counts, main="Income Distribution", 
        xlab="Annual Income")

#write.csv(mydata,"cupid.csv")
dtm = myDtm
dtm = removeSparseTerms(dtm, 0.60)
dist.mat <- dist(t(as.matrix(dtm)))
dist.mat  # check distance matrix
fit <- cmdscale(dist.mat, eig = TRUE)
points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])
rownames(fit$points)
ggplot(points, aes(x = x, y = y ,label=rownames(fit$points))) + geom_point(data = points, aes(x = x, y = y)) +geom_text() 

h <- hclust(dist.mat, method = "ward.D2")
plot(h)

clddtm = as.matrix(dtm)
word_freqs = sort(colSums(clddtm), decreasing = TRUE) 
# create a data frame with words and their frequencies
dm = data.frame(word = names(word_freqs), freq = word_freqs)

wordcloud(dm$word, dm$freq, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
#=======
#write.table(mydata$Description,file='desktop/Description.txt')
#lords <- Corpus(DirSource("desktop/Temp/"))
#lords <- tm_map(lords, stripWhitespace)
#lords <- tm_map(lords, PlainTextDocument)
#lords <- tm_map(lords, removeWords, stopwords("english"))
#lords <- tm_map(lords, stemDocument)
#this step will take more than 4 hours
#wordcloud(lords, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

#extract the Description part
essay<-mydata[ ,c('username', 'Description')]
# Delete Description to reduce data frame size
mydata <- mydata[, -which(substr(names(mydata),1,11) == "Description")]

#>>>>>>> Stashed changes

male_data <- subset(test1,test1$sex==1)
female_data <- subset(test1,test1$sex==0)

library(ggplot2)
male_height<-data.frame(table(male_data$height))
height<-as.vector(male_height$Var1)
frequency<-as.vector(male_height$Freq/35829)
plot(height,frequency,type="l",xlim=range(0,100),ylim=range(0,0.15))
height <- seq(0,100,length=35829)
frequency <- dnorm(height,mean=69.4,sd=3)
par(new=TRUE)
plot(height,frequency, type="l", col="red",ylim=range(0,0.15),main="Average male heights vs. OkCupid male heights")

female_height<-data.frame(table(female_data$height))
height<-as.vector(female_height$Var1)
frequency<-as.vector(female_height$Freq/24117)
plot(height,frequency,type="l",xlim=range(0,100),ylim=range(0,0.15))
height <- seq(0,100,length=24117)
frequency <- dnorm(height,mean=64,sd=3)
par(new=TRUE)
plot(height,frequency, type="l", col="red",ylim=range(0,0.15),main="Average female heights vs. OkCupid female heights")

income<-table(mydata$age,mydata$income)

library(lattice)
t1 <- table(female_data$income)
t2 <- table(male_data$income)
t3 <- table(female_data$age)
t4 <- table(male_data$age)
barchart(cbind(t1, t2), stack = F, horizontal = F)
barchart(cbind(t3,t4), stack = F, horizontal = F)

# heatmap for income
income<-as.matrix(table(mydata$age,mydata$income))
income_heatmap <- heatmap(income, Rowv=NA, Colv=NA, col = rainbow(256), scale="column", margins=c(5,10))

