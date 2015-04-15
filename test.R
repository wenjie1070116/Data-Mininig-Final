test<-read.csv('desktop/mydata.csv')
#test$offspring<-gsub("doesn&rsquo;t", "does not", test$offspring)
test <- test[, -which(names(test) == "X")]

#target <- which(names(test) == 'speaks')[1]
#cbind(test[,1:target,drop=F], (test$other_language="NA"), test[,(target+1):length(test),drop=F])

#data pre-processing
colnames(test)[21] <- "other_language"
#users who only speak English 0, other language 1
library(stringi)
test$other_language<-stri_detect_fixed(test$other_language,",")*1
test$sex <- sapply(test$sex,switch,"f"=0,"m"=1)
test$body_type <- sapply(test$body_type,switch," "=0,"a little extra"=1,"athletic"=2,"average"=3,"curvy"=4,"fit"=5,"full figured"=6,"jacked"=7,"overweight"=8,"rather not say"=9,"skinny"=10,"thin"=11,"used up"=12)
test$diet <- sapply(test$diet,switch," "=0,"anything"=1,"halal"=2,"kosher"=3,"mostly anything"=4,"mostly halal"=5,"mostly kosher"=6,"mostly other"=7,"mostly vegan"=8,"mostly vegetarian"=9,"other"=10,"strictly anything"=11,"strictly halal"=12,"strictly kosher"=13,"strictly other"=14,"strictly vegan"=15,"strictly vegetarian"=16,"vegan"=17,"vegetarian"=18)
test$drinks <- sapply(test$drinks,switch," "=0,"desperately"=1,"not at all"=2,"often"=3,"rarely"=4,"socially"=5,"very often"=6)
test$drugs <- sapply(test$drugs,switch," "=0,"never"=1,"often"=2,"sometimes"=3)
test$orientation <- sapply(test$orientation,switch,"bisexual"=0,"gay"=1,"straight"=2)
test$pets <- sapply(test$pets,switch," "=0,"dislikes cats"=1,"dislikes dogs"=2,"dislikes dogs and dislikes cats"=4,"dislikes dogs and has cats"=5,"dislikes dogs and likes cats"=6,"has cats"=7,"has dogs"=8,"has dogs and dislikes cats"=9,"has dogs and has cats"=10,"has dogs and likes cats"=11,"likes cats"=12,"likes dogs"=13,"likes dogs and dislikes cats"=14,"likes dogs and has cats"=15,"likes dogs and likes cats"=16)
test$sign <- sapply(test$sign,switch," "=0,"aquarius"=1,"aries"=2,"cancer"=3,"capricorn"=4,"gemini"=5,"leo"=6,"libra"=7,"pisces"=8,"sagittarius"=9,"scorpio"=10,"taurus"=11,"virgo"=12)
test$status <- sapply(test$status,switch,"unknown"=0,"single"=1,"seeing someone"=2,"available"=3,"married"=4)
test$smokes <- sapply(test$smokes, switch, " "=0,"no"=1,"sometimes"=2,"trying to quit"=3,"when drinking"=4,"yes"=5)
test$offspring <- sapply(test$offspring, switch, " "=0,"does not have kids"=1,"does not have kids, and does not want any"=2,"does not have kids, but might want them"=3,"does not have kids, but wants them"=4,"does not want kids"=5,"has a kid"=6,"has a kid, and might want more"=7,"has a kid, and wants more"=8,"has a kid, but does not want more"=9,"has kids"=10,"has kids, and might want more"=11,"has kids, and wants more"=12,"has kids, but does not want more"=13,"might want kids"=14,"wants kids"=15)
test$religion<-stri_detect_fixed(test$religion,"agnosticism")*1+stri_detect_fixed(test$religion,"atheism")*2+stri_detect_fixed(test$religion,"buddhism")*3+stri_detect_fixed(test$religion,"catholicism")*4+stri_detect_fixed(test$religion,"christianity")*5+stri_detect_fixed(test$religion,"hinduism")*6+stri_detect_fixed(test$religion,"islam")*7+stri_detect_fixed(test$religion,"judaism")*8+stri_detect_fixed(test$religion,"other")*9
