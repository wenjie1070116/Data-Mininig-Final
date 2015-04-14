test1<-read.csv('desktop/mydata.csv')
#test1$offspring<-gsub("doesn&rsquo;t", "does not", test1$offspring)
test1 <- test1[, -which(names(test1) == "X")]

#target <- which(names(test1) == 'speaks')[1]
#cbind(test1[,1:target,drop=F], (test1$other_language="NA"), test1[,(target+1):length(test1),drop=F])

#data pre-processing
colnames(test1)[21] <- "other_language"
#users who only speak English 0, other language 1
library(stringi)
test1$other_language<-stri_detect_fixed(test1$other_language,",")*1

test1$is.anything<-stri_detect_fixed(test1$diet,"anything")*1
test1$is.halal<-stri_detect_fixed(test1$diet,"halal")*1
test1$is.kosher<-stri_detect_fixed(test1$diet,"kosher")*1
test1$is.vegetarian<-stri_detect_fixed(test1$diet,"vegetarian")*1
test1$is.vegan<-stri_detect_fixed(test1$diet,"vegan")*1

test1$ethinicity.white<-stri_detect_fixed(test1$ethnicity,"white")*1
test1$ethinicity.black<-stri_detect_fixed(test1$ethnicity,"black")*1
test1$ethinicity.asian<-stri_detect_fixed(test1$ethnicity,"asian")*1
test1$ethinicity.native_american<-stri_detect_fixed(test1$ethnicity,"native")*1
test1$ethinicity.pacific_islander<-stri_detect_fixed(test1$ethnicity,"pacific")*1
test1$ethinicity.indian<-stri_detect_fixed(test1$ethnicity,"indian")*1
test1$ethinicity.middle_eastern<-stri_detect_fixed(test1$ethnicity,"middle")*1
test1$ethinicity.hispanic_or_latino<-stri_detect_fixed(test1$ethnicity,"hispanic")*1
test1$ethinicity.other<-stri_detect_fixed(test1$ethnicity,"other")*1


test1$drinks.desperately<-stri_detect_fixed(test1$drinks,"desperately")*1
test1$drinks.not_at_all<-stri_detect_fixed(test1$drinks,"not at all")*1
test1$drinks.often<-stri_detect_fixed(test1$drinks,"often")*1
test1$drinks.rarely<-stri_detect_fixed(test1$drinks,"rarely")*1
test1$drinks.socially<-stri_detect_fixed(test1$drinks,"socially")*1
test1$drinks.very_often<-stri_detect_fixed(test1$drinks,"very_often")*1

test1$drugs.never<-stri_detect_fixed(test1$drugs,"never")*1
test1$drugs.often<-stri_detect_fixed(test1$drugs,"often")*1
test1$drugs.sometimes<-stri_detect_fixed(test1$drugs,"sometimes")*1

test1$orientation.straight<-stri_detect_fixed(test1$orientation,"straight")*1
test1$orientation.gay<-stri_detect_fixed(test1$orientation,"gay")*1
test1$orientation.bisexual<-stri_detect_fixed(test1$orientation,"bisexual")*1

test1$sex <- sapply(test1$sex,switch,"f"=0,"m"=1)

test1$status.single<-stri_detect_fixed(test1$status,"single")*1
test1$status.seeing_someone<-stri_detect_fixed(test1$status,"seeing someone")*1
test1$status.available<-stri_detect_fixed(test1$status,"available")*1
test1$status.married<-stri_detect_fixed(test1$status,"married")*1

test1$smokes.no<-stri_detect_fixed(test1$smokes,"no")*1
test1$smokes.sometimes<-stri_detect_fixed(test1$smokes,"sometimes")*1
test1$smokes.trying_to_quit<-stri_detect_fixed(test1$smokes,"trying to quit")*1
test1$smokes.when_drinking<-stri_detect_fixed(test1$smokes,"when drinking")*1
test1$smokes.yes<-stri_detect_fixed(test1$smokes,"yes")*1

test1$body_type <- sapply(test1$body_type,switch," "=0,"a little extra"=1,"athletic"=2,"average"=3,"curvy"=4,"fit"=5,"full figured"=6,"jacked"=7,"overweight"=8,"rather not say"=9,"skinny"=10,"thin"=11,"used up"=12)
test1$pets <- sapply(test1$pets,switch," "=0,"dislikes cats"=1,"dislikes dogs"=2,"dislikes dogs and dislikes cats"=4,"dislikes dogs and has cats"=5,"dislikes dogs and likes cats"=6,"has cats"=7,"has dogs"=8,"has dogs and dislikes cats"=9,"has dogs and has cats"=10,"has dogs and likes cats"=11,"likes cats"=12,"likes dogs"=13,"likes dogs and dislikes cats"=14,"likes dogs and has cats"=15,"likes dogs and likes cats"=16)
test1$sign <- sapply(test1$sign,switch," "=0,"aquarius"=1,"aries"=2,"cancer"=3,"capricorn"=4,"gemini"=5,"leo"=6,"libra"=7,"pisces"=8,"sagittarius"=9,"scorpio"=10,"taurus"=11,"virgo"=12)
test1$offspring <- sapply(test1$offspring, switch, " "=0,"does not have kids"=1,"does not have kids, and does not want any"=2,"does not have kids, but might want them"=3,"does not have kids, but wants them"=4,"does not want kids"=5,"has a kid"=6,"has a kid, and might want more"=7,"has a kid, and wants more"=8,"has a kid, but does not want more"=9,"has kids"=10,"has kids, and might want more"=11,"has kids, and wants more"=12,"has kids, but does not want more"=13,"might want kids"=14,"wants kids"=15)
test1$religion<-stri_detect_fixed(test1$religion,"agnosticism")*1+stri_detect_fixed(test1$religion,"atheism")*2+stri_detect_fixed(test1$religion,"buddhism")*3+stri_detect_fixed(test1$religion,"catholicism")*4+stri_detect_fixed(test1$religion,"christianity")*5+stri_detect_fixed(test1$religion,"hinduism")*6+stri_detect_fixed(test1$religion,"islam")*7+stri_detect_fixed(test1$religion,"judaism")*8+stri_detect_fixed(test1$religion,"other")*9


location<-data.frame(test1[,which(names(test1) == "location")])
location$state<-gsub("^.*?,","",location$test1...which.names.test1......location...)
location$city<-gsub(",.*","",location$test1...which.names.test1......location...)
location$state = gsub("^ ", "",location$state)
