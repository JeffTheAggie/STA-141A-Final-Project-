library(MASS)
library(stargazer)
knitr::opts_chunk$set(echo = FALSE, warning=FALSE)

#Getting Our Data
library(data.table)
data <- read.csv("/Users/Ian/Documents/STA 141A/FinalProjectData.csv")
genres = c("Action","Adventure", "Cars", "Comedy", "Dementia", "Demons", "Drama","Ecchi","Fantasy","Game", "Harem", "Hentai", "Historical", "Horror", "Josei", "Kids", "Magic",  "Martial Arts",  "Mecha",   "Military", "Music",   "Mystery" ,      "Parody" , "Police",  "Psychological", "Romance", "Samurai", "School",  "Sci-Fi",  "Seinen","Shoujo", "Shoujo Ai", "Shounen", "Shounen Ai", "Slice of Life", "Space" , "Sports", "Super Power", "Supernatural",  "Thriller", "Vampire", "Yaoi", "Yuri" )
data[genres] = 0
attach(data)
#Inputting Genre
data$Action = ifelse(grepl("Action", genre), 1, 0)
data$Adventure = ifelse(grepl("Adventure", genre), 1, 0)
data$Cars = ifelse(grepl("Cars", genre), 1, 0)
data$Comedy = ifelse(grepl("Comedy", genre), 1, 0)
data$Dementia = ifelse(grepl("Dementia", genre), 1, 0)
data$Demons = ifelse(grepl("Demons", genre), 1, 0)
data$Drama = ifelse(grepl("Drama", genre), 1, 0)
data$Ecchi = ifelse(grepl("Ecchi", genre), 1, 0)
data$Fantasy = ifelse(grepl("Fantasy", genre), 1, 0)
data$Game = ifelse(grepl("Game", genre), 1, 0)
data$Harem = ifelse(grepl("Harem", genre), 1, 0)
data$Hentai = ifelse(grepl("Hentai", genre), 1, 0)
data$Historical = ifelse(grepl("Historical", genre), 1, 0)
data$Horror = ifelse(grepl("Horror", genre), 1, 0)
data$Josei = ifelse(grepl("Josei", genre), 1, 0)
data$Kids = ifelse(grepl("Kids", genre), 1, 0)
data$Magic = ifelse(grepl("Magic", genre), 1, 0)
data$`Martial Arts` = ifelse(grepl("Martial Arts", genre), 1, 0)
data$Mecha = ifelse(grepl("Mecha", genre), 1, 0)
data$Military = ifelse(grepl("Military", genre), 1, 0)
data$Music = ifelse(grepl("Music", genre), 1, 0)
data$Mystery = ifelse(grepl("Mystery", genre), 1, 0)
data$Parody = ifelse(grepl("Parody", genre), 1, 0)
data$Police = ifelse(grepl("Police", genre), 1, 0)
data$Psychological = ifelse(grepl("Psychological", genre), 1, 0)
data$Romance = ifelse(grepl("Romance", genre), 1, 0)
data$Samurai = ifelse(grepl("Samurai", genre), 1, 0)
data$School = ifelse(grepl("School", genre), 1, 0)
data$`Sci-Fi` = ifelse(grepl("Sci-Fi", genre), 1, 0)
data$Seinen = ifelse(grepl("Seinen", genre), 1, 0)
data$Shoujo = ifelse(grepl("Shoujo", genre), 1, 0)
data$`Shoujo Ai` = ifelse(grepl("Shoujo Ai", genre), 1, 0)
data$Shounen = ifelse(grepl("Shounen", genre), 1, 0)
data$`Shounen Ai` = ifelse(grepl("Shounen Ai", genre), 1, 0)
data$`Slice of Life` = ifelse(grepl("Slice of Life", genre), 1, 0)
data$Space = ifelse(grepl("Space", genre), 1, 0)
data$Sports = ifelse(grepl("Sports", genre), 1, 0)
data$`Super Power` = ifelse(grepl("Super Power", genre), 1, 0)
data$Supernatural = ifelse(grepl("Supernatural", genre), 1, 0)
data$Thriller = ifelse(grepl("Thriller", genre), 1, 0)
data$Vampire = ifelse(grepl("Vampire", genre), 1, 0)
data$Yaoi = ifelse(grepl("Yaoi", genre), 1, 0)
data$Yuri = ifelse(grepl("Yuri", genre), 1, 0)
firstReg = lm(members~score+source+status+favorites+producerCount+licensorCount+studioCount+genreCount+ Action + Adventure + Cars  +  Comedy  +  Dementia  +  Demons  +  Drama  + Ecchi  + Fantasy  + Game  +  Harem  + Hentai  +  Historical  +  Horror  +  Josei  +  Kids  +  Magic  +   `Martial Arts`  +   Mecha  + Military  + Music  +    Mystery   +   Parody   +  Police  +   Psychological  +  Romance  +  Samurai  + School  +   `Sci-Fi`  +   Seinen  + Shoujo  +  `Shoujo Ai`  +  Shounen  +  `Shounen Ai`  + `Slice of Life`  + Space   +  Sports  +  `Super Power`  +  Supernatural  +   Thriller  +  Vampire  + Yaoi  +  Yuri + duration + episodes + type + rating + factor(yearAired), data = data)

par(mfrow=c(2,3))
plot(firstReg)
boxplot(firstReg$residuals, main = "Boxplot of Residuals")
hist(firstReg$residuals, main = "Histogram of Residuals", xlab = "residuals")
secondReg = boxcox(firstReg)
power=secondReg$x[which.max(secondReg$y)]
BCTransform <- function(y, lambda=0) {
     if (lambda == 0L) { log(y) }
     else { (y^lambda - 1) / lambda }
  }
members_boxcox = BCTransform(data$members, power)
thirdReg <- lm(members_boxcox~score+source+status+favorites+producerCount+licensorCount+studioCount+genreCount+ Action + Adventure + Cars  +  Comedy  +  Dementia  +  Demons  +  Drama  + Ecchi  + Fantasy  + Game  +  Harem  + Hentai  +  Historical  +  Horror  +  Josei  +  Kids  +  Magic  +   `Martial Arts`  +   Mecha  + Military  + Music  +    Mystery   +   Parody   +  Police  +   Psychological  +  Romance  +  Samurai  + School  +   `Sci-Fi`  +   Seinen  + Shoujo  +  `Shoujo Ai`  +  Shounen  +  `Shounen Ai`  + `Slice of Life`  + Space   +  Sports  +  `Super Power`  +  Supernatural  +   Thriller  +  Vampire  + Yaoi  +  Yuri + duration + episodes + type + rating + factor(yearAired), data = data)

par(mfrow=c(2,3))
plot(thirdReg)
boxplot(thirdReg$residuals, main = "Boxplot of residuals")
hist(thirdReg$residuals, main = "Histogram of residuals", xlab = "Residuals")
cooksDistance <- cooks.distance(thirdReg)

# Plot the Cook's Distance using the traditional 4/n criterion
plot(cooksDistance, pch="*", cex=2, main="High Leverage Points by Cook's distance")  # plot cook's distance
abline(h = 4/(6668-141-1), col="red")  # add cutoff line
text(x=1:length(cooksDistance)+1, y=cooksDistance, labels=ifelse(cooksDistance>4/(6668-141-1), names(cooksDistance),""), col="red")  # add labels
highLeveragePoints <- as.numeric(names(cooksDistance)[cooksDistance > (4/(6668-141-1))])
highLeveragePoints = c(highLeveragePoints,1193, 1360, 1504, 5918, 6135, 1142, 1300, 1997 )
dataMinusOutliers <- data[-sort(highLeveragePoints), ]

fourthReg <- lm(members_boxcox[-sort(highLeveragePoints)]~score+source+status+favorites+producerCount+licensorCount+studioCount+genreCount+ Action + Adventure + Cars  +  Comedy  +  Dementia  +  Demons  +  Drama  + Ecchi  + Fantasy  + Game  +  Harem  + Hentai  +  Historical  +  Horror  +  Josei  +  Kids  +  Magic  +   `Martial Arts`  +   Mecha  + Military  + Music  +    Mystery   +   Parody   +  Police  +   Psychological  +  Romance  +  Samurai  + School  +   `Sci-Fi`  +   Seinen  + Shoujo  +  `Shoujo Ai`  +  Shounen  +  `Shounen Ai`  + `Slice of Life`  + Space   +  Sports  +  `Super Power`  +  Supernatural  +   Thriller  +  Vampire  + Yaoi  +  Yuri + duration + episodes + type + rating + factor(yearAired), data = dataMinusOutliers)

par(mfrow=c(2,3))
plot(fourthReg)
boxplot(fourthReg$residuals, main = "Boxplot of residuals")
hist(fourthReg$residuals, main = "Histogram of residuals")
summary(fourthReg)
set.seed(2020141)
trainIndex <- sample(6668, as.integer(6668*0.5))
trainingData= data[trainIndex,]
testingData = data[-trainIndex,]

stepwiseSelectionAIC <- step(lm(members~1, data = trainingData),
                          scope = ~score+source+status+favorites+producerCount+licensorCount+studioCount+genreCount+ duration + episodes + type + rating,
                          direction = "both",  k = 2,
                          trace = 0)

stepwiseSelectionBIC <- step(lm(members~1, data = trainingData),
                          scope = ~score+source+status+favorites+producerCount+licensorCount+studioCount+genreCount+ duration + episodes + type + rating,
                          direction = "both",  k = log(as.integer(6668*0.5)),
                          trace = 0)
stepwiseSelectionAIC$AIC <- AIC(stepwiseSelectionAIC)
stepwiseSelectionAIC$BIC <- BIC(stepwiseSelectionAIC)

stepwiseSelectionBIC$AIC <- AIC(stepwiseSelectionBIC)
stepwiseSelectionBIC$BIC <- BIC(stepwiseSelectionBIC)

stargazer(stepwiseSelectionAIC, stepwiseSelectionBIC, type = "html", keep.stat = c("aic", "bic","adj.rsq","n"), omit.table.layout = "tn")
testDataAIC <- lm(stepwiseSelectionAIC$model, data = testingData)
testDataBIC <-lm(stepwiseSelectionBIC$model, data = testingData)

testDataAIC$AIC <- AIC(testDataAIC)
testDataAIC$BIC <- BIC(testDataAIC)

testDataBIC$AIC <- AIC(testDataBIC)
testDataBIC$BIC <- BIC(testDataBIC)

stargazer(testDataAIC, testDataBIC, type = "html", keep.stat = c("aic", "bic","adj.rsq","n"), omit.table.layout = "tn")
