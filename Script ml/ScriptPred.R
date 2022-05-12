library(ggplot2)
library(MASS)
library(dplyr)
library(corrplot)
library(hrbrthemes)
df <- read.csv("donnees-hospitalieres-covid-19-dep-france.csv",sep = ';',encoding = "UTF-8")

# dfnew <- lapply(df, iconv, to = "ASCII//TRANSLIT")
#df<-df %>%
# mutate_if(is.character, 
#          function(col) iconv(col, to="ASCII//TRANSLIT"))



df2 = select_if(df, is.numeric)
df2

set.seed(420)
sample = sample.int(n = nrow(df), size = floor(.75*nrow(df)), replace = F)
train = df2[sample, ]
test  = df2[-sample, ]

matcor = cor(df2)
symnum(matcor, abbr.colnames=FALSE)
corrplot(matcor, method="number")

model1 = lm(Nb.actuellement.hospitalisÃ.s ~ Total.retour.Ã..domicile+Total.DÃ.cÃ.s , data=train)
summary(model1)
par(mfrow=c(2,2))
plot(model1)

pred1 <- predict(model1, newdata = test)
rmse <- sqrt(sum(pred1 - test$Nb.actuellement.hospitalisÃ.s)^2)/length(test$Nb.actuellement.hospitalisÃ.s+test$Total.DÃ.cÃ.s)
c(RMSE = rmse, R2=summary(model1)$r.squared)

#AICmodel = stepAIC(model, direction = "both")
#summary(AICmodel)
#vif(AICmodel)


model2 = lm(Nb.actuellement.en.soins.intensifs ~ Nb.actuellement.hospitalisÃ.s, data = train)
summary(model2)
par(mfrow=c(2,2))
plot(model2)

pred2 <- predict(model2, newdata = test)
rmse2 <- sqrt(sum(pred2 - test$Nb.actuellement.en.soins.intensifs)^2)/length(test$Nb.actuellement.en.soins.intensifs)
c(RMSE2 = rmse2, R2=summary(model2)$r.squared)

p2 = ggplot(data=test, aes(Nb.actuellement.hospitalisÃ.s, y=Nb.actuellement.en.soins.intensifs)) +
geom_point(aes(color = "Une capture"),color ="plum2") +   theme_ipsum() + geom_abline(intercept=model2$coefficients[1], slope=model2$coefficients[2],size=1,linetype="dashed", size=1.2)
+ geom_text(family = "Times New Roman")

p2

