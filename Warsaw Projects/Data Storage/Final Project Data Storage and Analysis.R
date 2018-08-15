setwd("C:/Users/Tom/Documents/Semestr letni 2015-2016/Bigdata/Przemek")



#dane uczace:
DANE <- read.table("Boston.txt", sep = ",", header = TRUE)


library(glmnet)


###############################################################
#                 EKSPLORACJA DANYCH                   

#Zmiana zmiennych ciaglych na binarne
summary(DANE)

medcrim<-median(DANE$CRIM)
medpric<-median(DANE$MEDV)

DANE$CRIM[DANE$CRIM < medcrim]="Niska przestepczosc"
DANE$CRIM[DANE$CRIM !="Niska przestepczosc"]="Duza przestepczosc"

DANE$MEDV[DANE$MEDV < medpric]="Niedocenione"
DANE$MEDV[DANE$MEDV !="Niedocenione"]="Przecenione"



# Cena domu, a przestepczosc
table1 <- table(DANE$MEDV,DANE$CRIM)

mosaicplot(table1,main="Ceny domu zalezne od przestepczosci",
           xlab="Cena domu",ylab="Przestepczosc",
           col=grey.colors(5, start = 0.9, end = 0.3, gamma = 2.2, alpha = NULL))
#wybor pieciu odcieni szarosci

# Cena domu, a dostep do obwodnicy
table2 <- table(DANE$RAD,DANE$MEDV)

mosaicplot(table2,main="Cena domu, a dostep do obwodnicy",
           xlab="DOstep do obwodnicy",ylab="Cena domu",
           col=grey.colors(2, start = 0.9, end = 0.3, gamma = 2.2, alpha = NULL))
#wybor pieciu odcieni szarosci


# wiek:
plot(density(DANE$wiek[DANE$zakup==1]), lwd=2, main="Gêstoœæ warunkowa dla wieku", xlab="wiek", ylab="gêstoœæ")
lines(density(DANE$wiek[DANE$zakup==0]), lwd=2, col="red")
legend("topright", c("zakup=1", "zakup=0"),
       lty = 1, lwd = 2, col = c("black", "red"))


# kwota
plot(density(DANE$kwota[DANE$zakup==0]), lwd=2, main="Gêstoœæ warunkowa dla kwoty", xlab="kwota", ylab="gêstoœæ", col="red")
lines(density(DANE$kwota[DANE$zakup==1]), lwd=2)
legend("topright", c("zakup=1", "zakup=0"),
       lty = 1, lwd = 2, col = c("black", "red"))


hist(DANE$kwota, prob = T, main="Histogram kwota", xlab="kwota", ylab="gêstoœæ")
#duza skosnosc, wiec logarytm tej zmiennej +liczba (¿eby nie logarytmowac zer)

DANE$kwota <- log(DANE$kwota+50)
DANE2$kwota <- log(DANE2$kwota+50)

table(DANE$kwota)



#w wojewodztwach wystepowaly polskie znaki np "swiêt"
levels(DANE$wojewodztwo) <- c("dolsl", "kujpom", "lubel", "lubus", "lodz", "malopol", "mazow",
                              "opol", "podkar", "podlas", "pomo", "slas", "swiet", "wrmaz", "wielkop", "zachpom")

levels(DANE2$wojewodztwo) <- c("dolsl", "kujpom", "lubel", "lubus", "lodz", "malopol", "mazow",
                              "opol", "podkar", "podlas", "pomo", "slas", "swiet", "wrmaz", "wielkop", "zachpom")

zb_uczacy <- DANE

#podzia³ zbioru DANE2 na testowy i walidacyjny
#random.numbers <- sample.int(nrow(DANE2))
#quantiles <- quantile(random.numbers, probs = c(0, 0.6, 1))

#split.labels <- cut(random.numbers, quantiles, include.lowest = TRUE,
#                    labels = c("zb_walidacyjny","zb_testowy"))


#split.data <- split(DANE2, split.labels)

#zb_walidacyjny <- split.data$zb_walidacyjny
#zb_testowy <- split.data$zb_testowy

#write.csv(zb_walidacyjny,"zbior_walidacyjny.csv")
#write.csv(zb_testowy,"zbior_testowy.csv")
#zapisalam dane po podziale zeby za kazdym razem pracowac na tych samych

#-------------------------------------------------------#
#                  WCZYTANIE DANYCH                     #
#-------------------------------------------------------#


zb_walidacyjny <- read.csv("zbior_walidacyjny.csv")
zb_testowy <- read.csv("zbior_testowy.csv")

zb_walidacyjny <- zb_walidacyjny[,-1]
zb_testowy <- zb_testowy[,-1]

GR.BADANA <- zb_uczacy[zb_uczacy$wysylka_oferty == 1, ]
GR.KONTROLNA <- zb_uczacy[zb_uczacy$wysylka_oferty == 0, ]

#grupa badana wynosi 2006 obserwacji, a grupa kontrolna 8028

table(GR.BADANA$zakup)
table(GR.KONTROLNA$zakup)


#-------------------------------------------------------#
#                 FUNKCJA LICZACA ZYSK                  #
#-------------------------------------------------------#


licz.zysk <- function(data,predykcje){
  kwota=0
  for (i in 1:length(predykcje)){
    if (predykcje[i]==1){
      kwota = kwota - 10
      if (data$zakup_wysylka[i]==1) kwota=kwota+100
    }
    if (predykcje[i]==0){
      if (data$zakup_bez_wysylki[i]==1) kwota=kwota+100
    }
  }
  kwota
}




#-------------------------------------------------------#
#     REGRESJA Z PE£NYM ZESTAWEM ZMIENNYCH              #
#-------------------------------------------------------#


model.badana <- glm(zakup ~ . , family = binomial(), data = GR.BADANA[,c(-1,-8)]) 
# 1 kolumna to numer, 8 to wysylka oferty (zawsze 1)
summary(model.badana)

model.kontrol <- glm(zakup ~ ., family = binomial(), data = GR.KONTROLNA[,c(-1,-8)])
# 1 kolumna to numer, 8 to wysylka oferty (zawsze 0)
summary(model.kontrol)


# Prawdopodobieñstwa prognozowane zakupu:

#prawdop zakupu jeœli wyœlemy ofertê
P.gr.badana <- predict(model.badana, newdata = zb_walidacyjny, type="response")
hist(sort(P.gr.badana))
plot(sort(P.gr.badana), type="l")

#prawdop zakupu jeœli nie wyœlemy oferty
P.gr.kontrolna <- predict(model.kontrol, newdata = zb_walidacyjny, type="response")
hist(P.gr.kontrolna)
plot(sort(P.gr.kontrolna), type="l")

roznica <- P.gr.badana - P.gr.kontrolna
hist(roznica, xlab="ró¿nica", main="histogram ró¿nica")

#ustalony próg odciêcia 0.1:
abline(v = 0.1, col = "red", lwd = 4, lty = 2)

decyzja <- ifelse(roznica > 0.1, 1, 0)
#jeœli ró¿nica prawdopodobieñstwa na zakup jest wiêksza od 0.1 to wysy³amy ofertê, w przeciwnym razie nie wysy³amy
table(decyzja)

# liczymy zysk
licz.zysk(zb_walidacyjny, decyzja)
#zysk na zbiorze walidacyjnym wynosi 1801730


#-------------------------------------------------------------------#
#      STANDARYZACJA, USUNIÊCIE OBSERWACJI ODSTAJACYCH              #
#-------------------------------------------------------------------#

DANE.B <- model.matrix(zakup ~., data = GR.BADANA[,c(-1,-8)])[,-1]
head(DANE.B)
DANE.B <- scale(DANE.B)
dim(DANE.B)


DANE.B <- cbind(zakup=GR.BADANA$zakup, DANE.B)
summary(DANE.B)

#Usuwamy obserwacje odstajace (ró¿niace sie o wiecej niz 6 odch. st.):
rows.to.erase <- c()

for(i in 1:nrow(DANE.B)){
  if(any(abs(DANE.B[i, ]) > 6)){
    rows.to.erase <- c(rows.to.erase, i)
  }
}
rows.to.erase

FINAL.DANE.B <- DANE.B[-rows.to.erase, ]
dim(FINAL.DANE.B)
colnames(FINAL.DANE.B)


# TO SAMO DLA GRUPY KONTROLNEJ:

DANE.K <- model.matrix(zakup~., data = GR.KONTROLNA[,c(-1,-8)])[,-1]
DANE.K <- scale(DANE.K)
dim(DANE.K)

DANE.K <- cbind(zakup=GR.KONTROLNA$zakup, DANE.K)

rows.to.erase <- c()

for(i in 1:nrow(DANE.K)){
  if(any(abs(DANE.K[i, ]) > 6)){
    rows.to.erase <- c(rows.to.erase, i)
  }
}
rows.to.erase

FINAL.DANE.K <- DANE.K[-rows.to.erase, ]
dim(FINAL.DANE.K)


# ---------------------------------------------------------------------#

DANE.WALIDACJA <- model.matrix(numer ~., data = zb_walidacyjny[,c(-8:-10)])[,-1]
DANE.WALIDACJA <- scale(DANE.WALIDACJA)
dim(DANE.WALIDACJA)
colnames(DANE.WALIDACJA)

DANE.TEST <- model.matrix(numer ~., data = zb_testowy[,c(-8:-10)])[,-1]
DANE.TEST <- scale(DANE.TEST)
dim(DANE.TEST)




#---------------------------------------------------------------------#
#      REGRESJA PONOWNIE ZE WSZYSTKIMI ZMIENNYMI                      #
#---------------------------------------------------------------------#


model.badana <- glm(zakup ~ . , family = binomial(), data = data.frame(FINAL.DANE.B))
summary(model.badana)

zmienne <- is.na(coef(model.badana)) 
zmienne
zmienne <- zmienne == FALSE

#usuwamy zmienne

FINAL.DANE.B <- FINAL.DANE.B[,zmienne]
FINAL.DANE.K <- FINAL.DANE.K[,zmienne]


DANE.WALIDACJA <- DANE.WALIDACJA[, zmienne[-1]]
DANE.TEST <- DANE.TEST[, zmienne[-1]]


#------------------------------------------------------------------#



model.badana <- glm(zakup ~ . , family = binomial(), data = data.frame(FINAL.DANE.B))
summary(model.badana)

model.kontrol <- glm(zakup ~ ., family = binomial(), data = data.frame(FINAL.DANE.K))
summary(model.kontrol)


P.gr.badana <- predict(model.badana, newdata = data.frame(DANE.WALIDACJA), type="response")
hist(sort(P.gr.badana))


P.gr.kontrolna <- predict(model.kontrol, newdata = data.frame(DANE.WALIDACJA), type="response")
hist(P.gr.kontrolna)



roznica <- P.gr.badana - P.gr.kontrolna
hist(roznica, main="histogram ró¿nica", xlab="ró¿nica")


abline(v = 0.1, col = "red", lwd = 4, lty = 2)

decyzja <- ifelse(roznica > 0.1, 1, 0)
table(decyzja)

licz.zysk(zb_walidacyjny, decyzja) # 1807390, wiêc wiêcej niz w regresji poprzedniej


#----------------------------------------------------------------#
#                    LASSO REGRESSION
#----------------------------------------------------------------#


lasso.badana <- cv.glmnet(x = FINAL.DANE.B[,-1], y = FINAL.DANE.B[,1], 
                          family = "binomial", alpha = 1)

lasso.kontr <- cv.glmnet(x = FINAL.DANE.K[,-1], y = FINAL.DANE.K[,1], family = "binomial", alpha = 1)


plot(lasso.badana)
lasso.badana$lambda.min 
lasso.badana$lambda.1se

lambda.bad <- lasso.badana$lambda.min
#lambda.bad <- lasso.badana$lambda.1se

coef(lasso.badana, s="lambda.min")

plot(lasso.kontr)
lasso.kontr$lambda.min
lasso.kontr$lambda.1se

lambda.kontr <- lasso.kontr$lambda.min


coef(lasso.kontr, s="lambda.min")

# Szacujemy modele:

lasso.badana.model <- glmnet(x = FINAL.DANE.B[,-1], y = FINAL.DANE.B[,1], family = "binomial", 
                             alpha = 1, lambda = lambda.bad, standardize = FALSE)

lasso.kontr.model <- glmnet(x = FINAL.DANE.K[,-1], y = FINAL.DANE.K[,1], family = "binomial", 
                            alpha = 1, lambda = lambda.kontr, standardize = FALSE)




P.badana.lasso <- predict(lasso.badana.model, newx = DANE.WALIDACJA, type = "response")
hist(P.badana.lasso)
#plot(sort(P.badana.lasso), type="l")

P.kontr.lasso <- predict(lasso.kontr.model, newx = DANE.WALIDACJA, type = "response")
hist(P.kontr.lasso)
plot(sort(P.kontr.lasso), type="l")


roznica <- P.badana.lasso - P.kontr.lasso
hist(roznica)
abline(v = 0.1, col = "red", lwd = 4, lty = 2)

decyzja <- ifelse(roznica > 0.1, 1, 0)

table(decyzja)

licz.zysk(zb_walidacyjny, decyzja) # 1808920


#---------------------------------------------------------------------#
#                       RIDGE REGRESSION
#---------------------------------------------------------------------#



ridge.badana <- cv.glmnet(x = FINAL.DANE.B[,-1], y = FINAL.DANE.B[,1], 
                          family = "binomial", alpha = 0)
ridge.kontr <- cv.glmnet(x = FINAL.DANE.K[,-1], y = FINAL.DANE.K[,1], family = "binomial", alpha = 0)


plot(ridge.badana)
ridge.badana$lambda.min 
ridge.badana$lambda.1se

lambda.bad <- ridge.badana$lambda.min #lasso.badana$lambda.1se

coef(ridge.badana, s="lambda.min")

plot(ridge.kontr)
ridge.kontr$lambda.min
ridge.kontr$lambda.1se

lambda.kontr <- ridge.kontr$lambda.min


coef(ridge.kontr, s="lambda.min")

# Szacujemy modele:

ridge.badana.model <- glmnet(x = FINAL.DANE.B[,-1], y = FINAL.DANE.B[,1], family = "binomial", 
                             alpha = 0, lambda = lambda.bad, standardize = FALSE)

ridge.kontr.model <- glmnet(x = FINAL.DANE.K[,-1], y = FINAL.DANE.K[,1], family = "binomial", 
                            alpha = 0, lambda = lambda.kontr, standardize = FALSE)




P.badana.ridge <- predict(ridge.badana.model, newx = DANE.WALIDACJA, type = "response")
hist(P.badana.ridge)
#plot(sort(P.badana.ridge), type="l")

P.kontr.ridge <- predict(ridge.kontr.model, newx = DANE.WALIDACJA, type = "response")
hist(P.kontr.ridge)
#plot(sort(P.kontr.ridge), type="l")


roznica <- P.badana.ridge - P.kontr.ridge
hist(roznica)
abline(v = 0.1, col = "red", lwd = 4, lty = 2)

decyzja <- ifelse(roznica > 0.1, 1, 0)

table(decyzja)

licz.zysk(zb_walidacyjny, decyzja) # 1800650


#-------------------------------------------------------------------#
#                             DRZEWO
#-------------------------------------------------------------------#


library(rpart)
library(rattle)


#grupa badana
rpart.model <- rpart(zakup ~ ., data.frame(FINAL.DANE.B), cp=0.00001, minsplit = 20)

# wybor optymalnego poziomu zlozonosci:
min.error <- which.min(rpart.model$cptable[,"xerror"])
opt.cp <- rpart.model$cptable[min.error,"CP"]

model.badana <- prune(rpart.model, cp = opt.cp) #przycinamy drzewo

plot(model.badana, compress = T, uniform = T, branch = 0.3, 
     nspace = 2, margin = 0.1)
text(model.badana, use.n=T, pretty=0)

fancyRpartPlot(model.badana)

#grupa kontrolna
rpart.model <- rpart(zakup ~ ., data.frame(FINAL.DANE.K), cp=0.00001, minsplit = 20)

# wybor optymalnego poziomu zlozonosci:
min.error <- which.min(rpart.model$cptable[,"xerror"])
opt.cp <- rpart.model$cptable[min.error,"CP"]

model.kontrolna <- prune(rpart.model, cp = opt.cp) #przycinamy drzewo

plot(model.kontrolna, compress = T, uniform = T, branch = 0.3, 
     nspace = 2, margin = 0.1)
text(model.kontrolna, use.n=T, pretty=0)

fancyRpartPlot(model.kontrolna)


P.gr.badana <- predict(model.badana, newdata = data.frame(DANE.WALIDACJA))
P.gr.badana <- data.frame(P.gr.badana)
hist(P.gr.badana$P.gr.badana)
#plot(sort(P.gr.badana), type="l")

P.gr.kontrolna <- predict(model.kontrolna, newdata = data.frame(DANE.WALIDACJA))
P.gr.kontrolna <- data.frame(P.gr.kontrolna)
hist(P.gr.kontrolna$P.gr.kontrolna)
#plot(sort(P.gr.kontrolna), type="l")

roznica <- P.gr.badana$P.gr.badana - P.gr.kontrolna$P.gr.kontrolna
hist(roznica)

#ustalony próg odciêcia 0.1:

abline(v = 0.1, col = "red", lwd = 4, lty = 2)

decyzja <- ifelse(roznica > 0.1, 1, 0)
table(decyzja)

licz.zysk(zb_walidacyjny, decyzja) #1734120


#-------------------------------------------------------------------#
#                             LAS LOSOWY
#-------------------------------------------------------------------#
library(randomForest)

model.badana <- randomForest(zakup ~ . , data = data.frame(FINAL.DANE.B), ntree=150)

plot(model.badana, col="black")
varImpPlot(model.badana, bg=1)


model.kontrol <- randomForest(zakup ~ ., data = data.frame(FINAL.DANE.K), ntree=150)

plot(model.kontrol, col="black")
varImpPlot(model.kontrol, bg=1)



P.gr.badana <- predict(model.badana, newdata = data.frame(DANE.WALIDACJA), type="response")
hist(sort(P.gr.badana))
#plot(sort(P.gr.badana), type="l")

P.gr.kontrolna <- predict(model.kontrol, newdata = data.frame(DANE.WALIDACJA), type="response")
hist(P.gr.kontrolna)
#plot(sort(P.gr.kontrolna), type="l")

roznica <- P.gr.badana - P.gr.kontrolna
hist(roznica)

#ustalony próg odciêcia 0.1:

abline(v = 0.1, col = "red", lwd = 4, lty = 2)

decyzja <- ifelse(roznica > 0.1, 1, 0)

licz.zysk(zb_walidacyjny, decyzja) #1757410


#---------------------------------------------------------------------#
#      REGRESJA Z OGRANICZONYM ZBIOREM ZMIENNYCH OBJAŒNIANYCH         #
#---------------------------------------------------------------------#


model.opt.badana <- glm(zakup ~ wiek + wyksztalceniepodstawowe + wyksztalceniepolicealne + wyksztalceniesrednie + wyksztalceniewyzsze, family = binomial(), data = data.frame(FINAL.DANE.B))
summary(model.opt.badana)


model.opt.kontrol <- glm(zakup ~ wiek + wyksztalceniepodstawowe + wyksztalceniepolicealne + wyksztalceniesrednie + wyksztalceniewyzsze, family = binomial(), data = data.frame(FINAL.DANE.K))
summary(model.opt.kontrol)


P.gr.badana <- predict(model.opt.badana, newdata = data.frame(DANE.WALIDACJA), type="response")
hist(sort(P.gr.badana))

P.gr.kontrolna <- predict(model.opt.kontrol, newdata = data.frame(DANE.WALIDACJA), type="response")
hist(P.gr.kontrolna)


roznica <- P.gr.badana - P.gr.kontrolna
hist(roznica, main="histogram ró¿nica", xlab="ró¿nica")

abline(v = 0.1, col = "red", lwd = 4, lty = 2)

decyzja <- ifelse(roznica > 0.1, 1, 0)
table(decyzja)

licz.zysk(zb_walidacyjny, decyzja) # 1786640


#-------------------------------------------------------------------#
#                             SIECI NEURONOWE
#-------------------------------------------------------------------#


library(nnet)

neurons <- 4
decays <- seq(0,40,length=100)

train.error<-valid.error<-numeric(length(decays)) #bledy dla zbioru testujacego i walidacyjnego
neural.nets<-list()

for (d in 1:length(decays)) {
  neural.nets[[d]]<-nnet(zakup~.,data=data.frame(FINAL.DANE.B),size=neurons,decay=decays[d],linout=F,maxit=10000,trace=FALSE)
  train.error[d]<-mean(neural.nets[[d]]$residuals^2)
  prediction<-predict(neural.nets[[d]],newdata=data.frame(FINAL.DANE.B))
  valid.error[d]<-mean((prediction-data.frame(FINAL.DANE.B)$zakup)^2)
}

#wybor najlepszego modelu sieci neuronowej
best.neural.net <- neural.nets[[which.min(valid.error)]]
best.neural.net$decay
#model sieci badana
model.badana <- best.neural.net


neurons <- 4
decays <- seq(0,40,length=100)

train.error<-valid.error<-numeric(length(decays))
neural.nets<-list()

for (d in 1:length(decays)) {
  neural.nets[[d]]<-nnet(zakup~.,data=data.frame(FINAL.DANE.K),size=neurons,decay=decays[d],linout=F,maxit=10000,trace=FALSE)
  train.error[d]<-mean(neural.nets[[d]]$residuals^2)
  prediction<-predict(neural.nets[[d]],newdata=data.frame(FINAL.DANE.K))
  valid.error[d]<-mean((prediction-data.frame(FINAL.DANE.K)$zakup)^2)
}

#wybor najlepszego modelu sieci neuronowej
best.neural.net <- neural.nets[[which.min(valid.error)]]
best.neural.net$decay
model.kontrol <- best.neural.net


P.gr.badana <- predict(model.badana, newdata = data.frame(DANE.WALIDACJA))
hist(sort(P.gr.badana))
#plot(sort(P.gr.badana), type="l")

P.gr.kontrolna <- predict(model.kontrol, newdata = data.frame(DANE.WALIDACJA))
hist(P.gr.kontrolna)
#plot(sort(P.gr.kontrolna), type="l")

roznica <- P.gr.badana - P.gr.kontrolna
hist(roznica)

#ustalony próg odciêcia 0.1:

abline(v = 0.1, col = "red", lwd = 4, lty = 2)

decyzja <- ifelse(roznica > 0.1, 1, 0)
table(decyzja)

licz.zysk(zb_walidacyjny, decyzja) # 1784730


#-------------------------------------------------------#
#         PREDYKCJA NA ZBIORZE TESTOWYM                 #
#-------------------------------------------------------#

P.badana.lasso <- predict(lasso.badana.model, newx = DANE.TEST, type = "response")
hist(P.badana.lasso)
#plot(sort(P.badana.lasso), type="l")

P.kontr.lasso <- predict(lasso.kontr.model, newx = DANE.TEST, type = "response")
hist(P.kontr.lasso)
#plot(sort(P.kontr.lasso), type="l")


roznica <- P.badana.lasso - P.kontr.lasso
hist(roznica)
abline(v = 0.1, col = "red", lwd = 4, lty = 2)

decyzja <- ifelse(roznica > 0.1, 1, 0)

table(decyzja)

licz.zysk(zb_testowy, decyzja) # 1206500




#-------------------------------------------------------#
#       WYLICZENIE MAX ZYSKU NA ZB. WALIDACYJNYM        #
#-------------------------------------------------------#

#maksymalny zysk jaki firma moze osiagnac na zbiorze walidacyjnym, czyli rozwiazanie optymalne
# to 2 246 690

decyzja_opt <- runif(nrow(zb_walidacyjny), 0,0)
decyzja_opt <- as.data.frame(decyzja_opt)

for (i in 1:nrow(decyzja_opt)){
  if (zb_walidacyjny$zakup_bez_wysylki[i]==1) decyzja_opt$decyzja_opt[i]=0
  if (zb_walidacyjny$zakup_bez_wysylki[i]==0 & zb_walidacyjny$zakup_wysylka[i]==1) decyzja_opt$decyzja_opt[i]=1
}



kwota=0
for (i in 1:60537){
  if (decyzja_opt$decyzja_opt[i]==1){
    kwota = kwota - 10
    if (zb_walidacyjny$zakup_wysylka[i]==1) kwota=kwota+100
    }
  if (decyzja_opt$decyzja_opt[i]==0){
    if (zb_walidacyjny$zakup_bez_wysylki[i]==1) kwota=kwota+100
    }
}
kwota



#-------------------------------------------------------#
#         WYLICZENIE MAX ZYSKU NA ZB. TESTOWYM          #
#-------------------------------------------------------#

#maksymalny zysk jaki firma moze osiagnac na zbiorze testowym, czyli rozwiazanie optymalne
# to 1 493 660

decyzja_opt <- runif(nrow(zb_testowy), 0,0)
decyzja_opt <- as.data.frame(decyzja_opt)

for (i in 1:nrow(decyzja_opt)){
  if (zb_testowy$zakup_bez_wysylki[i]==1) decyzja_opt$decyzja_opt[i]=0
  if (zb_testowy$zakup_bez_wysylki[i]==0 & zb_testowy$zakup_wysylka[i]==1) decyzja_opt$decyzja_opt[i]=1
}



kwota=0
for (i in 1:40358){
  if (decyzja_opt$decyzja_opt[i]==1){
    kwota = kwota - 10
    if (zb_testowy$zakup_wysylka[i]==1) kwota=kwota+100
  }
  if (decyzja_opt$decyzja_opt[i]==0){
    if (zb_testowy$zakup_bez_wysylki[i]==1) kwota=kwota+100
  }
}
kwota
