rm(list=ls())

#Zdefiniowanie gradientu
gradient = function(f, x, h) {
  n = length(x)
  g = rep(0,n)
  for (i in 1:n) {
    e_i = rep(0,n)
    e_i[i] = 1
    g[i] = (f(x + h * e_i) -  f(x - h * e_i)) / (2 * h) 
  }
  return(g)
}
install.packages("genalg")
library(genalg)

###########################################################################################################################
### METODA NAJSZYBSZEGO SPADKU  (STEPEST GRADIENT DESCENT) ###
###########################################################################################################################
# f-optymalizowana funkcja
# x0-punkt startowy
# n_iter-maksymalna liczba iteracji
# h-parametr perturbacyjny (pochodna numeryczna)
lineSearchMonteCarlo = function(f, x_0, x_1, sampleSize) {
  x_opt = x_0;
  f_opt = f(x_0)
  for (i in 1 : sampleSize) {
    x = x_0 + runif(1) * (x_1 - x_0)
    if (f(x) < f(x_opt)) {
      x_opt = x;
      f_opt = f(x_opt)
    }
  }
  return(x_opt)
}


metoda_najszybszego_spadku = function(f,x0, n_iter, alfa=0.01,h=10^-5) {
  k=1
  y_opt=f(x0)
  x_opt=x0
  x_new=x0 - alfa * gradient(f,x_opt,h)
  x_hist=x0
  f_hist=f(x0)
  time.taken<-rep(0,n_iter)
  while (k<n_iter) {
    start.time <- Sys.time()
    x_new = lineSearchMonteCarlo(f, x_opt, x_opt - alfa * gradient(f, x_opt, h), 100)
    #alfa=1/(k*10) #aktualizacja parametru alfa
    k=k+1
    x_opt=x_new
    x_hist=rbind(x_hist,x_opt)
    f_hist=rbind(f_hist,f(x_opt))
    end.time <- Sys.time()
    time.taken[k] <- end.time - start.time
  }
  cat("METODA NAJSZYBSZEGO SPADKU","\n","Znaleziono minimum funkcji w punkcie:",
      "\n","x=(",x_opt[1],",",x_opt[2],")","\n","Wartoœæ funkcji w tym punkcie wynosi:","\n","f(x)=",f(x_opt),"\n",
      "Liczba Iteracji;","\n",k,"\n","Sredni czas iteracji to",mean(time.taken))
  plot(c(1:k),f_hist,xlab="Numer iteracji",ylab="Wartoœæ funkcji", main="Metoda najszybszego spadku")
}



###########################################################################################################################
### METODA SYMULOWANEGO WY¿ARZANIA  (SIMULATED ANNEALING) ###
###########################################################################################################################

#Zdefiniowanie temeperatury
# a-parametr temperatury
# k-numer iteracji
temp=function(a,k) {
  temp=(1-a)^k
  return(temp)
}

#Funkcja losuj¹ca s¹siedni punkt
# f-funkcja
# x_opt-punkt startowy
# c-parametr przsuniêcia
# h-parametr perturbacyjny (pochodna numeryczna)
# a-parametr temperatury
# k-numer iteracji
punkt=function(f,x_opt,c,h,a,k){
  x_new = x_opt - c * f(x_opt) / gradient(f,x_opt,h)+sqrt(2*temp(a,k))*rnorm(1,0,1)
  return(x_new)
}

#Funkcja aktywacji
# f-optymailizowana funkcja
# x_opt-punkt pierwotny
# x_new-punkt przeznaczenia
# a-parametr temperatury
# k-liczba iteracji
f_aktyw=function(f,x_opt,x_new,a,k){
  prawd=min(1,exp(f(x_opt)-f(x_new))/temp(a,k))
  return(prawd)
}

#Algorytm
# f-ptymalizowana funkcja
# x0-punkt startowy
# n_iter-maksymalna liczba iteracji
# a-parametr temperatury
# h-parametr perturbacyjny (pochodna numeryczna)
symulowane_wyzarzanie = function(f,x0, n_iter=10000,a=0.1,h=10^-5,tol=10^-15) {
  k=1
  y_opt=f(x0)
  x_opt=x0
  c=1
  x_new=punkt(f,x_opt,c,h,a,k)
  x_hist=x0
  f_hist=f(x0)
  time.taken<-rep(0,10000)
  while (k<n_iter) {
    c=1/(k*1)
    k=k+1
   
      start.time <- Sys.time()
      
    x_new = punkt(f,x_opt,c,h,a,k) #generowanie losowego sasiedniego punktu
    if (f(x_new)<f(x_opt)) {
      x_opt=x_new
    } else {
      if (f_aktyw(f,x_opt,x_new,a,k)>runif(1,0,1)) {
        x_opt=x_new
      } else {
        x_opt=x_opt
      }
    }
    x_hist=rbind(x_hist,x_opt)
    f_hist=rbind(f_hist,f(x_opt))
    end.time <- Sys.time()
    time.taken[k] <- end.time - start.time
  }
  cat("METODA WY¯ARZANIA SYMULOWANEGO","\n","Znaleziono minimum funkcji w punkcie:",
      "\n","Znaleziono minimum funkcji w punkcie:",
      "\n","x=(",x_opt[1],",",x_opt[2],")","\n","Wartoœæ funkcji w tym punkcie wynosi:","\n","f(x)=",f(x_opt),"\n",
      "Liczba Iteracji;","\n",k,"\n","Sredni czas iteracji to",mean(time.taken))
  plot(c(10:k),f_hist[10:k],xlab="Numer iteracji",ylab="Wartoœæ funkcji", main="Symulowane Wy¿arzanie")
}

#Zdefiniowanie funkcji Schafer'a
fun=function(x){
  f=0.5 + ((sin(x[1]^2-x[2]^2))^2-0.5)/((1+0.001*(x[1]^2+x[2]^2))^2)
  return(f)
}

#Wyœwielt wykres funkcji Schafer'a
n_grid =  100
x <- seq(-2,2,length=n_grid)
val = matrix(0,n_grid,n_grid)
for (i in 1:n_grid){
  for (j in 1:n_grid){
    val[i,j]=fun(c(x[i],x[j]))
  }
}

#Porównanie metody najsybszego spadku i symulowanego wy¿arzania
x_start = c(6,6)
n_iter = 2000
par(mfrow=c(2,2))
symulowane_wyzarzanie(fun,x0=x_start,n_iter=n_iter,a=0.01,h=10^-5)
metoda_najszybszego_spadku(fun,x0=x_start,n_iter=n_iter)
persp(x,x,val, col='blue', theta = 80, phi=45)
contour(x,x,val)




###########################################################################################################################
### ALGORYTM GENETYCZNY ###
###########################################################################################################################
rm(list=ls())
#Zdefiniowanie funkcji Schafer'a
Schafer=function(x){
  f=0.5 + ((sin(x[1]^2-x[2]^2))^2-0.5)/((1+0.001*(x[1]^2+x[2]^2))^2)
  return(f)
}

#Definicja funkcji gradient
gradient = function(f, x, h) {
  n = length(x)
  g = rep(0,n)
  for (i in 1:n) {
    e_i = rep(0,n)
    e_i[i] = 1
    g[i] = (f(x + h * e_i) -  f(x - h * e_i)) / (2 * h) 
  }
  return(g)
}

#Algorytm
# f-optymalizowana funkcja
# pokolenia-liczba pokoleñ
# osob-liczba osobników w populacji
# LG-liczba genów
# PMut-prawdopodobieñstwo mutacji
# PCross-prawdopodobieñstwo krzy¿owania
# x_min-minimalna wartoœæ X
# x_max-maksymlana wartoœæ X
# eps-punkt odciêcia dla kryterium stop
# h-parametr perturbacyjny pochodnej numerycznej
alg_gen=function(f,pokolenia=1000,osob=20,LG=9,PMut=0.1,PCross=0.5,x_min=c(0,0),x_max=c(10,10),eps=0.00001,x0=c(1,1),h=10^-5) {
  min=f(x0) #delaracja zmiennej 'min' przechowuj¹cej wartoœæ funkcji celu dla najlepszego rozwi¹zania
  min_x=x0 #deklaracja zmiennej 'min_x' przechowuj¹cej najszepsze rozwi¹zanie
  f_hist=f(x0) #historyczne wartoœci funckji celu
  x_hist=x0 #historyczne wartoœci rozwi¹zania
  #Tworzenie populacji z³o¿onej z liczby:'osob' osobników
  populacja1=matrix(data=0,nrow=osob,ncol=16) #populacja podstawowa
  populacja2=matrix(data=0,nrow=osob,ncol=16) #populacja tymczasowa
  for (i in 1:osob) {
    for (j in 1:LG) {
      if (runif(1,0,1)<0.5) {
        populacja1[i,j]=1
      } else {
        populacja1[i,j]=0
      }
    }
  }
  
  #Pêtla dla wszystkich pokolen
  X=x0
  k=1
  time.taken<-rep(0,1000)
  while ((k<pokolenia) && (abs(gradient(f,min_x,h))>eps)) {
    start.time <- Sys.time()
    
    #kryteria stop
    #Przystosowanie schematu
    # Obliczenie sumarycznego przystosowania pokolenia
    PL=0 #Suma przystosowañ dla danego pokolenia
    for (i in 1:osob) {
      #Przystosowanie osobnika
      dec=0
      for (j in 1:LG) {
        if (populacja1[i,j]==1) {
          dec=dec+populacja1[i,j]*(2^(LG-j)) #odejmujemy j, czyli liczbê zmiennych na pozycjach ju¿ ustalonych
        }
      }
      populacja1[i,10]=dec #Przystosowanie osobnika
      X=((x_max-x_min)/(2^LG-1))*dec+x_min #rozwiazanie dla danego osobnika 
      populacja1[i,11]=X[1]
      populacja1[i,12]=X[2]
      Y=f(X) # wartosc funkcji celu w wybranym rozwi¹zaniu (przystosowanie osobnika)
      populacja1[i,13]=Y 
      PL=PL+Y #mianownik przystosowania wzglêdnego (suma wartosci funkcji celu dla wszystkich osobnikow)
    }
    
    #Przystosowanie wzglêdne
    for (i in 1:osob) {
      populacja1[i,14]=populacja1[i,13]/PL
    }
    
    #Najlepsze rozwi¹zanie w populacji
    Max=populacja1[1,13] 
    NrMax=1
    for (i in 2:osob) {
      if (populacja1[i,13]<Max) {
        Max=populacja1[i,13]
        NrMax=i
      }
    }
    f_hist=rbind(f_hist,populacja1[NrMax,13])
    x_hist=rbind(x_hist,c(populacja1[NrMax,11],populacja1[NrMax,12]))
    
    #Przedzia³y ruletki
    for (i in 1:osob) { 
      populacja1[i,15]=runif(1,0,1) #punkt startowy przedzia³u ruletki
      populacja1[i,16]=min(1,populacja1[i,15]+populacja1[i,14]) #punkt koñcowy przedzia³u ruletki
    }
    
    #Selekcja
    for (j in 1:osob) { 
      los=runif(1,0,1)
      if ((los>=populacja1[j,15]) & (los<populacja1[j,16])) {
        populacja2[j,]=populacja1[j,]
      }
    }
    
    #Krzy¿owanie
    for (i in 1:osob) { 
      los=runif(1,0,1) 
      if (los<PCross) { 
        Nr=round(runif(1,1,19)) #wybór osobnika, z którym zostanie skrzy¿owany osobnik i
        Cut=round(runif(1,1,8)) #punkt odciêcia dla krzy¿owania 
        c1=populacja2[i,(Cut+1):LG]
        c2=populacja2[Nr,(Cut+1):LG]
        populacja2[i,(Cut+1):LG]=c2
        populacja2[Nr,(Cut+1):LG]=c1 
      }
    }
    
    #Mutacja
    for (i in 1:osob) { 
      for (j in 1:LG) {
        los=runif(1,0,1)
        if (los<PMut) { 
          if (populacja2[i,j]==1) {
            populacja2[i,j]=0
          }  else {
            populacja2[i,j]=1 
          }
        }
      }
    }
    
    #Zamiana populacji
    populacja1=populacja2
    
    
    if (min>=f(x_hist[k,])) {
      min_x=x_hist[k,]
    }
    end.time <- Sys.time()
    time.taken[k] <- end.time - start.time
    k=k+1
  }
  
  cat("ALGORYTM GENETYCZY","\n","Znaleziono minimum funkcji w punkcie:",
      "\n","x=(",min_x[1],",",min_x[2],")","\n","Wartoœæ funkcji w tym punkcie wynosi:","\n","f(x)=",min,"\n",
      "Iloœæ potrzebnych iteracji:","\n",k,"\n","Sredni czas iteracji to",mean(time.taken))
  plot(c(1:(length(f_hist))),f_hist,xlab="Numer iteracji",ylab="Wartoœæ funkcji", main="Algorytm genetyczny")
  
}
alg_gen(Schafer,pokolenia=1000,osob=20,LG=9,PMut=0.1,PCross=0.5,x_min=c(-10,-10),x_max=c(10,10),eps=0.00001,x0=c(1,1))




rbga.results = rbga.bin(size=9, mutationChance=0.2, zeroToOneRatio=0.5, evalFunc=Schafer)
tail(rbga.results$evaluations,1)






##############################################################

##############################################################
##############################################################
##############################################################
##############################################################

#a) 
par(mfrow=c(2,1))
alg_gen(Schafer,pokolenia=1000,osob=20,LG=9,PMut=0.1,PCross=0.5,x_min=c(-10,-10),x_max=c(10,10),eps=0.00001,x0=c(1,1))
abline(h=0.0019, col="red")
symulowane_wyzarzanie(fun,x0=x_start,n_iter=n_iter,a=0.01,h=10^-5)
abline(h=0.009, col="red")



#b)
alg_gen(Schafer,pokolenia=1000,osob=20,LG=9,PMut=0.1,PCross=0.5,x_min=c(-10,-10),x_max=c(10,10),eps=0.00001,x0=c(1,1))
symulowane_wyzarzanie(fun,x0=x_start,n_iter=n_iter,a=0.01,h=10^-5)
#Sredni czas wykonania 1 iteracji znajduje sie w ostaniej linii wydruku kazdego z algorytmów

#c) Algorytm genetyczny w naszym wypadku tworzy na poczatko losowa grupe 16 osobnikóW w populacji stalej i tymczasowej 
#którzy w kolejnych krokach beda poddawani mutacjom i krzyzowaniu.
# #Nastepnie inicjowana jest glówna petla, która ma domyslnie 1000 pokolen i w jej trakcie nastepuja
# f=function(){ Nastepnie obliczamy przystosowanie bezwzgledne i wzgledne, które jest 
#   funkcja celu danego osobnika oraz ulamkiem calkowitej funkcji celu wszystkich
#   osobników jaki jest jego udzialem. Kolejnym korkiem jest wybór najlepszego 
#   rozwiazania w populacji i dokonania selekcji na podstawie ruletki poszczególnych
#    osobników. Zaleta tego rozwiazania jest dopuszczenie przy pewnym malym
#   prawdopodobienstwie rozwiazan gorszych w celu poszukiwania mutacji pozwalajacych
#   na stworzenie rozwiazania jeszcze lepszego. Pozniej dokonywane jest krzyzowanie polegajace
#   na wycieciu pewnego fragmentu chromosomu od danego osobnika populacji i przekazaniu go 
#   innemu osobnikowi, który zostal wybrany losowo i oddal fragment chromosomu o tej samej 
#   dlugosci. Ostatecznie dokonywana jest mutacja polegajaca na zamianie losowym osobnikom
#   czesci chromosomu na odwrotna, czyli z 0 na 1 i vice versa. Finalnie nastepuje nadpisanie 
#   populacji stalej i kolejna iteracja. W naszym przypadku algorytm nie dazy do rozwiazania 
#   optymalnego i    mozemy stwierdzic na podstawie wykresów, ze w pewnych momentach populacja 
#   odbiega znaczaco od minimum.
#   Algorytm wyszarzania natomiast na zdefiniowaniu na poczatku funkcji temperatury, aktywacji 
#   
#   i losujacej sredni punkt. Nastepnie zostaje wylosowany punkt startowy, ustalona zostaje 
#   pierwotne optimum. Potem rozpoczyna sie pierwsza iteracja w której losowy wybierany jest 
#   pierwszy punkt aby nastepnie ocenic czy jest lepszy od optymalnego czy nie. Jezeli jest 
#   gorszy to z okreslonym prawdopodobienstwem funkcji aktywacji optimum moze zostac nadpisane 
#   przez gorszy punkt. Funkcja aktywacji jednak zmniejsza to prawdopodobienstwo wraz z czasem,
#   wiec w pózniejszych stadiach algorytm dazy do jednego optimum i nie próbuje alternatywnych dróg.
# W metodzie najszybszego spadku natomiast wykorzystuje sie metoda Monte Carlo, która sprawdza 
# losowo wybrane punkty czy posiadaja nizsza wartosc funkcji celu niz punkt rozpoczecia. 
# Punkt docelowy obliczany jest przez dodanie punktu wyjscia i róznicy punktu pierwotnego 
# i docelowego pomnozonej przez losowy czynnik.  Algorytm jest prosty w dzialaniu, lecz ma 
# tendencje do utykania w minimach lokalnych, gdyz nie ma czynnika losowego jak w przypadku 
# pozostalych dwóch algorytmów (mutacja i funkcja aktywacji) który pozwolilby na eksploracje 
# nieoptymalnych rozwiazan, które moglyby prowadzic do minimum globalnego. Metoda najszybszego 
# spadku jest tez najbardziej czasochlonna metoda, gdyz dla symulacji w trakcie kolokwium srednie 
# czasy iteracji wygladaly nastepujaco. Dla 1000 iteracji wyszarzanie symulacyjne osiagnelo czas 0,26
# sekundy/  algorytm najszybszego spadku 1,71 sekundy/ algorytm genetyczny 1,93 sekundy. W przypadku 
# najmniejszej wartosci to wyszarzanie symulacyjne znalazlo minimum w punkcie -2,2 i -2,2 
# wynoszace 0,009/ algrytm najszybszego spadku w punkcie -5,9 i 5,9 wynoszace 0,06/ algorytm 
# genetyczny -0,6 i -0,6 wynoszace 0,0019. Oznacza to, ze przy niewielkim wzroscie czasu przy 
# obliczeniach algorytm genetyczny jest najlepszym wyborem do poszukiwania minimów, lecz nalezy 
# pamietac, ze algorytm ten jest trudny w zakodowaniu i nalezy sporo czasu poswiecic na stworzenie
# odpowiedniego chromosomu. Dlatego tez dla szybkich analiz duzych zbiorów danych najlepszym okazuje 
# sie algorytm wyszarzania, który jest szybki i znajduje szybko lokalne minimum, bo punkt -2,2 i -2,2
# jest polozony daleko od globalnego minimum funkcji Schaffera w punkcie 0,0.
# Nalezy tez zaznaczyc, ze algorytm genetyczny jest silniej zalezny od elementu losowego co widac na wykresie
# niz algorytm wyszarzania symulowanego


x_start = c(6,6)
n_iter = 1000
par(mfrow=c(2,2))
time.taken<-rep(0,3)
start.time <- Sys.time()
symulowane_wyzarzanie(fun,x0=x_start,n_iter=n_iter,a=0.01,h=10^-5)
end.time <- Sys.time()
time.taken[1] <- end.time - start.time
start.time <- Sys.time()
metoda_najszybszego_spadku(fun,x0=x_start,n_iter=n_iter)
end.time <- Sys.time()
time.taken[2] <- end.time - start.time
start.time <- Sys.time()
alg_gen(Schafer,pokolenia=1000,osob=20,LG=9,PMut=0.1,PCross=0.5,x_min=c(-10,-10),x_max=c(10,10),eps=0.00001,x0=c(1,1))
end.time <- Sys.time()
time.taken[3] <- end.time - start.time
cbind(c("Wyszarzanie","Najszybszy spadek","Algorytm Genetyczny"),time.taken)


