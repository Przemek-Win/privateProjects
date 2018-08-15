# -*- coding: utf-8 -*-
"""
Created on Mon May  9 18:09:30 2016

@author: kasia
"""

import numpy as np
import pandas as pd

class Bar:
    def __init__(self, stanGotówki):
        self.kolejkaKasy = []
        self.kolejkaDrive = []
        self.kasy = []
        self.drive = []
        self.kasa = stanGotówki
        
    def rozliczPrzychód(self, kwota):
        self.kasa += kwota
        
    def nowyKlient(self, klient):
        if isinstance(klient, Pieszy):
            self.kolejkaKasy.append(klient)
        else:
            self.kolejkaDrive.append(klient)
        
    def zmniejszKolejkę(self, klient):
        if isinstance(klient, Pieszy):
            self.kolejkaKasy.remove(klient)
        else:
            self.kolejkaDrive.remove(klient)
        
    def dodajKasę(self, stanowisko):
        self.kasy.append(stanowisko)
        
    def dodajDrive(self, stanowisko):
        self.drive.append(stanowisko)
        
    def wolnaKasa(self):
        for k in self.kasy:
            if k.wolne == 0:
                return(k)
        return(None)
    
    def wolnyDrive(self):
        for d in self.drive:
            if d.wolne == 0:
                return(d)
        return(None) 
        
    def zamknijBar(self):
        for k in self.kasy:
            k.wolne = 0
        for d in self.drive:
            d.wolne = 0
        self.kolejkaKasy = []
        self.kolejkaDrive = []
        
class Stanowisko:
    def __init__(self, bar, szybkośćObsługi): #szybkość Obsługi to parametr rozkładu
        self.wolne = 0
        self.bar = bar
        self.szybkośćObsługi = szybkośćObsługi
        
    def przyjmijZamówienie(self, klient, kuchnia, cena, wielkośćZam, t):
        self.bar.zmniejszKolejkę(klient) #usuwam klienta z kolejki
        kuchnia.wydajPosiłek(self.bar, wielkośćZam, t) #wydaję posiłek
        self.bar.rozliczPrzychód(cena * wielkośćZam) #rozliczam CF
        self.wolne = np.random.exponential(self.szybkośćObsługi)
        
class Kasa(Stanowisko):
    pass
    
class Drive(Stanowisko):
    pass

class Klient:
    def __init__(self, bar):
        self.bar = bar
        self.ileCzeka = 0
    
    def rezygnuj(self):
        self.bar.zmniejszKolejkę(self)
        
    def czekaj(self, t):
        self.ileCzeka += t

class Pieszy(Klient):
    pass

class Samochód(Klient):
    pass
        
class Posiłek:
    def __init__(self, momentPrzygotowania):
        self.momentPrzyg = momentPrzygotowania

class Magazyn:
    def __init__(self, bar, próg):
        self.bar = bar
        self.stan = 0
        self.próg = próg
    
    def pobierzTowar(self, ilość):
        self.stan -= ilość
  
    def sprawdźMagazyn(self, cena, ilość):
        if self.stan < self.próg:
            self.stan += ilość
            self.bar.rozliczPrzychód(-cena * ilość)
  
class Kuchnia:
    def __init__(self):
        self.gotowePosiłki = []
        self.wyrzucone = 0
        
    def przygotujPosiłek(self, magazyn, ilość, t):
        magazyn.pobierzTowar(ilość)
        posiłek = Posiłek(t)
        self.gotowePosiłki.append(posiłek)
        
    def wydajPosiłek(self, bar, ilość, t):
        try:
            self.gotowePosiłki = self.gotowePosiłki[ilość:]
        except IndexError:
            print("Buła")
    
    def wyrzuć(self, t):
        temp = self.gotowePosiłki[:]
        for pos in temp:
            if t - pos.momentPrzyg > 0.5:
                self.gotowePosiłki.remove(pos)
                self.wyrzucone += 1

def załóżBar(nKas, nDrive, szybkośćObsługiKasa, 
             szybkośćObsługiDrive, kapitałPoczątkowy):
#    input:
#        nKas - liczba kas
#        nDrive - liczba drive thru
#        szybkośćObsługi x2 - lambdy dla rozkładu wykładniczego czasu obsługi
#        kapitałPoczątkowy - jasne
#    output:
#        bar (obiekt do następnej funkcji)
    
    bar = Bar(kapitałPoczątkowy)
    
    for i in range(nKas):
        bar.dodajKasę(Kasa(bar, szybkośćObsługiKasa))

    for i in range(nDrive):
        bar.dodajDrive(Drive(bar, szybkośćObsługiDrive))        
    
    return(bar)
    
def prowadźDziałalność(bar, lambdaPiesi, lambdaSamochody, prógZamówień,
                       cenaPosiłku, ilośćPosiłków , horyzont ,
                          cenaTowaru):
    
#    input:
#        bar - obiekt z poprzedniej funkcji
#        lambdaPiesi - częstotliwość pojawiania się klientów pieszych
#        lambdaSamochody - częstotliwość pojawiania się klientów w Drive Thru
#        prógZamówień - jak mało mamy towaru w magazynie żeby zamawiać
#        cenaPosiłku - ile kosztuje posiłek
#        ilośćPosiłków - ile posiłków przygotowuje kuchnia (co 15 min)
#        horyzont - w minutach, defaultowo 1 dzień (12h)
#        cenaTowaru - ile płacimy za towar
#        
#    output:
#        kapitał po [horyzont] działalności
    
       
    #rozliczamy się w minutach                          
    magazyn = Magazyn(bar, prógZamówień)
    kuchnia = Kuchnia()
    
    #setup warunków startowych
    t1 = 0 #t1 - czas teraz
    t0 = 0 #t0 - czas poprzednio
    następnyPieszy = np.random.exponential(lambdaPiesi)
    następnySamochodem = np.random.exponential(lambdaSamochody)
    następnyKlient = min(następnyPieszy, następnySamochodem)
    wolnaKasa = 0
    wolnyDrive = 0
    stanowisko = None
    licznikRez = 0
    maksKasy = 0
    maksDrive = 0
    klienci = 0
    auta = 0
   
    # posiłki przygotowujemy w rownych interwalach czasowych (co 15 min)
    posilki = range(0, horyzont + 15, 15)
   
    #prowadzimy działalność
    while t1 <= horyzont:
        
        if len(bar.kolejkaKasy) > maksKasy:
            maksKasy = len(bar.kolejkaKasy)

        if len(bar.kolejkaDrive) > maksDrive:
            maksDrive = len(bar.kolejkaDrive)
            
        #robimy porządek w kuchni i przygotowujemy jedzenie
        wydajemy = [i for i in posilki if i <= t1]
        if wydajemy:
            posilki = posilki[(wydajemy[len(wydajemy)-1]):]
            for moment in wydajemy:
                kuchnia.przygotujPosiłek(magazyn, ilośćPosiłków, moment)
        kuchnia.wyrzuć(t1)
        
        #robimy porządek w magazynie
        magazyn.sprawdźMagazyn(cenaTowaru, 50)
        
        #aktualizujemy stan na kasach
        for kasa in bar.kasy:
            kasa.wolne = max(kasa.wolne - (t1 - t0), 0)
            if kasa.wolne < 0.01:
                kasa.wolne = 0
        for drive in bar.drive:
            drive.wolne = max(drive.wolne - (t1 - t0), 0)
            if drive.wolne < 0.01:
                drive.wolne = 0
            
        #aktualizujemy stan kolejki
        for klient in bar.kolejkaKasy:
            klient.czekaj(t1-t0)
            if klient.ileCzeka > 15:
                klient.rezygnuj()
                licznikRez += 1
        for klient in bar.kolejkaDrive:
            klient.czekaj(t1-t0)
            if klient.ileCzeka > 10:
                klient.rezygnuj()
                licznikRez += 1
                
        #obsługujemy    
        if t1 == t0 + następnyPieszy:
            klient = Pieszy(bar)
            klienci += 1
            zamówienie = 1 #int(np.random.poisson(1))
            bar.nowyKlient(klient)
            stanowisko = bar.wolnaKasa()
            if stanowisko != None:
                stanowisko.przyjmijZamówienie(klient, kuchnia, cenaPosiłku, zamówienie, t1)
            następnyPieszy = np.random.exponential(lambdaPiesi)
            następnySamochodem -= (t1 - t0)
        elif t1 == t0 + następnySamochodem:
            klient = Samochód(bar)
            auta += 1
            zamówienie = int(np.random.poisson(1.5))
            bar.nowyKlient(klient)
            stanowisko = bar.wolnyDrive()
            if stanowisko != None:
                stanowisko.przyjmijZamówienie(klient, kuchnia, cenaPosiłku, zamówienie, t1)
            następnySamochodem = np.random.exponential(lambdaSamochody)
            następnyPieszy -= (t1 - t0)
        elif t1 == t0 + wolnaKasa and bar.kolejkaKasy != []:
            klient = bar.kolejkaKasy[0]
            zamówienie = int(np.random.poisson(1))
            stanowisko = bar.wolnaKasa()
            stanowisko.przyjmijZamówienie(klient, kuchnia, cenaPosiłku, zamówienie, t1)
            następnyPieszy -= (t1 - t0)
            następnySamochodem -= (t1 - t0)
        elif t1 == t0 + wolnyDrive and bar.kolejkaDrive != []:
            klient = bar.kolejkaDrive[0]
            zamówienie = int(np.random.poisson(1.5))
            stanowisko = bar.wolnyDrive()
            stanowisko.przyjmijZamówienie(klient, kuchnia, cenaPosiłku, zamówienie, t1)
            następnyPieszy -= (t1 - t0)
            następnySamochodem -= (t1 - t0)
            
        #sprawdzamy co wydarzy się następne - nowy klient czy wolna kasa?
        wolnaKasa = min([x.wolne for x in bar.kasy])
        wolnyDrive = min([x.wolne for x in bar.drive])        
        następnyKlient = min(następnyPieszy, następnySamochodem)
        t0 = t1
        if bar.kolejkaKasy != []:
            t1 += min(następnyKlient, wolnaKasa)
        elif bar.kolejkaDrive != []:
            t1 += min(następnyKlient, wolnyDrive)
        else:
            t1 += następnyKlient
    
    #czyścimy kolejki i kasy na koniec dnia
    bar.zamknijBar()     
    return((bar.kasa, klienci, auta, maksKasy, maksDrive, licznikRez, kuchnia.wyrzucone))

wynik_skum = []
wynik_skum_l_por=[]
l_symul = 1000

def symulacja_porownanie(lambdaPiesi, lambdaSamochody, prógZamówień,
                     cenaPosiłku, ilośćPosiłków, horyzont,
                     cenaTowaru, nKas, nDrive, szybkośćObsługiKasa, szybkośćObsługiDrive, kapitałPoczątkowy):
    wynik_skum = []  
    for i in range(l_symul):        
        bar = załóżBar(nKas, nDrive, szybkośćObsługiKasa, 
             szybkośćObsługiDrive, kapitałPoczątkowy)
        wynik = prowadźDziałalność(bar, lambdaPiesi, lambdaSamochody, prógZamówień,
                     cenaPosiłku, ilośćPosiłków, horyzont, cenaTowaru)          
        wynik_skum.append(wynik)
        df=pd.DataFrame(wynik_skum, columns=['Średnia wartość w kasie pod koniec dnia',
        'Średnia liczba klientow pieszych', 'Średnia liczba klientow samochodowych',
        'Maksymalna długość kolejki dla pieszych (średnio)',
        'Maksymalna długość kolejki dla samochodów (średnio)',
        'Ile średnio zrezygnowało', 'Niesprzedane posiłki'])
        idx = df.mean()
    return(idx)

wynik_podsumowanie=symulacja_porownanie(lambdaPiesi=4, lambdaSamochody=7, prógZamówień=10,
                     cenaPosiłku=20, ilośćPosiłków=3, horyzont=12*60,
                     cenaTowaru=5, nKas=2, nDrive=2, szybkośćObsługiKasa=1, szybkośćObsługiDrive=1, kapitałPoczątkowy=0)
                     
print(wynik_podsumowanie) 
#        
    
    