import glob
import pandas as pd
import numpy as np
import math
import pickle
import datetime as dt
from datetime import datetime

global Base

Coinsprices= pd.read_csv('C:/ML/CoinsReducedfinal.csv',index_col=None, header=0) #Remember there is no bitcoin
Articles=pd.read_csv('C:/ML/Articles.csv',index_col=None, header=0)
Reddit= pickle.load(open("C:/ML/Reeditsum.p", "rb" ))
Coindictionary=pd.read_csv('C:/ML/Dictionary.csv',index_col=None, header=0)




Base=Coinsprices.iloc[:,[1,2,9]]
Base=pd.merge(Base, Coindictionary,left_on='Name',right_on='Abbrevation')
Base=Base.iloc[:,[0,1,2,5]]

###Changing dates to proper ones
for iterator in range(len(Base)):
    Base['Date of ICO'][iterator] = Base['Date of ICO'][iterator][:-10]
    
Base['Date of ICO']= pd.to_datetime(pd.Series(Base['Date of ICO']), format='%Y-%m-%d') 

print("Dates changed!")   
#

####Changing dates to proper ones
Temp=[]
for iterator in range(len(Articles)):
    Articles['Date of publication'][iterator] = Articles['Date of publication'][iterator][:-19]
    print('iterator 1: ' + iterator)
    if Articles['Sentiment']=='negative':
        Temp.append(-1)
    elif Articles['Sentiment']=='positive':
        Temp.append(-1)
    else:
        Temp.append(0)
    
  
Temps=[]
for iterator2 in range(len(Articles)):
    print('iterator 2: ' + iterator2)
    if Articles['Sentiment'][iterator2]=='negative':
        Temps.append(-1)
    elif Articles['Sentiment'][iterator2]=='positive':
        Temps.append(1)
    else:
        Temps.append(0)
        
Articles= Articles.drop('Sentiment',1)        
Articles = Articles.assign(Sentiment=pd.DataFrame(Temps).values)

Articles['Date of publication']= pd.to_datetime(pd.Series(Articles['Date of publication']), format='%Y-%m-%d')    

#
### potential other method Reddit['Date of comment'] = Reddit.Sample.str[:1]
#
#

lista=[]
for iterator3 in range(len(Reddit)):
    lista.append(datetime.fromtimestamp(int(Reddit['Date of comment'].iloc[iterator3])).isoformat())
    print(str(iterator3) + 'Data: ' + str(datetime.fromtimestamp(int(Reddit['Date of comment'].iloc[iterator3])).isoformat()))




Reddit = Reddit.assign(Comment_Date=pd.DataFrame(lista).values)
Reddit= Reddit.drop('Date of comment',1)
Reddit['ICO_Date']=pd.to_datetime(pd.Series(Reddit['ICO_Date']), format='%Y-%m-%d')
#
#


#Changing sentiment for Articles


##Saving
Base.to_csv('C:/ML/Base.csv')
Articles.to_csv("C:/ML/Articles2.csv")
p = pickle.Pickler(open("C:/ML/Reeditsum2.p", "wb"))
p.fast = True 
p.dump(Reddit)


###Change ICO Date
RSentiment1W=[]
RSentiment1M=[]
RSentiment1Y=[]

ROccurances1W=[]
ROccurances1M=[]
ROccurances1Y=[]   
  
RSalience1W=[]
RSalience1M=[]
RSalience1Y=[]
   
RFullSentiment1W=[]
RFullSentiment1M=[]
RFullSentiment1Y=[]   

RFullSentiment1W=[]
RFullSentiment1M=[]
RFullSentiment1Y=[]

ASentiment1W=[]
ASentiment1M=[]
ASentiment1Y=[]



def transpositionRed (listname1, listname2, listname3, column, method):
    if  math.isnan(pd.to_numeric(cointempr1w[column]).method()):
        listname1.append(0)
        print('Pozytywne')
    else:
        listname1.append(pd.to_numeric(cointempr1w[column]).method())
        print('Negatywne')
        
    if  math.isnan(pd.to_numeric(cointempr1m[column]).method()):
        listname2.append(0)
    else:
        listname2.append(pd.to_numeric(cointempr1m[column]).method())
    
    if  math.isnan(pd.to_numeric(cointempr1y[column]).mean()):
        listname3.append(0)
    else:
        listname3.append(pd.to_numeric(cointempr1y[column]).method())
        
    return listname1,listname2,listname3
    
    
for coin in range(len(Base['Coin_Name'])):
    
    
   
#    coin=0
    cointempa=Articles[Articles['Name of crypto']==Base['Coin_Name'][coin]]
    cointempr=Reddit[Reddit['Name']==Base['Coin_Name'][coin].lower()]
    
    cointempr1w=cointempr[(cointempr['ICO_Date'] > (Base['Date of ICO'][coin]- dt.timedelta(0, (60*24*60*7)))) & (cointempr['ICO_Date'] < (Base['Date of ICO'][coin]))]
    cointempr1m=cointempr[(cointempr['ICO_Date'] > (Base['Date of ICO'][coin]- dt.timedelta(0, (60*24*60*30)))) & (cointempr['ICO_Date'] < (Base['Date of ICO'][coin]))]
    cointempr1y=cointempr[(cointempr['ICO_Date'] > (Base['Date of ICO'][coin]- dt.timedelta(0, (60*24*60*356)))) & (cointempr['ICO_Date'] < (Base['Date of ICO'][coin]))]
    
    cointempa1w=cointempa[(cointempa['Date of publication'] > (Base['Date of ICO'][coin]- dt.timedelta(0, (60*24*60*7)))) & (cointempa['Date of publication'] < (Base['Date of ICO'][coin]))]
    cointempa1m=cointempa[(cointempa['Date of publication'] > (Base['Date of ICO'][coin]- dt.timedelta(0, (60*24*60*30)))) & (cointempa['Date of publication'] < (Base['Date of ICO'][coin]))]
    cointempa1y=cointempa[(cointempa['Date of publication'] > (Base['Date of ICO'][coin]- dt.timedelta(0, (60*24*60*356)))) & (cointempa['Date of publication'] < (Base['Date of ICO'][coin]))]
    

    if  math.isnan(pd.to_numeric(cointempr1w['Sentiment Word']).mean()):
        RSentiment1W.append(0)
        print('Pozytywne' + str(coin))
    else:
        RSentiment1W.append(pd.to_numeric(cointempr1w['Sentiment Word']).mean())
        print('Negatywne' + str(coin))
        
    if  math.isnan(pd.to_numeric(cointempr1m['Sentiment Word']).mean()):
        RSentiment1M.append(0)
    else:
        RSentiment1M.append(pd.to_numeric(cointempr1m['Sentiment Word']).mean())
    
    if  math.isnan(pd.to_numeric(cointempr1y['Sentiment Word']).mean()):
        RSentiment1Y.append(0)
    else:
        RSentiment1Y.append(pd.to_numeric(cointempr1y['Sentiment Word']).mean())
        
   
#    RSentiment1W, RSentiment1M, RSentiment1Y= transpositionRed (RSentiment1W, RSentiment1M, RSentiment1Y, 'Sentiment Word', mean)
#    ROccurances1W, ROccurances1M, ROccurances1Y= transpositionRed (ROccurances1W, ROccurances1M, ROccurances1Y, 'Occurences', sum)
#    RSalience1W, RSalience1M, RSalience1Y= transpositionRed (RSalience1W, RSalience1M, RSalience1Y, 'Salience Word', mean)
#    RFullSentiment1W, RFullSentiment1M, RFullSentiment1Y= transpositionRed (RFullSentiment1W, RFullSentiment1M, RFullSentiment1Y, 'Sentiment Full', mean)
    
    if  math.isnan(pd.to_numeric(cointempr1w['Occurences']).sum()):
        ROccurances1W.append(0)
    else:
        ROccurances1W.append(pd.to_numeric(cointempr1w['Occurences']).sum())
 
    if  math.isnan(pd.to_numeric(cointempr1m['Occurences']).sum()):
        ROccurances1M.append(0)
    else:
        ROccurances1M.append(pd.to_numeric(cointempr1m['Occurences']).sum())
        
    if  math.isnan(pd.to_numeric(cointempr1y['Occurences']).sum()):
        ROccurances1Y.append(0)
    else:
        ROccurances1Y.append(pd.to_numeric(cointempr1y['Occurences']).sum())    


        
    if  math.isnan(pd.to_numeric(cointempr1w['Salience Word']).mean()):
        RSalience1W.append(0)
    else:
        RSalience1W.append(pd.to_numeric(cointempr1w['Salience Word']).mean()) 

    if  math.isnan(pd.to_numeric(cointempr1m['Salience Word']).mean()):
        RSalience1M.append(0)
    else:
        RSalience1M.append(pd.to_numeric(cointempr1m['Salience Word']).mean())        

    if  math.isnan(pd.to_numeric(cointempr1y['Salience Word']).mean()):
        RSalience1Y.append(0)
    else:
        RSalience1Y.append(pd.to_numeric(cointempr1y['Salience Word']).mean())   
    

    if  math.isnan(pd.to_numeric(cointempr1w['Sentiment Full']).mean()):
        RFullSentiment1W.append(0)
    else:
        RFullSentiment1W.append(pd.to_numeric(cointempr1w['Sentiment Full']).mean()) 

    if  math.isnan(pd.to_numeric(cointempr1m['Sentiment Full']).mean()):
        RFullSentiment1M.append(0)
    else:
        RFullSentiment1M.append(pd.to_numeric(cointempr1m['Sentiment Full']).mean()) 
        
    if  math.isnan(pd.to_numeric(cointempr1y['Sentiment Full']).mean()):
        RFullSentiment1Y.append(0)
    else:
        RFullSentiment1Y.append(pd.to_numeric(cointempr1y['Sentiment Full']).mean())         

    

    if  math.isnan(pd.to_numeric(cointempa1w['Sentiment']).mean()):
        ASentiment1W.append(0)
    else:
        ASentiment1W.append(pd.to_numeric(cointempa1w['Sentiment']).mean())

    if  math.isnan(pd.to_numeric(cointempa1m['Sentiment']).mean()):
        ASentiment1M.append(0)
    else:
        ASentiment1M.append(pd.to_numeric(cointempa1m['Sentiment']).mean())

    if  math.isnan(pd.to_numeric(cointempa1y['Sentiment']).mean()):
        ASentiment1Y.append(0)
    else:
        ASentiment1Y.append(pd.to_numeric(cointempa1y['Sentiment']).mean())         
    
#    Rplace1W=Rplace1W.append(cointempr1w['Place in Reddit']).mean())
#    Rplace1M
#    Rplace1Y    Rdominant user
    
   
    
    
#def assignment (Variable, Base):
#    Base=Base.assign(Variable=pd.DataFrame(Variable).values)
#    return Base

Base=Base.assign(RSentiment1W=pd.DataFrame(RSentiment1W).values)
Base=Base.assign(RSentiment1M=pd.DataFrame(RSentiment1M).values)
Base=Base.assign(RSentiment1Y=pd.DataFrame(RSentiment1Y).values)
Base=Base.assign(ROccurances1W=pd.DataFrame(ROccurances1W).values)
Base=Base.assign(ROccurances1M=pd.DataFrame(ROccurances1M).values)
Base=Base.assign(ROccurances1Y=pd.DataFrame(ROccurances1Y).values)
Base=Base.assign(RSalience1W=pd.DataFrame(RSalience1W).values)
Base=Base.assign(RSalience1M=pd.DataFrame(RSalience1M).values)
Base=Base.assign(RSalience1Y=pd.DataFrame(RSalience1Y).values)
Base=Base.assign(RFullSentiment1W=pd.DataFrame(RFullSentiment1W).values)
Base=Base.assign(RFullSentiment1M=pd.DataFrame(RFullSentiment1M).values)
Base=Base.assign(RFullSentiment1Y=pd.DataFrame(RFullSentiment1Y).values)
Base=Base.assign(ASentiment1W=pd.DataFrame(ASentiment1W).values)
Base=Base.assign(ASentiment1M=pd.DataFrame(ASentiment1M).values)
Base=Base.assign(ASentiment1Y=pd.DataFrame(ASentiment1Y).values)


Base.to_csv('C:/ML/BaseFinal.csv')



   

