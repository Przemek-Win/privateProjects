import pandas as pd
import os
import pickle
import re
import logging
import datetime
import numpy as np 
import threading
import time

global iterator

exitFlag = 0

class myThread (threading.Thread):
   def __init__(self, root, datafile, named):
      threading.Thread.__init__(self)
      self.datafile = datafile
      self.root = root
      self.named = named
   def run(self):
      print ("Starting " + self.name)
      repair(self.root, self.datafile, self.named)
      print ("Exiting " + self.name())


logging.basicConfig(filename='C:/MergingNLP2.log', level=logging.INFO)

#datafile= 'Number8from1963412017_11bisdata.p'
#datafile[-16:]
####Repair of the file function   
def repair (root, datafile, named):
    newresultsred=pickle.load(open( 'C:/Users/przemek/Documents/Pickles raw reddits/' + datafile[-16:].replace('data',''), "rb" ) )
    
    coindict= pd.read_csv('C:/Users/przemek/Downloads/Dictionary.csv',index_col=None, header=0)


    #Putting all names in dict in lower case
    for i in range(len(coindict.iloc[:,2])):
        coindict.iloc[i,2]=coindict.iloc[i,2].lower()
    print("Coins loaded")
    dataset_sent=pd.DataFrame()
    repairfile=pickle.load(open( root + datafile.replace('data','fullentity'), "rb" ) )
    repairfilefull=pickle.load(open( root + datafile.replace('data','full'), "rb" ) )
    print('Pickle files opened...')
    start=0
    end = len(newresultsred)-1
    for numb in range(start ,end): #Change this to length of the data frame
    
        try:
            print('Starting retrieving entity number...' + str(numb) + 'from: ' + datafile + 'Iterator ' + named)
            responseES=repairfile[numb]
            response=repairfilefull[numb]
            found=0
            for x in range(len(responseES.entities)):
                    
                if found == 1:
                    break
                else:

                    for dic in range(len(coindict.iloc[:,2])):
                        if re.search(r"\b" + re.escape(coindict.iloc[dic,2]) + r"\b", responseES.entities[x].name.lower()):
        #                        print(dic, responseES.entities[x].name , coindict2.iloc[dic,2])
                            entit=x
                            name=coindict.iloc[dic,2]
                            found=1 # That is a flag for the first crypto found
                            break
                        elif re.search(r"\b" + re.escape(coindict.iloc[dic,1]) + r"\b", responseES.entities[x].name):  # I don't put lower case here, because some abbrevations of cryptos are too comon such as UNIT  
        #                        print(dic, responseES.entities[x].name , coindict2.iloc[dic,1])
                            entit=x
                            name=coindict.iloc[dic,2]
                            found=1 # That is a flag for the first crypto found
                            break
                        else:
                            entit=999
                            continue
    
        #        print(entit)
            if entit == 999 or len(responseES.entities)==0 : # Skipping the reedits that don't have any crypto recognized by Google NLP
        #           print('Lack of altcoins')
               continue
            else:
            # Extracting important data from the responses
                full_sent= response.document_sentiment.score
                magn=response.document_sentiment.magnitude
                salience= responseES.entities[entit].salience
                occur=len(responseES.entities[entit].mentions)
                word_sent=responseES.entities[entit].sentiment.score
                word_magn=responseES.entities[entit].sentiment.magnitude
                
    
                #Searching for place mentioned in Reddit based on category 2 that represents place
                for num in range(len(responseES.entities)):
                    if responseES.entities[num].type == 2:
                        place=responseES.entities[num].name
                        break
                    else:
                        place = 0
                        
                        
                columns=[[name],[full_sent],[magn],[salience],[occur],[word_sent],[word_magn],[place],[newresultsred['Article ID'].iloc[numb]],[newresultsred['Date of creation'].iloc[numb]]]      
                column_names=['Name','Sentiment Full','Magnitude Full','Salience Word','Occurences','Sentiment Word','Magnitude Word','Place in Reddit','Article ID','Date of comment']
                
                #Initializing new row into database and merging it with a whole set
                initialization=pd.DataFrame(np.array(columns).T,columns=column_names)
                dataset_sent=dataset_sent.append(initialization)
                
            if numb%10==0 and numb!=0 :  
                p = pickle.Pickler(open("C:/"  + 'repaired from' + str(start)+ f.replace('.p','data.p'), "wb"))
                p.fast = True 
                p.dump(dataset_sent)
 
    
    
        except Exception as e:
    
            print('Error ocured in last observation' + str(e))
            logging.info('Error ocured in last observation' + str(e) + str(datetime.datetime.now().time()))
            continue    
    
    
    
    
    
    

    
    
    
folder='C:/machine_learning_przemek/Google NLP output/Table/'
finalset=pd.DataFrame()
iterator=0
for root, dirs, filenames in os.walk(folder):
    for f in filenames:
        try:
            if 'data' in str(f) :
                print(f)
                temp= pickle.load(open(root + f , "rb" ))
                finalset=finalset.append(temp)
            
           
        except Exception as e:
            if str(e) == 'Ran out of input':
#                repair (root, f)
                print(e)
#                if iterator == 0:
#                    Thread1 = myThread(root, f, "Thread-1")
#                    Thread1.start()
#                    iterator=+1
#                    continue
#                elif iterator == 1:
#                    Thread2 = myThread(root, f, "Thread-2")
#                    Thread2.start()
#                    iterator=+1
#                    continue
#                elif iterator == 2:
#                    Thread3 = myThread(root, f, "Thread-3")
#                    Thread3.start()
#                    iterator=+1
#                    continue
#                elif iterator == 3:
#                    Thread4 = myThread(root, f, "Thread-4")
#                    Thread4.start()
#                    iterator=+1
#                    continue
#                elif iterator == 4:
#                    Thread5 = myThread(root, f, "Thread-5")
#                    Thread5.start()
#                    iterator=+1 
#                    continue
#                elif iterator == 5:
#                    Thread6 = myThread(root, f, "Thread-6")
#                    Thread6.start()
#                    iterator=+1
#                    continue
#                elif iterator == 6:
#                    Thread7 = myThread(root, f, "Thread-7")
#                    Thread7.start()
#                    iterator=+1
#                    continue
#                elif iterator == 7:
#                    Thread8 = myThread(root, f, "Thread-8")
#                    Thread8.start()
#                    iterator=+1
#                    continue
#                elif iterator == 8:
#                    Thread9 = myThread(root, f, "Thread-9")
#                    Thread9.start()
#                    iterator=+1
#                    continue
#                elif iterator == 9:
#                    Thread10 = myThread(root, f, "Thread-10")
#                    Thread10.start()
#                    iterator=+1
#                    continue
#                elif iterator == 10:
#                    Thread11 = myThread(root, f, "Thread-11")
#                    Thread11.start()
#                    iterator=+1  
#                    continue
#                else :
#                    Thread12 = myThread(root, f, "Thread-12")
#                    Thread12.start()
#                    iterator=+1  
#                    continue
            else:
                print("Other error occured in form of : " + str(e))
                logging.info('Other error occured in form of : ' + str(e) + 'at time: ' + str(datetime.datetime.now().time()) )
                continue
        
p = pickle.Pickler(open("C:/ML/Reeditsum.p", "wb"))
p.fast = True 
p.dump(finalset)