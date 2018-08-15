### Histogram

import numpy
y=y_test
y=numpy.concatenate((y,y_train,y_val), axis=0)
y1=numpy.zeros(len(y))
for i in range(0,len(y1)):
     s=0
     c=1
     while c<2:
          if y[i,s]==10:
               c=c+1
               y1[i]=y1[i]/10**(5-s)
               print("s", s)
               print(y1[i])
          else:
               if s==5:
                    y1[i]=y[i,s]*10**(4-s)+y1[i]
                    c=c+1
               else:
                    y1[i]=y[i,s]*10**(4-s)+y1[i]
                    s=s+1
                    print(s)
                    print("i",i)
                    print(y1[i])
a=numpy.zeros(len(y1))
y2=y1                    
y3=numpy.column_stack((y2,numpy.zeros(len(y2))))                    
for i in range(0,len(y3)):
     y3[i,1]=len(str(int(y3[i,0])))

import matplotlib.pyplot as plt
np.histogram
plt.title("Histogram with 'auto' bins")
plt.show()


