import numpy as np
import scipy as sci
import scipy.stats as stats
import pandas as pd

def dirichlet_approach(M=10, b='random'):
    # dirichlet(beta*alpha), beta concentration parameter; alpha base measure
    G = np.random.randint(M,pow(2,M))
    alpha=[1]*(M+1)
    alpha = list(np.array(alpha)/sum(np.array(alpha))) # normalize initial vector
    if b == 'random':
        # beta >0.001 prevents problems with dirichlet
        beta = 0.001+np.random.random()*(M+1) 
    elif b == 0.1:
        beta = 0.1*(M+1)
    else:
        beta = 1*(M+1)
    #draw probabilities for categorical distribution
    rv = sci.stats.dirichlet(beta*np.array(alpha)) 
    p = rv.rvs()[0]
    categories = np.arange(0,len(alpha))
    # create categorical distribution
    categorical = sci.stats.rv_discrete(name='categorical', values=(categories, p)) 
    # create random context
    context = []
    for i in range(G):    
        # randomly decide how many attributes the object has
        number_of_attributes = categorical.rvs()
        row = [1] * number_of_attributes + [0]*(M - number_of_attributes)
        # randomly decide which attributes specifically
        np.random.shuffle(row)
        context.append(row)
    return context


def context_to_file(context,name='test'):
    df = pd.DataFrame(context)
    df.to_csv(path_or_buf=name+".csv",header=False,index=False)
    return

if __name__ == '__main__':
    # generates one random context using the dirichlet approach and save it as csv
    context_to_file(dirichlet_approach())
