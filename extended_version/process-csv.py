#! /bin/python3

import os
import json
import zipfile
from datetime import date,datetime
import string
import re
import subprocess

import numpy as np
import scipy as sci
from scipy import stats
import pandas as pd
import matplotlib
import matplotlib.pyplot as plt
import altair as alt
alt.data_transformers.enable('default', max_rows=None)
alt.renderers.enable('default')

from textwrap import wrap
from functools import partial 

np.set_printoptions(precision=3)
today = date.today()

timestamp = lambda: datetime.now().strftime('%Y-%m-%dT%H-%M-%S')
import subprocess

subprocess.run('mkdir output', shell=True)


#### CONFIG ####
file = "./results-100.csv"
file = "./results-100-20200122.csv"
file = "./results-100-20200123.csv"

just_compute_example = False
#just_compute_example = True

savegraphicstofile = False
savegraphicstofile = True

tabletofile = False
tabletofile = True

##################

df = pd.read_csv(file, sep=";")

newnames = {'Olympic_Disciplines_Summer_2020': 'Olympic-Disciplines',
            'ben_and_jerry_ice_cream': 'Ice-Cream',
            'brunson-club-membership': 'Brunson-Club',
            'cointoss-random-1': 'Cointoss-1',
            'cointoss-random-2': 'Cointoss-2',
            'cointoss-random-3': 'Cointoss-3',
            'dirchlet-random-1': 'Dirichlet-1',
            'dirchlet-random-2': 'Dirichlet-2',
            'futtertabelle': 'Bird-Diet',
            'gewuerzplaner': 'Seasoning-Planner',
            'holzarten': 'Types-of-Wood',
            'living-beings-and-water': 'Living-Beings-and-Water',
            'sds6-Diagnosis': 'Diagnosis',
            'southern_woman': 'Southern-Woman'}

df['group-id'] = df['group-id'].replace('\.ctx|\.cxt', '', regex=True)\
                               .apply(lambda s: newnames[s] if s in newnames else s)

### I-PI PLOTS and BOXPLOTS for num-concepts/num-pseudo-intents/density/entropy/shannon-entropy ###

myorder = ['cointoss','dirichlet','original','resampling']
# purplebluegreen-3 with red
mypalette = ['#a2bddb','#549fc8','#bf130d','#138495']
#['teal','lightblue','red','steelblue']
myshapes = ['circle','square','diamond','cross']
myorderpalette = dict(zip(myorder,mypalette))


def doplot(df):
    subplots = []
    dfgs = df.groupby('group-id')
    for name,dfi in dfgs: #[list(dfgs)[0]]: #dfgs:          
        ##### PLOT 1 ######
        height = 180
        w1 = 400
        w2 = 80
        source = dfi
        plot1 = alt.Chart(
                source, 
                height=height,
                width=w1
            ).mark_point(size=20, filled=True).encode(
                x=alt.X('num-concepts',title='#intents'),
                y=alt.Y('num-pseudo-intents',title='#pseudo-intents', axis=alt.Axis(minExtent=50)),
                color=alt.Color('generator:N', scale = alt.Scale(range=mypalette)),
                shape=alt.Shape('generator:N', scale = alt.Scale(range=myshapes)),
            )

        originalpoint = alt.Chart(source[source.generator == 'original'])\
        .mark_point(size=100,shape="diamond", color='red', filled=True, opacity=1)\
        .encode(
            x='num-concepts',
            y='num-pseudo-intents',
        )
        plot1 += originalpoint
        
        
        # does not work on elements of combined charts, only on the biggest chart ... -.-
        #plot1 = plot1.configure_legend(
        #    strokeColor='gray',
        #    fillColor='#EEEEEE',
        #    padding=10,
        #    cornerRadius=10,
        #    orient='top-right'
        #)

        
        ##### PLOT 2 ######
        plot2 = alt.hconcat()
        for row in ['density',
                    'num-concepts',
                    'num-pseudo-intents',
             #'entropy',
             #'shannon-entropy',
             #'ratio-distinct-objects'
                   ]:

            if row in ['density', 'entropy', 'shannon-entropy','ratio-distinct-objects']:
                Y=alt.Y(row,scale={'domain':[0,1]})
            elif row == 'num-concepts':
                Y=alt.Y(row,title='#intents', axis=alt.Axis(minExtent=50))
            elif row == 'num-pseudo-intents':
                Y=alt.Y(row,title='#pseudo-intents', axis=alt.Axis(minExtent=50))
            else:
                Y=row
            plot2 |= alt.Chart(
                source,
                height=height,  
                width=w2
            ).mark_boxplot(
                median={"color": "red"},#{"color": "black"},
                outliers=alt.MarkConfig(shape='stroke',color='gray'),
                #box={"filled":False}
            ).encode(
                y=Y,
                x=alt.X('generator:N', axis=alt.Axis(labelAngle=-45)),
                color=alt.Color('generator:N', scale = alt.Scale(range=mypalette),legend=None),
            )
            alt.MarkConfig(shape='stroke')
        ##### PLOT 3 ######
        column='row-sum-distribution'
        orgdf = dfi[dfi.generator=='original']
        orgy = list(orgdf[column])[0]
        orgrowdensity = [dict(sorted((int(k),v) for k,v in json.loads(orgy).items()))]
        orgdatadf =pd.DataFrame(orgrowdensity)
        orgsource = pd.melt(orgdatadf)

        info = name + "\n"
        info += "Objects: " + str(list(orgdf['num-objects'])[0]) + "\n"
        info += "Attributes: " + str(list(orgdf['num-attributes'])[0]) + "\n"

        charts = []
        for n,dfj in dfi.groupby('generator'):
            ys =dfj[column]
            data=[]
            for y in ys:  
                data.append(dict(sorted((int(k),v) for k,v in json.loads(y).items())))       
            datadf =pd.DataFrame(data)
            source = pd.melt(datadf)

            
            error_bars = alt.Chart(source,
                                   title=alt.TitleParams(text=n,anchor='middle',fontSize=16),
                                  ).mark_errorbar(extent='stdev').encode(
              y=alt.Y('value', title='frequency',scale={'domain':[0,1]}),
              x=alt.X('variable' ,title='#attributes')
            )

            points = alt.Chart(source).mark_point(size=15,
                                                  filled=True, 
                                                  color='black',
                                                  #color=myorderpalette[n],#'black'
                                                 ).encode(
              y=alt.Y('value', aggregate='mean'),
              x=alt.X('variable'),
            )
            
            orgline2 = alt.Chart(orgsource).mark_line(color='red',strokeWidth=0.75).encode(
                x='variable',
                y='mean(value)',
            )

           
            chart2 = alt.LayerChart(
                layer = [error_bars,orgline2,points],
                width = 176,
                height= 90
            )

            charts.append(chart2)          
            

        plot3 = alt.HConcatChart(
            hconcat=charts,
        )
        
        ##### COMBINE PLOTS ######
        toprow = alt.ConcatChart(
            concat = [plot2, plot1],
        )
        toprow = toprow.resolve_scale(
            color='independent',
            shape='independent'
        )
        
        chart = alt.ConcatChart(
            concat = [toprow,plot3],
            #align = 'each',
            #bounds = 'full',
            #autosize =alt.AutoSizeParams(
            #    contains='padding',
            #    #resize = True,
            #    #type = 'none'
            #),
            columns = 1,
            #center =True,
        )
        
        chart.title = name
        chart = chart.configure_title(
            fontSize=20,
            #font='Courier',
            anchor='start',
            #color='gray'
        )
        chart = chart.configure_legend(
            strokeColor='gray',
            #fillColor='#EEEEEE',
            labelFontSize = 14,
            titleFontSize = 15,
            columns = 4,
            offset = 0,
            legendX = 35,
            legendY = 240,
            padding=8,
            cornerRadius=10,
            orient='none'
        )
        chart = chart.configure_axis(
            labelFontSize = 14,
            titleFontSize = 15,
        )
       
        
        subplots.append((info,chart,name))
        #subplots.append((info, tempplot))
        #subplots.append(('', plot3))

    #for s in subplots:
    #    print(s[0])
    #    s[1].display()
    return subplots



if savegraphicstofile:
    ret = []
    if just_compute_example:
        ret = doplot(df[df['group-id']=='Olympic-Disciplines'])
        ret = doplot(df[df['group-id']=='Forum-Romanum'])
    else:
        ret = doplot(df)
    for r in ret:
        r[1].save('./output/'+ str(r[2])+'.svg', webdriver='firefox')




process = subprocess.run('./all-svg-to-pdf',shell=True)



pd.options.display.float_format = '{:,.2f}'.format


names = {'id':r'context',
             'gen':r'generator',
             'a':r'\#attributes',
             'o':r'\#objects',
             'md':r'($\mu$)-density',
             'sd':r'$\sigma$-density',
             'mi':r'($\mu$)-\#intents',
             'si':r'$\sigma$-\#intents',
             'mpi':r'($\mu$)-\#pseudo-intents',
             'spi':r'$\sigma$-\#pseudo-intents'}
for k,v in names.items():
    names[k] = r'{\bfseries '+ v +'}'
    
fulldfcolumns = names.values()
#print(names.values())

def createtable(df):
    df=df[['group-id','generator','num-objects','num-attributes','num-concepts','num-pseudo-intents','density']]
    dfgroupids = df.groupby('group-id')
    fulldf = pd.DataFrame()    
    for name,dfi in dfgroupids:
        dfgens = dfi.groupby('generator')
        subdf = pd.DataFrame(columns=fulldfcolumns)
        for genname,dfj in dfgens:
            gendf = pd.DataFrame({names['id']:[name],names['gen']:[genname]})
            if not genname == 'original':
                gendf[names['md']] = dfj['density'].mean()
                gendf[names['sd']] = dfj['density'].std()
                gendf[names['mi']] = dfj['num-concepts'].mean()
                gendf[names['si']] = dfj['num-concepts'].std()
                gendf[names['mpi']] = dfj['num-pseudo-intents'].mean()
                gendf[names['spi']] = dfj['num-pseudo-intents'].std()
                subdf = pd.concat([subdf,gendf],sort=False)
            else:
                gendf[names['o']] = dfj['num-objects'].iloc[0]
                gendf[names['a']] = dfj['num-attributes'].iloc[0]
                gendf[names['md']] = dfj['density'].iloc[0]
                gendf[names['mi']] = dfj['num-concepts'].iloc[0]
                gendf[names['mpi']] = dfj['num-pseudo-intents'].iloc[0]
                subdf = pd.concat([gendf,subdf],sort=False)
                
        fulldf = pd.concat([fulldf,subdf])
        
    fulldf = fulldf.reset_index(drop=True)
    fulldf = fulldf.reindex(fulldfcolumns, axis=1)
    fulldf = fulldf.sort_values(by=[names['id'],names['o'],names['gen']]).fillna('')
    fulldf[names['id']] = fulldf[names['id']].replace('\.ctx|\.cxt', '', regex=True)
    
    def rename_gen(s):
        if s=='original':
            return 'True Context'#'Original'
        if s=='cointoss':
            return 'Cointoss'
        if s=='resampling':
            return 'Resample'
        if s=='dirichlet':
            return 'Dirichlet'
        return 'X'
        
    # used to sort the rows into  real - artifical
    def is_artificial(s):
        if "Cointoss" in s or "Dirichlet" in s:
            return 1
        else: return 0
    
    fulldf[names['gen']] = fulldf[names['gen']].apply(rename_gen)
    fulldf[names['mpi']] = fulldf[names['mpi']].round().astype('int32')
    fulldf[names['mi']] = fulldf[names['mi']].round().astype('int32')
    fulldf['artificial'] = fulldf[names['id']].apply(is_artificial)
    fulldf = fulldf.set_index([names['id'], names['gen']])
    fulldf = fulldf.sort_values(by=['artificial',names['id'],names['a'],names['gen']]) 
    fulldf = fulldf.drop(columns=['artificial'])
    #fulldf = fulldf.style.format({'mean-num-pseudo-intents':'{:,.1f}','mean-num-intents':'{:,.1f}'})
    fulldf = fulldf.rename(index={'Olympic_Disciplines_Summer_2020': 'Olympic-Disciplines'})\
                .rename(index={'ben_and_jerry_ice_cream': 'Ice-Cream'})\
                .rename(index={'brunson-club-membership': 'Brunson-Club'})\
                .rename(index={'cointoss-random-1': 'Cointoss-1'})\
                .rename(index={'cointoss-random-2': 'Cointoss-2'})\
                .rename(index={'cointoss-random-3': 'Cointoss-3'})\
                .rename(index={'dirchlet-random-1': 'Dirichlet-1'})\
                .rename(index={'dirchlet-random-2': 'Dirichlet-2'})\
                .rename(index={'futtertabelle': 'Bird-Diet'})\
                .rename(index={'gewuerzplaner': 'Seasoning-Planner'})\
                .rename(index={'holzarten': 'Types-of-Wood'})\
                .rename(index={'living-beings-and-water': 'Living-Beings-and-Water'})\
                .rename(index={'sds6-Diagnosis': 'Diagnosis'})\
                .rename(index={'southern_woman': 'Southern-Woman'})
    return fulldf



df = pd.read_csv(file, sep=";")
if tabletofile:
    with open('output/combined-table', 'w') as f:
        dfnew = createtable(df)
        dfnew.columns = [r'\rot{'+ c+ '}' for c in list(dfnew.columns)]
        s = dfnew.to_latex(index=True,
                           longtable=True,
                           multirow=True,
                           escape=False,
                           #bold_rows=True,
                           column_format='llrrrrrrrr')
        f.write(s)

