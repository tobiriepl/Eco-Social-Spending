# -*- coding: utf-8 -*-
"""
Created on Mon Jan 13 13:01:23 2025

@author: lena kilian
"""

import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

###########################
## Import and clean data ##
###########################

data_full = pd.read_csv('data_SI_F1.csv', index_col = [1, 2, 4])
data_full = data_full.apply(lambda x: x*data_full['pop']).drop(['Unnamed: 0', 'GDP (per capita)', 'pop'], axis=1)

# make dictionary to clean text later
dict_cat = {'Education':'Education & Recreation', 'Social':'Social Protection', 'Admin':'Public Administration', 
            'Order':'Order & Defense', 'Health':'Health & Environment',  'Housing':'Housing', 'Industry':'Industry',  
            'Total':'Total'}

threshs = ['High', 'Upper-middle', 'Lower-middle', 'Low']

for item in data_full.columns:
    if 'government spending' in item:
        data_full = data_full.rename(columns = {item : item.split(' ')[0] + '_WID'})
    if 'government (g) spending' in item:
        data_full = data_full.rename(columns = {item : item.split(' ')[0] + '_GLORIA'})
        
data_full.columns = pd.MultiIndex.from_arrays([[x.split('_')[0] for x in data_full.columns], [x.split('_')[-1] for x in data_full.columns]])
data_full = data_full.stack(level=0).reset_index().rename(columns={'level_3':'spending_cat', 'level_4':'Dataset'})
data_full['spending_cat'] = data_full['spending_cat'].map(dict_cat)

data_full = data_full.groupby(['Country Name', 'Social Thresholds', 'spending_cat']).mean().reset_index()

#################
## Correlation ##
#################

corr = data_full.groupby(['Social Thresholds', 'spending_cat']).corr('spearman')\
        .unstack(level=2)[[('GLORIA', 'WID')]].unstack(level=0).droplevel(axis=1, level=[0, 1])\
            [threshs].loc[list(dict_cat.values())]

corr['Total'] = data_full.groupby(['spending_cat']).corr('spearman')\
        .unstack(level=1)[[('GLORIA', 'WID')]]

corr.to_csv('SI_table_3.csv')
        
##############
## Boxplots ##
##############

sns.set_style({'font.family':'serif', 'font.serif':'Times New Roman'})

for item in list(dict_cat.values()):
    plot_data = data_full.loc[data_full['spending_cat'] == item]\
        .set_index(['Country Name', 'Social Thresholds'])[['WID', 'GLORIA']].stack().reset_index()\
            .rename(columns={'level_2':'Dataset', 0:'Spend'})
    plot_data['Social Thresholds'] = pd.Categorical(plot_data['Social Thresholds'], categories=threshs, ordered=True)

    sns.boxplot(data=plot_data, x='Social Thresholds', y='Spend', hue='Dataset', palette='viridis')
    plt.yscale('log')
    plt.xlabel('')
    plt.ylabel('Government spending [US$]')
    plt.title(item)
    plt.savefig('boxplot_SI_' + item + '.png', bbox_inches='tight', dpi=200)
    plt.show()
