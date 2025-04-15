# -*- coding: utf-8 -*-
"""
Created on Mon Jan 13 13:01:23 2025

@author: lena kilian
"""

import pandas as pd

data_full = pd.read_excel('data_supplementary.xlsx', sheet_name='Figure 3 Main Data')
data_full['Social Thresholds'] = [x.split(' ')[0] for x in data_full['thresh_level']]

dict_cats = {'health_envi':'Health & Environment', 'soc':'Social Protection', 'edu_recr':'Education & Recreation', 
             'hous':'Housing', 'indu':'Industry',  'ord_def':'Order & Defense', 'admin':'Public Administration'}

data = data_full.set_index(['Social Thresholds', 'name', 'year'])
keep = []
for x in list(dict_cats.keys()):
    keep.append('Percentage of Total Spend (%)-' + x)
    keep.append('Carbon Intensity (kgCO2/$)-' + x)
    data['Percentage of Total Spend (%)-' + x] = data[x + '_gov_spending_tot_wid'] * 100
    data['Carbon Intensity (kgCO2/$)-' + x] = data['adj_' + x + '_intensity_gov_g']
data = data[keep]

data.columns = pd.MultiIndex.from_arrays([[x.split('-')[0] for x in data.columns.tolist()], [x.split('-')[1] for x in data.columns.tolist()]])
data = data.swaplevel(axis=1)

plot_data = data.stack(level=0).reset_index()
plot_data.columns = ['Social Thresholds', 'name', 'year', 'spending_cat', 'Carbon_Intensity', 'spend_pct']

plot_data['spending_cat'] = plot_data['spending_cat'].map(dict_cats)                  
plot_data.to_csv('plot_data.csv')