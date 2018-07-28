#-*- coding: utf-8 -*-
from googlefinance.client import get_price_data
import pandas as pd
from datetime import date

stock_list = pd.read_csv('data/krx-list.csv')
stock_code = stock_list.code


# parameter setting
param = {
    'q': "005930", # Stock code (ex: "005930": Samsung Electronics)
    'i': "86400", # Interval size in seconds ("86400" = 1 day intervals)
    'x': "KRX", # Stock exchange symbol on which stock is traded (ex: "NASD")
    'p': "365d" # Period (Ex: "1Y" = 1 year)
}


# check file existence




# assign file path
PATH_FILE = 'data/'+param['q']+'.csv'

# figure out number of days to update
with open(PATH_FILE, 'r') as f:
    lines = f.readlines()
    last_row = lines[-1].split()[0] # the last day of current data set
    last_date = datetime.datetime.strptime(last_row, '%Y-%m-%d').date()

today = datetime.date.today() # today
diff = today - last_date # number of days to update


# loop    

for x in stock_code:
    param['q'] = x # assign stock code
    df = get_price_data(param) # get data via googlefinance
    df.to_csv('data/'+param['q']+'.csv', sep=',') # save as csv

