import pandas as pd

df = pd.read_html('http://kind.krx.co.kr/corpgeneral/corpList.do?method=download&searchType=13', header=0)[0]

df = df[['회사명', '종목코드', '상장일', '업종']]
df = df.rename(columns={'회사명': 'name', '종목코드': 'code', '상장일': 'list_date', '업종': 'industry'})

df = df.drop([300])
# df.code = df.code.apply(lambda x: x.zfill(6))

df.code = df.code.map('{:06d}'.format)

df.to_csv('data/krx-list.csv', sep=',')
