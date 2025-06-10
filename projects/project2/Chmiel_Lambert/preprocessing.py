import os, pandas as pd

dfs = []


for filename in os.listdir('DANE'):
    file_path = os.path.join('DANE', filename)
    tmp = pd.read_excel(file_path, engine='openpyxl')
    dfs.append(tmp)

df = pd.concat(dfs, ignore_index=True)
df = df.drop(columns=['Other_Race', 
                      'Other_Diagnosis', 
                      'Diagnosis_2', 
                      'Stratum', 
                      'Weight', 
                      'PSU', 
                      'Product_3', 
                      'Hispanic', 
                      'Product_2', 
                      'Body_Part_2', 
                      'Other_Diagnosis_2', 
                      'Fire_Involvement'])

df['Sex'] = df['Sex'].replace({0: 'UNKNOWN', 1: 'MALE', 2: 'FEMALE'})

df['Race'] = df['Race'].replace({
    0: 'N.S.',
    1: 'WHITE',
    2: 'BLACK/AFRICAN AMERICAN',
    3: 'OTHER',
    4: 'ASIAN',
    5: 'AMERICAN INDIAN/ALASKA NATIVE',
    6: 'NATIVE HAWAIIAN/PACIFIC ISLANDER'
})

body_part_dict = pd.Series(pd.read_csv('preprocessing/body_part.csv').Body_Part.values, index=pd.read_csv('preprocessing/body_part.csv').Code).to_dict()
df['Body_Part'] = df['Body_Part'].replace(body_part_dict)

prod_dict = pd.Series(pd.read_csv('preprocessing/product.csv',sep=';').Product_1.values, index=pd.read_csv('preprocessing/product.csv',sep=';').Code).to_dict()
df['Product_1'] = df['Product_1'].replace(prod_dict)

diag_dict = pd.Series(pd.read_csv('preprocessing/diag.csv',sep=';').Diagnosis.values, index=pd.read_csv('preprocessing/diag.csv',sep=';').Code).to_dict()
df['Diagnosis'] = df['Diagnosis'].replace(diag_dict)

diag_dict = pd.Series(pd.read_csv('preprocessing/disp.csv',sep=';').Disposition.values, index=pd.read_csv('preprocessing/disp.csv',sep=';').Code).to_dict()
df['Disposition'] = df['Disposition'].replace(diag_dict)

loc_dict = pd.Series(pd.read_csv('preprocessing/loc.csv',sep=';').Location.values, index=pd.read_csv('preprocessing/loc.csv',sep=';').Code).to_dict()
df['Location'] = df['Location'].replace(diag_dict)

sports = ['SOCCER', 
          'WRESTLING', 
          'SNOW SKIING', 
          'BICYCLES', 
          'VOLLEYBALL', 
          'BASKETBALL', 
          'SWIMMING', 
          'GOLF', 
          'SCOOTERS', 
          'DANCING', 
          'ICE SKATING', 
          'SNOWBOARDING', 
          'TENNIS', 
          'MARTIAL ARTS', 
          'BASEBALL', 
          'WEIGHT LIFTING']

def extract_sport(prod):
    if pd.notna(prod):
        for s in sports:
            if s in prod:
                return s
    return 'NaN'

df['Sport'] = df['Product_1'].apply(extract_sport)

df.to_excel('dane.xlsx', index=False)