# PART1_Main_Data
# krok 5:
# Tworzymy finalny dataset. Wywołujemy funkcję z kroku 4, a następnie tworzymy plik csv gotowy do obróbki w Rstudio.
# Każda z kolejnych części kodu jest analogiczna do części pierwszej, dlatego resztę pozostawimy bez opisów.
import pandas as pd
from dataf import main

index_ = ['Title','Year','Duration[min]','Original_Title','Score_users',
          'Number_of_ratings_users','Score_critics','Number_of_ratings_critics',
          'Director','Country_production','worldwide_premiere','Poland_premiere',
          'Boxoffice','Budget','Distribution']
df_filmweb = pd.DataFrame(columns=index_)
def create_dataset(df):
    main(df)    
if __name__ == '__main__':
    create_dataset(df_filmweb)
    df_filmweb.to_csv('filmweb.csv', index=False, na_rep='N/A')