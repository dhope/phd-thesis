#!/usr/bin/python3
''' Function to take columns from WESA counts and Falcon Obs
entered online and covert them into a dataframe with counts
as rows instead of columns '''
import numpy as np
import pandas as pd
import re
#import csvimport numpy as np
import pandas as pd
import re
import csv


def combineCounts(db_in, species):
    #### Counts

    col_names = list(db_in.columns.values)
    #print(col_names)

    for j in range(1,20):
        #print(j)
        count_col =  list(["RecordID","SiteID"])

        for i in col_names:
            if species == "WESA":
                if re.search("Count_{}((_)|($))".format(j) , i):
                    count_col.append(i)
                    found_it = True
            if species == "Falcon":
                if re.search("Falcon_{}((_)|($))".format(j) , i):
                    #print(i)
                    count_col.append(i)
                    found_it = True
        if found_it:
            count_col.append('Month')                            

            counts = db_in.ix[:,count_col].copy()
            if species == 'WESA':
                counts.rename(columns=lambda x: re.sub('Count_\d*((_)|($))','',x), inplace=True)
                counts.rename(columns={"": "Count"}, inplace=True)
            if species == 'Falcon':
                    
                    counts.rename(columns=lambda x: re.sub('Falcon_\d*((_)|($))','',x), inplace=True)
                    counts.rename(columns={"": "Num_Falcons"}, inplace=True)
                    
            counts['CountNum'] = str(j)
            #print(counts.columns.values)
            if j == 1: Counts_All = counts
            else: Counts_All = pd.concat([Counts_All, counts],ignore_index=True).copy()
       # print(len(Counts_All))
        found_it = False
    if species == 'WESA':
        Counts_All['Group'] = 'Wesa'
        Counts_All['ID'] = Counts_All['RecordID'] + '_W' + Counts_All['CountNum']
    if species == 'Falcon':
        Counts_All['Group'] = 'Falcon'
        Counts_All['ID'] = Counts_All['RecordID'] + '_F' + Counts_All['CountNum']
    Counts_All['Source'] = 'Online'
        
    return Counts_All