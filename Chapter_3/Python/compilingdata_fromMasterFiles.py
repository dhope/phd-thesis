#!/usr/bin/python3
'''This function should take data from Master Files:either that directly downloaded from GoogleDrive responses, that 
which volunteers emails in datasheets and I transferred to a master file or that which I entered manually into a 
spreadsheet.'''
import numpy as np
import pandas as pd
import re
#import csvimport numpy as np
import pandas as pd
import re
import csv
SSDB = '/home/dhope/Documents/SFU/PhD/SharingFiles_ForWindows/ShorebirdSurveyDatabase/'
inputfolder = '/home/dhope/Documents/SFU/PhD/SharingFiles_ForWindows/ShorebirdSurveyDatabase/Python/input/'
outputfolder = '/home/dhope/Documents/SFU/PhD/SharingFiles_ForWindows/ShorebirdSurveyDatabase/Python/output/'
masterfiles = '/home/dhope/Documents/SFU/PhD/SharingFiles_ForWindows/ShorebirdSurveyDatabase/MasterFiles/'
from combineCountsFun import combineCounts
import datacleaning_2017_from_Master as dc17
import datacleaning_2016_from_Master as dc16
import datacleaning_2015_from_Master as dc15
import datacleaning_2014_from_Master as dc14
import datacleaning_2013_from_Master as dc13



wesa_counts = 			pd.concat([dc13.Counts_all_2013, dc14.WESA_all_2014, dc15.WESA_all_2015, dc16.WESA_all_2016, dc17.WESA_all_2017], ignore_index = True).copy()
falcon_obs = 			pd.concat([dc13.falcon_all_2013, dc14.falcon_all_2014, dc15.falcon_all_2015, dc16.falcon_all_2016, dc17.falcon_all_2017], ignore_index = True).copy()
site_information = 		pd.concat([dc13.all_site_info_2013, dc14.all_site_info_2014, dc15.all_site_info_2015, dc16.all_site_info_2016, dc17.all_site_info_2017], ignore_index = True).copy()
observer_info = 		pd.concat([dc13.all_obs_info_2013, dc14.all_obs_info_2014, dc15.all_obs_info_2015, dc16.all_obs_info_2016, dc17.all_obs_info_2017], ignore_index = True).copy()



print(observer_info.columns.values)
print(wesa_counts.columns.values)

wesa_counts.to_csv(outputfolder + "wesa_counts_13_14_15_16_17.txt", sep = '\t')
falcon_obs.to_csv(outputfolder + "falcon_counts_13_14_15_16_17.txt", sep = '\t')
site_information.to_csv(outputfolder + "siteinfo_13_14_15_16_17.txt", sep = '\t')
observer_info.to_csv(outputfolder + "obsinfo_13_14_15_16_17.txt", sep = '\t')


''' Now take this data and move into R for final clean and analysis '''