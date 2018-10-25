#!/usr/bin/python3
'''
This script should provide a record of all 
edits made to the original data that was either
entered by the observer online or by David Hope manually
This script is based on ImportingFromMasterFiles.ipnb. 
See that file for more details.
'''

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


#############################
#------------------------- 2013 ------------------------------------
##
##############################

# Data entered online and file downloaded directly from google drive responses
downloadeddb = pd.read_excel(masterfiles + "DownloadedFromDrive/Summer Shorebird Pulse Data Form (Responses).xlsx",
                             sheetname = None, na_values=['NA'] )

onlineDownloaded = downloadeddb["Form Responses"].copy()
siteNames = onlineDownloaded['Survey Site'].unique() # Assign sites to download and enter siteIDs
dates = onlineDownloaded['Date'].unique()

##### Rows without dates###########
onlineDownloaded[pd.isnull(onlineDownloaded["Date"])]
# onlineDownloaded.loc[pd.isnull(onlineDownloaded.Date)] = "July 20, 2013"

# Only BB72 on one day is missing the data, so we can replace it manually
onlineDownloaded.Date[pd.isnull(onlineDownloaded["Date"])] = "July 20, 2013"


#### Rows without survey Sites #############
onlineDownloaded[pd.isnull(onlineDownloaded["Survey Site"])]
# Are for Holly - Jensens Bay; Seven - CHEM; and Laura - Whiffin Spit

# Fill those sites in to the data
onlineDownloaded["Survey Site"][pd.isnull(onlineDownloaded["Survey Site"])] = ["Jensens Bay N", "Chemainus River Estuary", "Whiffin Spit Park" ]

#Split the date into month, day and year columns
# a = onlineDownloaded['Date'].str.split(" ")#.str.split(" ")
# b = onlineDownloaded['Date'].str.split(" ")[:]
splitdates = pd.DataFrame(list(onlineDownloaded.Date.str.split(" ")), columns = ["Month", "Day", "Year"])
splitdates.Day = splitdates.Day.str.replace(',', '')
splitdates.Month = splitdates.Month.str.replace('August', '8')
splitdates.Month = splitdates.Month.str.replace('July', '7')
# print(splitdates.head())
onlineDownloaded["Month"] = splitdates["Month"]
onlineDownloaded["Day"] = splitdates["Day"]
onlineDownloaded["Year"] = splitdates["Year"]
#onlineDownloaded["Day"] = onlineDownloaded['Date'].str.split(" ")[1]


## Uncomment to write ColumnNames # Shouldn't need to do this again.
# ### Now need to export the column names so they can be matched with their replacements. 
# ## Same with Site names

# # 1 Export columns
# columnNames = np.array(onlineDownloaded.columns)
# masterCol = np.array(onlineFromMaster.columns)



# with open(SSDB + 'Python/keys/2013_ColumnNames_Online.txt', mode='wt', encoding='utf-8') as myfile:
#     myfile.write('\t'.join(columnNames))
#     myfile.write('\n')
#     myfile.write('\t'.join(masterCol))
# myfile.close()

# 2 Read in edited column names as dictionary


d = {}
with open(SSDB + 'Python/keys/2013_ColumnNames_Key.txt') as f:
    for line in f:
       lineST = line.strip("\n")
       (key, val) = lineST.split("\t")
       d[str(key)] = val
# 3 - Replace the originals with the new values
onlineDownloaded.rename(columns = d, inplace = True)

#4. Do the same with the Site Names and ID.
# siteNames = np.array(onlineDownloaded['Site'].unique())
# with open(SSDB + 'Python/keys/2013_SiteNames_Online.txt', mode='wt', encoding='utf-8') as myfile:
#     myfile.write('\n'.join(siteNames))

# 5 Read in edited SiteID as dataframe and join to original
siteCodes = pd.read_table(SSDB + 'Python/keys/2013_SiteNames_OnlineKey.txt',delimiter="\t")

################## DATA to MANUALY CORRECT !!!!!!!!!!!!!! WARNING!!############################
# This removes duplicates, remove this peice to discover dupliates again.

# Recorded by Catherine Watson as Fanny Bay. Correct info from Survey Log
onlineDownloaded.Site[onlineDownloaded.Timestamp == pd.to_datetime("2013-09-21 23:23:52")] = "Ship's Point" 
onlineDownloaded.Site[onlineDownloaded.Timestamp == pd.to_datetime("2013-09-21 23:21:17")] = "Ship's Point" 
onlineDownloaded.Site[onlineDownloaded.Timestamp == pd.to_datetime("2013-09-21 23:26:10")] = "Ship's Point"

# Recorded by Steven Roias in two entries. I just entered it manually as much easier. Removed the online records
onlineDownloaded = onlineDownloaded[onlineDownloaded.Timestamp != pd.to_datetime("2013-09-09 10:59:53")]
onlineDownloaded = onlineDownloaded[onlineDownloaded.Timestamp != pd.to_datetime("2013-09-09 11:08:37")]
#onlineDownloaded.Site[onlineDownloaded.Timestamp == pd.to_datetime("2013-09-09 11:08:37")] = "Chemainus River Estuary" 

# Remove duplicate entry by Bob and Barbara Lake.
onlineDownloaded = onlineDownloaded[onlineDownloaded.Timestamp != pd.to_datetime("2013-09-03 14:18:27")]

##### Create clean siteID column and create recordID

onlineDownloadedClean = pd.merge(onlineDownloaded.reset_index(drop=True).copy(), siteCodes, on=["Site"], how='left')
onlineDownloadedClean['RecordID'] = onlineDownloadedClean['SiteID'] + onlineDownloadedClean['Year'].map(str) + \
                onlineDownloadedClean['Month'].map(str) +  \
                onlineDownloadedClean['Day'].map(str)
        
# Two surveyors at Jensens Bay South on July 19. They were in different locations. Adjust Record ID.
onlineDownloadedClean.RecordID[onlineDownloadedClean.Timestamp == pd.to_datetime("2014-02-03 09:16:52")] = "SJEN2013719a"

################################################################################
'''
############## Update to datacleaning_2013.py
# Takes data directly from xlsx files and from downloaded data form now
without modifications outside of this script.
'''


online_2013 = onlineDownloadedClean.copy()



site_columns = ['RecordID', 'SiteID','Site', 'YES_NO_WESA_Obs','Time_Start', 'Tide_Start',
       'Time_End', 'Tide_End', 'Weather', 'Precipitation', 'Sea_State',
       'Tide_State_Start', 'Tide_State_End', 'Visibility', 'Vis_Reason',
       'Vis_Reason_Other', 'Equipment', 'NoActivity', 'Walkers', 'Dogs',
       'Power_Boats', 'Unpowered_Boat', 'Other_Activity', 'PhotoFiles', 'Comments']
observer_columns = ['RecordID', 'SiteID', 'Timestamp', 'Obs1', 'Obs2', 'Address',
       'Email']


observer = online_2013.ix[:,observer_columns].copy()
site_info = pd.DataFrame(online_2013.ix[:,site_columns]).copy() # Site info data frame
observer['Source'] = 'Online' # Enter the source of the data in case of future error
site_info['Source'] = 'Online'

#### Process WESA Counts

# col_names = list(online_2013.columns.values)
# # print(col_names)

# # Loop through count numbers up to 20( this is too high but doesn't matter)
# # Extract count columns and cobine in to a condensed dataframe.
# for j in range(1,20):
#     #print(j)
#     count_col =  list(["RecordID","SiteID"])
   
#     for i in col_names:
#         if re.search("Count_{}((_)|($))".format(j) , i):
#                 count_col.append(i)
#                 found_it = True
                                 
#     if found_it:

#         counts = online_2013.ix[:,count_col].copy()
#         counts.rename(columns=lambda x: re.sub('Count_\d*((_)|($))','',x), inplace=True)
#         counts.rename(columns={"": "Count"}, inplace=True)
#         counts['CountNum'] = str(j)
#         #print(counts.columns.values)
#         if j == 1: Counts_All = counts
#         else: Counts_All = pd.concat([Counts_All, counts],ignore_index=True).copy()
#    # print(len(Counts_All))
#     found_it = False

# Counts_All['Group'] = 'Wesa'
# Counts_All['ID'] = Counts_All['RecordID'] + '_W' + Counts_All['CountNum']
# Counts_All['Source'] = 'Online'

#### Repeat for Falcon counts

# F_col_names = list(online_2013.columns.values)
# #print(col_names)

# for j in range(1,7):
#     print(j)
#     F_count_col =  list(["RecordID","SiteID"])
   
#     for i in F_col_names:
#         if re.search("Falcon_{}((_)|($))".format(j) , i):
#                 #print(i)
#                 F_count_col.append(i)
                                 
#     #print(F_count_col)                             
#     F_counts = online_2013.ix[:,F_count_col].copy()
#     F_counts.rename(columns=lambda x: re.sub('Falcon_\d*((_)|($))','',x), inplace=True)
#     F_counts.rename(columns={"": "Num_Falcons"}, inplace=True)
    
#     F_counts['F_CountNum'] = str(j)
#     #print(F_counts.columns.values)
#     if j == 1: F_Counts_All = F_counts
#     else: F_Counts_All = pd.concat([F_counts,F_Counts_All],ignore_index=True).copy()
#     #print(len(F_Counts_All))
#     #print(F_Counts_All.columns.values)
# #print(F_Counts_All.columns.values)
# F_Counts_All['Group'] = 'Falcon'
# F_Counts_All['ID'] = F_Counts_All['RecordID'] + '_F' + F_Counts_All['F_CountNum']
# F_Counts_All['Source'] = 'Online'
from combineCountsFun import combineCounts
Counts_All = combineCounts( online_2013, 'WESA')
F_Counts_All = combineCounts(online_2013, 'Falcon')


### Extract manual data from Master Files
manuallyEntered_2013 =  pd.read_excel(masterfiles + "2013_ManuallyEntered.xlsx",
                             sheetname = None, na_values=['NA'] )

## ------------ Observer and Site Info -------------------------
observer_manual = pd.read_excel(masterfiles + "2013_ManuallyEntered.xlsx",
                             sheetname = 'Observer Information', na_values=['NA'], skiprows = 3 ) 
# manuallyEntered_2013['Observer Information'].copy()
site_manual = pd.read_excel(masterfiles + "2013_ManuallyEntered.xlsx",
                             sheetname = 'Site Information', na_values=['NA'], skiprows = 1 ) 
# manuallyEntered_2013['Site Information'].copy()
manual_2013 = manuallyEntered_2013['WESA Counts'].ix[1:].copy()
falcon_manual_2013 = manuallyEntered_2013['Falcon Obs'].copy()


colnames_obs = ['Obs1', 'Obs2', 'OtherObs',  'Address', 'Email', 'RecordID']
observer_manual.columns = colnames_obs
# Add the source of the data
observer_manual['Source'] = 'Manual'
site_manual['Source'] = 'Manual'
falcon_manual_2013['Source'] = 'Manual'
manual_2013['Source'] = 'Manual'


colnames_sites_man    = ['Survey Site', 'RecordID', 'SiteID', 'Date', 'Year', 'Month', 'Day', 'Time_Start', 'Time_End', \
                         'Tide_Start', 'Tide_End', 'Collaborative', 'Ratio_WESA', 'Ratio_LESA', 'Weather', 'Precipation',\
                         'Sea_State', 'Tide_State_Start', 'Tide_State_End', 'Visibility', 'Vis_Reason', 'Vis_Reason_Other', \
                         'Equipment', 'Walkers', 'Dogs', 'Power_Boats', 'Unpowered_Boat', 'Other', \
                         'PhotoFiles', 'YES_NO_WESA_Obs', 'Comments', 'Source']
# print(site_manual_clean.columns)
site_manual.columns = colnames_sites_man # Rename colums
site_manual.ix[0:1]


all_obs_info_2013 = pd.concat([observer_manual, observer], ignore_index=True).copy()
all_site_info_2013 = pd.concat([site_manual, site_info], ignore_index=True).copy()
Counts_all_2013 = pd.concat([Counts_All, manual_2013],ignore_index=True).copy()

falcon_2013_manual_and_datasheets = falcon_manual_2013.copy()


falcon_2013_manual_and_datasheets.columns = ['Date', 'Site', 'Year', 'Month', 'Day', 'RecordID', 'SiteID', 'Group', 'ID', \
                                             'Time', 'ObsNum', 'Species', 'Count', 'Behaviour', 'AttackLength', 'Success', \
                                             'PreySpecies', 'Attack_Type', 'Attack_Source', 'Comments', 'Source']

falcon_all_2013 = pd.concat([F_Counts_All, falcon_2013_manual_and_datasheets], ignore_index=True).copy()

all_obs_info_2013.to_csv(outputfolder + 'FromMaster_Observers_2013.csv',sep = '\t')
all_site_info_2013.to_csv(outputfolder + 'FromMaster_Sites_2013.csv',sep = '\t')
Counts_all_2013.to_csv(outputfolder + 'FromMaster_WESA_Counts_2013_update.csv',sep = '\t')
falcon_all_2013.to_csv(outputfolder + "FromMasterFalconCounts2013.csv", sep = '\t')

#### Check for any duplicates.
'''This is how I originally found the duplicate counts 
fixed above. It should now only print out from ALHE, which is being
excluded from the data. If there are others found will need to 
adjust above fixes. '''
all_obs_info_2013['SiteID2'] = all_obs_info_2013.RecordID.str[:4]
nrecords = all_obs_info_2013.groupby("RecordID").count().sort("SiteID2", ascending = False)

duplicates = list(nrecords[nrecords.SiteID2 > 1].index)
print(duplicates)
print(online_2013[online_2013.RecordID.isin(duplicates)].sort("RecordID"))


