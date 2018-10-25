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
#------------------------- 2014 ------------------------------------
##
##############################


# Import Data entered into Google Drive and then downloaded unedited.
onlineDownloaded = pd.read_excel(masterfiles + "DownloadedFromDrive/2014_Summer Shorebird Pulse Data Form (Responses)_Trial_fixDate.xlsx",
                             sheetname = 0, na_values=['NA'] )

siteNames = onlineDownloaded['Survey Site'].unique() # Assign sites to download and enter siteIDs
dates = onlineDownloaded['Survey Date'].unique()


################## DATA to MANUALY CORRECT !!!!!!!!!!!!!! WARNING!!############################
# This removes duplicates, remove this peice to discover dupliates again.
print(onlineDownloaded.shape)
# Remove data entered mistakenly by Stewart. Should go in 2016
onlineDownloaded = onlineDownloaded[onlineDownloaded.Timestamp < pd.datetime(2016,1,1)]



# For some reason there is an extra space for 104th sites
onlineDownloaded['Survey Site'][onlineDownloaded['Survey Site'].str.contains('Boundary Bay - 104th')] = 'Boundary Bay - 104th'
# onlineDownloaded.Site[onlineDownloaded.Site.str.contains('Boundary Bay - 104th')]

# WHIF - Enterted twice. Noted in comments that first entry was an error. Deleting the first one.
onlineDownloaded = onlineDownloaded[onlineDownloaded.Timestamp != pd.to_datetime("2014-07-22 13:00:04.914")].copy()

# ISVI Holy fuck these idiots must have dementia. Entered same date multiple times. I picked the ones that looked cleanest, but
# still subjective. There are differences between the entries, so I have no idea what the fuck they did.
onlineDownloaded = onlineDownloaded[onlineDownloaded.Timestamp != pd.to_datetime("2014-08-01 17:03:27.022")].copy()
onlineDownloaded = onlineDownloaded[onlineDownloaded.Timestamp != pd.to_datetime("2014-08-01 16:59:21.172")].copy()
onlineDownloaded = onlineDownloaded[onlineDownloaded.Timestamp != pd.to_datetime("2014-08-21 11:38:32.052")].copy()

# I for some reason entered it two different ways. Removing online version
onlineDownloaded = onlineDownloaded[onlineDownloaded.Timestamp != pd.to_datetime("2014-07-07 10:32:59.014")].copy()

# COBE - Wow, Catherine W. entered her data 3 times! Thanks
onlineDownloaded = onlineDownloaded[onlineDownloaded.Timestamp != pd.to_datetime("2014-08-25 22:17:07.693")].copy()
onlineDownloaded = onlineDownloaded[onlineDownloaded.Timestamp != pd.to_datetime("2014-08-25 22:16:58.092")].copy()

# BB12 Entered manually, removing online version
onlineDownloaded = onlineDownloaded[onlineDownloaded.Timestamp != pd.to_datetime("2014-07-25 13:47:43.540")].copy()


# Entered Aug 18 twice in addition to above mistake
onlineDownloaded = onlineDownloaded[onlineDownloaded.Timestamp != pd.to_datetime("2014-08-22 10:04:16.848")].copy()




# COWC Mislabelled Date for August 17 as August 18 -- Correct date from hardcopy
onlineDownloaded["Survey Date"][onlineDownloaded.Timestamp == pd.to_datetime("2014-08-20 16:59:13.740")] = "8/17"

# Errors discovered outside of this analysis. Were from manually checking results
# One day CHEM labelled as CROC online
onlineDownloaded["Survey Site"][onlineDownloaded['Survey Site'] == "Crockett Lake"] = "Chemainus River Delta"


# print(onlineDownloaded.shape)
onlineDownloadedClean = onlineDownloaded.reset_index(drop=True).copy()

# print(onlineDownloadedClean['Survey Date'].unique())

#Split the date into month, day and year columns
# a = onlineDownloadedClean['Date'].str.split(" ")#.str.split(" ")
# b = onlineDownloadedClean['Date'].str.split(" ")[:]
# print(pd.DataFrame(onlineDownloadedClean['Survey Date'].str.split("/", 1).tolist() ))

print("----------------------------------------------------")
# splitdates = pd.DataFrame(onlineDownloadedClean['Survey Date'].str.split("/",1).tolist(), 
# 	columns = ["Month", "Day"])
# # print(splitdates[pd.isnull(splitdates.Month)])
# # print(onlineDownloadedClean.shape)
# # print(splitdates.shape)
# # print(print(onlineDownloadedClean.index))
# onlineDownloadedClean["Month"] = splitdates["Month"]
# onlineDownloadedClean["Day"] = splitdates["Day"]
# print(onlineDownloadedClean.Month.unique())
# print(onlineDownloadedClean.Month, splitdates.Month)
# stop


onlineDownloadedClean['Month'], onlineDownloadedClean['Day'] = \
		zip(*onlineDownloadedClean['Survey Date'].apply(lambda x: x.split('/', 1)))

# Uncomment to write ColumnNames # Shouldn't need to do this again.
### Now need to export the column names so they can be matched with their replacements. 
## Same with Site names

# # 1 Export columns
# columnNames = np.array(onlineDownloadedClean.columns)
# masterCol = np.array(old_online_2014.columns)



# with open(SSDB + 'Python/keys/2014_ColumnNames_Online.txt', mode='wt', encoding='utf-8') as myfile:
#     myfile.write('\t'.join(columnNames))
#     myfile.write('\n')
#     myfile.write('\t'.join(masterCol))
# myfile.close()

# 2 Read in edited column names as dictionary
d = {}
with open(SSDB + 'Python/keys/2014_ColumnNames_Key.txt') as f:
    for line in f:
       lineST = line.strip("\n")
       (key, val) = lineST.split("\t")
       d[str(key)] = val
# 3 - Replace the originals with the new values
onlineDownloadedClean.rename(columns = d, inplace = True)

# 5 Read in edited SiteID as dataframe and join to original
siteCodes = pd.read_table(SSDB + 'Python/keys/SiteCodes.txt',delimiter="\t")

##### Create clean siteID column and create recordID
onlineDownloadedClean["Year"] = "2014"
online_2014 = pd.merge(onlineDownloadedClean, siteCodes, left_on=["Site"], right_on = ["OnlineName"], how='left')
online_2014['RecordID'] = online_2014['SiteID'] + online_2014['Year'].map(str) + \
                online_2014['Month'].map(str) +  \
                online_2014['Day'].map(str)

# print(online_2014)



###### More error removal ###############
# Remove two test records that I inserted. 
online_2014 = online_2014[online_2014.Month.isin(["7",'8'])]

# Alison and Nicki did seperate but equal surveys at 104th. Label them separately
online_2014.RecordID[online_2014.Timestamp == pd.to_datetime("2014-07-21 12:26:27.225")] = "BB042014719a"

### --------------Update to datacleaning_2014.py -----------------------

online_2014['Source'] = 'Online'

observer_columns = ['RecordID', 'SiteID', 'Timestamp', 'Obs1', 'Obs2', 'Address', 'Email',
       'Site', 'Source']

site_columns = ['RecordID', 'SiteID', 'Site', 'Collaborative', 'Ratio', 'Time_Start',
       'Tide_Start', 'Time_End', 'Tide_End', 'Date', 'Month', 'Day', 'Weather',
       'Precipitation', 'Sea_State', 'Tide_State_Start', 'Tide_State_End',
       'Visibility', 'Vis_Reason', 'Vis_Reason_Other', 'Equipment',
       'NoActivity', 'Walkers', 'Dogs', 'Power_Boats', 'Unpowered_Boat',
       'Other_Activity', 'PhotoFiles', 'YES_NO_WESA_Obs', 'Comments', 'Source' ]

observer = online_2014.ix[:,observer_columns].copy()
site_info = online_2014.ix[:, site_columns].copy() 

from combineCountsFun import combineCounts

## Convert WESA counts to rows
WESA_2014 = combineCounts(online_2014, "WESA")
print("Null IDs from WESA. Should be empty\n", WESA_2014[pd.isnull(WESA_2014.ID)]) 

# Convert falcon columns to dataframe rows
falc_2014 = combineCounts(online_2014, "Falcon")

## ---- Output counts where 0 WESA were observed 
no_obs_Online2014 = site_info[site_info.YES_NO_WESA_Obs != 'I saw at least 1 WESA during the survey']

columns_Iwant = ['RecordID','SiteID','Collaborative','Time_Start','Time_End','Date','Month','Day']
no_obs_Online2014_short = no_obs_Online2014.loc[:,columns_Iwant]
no_obs_Online2014_short['Count'] = 0
# print(no_obs_Online2014_short.head())
no_obs_Online2014_short.to_csv(outputfolder + 'noObsOnline2014_Master.csv', sep = '\t')

### Extract manual data from Master Files
manuallyEntered_2014 =  pd.read_excel(masterfiles + "2014_ManuallyEntered.xlsx",
                             sheetname = None, na_values=['NA'] )
volunteer_datasheets2014 = pd.read_excel(masterfiles + "2014Datasheets_FromObservers_Repaired.xlsx",
                                     sheetname = None, na_values = ['NA'])

## ------------ Observer and Site Info -------------------------
observer_manual2014 = pd.read_excel(masterfiles + "2014_ManuallyEntered.xlsx",
                             sheetname = 'Observer Information', na_values=['NA'], skiprows = 3 ) 
observer_datasheets2014 = pd.read_excel(masterfiles + "2014Datasheets_FromObservers_Repaired.xlsx",
                                     sheetname = 'Observer Information', na_values = ['NA'], skiprows = 3)
# manuallyEntered_2013['Observer Information'].copy()
site_manual2014 = pd.read_excel(masterfiles + "2014_ManuallyEntered.xlsx",
                             sheetname = 'Site Information', na_values=['NA'], skiprows = 1 ) 
site_datasheets2014 = pd.read_excel(masterfiles + "2014Datasheets_FromObservers_Repaired.xlsx",
                                     sheetname = 'Site Information', na_values = ['NA'], skiprows = 1)

# manuallyEntered_2013['Site Information'].copy()
WESA_manual_2014 = manuallyEntered_2014['WESA Counts'].copy().ix[1:].copy()

########### -------------- DANGER --- BIRGIT Uncertain here. Need to check how this biases results
## Modify Birgit's entry for Boundary Bay Based on her comments on the datasheets
WESA_manual_2014["Count"][WESA_manual_2014.ID == "BB922014719_W10"] = 1500
WESA_manual_2014["Count"][WESA_manual_2014.ID == "BB922014719_W3"] = 4000


WESA_datasheets_2014 = volunteer_datasheets2014['WESA Counts'].copy().ix[1:].copy()
falcon_manual_2014 = manuallyEntered_2014['Falcon Obs'].copy()
falcon_datasheets_2014 = volunteer_datasheets2014['Falcon Obs'].copy()

colnames_obs = ['Obs1', 'Obs2', 'OtherObs',  'Address', 'Email', 'RecordID']
observer_manual2014.columns = colnames_obs
observer_datasheets2014.columns = colnames_obs



### Add the source of the data as a column
observer_manual2014['Source'] = 'Manual'
site_manual2014['Source'] = 'Manual'
falcon_manual_2014['Source'] = 'Manual'
WESA_manual_2014['Source'] = 'Manual'

observer_datasheets2014['Source'] = 'Datasheet'
site_datasheets2014['Source'] = 'Datasheet'
falcon_datasheets_2014['Source'] = 'Datasheet'
WESA_datasheets_2014['Source'] = 'Datasheet'

# Standardize columns
colnames_sites_man    = ['Site', 'RecordID', 'SiteID', 'Date', 'Year', 'Month', 'Day', 'Time_Start', 'Time_End', \
                         'Tide_Start', 'Tide_End', 'Collaborative', 'Ratio_WESA', 'Ratio_LESA', 'Weather', 'Precipation',\
                         'Sea_State', 'Tide_State_Start', 'Tide_State_End', 'Visibility', 'Vis_Reason', 'Vis_Reason_Other', \
                         'Equipment', 'Walkers', 'Dogs', 'Power_Boats', 'Unpowered_Boat', 'Other', \
                         'PhotoFiles', 'YES_NO_WESA_Obs', 'Comments', 'Source']


site_manual2014.columns = colnames_sites_man # Rename colums
site_datasheets2014.columns = colnames_sites_man[:-2] + ["Source"]

# ------------Combine all source data together #########

all_obs_info_2014 = pd.concat([observer_manual2014, observer, observer_datasheets2014], ignore_index=True).copy()
all_site_info_2014 = pd.concat([site_manual2014, site_info, site_datasheets2014], ignore_index=True).copy()
WESA_all_2014 = pd.concat([WESA_2014, WESA_manual_2014, WESA_datasheets_2014],ignore_index=True).copy()


falcon_2014_manual_and_datasheets = falcon_manual_2014.copy()

falc_col = ['Date', 'Site', 'Year', 'Month', 'Day', 'RecordID', 'SiteID', 'Group', 'ID', \
                                             'Time', 'ObsNum', 'Species', 'Count', 'Behaviour', 'AttackLength', 'Success', \
                                             'PreySpecies', 'Attack_Type', 'Attack_Source', 'Comments', 'Source']
falcon_2014_manual_and_datasheets.columns = falc_col
falcon_datasheets_2014.columns = falc_col

falcon_all_2014 = pd.concat([falc_2014, falcon_2014_manual_and_datasheets, falcon_datasheets_2014], ignore_index=True).copy()


### ----------- Export data to csv -------------------------

all_obs_info_2014.to_csv(outputfolder + 'FromMaster_Observers_2014.csv',sep = '\t')
all_site_info_2014.to_csv(outputfolder + 'FromMaster_Sites_2014.csv',sep = '\t')
WESA_all_2014.to_csv(outputfolder + 'FromMaster_WESA_Counts_2014.csv',sep = '\t')
falcon_all_2014.to_csv(outputfolder + "FromMasterFalconCounts2014.csv", sep = '\t')



#### ----------------Check for any duplicates.------------------
'''This is how I originally found the duplicate counts 
fixed above. It should now only print out from ALHE, which is being
excluded from the data. If there are others found will need to 
adjust above fixes. '''
all_obs_info_2014['SiteID2'] = all_obs_info_2014.RecordID.str[:4]
nrecords = all_obs_info_2014.groupby("RecordID").count().sort("SiteID2", ascending = False)

duplicates = list(nrecords[nrecords.SiteID2 > 1].index)
print(duplicates)
print(all_obs_info_2014[all_obs_info_2014.RecordID.isin(duplicates)].sort("RecordID"))


