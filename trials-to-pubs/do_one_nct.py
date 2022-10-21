# -*- coding: utf-8 -*-
#Created on Thu Aug  1 10:43:18 2019
#@author: Arthur Holt
#
#The program performs an example model application for the the model described in 
#Smalheiser NR, Holt AW (2021). A Web-based Tool for Automatically linking Clinical Trials to their Publications.
#https://www.medrxiv.org/content/10.1101/2021.06.24.21259481v1



#needed python modules
#import mysql.connector
import pandas as pd
import numpy as np
import re
import json
import subprocess
from io import StringIO
from nameparser import HumanName
import sys
import datetime
import itertools
from scipy import stats
import os
import time
import requests
import pickle
import urllib
import logging
import sys



amtesting = 'Y'

#########################################
# locale information
#########################################
#These variables are used by the web app for storing files in permission-specific areas
#for this example, they all point to the location from which this program is run
appdir = os.getcwd()
cachedir = appdir
cachetest = cachedir
baseDir = appdir
os.chdir(baseDir)

if amtesting == 'Y':
  cachetest = appdir



#time variables that are used by the webserver to show approximate completion %
basestart = datetime.datetime.now()
esttime = 60*10


#This function updates the live web page as scoring is run.  Here, it just prints.
def outstat(mtxt,j):
  if amtesting == 'Y':
    print(mtxt)




#########################################
# job information
#########################################
job='0XF28DD'



#this variable is used throughout this script for the NCT number
trial = sys.argv[1]

statmsg ='<br><b>Beginning model application for ' + trial +  '</b><br>You may bookmark this page and return later.<br>'

outstat(statmsg,job)

trialquery = 'P'

#########################################
# model parameters
#########################################



#list of features, pubmed fields, and methods to compute
flist = """F03,pmid,aggmax
F01,authors,authtree
F16,titleabs,propoccurance
F13,titleabs,tsim
F24,mesh,meshsim
F31,authors,authtree
F30,abstract,occurance
FTA,ta_allcaps,mismatch
F25,mesh,meshsim
F33,grants,occurance
F29,grants,occurance
F15,titleabs,propoccurance
F02,titleabs,allcaps
F13,chemicals,reverseoccur
F27,abstract,occurance
F30,cn,occurance
F09,titleabs,occurance
F10,titleabs,occurance
F36,authors,authtree"""


#regression parameters
MODo={}
MODo['F03_pmid_aggmax'] = float(4.204533)
MODo['authcomp'] = float(0.035287)
MODo['F16_titleabs_propoccurance'] = float(1.295308)
MODo['F13_titleabs_tsimw_scr'] = float(0.000234)
MODo['F24_mesh_meshsim'] = float(1.526459)
MODo['startdate_lift'] = float(1.017043)
MODo['F30_abstract_occurance'] = float(1.401932)
MODo['F25_mesh_meshsim'] = float(0.766648)
MODo['F02_titleabs_allcaps'] = float(0.796624)
MODo['F33_grants_occurance'] = float(2.137037)
MODo['F29_grants_occurance'] = float(1.10225)
MODo['FTA_ta_allcaps_mismatch'] = float(-1.270504)
MODo['F15_titleabs_propoccurance'] = float(0.496415)
MODo['F27_abstract_occurance'] = float(2.082105)
MODo['F13_chemicals_reverseoccur'] = float(2.572171)
MODo['F10_titleabs_occurance'] = float(0.39156)
MODo['F30_cn_occurance'] = float(2.935671)
MODo['F09_titleabs_occurance'] = float(1.404767)

MODo_intercept = float(-2.34807)



#regression parameters for model 11, only difference is aggmax and authcomp are combined into one feature
MOD={}
MOD['agginvint'] = float(7.064431)
MOD['F16_titleabs_propoccurance'] = float(1.321135)
MOD['F13_titleabs_tsimw_scr'] = float(0.000237)
MOD['F24_mesh_meshsim'] = float(1.54084)
MOD['startdate_lift'] = float(1.03154)
MOD['F30_abstract_occurance'] = float(1.371825)
MOD['F25_mesh_meshsim'] = float(0.794547)
MOD['F02_titleabs_allcaps'] = float(0.805654)
MOD['F29_grants_occurance'] = float(1.19076)
MOD['F33_grants_occurance'] = float(2.002089)
MOD['FTA_ta_allcaps_mismatch'] = float(-1.290924)
MOD['F15_titleabs_propoccurance'] = float(0.50953)
MOD['F27_abstract_occurance'] = float(2.118923)
MOD['F13_chemicals_reverseoccur'] = float(2.641784)
MOD['F10_titleabs_occurance'] = float(0.412927)
MOD['F30_cn_occurance'] = float(2.980237)
MOD['F09_titleabs_occurance'] = float(1.448419)

MOD_intercept = float(-3.516664)



#mean substitution
MODsub = {}
MODsub['F03_pmid_aggmax'] = float(0.14455864)
MODsub['agginvint'] = float(0.244589999579225)
MODsub['authcomp'] = float(0)
MODsub['F16_titleabs_propoccurance'] = float(0)
MODsub['F13_titleabs_tsimw_scr'] = float(2608.84660876887)
MODsub['F24_mesh_meshsim'] = float(0.285332373297017)
MODsub['startdate_lift'] = float(1.07582781804635)
MODsub['F30_abstract_occurance'] = float(0)
MODsub['F25_mesh_meshsim'] = float(0.506728134364736)
MODsub['F02_titleabs_allcaps'] = float(0.158897310349453)
MODsub['F33_grants_occurance'] = float(0)
MODsub['F29_grants_occurance'] = float(0)
MODsub['FTA_ta_allcaps_mismatch'] = float(0)
MODsub['F15_titleabs_propoccurance'] = float(0)
MODsub['F27_abstract_occurance'] = float(0)
MODsub['F13_chemicals_reverseoccur'] = float(0.123653040492693)
MODsub['F10_titleabs_occurance'] = float(0)
MODsub['F30_cn_occurance'] = float(0)
MODsub['F09_titleabs_occurance'] = float(0)


MODnice = {}
MODnice['F03_pmid_aggmax']='Aggregator'
MODnice['agginvint']='Investigator/Aggregator Max'
MODnice['authcomp']='Investigator->authors'
MODnice['F16_titleabs_propoccurance']='Interventions->title/abs'
MODnice['F13_titleabs_tsimw_scr']='Text similarity'
MODnice['F24_mesh_meshsim']='Interventions MeSH->article MeSH'
MODnice['startdate_lift']='Pub Date'
MODnice['F30_abstract_occurance']='Acronym->abstract'
MODnice['F25_mesh_meshsim']='Condition MeSH->article MeSH'
MODnice['F02_titleabs_allcaps']='Title->title/abs All Caps'
MODnice['F33_grants_occurance']='Secondary ID->Grants'
MODnice['F29_grants_occurance']='Country->Grants'
MODnice['FTA_ta_allcaps_mismatch']='Trial->Article All Caps Mismatch'
MODnice['F15_titleabs_propoccurance']='Conditions->title/abs'
MODnice['F27_abstract_occurance']='Study ID->Abstract'
MODnice['F13_chemicals_reverseoccur']='Summary->chemicals'
MODnice['F10_titleabs_occurance']='Allocation->title/abs'
MODnice['F30_cn_occurance']='Acronym->CN'
MODnice['F09_titleabs_occurance']='Study Type->title/abs'


#this is used to reduce the size of the "Noteworthy Features"
MODmap = {}
MODmap['F03_pmid_aggmax']='A'
MODmap['agginvint']='B'
MODmap['authcomp']='C'
MODmap['F16_titleabs_propoccurance']='D'
MODmap['F13_titleabs_tsimw_scr']='E'
MODmap['F24_mesh_meshsim']='F'
MODmap['startdate_lift']='G'
MODmap['F30_abstract_occurance']='H'
MODmap['F25_mesh_meshsim']='I'
MODmap['F02_titleabs_allcaps']='J'
MODmap['F33_grants_occurance']='K'
MODmap['F29_grants_occurance']='L'
MODmap['FTA_ta_allcaps_mismatch']='M'
MODmap['F15_titleabs_propoccurance']='N'
MODmap['F27_abstract_occurance']='O'
MODmap['F13_chemicals_reverseoccur']='P'
MODmap['F10_titleabs_occurance']='Q'
MODmap['F30_cn_occurance']='R'
MODmap['F09_titleabs_occurance']='S'

#reverse lookup

MODmapR = {}

for k in MODmap.keys():
  MODmapR[MODmap[k]] = k




#trial start date difference lift
startcast = {}
startcast[-1]=0
startcast[0]=0
startcast[1]=0
startcast[2]=0
startcast[3]=0
startcast[4]=0
startcast[5]=0.26100307062436
startcast[6]=0.211873080859774
startcast[7]=0.15532241555783
startcast[8]=0.112205731832139
startcast[9]=0.0766376663254861
startcast[10]=0.0538638689866939
startcast[11]=0.0317297850562947
startcast[12]=0.029042988741044
startcast[13]=0.0180399181166837
startcast[14]=0.0152251791197543
startcast[15]=0.0097236438075742
startcast[16]=0.00895598771750256
startcast[17]=0.00588536335721596
startcast[18]=0.00537359263050153
startcast[19]=0.00230296827021494
startcast[20]=0.00281473899692937
startcast[21]=0

startlift = {}
startlift[0]=0.0791550925925926
startlift[1]=0.235266203703704
startlift[2]=0.798333333333333
startlift[3]=1.25251157407407
startlift[4]=1.34796296296296
startlift[5]=1.28313657407407
startlift[6]=1.29091435185185
startlift[7]=1.25299768518519
startlift[8]=1.10243055555556
startlift[9]=1.05590277777778
startlift[10]=0.996041666666667
startlift[11]=0.944351851851852
startlift[12]=1.38649305555556
startlift[13]=1.21094907407407
startlift[14]=1.57599537037037
startlift[15]=1.43228009259259
startlift[16]=1.36520833333333

#dictionary to map author counts to Aggregator values
aggdict = {}
aggdict[0]=0.236849605005533
aggdict[1]=0.799776153180279
aggdict[2]=0.858079976654628
aggdict[3]=0.879815195028985
aggdict[4]=0.922516947999999
aggdict[5]=0.924174518944444
aggdict[6]=0.947440810421052
aggdict[7]=0.934076053928571
aggdict[8]=0.9523414651875
aggdict[9]=0.990714104235294



#xml paths for trial data from clinicaltrials.gov
ctxpath = {}
ctxpath['F01'] = ['//clinical_study/overall_official/last_name']
ctxpath['F02'] = ['//clinical_study/brief_title']
ctxpath['F09'] = ['//clinical_study/study_type']
ctxpath['F10'] = ['//clinical_study/study_design_info/allocation']
ctxpath['F13'] = ['//clinical_study/brief_summary/textblock']
ctxpath['F15'] = ['//clinical_study/condition']
ctxpath['F16'] = ['//clinical_study/intervention/intervention_name']
ctxpath['F24'] = ['//clinical_study/intervention_browse/mesh_term']
ctxpath['F25'] = ['//clinical_study/condition_browse/mesh_term']
ctxpath['F27'] = ['//clinical_study/id_info/org_study_id']
ctxpath['F29'] = ['//clinical_study/location_countries/country']
ctxpath['F30'] = ['//clinical_study/acronym']
ctxpath['F31'] = ['//clinical_study/responsible_party/investigator_full_name']
ctxpath['F33'] = ['//clinical_study/id_info/secondary_id']
ctxpath['F36'] = ['//clinical_study/clinical_results/point_of_contact/name_or_title']


#dictionary to convert model probabilities to odds ratios for rescaling
oddsdict = {}
oddsdict[0]=.000000980894
oddsdict[0.01]=.000010909012
oddsdict[0.02]=.000030083876
oddsdict[0.03]=.000052726013
oddsdict[0.04]=.000107714488
oddsdict[0.05]=.000130691128
oddsdict[0.06]=.000168898908
oddsdict[0.07]=.000271835061
oddsdict[0.08]=.000280245585
oddsdict[0.09]=.000293219365
oddsdict[0.1]=.000453680491
oddsdict[0.11]=.000520910748
oddsdict[0.12]=.000365614418
oddsdict[0.13]=.000462469359
oddsdict[0.14]=.000553654586
oddsdict[0.15]=.000766747333
oddsdict[0.16]=.00067358043
oddsdict[0.17]=.000798057084
oddsdict[0.18]=.000945916233
oddsdict[0.19]=.000786674193
oddsdict[0.2]=.000784539495
oddsdict[0.21]=.000916537767
oddsdict[0.22]=.001059894371
oddsdict[0.23]=.0012932813
oddsdict[0.24]=.001167936043
oddsdict[0.25]=.00116234332
oddsdict[0.26]=.00168337759
oddsdict[0.27]=.00150080305
oddsdict[0.28]=.001496369729
oddsdict[0.29]=.001827211394
oddsdict[0.3]=.001201957066
oddsdict[0.31]=.001681508993
oddsdict[0.32]=.001884282263
oddsdict[0.33]=.001741317283
oddsdict[0.34]=.002063840322
oddsdict[0.35]=.002467667587
oddsdict[0.36]=.002020441346
oddsdict[0.37]=.002540659097
oddsdict[0.38]=.002877843944
oddsdict[0.39]=.002602175152
oddsdict[0.4]=.00208715538
oddsdict[0.41]=.002468806121
oddsdict[0.42]=.003221488068
oddsdict[0.43]=.002645933331
oddsdict[0.44]=.00391710469
oddsdict[0.45]=.003393614126
oddsdict[0.46]=.003268307906
oddsdict[0.47]=.003126243622
oddsdict[0.48]=.003161472564
oddsdict[0.49]=.004365004998
oddsdict[0.5]=.004391538247
oddsdict[0.51]=.004745231914
oddsdict[0.52]=.003621207074
oddsdict[0.53]=.004318455691
oddsdict[0.54]=.00412870284
oddsdict[0.55]=.005488120739
oddsdict[0.56]=.006002718567
oddsdict[0.57]=.005267462797
oddsdict[0.58]=.005278684021
oddsdict[0.59]=.006452648187
oddsdict[0.6]=.005768121771
oddsdict[0.61]=.007399729768
oddsdict[0.62]=.008908729576
oddsdict[0.63]=.007832669926
oddsdict[0.64]=.006445595567
oddsdict[0.65]=.010158397427
oddsdict[0.66]=.010557800364
oddsdict[0.67]=.009845436328
oddsdict[0.68]=.011673799726
oddsdict[0.69]=.012421854964
oddsdict[0.7]=.012037083613
oddsdict[0.71]=.011029480861
oddsdict[0.72]=.014187264289
oddsdict[0.73]=.013930215183
oddsdict[0.74]=.016788921613
oddsdict[0.75]=.018567606025
oddsdict[0.76]=.018835279687
oddsdict[0.77]=.021220179551
oddsdict[0.78]=.024055845325
oddsdict[0.79]=.02322490494
oddsdict[0.8]=.027167187971
oddsdict[0.81]=.035791250199
oddsdict[0.82]=.038623210933
oddsdict[0.83]=.03924893171
oddsdict[0.84]=.043537351896
oddsdict[0.85]=.058500425012
oddsdict[0.86]=.063474118298
oddsdict[0.87]=.07950136523
oddsdict[0.88]=.086653180387
oddsdict[0.89]=.108780978242
oddsdict[0.9]=.140013149188
oddsdict[0.91]=.180571560211
oddsdict[0.92]=.256457230448
oddsdict[0.93]=.353362562033
oddsdict[0.94]=.480092663109
oddsdict[0.95]=.687539392344
oddsdict[0.96]=1.202707010962
oddsdict[0.97]=2.233825247408
oddsdict[0.98]=4.747515196605
oddsdict[0.99]=22.7512738835
oddsdict[1]=81.


def adjustOdds(inProb):
  if inProb <= 0:
    return 0
  if inProb >= 1:
    return 1
  problow = np.floor(inProb*100)/100
  probhigh = np.ceil(inProb*100)/100
  if probhigh > problow:
    probdist = (inProb - problow) / (probhigh - problow)
  else:
    probdist = 0
  print(problow,probhigh,probdist)
  oddslow = oddsdict[problow]
  oddshigh = oddsdict[probhigh]
  oddsadjust = oddslow + (probdist*(oddshigh-oddslow))
  return 1/(1+(1/oddsadjust))




#convert feature list to a pandas dataframe
flistdf = pd.DataFrame(columns=['featnum','pubmed','method'], 
  data=[row.split(',') for row in flist.splitlines()])
flistdf['pubmedlist'] = flistdf.apply(lambda row: row['pubmed'].split('|'), axis=1)
flistdf['methodlist'] = flistdf.apply(lambda row: row['method'].split('|'), axis=1)

#create base dataframe that will hold the computed fields
clist = ['sample_id','posneg', 'testcontrol','NCT_num','PMID']
for index, row in flistdf.iterrows():
  for p in row['pubmedlist']:
    for m in row['methodlist']:
      if m in ['tsim','rksimax']:
        clist.extend([row['featnum']+'_'+p+'_'+m + 'unw_tms'])
        clist.extend([row['featnum']+'_'+p+'_'+m + 'unw_scr'])
        clist.extend([row['featnum']+'_'+p+'_'+m + 'w_tms'])
        clist.extend([row['featnum']+'_'+p+'_'+m + 'w_scr'])
      else:
        clist.extend([row['featnum']+'_'+p+'_'+m])

clist.append('startdate_lift')
clist.append('start_diff_lt5')
clist.append('start_diff_cast')
clist.append('authcomp')
clist.append('agginvint')
clist.append('prob')
modelform = pd.DataFrame(columns=clist)
modeldata = modelform.copy()



############################################
#import functions here
############################################

#for this example, all files are located in the current folder
lfiles = ''


#this was not implemented as a package
exec(open('TrialLink_functions_Example.py').read())

#regular expression to pick out capitalized words
caps = re.compile(r'\b([A-Z]{3,})\b') 
#regular expression to pick out nct number
nlink = re.compile('\[.*?\]') 
#regular expression to puck out capitals
fletter = re.compile(r'\b[A-Z]')


#stop list
#general stop words
with open (lfiles + 'stoplist.pkl','rb') as f:
  stoplist = pickle.load(f)


#nickname list
with open (lfiles + 'nicknames.pkl','rb') as f:
  nickdict = pickle.load(f)


#stop list for MeSH
with open (lfiles + 'stoplistmesh.pkl','rb') as f:
  stoplistmesh = pickle.load(f)

MeSHstop = stoplistmesh

#country list, converted to a dictionary for speed
#countriesdf = pd.read_csv(lfiles + 'country_list2.csv')
#countries = dict([(observed,official) for observed,official in zip(countriesdf.observed,countriesdf.official)])
#with open (lfiles + 'countries.pkl','wb') as f:
#  pickle.dump(countries,f)
with open (lfiles + 'countries.pkl','rb') as f:
  countries = pickle.load(f)

#ADAM abbreviation list
with open (lfiles + 'adam.pkl','rb') as f:
  adam = pickle.load(f)


#backslash is a problematic delimiter for parts of the name
escape_dict={'\a':r'\a',
             '\b':r'\b',
             '\c':r'\c',
             '\f':r'\f',
             '\n':r'\n',
             '\r':r'\r',
             '\t':r'\t',
             '\v':r'\v',
             '\'':r'\'',
             '\"':r'\"'}





#function to break up lists to sizes that sql code can handle in a where clause
def chunker(seq, size):
    return (seq[pos:pos + size] for pos in range(0, len(seq), size))



#new function added for the 'mis-matched all caps feature'

def comp_mismatch(csett,ptxt):
  #print(csett,ptxt)
  cval = 0
  if len(csett)==1 and len(ptxt)>0:
    t = list(csett)[0]
    if t not in adam2:
      cval = 1
      mcand = ptxt
      mcandin = mcand
      for w in mcand:
        if levenshtein(w,t) < 3:
          cval=0
  if len(ptxt)==1 and len(csett)>0:
    t = list(ptxt)[0]
    if t not in adam2:
      cval = 1
      mcand = csett
      mcandin = mcand
      for w in mcand:
        if levenshtein(w,t) < 3:
          cval=0
  return cval


#new, faster ADAM list
adam2 = set()
for a in adam:
  for b in a.split('|'):
    if ':' in b:
      adam2.add(b[:b.find(':')].upper())
    else:
      adam2.add(b.upper())

#function to remove stop words and ADAM abbrevations
def suppressWords(inWord):
  result = inWord.upper()
  if result in stoplist:
    result = 'remove'
  if result in adam2:
    result = 'remove'
  return result



############################################
#find already-linked articles 
############################################
passure  = '0'
searchres = []
searchlinks = []

#nct links
selectSQL = 'select c.pmid from PUBMED2015.ctmap c WHERE c.ctid_cleaned = "{0}"    ;'
#dbcursor.execute(selectSQL.format(trial) )    
#rks = dbcursor.fetchall()
with open('dataset_01.pkl', 'rb') as f:
  rks = pickle.load(f)

for r in rks:
  searchlinks.append(r[0])



#results reference
selectSQL = '''select a.value from clinicaltrials.clinicaltrials a JOIN clinicaltrials.xmlpaths b on a.xmlpathid = b.xmlpathid 
WHERE b.xmlpath = '//clinical_study/results_reference/PMID' and a.NCT_num = "{0}"    ;'''
#dbcursor.execute(selectSQL.format(trial) )    
#rks = dbcursor.fetchall()
with open('dataset_02.pkl', 'rb') as f:
  rks = pickle.load(f)

for r in rks:
  searchres.append(int(r[0]))


passureT = set(searchlinks) | set(searchres)
passure = ','.join([str(v) for v in list(passureT)])

statmsg += '<b>Found ' + str(len(searchlinks)) + ' linked articles from PubMed, ' + str(len(searchres)) + ' result references from ClinicalTrials.gov, '+ str(len(passureT)) + ' unique PMIDs</b><br>'
if trialquery == 'P':
  statmsg += 'Applying preselection, this will take about 60 seconds.<BR>'

outstat(statmsg,job)



#####################################
#get prospective pmids for this trial
#####################################
with open('stoplist.txt', 'r') as file:
    stopA = file.read()

with open('stoplist1000.txt', 'r') as file:
    stopB = file.read()

stopadd = '''
PATIENTS PURPOSE PLACEBO EVALUATE
DETERMINE EFFECTS SUBJECTS ASSESS
DISEASES AIM MONTHS WEEKS
OBJECTIVE YEARS - INVESTIGATORS
WELL PARTICIPANTS RECEIVE TREATING
CELLS TOLERABILITY CELL OUTCOMES
DAILY DRUGS ADULTS CHEMOTHERAPY
CONDUCTED INFECTIONS STUDIES DESIGNED
SYMPTOMS DISORDERS GIVEN
'''

listA = [v.upper() for v in stopA.split()]
listB = [v.upper() for v in stopB.split()]
listC = stopadd.split()


stopAll = set(listA) | set(listB) |  set(listC)
stop365 =  set(listA) | set(listC)

def is_number(s):
    try:
        float(s)
        return True
    except ValueError:
        return False

def cleanword(w):
  tw = w.upper()
  if tw in stopAll:
    tw = ' '
  if is_number(tw):
    tw = ' '
  if len(tw)==1:
    tw = ' '
  return tw

def cleanword365(w):
  tw = w.upper()
  if tw in stop365:
    tw = ' '
  if is_number(tw):
    tw = ' '
  if len(tw)==1:
    tw = ' '
  return tw


selectSQL = 'SELECT ctindex, trial_start_d, refdate_min from clinicaltrials.ctlookup WHERE NCT_num="' + trial + '" ;'
#dbcursor.execute(selectSQL)     
#res = dbcursor.fetchall()

with open('dataset_03.pkl', 'rb') as f:
  res = pickle.load(f)



if len(res) == 1:
  trialindex = res[0][0]
  trialstart = res[0][1]
  refdate = res[0][2]

if refdate is not None:
  y = str(refdate.year - 2)
else:
  y = '1987'


trial_condpick = ['//clinical_study/condition_browse/mesh_term','//clinical_study/condition']
trial_intpick = ['//clinical_study/intervention/intervention_name','//clinical_study/intervention/other_name',
'//clinical_study/intervention/arm_group_label','//clinical_study/intervention_browse/mesh_term','//clinical_study/intervention/description']
mesh_synpick = 'select syn from clinicaltrials.ctgov_MeSHtrans where mesh_term like "{0}";'


psql = '''select a.value from clinicaltrials.clinicaltrials a 
 where a.NCT_num = "{0}" and a.xmlpathid in 
 (select b.xmlpathid from clinicaltrials.xmlpaths b where b.xmlpath in ({1}) );  '''

matchcount = '''select PMID 
  from clinicaltrials.pub_words  
  where year(pubdate) > {0} and {1}   '''

matchcountD = '''select PMID, {2} as wcount, {3} as ccount, {4} as icount  
  from clinicaltrials.pub_words  
  where year(pubdate) > {0} and {1}   '''


#trial title and descriptions
#dbcursor.execute(psql.format(trial,'"//clinical_study/brief_title"'))
#rks = dbcursor.fetchall()

with open('dataset_04.pkl', 'rb') as f:
  rks = pickle.load(f)

if len(rks) > 0:
  ttitle = rks[0][0]


if len(ttitle) < 2:
  #dbcursor.execute(psql.format(trial,'"//clinical_study/official_title"'))
  #rks = dbcursor.fetchall()
  if len(rks) > 0:
    ttitle = rks[0][0]



tdesc = ''
#dbcursor.execute(psql.format(trial,'"//clinical_study/brief_summary/textblock","//clinical_study/brief_summary/textblock","//clinical_study/detailed_description/textblock"'))
#rks = dbcursor.fetchall()
with open('dataset_05.pkl', 'rb') as f:
  rks = pickle.load(f)


if len(rks) > 0:
  tdesc = ' ' + rks[0][0]

tstring = re.sub(r'[()*=±%;:|.,"]', ' ', ttitle + ' ' + tdesc)
tcomb = ' '.join([cleanword(v.upper()) for v in tstring.split()]) 
tcomb = ' '.join(tcomb.split())
#nset.at[index,'twords'] = tcomb


######################### #conditions
hascond = 0
condlist = []
#dbcursor.execute(psql.format(trial,','.join(['"'+v+'"' for v in trial_condpick])))
#rks = dbcursor.fetchall()
with open('dataset_06.pkl', 'rb') as f:
  rks = pickle.load(f)


#if len(rks) > 0:
#  for r in rks:
#    condlist.append(cleanword365(r[0].upper()))
#    dbcursor.execute(mesh_synpick.format(r[0].upper()))
#    rks2 = dbcursor.fetchall()
#    for r2 in rks2:
#      hascond=1
#      condlist.append(r2[0].upper())

with open('dataset_07.pkl', 'rb') as f:
  condlist = pickle.load(f)


condlist = set(condlist) - set([' '])




######################### interventions
intlist = []
#dbcursor.execute(psql.format(trial,','.join(['"'+v+'"' for v in trial_intpick])))
#rks = dbcursor.fetchall()

with open('dataset_08.pkl', 'rb') as f:
  rks = pickle.load(f)

if len(rks) > 0:
  for r in rks:
    rtemp = re.sub(r'[()*=±%;:|.,"]', ' ',r[0])
    intlist.extend([v.upper() for v in rtemp.split()])

intset = set([cleanword365(v) for v in ' '.join(intlist).split()]) - set([' '])
inttemp = ' '.join(list(intset))


#build where clauses

c1where = '+'.join([' (case when match(searchwords) against("{0}") > 0 then 1 else 0 end) '.format(w) for w in list(tcomb.split())])

if hascond==1:
  c2where = '+'.join([' (case when match(searchwords) against(\'"{0}"\') > 0 then 1 else 0 end) '.format(w) for w in list(condlist)]) 
  newcond = condlist
else:
    condtemp = ' '.join(list(condlist))
    newcond = set([cleanword365(w) for w in condtemp.split()])
    newcond.discard(' ')
    c2where = '+'.join([' (case when match(searchwords) against(\'"{0}"\') > 0 then 1 else 0 end) '.format(w) for w in list(newcond)]) 

if len(inttemp) > 2:
  c3where = '+'.join([' (case when match(searchwords) against("{0}") > 0 then 1 else 0 end) '.format(w) for w in inttemp.split()]) 
else:
  c3where = '+'.join([' (case when match(searchwords) against("X") > 0 then 1 else 1 end) ']) 



#these are trial word counts to be used in the preselect query
wcount = max(1,len(tcomb.split()))
ccount = max(1,len(newcond))
icount = max(1,len(intset))
wperc = 0.25 * wcount
cperc = 0.25 * ccount
iperc = 0.27 * icount
wpercs1 = 0.32 * len(tcomb.split())


rks_base = []

#this is the big preselect query, the else runs the pubmed query instead
#if trialquery == 'P':
#  #look to see if the candidate set has been precomputed
#  mpre = ''' select PMID, wcount,ccount,icount from 
#  clinicaltrials.trial_cand 
#  where NCT_num = "{0}";'''
#  dbcursor.execute(mpre.format(trial))
#  rks_base = dbcursor.fetchall()
#  #get potential candidates from the base article list
#  if len(rks_base) == 0:
#    mtest = '''select PMID, {1} as c1, {2} as c2, {3} as c3 from 
#    (select a.PMID, a.searchwords from clinicaltrials.pub_words a where a.pubdate > {0} and ({1}) > 0 ) b  ;'''
#    dbcursor.execute(mtest.format(y,c1where,c2where,c3where))
#    for r in dbcursor:
#      if (r[2]>0 and r[3]> 0) or r[2]>2 or r[3]>3 or r[1]>wpercs1:
#        rks_base.append(r)
#  #get potential candidates from the new article list
#  mtest = '''select PMID, {1} as c1, {2} as c2, {3} as c3 from 
#    (select a.PMID, a.searchwords from clinicaltrials.pub_words_new a where a.pubdate > {0} and ({1}) > 0 ) b  ;'''
#  dbcursor.execute(mtest.format(y,c1where,c2where,c3where))
#  for r in dbcursor:
#    if (r[2]>0 and r[3]> 0) or r[2]>2 or r[3]>3 or r[1]>wpercs1:
#      rks_base.append(r)
#  rdf = pd.DataFrame(rks_base,columns=['PMID','wcount','ccount','icount'])
#  if len(rdf) > 5000:
#    rdf['pavg'] = rdf.apply(lambda row: ((row['wcount']/wcount)+(row['ccount']/ccount)+(row['icount']/icount))/3,axis=1)
#    rdftop = rdf.sort_values(['pavg','PMID'],ascending=[False,False]).copy().head(5000)
#    pmgovlist = [str(v) for v in rdftop['PMID']]
#  else:
#    pmgovlist = [str(v) for v in rdf['PMID'] ]
#  rdf = None
#  rdf_top = None
#  rks_base = None
#else:
#  pubmedstr = urllib.parse.quote_plus(trialquery)
#  urlbase = 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&id=anne2001&retmode=JSON&retmax=100000&term='
#  r=requests.get(urlbase + pubmedstr )
#  xt = json.loads(r.text)
#  pmgovlist = xt['esearchresult']['idlist']
#  pmgovlist = pmgovlist[:100000]


with open('dataset_09.pkl', 'rb') as f:
  pmgovlist = pickle.load(f)



############################################# prepare article dates

#get a true date version of the publication dates
selectSQL = """select PMID, STR_TO_DATE(CONCAT(CONVERT(b.year,CHAR(4)),'-',
    (CASE WHEN b.month = '-' THEN 'jun' 
          WHEN b.month = '01' THEN 'jan' 
          WHEN b.month = '02' THEN 'feb' 
          WHEN b.month = '03' THEN 'mar' 
          WHEN b.month = '04' THEN 'apr' 
          WHEN b.month = '05' THEN 'may' 
          WHEN b.month = '06' THEN 'jun' 
          WHEN b.month = '07' THEN 'jul' 
          WHEN b.month = '08' THEN 'aug' 
          WHEN b.month = '09' THEN 'sep' 
          WHEN b.month = '1' THEN 'jan' 
          WHEN b.month = '10' THEN 'oct' 
          WHEN b.month = '11' THEN 'nov' 
          WHEN b.month = '12' THEN 'dec' 
          WHEN b.month = '2' THEN 'feb' 
          WHEN b.month = '3' THEN 'mar' 
          WHEN b.month = '4' THEN 'apr' 
          WHEN b.month = '5' THEN 'may' 
          WHEN b.month = '6' THEN 'jun' 
          WHEN b.month = '7' THEN 'jul' 
          WHEN b.month = '8' THEN 'aug' 
          WHEN b.month = '9' THEN 'sep' 
          WHEN b.month = 'autumn' THEN 'oct' 
          WHEN b.month = 'fall' THEN 'oct' 
          WHEN b.month = 'october' THEN 'oct' 
          WHEN b.month = 'spring' THEN 'apr' 
          WHEN b.month = 'summer' THEN 'jul' 
          WHEN b.month = 'winter' THEN 'jan' 
      ELSE b.month END)
   ,'-15'), '%Y-%b-%d') as pubdate 
FROM PUBMED2015.Articles b 
WHERE b.PMID in ({0}) and b.languages like '%eng%'  ;
"""

#ptups = []
#for chunk in chunker(pmgovlist,100):
#  dbcursor.execute(selectSQL.format(','.join(chunk)))
#  ptups.extend(dbcursor.fetchall())

#for chunk in chunker(list(passureT),100):
#  dbcursor.execute(selectSQL.format(','.join([str(v) for v in chunk])))
#  ptups.extend(dbcursor.fetchall())


with open('dataset_10.pkl', 'rb') as f:
  ptups = pickle.load(f)



nset = pd.DataFrame(ptups)
nset.columns = (['PMID','pubdate'])
nset['NCT_num'] = trial
nset['trial_start'] = trialstart
nctlist = list(set(nset['NCT_num']))
pmidlist = list(set(nset['PMID']))

statmsg += '<b>' + str(len(pmidlist)) + ' articles and linked articles have complete data on this server.</b><BR>'
outstat(statmsg,job)




############  keeping this because it computes number of article links
#full list of nct linked articles and pmids
selectSQL = """select PMID,1 as ctindex, NCT_num, 1 as Nout, SRC
FROM clinicaltrials.cleaned_links 
WHERE NCT_num in ({0})
   ;
"""


linkdict = {}
#dbcursor.execute(selectSQL.format(','.join(['"' + v + '"' for v in nctlist])))     
#rks = dbcursor.fetchall()

with open('dataset_11.pkl', 'rb') as f:
  rks = pickle.load(f)


if len(rks)>0:
  linklist = pd.DataFrame(rks,columns = ['PMID','ctindex','NCT_num','Nout','SRC'])
  linklist['PMIDc'] = linklist.apply(lambda row: str(row['PMID']), axis=1)
  linklist2 = linklist.loc[linklist['Nout']==1].copy()
  linkgroup = linklist2.groupby(['NCT_num'])['PMIDc'].apply(','.join).reset_index()
  linkgroup.columns=['NCT_num','links']
  for index, row in linkgroup.iterrows():
    if row['NCT_num'] not in linkdict.keys():
      linkdict[row['NCT_num']] = row['links']
  nset = nset.merge(linkgroup,left_on='NCT_num',right_on='NCT_num', how='left')
  pmidlist.extend(linklist2['PMID'])

pmidlist = list(set(pmidlist))



#have at least one article in the list to prevent crashing
if len(pmidlist) ==0:
  pmidlist = [32367616]




#############################################
######################## preload Article Data
#############################################
alltrialdict = {}
trialfeat = set(flistdf['featnum'])
trialfeat.discard('FTA')
trialfeat.discard('F03')

#instantiate all the dictionary elements
for n in nctlist:
  alltrialdict[n] = {}
  for f in trialfeat:
    alltrialdict[n][f] = ''


xsql = '''select a.NCT_num, group_concat(a.value separator "|") from clinicaltrials.clinicaltrials a 
 where a.xmlpathid in 
 (select b.xmlpathid from clinicaltrials.xmlpaths b where b.xmlpath in ({0}) )
 and a.NCT_Num in ({1}) 
 group by a.NCT_num;  '''


#for f in trialfeat:
#    for chunk in chunker(nctlist,500):
#      selectSQL = xsql.format(','.join(['"' + v + '"' for v in ctxpath[f]]) ,','.join(['"' + v + '"' for v in chunk])  )
#      dbcursor.execute(selectSQL)     
#      for resrow in dbcursor:
#        alltrialdict[resrow[0]][f] = resrow[1]

with open('dataset_12.pkl', 'rb') as f:
  alltrialdict = pickle.load(f)


for n in alltrialdict.keys():
  ttemp = ' '.join([suppressWords(v) for v in caps.findall(alltrialdict[n]['F13']) ] ) + ' ' + alltrialdict[n]['F02'] + ' ' +alltrialdict[n]['F27'] + ' '+alltrialdict[n]['F30'] + ' '
  alltrialdict[n]['FTA'] = set(caps.findall(ttemp))
  alltrialdict[n]['F03'] = n





############ make main article dataframe now in order to launch text sim early
statmsg += '<b>Processing article titles and authors.</b><br>'
outstat(statmsg,job)
progress = '{0} article titles and authors processed out of ' + str(len(pmidlist))

articletups = []

aSQL = '''SELECT a.PMID, a.authors, a.affiliation, a.email, a.title, a.grants, a.mesh, b.abstract from PUBMED2015.Articles a 
  left outer join PUBMED2015.Abstracts b
  ON a.PMID=b.PMID
  WHERE a.PMID in ('''

#for chunk in chunker(pmidlist,500):
#  selectSQL = aSQL + ','.join([str(v) for v in chunk]) + ') ;'
#  dbcursor.execute(selectSQL)
#  articletups.extend(dbcursor.fetchall())

with open('dataset_13.pkl', 'rb') as f:
  articletups = pickle.load(f)



foundlist = []
for t in articletups:
  foundlist.append(t[0])

for v in pmidlist:
  if int(v) not in foundlist:
    articletups.append((int(v),'-','-','-','-','-','-','-'))


article_rows = pd.DataFrame(articletups,columns = ['PMID','authors','affiliation','email', 'title','grants','mesh','abstract'])
articletups = []


#These files are processed with the text similarity model described in:
#http://arrowsmith.psych.uic.edu/arrowsmith_uic/word_text_summary.html



##################   file output for trials
#outf = open(cachetest + 'trialdata' + job + '.tsv',"w",errors='ignore') 

#for t in alltrialdict.keys():
#  #add the paragraph vectors to a text file
#  feat = 'F13'
#  vecchunk = alltrialdict[t][feat].replace('\t','').replace('\n','')
#  _ = outf.write(t + '\t' + feat + '\t' + vecchunk + '\n')

#outf.close()



##################### output title + abs for text similarity scoring
#statmsg += '<b>Appplying <a href="http://arrowsmith.psych.uic.edu/arrowsmith_uic/word_text_summary.html" target="_blank">text similarity model</a> in the background.</b><br>'
#outstat(statmsg,job)

#outf = open(cachetest + 'pubdata' + job + '.tsv',"w",errors='ignore') 

#for ai, ar in article_rows.iterrows():
#  ostr = ' '
#  if ar['title'] is not None:
#    ostr += ar['title']
#  if ar['abstract'] is not None:
#    ostr += ' ' + ar['abstract']
#  vecchunk = ostr.replace('\t','').replace('\n','').strip()
#  _ = outf.write(str(ar['PMID']) + '\t' + vecchunk + '\n')

#outf.close()

#tcmd = "./simscore_batchjob.pl "
#if amtesting == 'Y':
#  tcmd = "./simscore_batchjobT.pl "

#process = subprocess.Popen([tcmd+job], shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, bufsize=1)




############################ related articles ranking

statmsg += '<b>Retrieving related article rankings from PubMed.gov</b><BR>'
outstat(statmsg,job)
progress = '{0} article rankings retrieved out of ' + str(len(pmidlist)) 

#preload all of the related article rankings
rankrack={}
pc=0

pset = [str(v) for v in pmidlist]
pc = 0
insdate = datetime.datetime.now().strftime("%Y-%m-%d")


#function to repeatedly request data from PubMed
def ranksTrying(pset):
  tries=0
  good=0
  ubase = 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi'
  postobj = {'db':'pubmed','api_key':'','linkname':'pubmed_pubmed','retmode':'JSON','id':chunk}
  racktemp={}
  while good==0 and tries<10:
    try:
      x = requests.post(ubase,postobj)
      xt = json.loads(x.text)
      for xl in xt['linksets']:
        p = xl['ids'][0]
        ptemp = {}
        x2 = xl['linksetdbs'][0]['links']
        for i in range(0,len(x2)):
          if i < 4000:
            ptemp[x2[i]] = i +1
        racktemp[int(p)] = ptemp
      good=1
    except:
      tries+=1
      time.sleep(5)
  return racktemp



#for chunk in chunker(pset,500):
#  rt = ranksTrying(chunk)
#  for r in rt.keys():
#    rankrack[r] = rt[r]
#  pc += len(chunk)
#  outstat(statmsg + progress.format(str(pc)),job)


with open('dataset_14.pkl', 'rb') as f:
  rankrack = pickle.load(f)








########################## Feature 02 authority table - faster lookup method with dictionaries
statmsg += '<b>Retrieving <a href="http://arrowsmith.psych.uic.edu/arrowsmith_uic/author2.html" target="_blank">Author-ity</a> clusters.</b><br>'
outstat(statmsg,job)
progress = '{0} Author-ity clusters searched out of ' + str(len(pmidlist)) 


AuthorityRef = {}
AuthorityDetails = {}
pc=0
#for i in pmidlist:
#  selectSQL = 'select rowid, c1, ccount from PUBMED2015.Authorityclust where (MATCH(clist) AGAINST("' + str(i) + '_*" IN BOOLEAN MODE)) ; '
#  dbcursor.execute(selectSQL)
#  table_rows = dbcursor.fetchall()
#  #table_rows = []
#  templist = []
#  for row in table_rows:
#    templist.append(row[0])
#    if row[0] not in AuthorityDetails.keys():
#      AuthorityDetails[row[0]] = row
#  AuthorityRef[i] = templist
#  pc+=1
#  if pc%500 == 0:
#    outstat(statmsg + progress.format(str(pc)),job)

with open('dataset_16.pkl', 'rb') as f:
  AuthorityRef = pickle.load(f)

with open('dataset_17.pkl', 'rb') as f:
  AuthorityDetails= pickle.load(f)



########################## Feature 02-09 - raw data table
#finish processing the article_rows table
pc=0
#make all cap versions of title to speed up Feature 11
article_rows['titleCaps'] = ''
article_rows['titleCapsNoAdam'] = ''
sw = set(s for s in stoplist)
for index, row in article_rows.iterrows():
  lv = set([suppressWords(s.strip()) for s in caps.findall(row['title'])])
  lv.discard('remove')
  article_rows.at[index,'titleCapsNoAdam'] = lv - sw
  lv = set([s.strip() for s in caps.findall(row['title'])])
  lv.discard('remove')
  article_rows.at[index,'titleCaps'] = lv - sw
  pc += 1
  if pc % 500 == 0:
    outstat(statmsg + progress.format(str(pc)),job)


article_rows.set_index(['PMID'],inplace=True)

article_rows['authors2'] = article_rows.apply(lambda row: expandAuth(row['authors']),axis=1)





########################## Feature 10 - shared substances
statmsg += '<b>Processing article chemicals and accession numbers.</b><br>'
outstat(statmsg,job)


chem_dict = {}

idSQL = "select group_concat(a.eirid separator ',') from pubmed.eir a where a.hs like '%Chemical/NameOfSubstance';"
#dbcursor.execute(idSQL)
#ids = dbcursor.fetchall()
#idst = ids[0][0]

with open('dataset_18.pkl', 'rb') as f:
  idst = pickle.load(f)



selectSQL = """SELECT a.aid, group_concat(a.val separator '|') as chemicals
FROM pubmed.aelement a
WHERE a.eirid in ({1})
AND
a.aid in ({0}) 
GROUP BY a.aid; 
"""

#for chunk in chunker(pmidlist,500):
#  dbcursor.execute(selectSQL.format(','.join([str(v) for v in chunk]),idst))     
#  for r in dbcursor:
#      chem_dict[r[0]] = r[1]


with open('dataset_19.pkl', 'rb') as f:
  chem_dict = pickle.load(f)


foundlist = list(chem_dict.keys())
for v in pmidlist:
    if int(v) not in foundlist:
      chem_dict[int(v)] = ''



########################## Feature 10 - accession numbers
acc_dict = {}

idSQL = "select group_concat(a.eirid separator ',') from pubmed.eir a where a.hs like '%/AccessionNumberList/AccessionNumber%';"
#dbcursor.execute(idSQL)
#ids = dbcursor.fetchall()
#idst = ids[0][0]

with open('dataset_20.pkl', 'rb') as f:
  idst = pickle.load(f)


selectSQL = """SELECT a.aid, group_concat(a.val separator '|') as accessionn
FROM pubmed.aelement a
WHERE a.eirid in ({1})
AND
a.aid in ({0}) 
GROUP BY a.aid; 
"""

#for chunk in chunker(pmidlist,500):
#  dbcursor.execute(selectSQL.format(','.join([str(v) for v in chunk]),idst))     
#  for r in dbcursor:
#      acc_dict[r[0]] = r[1]


with open('dataset_21.pkl', 'rb') as f:
  acc_dict = pickle.load(f)


foundlist = list(acc_dict.keys())
for v in pmidlist:
    if int(v) not in foundlist:
      acc_dict[int(v)] = ''





########################## Feature 10 - collective names
col_dict = {}

idSQL = "select group_concat(a.eirid separator ',') from pubmed.eir a where a.hs like '%CollectiveName%';"
#dbcursor.execute(idSQL)
#ids = dbcursor.fetchall()
#idst = ids[0][0]

with open('dataset_22.pkl', 'rb') as f:
  idst = pickle.load(f)


selectSQL = """SELECT a.aid, group_concat(a.val separator '|') as accessionn
FROM pubmed.aelement a
WHERE a.eirid in ({1})
AND
a.aid in ({0}) 
GROUP BY a.aid; 
"""

#for chunk in chunker(pmidlist,500):
#  dbcursor.execute(selectSQL.format(','.join([str(v) for v in chunk]),idst))     
#  for r in dbcursor:
#      col_dict[r[0]] = r[1]


with open('dataset_23.pkl', 'rb') as f:
  col_dict = pickle.load(f)


foundlist = list(col_dict.keys())
for v in pmidlist:
    if int(v) not in foundlist:
      col_dict[int(v)] = ''





########################## Feature 12 - title cn all caps
statmsg += '<b>Processing article titles and abstracts.</b><br>'
outstat(statmsg,job)
progress = '{0} titles and abstracts processed out of ' + str(len(pmidlist)) 


abs_rowdict={}
abs_fulldict = {}
pc=0



#add a consolidated all caps set for title and abastracts
article_rows['ta_allcaps'] = article_rows.apply(lambda row: set(), axis=1)

for index, row in article_rows.iterrows():
  if row['abstract'] is not None:
    temp = set([suppressWords(s.strip()) for s in caps.findall(row['abstract'])])
    temp.discard('remove')
    abs_rowdict[index] = temp
    abs_fulldict[index] = row['abstract']
  else:
    abs_rowdict[index] = set()
    abs_fulldict[index] = ''
  article_rows.at[index,'ta_allcaps'] = (row['titleCaps'] | abs_rowdict[index])






################# run text similarity model
progress = '{0} text similarities calculated out of ' + str(len(pmidlist)) 

tnow = datetime.datetime.now()
timelock = (tnow-basestart).total_seconds() 
tsimlen = max(1,len(pmidlist))


#if process.poll() is None:
#  while True:
#    nextline = process.stdout.readline()
#    if process.poll() is not None:
#      break
#    t=nextline.decode()
#    pc = t.split(' ')[0]
#    if int(pc) % 100 == 0:
#      if esttime == 600:
#        esttime = timelock + (tsimlen-int(pc))*0.11
#      outstat(statmsg + progress.format(pc),job)
#    sys.stdout.flush()
#  output = process.communicate()[0]
#  exitCode = process.returncode
#else:
#  statmsg += 'Text similarity complete.'
#  outstat(statmsg ,job)

#simdat = pd.read_csv(cachetest + 'simout'+job+'.tsv',sep='\t',header=None)
#simdat.columns = ['PMID','NCT_num','feat','result']

with open('dataset_24.pkl', 'rb') as f:
  simdat = pickle.load(f)


#using a dictionary here with concatenated keys for speed during model application
simdict = {}
for index, row in simdat.iterrows():
  simdict[str(row['PMID'])+':'+row['NCT_num']+':'+row['feat']] = decode_sims(row['result'])




#################################################
# model application loop
#################################################
mod2 = {}

tttu = time.time()

timedict = {}
timedict['pmiddat'] = []
timedict['ctvalues'] = []

progress = '{0} model scores computed out of ' + str(len(pmidlist))


for mainindex,mainrow in nset.iterrows():
  ttu = time.time()
  trialdict =  alltrialdict[mainrow['NCT_num']]
  timedict['ctvalues'].append(((time.time()-ttu)*1000))
  newrow = modelform.copy()
  newrow.at[0,'sample_id'] = 0
  newrow.at[0,'NCT_num'] = mainrow['NCT_num']
  newrow.at[0,'PMID'] = mainrow['PMID']
  newrow.at[0,'posneg'] = 0
  newrow.at[0,'testcontrol'] = 0
  ttu = time.time()
  pdat = {}
  pdat['title'] = article_rows.at[mainrow['PMID'],'title']
  pdat['mesh'] = article_rows.at[mainrow['PMID'],'mesh']
  pdat['grants'] = article_rows.at[mainrow['PMID'],'grants']
  pdat['email'] = article_rows.at[mainrow['PMID'],'email']
  pdat['chemicals'] = chem_dict[mainrow['PMID']]
  pdat['authors'] = article_rows.at[mainrow['PMID'],'authors']
  pdat['accession'] = acc_dict[mainrow['PMID']]
  pdat['affiliation'] = article_rows.at[mainrow['PMID'],'affiliation']
  pdat['abstract'] = abs_fulldict[mainrow['PMID']]
  pdat['cn'] = col_dict[mainrow['PMID']]
  pdat['pmid'] = str(mainrow['PMID'])
  pdat['titleabs'] = pdat['title'] + ' ' + pdat['abstract']
  pdat['ta_allcaps'] = article_rows.at[mainrow['PMID'],'ta_allcaps']
  timedict['pmiddat'].append(((time.time()-ttu)*1000))
  for index, row in flistdf.iterrows():
    ctvalue = trialdict[row['featnum']]
    for p in row['pubmedlist']:
      for m in row['methodlist']:
        ttu = time.time()
        if m in ['tsim','rksimax']:
          simkey = str(mainrow['PMID'])+':'+mainrow['NCT_num']+':'+row['featnum']
          if simkey in simdict.keys():
            compdict = simdict[simkey]
            newrow.at[0,row['featnum']+'_'+p+'_'+m+'unw_tms'] = compdict['unw_tms']
            newrow.at[0,row['featnum']+'_'+p+'_'+m+'unw_scr'] = compdict['unw_scr']
            newrow.at[0,row['featnum']+'_'+p+'_'+m+'w_tms'] = compdict['w_tms']
            newrow.at[0,row['featnum']+'_'+p+'_'+m+'w_scr'] = compdict['w_scr']
          else:
            newrow.at[0,row['featnum']+'_'+p+'_'+m+'unw_tms'] = np.nan
            newrow.at[0,row['featnum']+'_'+p+'_'+m+'unw_scr'] = np.nan
            newrow.at[0,row['featnum']+'_'+p+'_'+m+'w_tms'] = np.nan
            newrow.at[0,row['featnum']+'_'+p+'_'+m+'w_scr'] = np.nan
        else:
          pmvalue = pdat[p]
          pmObj = getattr(sys.modules[__name__], 'comp_' + m )
          if (len(ctvalue) < 2 or len(pmvalue) < 2) and m != 'mismatch':
            compval = np.nan
          else:
            if m == 'mismatch':
              compval = pmObj(ctvalue,pmvalue)
            else:
              compval = pmObj(ctvalue.encode('ascii',errors='ignore').decode('ascii'),pmvalue)
          if m in ['occurance','propoccurance']:
            if np.isnan(compval):
              compval=0
            else:
              if compval > 0:
                compval = .5
              else:
                compval = -.5
          newrow.at[0,row['featnum']+'_'+p+'_'+m] = compval
        if m not in timedict.keys():
            timedict[m] = [((time.time()-ttu)*1000)]
        else:
            timedict[m].append(((time.time()-ttu)*1000))
        #print(datetime.datetime.now())
  if mainrow['trial_start'] is None:
    newrow.at[0,'startdate_lift'] = 0
    newrow.at[0,'start_diff_lt5'] = 0
    newrow.at[0,'start_diff_cast'] = 0
  else:
    dt = np.floor((mainrow['pubdate']-mainrow['trial_start']).days/365)
    if dt > -1 and dt < 17:
      newrow.at[0,'startdate_lift'] = startlift[dt]
    else:
      if dt < 0:
        newrow.at[0,'startdate_lift'] = 0
      else:
        newrow.at[0,'startdate_lift'] = 1
    if dt < 5 and dt> -1:
      newrow.at[0,'start_diff_lt5'] = 1
    else:
      newrow.at[0,'start_diff_lt5'] = 0
    newrow.at[0,'start_diff_cast'] = 0
  atemp = max(np.nan_to_num([newrow.at[0,'F01_authors_authtree'],newrow.at[0,'F31_authors_authtree'],newrow.at[0,'F36_authors_authtree']]))
  newrow.at[0,'authcomp'] = atemp
  arounded = round(min(900,max(0,atemp))/100,0)
  newrow.at[0,'agginvint'] = max(aggdict[arounded],newrow.at[0,'F03_pmid_aggmax'])
  score = float(MOD_intercept)
  for v in MOD.keys():
    #print(v,newrow.at[0,v])
    if pd.isnull(newrow.at[0,v]):
      newrow.at[0,v] = MODsub[v]
      score += MODsub[v]*MOD[v]
    else:
      score += float(newrow.at[0,v])*MOD[v]
  newrow.at[0,'prob'] =(np.exp(score) / (1 + np.exp(score)))
  if len(searchlinks)==1 and mainrow['PMID'] in searchlinks:
    newrow.at[0,'prob'] = 1
  #modeldata = modeldata.append(newrow)
  t=newrow.values.tolist()
  mod2[mainrow['NCT_num'] + ':'+str(mainrow['PMID'])] = t[0]
  if mainindex%500 == 0:
    #modeldata.to_csv('scoringpartial.csv')
    outstat(statmsg + progress.format(str(mainindex)),job)
    #print(mainindex,len(nset))



modeldata = pd.DataFrame.from_dict(mod2,orient='index',columns=newrow.columns)


#mark articles if they are NCT linked or in the Trial Results
modeldata['Trial Results'] = modeldata.apply(lambda row: 'yes' if row['PMID'] in searchres else 'no', axis=1)
modeldata['NCT Link'] = modeldata.apply(lambda row: 'yes' if row['PMID'] in searchlinks else 'no', axis=1)
modeldata['rank'] = -1
i=0

#make rank integers
for index,row in modeldata.sort_values(['prob'],ascending=False).iterrows():
  i+=1
  modeldata.at[index,'rank'] = i

#filter by rank or link status
modeldata['keep'] = modeldata.apply(lambda row: 1 if row['rank']<11 or row['prob'] > 0.7999 else 0, axis=1)
modeldata['keep'] = modeldata.apply(lambda row: 1 if row['Trial Results']=='yes' or row['NCT Link'] == 'yes' else row['keep'], axis=1)


modeldata.sort_values(['prob'],ascending=[False]).to_csv("scoreout_NCT03745053_data.csv")


