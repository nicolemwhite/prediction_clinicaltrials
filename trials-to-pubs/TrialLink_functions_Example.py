# -*- coding: utf-8 -*-
#Created on Thu Aug  1 10:43:18 2019
#@author: Arthur Holt


#######################  feature functions
#edit distance
def levenshtein(s, t):
        ''' From Wikipedia article; Iterative with two matrix rows. '''
        if s == t: return 0
        elif len(s) == 0: return len(t)
        elif len(t) == 0: return len(s)
        v0 = [None] * (len(t) + 1)
        v1 = [None] * (len(t) + 1)
        for i in range(len(v0)):
            v0[i] = i
        for i in range(len(s)):
            v1[0] = i + 1
            for j in range(len(t)):
                cost = 0 if s[i] == t[j] else 1
                v1[j + 1] = min(v1[j] + 1, v0[j + 1] + 1, v0[j] + cost)
            for j in range(len(v0)):
                v0[j] = v1[j]
                
        return v1[len(t)]


#functions to retrieve pubmed data
#these have been kept separate and literal in case there's a future need to 
#do special treatments to each field, or get them from another source
def get_title(pmID):
  global dbcursor
  sqlselect = "select title from PUBMED2015.Articles where PMID = " + str(pmID)
  dbcursor.execute(sqlselect)
  table_rows = dbcursor.fetchall()
  if len(table_rows) == 0:
    return ''
  else:
    ot = ' '.join(table_rows[0]).encode('ascii',errors='ignore')
    return ot.decode('ascii')

def get_mesh(pmID):
  global dbcursor
  sqlselect = "select mesh from PUBMED2015.Articles where PMID = " + str(pmID)
  dbcursor.execute(sqlselect)
  table_rows = dbcursor.fetchall()
  if len(table_rows) == 0:
    return ''
  else:
    ot = ' '.join(table_rows[0]).encode('ascii',errors='ignore')
    return ot.decode('ascii')

def get_grants(pmID):
  global dbcursor
  sqlselect = "select grants from PUBMED2015.Articles where PMID = " + str(pmID)
  dbcursor.execute(sqlselect)
  table_rows = dbcursor.fetchall()
  if len(table_rows) == 0:
    return ''
  else:
    ot = ' '.join(table_rows[0]).encode('ascii',errors='ignore')
    return ot.decode('ascii')

def get_email(pmID):
  global dbcursor
  sqlselect = "select email from PUBMED2015.Articles where PMID = " + str(pmID)
  dbcursor.execute(sqlselect)
  table_rows = dbcursor.fetchall()
  if len(table_rows) == 0:
    return ''
  else:
    ot = ' '.join(table_rows[0]).encode('ascii',errors='ignore')
    return ot.decode('ascii')

def get_chemicals(pmID):
  global dbcursor
  sqlselect = "select chemicals from PUBMED2015.features where PMID = " + str(pmID)
  dbcursor.execute(sqlselect)
  table_rows = dbcursor.fetchall()
  if len(table_rows) == 0:
    return ''
  else:
    ot = ' '.join(table_rows[0]).encode('ascii',errors='ignore')
    return ot.decode('ascii')



def raw(text):
    new_string=''
    for char in text:
        try: 
            new_string += escape_dict[char]
        except KeyError: 
            new_string += char
    return new_string

#function to decode raw author names
#function to decode raw author names
def decodeAuth(inStr):
  tmp = raw(inStr).strip().replace('\\','|')
  alist = ['-','-','-','-','-']
  if len(tmp) > 2:
    alist = tmp.split('|')
    if len(alist) > 2:
      aadd = alist[0]+'_'+alist[2]
      alist.append(aadd.upper())
  return alist


#replaced with a (hopefully) faster verson
def getAuthority(lPMID,rPMID):
  global AuthorityRef, AuthorityDetails
  resdf = ''
  if str(lPMID) in AuthorityRef.keys() and str(rPMID) in AuthorityRef.keys():
    lset = AuthorityRef[str(lPMID)]
    rset = AuthorityRef[str(rPMID)]
    cset = (set(lset) & set(rset))
    for r in cset:
      if r in AuthorityDetails.keys():
        #resdf = resdf.append({'c1':AuthorityDetails[r][1], 'ccount':AuthorityDetails[r][2]}, ignore_index=True)
        resdf += ',' + AuthorityDetails[r][1]
  return resdf.upper()


def compNick(name1,name2):
  res = 0
  for key in nickdict:
    if name1.upper() in key and name2.upper() in nickdict[key]:
      res = 1
    if name2.upper() in key and name1.upper() in nickdict[key]:
      res = 1
  return res

def altComp(lrow,rrow):
  name_1 = lrow[1].upper().strip()
  name_2 = rrow[1].upper().strip()
  res = -0.00
  #1.
  if lrow[0].upper().strip() != rrow[0].upper().strip():
    return 0
  #2.
  if len(name_1)>1 and len(name_2)>1:
    #2.a
    if name_1 == name_2:
      return 100
    if '-' in name_1 or '-' in name_2:
      #2.b
      if name_1.replace('-',' ') == name_2.replace('-',' '):
        return 43.9
      #2.c and #2.d
      if name_1[-2:][:1] == '-' or name_2[-2:][:1] == '-':
        if name_1 in name_2 or name_2 in name_1:
          return 43.9
      #2.e
      if name_1 in name_2 or name_2 in name_1:
        return 2.7
    #2.f
    if compNick(name_1,name_2) == 1:
      return 0.56
    #2.g
    if levenshtein(name_1, name_2) == 1:
      return 0.21
    #2.h
    if name_1 == name_2[:len(name_1)] or name_2 == name_1[:len(name_2)]:
      if len(name_1) > 2 and len(name_2) > 2:
        return 0.14
      #2.i
      if len(name_1) == 2 or len(name_2) == 2:
        return 0.34
    #2.j
    if name_1 == ''.join(fletter.findall(name_2)) or name_2 == ''.join(fletter.findall(name_1)):
      return 0.13 
  else:
    #3.a
    if lrow[2] == rrow[2] and lrow[2] != '-':
      res += 0.84
    #3.b
    if lrow[3] == rrow[3] and lrow[3] != '-':
      res += 4.92
    #3.c
    if lrow[3] != rrow[3] and lrow[3] != '-' and rrow[3] != '-':
      res -= 4.16 
  return res

#function to parse both sets of authors and compare
#using pandas dataframes, dictionaries would probably be faster
def compAuth(leftlist,leftPMID,rightlist,rightPMID):
  score = float(0)
  adffast = getAuthority(leftPMID,rightPMID)
  #print(adffast)
  #adffast = ','.join([v.upper() for v in adf['c1']])
  #adffast = '-'
  #faster way - single scan
  for leftrow in leftlist:
    if leftrow[5] in adffast and len(leftrow[5])>1:
      #lvect2.loc[leftindex,6] = 'A' 
      score+=100
    else:
      mscore = float(0)
      tn = leftrow[0].upper()
      for rightrow in rightlist:
        #print(leftrow,rightrow)
        #print('\n')
        #print(altComp(list(leftrow),list(rightrow)))
        mscore = max([mscore,altComp(list(leftrow),list(rightrow))])
      score += mscore
      #lvect2.loc[leftindex,7] = mscore
  return score


def compAuthAltOLD(leftList,rightList):
  if leftList=='-' or rightList == '-':
    return pd.DataFrame([['-',0]], columns=[6,7])
  score = 0
  lvect = leftList.split('|')
  rvect = rightList.split('|')
  lvect2 = pd.DataFrame([decodeAuth(c) for c in lvect])
  rvect2 = pd.DataFrame([decodeAuth(c) for c in rvect])
  lvect2[5] = lvect2.apply(lambda row: row[0]+'_'+row[2],axis=1)
  rvect2[5] = rvect2.apply(lambda row: row[0]+'_'+row[2],axis=1)
  lvect2[6] = '-'
  rvect2[6] = '-'
  lvect2[7] = 0
  rvect2[7] = 0
  for leftindex, leftrow in lvect2.iterrows():
    mscore = -5000
    for rightindex, rightrow in rvect2.iterrows():
      score = altComp(list(leftrow),list(rightrow))
      if score > mscore and score != 0:
        mscore = score
    if mscore == -5000:
      mscore = 0
    lvect2.loc[leftindex,7] = mscore
  #print(lvect2)
  #print(rvect2)
  return lvect2



def compAuthAlt(leftList,rightList):
  if len(leftList) < 2 or len(rightList) < 1:
    return 0
  score = float(0)
  lvect = [decodeAuth(c) for c in leftList.split('|')]
  rvect = [decodeAuth(c) for c in rightList.split('|')]
  for leftrow in lvect:
      mscore = float(0)
      tn = leftrow[0].upper()
      for rightrow in rvect:
        #print(leftrow,rightrow)
        mscore = max([mscore,altComp(leftrow,rightrow)])
      score += mscore
      #lvect2.loc[leftindex,7] = mscore
  return score


#function to compare affiliations
def compAff(leftStr,rightStr):
  lv = set([s.strip().upper() for s in re.sub(r'[^a-zA-Z0-9, ]+', '', leftStr).split(',')])
  rv = set([s.strip().upper() for s in re.sub(r'[^a-zA-Z0-9, ]+', '', rightStr).split(',')])
  return len(lv & rv )

#remove superfluous period at the end of emails
def deperiod(em):
  if em[-1]=='.':
    return em[:-1]
  else:
    return em

#function to process email strings for matching
def compEmail(leftStr,rightStr):
  if len(leftStr) < 4 or len(rightStr) < 4:
    return 0
  else:
    lv = set([deperiod(s.strip().upper()) for s in re.sub(r'[^a-zA-Z0-9| .@]+', '', leftStr).split('|')])
    rv = set([deperiod(s.strip().upper()) for s in re.sub(r'[^a-zA-Z0-9| .@]+', '', rightStr).split('|')])
    return len(lv & rv )


def comp_Emailaddr(leftStr, rightStr):
  lst = re.findall(r'\S+@\S+', leftStr)
  rst = rightStr.split('|')
  rs2 = set([v.upper() for v in rst])
  ls2 = set([v.upper() for v in lst]) 
  return len(rs2 & ls2)


#email domain comparisons
def getdomain(em):
  if em[-1]=='.':
    tmp = em[:-1]
  else:
    tmp = em
  tmp = re.search("@[\w.]+", tmp)
  if tmp is not None:
    tmp2 = tmp.group()[-3:].upper()
    #print(tmp.group())
    if tmp2[-3:] == 'COM':
      return tmp.group()
    else:
      return ''
  else:
    return ''

def getdomainD(em):
  if em[-1]=='.':
    tmp = em[:-1]
  else:
    tmp = em
  tmp = re.search("@[\w.]+", tmp)
  if tmp is not None:
    tmp2 = tmp.group()[-3:].upper()
    #print(tmp.group())
    return tmp.group()
  else:
    return ''



#function to process emails for matching
def compEmailD(leftStr,rightStr):
  if len(leftStr) < 4 or len(rightStr) < 4:
    return 0
  else:
    lv = set([getdomainD(s.strip().upper()) for s in re.sub(r'[^a-zA-Z0-9| .@]+', '', leftStr).split('|')])
    rv = set([getdomainD(s.strip().upper()) for s in re.sub(r'[^a-zA-Z0-9| .@]+', '', rightStr).split('|')])
    lv.discard('')
    rv.discard('')
    #print(list(lv))
    return len(lv & rv )


#function to process countries for matching
def compCountry(leftStr,rightStr):
  if len(leftStr) < 2 or len(rightStr) < 2:
    return 0
  else:
    lset = set()
    rset = set()
    for key in countries:
      if key in leftStr:
        lset.add(countries[key])
      if key in rightStr:
        rset.add(countries[key])
    if len(lset & rset) > 0:
      return 1
    else:
      return 0


#function to process grants for matching
def compGrants(leftStr,rightStr):
  lv = set([s.strip().upper() for s in leftStr.split('|')])
  rv = set([s.strip().upper() for s in rightStr.split('|')])
  return len(lv & rv )

#function to process sub for matching
def compSUB(leftStr,rightStr):
  lv = set([s.strip().upper() for s in leftStr.split('|')])
  rv = set([s.strip().upper() for s in rightStr.split('|')])
  return len(lv & rv )


#function to remove stop words and ADAM abbrevations
def suppressWords(inWord):
  result = inWord.upper()
  if result in stoplist:
    result = 'remove'
  for i in adam:
    if result in i.upper():
      result = 'remove'
  return result


#function to process capitalized strings for matching with adam remove
def compTcaps(leftStr,rightStr,stopwords):
  global caps
  sw = set(s for s in stopwords)
  lv = set([suppressWords(s.strip()) for s in caps.findall(leftStr)])
  rv = set([suppressWords(s.strip()) for s in caps.findall(rightStr)])
  lv.discard('remove')
  rv.discard('remove')
  return len((lv - sw) & (rv - sw))

#function to process capitalized strings for matching without adam remove
def compTcaps2(leftStr,rightStr,stopwords):
  global caps
  sw = set(s for s in stopwords)
  lv = set([s.strip() for s in caps.findall(leftStr)])
  rv = set([s.strip() for s in caps.findall(rightStr)])
  lv.discard('remove')
  rv.discard('remove')
  return len((lv - sw) & (rv - sw))


def get_authors(pmID):
  global dbcursor
  sqlselect = "select authors from PUBMED2015.Articles where PMID = " + str(pmID)
  dbcursor.execute(sqlselect)
  table_rows = dbcursor.fetchall()
  if len(table_rows) == 0:
    return ''
  else:
    t = ''.join(table_rows[0]).encode('ascii',errors='ignore')
    return t.decode('ascii')
    #[decodeAuth(v) for v in t.split('|')]

def decode_name(inName):
  name = HumanName(inName).as_dict()
  for k in name.keys():
    if len(name[k]) == 0:
      name[k]='-'
  return name['last'] + '\\' + name['first'] + '\\' + name['middle'][:1] + '\\-\\' + name['suffix']


def get_accession(pmID):
  global dbcursor
  sqlselect = "select accession_number from PUBMED2015.features where PMID = " + str(pmID)
  dbcursor.execute(sqlselect)
  table_rows = dbcursor.fetchall()
  if len(table_rows) == 0:
    return ''
  else:
    ot = ' '.join(table_rows[0]).encode('ascii',errors='ignore')
    return ot.decode('ascii')

def get_affiliation(pmID):
  global dbcursor
  sqlselect = "select affiliation from PUBMED2015.Articles where PMID = " + str(pmID)
  dbcursor.execute(sqlselect)
  table_rows = dbcursor.fetchall()
  if len(table_rows) == 0:
    return ''
  else:
    ot = ' '.join(table_rows[0]).encode('ascii',errors='ignore')
    return ot.decode('ascii')

def get_abstract(pmID):
  global dbcursor
  sqlselect = "select abstract from PUBMED2015.Abstracts where PMID = " + str(pmID)
  dbcursor.execute(sqlselect)
  table_rows = dbcursor.fetchall()
  if len(table_rows) == 0:
    return ''
  else:
    ot = ' '.join(table_rows[0]).encode('ascii',errors='ignore')
    return ot.decode('ascii')

def get_cn(pmID):
  global dbcursor
  sqlselect = "select collective_names from PUBMED2015.features where PMID = " + str(pmID)
  dbcursor.execute(sqlselect)
  table_rows = dbcursor.fetchall()
  if len(table_rows) == 0:
    return ''
  else:
    ot = ' '.join(table_rows[0]).encode('ascii',errors='ignore')
    return ot.decode('ascii')

def get_mpair(M1,M2):
  mt1 = M1.replace('"','')
  mt2 = M2.replace('"','')
  global dbcursor
  sqlselect = """SELECT a.odds_2 FROM Mesh_Pairs.mesh_pairs a WHERE a.mesh_key_1 IN (
    select b.mesh_key from Mesh_Pairs.mesh_keys b where mesh = "{0}" )
    and a.mesh_key_2 IN (select c.mesh_key from Mesh_Pairs.mesh_keys c where mesh = "{1}")
     ;
  """
  dbcursor.execute(sqlselect.format(mt1,mt2))
  table_rows = dbcursor.fetchall()
  tset =  pd.DataFrame(table_rows, columns=dbcursor.column_names )
  #print(tset)
  try:
    out = float(tset.at[0,'odds_2'])
  except:
    out = float(0)
  return out



def decode_sims(inTxt):
  lb = inTxt.find('{')
  rb = inTxt.find('}')
  try:
    tmpdict = json.loads(inTxt[lb:rb+1].replace("'",'"'))
    #print(inTxt[lb:rb+1].replace("'",'"'))
    #tmpdict = dict({'unw_tms':np.nan,'unw_scr':np.nan,'w_tms':np.nan,'w_scr':np.nan})
  except:
    tmpdict = dict({'unw_tms':np.nan,'unw_scr':np.nan,'w_tms':np.nan,'w_scr':np.nan})
  return tmpdict



#############################################
#functions to perform similarity computations
#############################################

def comp_occurance(ctText,pmText):
  return pmText.upper().count(ctText.upper())

def comp_propoccurance(ctText,pmText):
  itls = [v.upper() for v in ctText.split('|')]
  if len(itls) == 0:
    return float(0)
  else:
    tsum = 0
    ptem = pmText.upper()
    for g in itls:
      if g in ptem:
        tsum += 1
    #print (tsum,len(itls),itls)
    return float(tsum)/float(len(itls))

def comp_reverseoccur(ctText,pmText):
  itls = [v.upper() for v in pmText.split('|')]
  if len(itls) == 0:
    return float(0)
  else:
    tsum = 0
    ptem = ctText.upper()
    for g in itls:
      if g in ptem:
        tsum += 1
    #print (tsum,len(itls),itls)
    return float(tsum)/float(len(itls))

def comp_reversecount(ctText,pmText):
  itls = [v.upper() for v in pmText.split('|')]
  if len(itls) == 0:
    return 0
  else:
    tsum = 0
    ptem = ctText.upper()
    for g in itls:
      if g in ptem:
        tsum += 1
    return tsum

def comp_meshsim(leftStr,rightStr):
  global MeSHstop
  sw = set(s for s in MeSHstop)
  lv = set([s.strip().upper() for s in leftStr.split('|')])
  rv = set([s.strip().upper() for s in rightStr.split('|')])
  if len(leftStr) < 2 or len(rightStr) < 2:
    #print('yes, nulling')
    return 'NULL'
  else:
    l2 = (lv - sw)
    r2 = (rv - sw)
    lsize = len(l2)
    rsize = len(r2)
    tsum = float(0)
    if lsize > 0:
      tsum += float(len(l2 & r2)) / float(lsize)
    if rsize > 0:
      tsum += float(len(l2 & r2)) / float(rsize)
    #print(tsum, lsize,rsize,len(l2 & r2))
    return str(tsum)

def comp_meshsim2(leftStr,rightStr):
  global MeSHstop
  sw = set(s for s in MeSHstop)
  lv = set([s.strip().upper() for s in leftStr.split('|')])
  rv = set([s.strip().upper() for s in rightStr.split('|')])
  if len(leftStr) < 2 or len(rightStr) < 2:
    #print('yes, nulling')
    return 'NULL'
  else:
    l2 = (lv - sw)
    r2 = (rv - sw)
    lsize = len(l2)
    rsize = len(r2)
    tsum = float(0)
    if rsize > 0:
      tsum = float(len(l2 & r2)) / float(rsize)
    if lsize > 0:
      tsum = float(len(l2 & r2)) / float(lsize)
    #print(tsum, lsize,rsize,len(l2 & r2))
    return str(tsum)

def comp_meshsim3(leftStr,rightStr):
  global MeSHstop
  sw = set(s for s in MeSHstop)
  lv = set([s.strip().upper() for s in leftStr.split('|')])
  rv = set([s.strip().upper() for s in rightStr.split('|')])
  if len(leftStr) < 2 or len(rightStr) < 2:
    #print('yes, nulling')
    return 'NULL'
  else:
    l2 = (lv - sw)
    r2 = (rv - sw)
    lsize = len(l2)
    rsize = len(r2)
    tsum = float(0)
    #print(tsum, lsize,rsize,len(l2 & r2))
    return str(len(l2 & r2))

def comp_meshsimoddsavg(leftStr,rightStr):
  global MeSHstop
  sw = set(s for s in MeSHstop)
  lv = set([s.strip() for s in leftStr.split('|')])
  rv = set([s.strip() for s in rightStr.split('|')])
  if len(leftStr) < 2 or len(rightStr) < 2:
    print('yes, nulling')
    return 'NULL'
  else:
    l2 = list((lv - sw))
    r2 = list((rv - sw))
    lsize = len(l2)
    rsize = len(r2)
    combs = list(itertools.product(l2,r2))
    stack = pd.DataFrame(columns=['m1','m2','odds'])
    for c in combs:
      temp =  get_mpair(c[0],c[1])
      stack = stack.append({'m1':c[0],'m2':c[1],'odds':temp},ignore_index=True)
    return str(stack['odds'].mean())

def comp_meshsimoddsgeo(leftStr,rightStr):
  global MeSHstop
  sw = set(s for s in MeSHstop)
  lv = set([s.strip() for s in leftStr.split('|')])
  rv = set([s.strip() for s in rightStr.split('|')])
  if len(leftStr) < 2 or len(rightStr) < 2:
    #print('yes, nulling')
    return 'NULL'
  else:
    l2 = list((lv - sw))
    r2 = list((rv - sw))
    lsize = len(l2)
    rsize = len(r2)
    combs = list(itertools.product(l2,r2))
    stack = pd.DataFrame(columns=['m1','m2','odds'])
    for c in combs:
      temp =  get_mpair(c[0],c[1])
      if temp > 0:
        stack = stack.append({'m1':c[0],'m2':c[1],'odds':temp},ignore_index=True)
    if len(stack) == 0:
      return '0'
    else:
      return str(stats.gmean(stack['odds']))


def comp_tsim(ctText,pmText):
  file1 = open('/ANALYTICS/TrialLink2019/Models/textChunkA'+iwave+'.txt',"w")
  file1.write(ctText)
  file1.close()
  file2 = open('/ANALYTICS/TrialLink2019/Models/textChunkB'+iwave+'.txt',"w")
  file2.write(pmText)
  file2.close()
  pipe = subprocess.Popen(["./simscore"+iwave+".pl"], stdout=subprocess.PIPE)
  result = pipe.stdout.read()
  #print(result)
  if result[-1] == '1':
    result = result[:-1] 
  return result.decode('ascii')


def comp_allcaps(ctText,pmText):
  global caps
  global stoplist
  sw = set(s for s in stoplist)
  lv = set([s.strip() for s in caps.findall(ctText)])
  rv = set([s.strip() for s in caps.findall(pmText)])
  lv.discard('remove')
  rv.discard('remove')
  return len((lv - sw) & (rv - sw))

def comp_authtree(ctText,pmText):
  ct2 = '|'.join([decode_name(ctn) for ctn in ctText.split('|')])
  mlist = compAuthAlt(ct2,pmText)
  return mlist

def comp_pmidrankmin(nct,pmid):
  SQL = """SELECT a.PMID_similar, a.rank
  FROM aholt.triallink_pubranks a
  WHERE a.PMID = {0} and a.PMID_similar in 
    (select b.PMID FROM aholt.pubmed_nctlinks b 
      where b.NCT_cleaned = '{1}');"""
  dbcursor.execute(SQL.format(str(pmid),nct))
  table_rows = dbcursor.fetchall()
  rks = pd.DataFrame(table_rows, columns=dbcursor.column_names )
  retval = 50
  if len(rks) > 0:
    rks['ranktrim'] = rks.apply(lambda row: 50 if row['rank']>20 else row['rank'],axis=1)
    retval = rks['ranktrim'].min()
  return retval

def comp_pmidrankminu(nct,pmid):
  SQL = """SELECT a.PMID_similar, a.rank
  FROM aholt.triallink_pubranks a
  WHERE a.PMID = {0} and a.PMID_similar in 
    (select b.PMID FROM aholt.pubmed_nctlinks b 
      where b.NCT_cleaned = '{1}');"""
  dbcursor.execute(SQL.format(str(pmid),nct))
  table_rows = dbcursor.fetchall()
  rks = pd.DataFrame(table_rows, columns=dbcursor.column_names )
  retval = 5000
  if len(rks) > 0:
    #rks['ranktrim'] = rks.apply(lambda row: 50 if row['rank']>20 else row['rank'],axis=1)
    retval = rks['rank'].min()
  return retval

def comp_pmidrankminu1(nct,pmid):
  SQL = """SELECT a.PMID_similar, a.rank
  FROM aholt.triallink_pubranks a
  WHERE a.PMID = {0} and a.PMID_similar in 
    (select b.PMID FROM aholt.pubmed_nctlinks b 
      where b.NCT_cleaned = '{1}') and a.rank>1;"""
  dbcursor.execute(SQL.format(str(pmid),nct))
  table_rows = dbcursor.fetchall()
  rks = pd.DataFrame(table_rows, columns=dbcursor.column_names )
  retval = 5000
  if len(rks) > 0:
    #rks['ranktrim'] = rks.apply(lambda row: 50 if row['rank']>20 else row['rank'],axis=1)
    retval = rks['rank'].min()
  return retval

def comp_aggmax(nct,pmid):
  global linkdict
  rks = set()
  if nct in linkdict.keys():
    rks = set([int(v) for v in linkdict[nct].split(',')])
    rks.discard(int(pmid))
  aggset = []
  #print(rks)
  if len(rks) > 0:
    for r in rks:
      aggset.append((str(pmid),str(r)))
    scores = runAggregator(aggset)
    return max(scores)
  else:
    return float(np.nan)

def comp_aggavg(nct,pmid):
  SQL = "select b.PMID FROM aholt.pubmed_nctlinks b where b.NCT_cleaned = '{0}';"
  dbcursor.execute(SQL.format(nct))
  table_rows = dbcursor.fetchall()
  rks = pd.DataFrame(table_rows, columns=dbcursor.column_names )
  lstack = []
  if len(rks) > 0:
    for lindex, lrow in rks.iterrows():
      pipe = subprocess.Popen(["python","/ANALYTICS/Aggregator2019/Models/agg_applypairs.py",str(pmid),str(lrow['PMID'])], stdout=subprocess.PIPE)
      result = pipe.stdout.read()
      result = result.decode().replace('\n','')
      rdict = json.loads(result.replace("'",'"'))
      if not str(pmid) == str(lrow['PMID']):
        lstack.append(rdict['prob'])
  if len(lstack) > 0:
    return np.mean(lstack)
  else:
    return float(np.nan)

#comp_aggmax('NCT00312000',17630036)

def comp_pmidrankavg(nct,pmid):
  SQL = """SELECT a.PMID_similar, a.rank
  FROM aholt.triallink_pubranks a
  WHERE a.PMID = {0} and a.PMID_similar in 
    (select b.PMID FROM aholt.pubmed_nctlinks b 
      where b.NCT_cleaned = '{1}');"""
  dbcursor.execute(SQL.format(str(pmid),nct))
  table_rows = dbcursor.fetchall()
  rks = pd.DataFrame(table_rows, columns=dbcursor.column_names )
  retval = 50
  if len(rks) > 0:
    rks['ranktrim'] = rks.apply(lambda row: 50 if row['rank']>20 else row['rank'],axis=1)
    retval = rks['ranktrim'].mean()
  return retval


def comp_rksimax(ctText,pmid):
  SQL = """SELECT a.PMID_similar, a.rank
  FROM aholt.triallink_pubranks a
  WHERE a.PMID = {0} and not a.PMID_similar = {0} and a.rank < 7;"""
  dbcursor.execute(SQL.format(str(pmid)))
  table_rows = dbcursor.fetchall()
  rks = pd.DataFrame(table_rows, columns=dbcursor.column_names )
  #rks = rks.loc[rks['rank']<7]
  rks['unw_tms'] = float(0)
  rks['unw_scr'] = float(0)
  rks['w_tms'] = float(0)
  rks['w_scr'] = float(0)
  for cind, crow in rks.iterrows():
    ptxt = get_title(crow['PMID_similar']) + ' ' + get_abstra(crow['PMID_similar'])
    #print(ctText,ptxt)
    restt = comp_tsim(ctText,ptxt)
    #print(restt)
    try:
      restt = comp_tsim(ctText,ptxt)
      #print(restt)
      v = decode_sims(restt)
      #v =  dict({'unw_tms':0,'unw_scr':0,'w_tms':0,'w_scr':0})
    except:
      v =  dict({'unw_tms':0,'unw_scr':0,'w_tms':0,'w_scr':0})
    rks.at[cind,'unw_tms'] = v['unw_tms']
    rks.at[cind,'unw_scr'] = v['unw_scr']
    rks.at[cind,'w_tms'] = v['w_tms']
    rks.at[cind,'w_scr'] = v['w_scr']
  #print(rks)
  if len(rks) > 0:
    return "{'unw_tms':" + str(max(rks['unw_tms'])) + ",'unw_scr':" + str(max(rks['unw_scr'])) + ",'w_tms':" + str(max(rks['w_tms'])) + ",'w_scr':" + str(max(rks['w_scr'])) + "}"
  else:
    return "{'unw_tms':0,'unw_scr':0,'w_tms':0,'w_scr':0}"


#preprocess the author lists into lists of lists
def expandAuth(atext):
  alist = atext.split('|')
  #print(atext)
  blist = [decodeAuth(v) for v in alist]
  return blist

def runAggregator(inTups):
  global AuthorityRef, AuthorityDetails,rankrack,article_rows,chem_rows,abs_rowdict
  pmidperm = pd.DataFrame(inTups)
  pmidperm.columns = ['article1','article2']
  #pmidlist = list(set(pmidperm['article1']) | set(pmidperm['article2']))
  pstack = []
  ########################## main aggregator computation loop
  for index, row in pmidperm.iterrows():
    l = int(row['article1'])
    r = int(row['article2'])
    #using individual variables for all features for easy checking
    F02 = float(0)
    F08 = float(0)
    F02b_t = float(0)
    F12 = float(0)
    F01_t = 11.7585
    F09 = float(0)
    F10_t = float(0)
    F03 = float(0)
    F11 = float(0)
    F11_t = float(0)
    F07b_t = float(0)
    ########################## F01_t - related article rankings
    f01temp = 5000
    if l in rankrack.keys():
      if r in rankrack[l].keys():
        f01temp = rankrack[l][r] - 1
    if r in rankrack.keys():
      if l in rankrack[r].keys():
        f01temp = min(f01temp,rankrack[r][l]-1)
    if f01temp < 5000:
      if f01temp > 20:
        F01_t = 50
      else:
        F01_t = f01temp
    ########################## F02 - shared authors
    if article_rows.loc[l,'authors'] == '-' or article_rows.loc[r,'authors'] == '-':
      F02b_t = 1 - 3
      F02 = float(0)
    else:
      F02b_t = 0 - 3
      #print(article_rows.loc[l,'authors2'],l,article_rows.loc[r,'authors2'],r)
      F02 = compAuth(article_rows.loc[l,'authors2'],l,article_rows.loc[r,'authors2'],r)
    ########################## Feature 03 - affiliations
    F03 = compAff(article_rows.loc[l,'affiliation'],article_rows.loc[r,'affiliation'])
    ########################## Feature 07b_t - shared email domains
    F07b_t = compEmailD(article_rows.loc[l,'email'],article_rows.loc[r,'email']) - 1
    ########################## Feature 08 - shared country names
    F08 = compCountry(article_rows.loc[l,'affiliation'].upper(),article_rows.loc[r,'affiliation'].upper())
    ########################## Feature 09 - shared grant numbers
    F09 = compGrants(article_rows.loc[l,'grants'],article_rows.loc[r,'grants'])
    ########################## Feature 10 - shared chemicals
    if compSUB(chem_dict[l],chem_dict[r]) == 1:
      F10_t = 1
    ########################## Feature 11 - all cap word comparisons
    F11 = len(article_rows.loc[l,'titleCapsNoAdam'] & article_rows.loc[r,'titleCapsNoAdam'])
    F11b = len(article_rows.loc[l,'titleCaps'] & article_rows.loc[r,'titleCaps'])
    F11_t = F11b - F11
    ########################## Feature 12 - collective names
    F12 = len(abs_rowdict[l] & abs_rowdict[r])
    ########################## model parameters
    logit = (F02*0.0050753 + F08*0.8136765 + F02b_t*0.6437122 + F12*1.1295069 +
    F01_t*-0.0180124 + F09*0.2785423 + F10_t*0.4543176 +
    F03*0.1507674 + F11*0.8896055 +
    F11_t*0.428596 + F07b_t*0.3678568)
    prob = np.exp(logit) / (1 + np.exp(logit))
    #print(prob)
    pstack.append(prob)
  return pstack


