#!/usr/bin/env python3
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
trial = sys.argv
print(trial)
