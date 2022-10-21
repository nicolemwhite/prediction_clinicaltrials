This archive contains runnable example code for the Trials-to-Publication model described in:
Smalheiser NR, Holt AW (2021). A Web-based Tool for Automatically linking Clinical Trials to their Publications.
https://www.medrxiv.org/content/10.1101/2021.06.24.21259481v1 

The described web tool accesses live databases as well as PubMed.gov for up-to-date-data.  In this example, all data extracts have been "pickled" as python object, and the live queries have been commented out.  There is no dataset_15.pkl.

REQUIREMENTS:

* Python 3.6 running on Ubuntu 18.04 LTS, other Linux distributions with Python 3.6+ may work.
* Python Packages required:
  - pandas
  - numpy
  - scipy
  - nameparser
  - re,json, subprocess,sys,datetime,itertools,os,time,requests,pickle,urllib,logging
    (these packages are usually already installed in most python distros)

INSTRUCTIONS:

* Extract all files in zip to the same folder
* cd <to the folder where the files were extracted>
* execute:
  python3 TrialLink_Example.py

The output dataset is scoreout_NCT03745053_data.csv
This should be identical to scoreout_NCT03745053_data_original.csv

