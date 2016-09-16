import json
import pandas as pd

data = []

#exclude = []
<<<<<<< Updated upstream:analysis/export_data.py

raw = pd.read_json("initial-accuracy1-export.json")
=======
import os
basePath = os.path.dirname(os.path.abspath("initial-accuracy1-export.json"))
raw = pd.read_json(basePath + "/initial-accuracy1-export.json")
>>>>>>> Stashed changes:data/export_data.py

#data = [json.loads(part)['data'] for part in raw]

from pandas.io.json import json_normalize

# iquizdat = []
# for d in raw['instructquiz']:
#     if str(d)!='nan':
#         iquizdat.append(d)

famdat = []
for d in raw['familiarization']:
    if str(d)!='nan':
        famdat.append(d)

pquizdat = []
for d in raw['postquiz']:
    if str(d)!='nan':
        pquizdat.append(d)

famdat = []
for d in raw['familiarization']:
    if str(d)!='nan':
        famdat.append(d)

studydat = []
for d in raw['study']:
    if str(d)!='nan':
        studydat.append(d)

testdat = []
for d in raw['test']:
    if str(d)!='nan':
        testdat.append(d)


#iquizd = json_normalize(iquizdat, ['condnum', 'num_correct', 'phase', 'time', 'uniqueId'])

# iquizdat = pd.DataFrame(iquizdat)
famdat = pd.DataFrame(famdat)
pquizdat = pd.DataFrame(pquizdat)
famdat = pd.DataFrame(famdat)
studydat = pd.DataFrame(studydat)
testdat = pd.DataFrame(testdat)

<<<<<<< Updated upstream:analysis/export_data.py
# iquizdat.to_csv('initial-accuracy1_instructquiz_data.csv')
famdat.to_csv('initial-accuracy1_familiarization_data.csv')
pquizdat.to_csv('initial-accuracy1_postquiz_data.csv')
studydat.to_csv('initial-accuracy1_study_data.csv')
testdat.to_csv('initial-accuracy1_test_data.csv')
=======
iquizdat.to_csv('initial_accuracy1_instructquiz_data.csv')
pquizdat.to_csv('initial_accuracy1_postquiz_data.csv')
famdat.to_csv('initial_accuracy1_familiarization_data.csv')
studydat.to_csv('initial_accuracy1_study_data.csv')
testdat.to_csv('initial_accuracy1_test_data.csv')
>>>>>>> Stashed changes:data/export_data.py
