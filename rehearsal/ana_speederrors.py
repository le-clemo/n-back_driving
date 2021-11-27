# -*- coding: utf-8 -*-
"""
Created on Fri Jan 8 12:14:09 2021

@author: moritz
"""
# %% functions

from tqdm import tqdm as tq
import re
import matplotlib.pyplot as plt
import glob
import pandas as pd
import os
import numpy as np
from scipy import stats


def format_str(unformatted_str):
    """
    Formats the output into a multi column list,  output1 | output2 | output3
    """
    output = []
    for word in enumerate(unformatted_str):
        if word != '':
            output.append(word[1])
    return output

def create_blocks(dat):
    """[summary]

    Args:
        dat (dataframe): Pandas dataframe of the formatted act-r output

    Returns:
        [dataframe]: Returns three dataframes that have the indeces for
        the start/end of a block as well as the condition of the block. 

        E.g., start: 0, stop: 200, condition: highway 
        -> Highway block ranging from 0:200 in input df
    """
    vis_attention = np.array(dat['visAttention'])

    start = []
    stop = []
    condition = []
    for i, con in enumerate(vis_attention):
        if i == 0:
            start.append(i)
            condition.append(con)
        elif vis_attention[i] != vis_attention[i - 1]:
            stop.append(i - 1)
            start.append(i)
            condition.append(con)
        if i == len(vis_attention) - 1:
            stop.append(i)

    blocks = pd.DataFrame({
        'start': start,
        'stop': stop,
        'condition': condition
    })
    start = blocks['start']
    stop = blocks['stop']
    condition = blocks['condition']
    return start, stop, condition


def find_nback(regex, nbacklevels):
    return [poss for poss in nbacklevels if (poss in regex)][0]


def find_subj(regex):
    return re.findall(r'behavior_(\d+)', regex)[0]

class missing_data(Exception):
    pass

# %% 
# %% import the data
import_path = os.getcwd() + '\data\*behavior*.txt'
output_path = os.getcwd()
path_list = list(glob.glob(import_path))
if (path_list == []):
    raise missing_data('There is no data in the specified folder!')
srate = 0.05
nbacklevels = ['2back'] #'0back', '1back',  , '3back' , '4back'

count = np.zeros(len(nbacklevels))
for filename in path_list:
    for i in range(len(nbacklevels)):
        if nbacklevels[i] in filename:
            count[i] += 1
count = count.astype(int)

first_subj = np.empty_like(np.array(nbacklevels))
for filename in path_list:
    for i in range(len(nbacklevels)):
        if nbacklevels[i] in filename:
            cs = int(find_subj(filename))
            if first_subj[i] == '' or cs < int(first_subj[i]):
                first_subj[i] = "{:02d}".format(cs)

subj_list = []
for p in path_list:
    subj_list.append('subj_' + re.search(r'behavior_(\d+)', p).group(1))

data = dict.fromkeys(nbacklevels, 0)
for key in data:
    data[key] = dict.fromkeys(subj_list, 0)

pbar = tq(path_list, position=0, desc='Imported files: ', leave=True)
for filename in path_list:

    nback = find_nback(filename, nbacklevels)
    f = open(filename, 'r')
    lines = [line.rstrip('\n') for line in f]

    data_subj = []
    for row, word in enumerate(lines):
        unformatted_str = word.split('|')
        formatted_str = format_str(unformatted_str)
        data_subj.append(formatted_str)

    subj = 'subj_' + find_subj(filename)
    data_subj = pd.DataFrame(data_subj[1:], columns=data_subj[0])
    data[nback][subj] = data_subj
    pbar.update()
output = {}
print('\n\nImported', len(path_list), 'file(s) from',
      import_path.replace('*behavior*.txt', '.'))

# %% 

avg_errate_con, avg_errate_high = np.zeros(
    len(nbacklevels)), np.zeros(len(nbacklevels))
sem_errate_con, sem_errate_high = np.zeros(
    len(nbacklevels)), np.zeros(len(nbacklevels))
avg_error_con, avg_error_high = np.zeros(
    len(nbacklevels)), np.zeros(len(nbacklevels))
sem_error_con, sem_error_high = np.zeros(
    len(nbacklevels)), np.zeros(len(nbacklevels))

for n, nback in enumerate(data):
    noSign = 20*20  # no of samples in build up phase
    errors_con, errors_high = [], []
    errates_con, errates_high = [], []

    for s, subj in enumerate(subj_list):

        dat = data[nback][subj]
        if isinstance(dat, pd.DataFrame) == False:
            continue

        start, stop, condition = create_blocks(dat)
        speedlimit = np.array(dat['currentspeed'].astype(int))
        imagined_limit = dat['imaginedSpeedlimit'].replace(
            r'^\s*$', 'NaN', regex=True)
        imagined_limit = np.array(imagined_limit.astype(float))
        new_signs = np.where(np.diff(speedlimit))[0] + 1
        # alternative: new_signs = np.where(speedlimit[1:] != speedlimit[:-1])[0] + 1

        nbackspeed = np.zeros_like(speedlimit)
        # build-up phase
        nbackspeed[0:new_signs[0]] = 60
        for i in range(0, n):
            nbackspeed[new_signs[i]:new_signs[i + 1]
                       ] = speedlimit[new_signs[i]]

        # nback phase
        for i in range(n, len(new_signs)):
            start = new_signs[i]
            stop = new_signs[i + 
                             1] if i != len(new_signs) - 1 else len(nbackspeed)
            nbackspeed[start:stop] = speedlimit[new_signs[i - n]]

        # # of errors
        n_error_high, n_error_con = 0, 0
        trials_con, trials_high = 0, 0
        for i in range(n + 1, len(new_signs)):
            pos = new_signs[i] - 1

            # total number of trials per condition
            if dat['visAttention'][pos] == 'highway':
                trials_high += 1
            elif dat['visAttention'][pos] == 'construction':
                trials_con += 1

            # catch errors
            if(imagined_limit[pos] != nbackspeed[pos]):
                if dat['visAttention'][pos] == 'highway':
                    n_error_high += 1
                elif dat['visAttention'][pos] == 'construction':
                    n_error_con += 1
        errates_con.append(n_error_con/(trials_con - n))
        errors_con.append(n_error_con)
        errates_high.append(n_error_high/(trials_high - n))
        errors_high.append(n_error_high)

    avg_errate_con[n] = np.mean(errates_con)
    avg_errate_high[n] = np.mean(errates_high)

    avg_error_con[n] = np.mean(errors_con)
    avg_error_high[n] = np.mean(errors_high)

    sem_errate_con[n] = stats.sem(errates_con)
    sem_errate_high[n] = stats.sem(errates_high)

    sem_error_con[n] = stats.sem(errors_con)
    sem_error_high[n] = stats.sem(errors_high)

print('The average speed regulation error rate in the construction condition is:\n',
      avg_errate_con)
print('The average speed regulation error rate in the highway condition is:\n',
      avg_errate_high)

# saving results
output['avg_error_rate_con'] = avg_errate_con
output['avg_error_rate_high'] = avg_errate_high
output['sem_errate_con'] = sem_errate_con
output['sem_errate_high'] = sem_errate_high
output['avg_error_con'] = avg_error_con
output['avg_error_high'] = avg_error_high
output['sem_error_con'] = sem_error_con
output['sem_error_highway'] = sem_error_high

# plotting results
labels = ['0-back', '1-back', '2-back', '3-back', '4-back']
x = np.arange(len(labels))
width = 0.3

# number of errors
fig1, ax1 = plt.subplots()
plt.style.use('default')
rects1 = ax1.bar(x - width/2, avg_error_high, width,
                 label='Highway', yerr=sem_errate_high, capsize=5, ecolor='grey', edgecolor = 'black')
rects2 = ax1.bar(x + width/2, avg_error_con, width,
                 label='Construction', yerr=sem_errate_con, capsize=5, ecolor='grey', edgecolor = 'black')

ax1.set_ylabel('Number of errors')
#ax1.set_title(' Average number of errors in speed regulation task')
ax1.set_xticks(x)
ax1.set_xticklabels(labels)
ax1.legend(loc = 'best')

fig1.tight_layout()
plt.savefig('ana1/errors_number.pdf')
plt.show()

# error rates
fig2, ax2 = plt.subplots()
rects3 = ax2.bar(x - width/2, avg_errate_high, width,
                 label='highway', edgecolor = 'black')  # , yerr=sem_error_high, capsize=5, ecolor='grey')
rects4 = ax2.bar(x + width/2, avg_errate_con, width,
                 label='construction', edgecolor = 'black')  # , yerr=sem_error_con, capsize=5, ecolor='grey')

ax2.set_ylabel('Error rates in %')
#ax2.set_title('Average error rates in speed regulation task')
ax2.set_xticks(x)
ax2.set_xticklabels(labels)
ax2.legend()

fig2.tight_layout()
plt.savefig('ana1/error_rates.pdf')
plt.show()
# %%
