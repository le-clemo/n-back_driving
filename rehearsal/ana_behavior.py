# -*- coding: utf-8 -*-
"""
Created on Fri Jan 8 12:14:09 2021

@author: moritz
"""
# %% functions

import math
from tqdm import tqdm as tq
import re
import random as rd
import matplotlib.pyplot as plt
import glob
import pandas as pd
import os
import numpy as np
import sys
import csv
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


def del_lanechange(dat):
    """ deletes +- 3 seconds around a lane crossing that is part of a lange change manoeuvre

    Args:
        dat (dataframe): Pandas dataframe of the formatted act-r output

    Returns:
        dat (dataframe): Pandas dataframe with the rows deleted that involve a lane change
    """
    lanepos = (dat['lanepos']).astype(float)
    lane_center = np.floor(lanepos) + 0.5
    deviation = np.array(lanepos - lane_center)

    # delete +- 3 seconds around a lane change manoeuvre
    it, = np.where(dat['turning'] == 'true')
    drp = []
    seconds = 3

    for i, val in enumerate(it):
        b = True
        while b == True:
            if (deviation[val - 1] > 0.4 and deviation[val] < -0.4) or \
                    (deviation[val - 1] < -0.4 and deviation[val] > 0.4):
                start = int(val - seconds * (1 / srate))
                stop = min(int(val + seconds * (1 / srate)),
                           len(deviation - 1))
                drp.extend(range(start, stop))
                b = False
            elif (it[i] + 5 * (1 / srate)) < val:
                b = False
            elif val == len(deviation) - 1:
                b = False
            else:
                val += 1
    dat = dat.drop(dat.index[drp])
    return dat


def detect_lanechange(dat):

    lanepos = (dat['lanepos']).astype(float)
    lane_center = np.floor(lanepos) + 0.5
    deviation = np.array(lanepos - lane_center)

    # delete +- 3 seconds around a lane change manoeuvre
    it, = np.where(dat['turning'] == 'true')
    changes_range = []
    seconds = 3

    for i, val in enumerate(it):
        b = True
        while b == True:
            if (deviation[val - 1] > 0.4 and deviation[val] < -0.4) or \
                    (deviation[val - 1] < -0.4 and deviation[val] > 0.4):
                start = int(val - seconds * (1 / srate))
                stop = min(int(val + seconds * (1 / srate)),
                           len(deviation - 1))
                changes_range.extend(range(start, stop))
                b = False
            elif (it[i] + 5 * (1 / srate)) < val:
                b = False
            elif val == len(deviation) - 1:
                b = False
            else:
                val += 1

    return changes_range


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


def is_dataframe(data, nback, subj='subj_01'):
    return True if isinstance(data[nback][subj], pd.DataFrame) else False


class missing_data(Exception):
    pass


functions = [f.__name__ for f in globals().values() if type(f) ==
             type(lambda *args: None)]
print('Defined functions:', functions)

# %%
""" 
First we need to import the data from the .txt files. This is stored in a 
nested pandas dataframe with the order data[nbacklevel][subject] e.g., data['2back']['subj_23']

"""
# %% import the data
import_path = os.getcwd() + '\data\*behavior*.txt'
output_path = os.getcwd()
path_list = list(glob.glob(import_path))
if (path_list == []):
    raise missing_data('There is no data in the specified folder!')
srate = 0.05
nbacklevels = ['0-back', '1-back', '2-back', '3-back', '4-back']

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

# %% Steering reversal rates

mean_construction, mean_highway = np.zeros(
    len(nbacklevels)), np.zeros(len(nbacklevels))
sem_construction, sem_highway = np.zeros(
    len(nbacklevels)), np.zeros(len(nbacklevels))
nback= '2-back'
for n, nback in enumerate(data):

    rev_con_s = []
    rev_high_s = []

    for s, subj in enumerate(subj_list):

        dat = data[nback][subj]
        if isinstance(dat, pd.DataFrame) == False:
            continue

        changes_range = detect_lanechange(dat)
        steerAngle = np.array(dat['steerAngle'])
        visAttention = np.array(dat['visAttention'])

        rev_con = 0
        rev_high = 0

        for i in range(1, len(steerAngle)):
            # steering reversal from left to right or right to left
            if (float(steerAngle[i - 1]) * float(steerAngle[i])) < 0:
                if(visAttention[i] == 'highway') and i not in changes_range:
                    rev_high += 1
                elif(visAttention[i] == 'construction') and i not in changes_range:
                    rev_con += 1

        # length of construction/highway segments without lane-changes
        steerAngle = np.delete(steerAngle, changes_range)
        visAttention = np.delete(visAttention, changes_range)
        len_con = len(steerAngle[visAttention == 'construction'])
        len_high = len(steerAngle[visAttention == 'highway'])

        rev_con_s.append(rev_con / len_con / srate)
        rev_high_s.append(rev_high / len_high / srate)

    mean_construction[n] = np.mean(rev_con_s)
    mean_highway[n] = np.mean(rev_high_s)
    sem_construction[n] = stats.sem(rev_con_s)
    sem_highway[n] = stats.sem(rev_high_s)
print('mean SRRs in construction blocks:\n', mean_construction)
print('mean SRRs in highway blocks:\n', mean_highway)

# saving results
output['avg_SRR_con'] = mean_construction
output['avg_SRR_high'] = mean_highway
output['sem_SRR_con'] = sem_construction
output['sem_SRR_high'] = sem_highway

# plotting results
labels = ['0-back', '1-back', '2-back', '3-back', '4-back']
x = np.arange(len(labels))
width = 0.3

plt.style.use('default')
csfont = {'family': 'Times new roman',
          'size': 12.0}
plt.rc('font', **csfont)
fig, ax = plt.subplots()

rects1 = ax.bar(x - width/2, mean_highway, width,
                label='Highway', yerr=sem_highway, capsize=5, ecolor='grey', edgecolor='black')
rects2 = ax.bar(x + width/2, mean_construction, width,
                label='Construction', yerr=sem_construction, capsize=5, ecolor='grey', edgecolor='black')


ax.set_ylabel('Steering reversal rates in Hz')
ax.set_xlabel('N-back levels')
#ax.set_ylim(0.25, 0.3)
#ax.set_title('Mean SRRs')
ax.set_xticks(x)
ax.set_xticklabels(labels)
ax.legend()

fig.tight_layout()
plt.savefig('ana1/steering_reversals.pdf')
plt.show()
#outpath= r'C:\Users\Held\Documents\Data\data_human_uol\ana2\model_srr.csv'
#pd.DataFrame.from_dict(data=output).to_csv(outpath)

#output.to_csv(r'C:\Users\Held\Documents\Data\data_human_uol\ana2\model_srr.csv')
# %% deviations from lane center

mean_highway, mean_construction = np.zeros(len(
    nbacklevels)), np.zeros(len(nbacklevels))
std_highway, std_construction = np.zeros(
    len(nbacklevels)), np.zeros(len(nbacklevels))
sem_highway, sem_construction = np.zeros(
    len(nbacklevels)), np.zeros(len(nbacklevels))

for n, nback in enumerate(data):
    mean_highway_s, mean_construction_s = [], []
    std_construction_s, std_highway_s = [], []
    sem_highway_s, sem_construction_s = [], []

    if is_dataframe(data, nback,('subj_'+first_subj[0])) == False:
        continue

    for s, subj in enumerate(subj_list):

        if is_dataframe(data, nback, subj) == False:
            continue

        dat = data[nback][subj]
        dat = del_lanechange(dat)
        lanepos = (dat['lanepos']).astype(float)
        lane_center = np.floor(lanepos) + 0.5
        dist = np.array(lanepos - lane_center)

        # create blocks
        start, stop, condition = create_blocks(dat)
        highway = []
        construction = []
        for b in range(len(start)):
            block = dist[start[b]:stop[b]]
            if condition[b] == 'highway':
                highway.extend(block)
            else:
                construction.extend(block)

        mean_highway_s.append(np.mean(np.abs(highway)))
        mean_construction_s.append(np.mean(np.abs(construction)))

        std_highway_s.append(np.std(np.abs(highway)))
        std_construction_s.append(np.std(np.abs(construction)))

        sem_highway_s.append(stats.sem(np.abs(highway)))
        sem_construction_s.append(stats.sem(np.abs(construction)))

    mean_highway[n] = np.mean(mean_highway_s)
    mean_construction[n] = np.mean(mean_construction_s)
    std_highway[n] = np.mean(std_highway_s)
    std_construction[n] = np.mean(std_construction_s)
    sem_highway[n] = np.mean(sem_highway_s)
    sem_construction[n] = np.mean(sem_construction_s)

print('average lane deviation in construction blocks:\n', mean_construction)
print('average lane deviation in highway blocks:\n', mean_highway)
print('average standard deviation in the construction blocks:\n', std_construction)
print('average standard deviation in the highway blocks:\n', std_highway)

# saving results
output['lane_deviation_con'] = mean_construction
output['lane_deviation_high'] = mean_highway
output['std_lane_deviation_con'] = std_construction
output['std_lane_deviation_high'] = std_highway
output['sem_deviation_con'] = sem_construction
output['sem_deviation_high'] = sem_highway

# plotting results
labels = ['0-back', '1-back', '2-back', '3-back', '4-back']
x = np.arange(len(labels))
width = 0.35

# lane deviation
fig1, ax1 = plt.subplots()
rects1 = ax1.bar(x - width/2, mean_highway, width,
                 label='highway', yerr=sem_highway, capsize=5, ecolor='grey')
rects2 = ax1.bar(x + width/2, mean_construction, width,
                 label='construction', yerr=sem_construction, capsize=5, ecolor='grey')

ax1.set_ylabel('lane deviation in m')
ax1.set_title('average lane deviations')
ax1.set_xticks(x)
ax1.set_xticklabels(labels)
ax1.legend()

fig1.tight_layout()
plt.savefig('ana1/lane_deviations.pdf')
plt.show()

# %% lane-violations

mean_highway, mean_construction = np.zeros(
    len(nbacklevels)), np.zeros(len(nbacklevels))
sem_highway, sem_construction = np.zeros(
    len(nbacklevels)), np.zeros(len(nbacklevels))

for n, nback in enumerate(data):
    violations_con_s = []
    violations_high_s = []

    if is_dataframe(data, nback,('subj_'+ first_subj[0])) == False:
        continue

    for s, subj in enumerate(subj_list):

        if is_dataframe(data, nback, subj) == False:
            continue

        dat = data[nback][subj]
        dat = del_lanechange(dat)
        lanepos = (dat['lanepos']).astype(float)
        lane_center = np.floor(lanepos) + 0.5
        followedLane = (dat['followedLane']).astype(float)
        deviation = np.array(lanepos - lane_center)
        lanepos = np.array(dat['lanepos']).astype(float)
        offLane = np.logical_or(np.array(followedLane - lanepos > 0),
                                np.array(followedLane - lanepos < -1))

        start, stop, condition = create_blocks(dat)

        vio_con_blocks = []
        vio_high_blocks = []
        for b in range(len(start)):
            oL = offLane[start[b]:stop[b]]

            vio = 0
            for i in range(1, len(oL)):
                if oL[i] == True and oL[i - 1] != b and oL[i - 1] != True:
                    vio += 1
            if condition[b] == 'highway':
                vio_high_blocks.append(vio)
            elif condition[b] == 'construction':
                vio_con_blocks.append(vio)

        violations_con_s.append(np.mean(vio_con_blocks))
        violations_high_s.append(np.mean(vio_high_blocks))

    mean_construction[n] = np.mean(violations_con_s)
    mean_highway[n] = np.mean(violations_high_s)
    sem_construction[n] = stats.sem(violations_con_s)
    sem_highway[n] = stats.sem(violations_high_s)

print('average number of lane_violations in construction site:\n', mean_construction)
print('average number of lane_violations in highway condition:\n', mean_highway)

# saving results
output['lane_violations_con'] = mean_construction
output['lane_violations_high'] = mean_highway
output['sem_violations_con'] = sem_construction
output['sem_violations_high'] = sem_highway

# plotting results
labels = ['0-back', '1-back', '2-back', '3-back', '4-back']
x = np.arange(len(labels))
width = 0.35

# lane violations
fig, ax = plt.subplots()
rects1 = ax.bar(x - width/2, mean_highway, width,
                label='highway', yerr=sem_highway, capsize=5, ecolor='grey')
rects2 = ax.bar(x + width/2, mean_construction, width,
                label='construction', yerr=sem_construction, capsize=5, ecolor='grey')

ax.set_ylabel('Number of Lane violations')
ax.set_title('Average lane violations')
ax.set_xticks(x)
ax.set_xticklabels(labels)
ax.legend()

fig.tight_layout()
plt.savefig('ana1/lane_violations.pdf')
plt.show()

# %% driving path of example participant

# random participant and nback-level
nback = nbacklevels[rd.randrange(len(nbacklevels))]
s = rd.randrange(len(data[nback]) - 1)
subj = subj_list[s]

# random segment, max. 60s long - will be restricted later anyway
seglen_in_s = 30
seglen = int(seglen_in_s/srate)

# find valid dataset in case there is no data for participant/nbacklevel
while is_dataframe(data, nback, subj) == False:
    nback = nbacklevels[rd.randrange(len(nbacklevels))]
    s = rd.randrange(len(nback) - 1)
    subj = subj_list[s]

dat = data[nback][subj]
lanepos = np.array(dat['lanepos'].astype(float))
start, stop, condition = create_blocks(dat)

# random block
i = rd.randrange(len(start) - 2) if len(start) > 1 else 1
high_ex = pd.DataFrame(
    {
        'start': start[i],
        'stop': stop[i],
        'condition': condition[i]
    }, index=[i])
con_ex = pd.DataFrame(
    {
        'start': start[i + 1],
        'stop': stop[i + 1],
        'condition': condition[i + 1]
    }, index=[i + 1])
condition = [high_ex, con_ex]

fig = plt.figure()

for i, con in enumerate(condition):
    lanewidth = 2.5 if np.array(con['condition'])[0] == 'construction' else 3.5
    start, = con['start']
    stop, = con['stop']
    block = np.array(lanepos[start:stop])*lanewidth-lanewidth
    steerAngle = np.array(dat['steerAngle']).astype(float)
    steerAngleblock = np.array(steerAngle[start:stop])
    reversal = np.zeros_like(steerAngleblock)

    for i in range(len(steerAngleblock)):
        if (float(steerAngleblock[i - 1]) * float(steerAngleblock[i])) < 0:
            reversal[i] = block[i]
    reversal[reversal == 0] = np.nan

    start = rd.randrange(0, len(block) - seglen)
    stop = start + seglen
    x = np.linspace(0, seglen_in_s, seglen)
    for j in range(0, 4):
        lane = np.empty([len(block)])
        lane[:] = j
        if j == 0:
            plt.plot(x, lane[start:stop]*lanewidth, color='white')
        elif j == 3:
            plt.plot(x, lane[start:stop]*lanewidth, color='white')
        else:
            plt.plot(x, lane[start:stop]*lanewidth,
                     color='white', linestyle='dashed')

    title, = con['condition'] + ' ' + nback
    plt.title(title)
    plt.plot(x, block[start:stop], color='red')
    ax = plt.gca()
    ax.set_facecolor('grey')
    plt.gca().invert_yaxis()
    plt.ylim(12, -1)
    #plt.xlim(0, 1000)
    plt.scatter(x, reversal[start:stop], marker='x', color='lime')
    fname = 'ana1/example_' + title + '.pdf'
    plt.savefig(str(fname))

    ax2 = ax.twinx()
    ax2.set_ylabel('steering angle')
    ax2.plot(x, steerAngleblock[start:stop], color='orange')
    ax2.set_ylim(-0.2, 0.2)
    plt.show()



# %% speed regulation errors

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
rects1 = ax1.bar(x - width/2, avg_error_high, width,
                 label='Highway', yerr=sem_errate_high, capsize=5, ecolor='grey', edgecolor = 'black')
rects2 = ax1.bar(x + width/2, avg_error_con, width,
                 label='Construction', yerr=sem_errate_con, capsize=5, ecolor='grey', edgecolor = 'black')

ax1.set_ylabel('Number of errors')
#ax1.set_title(' Average number of errors in speed regulation task')
ax1.set_xticks(x)
ax1.set_xticklabels(labels)
ax1.legend()

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

# %% overtaking distance

mean_distance_con, std_distance_con, = np.zeros(
    len(nbacklevels)), np.zeros(len(nbacklevels))
mean_distance_high, std_distance_high = np.zeros(
    len(nbacklevels)), np.zeros(len(nbacklevels))
sem_distance_high, sem_distance_con = np.zeros(
    len(nbacklevels)), np.zeros(len(nbacklevels))

for n, nback in enumerate(data):

    overtaking_distance_high, overtaking_distance_con = [], []

    for s, subj in enumerate(subj_list):

        dat = data[nback][subj]
        if isinstance(dat, pd.DataFrame) == False:
            continue

        simcarFracIndex = np.array(dat['simcarFracIndex'].astype(float))
        autocarFracIndex = np.array(dat['autocarFracIndex'].astype(float))
        dist = autocarFracIndex - simcarFracIndex
        overtaking, = np.where(np.array(dat['turning'] == 'true'))
        lanepos = np.array(dat['lanepos'].astype(float))

        new_overtaking = []
        for i in range(len(overtaking)):
            j = overtaking[i]

            while j + 1 != len(lanepos) and \
                    math.floor(lanepos[j]) == math.floor(lanepos[j + 1]):
                j += 1
            new_overtaking.append(j)

        for i in range(len(new_overtaking)):
            if dist[new_overtaking[i]] < 0:
                new_overtaking[i] = float('NaN')
        new_overtaking = (np.array(new_overtaking)[
                          ~np.isnan(new_overtaking)]).astype(int)

        for i in range(len(new_overtaking)):
            pos = new_overtaking[i]
            if dat['visAttention'][pos] == 'highway':
                overtaking_distance_high.append(dist[pos])
            elif dat['visAttention'][pos] == 'construction':
                overtaking_distance_con.append(dist[pos])

    mean_distance_con[n] = np.mean(overtaking_distance_con)
    std_distance_con[n] = np.std(overtaking_distance_con)
    sem_distance_con[n] = stats.sem(overtaking_distance_con)
    mean_distance_high[n] = np.mean(overtaking_distance_high)
    std_distance_high[n] = np.std(overtaking_distance_high)
    sem_distance_high[n] = stats.sem(overtaking_distance_high)

print('construction:\n', mean_distance_con)
print('highway:\n', mean_distance_high)

# saving results
output['mean_distance_con'] = mean_distance_con
output['mean_distance_high'] = mean_distance_high
output['std_distance_con'] = std_distance_con
output['std_distance_high'] = std_distance_high
output['sem_distanc_con'] = sem_distance_con
output['sem_distance_high'] = sem_distance_high

# plotting results
labels = ['0-back', '1-back', '2-back', '3-back', '4-back']
x = np.arange(len(labels))
width = 0.3

# overtaking distance
fig1, ax1 = plt.subplots()
rects1 = ax1.bar(x - width/2, mean_distance_high, width,
                 label='highway', yerr=sem_distance_high, capsize=5, ecolor='grey')
rects2 = ax1.bar(x + width/2, mean_distance_con, width,
                 label='construction', yerr=sem_distance_con, capsize=5, ecolor='grey')

ax1.set_ylabel('distance in m')
ax1.set_title('Average overtaking distance')
ax1.set_xticks(x)
ax1.set_xticklabels(labels)
ax1.legend()

fig1.tight_layout()
plt.savefig('ana1/overtaking_distance.pdf')
plt.show()

#%% average overtaking

lanewidth = 2.5
length = 100
x = np.linspace(0, length, 10000)

fig, ax = plt.subplots() 
for j in range(0, 4):
    lane = np.empty_like(x)
    lane[:] = j
    if j == 0:
        plt.plot(x, lane*lanewidth, color='white')
    elif j == 3:
        plt.plot(x, lane*lanewidth, color='white')
    else:
        plt.plot(x, lane*lanewidth,
                    color='white', linestyle='dashed')
    ax.set_facecolor('grey')

for j,dist in enumerate(mean_distance_high):
    dtc = np.empty_like(x)
    dtc[:] =np.nan
    stop = 8000
    start = stop - int(round(dist*100))
    dtc[start:stop] = 3 + j*(lanewidth/6)
    plt.plot(x, dtc)

title = 'test'
plt.title(title)
ax = plt.gca()
plt.gca().invert_yaxis()

fname = 'ana1/example_' + title + '.pdf'
#plt.savefig(str(fname))

ax2 = ax.twinx()

ax2.set_ylabel('steering angle')
plt.show()


# %% save to file
pd.DataFrame.from_dict(output).T.to_csv('ana1/analysis.csv')

# %%
