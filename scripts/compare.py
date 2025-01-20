#!/usr/bin/env python3

import sys
import re

logfile_1 = sys.argv[1]
logfile_2 = sys.argv[2]
if len(sys.argv) > 3:
    mintime = float(sys.argv[3])
else:
    mintime = 0.01
if len(sys.argv) > 4:
    minchange = float(sys.argv[4])
else:
    minchange = 0.035

testname = re.compile(r'^[^\[]*\[(?P<name>.*)\]$')

def readName(input):
    r = testname.match(input)
    if r is None:
        return input
    else:
        return r.group("name")

def readData(file_name):
    data_lines = []
    with open(file_name, 'r') as file_data:
        for line in file_data:
            if ' call     ' in line:
                data_line = [l for l in line.strip('\n').split(' ') if l != '']
                time = float(data_line[0][:-1])
                test = readName(data_line[2])
                if time > 0.5:
                    data_lines.append((test, time))
    return data_lines

def longestCommonPrefix(my_str):
    if not my_str:
        return ''
    prefix = my_str[0]
    for word in my_str:
        if len(prefix) > len(word):
            prefix, word = word, prefix
        while len(prefix) > 0:
            if word[:len(prefix)] == prefix:
                break
            else:
                prefix = prefix[:-1]
    return prefix

def getKeys(file_name1, file_name2):
    prefix = longestCommonPrefix([file_name1, file_name2])
    return file_name1[len(prefix):].rstrip(".log"), file_name2[len(prefix):].rstrip(".log")

data_entries = {}
key1, key2 = getKeys(logfile_1, logfile_2)
data_entries[key1] = { test : time for (test, time) in readData(logfile_1) }
data_entries[key2] = { test : time for (test, time) in readData(logfile_2) }
common_passing_tests = [k1 for k1 in data_entries[key1] if k1 in data_entries[key2]]

def filterEntries(test, t1, t2):
    return (t1 != t2)                                                   \
       and (t1 > mintime or t2 > mintime)                               \
       and ((t1 / t2) - 1.0 > minchange or (t2 / t1) - 1.0 > minchange)

# test , time1 , time2 , time1 / time2
final_table = []
for test in common_passing_tests:
    time1 = data_entries[key1][test]
    time2 = data_entries[key2][test]
    if filterEntries(test, time1, time2):
        final_table.append((test, time1, time2, time1 / time2))
final_table.sort(key = lambda e: e[3])
if len(final_table) == 0:
    sys.exit(0)
total_time_1 = sum([e[1] for e in final_table])
total_time_2 = sum([e[2] for e in final_table])
if total_time_2 > 0.0:
    final_table.append(('TOTAL', total_time_1, total_time_2, total_time_1 / total_time_2))
final_table = [ (str(e0), str(e1), str(e2), str(e3)) for (e0, e1, e2, e3) in final_table ]
final_table.insert(0, ('Test', key1 + ' time', key2 + ' time', '(' + key1 + '/' + key2 + ') time'))
final_table.insert(1, ('', '', '', ''))

column_width0 = max([len(c[0]) for c in final_table])
column_width1 = max([len(c[1]) for c in final_table])
column_width2 = max([len(c[2]) for c in final_table])
column_width3 = max([len(c[3]) for c in final_table])

def mkColumn(c, w):
    return ' ' + c.ljust(w) + ' '

columns = ['|' + '|'.join((mkColumn(c0, column_width0), mkColumn(c1, column_width1), mkColumn(c2, column_width2), mkColumn(c3, column_width3))) for (c0, c1, c2, c3) in final_table]
columns[1] = columns[1].replace(' ', '-')

print('\n'.join(columns))
