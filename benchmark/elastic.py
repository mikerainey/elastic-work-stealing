#!/usr/bin/env python

# ===========================================
# Elastic task scheduling benchmarking script
# ===========================================

# Imports
# =======

import sys, time, shutil, glob, argparse, psutil, pathlib, fnmatch, os, tempfile
import simplejson as json
from copy import deepcopy
sys.setrecursionlimit(150000)

from flexibench import table as T, benchmark as B, query as Q

# Parameters
# ==========

timestr = time.strftime("%Y-%m-%d-%H-%M-%S")

default_results_path = 'results-' + timestr

taskparts_home = '../../'
path_to_benchmarks = '../../nix-packages/result/'

# default setting for the nb of worker threads to be used by taskparts
# (can be overridden by -num-workers); should be the count of the
# number of cores in the calling system
sys_num_workers = psutil.cpu_count(logical=False)

default_num_workers = sys_num_workers

path_to_binaries = path_to_benchmarks + '/examples/'
path_to_infiles = os.getcwd() + '/../../../infiles'

benchmark_key = 'benchmark'
benchmark_inputs = {
    'quickhull': T.mk_table1('n', 100000000),
    'samplesort': T.mk_table1('n', 100000000),
    'fast_fourier_transform': T.mk_table1('n', 100000000)
}

few_benchmarks = [ 'quickhull', 'samplesort' ]

parser = argparse.ArgumentParser('Benchmark elastic task scheduling')
parser.add_argument('--few_benchmarks', dest ='few_benchmarks',
                    action ='store_true',
                    help = ('run only benchmarks ' + str(few_benchmarks)))
parser.add_argument('-results_path',
                    help = 'path to a folder in which to generate results files; default: ' +
                    default_results_path)
args = parser.parse_args()

benchmark_inputs = benchmark_inputs if not(args.few_benchmarks) else dict(filter(lambda kv: kv[0] in few_benchmarks, benchmark_inputs.items()))

# Key types
# --------- 

ranked_command_line_arg_keys = {'n': 0}

# keys whose associated values are to be passed as environment
# variables
env_arg_keys = []
# keys that are not meant to be passed at all (just for annotating
# rows)
silent_keys = [ ]

prog_keys = [ benchmark_key ]

def is_prog_key(k):
    return (k in prog_keys)
def is_silent_key(k):
    return (k in silent_keys)
def is_env_arg_key(k):
    return (k in env_arg_keys) and not(is_prog_key(k))
def is_ranked_command_line_arg_key(k):
    return (k in ranked_command_line_arg_keys)
def is_command_line_arg_key(k):
    return not(is_silent_key(k)) and not(is_env_arg_key(k)) and not(is_prog_key(k)) and not(is_ranked_command_line_arg_key(k))

# Benmchmark runs
# ===============

#  given a row, specifies the path of the program to be run
def program_of_row(row):
    assert(benchmark_key in row)
    return row[benchmark_key]

def virtual_run_benchmarks_of_rows(rows):
    i = 1
    n = len(rows)
    for row in rows:
        br_i = B.run_of_row(row,
                            program_of_row = program_of_row,
                            is_command_line_arg_key = is_command_line_arg_key,
                            is_env_arg_key = is_env_arg_key,
                            rank_of_command_line_arg_key =
                            lambda k: None if not(is_ranked_command_line_arg_key(k)) else ranked_command_line_arg_keys[k])
        i += 1

stats_info = {
    'PARLAYLIB_TIMER_OUTFILE': {'results': [], 'tmpfile': 'timer.txt', 'jsonfile': 'timer.json'},
    'TASKPARTS_STATS_OUTFILE': {'results': [], 'tmpfile': 'stats.txt', 'jsonfile': 'stats.json'}
}

def run_benchmark(br, stats0 = stats_info,
                  cwd = None, timeout_sec = None, verbose = True):
    stats = deepcopy(stats0)
    br_i = deepcopy(br)
    # generate a temporary file in which to store the stats output
    for k in stats:
        stats_fd, stats_path = tempfile.mkstemp(suffix = '.json', text = True)
        stats[k]['path'] = stats_path
        os.close(stats_fd)
    # let the taskparts runtime know about the temporary file above
    br_i['benchmark_run']['env_args'] += [{'var': k, 'val': stats[k]['path']} for k in stats]
    # set up other taskparts parameters
    if verbose:
        print(B.string_of_benchmark_run(br))
    # run the benchmark
    br_o = B.run_benchmark(br_i, cwd, timeout_sec, verbose = False)
    # collect the stats output of the benchmark run
    for k in stats:
        results = []
        stats_path = stats[k]['path']
        if os.stat(stats_path).st_size != 0:
            results = json.load(open(stats_path, 'r'))
        stats[k]['results'] = results
        # remove the temporary file we used for the stats output
        open(stats_path, 'w').close()
        os.unlink(stats_path)
    return {'stats': stats, 'trace': br_o}

def merge_dictionaries(dict1, dict2):
    merged_dict = dict1.copy()
    merged_dict.update(dict2)
    return merged_dict

traces_tmp_file = 'traces.txt'

def json_of_tmp_file(results_txt, results_json):
    with open(results_txt, 'r') as results:
        lines = results.readlines()
        ds = []
        for line in lines:
            ds.append(json.loads(line))
        with open(results_json, 'w') as results:
            results.write(json.dumps(ds, indent=2))
            print('Emitted ' + results_json)

def run_benchmarks():
    if os.path.exists(traces_tmp_file):
        os.remove(traces_tmp_file)
    for k,v in stats_info.items():
        if os.path.exists(v['tmpfile']):
            os.remove(v['tmpfile'])
    rows = T.rows_of(T.mk_append([T.mk_cross2(T.mk_table1(benchmark_key, b), benchmark_inputs[b])
                                  for b in benchmark_inputs]))
    virtual_run_benchmarks_of_rows(rows)
    for row in rows:
        br_i = B.run_of_row(row,
                            program_of_row = program_of_row,
                            is_command_line_arg_key = is_command_line_arg_key,
                            is_env_arg_key = is_env_arg_key,
                            rank_of_command_line_arg_key =
                            lambda k: None if not(is_ranked_command_line_arg_key(k)) else ranked_command_line_arg_keys[k])
        b = run_benchmark(br_i, cwd = path_to_binaries)
        timestamp = b['trace']['benchmark_run']['timestamp']
        with open(traces_tmp_file, 'a') as traces:
            traces.write(json.dumps(b['trace']) + '\n')
        for k,v in stats_info.items():
            with open(v['tmpfile'], 'a') as results:
                for r in b['stats'][k]['results']:
                    r['timestamp'] = timestamp
                    results.write(json.dumps(merge_dictionaries(row, r)) + '\n')
    for k,v in stats_info.items():
        json_of_tmp_file(v['tmpfile'], v['jsonfile'])
    json_of_tmp_file(traces_tmp_file, 'traces.json')

run_benchmarks()
