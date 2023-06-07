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
parlaylib_num_workers_key = 'PARLAY_NUM_THREADS'

path_to_binaries = path_to_benchmarks + '/examples/'
path_to_infiles = os.environ.get('INFILES_PATH')

def get_executables(folder):
    executables = []
    
    for root, dirs, files in os.walk(folder):
        for file in files:
            filepath = os.path.join(root, file)
            if os.access(filepath, os.X_OK):
                executables.append(filepath)
    
    return executables

input_types = {
    "<n>",
    "<num_points>",
    "<size>",
    "<num_vertices>",
    "<filename>",
    "<search_string>"
}

benchmark_key = 'benchmark'
# benchmark_inputs = {
#     'quickhull': T.mk_table1('n', 100000000),
#     'samplesort': T.mk_table1('n', 100000000),
#     'fast_fourier_transform': T.mk_table1('n', 100000000)
# }

benchmark_inputs_init = { os.path.basename(p): T.mk_unit() for p in get_executables(path_to_binaries) }

benchmark_inputs = { os.path.basename(p): T.mk_unit() for p in get_executables(path_to_binaries) }
benchmark_inputs['suffix_array'] = T.mk_table1('filename', 'chr22.dna')
benchmark_inputs['decision_tree_c45'] = T.mk_cross2(T.mk_table1('filename', 'covtype.data.test'),
                                                    T.mk_table1('filename', 'kddcup.data.test'))
benchmark_inputs['tokens'] = T.mk_table1('filename', 'wikisamp.xml')
benchmark_inputs['radix_tree'] = T.mk_table1('filename', 'wikisamp.xml')
benchmark_inputs['suffix_tree'] = T.mk_table1('filename', 'wikisamp.xml')
benchmark_inputs['longest_repeated_substring'] = T.mk_table1('filename', 'wikisamp.xml')
benchmark_inputs['lasso_regression'] = T.mk_table1('filename', 'finance1000.lasso.txt')
benchmark_inputs['rabin_karp'] = T.mk_cross2(T.mk_table1('search_string', 'xxx'),
                                             T.mk_table1('textfilename', 'wikisamp.xml'))
benchmark_inputs['knuth_morris_pratt'] = T.mk_cross2(T.mk_table1('search_string', 'xxx'),
                                                     T.mk_table1('textfilename', 'orkut.adj'))
benchmark_inputs['bigint_add'] = T.mk_table1('n', 8000000000)
benchmark_inputs['word_counts'] = T.mk_cross2(T.mk_table1('n', 5), T.mk_table1('textfilename', 'wikisamp.xml'))

benchmark_overrides = {'word_counts', 'min_spanning_tree', 'bigint_add', 'find_if'}

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

ranked_command_line_arg_keys = {'n': 0, 'filename': 0, 'search_string': 0, 'textfilename': 1}

# keys whose associated values are to be passed as environment
# variables
env_arg_keys = [parlaylib_num_workers_key]
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

def run_benchmarks(rows, stats_info, traces_outfile, timeout_sec = None):
    if os.path.exists(traces_tmp_file):
        os.remove(traces_tmp_file)
    for k,v in stats_info.items():
        if os.path.exists(v['tmpfile']):
            os.remove(v['tmpfile'])
    virtual_run_benchmarks_of_rows(rows)
    for row in rows:
        br_i = B.run_of_row(row,
                            program_of_row = program_of_row,
                            is_command_line_arg_key = is_command_line_arg_key,
                            is_env_arg_key = is_env_arg_key,
                            rank_of_command_line_arg_key =
                            lambda k: None if not(is_ranked_command_line_arg_key(k)) else ranked_command_line_arg_keys[k])
        b = run_benchmark(br_i, cwd = path_to_binaries, timeout_sec = timeout_sec)
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
    json_of_tmp_file(traces_tmp_file, traces_outfile)

rows = T.rows_of(T.mk_append([T.mk_cross2(T.mk_table1(benchmark_key, b), benchmark_inputs_init[b])
                              for b in benchmark_inputs_init]))
run_benchmarks(rows, {}, 'traces.json')

# Classify all benchmarks by their inputs
# ---------------------------------------

inputs = {}

with open('traces.json', 'r') as traces:
    tr = json.load(traces)
    for r in tr:
        s = r['benchmark_run']['stdout'].split()
        if len(s) < 3:
            i = 0
        else:
            i = 1
        b = s[i]
        cl1 = s[i+1]
        if cl1 in inputs:
            inputs[cl1].append(b)
        else:
            inputs[cl1] = [b]

benchmarks_with_int_input = inputs['<n>'] + inputs['<num_points>'] + inputs['<size>'] + inputs['<num_vertices>']

#print(benchmarks_with_int_input)
#benchmarks_with_int_input = ['filter']

# Find good input sizes
# ---------------------

target_time = 1.0
failed_inputs = {}

# keep doubling input size n until the wall-clock time of the benchmark exceeds target_time seconds
for b in benchmarks_with_int_input:
    if b in benchmark_overrides:
        continue
    lg = 0
    n = -1
    t = 0.0
    while t < target_time:
        if lg >= 32:
            failed_inputs[b] = t
            break
        n = 2 ** lg
        rows = T.rows_of(T.mk_append([T.mk_cross([T.mk_table1(benchmark_key, b), T.mk_table1('n', n),
                                                  T.mk_table1(parlaylib_num_workers_key, 1)])]))
        run_benchmarks(rows, stats_info, 'traces.json', timeout_sec = 600.0)
        lg += 1
        with open('timer.json', 'r') as timer:
            r = json.load(timer) # load the timer results of the benchamrk
            if len(r) == 0:
                continue
            l = r[-1] # get the last timer result in the series
            t = l['exectime'] # get the wall-clock time
            print('exectime ' + str(t))
    benchmark_inputs[b] = T.mk_table1('n', n)
    print(n)

benchmarks_info = 'benchmarks.json'
with open(benchmarks_info, 'w') as benchmarks:
    benchmarks.write(json.dumps(benchmark_inputs, indent=2))

with open('failed.json', 'w') as failed:
    failed.write(json.dumps(failed_inputs, indent=2))
print(failed_inputs)
