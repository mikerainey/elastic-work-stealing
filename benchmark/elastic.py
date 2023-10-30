#!/usr/bin/env python

# ===========================================
# Elastic task scheduling benchmarking script
# ===========================================

# Imports
# =======

import sys, time, shutil, glob, argparse, psutil, pathlib, fnmatch, os, tempfile
import simplejson as json
from copy import deepcopy
from datetime import datetime
sys.setrecursionlimit(150000)

from flexibench import table as T, benchmark as B, query as Q

# Parameters
# ==========

timestr = time.strftime("%Y-%m-%d-%H-%M-%S")

default_results_path = 'results-' + timestr

taskparts_home = '../../'
path_to_binaries = os.environ.get('PARLAY_SERIAL')

# default setting for the nb of worker threads to be used by taskparts
# (can be overridden by -num-workers); should be the count of the
# number of cores in the calling system
sys_num_workers = psutil.cpu_count(logical=False)

default_num_workers = sys_num_workers
parlaylib_num_workers_key = 'PARLAY_NUM_THREADS'
taskparts_num_workers_key = 'TASKPARTS_NUM_WORKERS'

path_to_infiles = os.environ.get('INFILES_PATH') + '/'

custom_malloc_ld_preload_key="LD_PRELOAD"
default_custom_malloc_ld_preload=os.environ.get('JEMALLOC_PRELOAD_PATH')

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
    "<filename>",
    "<search_string>"
}

benchmark_key = 'benchmark'
# benchmark_inputs = {
#     'quickhull': T.mk_table1('n', 100000000),
#     'samplesort': T.mk_table1('n', 100000000),
#     'fast_fourier_transform': T.mk_table1('n', 100000000)
# }

def with_infile_prefix(f):
    return path_to_infiles + f

benchmark_inputs_init = { os.path.basename(p): T.mk_unit() for p in get_executables(path_to_binaries) }

graph_infiles0 = [with_infile_prefix('com_orkut_sym.adj')]
graph_infiles =  [with_infile_prefix(f) for f in ['com_orkut_sym.adj' 'livejournal_sym.adj', 'youtube_sym.adj', 'usa_road_sym.adj']]
mk_graph_infiles = T.mk_append([T.mk_table1('filename', f) for f in graph_infiles0])

wikisamp_infile = 'wikisamp.xml'
text_infiles = [with_infile_prefix(f) for f in [wikisamp_infile]]
mk_text_infiles = T.mk_append([T.mk_table1('filename', f) for f in text_infiles])

mk_text_search_inputs = T.mk_cross2(T.mk_table1('search_string', 'xxx'),
                                    T.mk_append([T.mk_table1('textfilename', f) for f in text_infiles]))

benchmark_inputs = { os.path.basename(p): T.mk_unit() for p in get_executables(path_to_binaries) }
benchmark_inputs['suffix_array'] = T.mk_table1('filename', with_infile_prefix('chr22.dna'))
benchmark_inputs['decision_tree_c45'] = T.mk_cross2(T.mk_table1('filename', with_infile_prefix('covtype.data.test')),
                                                    T.mk_table1('filename', with_infile_prefix('kddcup.data.test')))
benchmark_inputs['tokens'] = mk_text_infiles
benchmark_inputs['radix_tree'] = mk_text_infiles
benchmark_inputs['suffix_tree'] = mk_text_infiles
benchmark_inputs['longest_repeated_substring'] = mk_text_infiles
benchmark_inputs['rabin_karp'] = mk_text_search_inputs
benchmark_inputs['knuth_morris_pratt'] = mk_text_search_inputs
benchmark_inputs['word_counts'] = T.mk_cross2(T.mk_table1('n', 5),
                                              T.mk_table1('textfilename', with_infile_prefix('wikisamp.xml')))
benchmark_inputs['bigint_add'] = T.mk_table1('n', 8000000000)
benchmark_inputs['lasso_regression'] = T.mk_table1('filename', with_infile_prefix('finance1000.lasso.txt'))

benchmark_overrides = {'word_counts', 'bigint_add', 'find_if'}

few_benchmarks = [ 'quickhull', 'samplesort' ]
few_benchmarks = [ 'tokens', 'BFS' ]

parser = argparse.ArgumentParser('Benchmark elastic task scheduling')
parser.add_argument('--run_experiment', dest ='run_experiment',
                    action ='store_true', default=False,
                    help = ('run benchmarks'))
parser.add_argument('--commit_results', dest ='commit_results',
                    action ='store_true', default=False,
                    help = ('commit results to a new folder in ../results/'))
parser.add_argument('--few_benchmarks', dest ='few_benchmarks',
                    action ='store_true',
                    help = ('run only benchmarks ' + str(few_benchmarks)))
parser.add_argument('--only_parallel', dest ='only_parallel',
                    action ='store_true',
                    help = ('run only parallel benchmarks'))
parser.add_argument('-results_path',
                    help = 'path to a folder in which to generate results files; default: ' +
                    default_results_path)
parser.add_argument('-benchmarks_path', dest ='benchmarks_path',
                    help = ('path to the benchmarks json file'))
parser.add_argument('--need_input_generation', dest ='need_input_generation',
                    action ='store_true',
                    help = ('generate the file benchmarks.json, which stores the benchmark/input data'))
parser.add_argument('-target_time_secs',
                    dest ='target_time_secs', type=float, default=1.0,
                    help = ('target time for benchmarks to run'))
args = parser.parse_args()

if args.benchmarks_path != None:
    with open(args.benchmarks_path, 'r') as benchmarks:
        benchmark_inputs = json.load(benchmarks)

benchmark_inputs = benchmark_inputs if not(args.few_benchmarks) else dict(filter(lambda kv: kv[0] in few_benchmarks, benchmark_inputs.items()))

# Key types
# --------- 

ranked_command_line_arg_keys = {'n': 0, 'filename': 0, 'search_string': 0, 'textfilename': 1}

# keys whose associated values are to be passed as environment
# variables
env_arg_keys = [parlaylib_num_workers_key, taskparts_num_workers_key, custom_malloc_ld_preload_key]
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
    'TASKPARTS_STATS_OUTFILE': {'results': [], 'tmpfile': 'stats.txt', 'jsonfile': 'stats.json'},
    'PARLAYLIB_OVERALL_STATS_OUTFILE': {'results': [], 'tmpfile': 'overall.txt', 'jsonfile': 'overall.json'}
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
        print(B.string_of_benchmark_run(br, path_prefix=''))
    # run the benchmark
    br_o = B.run_benchmark(br_i, timeout_sec=timeout_sec, cwd=None, verbose = False, path_prefix='') # FIXME!!!!! cwd is bogus
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

def run_benchmarks(rows, stats_info, traces_outfile,
                   path_to_binaries = path_to_binaries, timeout_sec = None):
    if os.path.exists(traces_tmp_file):
        os.remove(traces_tmp_file)
    for k,v in stats_info.items():
        if os.path.exists(v['tmpfile']):
            os.remove(v['tmpfile'])
    virtual_run_benchmarks_of_rows(rows)
    for row in rows:
        br_i = B.run_of_row(row,
                            program_of_row = lambda p: path_to_binaries + '/' + program_of_row(p),
                            is_command_line_arg_key = is_command_line_arg_key,
                            is_env_arg_key = is_env_arg_key,
                            rank_of_command_line_arg_key =
                            lambda k: None if not(is_ranked_command_line_arg_key(k)) else ranked_command_line_arg_keys[k])
        b = run_benchmark(br_i, cwd = path_to_infiles, timeout_sec = timeout_sec)
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

# Experiment setup
# ================

# Classify all benchmarks by their inputs
# ---------------------------------------

# returns a list of benchmarks that take an integer input;
# also updates benchmark_inputs to identify all graph benchmarks which should take a graph file as input
def classify_benchmarks():
    rows = T.rows_of(T.mk_append([T.mk_cross2(T.mk_table1(benchmark_key, b), benchmark_inputs_init[b])
                                  for b in benchmark_inputs_init]))
    run_benchmarks(rows, {}, 'traces.json')
    inputs = {}
    with open('traces.json', 'r') as traces:
        tr = json.load(traces)
        for r in tr:
            s = r['benchmark_run']['stdout'].split()
            i = 0 if len(s) < 3 else 1
            b = s[i]
            cl1 = s[i+1]
            if len(s) > 3 and cl1 != '<search_string>' and s[1] != 'word_counts':
                # we hereby believe that b is a graph benchmark
                benchmark_inputs[b] = mk_graph_infiles
            elif b == 'filter_kruskal':
                 benchmark_inputs[b] = mk_graph_infiles
            elif cl1 in inputs:
                inputs[cl1].append(b)
            else:
                inputs[cl1] = [b]
    return inputs['<n>'] + inputs['<num_points>'] + inputs['<size>']

# Find good input sizes
# ---------------------

# keep doubling input size n until the wall-clock time of the benchmark exceeds target_time_secs seconds;
# stores results in benchmarks_outfile
def find_target_input_sizes(benchmark_inputs0, benchmarks_with_int_input,
                            target_time_secs=args.target_time_secs, benchmarks_outfile='benchmarks.json', failed_outfile='failed.json'):
    benchmark_inputs = deepcopy(benchmark_inputs0)
    failed_inputs = {}
    for b in benchmarks_with_int_input:
        if b in benchmark_overrides:
            continue
        lg = 0
        n = -1
        t = 0.0
        while t < target_time_secs:
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
    with open(benchmarks_outfile, 'w') as benchmarks:
        benchmarks.write(json.dumps(benchmark_inputs, indent=2))
    with open(failed_outfile, 'w') as failed:
        failed.write(json.dumps(failed_inputs, indent=2))

def mk_benchmarks(benchmark_inputs):
    return T.mk_append([T.mk_cross2(T.mk_table1(benchmark_key, b), benchmark_inputs[b])
                        for b in benchmark_inputs])

# Driver
# =====

if args.commit_results:
    # Define the directory path
    results_dir = '../results/'
    # Create a formatted timestamp
    timestamp = datetime.now().strftime('%Y-%m-%d_%H-%M-%S')
    # Create a new folder with the timestamp
    new_folder_path = os.path.join(results_dir, timestamp)
    os.makedirs(new_folder_path)
    # List all files in the current directory
    files = os.listdir()
    # Move files with the .json extension to the new folder
    for filename in files:
        if filename.endswith('.json') and filename != 'benchmarks.json':
            source_path = os.path.join(os.getcwd(), filename)
            destination_path = os.path.join(new_folder_path, filename)
            shutil.move(source_path, destination_path)
    # Verify that only benchmarks.json is left
    if 'benchmarks.json' in files:
        print("All .json files moved to the timestamped folder.")
    else:
        print("No .json files found to move.")
    # Optionally, list the contents of the results directory
    result_contents = os.listdir(results_dir)
    print("Contents of the results directory:")
    for item in result_contents:
        print(item)

if args.need_input_generation:
    benchmarks_with_int_input = classify_benchmarks()        
    find_target_input_sizes(benchmark_inputs, benchmarks_with_int_input)

mk_custom_malloc = T.mk_unit() if default_custom_malloc_ld_preload == None else T.mk_table1(custom_malloc_ld_preload_key, default_custom_malloc_ld_preload)

if args.run_experiment:
    procs = [sys_num_workers] + ([] if args.only_parallel else [1])
    parlay_infos = {
        'homegrown': {'binpath': os.environ.get('PARLAY_HOMEGROWN'),
                      'mk': T.mk_append([T.mk_table1(parlaylib_num_workers_key, p) for p in procs])},
        'taskparts': {'binpath': os.environ.get('PARLAY_TASKPARTS'),
                      'mk': T.mk_append([T.mk_table1(taskparts_num_workers_key, p) for p in procs])},
        'taskparts-ne': {'binpath': os.environ.get('PARLAY_TASKPARTS_NONELASTIC'),
                         'mk': T.mk_append([T.mk_table1(taskparts_num_workers_key, p) for p in procs])}
    }
    if not(args.only_parallel):
        parlay_infos['serial'] = {'binpath': os.environ.get('PARLAY_SERIAL'),
                                  'mk': T.mk_unit()}
    for k,v in parlay_infos.items():
        stats_info = {
            'PARLAYLIB_TIMER_OUTFILE': {'results': [], 'tmpfile': k+'_timer.txt', 'jsonfile': k+'_timer.json'},
            'TASKPARTS_STATS_OUTFILE': {'results': [], 'tmpfile': k+'_stats.txt', 'jsonfile': k+'_stats.json'},
            'PARLAYLIB_OVERALL_STATS_OUTFILE': {'results': [], 'tmpfile': k+'_overall.txt', 'jsonfile': k+'_overall.json'}
        }
        print('Running benchmarks for binary configuration: ' + k)
        with open('benchmarks.json', 'r') as benchmarks:
            rows = T.rows_of(T.mk_append([T.mk_cross([T.mk_table1('benchmark', k), vb, v['mk'], mk_custom_malloc])
                                          for k,vb in json.load(benchmarks).items()
                                          if not(args.few_benchmarks) or k in few_benchmarks]))
        run_benchmarks(rows, stats_info, k+'_traces.json',
                       path_to_binaries=v['binpath'], timeout_sec = 600.0)
