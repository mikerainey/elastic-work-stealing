import json
import pandas as pd
import os

# Specify the folder containing the JSON files
folder = '2024-07-29_19-41-03'

# Load JSON data from files
file_names = ['homegrown-ne_timer.json', 'homegrown_timer.json', 'taskparts-ne_timer.json', 'taskparts_timer.json']
data = {}

for file in file_names:
    file_path = os.path.join(folder, file)
    with open(file_path, 'r') as f:
        data[file] = json.load(f)

# Prepare dataframes for elapsed and usertime
elapsed_df = pd.DataFrame(columns=['Benchmark', 'homegrown_ne', 'taskparts_ne', 'homegrown', 'taskparts'])
usertime_df = pd.DataFrame(columns=['Benchmark', 'homegrown_ne', 'taskparts_ne', 'homegrown', 'taskparts'])

# Helper function to process the data and fill the dataframes
def process_data(key):
    benchmarks = {}
    
    for file, runs in data.items():
        for run in runs:
            benchmark = run['benchmark']
            value = run[key]
            if benchmark not in benchmarks:
                benchmarks[benchmark] = {'homegrown-ne': float('inf'), 'homegrown': float('inf'), 'taskparts-ne': float('inf'), 'taskparts': float('inf')}
                
            if 'homegrown-ne' in file:
                benchmarks[benchmark]['homegrown-ne'] = min(benchmarks[benchmark]['homegrown-ne'], value)
            elif 'homegrown' in file:
                benchmarks[benchmark]['homegrown'] = min(benchmarks[benchmark]['homegrown'], value)
            elif 'taskparts-ne' in file:
                benchmarks[benchmark]['taskparts-ne'] = min(benchmarks[benchmark]['taskparts-ne'], value)
            elif 'taskparts' in file:
                benchmarks[benchmark]['taskparts'] = min(benchmarks[benchmark]['taskparts'], value)
    
    return benchmarks

elapsed_benchmarks = process_data('exectime')
usertime_benchmarks = process_data('usertime')

# Fill dataframes
elapsed_rows = []
for benchmark, values in elapsed_benchmarks.items():
    elapsed_rows.append({'Benchmark': benchmark, 'homegrown_ne': values['homegrown-ne'], 'taskparts_ne': values['taskparts-ne'], 'homegrown': values['homegrown'], 'taskparts': values['taskparts']})
elapsed_df = pd.concat([elapsed_df, pd.DataFrame(elapsed_rows)], ignore_index=True)

usertime_rows = []
for benchmark, values in usertime_benchmarks.items():
    usertime_rows.append({'Benchmark': benchmark, 'homegrown_ne': values['homegrown-ne'], 'taskparts_ne': values['taskparts-ne'], 'homegrown': values['homegrown'], 'taskparts': values['taskparts']})
usertime_df = pd.concat([usertime_df, pd.DataFrame(usertime_rows)], ignore_index=True)

# Function to convert dataframe to LaTeX table
def dataframe_to_latex(df, title):
    latex_table = "\\begin{table}[H]\n\\centering\n"
    latex_table += "\\begin{tabular}{|l|c|c||c|c|}\n"
    latex_table += "\\hline\n"
    latex_table += " & \\multicolumn{2}{|c||}{non-elastic} & \\multicolumn{2}{c|}{elastic} \\\\\n"
    latex_table += "\\hline\n"
    latex_table += "Benchmark & homegrown & taskparts & homegrown & taskparts \\\\\n"
    latex_table += "\\hline\n"
    
    for index, row in df.iterrows():
        latex_table += f"{row['Benchmark']} & {row['homegrown_ne']} & {row['taskparts_ne']} & {row['homegrown']} & {row['taskparts']} \\\\\n"
    
    latex_table += "\\hline\n"
    latex_table += "\\end{tabular}\n"
    latex_table += f"\\caption{{{title}}}\n"
    latex_table += "\\end{table}\n"
    
    return latex_table

# Generate LaTeX tables
elapsed_latex = dataframe_to_latex(elapsed_df, 'Elapsed Time Results')
usertime_latex = dataframe_to_latex(usertime_df, 'Usertime Results')

# Print LaTeX tables
print(elapsed_latex)
print(usertime_latex)
