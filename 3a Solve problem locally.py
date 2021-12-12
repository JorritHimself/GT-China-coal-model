# Packages
import sys # For writing solution to file
#import os.path
#from os import path
import os
# To read in problem file from lp file
from mip import *
import zipfile
  
# Extract problem from zip
path_to_zip_file = './3 problemfiles/prob v12 baseline 2019.zip'
with zipfile.ZipFile(path_to_zip_file, 'r') as zip_ref:
    zip_ref.extractall('./3 problemfiles')
# Read problem
problpfile = './3 problemfiles/prob v12 baseline 2019.lp'
# Write solution to
solutionoutputfile = './4 solutionfiles/solution v12 baseline 2019.txt'

### Define problem
cn_coal_problem = Model(sense=MINIMIZE, solver_name=CBC)
cn_coal_problem.max_mip_gap = 0.0001
cn_coal_problem.max_mip_gap_abs = 0.1
### Read in the problem file
cn_coal_problem.read(problpfile)
### Solve the problem
status = cn_coal_problem.optimize(max_seconds=6000, max_seconds_same_incumbent=180)

# Print solution when optimal
if status == OptimizationStatus.OPTIMAL:
    print("Total prod and transport costs are (mln USD) = {}".format(cn_coal_problem.objective_value))

# Print optimal flows along each of the edges summary: output limited to lines that are non-zero
original_stdout = sys.stdout
with open(solutionoutputfile, 'w') as f:
    sys.stdout = f
    for v in cn_coal_problem.vars:
       if abs(v.x) > 1e-9: # only printing non-zeros
            print(v.name, "=", v.x) 
sys.stdout = original_stdout
  
# Delete unzipped problem file
os.remove(problpfile)