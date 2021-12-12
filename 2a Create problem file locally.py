# Packages
import pandas as pd
import numpy as np
import xlrd # Required dependency for pd.read_excel
import pulp
from pulp import * # PuLP modeller functions
import sys # For writing solution to file
import os.path
from os import path

# Year to run it for
selected_year = 2025

# Get input from
inputdatapath = './2 build'
# Write problem to
problpfile = './3 problemfiles/prob v12 y'+str(selected_year)+'.lp'

# input file names
edges_file = os.path.join(inputdatapath, "all edges vv plus costs and capa latest.csv")
coal_quality_file = os.path.join(inputdatapath, "coal qualities summary latest.csv")
demand_file = os.path.join(inputdatapath, "demand all latest.csv")
elec_capa_file = os.path.join(inputdatapath, "electric capacities latest.csv")
port_capa_file = os.path.join(inputdatapath, "port capacities latest.csv")
steel_capa_file = os.path.join(inputdatapath, "steel prod capacities latest.csv")
mines_file = os.path.join(inputdatapath, "prod capa cost price brand by mine latest.csv")

########################################################    END HEAD ##################
### Read in data from file
df_nodes = pd.read_csv(demand_file)
df_edges = pd.read_csv(edges_file)
df_supply = pd.read_csv(mines_file)
df_coaltypes = pd.read_csv(coal_quality_file)
df_elec_capa = pd.read_csv(elec_capa_file)
df_port_capa = pd.read_csv(port_capa_file)
df_steel_capa = pd.read_csv(steel_capa_file)
# keep data for selected year only
df_nodes = df_nodes[(df_nodes['year']==selected_year)]
df_edges = df_edges[(df_edges['year']==selected_year)]
df_supply = df_supply[(df_supply['year']==selected_year)]
df_elec_capa = df_elec_capa[(df_elec_capa['year']==selected_year)]
df_port_capa = df_port_capa[(df_port_capa['year']==selected_year)]
df_steel_capa = df_steel_capa[(df_steel_capa['year']==selected_year)]

### List of all the nodes
nodelist = df_nodes['node_id'].drop_duplicates(keep='first').to_list()
### Dictionaries with demand for electricity and each coking coal type by node
nodedata = df_nodes.set_index('node_id')
nodedata = nodedata[['elec_demand_PJ', 'steel_demand_Mt', 'other_demand_PJ']]
nodedata  = nodedata.T.to_dict('list')
(elec_demand_PJ, steel_demand_Mt, other_demand_PJ) = splitDict(nodedata) # Splits data dictionary so we can use variable names more easily in below problem definition

### List of all the edges
edgelist = df_edges[['orig_node_id', 'dest_node_id']]
edgelist = list(edgelist.itertuples(index=False, name=None))
### List of all edges with restricted capacity: this is what will be sued to define the transport capacity constraints
restricted_edgelist = df_edges.dropna(how='any', subset=['cap_Mt'])
restricted_edgelist = restricted_edgelist[['orig_node_id', 'dest_node_id']]
restricted_edgelist = list(restricted_edgelist.itertuples(index=False, name=None))
### List of all the edges between steel plants and provincial demand centers: this is what will be used to define the transport capacity constraints
steelplant_edgelist = df_edges.loc[df_edges['orig_node_type'] == "stpt"]
steelplant_edgelist = steelplant_edgelist[['orig_node_id', 'dest_node_id']]
steelplant_edgelist = list(steelplant_edgelist.itertuples(index=False, name=None))
### Edge data  
edgedata = df_edges
edgedata['orig_dest'] = list(zip(edgedata['orig_node_id'], edgedata['dest_node_id']))
edgedata = edgedata.set_index(['orig_dest'])
edgedata = edgedata[['transp_cost_tot_usd', 'transm_cost_usd_GJ', 'cap_Mt', 'conversion_eff']]
edgedata  = edgedata.T.to_dict('list')
(transp_cost_tot_usd, transm_cost_usd_GJ, cap_Mt, conversion_eff) = splitDict(edgedata) # Splits data dictionary so we can use variable names more easily in below problem definition

### List of all coal types
coaltypelist = df_coaltypes['coal_group'].dropna().to_list()
### Coal type data
coaltypedata = df_coaltypes
coaltypedata = coaltypedata.set_index(['coal_group'])
coaltypedata = coaltypedata[['coking_coal_t_HCC', 'coking_coal_t_SCC', 'coking_coal_t_PCI', 'CV_PJ_p_Mt_therm']]
# keep only distinct coal quality group names
coaltypedata = coaltypedata.drop_duplicates(keep='first')
coaltypedata  = coaltypedata.T.to_dict('list')
(coking_coal_t_HCC, coking_coal_t_SCC, coking_coal_t_PCI, CV_PJ_p_Mt_therm) = splitDict(coaltypedata)

### List of all port capacities 
portlist = df_port_capa['node_id'].dropna().to_list()
### Port capacity data
portdata = df_port_capa.set_index(['node_id'])
portdata = portdata[['cap_Mt', 'port_cap_Mt']]
portdata  = portdata.T.to_dict('list')
(cap_Mt_port_notused, port_cap_Mt) = splitDict(portdata)

### List of all electric capacities 
elec_capa_list = df_elec_capa[['orig_node_id', 'dest_node_id']]
elec_capa_list = list(elec_capa_list.itertuples(index=False, name=None))
### Electric capacity data
elec_capa_data = df_elec_capa
elec_capa_data['orig_dest'] = list(zip(elec_capa_data['orig_node_id'], elec_capa_data['dest_node_id']))
elec_capa_data = elec_capa_data.set_index(['orig_dest'])
elec_capa_data = elec_capa_data[['transm_cost_usd_GJ', 'cap_PJ_corrected']]
elec_capa_data = elec_capa_data.T.to_dict('list')
# Split data dictionary so we can use variable names more easily in below problem definition
(transm_cost_usd_GJ_notused, cap_PJ_corrected) = splitDict(elec_capa_data) 

### List of all steel plant capacities 
steelplantlist = df_steel_capa['node_id'].dropna().to_list()
### Steel plant capacity data
steelplant_data = df_steel_capa
steelplant_data = steelplant_data.drop(['node_name', 'year'], axis = 1) 
steelplant_data = steelplant_data.set_index(['node_id'])
steelplant_data  = steelplant_data.T.to_dict('list')
(steel_cap_Mt, steel_cap_Mt_corrected) = splitDict(steelplant_data)
### List of all provincial steel demand centres
steel_dc_list = df_nodes.loc[df_nodes['steel_demand_Mt'] != 0]
steel_dc_list = steel_dc_list['node_id'].dropna().to_list()

### Supply list: expand to combinations of all nodes and coaltypes
## Create full list of nodes and coal types, with unique and non-missing values
nodelist_temp_df = df_nodes[['node_id', 'elec_demand_PJ']] # Need to keep 2 vars for maintaining df structure
coaltypelist_temp_df = df_coaltypes[['coal_group', 'CV_PJ_p_Mt_therm']] # Need to keep 2 vars for maintaining df structure
nodelist_temp_df.insert(2, 'mergevar', 1, allow_duplicates= True)
# keep only distinct coal quality group names
coaltypelist_temp_df = coaltypelist_temp_df.drop_duplicates(keep='first')
coaltypelist_temp_df.insert(2, 'mergevar', 1, allow_duplicates= True)
supplylist = pd.merge(nodelist_temp_df, coaltypelist_temp_df, how='outer', on='mergevar')
supplylist_temp = supplylist.drop(['elec_demand_PJ', 'CV_PJ_p_Mt_therm', 'mergevar'], axis = 1) 

# Expand supply limits to all combinations of nodes and coal types
supplydata = df_supply
supplydata = pd.merge(supplylist_temp, supplydata, how='left', on=['node_id', 'coal_group']).fillna(value=0)
supplydata['indextemp'] = list(zip(supplydata['node_id'], supplydata['coal_group']))
supplydata = supplydata.set_index(['indextemp'])
supplydata = supplydata[['prod_capa_Mt', 'total_cash_cost_ex_transp', 'c1_cash_cost_ex_transp']]
supplydata  = supplydata.T.to_dict('list')
# Split data dictionary so we can use variable names more easily in below problem definition
(prod_capa_Mt, total_cash_cost_ex_transp, c1_cash_cost_ex_transp) = splitDict(supplydata) 
# And only now make (unique) list out of the supplylist
supplylist = list(supplylist_temp.itertuples(index=False, name=None)) 

### Flows list: expand to combinations of all edges and coaltypes
flowslist = df_edges[['orig_node_id', 'dest_node_id']]
flowslist.insert(2, 'mergevar', 1, allow_duplicates= True)
flowslist = pd.merge(flowslist, coaltypelist_temp_df, how='outer', on='mergevar')
flowslist = flowslist[['orig_node_id', 'dest_node_id', 'coal_group']]
flowslist = list(flowslist.itertuples(index=False, name=None))

### Electrical edge flows list: keep only combinations of edges and coaltypes for the UHV links
# This is used in the optimization, to calculate the 
uhvflowslist = df_edges[['orig_node_id', 'dest_node_id', 'orig_node_type', 'dest_node_type']]
uhvflowslist = uhvflowslist.loc[(uhvflowslist['orig_node_type']== "eldc") & (uhvflowslist['dest_node_type']== "eldc")]
uhvflowslist.insert(2, 'mergevar', 1, allow_duplicates= True)
uhvflowslist = pd.merge(uhvflowslist, coaltypelist_temp_df, how='outer', on='mergevar')
uhvflowslist = uhvflowslist[['orig_node_id', 'dest_node_id', 'coal_group']]
uhvflowslist = list(uhvflowslist.itertuples(index=False, name=None))

### UHV line transmission cost data
transm_cost_data = df_edges[['orig_node_id', 'dest_node_id','orig_node_type', 'dest_node_type', 'transm_cost_usd_GJ', 'conversion_eff']]
transm_cost_data = transm_cost_data.loc[(transm_cost_data['orig_node_type']== "eldc") & (transm_cost_data['dest_node_type']== "eldc")]
transm_cost_data['orig_dest'] = list(zip(transm_cost_data['orig_node_id'], transm_cost_data['dest_node_id']))
transm_cost_data = transm_cost_data.set_index(['orig_dest'])
transm_cost_data = transm_cost_data[['transm_cost_usd_GJ', 'conversion_eff']]
transm_cost_data = transm_cost_data.T.to_dict('list')
# Split data dictionary so we can use variable names more easily in below problem definition
(transm_cost_usd_GJ, conv_eff_notused) = splitDict(transm_cost_data) 

##################   MODEL FORMULATION SECTION ###########################################
##### time stamp: start build    
now = datetime.now()
start_build_time = now.strftime("%H:%M:%S")
# print note when building started
print("Started building at ", start_build_time)
##### Define the problem variable and optimizaton type    
cn_coal_problem = LpProblem("china_coal_cost_problem",LpMinimize)

# Create problem variables, define flows as non-negative here (needed to prevent unidirectional edges used to transport both ways)
massflowtot = LpVariable.dicts("flow_total",edgelist,lowBound = 0,upBound=None,cat=LpContinuous)
massflowtype = LpVariable.dicts("flow_by_coaltype",flowslist,lowBound = 0,upBound=None,cat=LpContinuous)
supplyitembynode = LpVariable.dicts("supply_coaltype_by_node",supplylist,lowBound = 0,upBound=None,cat=LpContinuous)

# Constraint: Mass Balance pt1: 
# Nodes cannot supply coal types they do not have
for item in supplylist:
    supplyitembynode[item].bounds(0, prod_capa_Mt[item])

# Constraint: Mass Balance pt2: 
# Amount of each coal type flowing out of each node cannot exceed supply plus amount flowing into of each node
# Note: demand in each node here is irrelevant, as nodes demand GJ not tons of some type of coal
for item in supplylist:
    cn_coal_problem += (supplyitembynode[item]+ 
                        lpSum([massflowtype[(i,j,ct)] for (i,j,ct) in flowslist if (j,ct) == item]) >=
                        lpSum([massflowtype[(i,j,ct)] for (i,j,ct) in flowslist if (i,ct) == item]))

# Constraint: Energy Balance: energy demand in each node must be satisfied:
# supply of each coal type in each node multiplied with energy content of that coaltype +
# flows of each coal type into that node multiplied with energy content of that coaltype >=
# energy demand for electricity generation + energy demand for other purposes within that node +
# flows of each coal type out of that node multiplied with energy content of that coaltype
# Note: Flows are multiplied with conversion efficiency, which is 1 for all links except power plant unit to electricity demand centers and UHV line connections
# Note: UHV line connectiosn carry power, not coal, but for the model that doesn't matter
for node in nodelist:
    cn_coal_problem += (lpSum([supplyitembynode[(i,ct)]*CV_PJ_p_Mt_therm[ct] for (i,ct) in supplylist if i == node]) + 
                        lpSum([massflowtype[(i,j,ct)]*CV_PJ_p_Mt_therm[ct]*conversion_eff[i,j] for (i,j,ct) in flowslist if j == node]) >= 
                        elec_demand_PJ[node] + 
                        other_demand_PJ[node] + 
                        lpSum([massflowtype[(i,j,ct)]*CV_PJ_p_Mt_therm[ct]*conversion_eff[i,j] for (i,j,ct) in flowslist if i == node]))

# Constraint: Coking coal demand in each steel demand node must be satisfied, pt1: HCC:
# Every ton of steel will need 0.599 t of Hard Coking Coal
# Demand of Soft Coking Coal and PCI are tied with this demand for HCC
# This is to make sure every steel plant uses the right mix of HCC/SCC/PCI, and that a provincial demand center does not get all its HCC from via one steelplant, and SCC from another
for node in steel_dc_list:
    cn_coal_problem += (lpSum([massflowtype[(i,j,ct)]*coking_coal_t_HCC[ct] for (i,j,ct) in flowslist if j == node]) >= 
                        steel_demand_Mt[node]*0.581)

# Constraint: Coking coal demand in each steel demand node must be satisfied, pt2: SCC:
# Hard coking coal, soft coking coal and PCI must be of a mix of 0.599 : 0.182 : 0.185 (as volumes for one tonne of steel)
# This is done by making sure the flows over each edge from steel plant to provincial steel demand center are of that mix.
# This is to prevent the provincial level steel demand centers getting their hard coking coal from one steelplant, and soft coking coal from another
for edge in steelplant_edgelist:
    cn_coal_problem += (lpSum([massflowtype[(i,j,ct)]*coking_coal_t_HCC[ct]/0.581 for (i,j,ct) in flowslist if (i, j) == edge]) == 
                        lpSum([massflowtype[(i,j,ct)]*coking_coal_t_SCC[ct]/0.176 for (i,j,ct) in flowslist if (i, j) == edge]))

# Constraint: Coking coal demand in each steel demand node must be satisfied, pt3: PCI:
# Hard coking coal, soft coking coal and PCI must be of a mix of 0.599 : 0.182 : 0.185 (as volumes for one tonne of steel)
# This is done by making sure the flows over each edge from steel plant to provincial steel demand center are of that mix.
# This is to prevent the provincial level steel demand centers getting their hard coking coal from one steelplant, and soft coking coal from another
for edge in steelplant_edgelist:
    cn_coal_problem += (lpSum([massflowtype[(i,j,ct)]*coking_coal_t_HCC[ct]/0.581 for (i,j,ct) in flowslist if (i, j) == edge]) == 
                        lpSum([massflowtype[(i,j,ct)]*coking_coal_t_PCI[ct]/0.179 for (i,j,ct) in flowslist if (i, j) == edge]))

# Constraint: transport capacity of each edge cannot be exceeded
# Total flows of all types of coal over each edge cannot exceed the edges' transport capacity
for edge in restricted_edgelist:
    cn_coal_problem += (lpSum([massflowtype[(i,j,ct)] for (i,j,ct) in flowslist if (i, j) == edge]) <= cap_Mt[edge])

# Constraint: electrical transmission capacity of edges cannot be exceeded 
# Note that transmisison capacities only limited for edges from power plant unit to elec demand center and UHV lines
# Each edge basically has physical (Mt) and energy flows (GJ) capacities, but only one is constrained for any edge
for edge in elec_capa_list:
    cn_coal_problem += (lpSum([massflowtype[(i,j,ct)]*CV_PJ_p_Mt_therm[ct]*conversion_eff[i,j] for (i,j,ct) in flowslist if (i, j) == edge]) <= cap_PJ_corrected[edge])

# Constraint: port capacity of each node cannot be exceeded
# Constraint defned as maximum amount of coal (Mt) leaving a port cannot exceed its annual capacity
for node in portlist:
    cn_coal_problem += (lpSum([massflowtype[(i,j,ct)] for (i,j,ct) in flowslist if i == node]) <= port_cap_Mt[node])
    
# Constraint: production capacity of steel plant nodes cannot be exceeded
# Constraint defined as the equivalent maximum amount of hard coking coal consumed/transported
for node in steelplantlist:
    cn_coal_problem += (lpSum([massflowtype[(i,j,ct)] for (i,j,ct) in flowslist if i == node]) <= steel_cap_Mt_corrected[node]*0.967)

# For reporting, not a constraint: massflowtotal over each edge is the sum of all flows by coaltype
for edge in edgelist:
    cn_coal_problem += (massflowtot[edge] == lpSum([massflowtype[(i,j,ct)] for (i,j,ct) in flowslist if (i, j) == edge]))
    
##### Objective function
cn_coal_problem += (lpSum([massflowtype[(i,j,ct)]*total_cash_cost_ex_transp[i,ct]*1e6 for (i,j,ct) in flowslist]) + # all production costs (all non-producing nodes are included in this sum but have 0 supply and 0 prod costs)
                    lpSum([massflowtot[edge]*transp_cost_tot_usd[edge]*1e6 for edge in edgelist]) + # all transport costs
                    lpSum([massflowtype[(i,j,ct)]*CV_PJ_p_Mt_therm[ct]*conversion_eff[i,j]*transm_cost_usd_GJ[i,j]*1e6 for (i,j,ct) in uhvflowslist]) # all UHV transmission costs
                    )

##### Export problem to lp file
cn_coal_problem.writeLP(problpfile)