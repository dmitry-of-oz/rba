# The Yield and Market Function Effects of the Reserve Bank of Australia's Bond Purchases
# Richard Finlay, Dmitry Titkov and Michelle Xiang / MPI / DM
# July 2021

# 0. Preliminaries ####

source("define_functions.R")
source("define_settings.R")
source("define_colours.R") # from Calvin He's 2021 RDP
source("define_theme.R") # ditto

# 1. Load data ####

source("load_findur_data.R")
source("load_mops_data.R") # uses the RBA's 'mops' package
source("load_aofm_data.R") # uses the RBA's 'readaofm' package
source("load_auctions_data.R")
source("load_austraclear_data.R")
source("load_bloomberg_data.R") # requires a Bloomberg connection for fresh Bloomberg data to be loaded
source("load_dc_data.R")
source("load_iv_data.R")
source("load_spd_data.R")
source("load_tick_data.R")
source("load_yieldbroker_data.R")

# 2. Process data and run models ####

source("process_counterfactual_approaches.R")
source("process_event_studies.R")
source("process_flow_effects.R")
source("process_market_functioning_impacts.R")
source("process_yield_target_analysis.R")
source("model_10y_ags_yield.R")
source("model_bid_offer_spreads_and_turnover.R")
source("model_cross_sectional_flows.R")
source("model_time_series_flows.R")
source("model_yield_target_purchases.R")

# 3. Produce outputs ####

source("create_graphs.R")
source("create_tables.R")