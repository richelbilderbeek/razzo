# razzo

## Function overview

 * Data creation: produces parameter files and output
 * Results creation: creates figures and tables from the data

### Data creation

Produces parameter files and output

#### File handling functions

All functions that end with `_file` or `_files`.
These functions are friendly to scripted use and
require only filenames as arguments.

Status|Description|Function name
---|---|---
Done|Done|Create parameter files|`raz_create_parameters_files`
Done|Create MBD tree file|`raz_create_mbd_tree_file`
Done|Create MBD alignment file|`raz_create_mbd_alignment_file`
Done|Create twin BD tree file|`raz_create_bd_tree_file`
Done|Create twin BD alignment file|`raz_create_bd_alignment_file`
Done|Create MBD posterior files|`raz_create_mbd_posterior_files`
Done|Create twin BD posterior files|`raz_create_bd_posterior_files`
Done|Create MBD nLTT file|`raz_create_mbd_nltt_file`
Done|Create twin BD nLTT file|`raz_create_bd_nltt_file`

#### Data handling functions

![Overview of the data handling functions](overview.png)

These functions do the actual work. 
They work on parsed data and are not intended to be called from a script

Status|Description|Function name
---|---|---
Done|Create parameter|`raz_create_parameters`
Done|Create MBD tree|`raz_create_mbd_tree`
Done|Create BD tree|`raz_create_bd_tree`
Done|Create MBD alignment|`raz_create_mbd_alignment`
Done|Create BD alignment|`raz_create_bd_alignment`
Done|Create MBD posterior|`raz_mbd_create_posterior`
Done|Create BD posterior|`raz_bd_create_posterior`
Done|Create MBD nLTT|`raz_create_mbd_nltt`
Done|Create BD nLTT|`raz_create_bd_nltt`
.   |Estimate MBD marginal likelihood|`raz_est_mbd_marg_lik`
.   |Estimate BD marginal likelihood|`raz_est_bd_marg_lik`

### Results creation

Creates figures and tables from the data.

#### Data handling

All these functions take the project folder's name as an argument
and return a tidy data frame or figure.
They are not intended to be called from a script

Status|Description|Function name
---|---|---
Issue|Collect marginal likelihoods|`raz_collect_mar_log_liks`
Issue|Collect effective sample sizes|`raz_collect_esses`
Issue|Collect nLTT statistics|`raz_collect_nltt_stats`
Issue|Create figure 1|`raz_create_fig_1`

#### File handling

All functions that end with `_file` or `_files`.
These functions are friendly to scripted use and
require only the project folder's name as an argument.

Status|Description|Function name
---|---|---
Issue|Collect marginal likelihoods|`raz_create_mar_log_liks_file`
Issue|Collect effective sample sizes|`raz_create_esses_file`
Issue|Create nLTT statistics file|`raz_create_nltt_stats_file`
Issue|Create figure 1|`raz_create_fig_1_file`
