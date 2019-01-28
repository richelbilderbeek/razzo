# razzo

## Folder structure

In `razzo_project`, there are two folders:

 * `data`: the simulated data
 * `results`: the interpreted data, e.g. tables and figures

### The `data` folder

In `data`, there are folders named after their parameters, e.g. `0.2-0.15-1-0.1`.
In each of these folders, there are folders named after their seeds, e.g. `1`.
In each of these folders, there are folders named after their site and clock models, e.g. `strict-jc69`.

In each of these folders, there are:

Filename|Description|Created by
-------------|---------------------------------------|--------------------------
`parameters.csv`|the parameter file|`create_parameter_files`
`mbd.tree`|the true MBD tree|`create_mbd_tree_file`
`mbd.fasta`|the true MBD alignment|`create_mbd_alignment_file`
`bd.tree`|the twin BD tree|`create_bd_tree_file`
`bd.fasta`|the twin BD alignment|`create_bd_alignment_file`
`mbd.trees`|the posterior trees from `mbd.tree`|`create_mbd_posterior_files`
`mbd.log`|the posterior parameter estimates from `mbd.tree`|`create_mbd_posterior_files`
`mbd_marg_lik.csv`|the posterior's marginal likelihood from `mbd.tree`|`create_mbd_posterior_files`
`bd.trees`|the posterior trees from `bd.tree`|`create_bd_posterior_files`
`bd.log`|the posterior parameter estimates from `bd.tree`|`create_bd_posterior_files`
`bd_marg_lik.csv`|the posterior's marginal likelihood from `bd.tree`|`create_mb_posterior_files`
`mbd_nltts.csv`|the nLTT statistic distribution between `mbd.tree` and `mbd.trees`|`create_mbd_nltt_file`
`bd_nltts.csv`|the nLTT statistic distribution between `bd.tree` and `bd.trees`|`create_bd_nltt_file`

See [a razzo_project build log](https://travis-ci.org/richelbilderbeek/razzo_project/jobs/457099656#L1789)
to see such a `data` folder.

### The `results` folder

In `results`, there are only files:

Filename|Description|Created by
-------------|---------------------------------------|--------------------------
`esses.csv`|all ESSes|`create_esses_file`
`marg_liks.csv`|all marginal likelihoods|`create_marg_liks_file`
`nltt_stats.csv`|all nLTT statistics, in short form|`create_esses_file`
`figure_1.png`|figure 1|`create_fig_1_file`
`figure_2.png`|figure 2|`create_fig_2_file`

See [a razzo_project build log](https://travis-ci.org/richelbilderbeek/razzo_project/builds/458324105#L2074)
to see such a `results` folder.

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
?|Create parameter files|`create_parameters_files`
?|Create all output files|`run_razzo_from_file`

#### Data handling functions

![Overview of the data handling functions](overview.png)

These functions do the actual work. 
They work on parsed data and are not intended to be called from a script

Status|Description|Function name
---|---|---
?|1. Create parameters|`create_parameters`
?|2. Run the experiment|`run_razzo`

### Results creation

Creates figures and tables from the data.

#### Data handling

All these functions take the project folder's name as an argument
and return a tidy data frame or figure.
They are not intended to be called from a script

Status|Description|Function name
---|---|---
?|Collect marginal likelihoods|`collect_marg_liks`
?|Collect effective sample sizes|`collect_esses`
?|Collect nLTT statistics|`collect_nltt_stats`
?|12. Create figure 1|`create_fig_1`

#### File handling

All functions that end with `_file` or `_files`.
These functions are friendly to scripted use and
require only the project folder's name as an argument.

Status|Description|Function name
---|---|---
?|Collect marginal likelihoods|`create_marg_liks_file`
?|Collect effective sample sizes|`create_esses_file`
?|Create nLTT statistics file|`create_nltt_stats_file`
?|Create figure 1|`create_fig_1_file`
