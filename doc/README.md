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

Filename                     |Description
-----------------------------|----------------------------------------------------------------------------------------------------------------------
`parameters.RDa`             |the parameter file
`mbd.tree`                   |the true MBD tree
`mbd.fasta`                  |the true MBD alignment
`mbd_gen.xml`                |the BEAST2 input file, using the generative model
`mbd_gen.trees`              |the posterior trees from the true alignment, using the generative model
`mbd_gen.log`                |the posterior parameter estimates from the true tree, using the generative model
`mbd_gen_evidence.trees`     |the trees to estimate the marginal likelihood, using the generative model
`mbd_gen_evidence.log`       |the parameter estimates to estimate the marginal likelihood, using the generative model
`mbd_gen.xml.state`          |the final posterior state from the true tree, using the generative model
`mbd_best.xml`               |the BEAST2 input file, using the best candidate model
`mbd_best.trees`             |the posterior trees from the true tree, using best candidate model
`mbd_best.log`               |the posterior parameter estimates from the true tree, using the best candidate model
`mbd_best.xml.state`         |the final posterior state from the true tree, using the best candidate model
`mbd_best_[x]_evidence.trees`|the trees to estimate the marginal likelihood, for the `x`th candidate model (`x == 1` for first model)
`mbd_best_[x]_evidence.log`  |the parameter estimates to estimate the marginal likelihood, for the `x`th candidate model (`x == 1` for first model)
`mbd_marg_lik.csv`           |the evidences (aka marginal likelihoods) all models have for the true alignment 
`mbd_nltts_gen.csv`          |the error between true tree and its posterior for the generative model
`mbd_nltts_best.csv`         |the error between true tree and its posterior for the best candidate model

Filename                 |Description
-------------------------|---------------------------------------
`mbd_twin.tree`          |the twin MBD tree
`mbd_twin.fasta`         |the twin MBD alignment
`mbd_gen_twin.trees`     |the posterior trees from the twin alignment, using the generative model
`mbd_gen_twin.log`       |the posterior parameter estimates from the twin tree, using the generative model
`mbd_best_twin.trees`    |the posterior trees from the twin tree, using best candidate model
`mbd_best_twin.log`      |the posterior parameter estimates from the twin tree, using the best candidate model
`mbd_marg_lik_twin.csv`  |the evidences (aka marginal likelihoods) all models have for the twin alignment 
`mbd_nltts_gen_twin.csv` |the error between twin tree and its posterior for the generative model
`mbd_nltts_best_twin.csv`|the error between twin tree and its posterior for the best candidate model

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

### How to create the dependency graph from the `.dot` file?

```
dot -Tps dependencies.dot -o dependencies.ps; convert dependencies.ps dependencies.png
```
