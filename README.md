# razzo

Branch|[![Travis CI logo](pics/TravisCI.png)](https://travis-ci.org)|[![AppVeyor logo](pics/AppVeyor.png)](https://www.appveyor.com)|[![Codecov logo](pics/Codecov.png)](https://www.codecov.io)
---|---|---|---
`master`|[![Build Status](https://travis-ci.org/richelbilderbeek/razzo.svg?branch=master)](https://travis-ci.org/richelbilderbeek/razzo) |[![Build status](https://ci.appveyor.com/api/projects/status/o6htu70cv6ttqw5r/branch/master?svg=true)](https://ci.appveyor.com/project/richelbilderbeek/razzo/branch/master)| [![codecov.io](https://codecov.io/github/richelbilderbeek/razzo/coverage.svg?branch=master)](https://codecov.io/github/richelbilderbeek/razzo?branch=master)
`develop`|[![Build Status](https://travis-ci.org/richelbilderbeek/razzo.svg?branch=develop)](https://travis-ci.org/richelbilderbeek/razzo) |[![Build status](https://ci.appveyor.com/api/projects/status/o6htu70cv6ttqw5r/branch/develop?svg=true)](https://ci.appveyor.com/project/richelbilderbeek/razzo/branch/develop)| [![codecov.io](https://codecov.io/github/richelbilderbeek/razzo/coverage.svg?branch=develop)](https://codecov.io/github/richelbilderbeek/razzo?branch=develop)
`giovanni`|[![Build Status](https://travis-ci.org/richelbilderbeek/razzo.svg?branch=giovanni)](https://travis-ci.org/richelbilderbeek/razzo) |[![Build status](https://ci.appveyor.com/api/projects/status/o6htu70cv6ttqw5r/branch/giovanni?svg=true)](https://ci.appveyor.com/project/richelbilderbeek/razzo/branch/giovanni)| [![codecov.io](https://codecov.io/github/richelbilderbeek/razzo/coverage.svg?branch=giovanni)](https://codecov.io/github/richelbilderbeek/razzo?branch=giovanni)
`richel`|[![Build Status](https://travis-ci.org/richelbilderbeek/razzo.svg?branch=richel)](https://travis-ci.org/richelbilderbeek/razzo) |[![Build status](https://ci.appveyor.com/api/projects/status/o6htu70cv6ttqw5r/branch/richel?svg=true)](https://ci.appveyor.com/project/richelbilderbeek/razzo/branch/richel)| [![codecov.io](https://codecov.io/github/richelbilderbeek/razzo/coverage.svg?branch=richel)](https://codecov.io/github/richelbilderbeek/razzo?branch=richel)

Research project by Giovanni Laudanno and Richel J.C. Bilderbeek.

The research project uses three GitHub repo's:

 * [razzo](https://github.com/richelbilderbeek/razzo): R code
 * [razzo_article](https://github.com/richelbilderbeek/razzo_article): scientific manuscript (private GitHub for now)
 * [razzo_project](https://github.com/richelbilderbeek/razzo_project): bash scripts

## Roadmap

### File handling functions

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
?|Create marginal likelihood file|`raz_create_mar_lik_file`
?|Create figure 1|`raz_create_fig_1`
?|Create figure 2|`raz_create_fig_2`

### Data handling functions

These functions do the actual work. The work on parsed data
and are not intended to be called from a script

Status|Description|Function name
---|---|---
Done|Create parameter|`raz_create_parameters`
Done|Create MBD tree|`raz_create_mbd_tree`
Done|Create MBD alignment|`raz_create_mbd_alignment`
Done|Create twin BD tree|`raz_create_bd_tree`
Done|Create BD alignment|`raz_create_bd_alignment`
Done|Create posterior|`raz_create_posterior`
Done|Create nLTT|`raz_create_nltt`
?|Create marginal likelihoods|`raz_create_mar_lik`

## Installation

If you use the `devtools` R package, this is easy:

```
devtools::install_github("richelbilderbeek/beautier")
devtools::install_github("richelbilderbeek/tracerer")
devtools::install_github("richelbilderbeek/beastier")
devtools::install_github("richelbilderbeek/mauricer")
devtools::install_github("richelbilderbeek/babette")
devtools::install_github("Giappo/mbd")
devtools::install_github("richelbilderbeek/pirouette")
devtools::install_github("richelbilderbeek/razzo")
```

## Package dependencies

Package|[![Travis CI logo](pics/TravisCI.png)](https://travis-ci.org)|[![Codecov logo](pics/Codecov.png)](https://www.codecov.io)
---|---|---
[beautier](https://github.com/richelbilderbeek/beautier)|[![Build Status](https://travis-ci.org/richelbilderbeek/beautier.svg?branch=master)](https://travis-ci.org/richelbilderbeek/beautier)|[![codecov.io](https://codecov.io/github/richelbilderbeek/beautier/coverage.svg?branch=master)](https://codecov.io/github/richelbilderbeek/beautier/branch/master)
[beastier](https://github.com/richelbilderbeek/beastier)|[![Build Status](https://travis-ci.org/richelbilderbeek/beastier.svg?branch=master)](https://travis-ci.org/richelbilderbeek/beastier)|[![codecov.io](https://codecov.io/github/richelbilderbeek/beastier/coverage.svg?branch=master)](https://codecov.io/github/richelbilderbeek/beastier/branch/master)
[mauricer](https://github.com/richelbilderbeek/mauricer)|[![Build Status](https://travis-ci.org/richelbilderbeek/mauricer.svg?branch=master)](https://travis-ci.org/richelbilderbeek/mauricer)|[![codecov.io](https://codecov.io/github/richelbilderbeek/mauricer/coverage.svg?branch=master)](https://codecov.io/github/richelbilderbeek/mauricer/branch/master)
[mbd](https://github.com/Giappo/mbd)|[![Build Status](https://travis-ci.org/Giappo/mbd.svg?branch=master)](https://travis-ci.org/Giappo/mbd)|[![codecov.io](https://codecov.io/github/Giappo/mbd/coverage.svg?branch=master)](https://codecov.io/github/Giappo/mbd/branch/master)
[tracerer](https://github.com/richelbilderbeek/tracerer)|[![Build Status](https://travis-ci.org/richelbilderbeek/tracerer.svg?branch=master)](https://travis-ci.org/richelbilderbeek/tracerer)|[![codecov.io](https://codecov.io/github/richelbilderbeek/tracerer/coverage.svg?branch=master)](https://codecov.io/github/richelbilderbeek/tracerer/branch/master)
