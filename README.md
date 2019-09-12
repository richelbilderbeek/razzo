# razzo

Branch|[![Travis CI logo](pics/TravisCI.png)](https://travis-ci.org)|[![AppVeyor logo](pics/AppVeyor.png)](https://www.appveyor.com)|[![Codecov logo](pics/Codecov.png)](https://www.codecov.io)
---|---|---|---
`master`|[![Build Status](https://travis-ci.org/richelbilderbeek/razzo.svg?branch=master)](https://travis-ci.org/richelbilderbeek/razzo) |[![Build status](https://ci.appveyor.com/api/projects/status/o6htu70cv6ttqw5r/branch/master?svg=true)](https://ci.appveyor.com/project/richelbilderbeek/razzo/branch/master)| [![codecov.io](https://codecov.io/github/richelbilderbeek/razzo/coverage.svg?branch=master)](https://codecov.io/github/richelbilderbeek/razzo?branch=master)
`develop`|[![Build Status](https://travis-ci.org/richelbilderbeek/razzo.svg?branch=develop)](https://travis-ci.org/richelbilderbeek/razzo) |[![Build status](https://ci.appveyor.com/api/projects/status/o6htu70cv6ttqw5r/branch/develop?svg=true)](https://ci.appveyor.com/project/richelbilderbeek/razzo/branch/develop)| [![codecov.io](https://codecov.io/github/richelbilderbeek/razzo/coverage.svg?branch=develop)](https://codecov.io/github/richelbilderbeek/razzo?branch=develop)
`giovanni`|[![Build Status](https://travis-ci.org/richelbilderbeek/razzo.svg?branch=giovanni)](https://travis-ci.org/richelbilderbeek/razzo) |[![Build status](https://ci.appveyor.com/api/projects/status/o6htu70cv6ttqw5r/branch/giovanni?svg=true)](https://ci.appveyor.com/project/richelbilderbeek/razzo/branch/giovanni)| [![codecov.io](https://codecov.io/github/richelbilderbeek/razzo/coverage.svg?branch=giovanni)](https://codecov.io/github/richelbilderbeek/razzo?branch=giovanni)
`richel`|[![Build Status](https://travis-ci.org/richelbilderbeek/razzo.svg?branch=richel)](https://travis-ci.org/richelbilderbeek/razzo) |[![Build status](https://ci.appveyor.com/api/projects/status/o6htu70cv6ttqw5r/branch/richel?svg=true)](https://ci.appveyor.com/project/richelbilderbeek/razzo/branch/richel)| [![codecov.io](https://codecov.io/github/richelbilderbeek/razzo/coverage.svg?branch=richel)](https://codecov.io/github/richelbilderbeek/razzo?branch=richel)

Research project by Giovanni Laudanno and Richel J.C. Bilderbeek.

![](pics/razzo_logo.png)

Primary tasks:

 * Giovanni Laudanno: making each step right
 * Richel J.C. Bilderbeek: big picture, software architecture, testing, continuous integration

The research project uses three GitHub repo's:

 * [razzo](https://github.com/richelbilderbeek/razzo): R code
 * `https://github.com/richelbilderbeek/razzo_article`: scientific manuscript (private GitHub for now)
 * [razzo_project](https://github.com/richelbilderbeek/razzo_project): bash scripts

## Roadmap

### Project stages

 * `Ignition`: prepare to do the experiment badly, e.g. short MCMC chains, few replicates, etc.
 * `Launch`: prepare to do the experiment correctly, in line with manuscript
 * `Flight`: running the experiment, maintaining the process
 * `Land`: write down results

## Function overview

See [doc](doc/README.md).

## Installation

If you use the `devtools` R package, this is easy:

```
devtools::install_github("ropensci/beautier")
devtools::install_github("ropensci/tracerer")
devtools::install_github("ropensci/beastier")
devtools::install_github("ropensci/mauricer")
devtools::install_github("ropensci/babette")
devtools::install_github("Giappo/mbd")
devtools::install_github("richelbilderbeek/pirouette")
devtools::install_github("richelbilderbeek/razzo")
```

## Package dependencies

Package|[![Travis CI logo](pics/TravisCI.png)](https://travis-ci.org)|[![Codecov logo](pics/Codecov.png)](https://www.codecov.io)
---|---|---
[babette](https://github.com/ropensci/babette)|[![Build Status](https://travis-ci.org/ropensci/babette.svg?branch=master)](https://travis-ci.org/ropensci/babette)|[![codecov.io](https://codecov.io/github/ropensci/babette/coverage.svg?branch=master)](https://codecov.io/github/ropensci/babette/branch/master)
[beautier](https://github.com/ropensci/beautier)|[![Build Status](https://travis-ci.org/ropensci/beautier.svg?branch=master)](https://travis-ci.org/ropensci/beautier)|[![codecov.io](https://codecov.io/github/ropensci/beautier/coverage.svg?branch=master)](https://codecov.io/github/ropensci/beautier/branch/master)
[beastier](https://github.com/ropensci/beastier)|[![Build Status](https://travis-ci.org/ropensci/beastier.svg?branch=master)](https://travis-ci.org/ropensci/beastier)|[![codecov.io](https://codecov.io/github/ropensci/beastier/coverage.svg?branch=master)](https://codecov.io/github/ropensci/beastier/branch/master)
[mauricer](https://github.com/ropensci/mauricer)|[![Build Status](https://travis-ci.org/ropensci/mauricer.svg?branch=master)](https://travis-ci.org/ropensci/mauricer)|[![codecov.io](https://codecov.io/github/ropensci/mauricer/coverage.svg?branch=master)](https://codecov.io/github/ropensci/mauricer/branch/master)
[mbd](https://github.com/Giappo/mbd)|[![Build Status](https://travis-ci.org/Giappo/mbd.svg?branch=master)](https://travis-ci.org/Giappo/mbd)|[![codecov.io](https://codecov.io/github/Giappo/mbd/coverage.svg?branch=master)](https://codecov.io/github/Giappo/mbd/branch/master)
[tracerer](https://github.com/ropensci/tracerer)|[![Build Status](https://travis-ci.org/ropensci/tracerer.svg?branch=master)](https://travis-ci.org/ropensci/tracerer)|[![codecov.io](https://codecov.io/github/ropensci/tracerer/coverage.svg?branch=master)](https://codecov.io/github/ropensci/tracerer/branch/master)
