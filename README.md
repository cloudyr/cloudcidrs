
[![CRAN](http://www.r-pkg.org/badges/version/cloudcidrs)](http://cran.r-project.org/package=cloudcidrs)
[![Build
Status](https://travis-ci.org/cloudyr/cloudcidrs.png?branch=master)](https://travis-ci.org/cloudyr/cloudcidrs)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/cloudyr/cloudcidrs?branch=master&svg=true)](https://ci.appveyor.com/project/cloudyr/cloudcidrs)
[![Coverage
Status](https://img.shields.io/codecov/c/github/cloudyr/cloudcidrs/master.svg)](https://codecov.io/github/cloudyr/cloudcidrs?branch=master)

# cloudcidrs

Tools to Obtain and Work with Cloud Provider CIDR Blocks

## Description

Cloud providers will be added on an as-needed or request basis (i.e. if
you need another cloud provider, file an issue or — prefereably — PR).

Some cloud providers provide either an API or a file that contains all
of the public networks that make up their cloud infrastructure. Many
force you to obtain this data from publicly available internet routing
registration data. Tools are provided that provide a standard API to
obtain the network information for supported cloud providers. Each
provider function returns processed, raw data structures that can be
normalized with additional functions to enable predictable and
consistent data formats for further processing.

A future plan is to
[`memoise`](https://cran.r-project.org/package=memoise) the results and
also provide disk-level caching since these CIDRs don’t change
frequently enough to warrant network traffic for each call.

## What’s Inside the Tin

The following functions are implemented:

  - `all_ranges`: Build a complete data frame of all known cloud
    provider ranges
  - `amazon_ranges`: Amazon AWS cloud ranges
  - `azure_ranges`: Azure ranges
  - `digitalocean_ranges`: Digital Ocean ranges
  - `google_ranges`: Google Cloud ranges
  - `linode_ranges`: Linode ranges
  - `normalize_ipv4`: Normalize Cloud CIDR return values
  - `ovh_ranges`: OVH ranges
  - `rackspace_ranges`: Rackspace ranges
  - `softlayer_ranges`: Softlayer ranges

## Installation

``` r
devtools::install_git("https://github.com/cloudyr/cloudcidrs.git")
```

``` r
options(width=120)
```

## Usage

``` r
library(cloudcidrs)

# current verison
packageVersion("cloudcidrs")
```

    ## [1] '0.1.1'

``` r
all_ranges()
```

    ## # A tibble: 4,472 x 7
    ##    provider cidr          minimum_ip maximum_ip     min_numeric max_numeric check_date
    ##    <chr>    <chr>         <chr>      <chr>                <dbl>       <dbl> <date>    
    ##  1 amazon   13.32.0.0/15  13.32.0.0  13.33.255.255    220200960   220332031 2018-06-12
    ##  2 amazon   13.35.0.0/16  13.35.0.0  13.35.255.255    220397568   220463103 2018-06-12
    ##  3 amazon   13.52.0.0/16  13.52.0.0  13.52.255.255    221511680   221577215 2018-06-12
    ##  4 amazon   13.53.0.0/16  13.53.0.0  13.53.255.255    221577216   221642751 2018-06-12
    ##  5 amazon   13.54.0.0/15  13.54.0.0  13.55.255.255    221642752   221773823 2018-06-12
    ##  6 amazon   13.56.0.0/16  13.56.0.0  13.56.255.255    221773824   221839359 2018-06-12
    ##  7 amazon   13.57.0.0/16  13.57.0.0  13.57.255.255    221839360   221904895 2018-06-12
    ##  8 amazon   13.58.0.0/15  13.58.0.0  13.59.255.255    221904896   222035967 2018-06-12
    ##  9 amazon   13.112.0.0/14 13.112.0.0 13.115.255.255   225443840   225705983 2018-06-12
    ## 10 amazon   13.124.0.0/16 13.124.0.0 13.124.255.255   226230272   226295807 2018-06-12
    ## # ... with 4,462 more rows

-----

[![cloudyr project
logo](http://i.imgur.com/JHS98Y7.png)](https://github.com/cloudyr)
