# SparkRocks - Parallel rock slicing implementation on Apache Spark.
> Cambridge Berkeley - Geomechanics

[![License](https://img.shields.io/badge/License-GPL%20v2-blue.svg)](https://raw.githubusercontent.com/cb-geo/spark-rocks/master/LICENSE)

# Overview

SparkRocks is a parallel fractured rock mass generator that runs on Apache
Spark. The block cutting algorithm is based on a subdivision approach and linear
programming optimization as described in *A new rock slicing method based on
linear programming* by Boon et al. (2015). It can be run both locally or on the
cloud and on any operating system. See (???) for a complete description of
SparkRocks and the underlying implementation.

# Usage

SparkRocks is run on Apache Spark by submitting the SparkRocks
`sparkRocks-assembly-1.0.jar` to Spark. Spark manages the execution and
deployment of SparkRocks so the user does not need to do any additional work to
scale analyses to larger scale problems. Documentation on how to deploy Spark
locally or on the cloud are provided at [Submitting
Applications](http://spark.apache.org/docs/latest/submitting-applications.html).
The examples that follow assume SparkRocks is being run on Amazon EMR.

## Command line arguments

SparkRocks is run from the command line using only a few, straightforward
arguments as follows:

```
spark-submit path/to/sparkRocks-assembly-1.0.jar [required inputs] [optional inputs]
```

The required inputs are:

`-i [path/to/input/file]`
This provides the path the input file that is described below

`-n [integer number of partitions]`  
 
## Input


