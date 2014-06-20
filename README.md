ICWSM 2014 Tutorial
=================

Welcome to the github repository for the ICWSM 2014 tutorial on online experiments for computational social science! You'll find files from


### Repository contents
-`webapp/`: Demo experiment, conducted on all tutorial participants. Includes data analyzed in `analyzing_experiments.R`.

- `0-planout-intro.ipynb`: IPython notebook containing an introduction to PlanOut and examples of experimental designs in PlanOut

- `1-logging.ipynb`: How logging works in PlanOut, with a worked example

- `ICWSM Tutorial.pdf`: Slides for the non-coding part of the tutorial

- `analyzing_experiments.R`: Analysis of demo experiment

- `bootstrap.R`: Sample power analysis for the demo experiment

- `css_stats.R`: Functions for working with weighted data and multi-way bootstrapping

- `power_part1.R`: Intro to power analysis: confidence intervals for normal and binary data, using simulations to understand Type I, Type II, and Type M errors, and do power analysis.

- `power_part2.R`: Critical thinking and power analysis exercise.

- `power_part2_gen_data.R`: Script that generates fake data used in `power_part2.R`

- `gift_data.csv`: Data used for power analysis exercise.


### Description of tutorial
This tutorial teaches attendees how to design, plan, implement, and analyze online experiments. First, we review basic concepts in causal inference and motivate the need for experiments. Then we will discuss basic statistical tools to help plan experiments: exploratory analysis, power calculations, and the use of simulation in R. We then discuss statistical methods to estimate causal quantities of interest and construct appropriate confidence intervals. Particular attention will be given to scalable methods suitable for “big data”, including working with weighted data and clustered bootstrapping. We then discuss how to design and implement online experiments using PlanOut, an open-source toolkit for advanced online experimentation used at Facebook. We will show how basic “A/B tests”, within-subjects designs, as well as more sophisticated experiments can be implemented. We demonstrate how experimental designs from social computing literature can be implemented, and also review in detail two very large field experiments conducted at Facebook using PlanOut. Finally, we will discuss issues with logging and common errors in the deployment and analysis of experiments. Attendees will be given code examples and participate in the planning, implementation, and analysis of a Web application using Python, PlanOut, and R.

### Prerequisites
Basic knowledge of statistics and probability theory, and some familiarity with programming. We will be using R to do exercises involving power calculations and data analysis, so we recommend that attendees either have experience with R or come with a buddy who knows R.

### Outcomes:
- Understand the strengths and limitations of observational and experimental research.

- Learn how to do power calculations via simulation to plan experiments in R.

- Learn how to implement experiments using Python and PlanOut, an open-source toolkit for online experimentation.

- Learn about common pitfalls and best practices for deploying, logging, and analyzing experiments.

- Learn how to integrate experimentation into Web applications, and analyze the results using R.

## Loading up the tutorial notebooks
Navigate to your checked out version of PlanOut and type:

```
ipython notebook --pylab inline
```

### More information
See the (syllabus)[http://eytan.github.io/icwsm14_tutorial/] for more information about the tutorial.

