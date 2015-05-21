WWW 2015 Tutorial
=================

Welcome to the github repository for the WWW 2015 tutorial on online experiments for computational social science!

[You can view all slides and materials on the web.](https://eytan.github.io/www-15-tutorial)


### Repository contents

- `0-estimation-and-power.ipynb`: IPython notebook discussing inference for experiments and showing how to do a power analysis via simulation.

- `1-planout-intro.ipynb`: IPython notebook containing an introduction to PlanOut and examples of experimental designs in PlanOut.

- `2-making-your-own-data.ipynb`: How to extract a data frame from logs generated through PlanOut.

- `4-analyzing-experiments.ipynb`: How to analyze the experimental data we produced, including scaling analysis to big data.

- `webapp/`: Demo experiment which will work with MTurk.

- `data/`: Data extracted from logfiles used in selective exposure experiment.

- `css_stats.R`: Functions for working with weighted data and multi-way bootstrapping

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

### Required Software

1. Download and install [Anaconda](https://store.continuum.io/cshop/anaconda/), a distribution of Python that includes most packages you will need:
  
2. Install [R](http://cran.r-project.org/) (R 3.1 or later)

3. Install required R packages by typing into R:

```
>> install.packages(c('dplyr', 'ggplot2', 'sandwich', 'foreach', 'doMC', 'sandwich', ‘lmtest’, ‘broom’, ))
```

4. Install iPython, PlanOut, and Rpy2. In your terminal, type:

```
$ pip install -U ipython planout rpy2
```

## Loading up the tutorial notebooks

Navigate to your checked out version of PlanOut and type:

```
ipython notebook --pylab inline
```

You can also start a web server to serve up the slides by entering, e.g.,:

```
ipython nbconvert 0-estimation-and-power.ipynb --to slides --post serve
```

### More information
See the [syllabus](https://eytan.github.io/www-15-tutorial/) for more information about the tutorial.
