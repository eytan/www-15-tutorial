ICWSM 2014 Tutorial
=================

### Summary
Taught by two researchers on the Facebook Data Science team, this tutorial teaches attendees how to design, plan, implement, and analyze online experiments. First, we review basic concepts in causal inference and motivate the need for experiments. Then we will discuss basic statistical tools to help plan experiments: exploratory analysis, power calculations, and the use of simulation in R.  We then discuss statistical methods to estimate causal quantities of interest and construct appropriate confidence intervals. Particular attention will be given to scalable methods suitable for “big data”, including working with weighted data and clustered bootstrapping. We then discuss how to design and implement online experiments using PlanOut, an open-source toolkit for advanced online experimentation used at Facebook.  We will show how basic “A/B tests”, within-subjects designs, as well as more sophisticated experiments can be implemented.  We demonstrate how experimental designs from social computing literature can be implemented, and also review in detail two very large field experiments conducted at Facebook using PlanOut.  Finally, we will discuss issues with logging and common errors in the deployment and analysis of experiments. Attendees will be given code examples and participate in the planning, implementation, and analysis of a Web application using Python, PlanOut, and R.


### Prerequisites
Basic knowledge of statistics and probability theory, and some familiarity with programming.  We will be using R to do exercises involving power calculations and data analysis, so we recommend that attendees either have experience with R or come with a buddy who knows R.

### Outcomes:
- Understand the strengths and limitations of observational and experimental research.

- Learn how to do power calculations via simulation to plan experiments in R.

- Learn how to implement experiments using Python and PlanOut, an open-source toolkit for online experimentation.

- Learn about common pitfalls and best practices for deploying, logging, and analyzing experiments.

- Learn how to integrate experimentation into Web applications, and analyze the results using R.

### Justification
While online communication has allowed us to collect and observe social phenomenon at an unprecedented scale, but such data do not, in general, allow researchers to make causal claims about why a phenomenon occurs.  These questions are most easily answered via randomized experiments.  Our tutorial teaches participants how to design, plan, implement, and analyze online experiments.

Software requirements: git, R, Python, PlanOut (to be available on github), Flask (available on github), sample R and python code (to be available on github).

Other requirements: a projector, Internet access, microphone, paper printouts (we will provide the latter).
