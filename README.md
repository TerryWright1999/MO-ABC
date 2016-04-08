# MO-ABC
Approximate Bayesian Computing with automatic selection of summary statistics. The idea is that you want to make posterior estimates on some parameters. You have a huge number of summary statistics which might be related to the parameters in question. Genetic Programming is used to find the best combination of subsets. That is subsets with the lease entropy and at the same time are short and so do not introduce extra noise. A Pareto Front of possible subsets are created. ABC is performed with each subset.

In any situation where you have a model regardless of how complicated it is, as long as you can simulate at a reasonable speed then this approach will be able to automate the process of making posterior estimates. In fact the prior to posterior process can be automated.
