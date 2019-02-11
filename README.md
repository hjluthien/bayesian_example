# Bayesian Example
Small Shiny App which uses example of coin tosses to help demonstrate value of Bayesian analysis.

If no data is input by the user, example data (from the coin toss example detailed on the app) is used. Else, the users' data is used. 

If a "Flat" prior is chosen, then the probabilities of each of the models is chosed to be 1 /spacing. If a "Normal" prior is chosen by the user, a Normal distribution is taken as the prior. Its mean and standard deviation are determined by the user. This can be used to demonstrate the prior's impact on the posterior.

# To run

On the RStudio console, run `shiny::runApp()`

# Acknowledgements

This example was taken from [Think Bayes by Allen B. Downey](https://greenteapress.com/wp/think-bayes/). 

# Resources

https://www.probabilisticworld.com/calculating-coin-bias-bayes-theorem/
