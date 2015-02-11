# RUSBoost

This package is an adaptation of the AdaBoost.M1 implementation from the 'adabag' package (Alfaro et al.). RUSBoost is an algorithm created by Seiffert et al., which combines random undersampling and AdaBoost. My contribution was to integrate the undersampling component into the AdaBoost function. 

## What does it do?
With this package, you get two things. You can use the RUSBoost algorithm to model your data, and you can make predictions with the resulting model. 

## Why would I want to do that?
The short version is that the algorithm is a powerful way to handle strongly unbalanced data, where one class is much more heavily represented than others--typically in binary situations, such as when an event does not occur in 90% of observations. When a dataset is unbalanced, learning algorithms tend to overpredict the majority class. Researchers like to counteract this tendency by oversampling minority class examples, undersampling majority class examples, or boosting. RUSBoost combines  the genius of adaptive boosting with the pragmatism of undersampling.


## But how does it work?
Magic. Well, a little magic, but mostly through [boosting](http://www.cs.princeton.edu/~schapire/papers/explaining-adaboost.pdf). AdaBoost is an ensemble method which creates models iteratively. After each round of model creation, errors are calculated and weights to training instances are reassigned, so that the instances which were incorrectly predicted are more important--and thus more likely to be correctly classified--in successive models. At the end, all of the models get to vote on which class they think each observation is, and the predictions output for new, unlabeled data are based on a weighted majority. RUSBoost is a tweak to AdaBoost. In each model creation iteration, the overrepresented class is undersampled, in order to better model the minority class(es). 

 If you know what SMOTEBoost is, this is its cooler younger brother. It doesn't try as hard to be clever, and as a result it ends up being right more often and working less.

## Is it buggy?
Sorry Hadley, but it doesn't play well with the `tbl_df()` wrapper. Convert your [tibble diffs](https://twitter.com/hadleywickham/status/524200537861128192) back to dataframes before feeding them into `rusb()`.

For now, you can only use the function for two-class classification problems. It shouldn't be too hard to extend to multiple classes, and you're welcome to do so for me.

Please let me know here (with an issue/pull request) or via email if you find other issues.

## This sounds INCREDIBLE. How do I get started?

Install using using devtools:

```R
# install.packages("devtools")
devtools::install_github("steveohh/rusboost")
```
It wouldn't hurt to read the paper, either. 

## References

* Alfaro, E., Gamez, M. and Garcia, N. (2013): "adabag: An R Package for Classification with Boosting and Bagging". Journal of Statistical Software, Vol 54, 2, pp. 1-35.
* Seiffert, C., Khoshgoftaar, T, Van Hulse, J., and Napolitano, A. (2009): "RUSBoost: A Hybrid Approach for Alleviating Class Imbalance". IEEE, Vol 40, 1, pp. 185-197.
