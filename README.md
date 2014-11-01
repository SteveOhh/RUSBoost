# rusboost

This package is an adaptation of the AdaBoost.M1 implementation
from the 'adabag' package, which was authored by Esteban Alfaro-Cortes,
Matias Gamez-Martinez and Noelia Garcia-Rubiorandom. RUSBoost is an
algorithm created by Seiffert et al., which combines random undersampling
and AdaBoost. My contribution was to integrate the undersampling component
into the Adaboost function. Please see the 'adabag' package for more on how
AdaBoost works.

## Installation

Install using using devtools:

```R
# install.packages("devtools")
devtools::install_github("steveohh/rusboost")
```

## References

* Alfaro, E., Gamez, M. and Garcia, N. (2013): "adabag: An R Package for Classification with Boosting and Bagging". Journal of Statistical Software, Vol 54, 2, pp. 1-35.
* Seiffert, C., Khoshgoftaar, T, Van Hulse, J., and Napolitano, A. (2009): "RUSBoost: A Hybrid Approach for Alleviating Class Imbalance". IEEE, Vol 40, 1, pp. 185-197.
