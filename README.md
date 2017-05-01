# buxton-challenge-2014

These R files contain statistical models that I built for a predictive analytics challenge in November 2014, sponsored by Buxton and the Wharton Customer Analytics Initiative. The challenge was to predict the sales revenue of potential new store locations of a fast food chain, given a data set with sales and over 700 features for 300 existing locations. Notably, this is a "p > N" setting which breaks standard regression models. I used statistical learning methods to handle this issue robustly, while also satisfying model interpretability requirements in the competition rules. My team and I communicated our methods and recommendations to a "non-technical" panel of judges who roleplayed the chain's executive leadership.

**Description of Files**

ElasticNet.R fits an Elastic Net (R package `glmnet`), which performs shrinkage and variable selection via L1-L2 regularization to generate a multiple regression model. I also wrote code for scrubbing, since the data set had missing and mislabeled values in some places, and implemented cross-validation to optimize predictive accuracy.

LocalLASSO.R implements a semiparametric model which fits a weighted multiple regression (LASSO) for each existing store, with the weights given by a composite kernel capturing demographic similarities between pairs of stores. The sales revenue prediction for each potential store is a local average of the predictions given by the models associated with the k-most-similar existing stores, again using the kernel as a similarity metric.

ComputeBhatt.R outputs five .csv files, each containing a matrix of the Bhattacharyya distances between pairs of stores for one of the five demographic categories (Age, Income, Household Size, Family Size, and Education). These are combined to form the composite kernel in LocalLASSO.R.

Due to a nondiclsoure agreement, the actual data set is unavailable. My team's presentation and write-up are available by request; please email sirallen@sas.upenn.edu.
