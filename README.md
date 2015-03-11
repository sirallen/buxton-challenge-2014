# buxton-challenge-2014

These R files contain statistical models that I built for a predictive analytics challenge in November 2014, sponsored by Buxton and the Wharton Customer Analytics Initiative. The challenge was to predict the sales revenue of potential new store locations of a fast food chain, given a data set with sales and over 700 features for 300 existing locations.

ElasticNet.R contains my implementation of an Elastic Net (R package glmnet), which performs regularization and variable selection to generate a multiple regression model, as well as cross-validation to optimize predictive accuracy. I also wrote code for scrubbing, since the data set had missing and mislabeled values in some places.

LocalLASSO.R contains an implementation of local regression, which builds a weighted multiple regression (LASSO) for each existing store, with the weights given by a composite kernel capturing demographic similarities between pairs of stores. The sales revenue prediction for each potential store is a weighted average of the predictions given by the models associated with the k-most-similar existing stores, again using the kernel as a similarity metric.

Due to a nondiclsoure agreement, the actual data set is unavailable. My team's presentation and write-up are available by request; please email sirallen@sas.upenn.edu.
