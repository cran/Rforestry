% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forestry.R
\name{predict-forestry}
\alias{predict-forestry}
\alias{predict.forestry}
\title{predict-forestry}
\usage{
\method{predict}{forestry}(
  object,
  newdata = NULL,
  aggregation = "average",
  holdOutIdx = NULL,
  trainingIdx = NULL,
  seed = as.integer(runif(1) * 10000),
  nthread = 0,
  exact = NULL,
  trees = NULL,
  weightMatrix = FALSE,
  ...
)
}
\arguments{
\item{object}{A `forestry` object.}

\item{newdata}{A data frame of testing predictors.}

\item{aggregation}{How the individual tree predictions are aggregated:
`average` returns the mean of all trees in the forest; `terminalNodes` also returns
the weightMatrix, as well as "terminalNodes", a matrix where
the ith entry of the jth column is the index of the leaf node to which the
ith observation is assigned in the jth tree; and "sparse", a matrix
where the ith entry in the jth column is 1 if the ith observation in
newdata is assigned to the jth leaf and 0 otherwise. In each tree the
leaves are indexed using a depth first ordering, and, in the "sparse"
representation, the first leaf in the second tree has column index one more than
the number of leaves in the first tree and so on. So, for example, if the
first tree has 5 leaves, the sixth column of the "sparse" matrix corresponds
to the first leaf in the second tree.
`oob` returns the out-of-bag predictions for the forest. We assume
that the ordering of the observations in newdata have not changed from
training. If the ordering has changed, we will get the wrong OOB indices.
`doubleOOB` is an experimental flag, which can only be used when OOBhonest = TRUE
and doubleBootstrap = TRUE. When both of these settings are on, the
splitting set is selected as a bootstrap sample of observations and the
averaging set is selected as a bootstrap sample of the observations which
were left out of bag during the splitting set selection. This leaves a third
set which is the observations which were not selected in either bootstrap sample.
This predict flag gives the predictions using- for each observation- only the trees
in which the observation fell into this third set (so was neither a splitting
nor averaging example).
`coefs` is an aggregation option which works only when linear aggregation
functions have been used. This returns the linear coefficients for each
linear feature which were used in the leaf node regression of each predicted
point.}

\item{holdOutIdx}{This is an optional argument, containing a vector of indices
from the training data set that should be not be allowed to influence the
predictions of the forest. When a vector of indices of training observations are
given, the predictions will be made only with trees in the forest that
do not contain any of these indices in either the splitting or averaging sets.
This cannot be used at the same time as any other aggregation options.
If `weightMatrix = TRUE`, this will return the
weightMatrix corresponding to the predictions made with trees respecting
holdOutIdx. If there are no trees that have held out all of the indices
in holdOutIdx, then the predictions will return NaN.}

\item{trainingIdx}{This is an optional parameter to give the indices of the observations
in `newdata` from the training data set. This is used when we want to run predict on
only a subset of observations from the training data set and use `aggregation = "oob"` or
`aggregation = "doubleOOB"`. For example, at the tree level, a tree make out of
bag (`aggregation = "oob"`) predictions for the indices in the set
setdiff(trainingIdx,tree$averagingIndices) and will make double out-of-bag
predictions for the indices in the set
setdiff(trainingIdx,union(tree$averagingIndices,tree$splittingIndices).
Note, this parameter must be set when predict is called with an out-of-bag
aggregation option on a data set not matching the original training data size.
The order of indices in `trainingIdx` also needs to match the order of observations
in newdata. So for an arbitrary index set `trainingIdx` and dataframe `newdata`,
 of the same size as the training set, the predictions from `predict(rf, newdata[trainingIdx,],`
`aggregation = "oob", trainingIdx = trainingIdx)` should match the
predictions of to `predict(rf, newdata, aggregation = "oob")[trainingIdx]`.
This option also works with the `weightMatrix` option and will return the
(smaller) weightMatrix for the observations in the passed data frame.}

\item{seed}{random seed}

\item{nthread}{The number of threads with which to run the predictions with.
This will default to the number of threads with which the forest was trained
with.}

\item{exact}{This specifies whether the forest predictions should be aggregated
in a reproducible ordering. Due to the non-associativity of floating point
addition, when we predict in parallel, predictions will be aggregated in
varied orders as different threads finish at different times.
By default, exact is TRUE unless N > 100,000 or a custom aggregation
function is used.}

\item{trees}{A vector (1-indexed) of indices in the range 1:ntree which tells
predict which trees in the forest to use for the prediction. Predict will by
default take the average of all trees in the forest, although this flag
can be used to get single tree predictions, or averages of diffferent trees
with different weightings. Duplicate entries are allowed, so if trees = c(1,2,2)
this will predict the weighted average prediction of only trees 1 and 2 weighted by:
predict(..., trees = c(1,2,2)) = (predict(..., trees = c(1)) +
                                   2*predict(..., trees = c(2))) / 3.
note we must have exact = TRUE, and aggregation = "average" to use tree indices.}

\item{weightMatrix}{An indicator of whether or not we should also return a
matrix of the weights given to each training observation when making each
prediction. When getting the weight matrix, aggregation must be one of
`average`, `oob`, and `doubleOOB`.}

\item{...}{additional arguments.}
}
\value{
A vector of predicted responses.
}
\description{
Return the prediction from the forest.
}
