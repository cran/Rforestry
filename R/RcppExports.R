# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

rcpp_cppDataFrameInterface <- function(x, y, catCols, linCols, numRows, numColumns, featureWeights, featureWeightsVariables, deepFeatureWeights, deepFeatureWeightsVariables, observationWeights, customSplitSample, customAvgSample, customExcludeSample, monotonicConstraints, groupMemberships, monotoneAvg) {
    .Call(`_Rforestry_rcpp_cppDataFrameInterface`, x, y, catCols, linCols, numRows, numColumns, featureWeights, featureWeightsVariables, deepFeatureWeights, deepFeatureWeightsVariables, observationWeights, customSplitSample, customAvgSample, customExcludeSample, monotonicConstraints, groupMemberships, monotoneAvg)
}

rcpp_cppBuildInterface <- function(x, y, catCols, linCols, numRows, numColumns, ntree, replace, sampsize, mtry, splitratio, OOBhonest, doubleBootstrap, nodesizeSpl, nodesizeAvg, nodesizeStrictSpl, nodesizeStrictAvg, minSplitGain, maxDepth, interactionDepth, seed, nthread, verbose, middleSplit, maxObs, featureWeights, featureWeightsVariables, deepFeatureWeights, deepFeatureWeightsVariables, observationWeights, customSplitSample, customAvgSample, customExcludeSample, monotonicConstraints, groupMemberships, minTreesPerFold, foldSize, monotoneAvg, hasNas, naDirection, linear, overfitPenalty, doubleTree, existing_dataframe_flag, existing_dataframe) {
    .Call(`_Rforestry_rcpp_cppBuildInterface`, x, y, catCols, linCols, numRows, numColumns, ntree, replace, sampsize, mtry, splitratio, OOBhonest, doubleBootstrap, nodesizeSpl, nodesizeAvg, nodesizeStrictSpl, nodesizeStrictAvg, minSplitGain, maxDepth, interactionDepth, seed, nthread, verbose, middleSplit, maxObs, featureWeights, featureWeightsVariables, deepFeatureWeights, deepFeatureWeightsVariables, observationWeights, customSplitSample, customAvgSample, customExcludeSample, monotonicConstraints, groupMemberships, minTreesPerFold, foldSize, monotoneAvg, hasNas, naDirection, linear, overfitPenalty, doubleTree, existing_dataframe_flag, existing_dataframe)
}

rcpp_cppPredictInterface <- function(forest, x, aggregation, seed, nthread, exact, returnWeightMatrix, use_weights, use_hold_out_idx, tree_weights, hold_out_idx) {
    .Call(`_Rforestry_rcpp_cppPredictInterface`, forest, x, aggregation, seed, nthread, exact, returnWeightMatrix, use_weights, use_hold_out_idx, tree_weights, hold_out_idx)
}

rcpp_OBBPredictInterface <- function(forest) {
    .Call(`_Rforestry_rcpp_OBBPredictInterface`, forest)
}

rcpp_OBBPredictionsInterface <- function(forest, x, existing_df, doubleOOB, returnWeightMatrix, exact, use_training_idx, training_idx) {
    .Call(`_Rforestry_rcpp_OBBPredictionsInterface`, forest, x, existing_df, doubleOOB, returnWeightMatrix, exact, use_training_idx, training_idx)
}

rcpp_getObservationSizeInterface <- function(df) {
    .Call(`_Rforestry_rcpp_getObservationSizeInterface`, df)
}

rcpp_AddTreeInterface <- function(forest, ntree) {
    invisible(.Call(`_Rforestry_rcpp_AddTreeInterface`, forest, ntree))
}

rcpp_CppToR_translator <- function(forest) {
    .Call(`_Rforestry_rcpp_CppToR_translator`, forest)
}

rcpp_reconstructree <- function(x, y, catCols, linCols, numRows, numColumns, R_forest, replace, sampsize, splitratio, OOBhonest, doubleBootstrap, mtry, nodesizeSpl, nodesizeAvg, nodesizeStrictSpl, nodesizeStrictAvg, minSplitGain, maxDepth, interactionDepth, seed, nthread, verbose, middleSplit, maxObs, minTreesPerFold, featureWeights, featureWeightsVariables, deepFeatureWeights, deepFeatureWeightsVariables, observationWeights, customSplitSample, customAvgSample, customExcludeSample, monotonicConstraints, groupMemberships, monotoneAvg, hasNas, naDirection, linear, overfitPenalty, doubleTree) {
    .Call(`_Rforestry_rcpp_reconstructree`, x, y, catCols, linCols, numRows, numColumns, R_forest, replace, sampsize, splitratio, OOBhonest, doubleBootstrap, mtry, nodesizeSpl, nodesizeAvg, nodesizeStrictSpl, nodesizeStrictAvg, minSplitGain, maxDepth, interactionDepth, seed, nthread, verbose, middleSplit, maxObs, minTreesPerFold, featureWeights, featureWeightsVariables, deepFeatureWeights, deepFeatureWeightsVariables, observationWeights, customSplitSample, customAvgSample, customExcludeSample, monotonicConstraints, groupMemberships, monotoneAvg, hasNas, naDirection, linear, overfitPenalty, doubleTree)
}

rcpp_cppImputeInterface <- function(forest, x, seed) {
    .Call(`_Rforestry_rcpp_cppImputeInterface`, forest, x, seed)
}

