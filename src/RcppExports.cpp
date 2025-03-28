// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <RcppThread.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// rcpp_cppDataFrameInterface
SEXP rcpp_cppDataFrameInterface(Rcpp::List x, Rcpp::NumericVector y, Rcpp::NumericVector catCols, Rcpp::NumericVector linCols, int numRows, int numColumns, Rcpp::NumericVector featureWeights, Rcpp::NumericVector featureWeightsVariables, Rcpp::NumericVector deepFeatureWeights, Rcpp::NumericVector deepFeatureWeightsVariables, Rcpp::NumericVector observationWeights, Rcpp::NumericVector monotonicConstraints);
RcppExport SEXP _Rforestry_rcpp_cppDataFrameInterface(SEXP xSEXP, SEXP ySEXP, SEXP catColsSEXP, SEXP linColsSEXP, SEXP numRowsSEXP, SEXP numColumnsSEXP, SEXP featureWeightsSEXP, SEXP featureWeightsVariablesSEXP, SEXP deepFeatureWeightsSEXP, SEXP deepFeatureWeightsVariablesSEXP, SEXP observationWeightsSEXP, SEXP monotonicConstraintsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type catCols(catColsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type linCols(linColsSEXP);
    Rcpp::traits::input_parameter< int >::type numRows(numRowsSEXP);
    Rcpp::traits::input_parameter< int >::type numColumns(numColumnsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type featureWeights(featureWeightsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type featureWeightsVariables(featureWeightsVariablesSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type deepFeatureWeights(deepFeatureWeightsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type deepFeatureWeightsVariables(deepFeatureWeightsVariablesSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type observationWeights(observationWeightsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type monotonicConstraints(monotonicConstraintsSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_cppDataFrameInterface(x, y, catCols, linCols, numRows, numColumns, featureWeights, featureWeightsVariables, deepFeatureWeights, deepFeatureWeightsVariables, observationWeights, monotonicConstraints));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_cppBuildInterface
SEXP rcpp_cppBuildInterface(Rcpp::List x, Rcpp::NumericVector y, Rcpp::NumericVector catCols, Rcpp::NumericVector linCols, int numRows, int numColumns, int ntree, bool replace, int sampsize, int mtry, double splitratio, int nodesizeSpl, int nodesizeAvg, int nodesizeStrictSpl, int nodesizeStrictAvg, double minSplitGain, int maxDepth, int interactionDepth, int seed, int nthread, bool verbose, bool middleSplit, int maxObs, Rcpp::NumericVector featureWeights, Rcpp::NumericVector featureWeightsVariables, Rcpp::NumericVector deepFeatureWeights, Rcpp::NumericVector deepFeatureWeightsVariables, Rcpp::NumericVector observationWeights, Rcpp::NumericVector monotonicConstraints, bool hasNas, bool linear, double overfitPenalty, bool doubleTree, bool existing_dataframe_flag, SEXP existing_dataframe);
RcppExport SEXP _Rforestry_rcpp_cppBuildInterface(SEXP xSEXP, SEXP ySEXP, SEXP catColsSEXP, SEXP linColsSEXP, SEXP numRowsSEXP, SEXP numColumnsSEXP, SEXP ntreeSEXP, SEXP replaceSEXP, SEXP sampsizeSEXP, SEXP mtrySEXP, SEXP splitratioSEXP, SEXP nodesizeSplSEXP, SEXP nodesizeAvgSEXP, SEXP nodesizeStrictSplSEXP, SEXP nodesizeStrictAvgSEXP, SEXP minSplitGainSEXP, SEXP maxDepthSEXP, SEXP interactionDepthSEXP, SEXP seedSEXP, SEXP nthreadSEXP, SEXP verboseSEXP, SEXP middleSplitSEXP, SEXP maxObsSEXP, SEXP featureWeightsSEXP, SEXP featureWeightsVariablesSEXP, SEXP deepFeatureWeightsSEXP, SEXP deepFeatureWeightsVariablesSEXP, SEXP observationWeightsSEXP, SEXP monotonicConstraintsSEXP, SEXP hasNasSEXP, SEXP linearSEXP, SEXP overfitPenaltySEXP, SEXP doubleTreeSEXP, SEXP existing_dataframe_flagSEXP, SEXP existing_dataframeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type catCols(catColsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type linCols(linColsSEXP);
    Rcpp::traits::input_parameter< int >::type numRows(numRowsSEXP);
    Rcpp::traits::input_parameter< int >::type numColumns(numColumnsSEXP);
    Rcpp::traits::input_parameter< int >::type ntree(ntreeSEXP);
    Rcpp::traits::input_parameter< bool >::type replace(replaceSEXP);
    Rcpp::traits::input_parameter< int >::type sampsize(sampsizeSEXP);
    Rcpp::traits::input_parameter< int >::type mtry(mtrySEXP);
    Rcpp::traits::input_parameter< double >::type splitratio(splitratioSEXP);
    Rcpp::traits::input_parameter< int >::type nodesizeSpl(nodesizeSplSEXP);
    Rcpp::traits::input_parameter< int >::type nodesizeAvg(nodesizeAvgSEXP);
    Rcpp::traits::input_parameter< int >::type nodesizeStrictSpl(nodesizeStrictSplSEXP);
    Rcpp::traits::input_parameter< int >::type nodesizeStrictAvg(nodesizeStrictAvgSEXP);
    Rcpp::traits::input_parameter< double >::type minSplitGain(minSplitGainSEXP);
    Rcpp::traits::input_parameter< int >::type maxDepth(maxDepthSEXP);
    Rcpp::traits::input_parameter< int >::type interactionDepth(interactionDepthSEXP);
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    Rcpp::traits::input_parameter< int >::type nthread(nthreadSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    Rcpp::traits::input_parameter< bool >::type middleSplit(middleSplitSEXP);
    Rcpp::traits::input_parameter< int >::type maxObs(maxObsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type featureWeights(featureWeightsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type featureWeightsVariables(featureWeightsVariablesSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type deepFeatureWeights(deepFeatureWeightsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type deepFeatureWeightsVariables(deepFeatureWeightsVariablesSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type observationWeights(observationWeightsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type monotonicConstraints(monotonicConstraintsSEXP);
    Rcpp::traits::input_parameter< bool >::type hasNas(hasNasSEXP);
    Rcpp::traits::input_parameter< bool >::type linear(linearSEXP);
    Rcpp::traits::input_parameter< double >::type overfitPenalty(overfitPenaltySEXP);
    Rcpp::traits::input_parameter< bool >::type doubleTree(doubleTreeSEXP);
    Rcpp::traits::input_parameter< bool >::type existing_dataframe_flag(existing_dataframe_flagSEXP);
    Rcpp::traits::input_parameter< SEXP >::type existing_dataframe(existing_dataframeSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_cppBuildInterface(x, y, catCols, linCols, numRows, numColumns, ntree, replace, sampsize, mtry, splitratio, nodesizeSpl, nodesizeAvg, nodesizeStrictSpl, nodesizeStrictAvg, minSplitGain, maxDepth, interactionDepth, seed, nthread, verbose, middleSplit, maxObs, featureWeights, featureWeightsVariables, deepFeatureWeights, deepFeatureWeightsVariables, observationWeights, monotonicConstraints, hasNas, linear, overfitPenalty, doubleTree, existing_dataframe_flag, existing_dataframe));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_cppMultilayerBuildInterface
SEXP rcpp_cppMultilayerBuildInterface(Rcpp::List x, Rcpp::NumericVector y, Rcpp::NumericVector catCols, Rcpp::NumericVector linCols, int numRows, int numColumns, int ntree, int nrounds, double eta, bool replace, int sampsize, int mtry, double splitratio, int nodesizeSpl, int nodesizeAvg, int nodesizeStrictSpl, int nodesizeStrictAvg, double minSplitGain, int maxDepth, int seed, int nthread, bool verbose, bool middleSplit, int maxObs, Rcpp::NumericVector featureWeights, Rcpp::NumericVector featureWeightsVariables, Rcpp::NumericVector deepFeatureWeights, Rcpp::NumericVector deepFeatureWeightsVariables, Rcpp::NumericVector observationWeights, Rcpp::NumericVector monotonicConstraints, bool linear, double overfitPenalty, bool doubleTree, bool existing_dataframe_flag, SEXP existing_dataframe);
RcppExport SEXP _Rforestry_rcpp_cppMultilayerBuildInterface(SEXP xSEXP, SEXP ySEXP, SEXP catColsSEXP, SEXP linColsSEXP, SEXP numRowsSEXP, SEXP numColumnsSEXP, SEXP ntreeSEXP, SEXP nroundsSEXP, SEXP etaSEXP, SEXP replaceSEXP, SEXP sampsizeSEXP, SEXP mtrySEXP, SEXP splitratioSEXP, SEXP nodesizeSplSEXP, SEXP nodesizeAvgSEXP, SEXP nodesizeStrictSplSEXP, SEXP nodesizeStrictAvgSEXP, SEXP minSplitGainSEXP, SEXP maxDepthSEXP, SEXP seedSEXP, SEXP nthreadSEXP, SEXP verboseSEXP, SEXP middleSplitSEXP, SEXP maxObsSEXP, SEXP featureWeightsSEXP, SEXP featureWeightsVariablesSEXP, SEXP deepFeatureWeightsSEXP, SEXP deepFeatureWeightsVariablesSEXP, SEXP observationWeightsSEXP, SEXP monotonicConstraintsSEXP, SEXP linearSEXP, SEXP overfitPenaltySEXP, SEXP doubleTreeSEXP, SEXP existing_dataframe_flagSEXP, SEXP existing_dataframeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type catCols(catColsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type linCols(linColsSEXP);
    Rcpp::traits::input_parameter< int >::type numRows(numRowsSEXP);
    Rcpp::traits::input_parameter< int >::type numColumns(numColumnsSEXP);
    Rcpp::traits::input_parameter< int >::type ntree(ntreeSEXP);
    Rcpp::traits::input_parameter< int >::type nrounds(nroundsSEXP);
    Rcpp::traits::input_parameter< double >::type eta(etaSEXP);
    Rcpp::traits::input_parameter< bool >::type replace(replaceSEXP);
    Rcpp::traits::input_parameter< int >::type sampsize(sampsizeSEXP);
    Rcpp::traits::input_parameter< int >::type mtry(mtrySEXP);
    Rcpp::traits::input_parameter< double >::type splitratio(splitratioSEXP);
    Rcpp::traits::input_parameter< int >::type nodesizeSpl(nodesizeSplSEXP);
    Rcpp::traits::input_parameter< int >::type nodesizeAvg(nodesizeAvgSEXP);
    Rcpp::traits::input_parameter< int >::type nodesizeStrictSpl(nodesizeStrictSplSEXP);
    Rcpp::traits::input_parameter< int >::type nodesizeStrictAvg(nodesizeStrictAvgSEXP);
    Rcpp::traits::input_parameter< double >::type minSplitGain(minSplitGainSEXP);
    Rcpp::traits::input_parameter< int >::type maxDepth(maxDepthSEXP);
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    Rcpp::traits::input_parameter< int >::type nthread(nthreadSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    Rcpp::traits::input_parameter< bool >::type middleSplit(middleSplitSEXP);
    Rcpp::traits::input_parameter< int >::type maxObs(maxObsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type featureWeights(featureWeightsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type featureWeightsVariables(featureWeightsVariablesSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type deepFeatureWeights(deepFeatureWeightsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type deepFeatureWeightsVariables(deepFeatureWeightsVariablesSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type observationWeights(observationWeightsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type monotonicConstraints(monotonicConstraintsSEXP);
    Rcpp::traits::input_parameter< bool >::type linear(linearSEXP);
    Rcpp::traits::input_parameter< double >::type overfitPenalty(overfitPenaltySEXP);
    Rcpp::traits::input_parameter< bool >::type doubleTree(doubleTreeSEXP);
    Rcpp::traits::input_parameter< bool >::type existing_dataframe_flag(existing_dataframe_flagSEXP);
    Rcpp::traits::input_parameter< SEXP >::type existing_dataframe(existing_dataframeSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_cppMultilayerBuildInterface(x, y, catCols, linCols, numRows, numColumns, ntree, nrounds, eta, replace, sampsize, mtry, splitratio, nodesizeSpl, nodesizeAvg, nodesizeStrictSpl, nodesizeStrictAvg, minSplitGain, maxDepth, seed, nthread, verbose, middleSplit, maxObs, featureWeights, featureWeightsVariables, deepFeatureWeights, deepFeatureWeightsVariables, observationWeights, monotonicConstraints, linear, overfitPenalty, doubleTree, existing_dataframe_flag, existing_dataframe));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_cppPredictInterface
Rcpp::List rcpp_cppPredictInterface(SEXP forest, Rcpp::List x, std::string aggregation, int seed);
RcppExport SEXP _Rforestry_rcpp_cppPredictInterface(SEXP forestSEXP, SEXP xSEXP, SEXP aggregationSEXP, SEXP seedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type forest(forestSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type x(xSEXP);
    Rcpp::traits::input_parameter< std::string >::type aggregation(aggregationSEXP);
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_cppPredictInterface(forest, x, aggregation, seed));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_cppMultilayerPredictInterface
Rcpp::List rcpp_cppMultilayerPredictInterface(SEXP multilayerForest, Rcpp::List x, std::string aggregation, int seed);
RcppExport SEXP _Rforestry_rcpp_cppMultilayerPredictInterface(SEXP multilayerForestSEXP, SEXP xSEXP, SEXP aggregationSEXP, SEXP seedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type multilayerForest(multilayerForestSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type x(xSEXP);
    Rcpp::traits::input_parameter< std::string >::type aggregation(aggregationSEXP);
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_cppMultilayerPredictInterface(multilayerForest, x, aggregation, seed));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_OBBPredictInterface
double rcpp_OBBPredictInterface(SEXP forest);
RcppExport SEXP _Rforestry_rcpp_OBBPredictInterface(SEXP forestSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type forest(forestSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_OBBPredictInterface(forest));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_OBBPredictionsInterface
Rcpp::NumericVector rcpp_OBBPredictionsInterface(SEXP forest);
RcppExport SEXP _Rforestry_rcpp_OBBPredictionsInterface(SEXP forestSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type forest(forestSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_OBBPredictionsInterface(forest));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_VariableImportanceInterface
Rcpp::List rcpp_VariableImportanceInterface(SEXP forest);
RcppExport SEXP _Rforestry_rcpp_VariableImportanceInterface(SEXP forestSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type forest(forestSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_VariableImportanceInterface(forest));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_getObservationSizeInterface
double rcpp_getObservationSizeInterface(SEXP df);
RcppExport SEXP _Rforestry_rcpp_getObservationSizeInterface(SEXP dfSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type df(dfSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_getObservationSizeInterface(df));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_AddTreeInterface
void rcpp_AddTreeInterface(SEXP forest, int ntree);
RcppExport SEXP _Rforestry_rcpp_AddTreeInterface(SEXP forestSEXP, SEXP ntreeSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type forest(forestSEXP);
    Rcpp::traits::input_parameter< int >::type ntree(ntreeSEXP);
    rcpp_AddTreeInterface(forest, ntree);
    return R_NilValue;
END_RCPP
}
// rcpp_CppToR_translator
Rcpp::List rcpp_CppToR_translator(SEXP forest);
RcppExport SEXP _Rforestry_rcpp_CppToR_translator(SEXP forestSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type forest(forestSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_CppToR_translator(forest));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_gammas_translator
Rcpp::NumericVector rcpp_gammas_translator(SEXP multilayerForest);
RcppExport SEXP _Rforestry_rcpp_gammas_translator(SEXP multilayerForestSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type multilayerForest(multilayerForestSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_gammas_translator(multilayerForest));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_residuals_translator
Rcpp::List rcpp_residuals_translator(SEXP multilayerForest);
RcppExport SEXP _Rforestry_rcpp_residuals_translator(SEXP multilayerForestSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type multilayerForest(multilayerForestSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_residuals_translator(multilayerForest));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_multilayer_CppToR_translator
Rcpp::List rcpp_multilayer_CppToR_translator(SEXP multilayerForest);
RcppExport SEXP _Rforestry_rcpp_multilayer_CppToR_translator(SEXP multilayerForestSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type multilayerForest(multilayerForestSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_multilayer_CppToR_translator(multilayerForest));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_reconstructree
Rcpp::List rcpp_reconstructree(Rcpp::List x, Rcpp::NumericVector y, Rcpp::NumericVector catCols, Rcpp::NumericVector linCols, int numRows, int numColumns, Rcpp::List R_forest, bool replace, int sampsize, double splitratio, int mtry, int nodesizeSpl, int nodesizeAvg, int nodesizeStrictSpl, int nodesizeStrictAvg, double minSplitGain, int maxDepth, int interactionDepth, int seed, int nthread, bool verbose, bool middleSplit, int maxObs, Rcpp::NumericVector featureWeights, Rcpp::NumericVector featureWeightsVariables, Rcpp::NumericVector deepFeatureWeights, Rcpp::NumericVector deepFeatureWeightsVariables, Rcpp::NumericVector observationWeights, Rcpp::NumericVector monotonicConstraints, bool hasNas, bool linear, double overfitPenalty, bool doubleTree);
RcppExport SEXP _Rforestry_rcpp_reconstructree(SEXP xSEXP, SEXP ySEXP, SEXP catColsSEXP, SEXP linColsSEXP, SEXP numRowsSEXP, SEXP numColumnsSEXP, SEXP R_forestSEXP, SEXP replaceSEXP, SEXP sampsizeSEXP, SEXP splitratioSEXP, SEXP mtrySEXP, SEXP nodesizeSplSEXP, SEXP nodesizeAvgSEXP, SEXP nodesizeStrictSplSEXP, SEXP nodesizeStrictAvgSEXP, SEXP minSplitGainSEXP, SEXP maxDepthSEXP, SEXP interactionDepthSEXP, SEXP seedSEXP, SEXP nthreadSEXP, SEXP verboseSEXP, SEXP middleSplitSEXP, SEXP maxObsSEXP, SEXP featureWeightsSEXP, SEXP featureWeightsVariablesSEXP, SEXP deepFeatureWeightsSEXP, SEXP deepFeatureWeightsVariablesSEXP, SEXP observationWeightsSEXP, SEXP monotonicConstraintsSEXP, SEXP hasNasSEXP, SEXP linearSEXP, SEXP overfitPenaltySEXP, SEXP doubleTreeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type catCols(catColsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type linCols(linColsSEXP);
    Rcpp::traits::input_parameter< int >::type numRows(numRowsSEXP);
    Rcpp::traits::input_parameter< int >::type numColumns(numColumnsSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type R_forest(R_forestSEXP);
    Rcpp::traits::input_parameter< bool >::type replace(replaceSEXP);
    Rcpp::traits::input_parameter< int >::type sampsize(sampsizeSEXP);
    Rcpp::traits::input_parameter< double >::type splitratio(splitratioSEXP);
    Rcpp::traits::input_parameter< int >::type mtry(mtrySEXP);
    Rcpp::traits::input_parameter< int >::type nodesizeSpl(nodesizeSplSEXP);
    Rcpp::traits::input_parameter< int >::type nodesizeAvg(nodesizeAvgSEXP);
    Rcpp::traits::input_parameter< int >::type nodesizeStrictSpl(nodesizeStrictSplSEXP);
    Rcpp::traits::input_parameter< int >::type nodesizeStrictAvg(nodesizeStrictAvgSEXP);
    Rcpp::traits::input_parameter< double >::type minSplitGain(minSplitGainSEXP);
    Rcpp::traits::input_parameter< int >::type maxDepth(maxDepthSEXP);
    Rcpp::traits::input_parameter< int >::type interactionDepth(interactionDepthSEXP);
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    Rcpp::traits::input_parameter< int >::type nthread(nthreadSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    Rcpp::traits::input_parameter< bool >::type middleSplit(middleSplitSEXP);
    Rcpp::traits::input_parameter< int >::type maxObs(maxObsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type featureWeights(featureWeightsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type featureWeightsVariables(featureWeightsVariablesSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type deepFeatureWeights(deepFeatureWeightsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type deepFeatureWeightsVariables(deepFeatureWeightsVariablesSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type observationWeights(observationWeightsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type monotonicConstraints(monotonicConstraintsSEXP);
    Rcpp::traits::input_parameter< bool >::type hasNas(hasNasSEXP);
    Rcpp::traits::input_parameter< bool >::type linear(linearSEXP);
    Rcpp::traits::input_parameter< double >::type overfitPenalty(overfitPenaltySEXP);
    Rcpp::traits::input_parameter< bool >::type doubleTree(doubleTreeSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_reconstructree(x, y, catCols, linCols, numRows, numColumns, R_forest, replace, sampsize, splitratio, mtry, nodesizeSpl, nodesizeAvg, nodesizeStrictSpl, nodesizeStrictAvg, minSplitGain, maxDepth, interactionDepth, seed, nthread, verbose, middleSplit, maxObs, featureWeights, featureWeightsVariables, deepFeatureWeights, deepFeatureWeightsVariables, observationWeights, monotonicConstraints, hasNas, linear, overfitPenalty, doubleTree));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_reconstruct_forests
Rcpp::List rcpp_reconstruct_forests(Rcpp::List x, Rcpp::NumericVector y, Rcpp::NumericVector catCols, Rcpp::NumericVector linCols, int numRows, int numColumns, Rcpp::List R_forests, Rcpp::List R_residuals, int nrounds, double eta, bool replace, int sampsize, double splitratio, int mtry, int nodesizeSpl, int nodesizeAvg, int nodesizeStrictSpl, int nodesizeStrictAvg, double minSplitGain, int maxDepth, int seed, int nthread, bool verbose, bool middleSplit, int maxObs, Rcpp::NumericVector featureWeights, Rcpp::NumericVector featureWeightsVariables, Rcpp::NumericVector deepFeatureWeights, Rcpp::NumericVector deepFeatureWeightsVariables, Rcpp::NumericVector observationWeights, Rcpp::NumericVector monotonicConstraints, Rcpp::NumericVector gammas, bool linear, double overfitPenalty, bool doubleTree);
RcppExport SEXP _Rforestry_rcpp_reconstruct_forests(SEXP xSEXP, SEXP ySEXP, SEXP catColsSEXP, SEXP linColsSEXP, SEXP numRowsSEXP, SEXP numColumnsSEXP, SEXP R_forestsSEXP, SEXP R_residualsSEXP, SEXP nroundsSEXP, SEXP etaSEXP, SEXP replaceSEXP, SEXP sampsizeSEXP, SEXP splitratioSEXP, SEXP mtrySEXP, SEXP nodesizeSplSEXP, SEXP nodesizeAvgSEXP, SEXP nodesizeStrictSplSEXP, SEXP nodesizeStrictAvgSEXP, SEXP minSplitGainSEXP, SEXP maxDepthSEXP, SEXP seedSEXP, SEXP nthreadSEXP, SEXP verboseSEXP, SEXP middleSplitSEXP, SEXP maxObsSEXP, SEXP featureWeightsSEXP, SEXP featureWeightsVariablesSEXP, SEXP deepFeatureWeightsSEXP, SEXP deepFeatureWeightsVariablesSEXP, SEXP observationWeightsSEXP, SEXP monotonicConstraintsSEXP, SEXP gammasSEXP, SEXP linearSEXP, SEXP overfitPenaltySEXP, SEXP doubleTreeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type catCols(catColsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type linCols(linColsSEXP);
    Rcpp::traits::input_parameter< int >::type numRows(numRowsSEXP);
    Rcpp::traits::input_parameter< int >::type numColumns(numColumnsSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type R_forests(R_forestsSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type R_residuals(R_residualsSEXP);
    Rcpp::traits::input_parameter< int >::type nrounds(nroundsSEXP);
    Rcpp::traits::input_parameter< double >::type eta(etaSEXP);
    Rcpp::traits::input_parameter< bool >::type replace(replaceSEXP);
    Rcpp::traits::input_parameter< int >::type sampsize(sampsizeSEXP);
    Rcpp::traits::input_parameter< double >::type splitratio(splitratioSEXP);
    Rcpp::traits::input_parameter< int >::type mtry(mtrySEXP);
    Rcpp::traits::input_parameter< int >::type nodesizeSpl(nodesizeSplSEXP);
    Rcpp::traits::input_parameter< int >::type nodesizeAvg(nodesizeAvgSEXP);
    Rcpp::traits::input_parameter< int >::type nodesizeStrictSpl(nodesizeStrictSplSEXP);
    Rcpp::traits::input_parameter< int >::type nodesizeStrictAvg(nodesizeStrictAvgSEXP);
    Rcpp::traits::input_parameter< double >::type minSplitGain(minSplitGainSEXP);
    Rcpp::traits::input_parameter< int >::type maxDepth(maxDepthSEXP);
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    Rcpp::traits::input_parameter< int >::type nthread(nthreadSEXP);
    Rcpp::traits::input_parameter< bool >::type verbose(verboseSEXP);
    Rcpp::traits::input_parameter< bool >::type middleSplit(middleSplitSEXP);
    Rcpp::traits::input_parameter< int >::type maxObs(maxObsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type featureWeights(featureWeightsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type featureWeightsVariables(featureWeightsVariablesSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type deepFeatureWeights(deepFeatureWeightsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type deepFeatureWeightsVariables(deepFeatureWeightsVariablesSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type observationWeights(observationWeightsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type monotonicConstraints(monotonicConstraintsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type gammas(gammasSEXP);
    Rcpp::traits::input_parameter< bool >::type linear(linearSEXP);
    Rcpp::traits::input_parameter< double >::type overfitPenalty(overfitPenaltySEXP);
    Rcpp::traits::input_parameter< bool >::type doubleTree(doubleTreeSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_reconstruct_forests(x, y, catCols, linCols, numRows, numColumns, R_forests, R_residuals, nrounds, eta, replace, sampsize, splitratio, mtry, nodesizeSpl, nodesizeAvg, nodesizeStrictSpl, nodesizeStrictAvg, minSplitGain, maxDepth, seed, nthread, verbose, middleSplit, maxObs, featureWeights, featureWeightsVariables, deepFeatureWeights, deepFeatureWeightsVariables, observationWeights, monotonicConstraints, gammas, linear, overfitPenalty, doubleTree));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_cppImputeInterface
std::vector< std::vector<double> > rcpp_cppImputeInterface(SEXP forest, Rcpp::List x, int seed);
RcppExport SEXP _Rforestry_rcpp_cppImputeInterface(SEXP forestSEXP, SEXP xSEXP, SEXP seedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type forest(forestSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_cppImputeInterface(forest, x, seed));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_Rforestry_rcpp_cppDataFrameInterface", (DL_FUNC) &_Rforestry_rcpp_cppDataFrameInterface, 12},
    {"_Rforestry_rcpp_cppBuildInterface", (DL_FUNC) &_Rforestry_rcpp_cppBuildInterface, 35},
    {"_Rforestry_rcpp_cppMultilayerBuildInterface", (DL_FUNC) &_Rforestry_rcpp_cppMultilayerBuildInterface, 35},
    {"_Rforestry_rcpp_cppPredictInterface", (DL_FUNC) &_Rforestry_rcpp_cppPredictInterface, 4},
    {"_Rforestry_rcpp_cppMultilayerPredictInterface", (DL_FUNC) &_Rforestry_rcpp_cppMultilayerPredictInterface, 4},
    {"_Rforestry_rcpp_OBBPredictInterface", (DL_FUNC) &_Rforestry_rcpp_OBBPredictInterface, 1},
    {"_Rforestry_rcpp_OBBPredictionsInterface", (DL_FUNC) &_Rforestry_rcpp_OBBPredictionsInterface, 1},
    {"_Rforestry_rcpp_VariableImportanceInterface", (DL_FUNC) &_Rforestry_rcpp_VariableImportanceInterface, 1},
    {"_Rforestry_rcpp_getObservationSizeInterface", (DL_FUNC) &_Rforestry_rcpp_getObservationSizeInterface, 1},
    {"_Rforestry_rcpp_AddTreeInterface", (DL_FUNC) &_Rforestry_rcpp_AddTreeInterface, 2},
    {"_Rforestry_rcpp_CppToR_translator", (DL_FUNC) &_Rforestry_rcpp_CppToR_translator, 1},
    {"_Rforestry_rcpp_gammas_translator", (DL_FUNC) &_Rforestry_rcpp_gammas_translator, 1},
    {"_Rforestry_rcpp_residuals_translator", (DL_FUNC) &_Rforestry_rcpp_residuals_translator, 1},
    {"_Rforestry_rcpp_multilayer_CppToR_translator", (DL_FUNC) &_Rforestry_rcpp_multilayer_CppToR_translator, 1},
    {"_Rforestry_rcpp_reconstructree", (DL_FUNC) &_Rforestry_rcpp_reconstructree, 33},
    {"_Rforestry_rcpp_reconstruct_forests", (DL_FUNC) &_Rforestry_rcpp_reconstruct_forests, 35},
    {"_Rforestry_rcpp_cppImputeInterface", (DL_FUNC) &_Rforestry_rcpp_cppImputeInterface, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_Rforestry(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
