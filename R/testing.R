# testRowsWeights <- gridRowsWeightsSpPoly(spgrid = tMaxGrid, poly = states[1:2,], 
#                                          idPoly = states$STATEFP[1:2])
# 
# cl <- makeCluster(detectCores())
# registerDoParallel(cl)
# testRowsWeightsPar <- gridRowsWeightsSpPolyPar(cl = cl,
#                       spgrid = tMaxGrid, 
#                       poly = states[1:2,], 
#                       idPoly = states$STATEFP[1:2])
# 
# stopCluster(cl)
# sapply(1:length(testRowsWeights), function(x) {
#   all(testRowsWeights[[x]] == testRowsWeightsPar[[x]])
# })
# polyNew <- overGridPoly(spgrid = tMaxGrid, poly = states[1:2,],
#              idPoly = states$STATEFP,
#              gridVar = "band1")
