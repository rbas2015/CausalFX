test_that("wppIntervalGenerationAnalytic gives Balke-Pearl bounds", {
  out1 = out2 = matrix(NA, 1e3,2)
  
  
  for (i in 1:1e3) {
    tmp = list(valid=FALSE)
    # generate distribution satisfying IV bounds
    while (!tmp$valid) {
      p = rprobdist(2,3,3)
      tmp = binAnalyticalIV(matrix(p,1,8))
    }
    
    out1[i,] = tmp$bounds   # Balke-Pearl bounds
    out2[i,] = wppIntervalGenerationAnalytical(p[1:4], p[1:4+4], P_W=0.5, eps=c(0,1,1,1,1,1))
  }
  
  # bounds should be same to numerical tolerance
  expect_equal(out1, out2)
})
