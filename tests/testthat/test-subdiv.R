context('Testing the subdiv() function')

rdirichlet = function(n, alpha) {
  k = length(alpha)
  r = matrix(0, nrow=n, ncol=k) 
  for (i in 1:k) {
    r[,i] = rgamma(n, alpha[i], 1)
  }
  r = matrix(mapply(function(r, s) {return (r/s)}, r, rowSums(r)), ncol=k)
  return (r)
}

numspecies <- 100
weights <- t(rdirichlet(1, rep(1, numspecies)))
numcommunities <- 8
manyweights <- t(rdirichlet(numcommunities, rep(1, numspecies)))

pop <- data.frame(a=c(1,1,0),b=c(2,0,0),c=c(3,1,0))
pop <- pop / sum(pop)

test_that("Subcommunity diversity across multiple populations", {
  expect_equivalent(as.matrix(subdiv(alpha(supercommunity(pop)), 0)$diversity), t(c(8,4,4)))
  expect_equivalent(as.matrix(subdiv(alphabar(supercommunity(pop)), 0)$diversity), t(c(2,1,2)))
  expect_equivalent(as.matrix(subdiv(beta(supercommunity(pop)), 0)$diversity), t(c(1/4,1/3,1/2)))
  expect_equivalent(as.matrix(subdiv(betabar(supercommunity(pop)), 0)$diversity), t(c(1,4/3,1)))
  expect_equivalent(as.matrix(subdiv(rho(supercommunity(pop)), 0)$diversity), t(c(4,3,2)))
  expect_equivalent(as.matrix(subdiv(rhobar(supercommunity(pop)), 0)$diversity), t(c(1,3/4,1)))
  expect_equivalent(as.matrix(subdiv(gamma(supercommunity(pop)), 0)$diversity), t(c(8/3,4/3,2)))
})

