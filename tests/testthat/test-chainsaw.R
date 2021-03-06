context('Testing chainsaw')

test_that("Answers match up in the ultrametric case", {
  tree <- ape::read.tree(text="(A:2,B:2);")
  leaf.abundance <- c(0.6, 0.4)
  data <- as.rdphylo(leaf.abundance, tree)
  interval <- 2
  super <- chainsaw(data, leaf.abundance, interval)
  
  tree2 <- ape::read.tree(text="(A:2,B:2)R:2;")
  super2 <- supercommunity(leaf.abundance, tree2) # type_weights
  
  expect_equivalent(supercommunity.A.bar(super, 0:2), 
                    supercommunity.A.bar(super2, 0:2))
  
})
  

test_that("Answers match up in the non-ultrametric case", {
  tree <- ape::read.tree(text="(A:1,B:2);")
  leaf.abundance <- c(0.6, 0.4)
  data <- as.rdphylo(leaf.abundance, tree)
  interval <- 2
  expect_warning(super <- chainsaw(data, leaf.abundance, interval))
  
  tree2 <- ape::read.tree(text="(A:1,B:2)R:2;")
  super2 <- supercommunity(leaf.abundance, tree2) # type_weights
  
  expect_equivalent(supercommunity.A.bar(super, 0:2), 
                    supercommunity.A.bar(super2, 0:2))
})


test_that("Setting chainsaw interval to 1 returns the phylogeny intact", {
  tree2 <- ape::read.tree(text="(A:1,B:2)R:2;")
  leaf.abundance <- c(0.6, 0.4)
  super2 <- supercommunity(leaf.abundance, tree2) # type_weights
  
expect_equal(super2, chainsaw(rdphylo(leaf.abundance, tree2), leaf.abundance, 1))
})