library(BMLGrid)

addition_test = function ()
{
  cat("Running additional test...\n")
  set.seed(2)
  g=createBMLGrid(2,4,c(1,1))
  resultb=runBMLGrid(g,1)
  nextp=which(resultb$grid == "blue", arr.ind = TRUE)
  if (all(nextp!=c(2,3))) 
    stop ("Error in addition!")
  set.seed(2)
  resultr=runBMLGrid(g,2)
  nextp2=which(resultr$grid == "red", arr.ind = TRUE)
  if (all(nextp2!=c(2,2)))
    stop ("Error in addition!")
}