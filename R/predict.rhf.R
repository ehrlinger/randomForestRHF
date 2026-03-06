predict.rhf <- function(object,
                        newdata,
                        get.tree = NULL,
                        block.size = 10,
                        membership = TRUE,
                        seed = NULL,
                        do.trace = FALSE,
                        ...)
{
    predict.rhf.workhorse(object,
                          newdata,
                          get.tree = get.tree,
                          block.size = block.size,
                          membership = membership,
                          seed = seed,
                          do.trace = do.trace, ...)
}
