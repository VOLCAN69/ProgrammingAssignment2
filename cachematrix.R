## Put comments here that give an overall description of what your functions do
   ## ......
   ## Not used functions on the end of file





## AutomatizedProcessMatrix - Automatizated generate Matrix and Reverse Matrix
   ## from params
AutomatizedProcessMatrix <- function(Ndepth = numeric
                                     , Nmin = numeric, Nmax = numeric){
        ## Return a matrix that is the inverse of 'x'
        
        ## Validators
        if(Ndepth <= 0 || Nmin <= 0 || Nmax <= 0){
                return(print("ERROR: Values must be > 0"))
        }
        #.Machine$integer.max
        
        ## Generate my special Matrix
        Nmat = genMatrixSquare(Ndepth, Nmin, Nmax)
        
        ## Generated Matrix (Normal) on CSV file
        write.csv(Nmat,"VOLCAN_Nmat.csv")
        #print(system.time(write.csv(Nmat,"PFC_Nmat.csv")))
        
        ## Returns Reverse Matrix from Nmat
        Rmat = setReverseMatrix(Nmat, Ndepth)
        
        ## Generated Matrix (Reverse) on CSV file
        write.csv(Nmat,"VOLCAN_Rmat.csv")
        #print(system.time(write.csv(Nmat,"VOLCAN_Rmat.csv")))
        
        ## Show (only) Reverse Matrix
        Rmat
}


## genMatrixSquare - Generate a Normal Matrix (random) from Params
genMatrixSquare <- function(Ndepth = numeric, Nmin = numeric, Nmax = numeric){

        ## Validators
        if(Ndepth <= 0 || Nmin <= 0 || Nmax <= 0){
                return(print("ERROR: Values must be > 0"))
        }
        
        ## Generate structure of matrix
        Nmat <- matrix(data = 0, nrow = Ndepth, ncol = Ndepth)

        set.seed(1)

        ## Nourish Matrix
        for(i in 1:Ndepth){
                Nmat[i,] <- as.integer(runif(Ndepth,Nmin,Nmax))
        }

        return(Nmat)
}


## setReverseMatrix - Copy structure from X and generate new Matrix
   ## (Reverse) from original passed trought param
   ## Aditionally, can define less Ndepth from Original for a little version
setReverseMatrix <- function(x = matrix(), Ndepth){
        Rmat <- matrix(data = 0, nrow = Ndepth, ncol = Ndepth)
        vec <- 0

        # Always same on extra corners
        Rmat[1,Ndepth] <- x[Ndepth,1]
        Rmat[Ndepth,1] <- x[1,Ndepth]
        
        # This part includes diag()
        for(n in 1:Ndepth){
                posV <- 1
                posH <- n
                
                dif <- posH-1
                laps <- Ndepth - posH + 1

                newX <- Ndepth
                newY <- laps
                
                for(i in 1:laps){
                        vec <- x[i+dif,i]
                        Rmat[newY,newX] <- vec
                        newX <- newX - 1
                        newY <- newY - 1           
                }
        }
        
        for(n in 2:Ndepth){
                posV <- n
                posH <- 1
                
                dif <- posV-1
                laps <- Ndepth - posV + 1
                
                newX <- Ndepth
                newY <- laps

                for(i in 1:laps){
                        vec <- x[i,i+dif]
                        Rmat[newX,newY] <- vec
                        newX <- newX - 1
                        newY <- newY - 1           
                }
        }
        
        return(Rmat)
}





## makeCacheMatrix - NOT USED
#makeCacheMatrix <- function(x = numeric(), ...){
#        makeVector(x)
#}


## makeVector - NOT USED
# makeVector <- function(x = numeric()){
#         m <- NULL
#         set <- function (y){
#                 x <<- y
#                 m <<- NULL
#         }
#         get <- function () x
#         setmean <- function (mean) m <<- mean
#         getmean <- function () m
#         list(set = set, get = get,
#              setmean = setmean,
#              getmean = getmean)
# }


## cachemean - NOT USED
# cachemean <- function(x, ...){
#         m <- x$getmean()
#         if(!is.null(m)){
#                 message("getting cached data")
#                 return(m)
#         }
#         data <- x$get()
#         m <- mean(data, ...)
#         x$setmean(m)
#         m
# }

