# Input format
# Judge poster score poster score poster score ...
#
# Judge name can have no spaces.  Tabs or spaces are ok between fields
#
#
# Adjust the variables as needed:
#    f <- names of files for each category
#    fnames <- descriptive names for f
#    fdir <- directory name
# assumes that the files have a ".txt" extension
#
#    judge.min <- minimum number of posters a judge must evaluate to allow standardization
#    poster.min <- each poster should be judged by at least this number

DEBUG <- 1 # 0=none, 1="print judging deficiencies", 2="add raw data tables"

#f <- c("student.res", "student.cv") # plain text files with the extension of ".txt"
f <- c("test1","test2") # plain text files with the extension of ".txt"
fdir <- "test"
f <- paste(fdir,"/",f,".txt",sep="")
#names(f) <- c("Student Research","Student Clinical Vignette")
names(f) <- c("Test Data file One","Test Data file Two")
judge.min <- 3
poster.min <- 3

#end of user modificiations

require(reshape2)
require(dplyr)

my.dcast <- function(...) { suppressMessages(dcast(...)) }
#my.dcast <- dcast

poster.score <- function(ifile) {
  cat("===================================================================================================\n")
	cat("=> ", ifile, "\n")
	cat("===================================================================================================\n")
  op <- options(digits = 5)
	if (DEBUG < 1) options( warn = -999 )

  get.data <- function(ifile) {
    input <- readLines(ifile)
    score <- result <- NULL
    for (i in input){
      x <- strsplit(i, '[[:space:]]+')[[1]]
      x.l <- length(x)
      result <- rbind(result,
            data.frame(judge = paste("j_", rep(x[1], x.l %/% 2), sep = ''),
                  poster = as.integer(x[seq(2, x.l, 2)]),
                  score = as.integer(x[seq(3, x.l, 2)]),
                  stringsAsFactors = FALSE))
  }
  return(result)
}

    check.counts <- function(melted=df) {
        if (DEBUG > 1) print(melted, row.names=FALSE)
        DF <- my.dcast(melted,poster~.); names(DF) <- c("Poster", "Count")
        cat("POSTERS WITH < 3 EVALUATIONS\n"); print(DF[(DF[,2]<poster.min),],row.names=FALSE)
        DF <- my.dcast(melted,judge~.); names(DF) <- c("Judge", "Count")
        cat("JUDGES WITH < 3 EVALUATIONS\n");  print(DF[(DF[,2]<judge.min),],row.names=FALSE)
    }

    df <- get.data(ifile) #judge poster score poster score ...
    if (DEBUG > 0) check.counts(df) #check numbers

    wide.scaled <- wide <- my.dcast(df, poster~judge,value="score")
    wide[,ncol(wide)+1] <- rowMeans(wide[,-1], na.rm=TRUE)  #add final column with mean poster score
    names(wide)[ncol(wide)] <- "raw.mean"
    if (DEBUG > 1) {
		  cat("RAW SCORES\n")
		  print(as.matrix(wide), row.names=FALSE, na.print="-")
		}

	wide.scaled[,-1] <- scale(wide.scaled[,-1]) #scale results to normal dist (0,1)
    wide.scaled[,ncol(wide.scaled)+1] <- rowMeans(wide.scaled[,-1],na.rm=TRUE)  #add final column with mean score
    names(wide.scaled)[ncol(wide.scaled)] <- "scaled.mean"
    if (DEBUG > 1) {
		  cat("SCALED SCORES\n")
		  print(as.matrix(wide.scaled),row.names=FALSE,na.print="-")
		}

	sorted.scaled<-with(wide.scaled,wide.scaled[ order(-scaled.mean, poster),])
    row.names(sorted.scaled) <- 1:nrow(sorted.scaled)
    top <- 10
    cat(paste("The top",top,"SORTED, SCALED SCORES\n"));
    #print(cbind(1:top,with(wide.scaled,head(wide.scaled[ order(-scaled.mean, poster) ,], top)[,c(1,ncol(wide.scaled))])),row.names=F)
    print(head(sorted.scaled,top)[,c(1,ncol(sorted.scaled))])
    if (DEBUG > 1) {
	    cat("|\n")
		  cat(paste("The bottom",top,"SORTED, SCALED SCORES\n"));
		  print(tail(sorted.scaled,top)[,c(1,ncol(sorted.scaled))])
		}
	cat("***************************************************************************************************\n\n")
	#{ sink("/dev/null"); options(op); sink(); }
}

junk <- lapply(f, poster.score)