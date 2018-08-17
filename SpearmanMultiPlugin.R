
#print("0\n");
p_value <- 0.001;
#print("1\n");
libs <- c("Hmisc");
#print("2\n");
lapply(libs, require, character.only=T);

#print("3\n");
input <- function(inputfile) {
  #print("HELLO WORLD\n");
  readline();
  d = read.table(inputfile);
  #print("B\n");
  file1 <- d[1, 1]
  #print("D MATRIX\n");
  #print(d);
  #print("FIRST FILE\n");
  #print(file1);
  #print("C\n");
  file2 <- d[2, 1]
  #print("D\n");
  #print(file2);

  rank1 <<- read.csv(toString(file1), header = TRUE);
  #print("E\n");
  rank2 <<- read.csv(toString(file2), header = TRUE);
  #print("F\n");
}


#print("4\n");
run <- function() {
  # Matrix 1
  cn1 <<- colnames(rank1);
  cn1 <<- cn1[2:length(cn1)];
  rank1 <<- rank1[,-1];
  rank1 <<- apply(rank1, 1, as.numeric);
  rank1 <<- t(rank1);

  cn2 <<- colnames(rank2);
  cn2 <<- cn2[2:length(cn2)];
  rank2 <<- rank2[,-1];
  rank2 <<- apply(rank2, 1, as.numeric);
  rank2 <<- t(rank2);

  #print("G\n");
  #print("RANK1\n");
  #print(rank1);
  #print("RANK2\n");
  #print(rank2);
  #print("NOW DOING CORRELATIONS\n");
  correlations <<- rcorr(rank1[,], rank2[,], type=c("spearman"));
  #print("H\n");
  pc <<- as.matrix(correlations$r);
  pc[is.na(pc)] <<- 0;
  empty <- c("");

  # Zero out edges above the p-value threshold
  pc[which(correlations$P>p_value)] <<- 0;
  #print("PC\n");
  #print(pc);

  # Zero out non-heterogeneous edges
  #print("I\n");
  for(i in 1:ncol(rank1)) {
     for(j in 1:ncol(rank1)) {
        pc[i, j] <<- 0;
     }
  }
  #print("J\n");
  #print("NCOL\n");
  #print(ncol(pc));
  x <- ncol(rank1)+1
  y <- ncol(pc)
  #print(x);
  #print(y);
  for(i in x:y) {
     for (j in x:y) {
        #print(i);
        #print(j);
        #print(ncol(pc));
        pc[i, j] <<- 0;
     }
  }
  #print("K\n");
}

#print("5\n");
output <- function(outputfile) {
   #print("PC:\n");
   #print(pc);
   write.table(pc, file=outputfile, sep=",", append=FALSE, row.names=unlist(c(cn1, cn2)), col.names=unlist(c(cn1,cn2)), na="");
}


#print("6\n");
