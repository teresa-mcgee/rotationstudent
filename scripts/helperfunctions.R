getFounderList<- function(){
  return(c("A.J.","C57Bl.6J","X129SvlmJ","NOD","NZO","CAST","PWK","WSB"))
}

getDiploProb<- function(qtl_locus,directory = getwd()){
  
  # files are for each CC Strain, where the columns are 36 Diplotypes and rows
  # are SNPs
  # column 1:3 are the SNP id, chromosome, and location
  
  genoFiles<- list.files(directory)
  #creating an empty data frame 
  geno<- as.data.frame(matrix(nrow = length(genoFiles),ncol = 39))
  
  CCStrain<- seq(1:length(genoFiles))
  #pull the row with the snp loci from each file
  for(i in 1:(length(genoFiles))){
    #import file into dataframe
    tempGeno<- read.csv(file = paste(directory,genoFiles[i],sep = "/"))
    #add the row to the diplostate dataframe
    #some of the marker names (ones with b6 to begin) have 
    #extra characters
    tempGeno$marker<-gsub(pattern = '_',
                          replacement = "",
                          x = tempGeno$marker)
    tempGeno$marker<-gsub(pattern = '-',
                          replacement = "",
                          x = tempGeno$marker)
    geno[i,]<- tempGeno[tempGeno$marker == qtl_locus,]
    #track which CC strain this came from so the row names are in order
    CCStrain[i]<- unlist(strsplit(genoFiles[i],split = "_")[[1]][1])
  }
  
  #naming rows and columns for diplostates and strains
  rownames(geno)<- CCStrain
  colnames(geno)<- colnames(tempGeno)
  
  return(geno)
}
