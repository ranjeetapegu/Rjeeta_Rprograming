StateRanking <- function(schooltype,yr,num="best") {
  ##Read number student vs teachers Data from csv
  studentTeacher <-read.csv("Number_of_Students_Per_Teacher-Primary_and_Upper_primary_School.csv",colClasses = "character",header=TRUE)
  st=toupper(schooltype)
  ##check that state and outcome are valid
  
  if(yr > 2010 ){
    print("the data is only from 2001 to 2010")
  } else if (yr < 2001 ) {
    print("the data is only from 2001 to 2010")
  } else 
    
    coln <- as.numeric(substr(yr,3,4)) + 2
  ## State wise and Year wise number of student versus teacher
  data1 <- studentTeacher[toupper(studentTeacher$School.Type)==st,c(1,2,coln)] 
  ##Renaming column
  names(data1)[3]<-"NosofTeacher" ;
  data1 <- data1[order(as.numeric(data1$NosofTeacher)),,]
  nr <- nrow(data1)
  if (nr == "0") { print("invalid schooltype")
  } else 
  if (num =="best"){
  r3 <- transform(data1,state_rank = ave(as.numeric(data1$NosofTeacher),FUN = function(x) rank(x, ties.method = "first")))
  ##  r3 <- transform(data1,state_rank = ave(as.numeric(data1$NosofTeacher),data1$India..State..UTs,data1$School.Type ,  FUN = function(x) rank(- x, ties.method = "first")))
    r4 <- r3[r3$state_rank==1,,]
  } else if (num =="worst") {
   ## r3 <- transform(data1,state_rank = ave(as.numeric(data1$NosofTeacher),data1$India..State..UTs,data1$School.Type ,  FUN = function(x) rank(- x, ties.method = "first")))
  r3 <- transform(data1,state_rank = ave(as.numeric(data1$NosofTeacher),FUN = function(x) rank(-x, ties.method = "first")))
    r4 <- r3[r3$state_rank==1,,]
  } else
  r3 <- transform(data1,state_rank = ave(as.numeric(data1$NosofTeacher),FUN = function(x) rank(x, ties.method = "first")))
  ## r3 <- transform(data1,state_rank = ave(as.numeric(data1$NosofTeacher),data1$India..State..UTs,data1$School.Type ,  FUN = function(x) rank(- x, ties.method = "first")))
    nu= as.numeric(num)
  r4 <- r3[r3$state_rank==nu,,]

  print(r4)
}