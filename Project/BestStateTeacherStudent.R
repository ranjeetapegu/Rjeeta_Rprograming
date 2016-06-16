
BestSchoolState <- function(schooltype,yr) {
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
   nr <- nrow(data1)
  if (nr == "0") { print("invalid schooltype")
  } else 
    teachers <-min(as.numeric(data1[,3]),na.rm=TRUE)
    bestState <-na.omit(data1[data1$NosofTeacher ==teachers,,])
    print(bestState)
}
