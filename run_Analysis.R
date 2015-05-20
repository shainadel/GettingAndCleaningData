# Setting Generic File Names
Train_File_Name <- "train//X_train.txt"
Train_User_Id_File_Name <- "train//subject_train.txt"
Train_Activity_Id_File_Name <- "train//y_train.txt"

Test_File_Name <- "test//X_test.txt"
Test_User_Id_File_Name <- "test//subject_test.txt"
Test_Activity_Id_File_Name <- "test//y_test.txt"


Feature_Name <-read.table("features.txt")
Activity_list <-read.table("activity_labels.txt")

# Getting Training Data
File_Name = Train_File_Name
Activity_Id_File = Train_Activity_Id_File_Name
User_Id_File = Train_User_Id_File_Name

DataSet <-read.table(File_Name, row.names = c(), col.names = Feature_Name[,2])
Act_ID <-read.table(Activity_Id_File)
Activity_Name <- factor(Act_ID[,1],c(1:6),Activity_list[,2])
DataSet2 <-cbind(Activity_Name,DataSet)
User_Id <- read.table(User_Id_File)
Train_DataSet <- cbind(User_Id, DataSet2)
colnames(Train_DataSet)[1] <- "User_Id"

# Getting Test Data
File_Name = Test_File_Name
Activity_Id_File = Test_Activity_Id_File_Name
User_Id_File = Test_User_Id_File_Name

DataSet <-read.table(File_Name, row.names = c(), col.names = Feature_Name[,2])
Act_ID <-read.table(Activity_Id_File)
Activity_Name <- factor(Act_ID[,1],c(1:6),Activity_list[,2])
DataSet2 <-cbind(Activity_Name,DataSet)
User_Id <- read.table(User_Id_File)
Test_DataSet <- cbind(User_Id, DataSet2)
colnames(Test_DataSet)[1] <- "User_Id"

#Merge The Data Sets 
FullData <- rbind(Train_DataSet, Test_DataSet)

#Sorting
#SortedData <- FullData[order(FullData$"User_Id"),]

COl_Names <- colnames(FullData)
# Getting relevant Culomns only (User_Id, Activity_Name, Mean, Std ,Columns)
Filtered_Set <- FullData[,c(1, 2, sort(c(grep("mean",COl_Names),grep("std",COl_Names))))]

Column_Count <- dim(Filtered_Set)[2]
# Creating A new Clean Data set 
Average_set <- Filtered_Set[1,]
# Clearing the 1st line 
Average_set[1:Column_Count] <- 0

# Looping on User Id
for(i in (sort(unique(Filtered_Set$"User_Id"))))
{
  User_Row <- Filtered_Set[Filtered_Set$"User_Id" == i,]
  # Looping On Activity Id
  for(j in unique(User_Row$"Activity_Name"))
  {
    #Getting a specific Activity For a User
    User_Activity_Row <- User_Row[User_Row$"Activity_Name" == j,]
    Average_Row <- vector(length = Column_Count)
    Average_Row[1] <- i 
    Average_Row[2] <- j 
    # Calculating the Average
    for (k in c(3:Column_Count))
    {
      Average_Row[k] <- mean(User_Activity_Row[,k])
    }
    # Adding the Average Value line to the Set
    Average_set <- rbind(Average_set, Average_Row)
  }
}
# removing the first empty row added in the creation process
Average_set <- Average_set [-c(1),]
# Step #5 Data Set File 
write.table(Average_set,"New_Data_File", row.names = FALSE)





