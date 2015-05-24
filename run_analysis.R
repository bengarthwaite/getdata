run_analysis <- function() {
        ## Load required libraries
        library(dplyr)
        library(data.table)
        library(reshape2)
        
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the files
        directory <- "/Users/ben/Coursework/Data/UCI HAR Dataset"
        ## Get all files in directory
        ##files <- list.files(directory, pattern="*.txt", recursive=TRUE)

        ## Load relevant files
        subject_test <- data.table(read.table(file.path(directory, "test", "subject_test.txt")))
        subject_train <- data.table(read.table(file.path(directory, "train", "subject_train.txt")))
        y_test  <- data.table(read.table(file.path(directory, "test" , "Y_test.txt" )))
        y_train <- data.table(read.table(file.path(directory, "train", "Y_train.txt")))
        x_test <- data.table(read.table(file.path(directory, "test" , "X_test.txt" )))
        x_train <- data.table(read.table(file.path(directory, "train" , "X_train.txt" )))
        activity_labels <- data.table(read.table(file.path(directory, "activity_labels.txt")))
        features <- data.table(read.table(file.path(directory, "features.txt")))

        ## Combine subject datasets & set column name
        subjects <- rbind(subject_train, subject_test)
        setnames(subjects, names(subjects), "SubjectId")
        
        ## Combine activity datasets & set column name
        activities <- rbind(y_train, y_test)
        setnames(activities, names(activities), "ActivityId")
        ## Set Activity Labels
        setnames(activity_labels, names(activity_labels), c("ActivityId", "ActivityLabel"))
        ## Add Activity Labels to Activities
        activities <- merge(activities, activity_labels, by="ActivityId")
        
        ## Combine SubjectId & ActivityId
        identifiers <- cbind(subjects, activities)
        
        ## Combine feature datasets
        data <- rbind(x_train, x_test)
        ## Add Feature labels to features dataset
        setnames(features, names(features), c("FeatureId", "FeatureLabel"))
        ## Add Feature Labels as Column names
        setnames(data, names(data), as.vector(features[,FeatureLabel]))
        
        ## Get features with mean or std in their label text
        featuresrequired <- features[grepl("mean\\(\\)|std\\(\\)", FeatureLabel)]$FeatureId
        
        ## subset to just data columns required
        data <- data[, featuresrequired, with=FALSE]
        
        ## Combine SubjectId & ActivityId with main dataset
        data <- cbind(identifiers, data)
        
        ## Set Identifiers as key
        setkey(data, SubjectId, ActivityId, ActivityLabel)
        
        ## Reshape data table 
        #data <- data.table(melt(data, key(data), variable.name="FeatureLabel"))
        
        ## Set data keys to group by
        groupby <- data %>% group_by(SubjectId,ActivityId,ActivityLabel)
        
        ## Summarise the mean of each column 
        output <- groupby %>% summarise_each(funs(mean))
        
        ## Save output as text file
        write.table(output, file="tidy.txt",row.name=FALSE)
}