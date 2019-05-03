# Optimize Recommendation

User-based filtering vs Item-based filtering
![github](https://github.com/ab4499/Optimize_Recommendation/blob/master/Table%26Graph/item_based_filltering.jpeg "github")

## Goal
This project build recommendation system to recommend the sequence of learning class units and partners that students could cooperate with in the class. The recommendation is based on a survey that asks about students perception on difficulty and interest of each unit. The purpose is to build a system that enables personalized study and make study recommendation specific to each student based on their interest and perception of difficulty of the units. The recommender is based on a type of collaborative filter. 
1) The recommendation system will suggest which unit a student should learn next. 
2) The recommendation system will suggest two classmates that a student can partner with in the class.

## Process
We have two datasets:
Difficulty - Students' perception (rating) on the difficulty of each unit
Interest - Students' perception (rating) of their interest of each unit

#### Load data
    DD<-read.csv("difficulty.csv", header=TRUE)
    DI<-read.csv("interest.csv", header=TRUE)
#### Convert dataframe to matrices
    DI1<-DI[!duplicated(DI$name), ]
    rownames(DI1)<-DI1$name
    DI1<-select(DI1, -1)
    DI1<-as.matrix(DI1)
   
    DD1<-DD[!duplicated(DD$name), ]
    rownames(DD1)<-DD1$name
    DD1<-select(DD1, -1)
    DD1<-as.matrix(DD1)
   
#### Generate a user-based similarity matrix based on cosine similarity using the ratings！

    DI1 <- t(DI1)
    DI1[is.na(DI1)]=0
    DD1<-t(DD1)
    DD1[is.na(DD1)]=0

    I.SIM <- cosine(DI1) 
    diag(I.SIM)<-NA
    D.SIM<-cosine(DD1)
    diag(D.SIM)<-NA
    
![github](https://github.com/ab4499/Optimize_Recommendation/blob/master/Table%26Graph/I.SIM.png "github")

  
#### Now, we can make a quick query to find out which classmates are most similar to a certain student 
    stu.name <- ""
    # orders the column of the matrix corresponding to the student according to similarity and returns the top two student ids for the students who's interests are most similar to yours
    head(rownames(I.SIM[order(I.SIM[stu.name,], decreasing = TRUE),]), n = 2)
    # orders the column of the matrix corresponding to the student according to similarity and returns the top two student ids for the students who's difficulty are most similar to yours
    head(rownames(D.SIM[order(D.SIM[stu.name,], decreasing=TRUE),]), n=2)

This is a basic user-based collaborative filter. 
Then we create a unit-based, rather than student-based similarity matrix for difficulty. Then use the similarity matrix to provide a suggested next unit to a student who is looking for the unit that is most similar in terms of difficulty to the "prediction" unit.

Finally, educational settings have important differences to purely commercial settings such as film or product suggestions. In education we want people not to just follow their interests as they may simply choose things that are easy for them so they learn very little. To reduce this possibility with the collaborative filter we create a composite measure from interest and difficulty, then construct a similarity matrix using this measure. With this similarity matrix we can generate a suggestion for a student who has just completed the "prediction" unit.

    DD2<-t(DD1)
    D.SIM2<-cosine(DD2)
    diag(D.SIM2)<-NA
    head(rownames(D.SIM2[order(D.SIM2["pred.dif",], decreasing=TRUE),]),2)
    
    DI3<-gather(DI, unit, interest, -name)
    DD3<-gather(DD, unit, difficulty, -name)

    D1<-data.frame(DI3$name, DI3$unit, DI3$interest, DD3$difficulty)
    colnames(D1)<-c("name", "unit", "interest", "difficulty")
    D1<-filter(D1, !is.na(interest), !is.na(difficulty))
    #Remove int from unit label
    D1$unit<-gsub(".int", "", D1$unit)
    D2<-select(D1, interest, difficulty)

    # RUN PCA
    pc<-prcomp(D2)
    #Extract PC1 loadings as new measure and attach to student name & unit
    D3<-data.frame(D1$name, D1$unit, pc$x[, 1])
    colnames(D3)<-c("name", "unit", "PC1")

    #Recreate unit by student matrix
    D4<-spread(D3, unit, PC1)
    rownames(D4)<-D4$name
    D4$name<-NULL
    D4<-as.matrix(D4)
    D4<-ifelse(is.na(D4), 0, D4)

    #Generate cosine similarity matrix for units
    C.SIM<-cosine(D4)
    diag(C.SIM)<-NA
    #Search for most similar unit to "prediction" unit
    head(rownames(C.SIM[order(C.SIM["pred", ], decreasing=TRUE), ]), n=1)
    
   Now, students can choose the order of learning different units based on this recommender system.
