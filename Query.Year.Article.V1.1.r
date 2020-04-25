#############################################################
#                          SETTINGS                         #
#############################################################


########    Location to save excel with findings                         #################
path<-"C:/Users/Chronis-SA/Desktop/Derma/lit.rev"
########    Name of the query (unique)                                   #################
Name.of.search<- "Test run 2"
########    Start Year - Change only for later                           ##################
Start.Year<-1995
########    QUERY                                                        ##############################################
queries<- c(
            "((CONVOLUTIONAL NEURAL NETWORKS) OR (NEURAL NETWORKS) OR (ARTIFICIAL INTELLIGENCE) OR (DEEP LEARNING))
            AND ((SKIN CANCER) OR  (MELANOMA) OR (SQUAMOUS CELL CARCINOMA) OR  (BASAL CELL CARCINOMA) OR  (SKIN MALIGNANT) OR  (SKIN PREMALIGNANT) OR  (SKIN PRE-MALIGNANT)) 
            AND ((AUROC) OR (AUC) OR (ROC)  OR (SENSITIVITY) OR (SPECIFICITY) OR (NPV) OR (PPV) OR (AREA UNDER THE CURVE))
            AND ((DERMOSCOPY) OR (DERMATOSCOPY) OR (DIAGNOSIS) OR (DIAGNOSTIC) OR (DIAGNOSED))
            AND ((TRIAL) OR (CLINICIAN) OR (CLINICIANS) OR (PRIMARY CARE) OR (DERMATOLOGIST) OR (DERMATOLOGISTS) OR (DOCTOR) OR (DOCTORS))
            AND ((%) OR (PERCENTAGE))"
)



#############################################################
Literature(Name.of.search, Start.Year, path, queries)
#############################################################
#############################################################            
Literature <- function(Name.of.search, Start.Year, path, queries) {
  
  library(easyPubMed)
  library(devtools)
  library(ggplot2)
  library(stringr)
  library(tidyverse)
  library(openxlsx)




summary.df.y.q<-data.frame(pmid="1",	doi="1",	title="A",abstract="A",	year="1821",	month="c", day="44", jabbrv="x",	journal="chr",
                           keywords="i", lastname="kem", firstname="c",	address="x",	email="d",Query="looooooooooooooooooooooooooooong")
for (z in 1: length(queries)){
  myquery <- queries[z] 
  title.for.save<- str_replace_all(myquery[1], "[(]", "")
  title.for.save<-str_replace_all(title.for.save[1], "[)]", "")
  title.for.save<-str_replace_all(title.for.save[1], "\n", "")
  
    summary.df.y<-data.frame(pmid="1",	doi="1",	title="A",abstract="A",	year="1821",	month="c", day="44", jabbrv="x",	journal="chr",
    keywords="i", lastname="kem", firstname="c",	address="x",	email="d")
  for (y in 0:25){ 
    my.query <- paste(myquery, "AND (\"",Start.Year+y,"\"[EDAT] : \"",Start.Year+y+1,"\"[EDAT])")
    my.idlist <- get_pubmed_ids(my.query)
    my.idlist$Count
    
    if(my.idlist$Count>1){
      
    batch.xml <- fetch_pubmed_data(my.idlist, retstart = 1, retmax = 5000)
    record.list <- easyPubMed::articles_to_list(batch.xml)
    
        summary.df<-data.frame(pmid="1",	doi="1",	title="A",abstract="A",	year="1821",	month="c", day="44", jabbrv="x",	journal="chr",
        keywords="i", lastname="kem", firstname="c",	address="x",	email="d")
        count<-as.numeric(my.idlist$Count)
        for (i in 1:(count-1)) {
        temp<-article_to_df(record.list[[i]], autofill = TRUE, max_chars = 20000, getKeywords = TRUE, getAuthors = TRUE)
        summary.df<-rbind(summary.df,temp)
        print(temp$title[1])      
                }  # all findings in a df ends
        summary.df<-summary.df[2:nrow(summary.df),]
        
      summary.df.y<- rbind(summary.df.y,summary.df)
      print(Start.Year+y)
    }else{
        print(paste0("No findings for year ",Start.Year+y))}
        
                }                         #### Year loop ends
    summary.df.y<-summary.df.y[2:nrow(summary.df.y),]
    summary.df.y$Query<-myquery
    summary.df.y.q<-summary.df.y
    #write.csv(summary.df.y.q, paste0("path/Counts of publications for q y r ",z,"_",y, "_",i, ".csv"))
    
    ##############
    ##############
    ##############
    ##############
    
    if(nrow(summary.df.y)>2) {
    
    RESULTS<-tapply(summary.df.y.q$title, summary.df.y.q$year, unique)
    RESULTS
    Counts<-data.frame(length(RESULTS$`1995`),length(RESULTS$`1996`),length(RESULTS$`1997`),length(RESULTS$`1998`),length(RESULTS$`1999`),length(RESULTS$`2000`),length(RESULTS$`2001`),length(RESULTS$`2002`),
                       length(RESULTS$`2003`),length(RESULTS$`2004`),length(RESULTS$`2005`),length(RESULTS$`2006`),
                       length(RESULTS$`2007`),length(RESULTS$`2008`),length(RESULTS$`2009`),length(RESULTS$`2010`),
                       length(RESULTS$`2011`),length(RESULTS$`2012`),length(RESULTS$`2013`),length(RESULTS$`2014`),
                       length(RESULTS$`2015`),length(RESULTS$`2016`),length(RESULTS$`2017`),length(RESULTS$`2018`),
                       length(RESULTS$`2019`),length(RESULTS$`2020`))
    
    Counts<-t(Counts)
    Years<-data.frame(1995:2020)
    Counts.Summary<-cbind(Years, Counts)
    Counts.Summary$Query.Used<-myquery
    colnames(Counts.Summary)<- c("Year", "Counts", "Query Used in PubMed search")
    
    ## Create a blank workbook
    wb <- createWorkbook()
    addWorksheet(wb, "Articles found")
    addWorksheet(wb, "Counts per year")
    
      writeData(wb, "Articles found", summary.df.y.q)
      writeData(wb, "Counts per year", Counts.Summary)
      
    
    
    ## Save workbook to working directory
    saveWorkbook(wb, file = paste0(path,"/Literature review ",Name.of.search, "_",z,"_",y,"_",i,".xlsx"), overwrite = TRUE)}else{print("NO FINDINGS")}
    
                          }  ## multiple queries ends  
 

} 
#############################################################


