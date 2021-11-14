library(readtext)
library(stringr)
library(dplyr)
library(tidyr)

#Clear data
if(exists("File_Exported")){rm(File_Exported)}
if(exists("KW_Vector")){rm(KW_Vector)}

# Import Zotero File
ZoteroBib <- readtext("/Users/stefanopagliari/Library/Mobile Documents/iCloud~md~obsidian/Documents/Obsidian ICloud/80. Resources/StefanoPagliariZotero.bib")

ZoteroBib <- data.frame(Reference = unlist(str_split(ZoteroBib$text, "\n\n@")))

#Clean the text
ZoteroBib$Reference <- gsub("undefined/ed", "undefined", ZoteroBib$Reference, fixed = T)

#Find the bibkey

ZoteroBib$CiteKey <- str_sub(ZoteroBib$Reference, 
        str_locate(pattern = "\\{", ZoteroBib$Reference)[,1]+1, 
        str_locate(pattern = ",", ZoteroBib$Reference)[,1]-1)

# Find the date modified
ZoteroBib$Date_Modified <- str_sub(ZoteroBib$Reference, 
                             str_locate(pattern = "datemodified = ", ZoteroBib$Reference)[,1] + 16, 
                             str_locate(pattern = "datemodified = ", ZoteroBib$Reference)[,1] + 35)
ZoteroBib$Date_Modified = strptime(ZoteroBib$Date_Modified,format='%Y-%m-%dT%H:%M:%SZ')
ZoteroBib$Date_Modified = ZoteroBib$Date_Modified +1*60*60

#Import the last time the script was run
Last_Time <- rio::import(file = "/Users/stefanopagliari/Library/Mobile Documents/iCloud~md~obsidian/Documents/Obsidian ICloud/80. Resources/R Script/script_time_run.txt")  

Last_Time <- as.POSIXlt(paste(as.character(Last_Time$V1), as.character(Last_Time$V2), sep = " "))
 
# Identify which Zotero references have been edited since the last time the script was run
Vector_Sources <- which(ZoteroBib$Date_Modified >  Last_Time)




#find the file location
ZoteroBib$Folder <- str_sub(ZoteroBib$Reference, 
        str_locate(ZoteroBib$Reference, "Zotero/storage/")[,2]+1,
        str_locate(ZoteroBib$Reference, "Zotero/storage/")[,2]+8)



#### IMPORT INDIVIDUAL NOTE #####
#w <- Vector_Sources[1]
#w <- 1695
#SelectedKey = "OsnabruggePlayingGalleryEmotive2021"

for (w in Vector_Sources){
  
SelectedKey =  ZoteroBib$CiteKey[w]

Selected_Note_N <- which(ZoteroBib$CiteKey== SelectedKey)




#### Create ZNotes
Selected_Note <- data.frame(Text = unlist(str_split(ZoteroBib$Reference[Selected_Note_N], "\\\\par", )))
ifelse(nrow(Selected_Note)==1,
       ANNOTATION_FOUND <-  F,
       ANNOTATION_FOUND <-  T
)
Temp <- data.frame(Text = unlist(str_split(Selected_Note$Text[1], "\\\n")))
if(ANNOTATION_FOUND == T) {
Temp2 <- data.frame(Text = Selected_Note[2:nrow(Selected_Note),])
Selected_Note <- bind_rows(Temp, Temp2)
Temp <- Selected_Note[nrow(Selected_Note),]
Temp <- data.frame(Text = unlist(str_split(Temp, "\\},\\\n")))
Temp2 <- data.frame(Text = Selected_Note[1:nrow(Selected_Note)-1,])
Selected_Note <- bind_rows(Temp2, Temp)}
if(ANNOTATION_FOUND == F) {
  
  Selected_Note <- Temp }



#Selected_Note <- data.frame(Text = unlist(str_split(ZoteroBib$Reference[Selected_Note_N], "\\\n", )))
#Selected_Note <- Selected_Note %>%
#  filter(Text != "\\par")



# Split the value from the data
Selected_Note <- data.frame(str_split_fixed(Selected_Note$Text, " = ", 2))
colnames(Selected_Note) <- c("Variable", "Value")

#Clean data
Selected_Note$Variable <- trimws(Selected_Note$Variable)
Selected_Note$Value <- trimws(Selected_Note$Value)
Selected_Note <- Selected_Note %>%
  filter(Variable!="}")
Selected_Note <- Selected_Note %>%
  filter(Variable!="")



# Find the citekey and type
Temp <- data.frame("Variable" = "type", "Value" = str_sub(Selected_Note[1,1], 1, str_locate(Selected_Note[1,1], "\\{")[1]-1))
Selected_Note[1,] <- data.frame("Variable" = "citekey", "Value" = ZoteroBib$CiteKey[Selected_Note_N])

Selected_Note <- rbind(Selected_Note[1,],
               Temp,
               Selected_Note[2:nrow(Selected_Note),])

#Find if a source has Zotero annotations
IsZoteroAnnotation <- NA
ifelse((sum(str_detect(Selected_Note$Value, "\\{\\\\section\\{Annotations"))>0),
       IsZoteroAnnotation <- TRUE,
       IsZoteroAnnotation <- FALSE)

# Identify the note
Temp <- which(Selected_Note$Value=="")
Selected_Note$Value[Temp] = Selected_Note$Variable[Temp]
Selected_Note$Variable[Temp] = "Annotation"
Selected_Note$Value[Temp] <- gsub("\n", "", Selected_Note$Value[Temp])

Metadata <- Selected_Note %>% filter(Variable != "Annotation")
Selected_Note <- Selected_Note %>% filter(Variable == "Annotation") %>%
  rename("Text" = "Value")

#Clean Metadata
Metadata[] <- lapply(Metadata, gsub, pattern="\\},", replacement='')
Metadata[] <- lapply(Metadata, gsub, pattern="\\{", replacement='')
Metadata[] <- lapply(Metadata, gsub, pattern="\\}", replacement='')
Metadata[] <- lapply(Metadata, gsub, pattern="\\\\textemdash", replacement=' -')
#Temp <- Metadata
#Temp <- lapply(Temp, gsub, pattern="\\c ", replacement='')

#Pivot Metadata Wider
Metadata <- pivot_wider(Metadata, names_from = "Variable", values_from = "Value")

#Clean Metadata
Metadata$author <- gsub("\\", "", Metadata$author, fixed = T)
Metadata$author <- gsub("\"u", "ü", Metadata$author, fixed = T)

#### PROCESS HIGHLIGHTS #####

if (IsZoteroAnnotation==TRUE){
#Identify the quotes
  
Selected_Note[] <- lapply(Selected_Note, gsub, pattern="\\},", replacement='')
Selected_Note$Type <- NA
Selected_Note$Type[str_sub(Selected_Note$Text, 1, 2) == "``"] <- "Highlight" 
Selected_Note$End_Highlight <- unlist(gregexpr(paste0("'' \\(", gsub(",","",word(Metadata$author))), Selected_Note$Text))
Selected_Note$End_Highlight <- as.integer(Selected_Note$End_Highlight)
#Selected_Note$End_Highlight <- unlist(gregexpr("\\(", Selected_Note$Text))
Selected_Note$End_Highlight[Selected_Note$End_Highlight=="-1"] <- -2

#Find the end of the citation
r <- 3
for (r in 1:nrow(Selected_Note)){
  Temp  <- unlist(gregexpr(", p[.]" , Selected_Note$Text[r]))  
  Temp <- Temp[length(Temp)]
  Selected_Note$p_marker[r] <- Temp
  
  Temp <- unlist(gregexpr("\\)" , Selected_Note$Text[r]))
  Temp <- Temp[which(Temp > Selected_Note$p_marker[r])]
  Temp <- Temp[1]
  Selected_Note$End_Cit[r] <- Temp
}

Selected_Note$End_Cit[Selected_Note$End_Cit=="-1"] <- NA
Selected_Note$Key <- str_sub(Selected_Note$Text, Selected_Note$End_Highlight+3, Selected_Note$End_Cit)
Selected_Note$Comment <- trimws(str_sub(Selected_Note$Text, Selected_Note$End_Cit+2, nchar(Selected_Note$Text)))
Selected_Note$Character <- word(Selected_Note$Comment, 1)
Selected_Note <- Selected_Note %>%
  mutate(Type = case_when(
    word(Selected_Note$Comment, 1) == "\\%" ~ "Summary",
    word(Selected_Note$Comment, 1) == "!" ~ "Important",
    word(Selected_Note$Comment, 1) == "=" ~ "Keyword",
    word(Selected_Note$Comment, 1) == "+" ~ "Continuation",
    word(Selected_Note$Comment, 1) == "?" ~ "Todo",
    word(Selected_Note$Comment, 1) == "\\#" ~ "Header 1",
    word(Selected_Note$Comment, 1) == "\\#\\#" ~ "Header 2",
    word(Selected_Note$Comment, 1) == "\\#\\#\\#" ~ "Header 3",
    word(Selected_Note$Comment, 1) == "\\#\\#\\#\\#" ~ "Header 4",
    word(Selected_Note$Comment, 1) == "\\#\\#\\#\\#\\#" ~ "Header 5",
    word(Selected_Note$Comment, 1) == "\\#\\#\\#\\#\\#\\#" ~ "Header 6",
    word(Selected_Note$Comment, 1) == "" ~ "Highlight",
    TRUE ~ "Comment")
  )

#Filter extraneous text
Selected_Note <- Selected_Note %>%
  filter(p_marker != "-1")


# Move Summary Comments before the highlightd ext
Selected_Note <- Selected_Note %>%
  mutate(Text = ifelse(
    Type == "Summary", paste0("- **",Comment, "**: ", str_sub(Text, 1, End_Cit)),
    Text))

Selected_Note <- Selected_Note %>%
  mutate(Text = ifelse(
    Type == "Summary", gsub("- **\\%", "- **", Selected_Note$Text, fixed = T),
    Text))


# Identify keywords
KW_Vector <- c(str_sub(Selected_Note$Text, 1, Selected_Note$End_Highlight))[which(Selected_Note$Type=="Keyword")]
KW_Vector <- gsub("`", "", KW_Vector)
KW_Vector <- gsub("'", "", KW_Vector)
KW_Vector <- paste0("[[", KW_Vector, "]]")

Selected_Note <- Selected_Note %>%
  filter(Type != "Keyword") 

# Add Important tag
Selected_Note <- Selected_Note %>%
  mutate(Text = ifelse(
    Type == "Important", paste0("- ** #❗️ ", 
                                gsub("!","", Comment), 
                                " ::** ", 
                                str_sub(Text, 1, End_Cit)),
    Text))

# Add header tags

Selected_Note <- Selected_Note %>%
  mutate(Text = ifelse(
    Type == "Header 1", paste0("### ", str_sub(Text, 1, End_Cit)),
    Text))



Selected_Note <- Selected_Note %>%
  mutate(Text = ifelse(
    Type == "Header 2", paste0("#### ", str_sub(Text, 1, End_Cit)),
    Text))

Selected_Note <- Selected_Note %>%
  mutate(Text = ifelse(
    Type == "Header 3", paste0("### ", str_sub(Text, 1, End_Cit)),
    Text))

Selected_Note <- Selected_Note %>%
  mutate(Text = ifelse(
    Type == "Header 4", paste0("#### ", str_sub(Text, 1, End_Cit)),
    Text))

Selected_Note <- Selected_Note %>%
  mutate(Text = ifelse(
    Type == "Header 5", paste0("##### ", str_sub(Text, 1, End_Cit)),
    Text))

Selected_Note <- Selected_Note %>%
  mutate(Text = ifelse(
    Type == "Header 6", paste0("###### ", str_sub(Text, 1, End_Cit)),
    Text))

# Remove citation details from header
Selected_Note <- Selected_Note %>%
  mutate(Text = ifelse(
    str_detect(Type, "Header"), 
    str_remove(Text, Key), 
    Text))

# Remove citation details from header
Selected_Note <- Selected_Note %>%
  mutate(Text = ifelse(
    str_detect(Type, "Header"), 
    str_remove(Text, "\\(\\)"), 
    Text))

# Remove quotation marks from header
Selected_Note <- Selected_Note %>%
  mutate(Text = ifelse(
    str_detect(Type, "Header"), 
    gsub("`", "", Text), 
    Text))

Selected_Note <- Selected_Note %>%
  mutate(Text = ifelse(
    str_detect(Type, "Header"), 
    gsub("'", "", Text), 
    Text))

# Add bullet point in front of highlight
# Tag the tasks  
Selected_Note <- Selected_Note %>%
  mutate(Text = ifelse(
    Type == "Highlight", paste0("* ", 
                           Text),
    Text))

# Correct sentences that need to be attached to the previous one
Continuation_Vec <- str_which(Selected_Note$Type, "Continuation")

Selected_Note$Text[Continuation_Vec-1] <-   str_remove(Selected_Note$Text[Continuation_Vec-1], Selected_Note$Key[Continuation_Vec-1])
Selected_Note$Text[Continuation_Vec-1] <-   str_remove(Selected_Note$Text[Continuation_Vec-1], "\\(\\)")

for (c in 1:length(Continuation_Vec)){
Selected_Note$Text[Continuation_Vec[c]-1] <- paste0(Selected_Note$Text[Continuation_Vec[c]-1], Selected_Note$Text[Continuation_Vec[c]], collapse = "")
}
Selected_Note$Text[Continuation_Vec-1] <-   str_replace(Selected_Note$Text[Continuation_Vec-1], "'' ``", " ")
if(length(Selected_Note$Comment[Continuation_Vec]>0)){Selected_Note$Text[Continuation_Vec-1] <-   gsub(Selected_Note$Comment[Continuation_Vec], "", Selected_Note$Text[Continuation_Vec-1] , fixed = T)}
Selected_Note <- Selected_Note %>% filter(Type != "Continuation")

# Tag the tasks  
Selected_Note <- Selected_Note %>%
  mutate(Text = ifelse(
    Type == "Todo", paste0("- [ ] ** #todo ", 
                                gsub("?","", Comment, fixed = T), 
                                " :** ", 
                                str_sub(Text, 1, End_Cit)),
    Text))


###### CREATE LINK TO CORRECT PAGE OF THE PDF
#Find pages
Start_Page <- str_match(ZoteroBib$Reference[Selected_Note_N], "pages\\s+\\S+\\s+(\\S+)")[,2]
Start_Page <- word(str_extract_all(Start_Page, "[0-9]+"),1)
Start_Page <- as.numeric(gsub("\\D+", "", Start_Page))
if(is.na(Start_Page)==F){

Selected_Note$Key_Page <- word(str_extract_all(Selected_Note$Key, "[0-9]+"),2)
Selected_Note$Key_Page <- as.numeric(gsub("\\D+", "", Selected_Note$Key_Page))

 

Selected_Note$Quote_Page <- Selected_Note$Key_Page - Start_Page  + 1



#Find the link in the pdf
Selected_Note$Key2 <- 
  str_replace(Selected_Note$Key, "\\(", "\\[")
Selected_Note$Key2 <- 
  str_replace(Selected_Note$Key2, "\\)", "\\]") 



Selected_Note$Key3 <- paste0(Selected_Note$Key2, "(",
       "zotero://open-pdf/library/items/",
       ZoteroBib$Folder[Selected_Note_N],
       "?page=",
       Selected_Note$Quote_Page,
       ")")

 
#Add link for comments
Num_Comment <- which(Selected_Note$Type=="Comment")
for (i in Num_Comment){
Selected_Note$Key3[i] <-  gsub(Selected_Note$Key2[i], "[ ]", Selected_Note$Key3[i], fixed = T)
}

#Create link to the pdf
Selected_Note <- Selected_Note %>%
  mutate(Text = case_when(
    is.na(Key)==F ~ str_replace(Selected_Note$Text,
                                         Selected_Note$Key,
                                         Selected_Note$Key3),
  TRUE ~ Text))

}

#### ADD BLOCKQUOTE TO COMMENT
ifelse("Key3" %in% colnames(Metadata),
       Selected_Note <- Selected_Note %>%
        mutate(Text = ifelse(
          Type == "Comment", paste0("> **", 
                           Comment, 
                           "**", 
                           Key3),
          Text)),
       Selected_Note <- Selected_Note %>%
         mutate(Text = ifelse(
           Type == "Comment", paste0("> **", 
                                     Comment, 
                                     "**"),
           Text))
)


#Clean the quotation marks

#Selected_Note$Text <- gsub("* ``", "* \"", Selected_Note$Text, fixed = T)
Selected_Note$Text <- gsub("``", "\"", Selected_Note$Text, fixed = T)
Selected_Note$Text <- gsub("`", "'", Selected_Note$Text, fixed = T)
Selected_Note$Text <- gsub("textemdash", "--", Selected_Note$Text, fixed = T)


#Export 
cat(Selected_Note$Text, file = "/Users/stefanopagliari/Library/Mobile Documents/iCloud~md~obsidian/Documents/Obsidian ICloud/Test2.md", sep = "\n\n")
}

  
#####YAML ####
 

   
  
  ZT_Selected <- data.frame(Sentences = "---")
  
  #Create "Parent" for breadcrumbs
  
  #Add Collections to the tag block
  Breadcrumbs <- paste0("Parent: \n",
                  "  - [[🗄️]]")
  if("collections" %in% colnames(Metadata)){
    Metadata$collections <- gsub("textasciitilde\\{\\}","~",Metadata$collections)
    Metadata$collections <- gsub("\\\\","",Metadata$collections)
    Metadata$collections <- gsub("textasciitilde ","~ ",Metadata$collections)
    Collection <- paste0("  - [[",trimws(unlist(str_split(Metadata$collections, ","))), "]]", sep = "\n", collapse = "")
    Breadcrumbs <- paste("Parent: \n", Collection, sep = "", collapse = "\n")  
  }
  
  ZT_Selected <- rbind(ZT_Selected,
                       data.frame(Sentences = Breadcrumbs))
   
  ### CLOSE YAML
  ZT_Selected <- rbind(ZT_Selected,
                       data.frame(Sentences = "---"))
  
#### STATUS AND TYPE ####

                       
                       
#Check status from zotero
  
Status <- trimws(unlist(str_split(Metadata$keywords, ",")))
Status <- str_remove(Status, "\\\\")
#List of accepted vector tags
Status_Vector <- c("#todo/read", 
                   "#todo/toread", 
                   "#todo/skim", 
                   "#todo/process", 
                   "#todo/toprocess", 
                   "complete", 
                   "completed")

Status <- Status[which(Status %in% Status_Vector)] 

Status <- gsub("#todo/to", "#todo/", Status)

ZT_Selected <- rbind(ZT_Selected, 
                     data.frame(Sentences = paste0("**Type**:: #reference/zotero \n",
                                                   "**Status**:: ",
                                                   Status)))

  
  
  
  #Add TITLE and AUTHORS
  ZT_Selected <- rbind(ZT_Selected, 
                       data.frame(Sentences = paste("#", 
                                                    Metadata$title,
                                                    "\n",
                                                    "* ",
                                                    paste("[[",(unlist(str_split(Metadata$author, " and "))),"]]",  collapse = ", ")
  )))
  
  
  
  #ADD BLOCK WITH METADATA
  Metadata_Block <- paste("```ad-info \ntitle: Metadata\ncollapse: closed\n",
                          "* **Citekey**: ", Metadata$citekey, "\n",
                          "* **Type**: ", tolower(Metadata$type), "\n")
  Metadata_Block <- paste(Metadata_Block, "* **Title**:: ", Metadata$title, "\n")
  Metadata_Block <- paste(Metadata_Block, "* **Author**:: ",
                    paste0("[[",(unlist(str_split(Metadata$author, " and "))),"]]", sep="; ", collapse = ""), 
                  "\n")
  
  if("year" %in% colnames(Metadata)){if(is.na(Metadata$year)==F){Metadata_Block <- paste(Metadata_Block, "* **Date**: ", Metadata$year, "\n")}}
  if("booktitle" %in% colnames(Metadata)) {if(is.na(Metadata$booktitle)==F){Metadata_Block <- paste(Metadata_Block, "* **Book**: ", Metadata$booktitle, "\n")}}
  if("editor" %in% colnames(Metadata)){if (is.na(Metadata$editor)==F){Metadata_Block <- paste(Metadata_Block, "* **Editor**: ", paste("[[",(unlist(str_split(Metadata$editor, " and "))),"]]",  collapse = ", "), "\n")}}                  
  if("publisher" %in% colnames(Metadata)){ if(is.na(Metadata$publisher)==F){Metadata_Block <- paste(Metadata_Block, "* **Publisher**: ", Metadata$publisher, "\n")}}
  if("institution" %in% colnames(Metadata)){ if(is.na(Metadata$institution)==F){Metadata_Block <- paste(Metadata_Block, "* **Institution**: ", Metadata$institution, "\n")}}
  if("journal" %in% colnames(Metadata)){ if(is.na(Metadata$journal)==F){Metadata_Block <- paste(Metadata_Block, "* **Journal**: ", Metadata$journal, "\n")}}
  if("volume" %in% colnames(Metadata)){ if(is.na(Metadata$volume)==F){Metadata_Block <- paste(Metadata_Block, "* **Volume**: ", Metadata$volume, "\n")}}                  
  if("number" %in% colnames(Metadata)){ if(is.na(Metadata$number)==F){Metadata_Block <- paste(Metadata_Block, "* **Issue**: ", Metadata$number, "\n")}}                 
  if("pages" %in% colnames(Metadata)){ if(is.na(Metadata$pages)==F){Metadata_Block <- paste(Metadata_Block, "* **Pages**: ", Metadata$pages, "\n")}}                  
  if("doi" %in% colnames(Metadata)){ if(is.na(Metadata$doi)==F){Metadata_Block <- paste(Metadata_Block, "* **DOI**: ", Metadata$doi, "\n")}}                  
  Metadata_Block <- paste(Metadata_Block,"```") 
  ZT_Selected <- rbind(ZT_Selected,
                       data.frame(Sentences = Metadata_Block))
  
  
  
  
#  ### ADD DATE IN THE YAML
#  if(exists("Metadata$year")){
#    DATE <- paste("Date: ",
#                  "\"",
#                  "\"",
##                  Metadata$year,
#                  sep="", collapse = "") 
#    
#    ZT_Selected <- rbind(ZT_Selected,
#                         data.frame(Sentences = DATE))
    
#    #### ADD THE YEAR IN THE METADATA
#    Years_Vec <- as.character(c(1980:2022))
#    Year_Num <- str_extract(Metadata$year, Years_Vec)
#    Year_Num <- Year_Num[is.na(Year_Num)==F]
#    YEAR <- paste("Year: ",
#                  "\"",
#                  Year_Num,
#                  "\"",
#                  sep="", collapse = "") 
#    ZT_Selected <- rbind(ZT_Selected,
#                         data.frame(Sentences = YEAR))
#  }
  
  
  
  #ADD BLOCK WITH ABSTRACT
  if("abstract" %in% colnames(Metadata)){
  if(is.na(Metadata$abstract)==F){
    Abstract_Block <- paste("```ad-abstract \ntitle: Abstract\ncollapse: closed",
                            "\n",
                            Metadata$abstract, 
                            "\n",
                            "```")
  }
  ZT_Selected <- rbind(ZT_Selected,
                       data.frame(Sentences = Abstract_Block))
  }
  
  #ADD BLOCK WITH LINKS
  Link_Block <- paste("```ad-note \ntitle: Document \ncollapse: closed", "\n")
  if("url" %in% colnames(Metadata)){Link_Block <- paste(Link_Block, "* [Website](", Metadata$url, ")","\n")}
  if("localLibrary" %in% colnames(Metadata)){Link_Block <- paste(Link_Block, "* [Zotero](", Metadata$localLibrary,")", "\n")}
  if("file" %in% colnames(Metadata)){Link_Block <- paste(Link_Block, "* [Local File](", paste0("file://", Metadata$file),")", "\n")}
  Link_Block <- paste(Link_Block,"```") 
  
  ZT_Selected <- rbind(ZT_Selected,
                       data.frame(Sentences = Link_Block))
  
  ### ADD BLOCKS WITH TAGS
  #Create tag bloc
  Tag_Block <- c("```ad-quote\ntitle: Tags and Links\ncollapse: closed")
  
  #Add Collections to the tag block
  if("collections" %in% colnames(Metadata)){
    Metadata$collections <- gsub("textasciitilde\\{\\}","~",Metadata$collections)
    Metadata$collections <- gsub("\\\\","",Metadata$collections)
    Metadata$collections <- gsub("textasciitilde ","~ ",Metadata$collections)
    Collection <- paste0("[[",trimws(unlist(str_split(Metadata$collections, ","))), "]]")
    Tag_Block <- paste0(Tag_Block,
                        "\n",
                        paste0("* **Collection**:: ", paste0(Collection, sep = "; ", collapse = "")))
    }
  
  #Identify the topics
  Topic <- trimws(unlist(str_split(Metadata$keywords, ",")))
  Topic <- str_remove(Topic, "\\\\")
  Topic <- Topic[Topic != "\u26d4 No DOI found"]
                 
  #Identify the tags
  Tags <- Topic[which(substr(Topic, 1, 1)== "#")] 
  
  #Add links to Topics
  Topic <- Topic[which(substr(Topic, 1, 1)!= "#")] 
  if(length(Topic)>0){
  Topic <- paste0("[[",Topic, "]]")
  Topic <- sort(Topic)}
  if(exists("KW_Vector")){
  if(length(Topic)>0 & length(KW_Vector)>0) {Topic <- sort(c(Topic, KW_Vector))}
  if(length(Topic)==0 & length(KW_Vector)>0) {Topic <- sort(KW_Vector)}
  }
  
  

  if(length(Topic)>0){Tag_Block <- paste0(Tag_Block,
                                          "\n",
                                          paste0("* Keywords: ", paste0(Topic, collapse = "; ")))}
  
                                          
  if(Tag_Block != "```ad-quote\ntitle: Tags and Links\ncollapse: closed"){  
    Tag_Block <- paste0(Tag_Block,"\n", "```" )
    ZT_Selected <- rbind(ZT_Selected,
                         data.frame(Sentences = Tag_Block))
  }
   

  
  
  
  
  #Merge Metadata and Notes
  if(IsZoteroAnnotation==T ){
    Selected_Note <- select(Selected_Note, "Text")
    Selected_Note <- rbind(data.frame(Text = c("", "# Annotations")),
                           Selected_Note)
    ZT_Selected <- ZT_Selected %>%
      rename("Text"= "Sentences") %>%
      bind_rows(Selected_Note)
  }
  
  if(IsZoteroAnnotation==F ){
    ZT_Selected <- ZT_Selected %>%
      rename("Text"= "Sentences")
  }
   
####  IMPORT EXISTING VERSION ON OBSIDIAN VAULT ####
  name_file <- paste0("/Users/stefanopagliari/Library/Mobile Documents/iCloud~md~obsidian/Documents/Obsidian ICloud/40. Reference/", SelectedKey, ".md")
  list_files <- list.files(path = "/Users/stefanopagliari/Library/Mobile Documents/iCloud~md~obsidian/Documents/Obsidian ICloud/40. Reference/" )
  
  if (sum(grepl(SelectedKey, list_files))>0){
    
  
  
  Old_MD <- readtext(name_file)
  Old_MD <- data.frame(Reference = unlist(str_split(Old_MD$text, "\n\n")))
  
  
  #Find the status in the existing file
  
  Old_Status_row <- str_detect(Old_MD$Reference, "\\*\\*Status\\*\\*\\:\\:")
  
  Old_Status <- str_sub(Old_MD$Reference[Old_Status_row],
          str_locate(Old_MD$Reference[Old_Status_row], "\\*\\*Status\\*\\*\\:\\:")[1],
          nchar(Old_MD$Reference[Old_Status_row]))
  
  
  # Find the status in the new file
  New_status_row <- str_detect(ZT_Selected$Text, "\\*\\*Status\\*\\*\\:\\:")
  New_Status <- str_sub(ZT_Selected$Text[New_status_row],
                        str_locate(ZT_Selected$Text[New_status_row], "\\*\\*Status\\*\\*\\:\\:")[1],
                        nchar(ZT_Selected$Text[New_status_row]))
  
  
  #Coimpare the old and new status
  Old_Status_Short <- NA
  if(Old_Status == "**Status**:: #todo/read"){Old_Status_Short <- 1}
  if(Old_Status == "**Status**:: #todo/process"){Old_Status_Short <- 2}
  if(Old_Status == "**Status**:: complete"){Old_Status_Short <- 3}

  New_Status_Short <- NA
  if(New_Status == "**Status**:: #todo/read"){New_Status_Short <- 1}
  if(New_Status == "**Status**:: #todo/process"){New_Status_Short <- 2}
  if(New_Status == "**Status**:: complete"){New_Status_Short <- 3}
  
  #If the existing status is more advanced replace it
  if(is.na(New_Status_Short)==F){
    if(is.na(Old_Status_Short)==F){
  if(Old_Status_Short>New_Status_Short){
    ZT_Selected$Text[New_status_row] <- gsub(New_Status,
         Old_Status,
         ZT_Selected$Text[New_status_row], fixed = T)
  }
    }
  }
  
  # Find comments added manually on Obsidian
  
Added_Comment <- intersect(
  intersect( #Find rows that do not start with
    which(str_sub(Old_MD$Reference, 1, 1)!="*"),
    which(str_sub(Old_MD$Reference, 1, 1)!="#")),
    which(str_sub(Old_MD$Reference, 1, 1)!="-"))

Added_Comment <- Added_Comment[Added_Comment>which(str_detect(Old_MD$Reference, "# Annotations"))[1]]
Added_Comment <- Added_Comment[str_detect(Old_MD$Reference[Added_Comment], "\n* \"")==F]

 
if (length(Added_Comment)>0){
  for (r in Added_Comment){
    Match <- which(ZT_Selected$Text == Old_MD$Reference[r-1])
    ZT_Selected <- dplyr::add_row(
      ZT_Selected,
      Text = Old_MD$Reference[r],
      .before = Match+1
    )
     
  }
}

  #Find references added on Obsidian and add them to the file exported
  References_vec <-  unlist(gregexpr("\\)\\)\\\n \\^", Old_MD$Reference))
  Num_References_vec <- which(References_vec != -1)
  Pos_References_vec <- References_vec[References_vec != -1]
  Pos_References_vec <- Pos_References_vec[Pos_References_vec != -1]
  
  r <- 1
  if (length(Num_References_vec)>0){
  for (r in 1:length(Num_References_vec)){
    Ref <- paste0(word(str_sub(Old_MD$Reference[Num_References_vec[r]], Pos_References_vec[r], Pos_References_vec[r]+20),1:2),
                  collapse = " ")
    Ref <- gsub("))", "", Ref)
    Ref <- gsub("\\*", "", Ref)
    Match <- which(grepl(str_sub(Old_MD$Reference[Num_References_vec[r]], 1, 20), ZT_Selected$Text))
    ZT_Selected$Text[Match] <- paste0(ZT_Selected$Text[Match], Ref)
  }
  }
           
  
 
  

# Find edits that have been made to a reference
  
  
  Equal_Row <- grepl(Old_MD$Reference,
          ZT_Selected$Text)    
  
  Different_Rows<- anti_join(Old_MD, ZT_Selected, by = c("Reference" = "Text"))
  Different_Rows_ID <- which(Old_MD$Reference %in% Different_Rows$Reference)
  Different_Rows_ID <- Different_Rows_ID[Different_Rows_ID>which(str_detect(Old_MD$Reference, "# Annotations"))[1]]
  
  if (length(Different_Rows_ID)>0){
    for (r in Different_Rows_ID){
    Match_Amended <-   c(
        which(grepl(str_sub(Old_MD$Reference[r], 1, nchar(Old_MD$Reference[r])/2), ZT_Selected$Text, fixed = T)),
        which(grepl(str_sub(Old_MD$Reference[r], nchar(Old_MD$Reference[r])/2, nchar(Old_MD$Reference[r])), ZT_Selected$Text, fixed = T))
      )[1] 
      
      ZT_Selected$Text[Match_Amended] <- Old_MD$Reference[r]
    }}
  }
  
#### EXPORT MD DOCUMENT TO OBSIDIAN VAULT ####
  
  
  cat(ZT_Selected$Text, file = name_file, sep = "\n\n")
  
  #Create a vector with the file created
  ifelse(exists("File_Exported")==F,
         File_Exported <- SelectedKey,
         File_Exported <- c(File_Exported, SelectedKey)
         )
  
}


##### INDEX of 🗄️.References ####

 
#find the file location
Folders_Vector <- str_sub(ZoteroBib$Reference, 
        str_locate(ZoteroBib$Reference, "collections = \\{")[,2]+1,
        nchar(ZoteroBib$Reference))

Folders_Vector <- str_sub(Folders_Vector, 
                          1,
                          str_locate(Folders_Vector, "\\},\\\n")[,1]-1)


Folders_Vector <- Folders_Vector[is.na(Folders_Vector)==F]
Folders_Vector <- sort(unique(unlist(str_split(Folders_Vector, ", "))))
Folders_Vector <- gsub("textasciitilde\\{\\}","~",Folders_Vector)
Folders_Vector <- gsub("\\\\","",Folders_Vector)
Folders_Vector <- gsub("textasciitilde ","~ ",Folders_Vector)

# Create Reference MOC
Reference_Index <- data.frame(Sentences = "---")
 

##### ADD Parent TO FOLDER NOTES ######
# Find list with all files
All_MD <- list.files("/Users/stefanopagliari/Library/Mobile Documents/iCloud~md~obsidian/Documents/Obsidian ICloud/", "*.md", recursive = T)

i <- 3
for (i in 1:length(Folders_Vector)){

  Selected_Folder <- Folders_Vector[i]
  Selected_File <- which(str_detect(All_MD, paste0("/",Selected_Folder, ".md")))
  # Find if the file exists already
  ifelse(length(Selected_File)>0, 
         File_Exists <- T, 
         File_Exists <- F) 
  
  
  #If the file does not exist already, create it
  if(File_Exists == F){
    Selected_Note <- data.frame(Sentences = "---")
  
  #Create "Parent" for breadcrumbs
  
  #Add Collections to the tag block
  Breadcrumbs <- paste0("Parent: \n",
                        "  - [[🗄]]\n")
  
  Selected_Note <- rbind(Selected_Note,
                           data.frame(Sentences = Breadcrumbs))
  
  ### CLOSE YAML
  Selected_Note <- rbind(Selected_Note,
                           data.frame(Sentences = "---"))
  
  name_file <- paste0("/Users/stefanopagliari/Library/Mobile Documents/iCloud~md~obsidian/Documents/Obsidian ICloud/", Selected_Folder , ".md")
  cat(Selected_Note$Sentences , file = name_file, sep = "\n\n")
  print(Selected_Folder)
  }
}
  
##### Export current time #####
  Current_Time <- Sys.time()
 # cat(as.character(Current_Time), file = "/Users/stefanopagliari/Library/Mobile Documents/iCloud~md~obsidian/Documents/Obsidian ICloud/80. Resources/R Script/script_time_run.txt")

  #Print which files have been exported
  print(File_Exported)
