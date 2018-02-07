
library(tm)

temp = proc.time()
clean_text = function(txt)
{
  txt <- removeNumbers(tolower(txt))
  txt <- removePunctuation(txt)
  txt <- removeWords(txt, stopwords("english"))
  txt <- stripWhitespace(txt)
  txt<-unlist(strsplit(txt," "))
  txt=txt[txt!=""]
}
predict.data1=function(txt)
{
  ## load the data1
  load("wf_uni.RData")
  data_uni_1= wf_uni
  load("wf_bi.RData")
  data_bi_1=wf_bi
  load("wf_tri.RData")
  data_tri_1=wf_tri
  load("wf_fth.RData")
  data_fth_1=wf_fth
################################
  output="N/A"
  length_list=as.numeric(as.character(length(txt)))
  # search for the last word in the sentence
    if(length_list>0)
    {
      # the word is in the first position
      subset_uni=data_uni_1[txt[length_list]==data_uni_1$word,]
      sub_bi=data_bi_1[txt[length_list]==data_bi_1$word_1,]
      sub_tri=data_tri_1[txt[length_list]==data_tri_1$word_1,]
      sub_fth=data_fth_1[txt[length_list]==data_fth_1$word_1,]
      ### intersection between  two sets
      list_word1_1 = merge(sub_tri,sub_bi, by=c("word_1","word_2"), all.x=FALSE, all.y=FALSE)
      list_word1_2= merge(sub_fth,sub_tri, by=c("word_1","word_2","word_3"), all.x=FALSE, all.y=FALSE)
      
      #######################################################
      # the word is in the second position
      sub_tri_2=data_tri_1[txt[length_list]==data_tri_1$word_2,]
      sub_fth_2=data_fth_1[txt[length_list]==data_fth_1$word_2,]
      
      list_word2_1= merge(sub_fth_2,sub_tri_2, by=c("word_2","word_3"), all.x=FALSE, all.y=FALSE)
      
      # the word third position
      ########################
      sub_fth_3=data_fth_1[txt[length_list]==data_fth_1$word_3,]
      
      ######### the backoff algorithm
      # the first priority on the intersection between the data sets as the sequence
      # appear  in two n-gram
      # the second priorty will be the longest predicted sequence from 
      # 4-gram ----> 3-gram -----> 2-gram
      output="N/A"
      # choose one row of the dataset that have max frequency
      if(nrow(list_word1_2)>1){
        list_word1_2=list_word1_2[which.max(as.numeric(as.character(list_word1_2$freq.x))),]
        output=paste(list_word1_2$word_2,list_word1_2$word_3,list_word1_2$word_4,sep=" ")
      }else if(nrow(list_word2_1)>1){
        list_word2_1=list_word2_1[which.max(as.numeric(as.character(list_word2_1$freq.x))),]
        output=paste(list_word2_1$word_3, list_word2_1$word_4,sep=" ")
      }else if(nrow(list_word1_1) >1){
        list_word1_1=list_word1_1[which.max(as.numeric(as.character(list_word1_1$freq.x))),]
        output=paste(list_word1_1$word_2,list_word1_1$word_3,sep=" ")
      } else if(nrow(sub_fth)>1 ){
        sub_fth=sub_fth[which.max(as.numeric(as.character(sub_fth$freq))),]
        output = paste(sub_fth$word_2,sub_fth$word_3,sub_fth$word_4,sep=" ")
      }else if (nrow(sub_fth_2)>1){
        sub_fth_2=sub_fth_2[which.max(as.numeric(as.character(sub_fth_2$freq))),]
        output = paste(sub_fth_2$word_3,sub_fth_2$word_4,sep=" ")
      }else if (nrow(sub_fth_3)>1){
        sub_fth_3=sub_fth_3[which.max(as.numeric(as.character(sub_fth_3$freq))),]
        output = sub_fth_3$word_4
      }else if(nrow(sub_tri)>1){
        sub_tri=sub_tri[which.max(as.numeric(as.character(sub_tri$freq))),]
        output = paste(sub_tri$word_2,sub_tri$word_3,sep=" ")
      } else if(nrow(sub_tri_2)>1){
        sub_tri_2=sub_tri_2[which.max(as.numeric(as.character(sub_tri_2$freq))),]
        output = sub_tri_2$word_3
      }else if(nrow(sub_bi)>1){
        sub_bi=sub_bi[which.max(as.numeric(as.character(sub_bi$freq))),]
        output = sub_bi$word_2
      }
    } # RETURN IF 
  return(output)
}

predict.data2=function(txt)
{
  ## load the data2
  load("wf_uni_2.RData")
  data_uni_1= wf_uni
  load("wf_bi_2.RData")
  data_bi_1=wf_bi
  load("wf_tri_2.RData")
  data_tri_1=wf_tri
  load("wf_fth_2.RData")
  data_fth_1=wf_fth
  ################################
  output="N/A"
  length_list=as.numeric(as.character(length(txt)))
  # search for the last word in the sentence
  if(length_list>0)
  {
    # the word is in the first position
    subset_uni=data_uni_1[txt[length_list]==data_uni_1$word,]
    sub_bi=data_bi_1[txt[length_list]==data_bi_1$word_1,]
    sub_tri=data_tri_1[txt[length_list]==data_tri_1$word_1,]
    sub_fth=data_fth_1[txt[length_list]==data_fth_1$word_1,]
    ### intersection between  two sets
    list_word1_1 = merge(sub_tri,sub_bi, by=c("word_1","word_2"), all.x=FALSE, all.y=FALSE)
    list_word1_2= merge(sub_fth,sub_tri, by=c("word_1","word_2","word_3"), all.x=FALSE, all.y=FALSE)
    
    #######################################################
    # the word is in the second position
    sub_tri_2=data_tri_1[txt[length_list]==data_tri_1$word_2,]
    sub_fth_2=data_fth_1[txt[length_list]==data_fth_1$word_2,]
    
    list_word2_1= merge(sub_fth_2,sub_tri_2, by=c("word_2","word_3"), all.x=FALSE, all.y=FALSE)
    
    # the word third position
    ########################
    sub_fth_3=data_fth_1[txt[length_list]==data_fth_1$word_3,]
    
    ######### the backoff algorithm
    # the first priority on the intersection between the data sets as the sequence
    # appear  in two n-gram
    # the second priorty will be the longest predicted sequence from 
    # 4-gram ----> 3-gram -----> 2-gram
    output="N/A"
    # choose one row of the dataset that have max frequency
    if(nrow(list_word1_2)>1){
      list_word1_2=list_word1_2[which.max(as.numeric(as.character(list_word1_2$freq.x))),]
      output=paste(list_word1_2$word_2,list_word1_2$word_3,list_word1_2$word_4,sep=" ")
    }else if(nrow(list_word2_1)>1){
      list_word2_1=list_word2_1[which.max(as.numeric(as.character(list_word2_1$freq.x))),]
      output=paste(list_word2_1$word_3, list_word2_1$word_4,sep=" ")
    }else if(nrow(list_word1_1) >1){
      list_word1_1=list_word1_1[which.max(as.numeric(as.character(list_word1_1$freq.x))),]
      output=paste(list_word1_1$word_2,list_word1_1$word_3,sep=" ")
    } else if(nrow(sub_fth)>1 ){
      sub_fth=sub_fth[which.max(as.numeric(as.character(sub_fth$freq))),]
      output = paste(sub_fth$word_2,sub_fth$word_3,sub_fth$word_4,sep=" ")
    }else if (nrow(sub_fth_2)>1){
      sub_fth_2=sub_fth_2[which.max(as.numeric(as.character(sub_fth_2$freq))),]
      output = paste(sub_fth_2$word_3,sub_fth_2$word_4,sep=" ")
    }else if (nrow(sub_fth_3)>1){
      sub_fth_3=sub_fth_3[which.max(as.numeric(as.character(sub_fth_3$freq))),]
      output = sub_fth_3$word_4
    }else if(nrow(sub_tri)>1){
      sub_tri=sub_tri[which.max(as.numeric(as.character(sub_tri$freq))),]
      output = paste(sub_tri$word_2,sub_tri$word_3,sep=" ")
    } else if(nrow(sub_tri_2)>1){
      sub_tri_2=sub_tri_2[which.max(as.numeric(as.character(sub_tri_2$freq))),]
      output = sub_tri_2$word_3
    }else if(nrow(sub_bi)>1){
      sub_bi=sub_bi[which.max(as.numeric(as.character(sub_bi$freq))),]
      output = sub_bi$word_2
    }
  } # RETURN IF 
  return(output)
}

predict.data3=function(txt)
{
  ## load the data2
  load("wf_uni_3.RData")
  data_uni_1= wf_uni
  load("wf_bi_3.RData")
  data_bi_1=wf_bi
  load("wf_tri_3.RData")
  data_tri_1=wf_tri
  load("wf_fth_3.RData")
  data_fth_1=wf_fth
  ################################
  output="N/A"
  length_list=as.numeric(as.character(length(txt)))
  # search for the last word in the sentence
  if(length_list>0)
  {
    # the word is in the first position
    subset_uni=data_uni_1[txt[length_list]==data_uni_1$word,]
    sub_bi=data_bi_1[txt[length_list]==data_bi_1$word_1,]
    sub_tri=data_tri_1[txt[length_list]==data_tri_1$word_1,]
    sub_fth=data_fth_1[txt[length_list]==data_fth_1$word_1,]
    ### intersection between  two sets
    list_word1_1 = merge(sub_tri,sub_bi, by=c("word_1","word_2"), all.x=FALSE, all.y=FALSE)
    list_word1_2= merge(sub_fth,sub_tri, by=c("word_1","word_2","word_3"), all.x=FALSE, all.y=FALSE)
    
    #######################################################
    # the word is in the second position
    sub_tri_2=data_tri_1[txt[length_list]==data_tri_1$word_2,]
    sub_fth_2=data_fth_1[txt[length_list]==data_fth_1$word_2,]
    
    list_word2_1= merge(sub_fth_2,sub_tri_2, by=c("word_2","word_3"), all.x=FALSE, all.y=FALSE)
    
    # the word third position
    ########################
    sub_fth_3=data_fth_1[txt[length_list]==data_fth_1$word_3,]
    
    ######### the backoff algorithm
    # the first priority on the intersection between the data sets as the sequence
    # appear  in two n-gram
    # the second priorty will be the longest predicted sequence from 
    # 4-gram ----> 3-gram -----> 2-gram
    output="N/A"
    # choose one row of the dataset that have max frequency
    if(nrow(list_word1_2)>1){
      list_word1_2=list_word1_2[which.max(as.numeric(as.character(list_word1_2$freq.x))),]
      output=paste(list_word1_2$word_2,list_word1_2$word_3,list_word1_2$word_4,sep=" ")
    }else if(nrow(list_word2_1)>1){
      list_word2_1=list_word2_1[which.max(as.numeric(as.character(list_word2_1$freq.x))),]
      output=paste(list_word2_1$word_3, list_word2_1$word_4,sep=" ")
    }else if(nrow(list_word1_1) >1){
      list_word1_1=list_word1_1[which.max(as.numeric(as.character(list_word1_1$freq.x))),]
      output=paste(list_word1_1$word_2,list_word1_1$word_3,sep=" ")
    } else if(nrow(sub_fth)>1 ){
      sub_fth=sub_fth[which.max(as.numeric(as.character(sub_fth$freq))),]
      output = paste(sub_fth$word_2,sub_fth$word_3,sub_fth$word_4,sep=" ")
    }else if (nrow(sub_fth_2)>1){
      sub_fth_2=sub_fth_2[which.max(as.numeric(as.character(sub_fth_2$freq))),]
      output = paste(sub_fth_2$word_3,sub_fth_2$word_4,sep=" ")
    }else if (nrow(sub_fth_3)>1){
      sub_fth_3=sub_fth_3[which.max(as.numeric(as.character(sub_fth_3$freq))),]
      output = sub_fth_3$word_4
    }else if(nrow(sub_tri)>1){
      sub_tri=sub_tri[which.max(as.numeric(as.character(sub_tri$freq))),]
      output = paste(sub_tri$word_2,sub_tri$word_3,sep=" ")
    } else if(nrow(sub_tri_2)>1){
      sub_tri_2=sub_tri_2[which.max(as.numeric(as.character(sub_tri_2$freq))),]
      output = sub_tri_2$word_3
    }else if(nrow(sub_bi)>1){
      sub_bi=sub_bi[which.max(as.numeric(as.character(sub_bi$freq))),]
      output = sub_bi$word_2
    }
  } # RETURN IF 
  return(output)
}
clc_time=function()
{
  return(proc.time() - temp)
}