

library(rvest)

page0_url<-read_html ("https://www.tripadvisor.com/Restaurants-g187371-Cologne_North_Rhine_Westphalia.html")

pages_bottom<-page0_url%>% 
        html_nodes(" .pageNum ") %>% 
        html_attr(name="data-page-number")
        

npages=tail(pages_bottom,1)

dat<-matrix(nrow=30*npages,ncol=2)
offset=0
idx_s=0

for (i in 1:npages)
{
        #chabge page url in every interation to go to the next page 
        page_url<-paste("https://www.tripadvisor.com/Restaurants-g187371-oa",offset,
                        "-Cologne_North_Rhine_Westphalia.html#EATERY_LIST_CONTENTS",sep="")
        #parse HTML page
        link<-read_html(page_url)
        
        #get restaurant names from this page
        R_names<-link %>%
                html_nodes("a.property_title") %>%
                html_text() %>%
                gsub('[\r\n\t]', '', .)
        
        R_inpage<-length( R_names)
        
        R_url<-link %>% 
                html_nodes(".shortSellDetails h3 a") %>% 
                html_attr(name="href")
        
        R_url<-paste("https://www.tripadvisor.com",R_url,sep="")
        
        
        dat[(idx_s+1):(idx_s+length(R_names)),1]<-R_names
        dat[(idx_s+1):(idx_s+length(R_names)),2]<-R_url
        
        idx_s=idx_s+length(R_names)
        
        #increment the offset to refer to the next page
        offset<-offset+30      
}

dat<-na.omit(dat)