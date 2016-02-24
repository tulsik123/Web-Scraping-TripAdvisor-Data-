library(rvest)

#parse html search result (here: restaurants in Cologne)
page0_url<-read_html ("https://www.tripadvisor.com/Restaurants-g187371-Cologne_North_Rhine_Westphalia.html")

#find the the lnumber of the last page listed in the bottom
npages<-page0_url%>% 
        html_nodes(" .pageNum ") %>% 
        html_attr(name="data-page-number") %>%
        tail(.,1) %>%
        as.numeric()

#create an empty matrix
dat<-matrix(nrow=30*npages,ncol=2)

offset=0 #offset of page url
idx_s=0 #start index of the entries in the matrix

for (i in 1:npages)
{
        #change page url in every interation to go to the next page 
        page_url<-paste("https://www.tripadvisor.com/Restaurants-g187371-oa",offset,
                        "-Cologne_North_Rhine_Westphalia.html#EATERY_LIST_CONTENTS",sep="")
        #parse HTML page
        link<-read_html(page_url)
        
        #get restaurant names from this page
        R_names<-link %>%
                html_nodes("a.property_title") %>%
                html_text() %>%
                gsub('[\r\n\t]', '', .)
        
        #get the links of the restaurants in the page
        R_url<-link %>% 
                html_nodes(".shortSellDetails h3 a") %>% 
                html_attr(name="href")

        R_url<-paste("https://www.tripadvisor.com",R_url,sep="")
        
        #get the number of restaurants in the page
        R_count<-length( R_names)
        
        #add restaurant names and url to the matrix
        dat[(idx_s+1):(idx_s+R_count),1]<-R_names
        dat[(idx_s+1):(idx_s+R_count),2]<-R_url
        
        #incrementthe start index
        idx_s=idx_s+length(R_names)
        
        #increment the offset to refer to the next page
        offset<-offset+30      
}

#Remove NA values
dat<-na.omit(dat)

#Convert the matrix into a dataframe
dat<-data.frame(dat,stringsAsFactors = F)

#Change the names of the data frame columns
names(dat)<-c("Name","url")


len=dim(dat)[1]

Reviews<-vector(mode="numeric", length=len)
Stars<-vector(mode="numeric", length=len)
Cuisine<-vector(mode="list", length=len)
Photos<-vector(mode="numeric", length=len)
NearBy<-vector(mode="list", length=len)
NearByURL<-vector(mode="list", length=len)



for(i in 1:3)
{
        rest_url<-dat[i,2] 
        #parse HTML page
        rest_cont<-read_html(rest_url)
        
        #get number of reviews
        Reviews[i]<-rest_cont %>% 
                html_nodes("#TABS_REVIEWS .tabs_pers_counts") %>% 
                html_text() %>%
                gsub('[(/)]',"",.)
        
        #Stars
        Stars[i]<-rest_cont %>%
                html_nodes(".rr45") %>%
                html_attr("content")
        
        
        #cuisine
        Cuisine[[i]]<-rest_cont %>%
                html_nodes("div.detail.separator a") %>%
                html_text() %>%
                gsub('[\r\n\t]', '', .)
        
        #photos
        Photos[i]<-rest_cont %>%
                html_nodes("div.count")%>%
                html_text()%>%
                gsub('[(/)]',"",.)
        
        
        #nearby url of rest and attractions
        nearBy_url<-rest_cont %>%
                html_nodes(".nameWrapper a ")%>%
                html_attr(name="href")
        
        #index of nearby rest
        ix<-grep("Restaurant",nearBy_url)
        
        
        NearBy[[i]]<-rest_cont %>%
                html_nodes(".nameWrapper")%>%
                html_text() %>%
                gsub('[\r\n\t]', '', .) %>%
                .[ix]
        #url of nearby rest a
        NearByURL[[i]]<-paste("http://www.tripadvisor.com",nearBy_url[ix],sep="")
}

dat<-data.frame(cbind(dat$Name,dat$url,Reviews,Stars,Photos,Cuisine,NearBy,NearByURL))

#Write data frame to a CSV file
write.table(dat,file="Cologne_Rest.csv",sep=",",row.names = F)
