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

Restaurant_Name<-vector(mode="character", length=30*npages)
Restaurant_URL<-vector(mode="character", length=30*npages)


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
        # dat[(idx_s+1):(idx_s+R_count),1]<-R_names
        # dat[(idx_s+1):(idx_s+R_count),2]<-R_url

        
        Restaurant_Name[(idx_s+1):(idx_s+R_count)]<-R_names
        Restaurant_URL[(idx_s+1):(idx_s+R_count)]<-R_url
        
        #increment the start index
        idx_s=idx_s+length(R_names)
        
        #increment the offset to refer to the next page
        offset<-offset+30      
}

#Remove NA values
# dat<-na.omit(dat)

Restaurant_Name<-na.omit(Restaurant_Name)
Restaurant_URL<-na.omit(Restaurant_URL)

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
        rest_url<-Restaurant_URL[i]
        #parse HTML page
        rest_cont<-read_html(rest_url)
        
        #get number of reviews
        
        Reviews[i]<-rest_cont %>% 
                html_nodes("#TABS_REVIEWS .tabs_pers_counts") %>% 
                html_text() %>%
                gsub('[(/)]',"",.) %>%
                as.numeric()
        
        #Stars
        Stars[i]<-rest_cont %>%
                html_nodes(".rr45") %>%
                html_attr("content") %>%
                as.numeric()
        
        
        #cuisine
        Cuisine[[i]]<-rest_cont %>%
                html_nodes("div.detail.separator a") %>%
                html_text() %>%
                gsub('[\r\n\t]', '', .)
        
        #photos
        Photos[i]<-rest_cont %>%
                html_nodes("div.count")%>%
                html_text()%>%
                gsub('[(/)]',"",.) %>%
                as.numeric()
        
        
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

dat<-cbind.data.frame(Restaurant_Name,Restaurant_URL,Reviews,Stars,Photos,Cuisine,NearBy,NearByURL,stringsAsFactors=F)


ff<-data.frame(as.matrix(cbind(Restaurant_Name,Restaurant_URL,Reviews,Stars,Photos,Cuisine,NearBy,NearByURL)))

ff<-data.frame(as.matrix(cbind(Restaurant_Name,Restaurant_URL,Reviews,Stars,Photos)),stringsAsFactors=F)


#Write data frame to a CSV file
write.table(ff,file="Cologne_Rest_test.csv",sep=",",row.names = F)


