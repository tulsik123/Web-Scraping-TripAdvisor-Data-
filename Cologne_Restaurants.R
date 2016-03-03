library(rvest)

#parse html search result (here: restaurants in Cologne)
page0_url<-read_html ("https://www.tripadvisor.com/Restaurants-g187371-Cologne_North_Rhine_Westphalia11.html")

# find the the lnumber of the last page listed in the bottom of the main page
npages<-page0_url%>% 
        html_nodes(" .pageNum ") %>% 
        html_attr(name="data-page-number") %>%
        tail(.,1) %>%
        as.numeric()

Restaurant_Name<-vector(mode="character", length=30*npages)
Restaurant_URL<-vector(mode="character", length=30*npages)

offset=0 #offset of page url
idx_s=0 #start index of the entries in the vectors

for (i in 1:npages)
{
        #change page url in every iteration to go to the next page 
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
        
        Restaurant_Name[(idx_s+1):(idx_s+R_count)]<-R_names
        Restaurant_URL[(idx_s+1):(idx_s+R_count)]<-R_url
        
        #increment the start index
        idx_s=idx_s+length(R_names)
        
        #increment the offset to refer to the next page
        offset<-offset+30      
}

#remove empty values
Restaurant_Name<-Restaurant_Name [Restaurant_Name!=""]
Restaurant_URL<-Restaurant_URL[Restaurant_URL!=""]

#get the total number of restaurants
len=length(Restaurant_Name)

#create vectors to fill with the scarapped values 
Reviews<-vector(mode="numeric", length=len)
Stars<-vector(mode="numeric", length=len)
Cuisine<-vector(mode="list", length=len)
Photos<-vector(mode="numeric", length=len)
NearBy<-vector(mode="list", length=len)
NearByURL<-vector(mode="list", length=len)


#loop (len) times
for(i in 1:len)
{
        #read restaurant URL
        rest_url<-Restaurant_URL[i]
        #parse HTML page
        rest_cont<-read_html(rest_url)

        ####REVIEWS####
        #get the html_nodes corresponding to the reviews
        reviews_nodes<-rest_cont %>% 
                html_nodes("#TABS_REVIEWS .tabs_pers_counts")
        
        #check if the html_nodes is not empty, get the html text and convert to numeric
        Reviews[i]<- ifelse(length(reviews_nodes)!=0,
                            reviews_nodes%>% 
                                    html_text() %>%
                                    gsub('[(/)]',"",.) %>%
                                    as.numeric(),
                         NA
                 )

        ####STARS####
        #get the html_nodes corresponding to stars
        stars_nodes<-rest_cont %>%
                html_nodes(".rating_rr_fill")
        
        #check if the html_nodes is not empty, get the content and convert to numeric
        Stars[i]<- ifelse(length(stars_nodes)!=0,
                          stars_nodes %>% 
                                  html_attr("content") %>%
                                  as.numeric(),
                          NA
                          )

        ####CUISINE####
        #get the html_nodes corresponding to cuisine
        cuisine_nodes<-rest_cont %>%
                html_nodes("div.detail.separator a")
        
        ##check if the html_nodes is not empty, get the html text
        if(length(cuisine_nodes)!=0) 
        {
                Cuisine[[i]]<- cuisine_nodes %>%
                        html_text() %>%
                        gsub('[\r\n\t]', '', .)
        }
        else
        {
                Cuisine[[i]] <-NA    
        }
        
        ####PHOTOS####
        #get the html_nodes corresponding to photos
        photos_nodes<-rest_cont %>%
                html_nodes("div.count")
        
        ##check if the html_nodes is not empty, get the content and convert to numeric
        Photos[i]<-ifelse(length(photos_nodes)!=0,
                          photos_nodes%>%
                                  html_text()%>%
                                  gsub('[(/)]',"",.) %>%
                                  as.numeric(),
                          0
                          )
        
        
        ####EARBY RESTAURANTS####
        #getnearby url of rest and attractions
        nearBy_url<-rest_cont %>%
                html_nodes(".nameWrapper a ")%>%
                html_attr(name="href")
        
        #get index of nearby rest
        ix<-grep("Restaurant",nearBy_url)
        
        #get the names of the nearby restaurants
        NearBy[[i]]<-rest_cont %>%
                html_nodes(".nameWrapper")%>%
                html_text() %>%
                gsub('[\r\n\t]', '', .) %>%
                .[ix]
        
        #get the URL of the nearby restaurants
        NearByURL[[i]]<-paste("http://www.tripadvisor.com",nearBy_url[ix],sep="")
}

# dat<-cbind.data.frame(Restaurant_Name,Restaurant_URL,Reviews,Stars,Photos,Cuisine,NearBy,NearByURL,stringsAsFactors=F)
# ff<-data.frame(as.matrix(cbind(Restaurant_Name,Restaurant_URL,Reviews,Stars,Photos,Cuisine,NearBy,NearByURL)))

#create a data frame to from the vectors filled in the previous loop
ff<-data.frame(Restaurant_Name,Restaurant_URL,Reviews,Stars,Photos,stringsAsFactors=F)

#save in RDs file
save(ff,file="Cologne_Rest.Rds")

#Write data frame to a CSV file
write.table(ff,file="Cologne_Rest_test2.csv",sep=",",row.names = F)


