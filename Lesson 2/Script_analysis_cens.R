        
        #============================================================#
        #  INTRODUCTION TO R FOR UNIVARIATE AND BIVARIATE ANALYSIS   #
        #                                                            #
        #                      Saverio Minardi                       #
        #============================================================#




#Let's start by emptying the environment
     

    #Remove everything
    rm(list=ls())  
 
    #Remove only one object
    my_object<-0
    rm(my_object)    
    
    #Remove everything except one object
    my_object<-0
    my_object2<-0
    rm(list = setdiff(ls(), "my_object")) 
 
    #Remove everything
    rm(list=ls()) 
    
#Set a working directory
    
    getwd() #tells us which one is our current working directory
    setwd("") #Set a new working directory (/ not \)
    
    
    
                             #### Import data ####
    
    
#Import a csv dataset
    
    cens <- read.csv("Data/ipums_cens_subs.csv")
    class()
    
                           #### Check the dataset ####
    
    
    View(cens) #Browse the full data matrix
    
    str(cens)  #Show the top observations for each column with the variable type
    
    head(cens, n=10) #Shows the to rows of the dataset
    

  
                            #### Rename Variables ####
    
    
    colnames(cens) # is a specific function used to get or set the column names of a matrix or data frame
    
    #More than one way to change names:
    
    #Colnames(cens) is a vector and we can access its elements
    
    colnames(cens)[1] <- "person_id"  
    
    #or
    
    colnames(cens)[colnames(cens) == "person_id"] <- "id"
    
    
                             #### Replace values ####
    
    # Each variable is a vector and we can therefore access and change elements
    
    cens$eldch[cens$eldch == 99] <- NA
    
    #or
    
    cens$eldch <- ifelse(cens$eldch == 99, # If 99 is found in the ver eldch
                         NA,               # Replalce with NA
                         cens$eldch)       # Else replace with existing eldch value
    
    
    
                          #### Create a new variable ####
    
    
     #Create a variable with all missing
    
    cens$new_var<-NA 
    
    #Create a dummy variable indicating people older than 80
    
    cens$old<-ifelse(cens$age>=80, 1, 0) #Dichotomous if older than 80
    
    #Any function
    cens$age_category <- cut(cens$age, breaks = seq(0, 125, 25), labels = FALSE, right = FALSE)
    
                         #### Drop existing variables ####
    
    # Assign null
    cens$new_var<-NULL
    
    #Subset the data
    cens <- subset(cens, select = -c(new_var))
    
    
                           #### Select observations ####
    
    #Select the rows based on values
    
    cens_males <-cens[cens$sex==1,]
    
    
                          ####     Missing data    #####
    
    #Is.na() is the basic function to detect missing values. It returns a logical
    #vector T if value is missing F if not
    
    is.na(cens$eldch)
    
    #We can table it to have na idea:
    table(is.na(cens$eldch))
    
    #We can quickly produce summary of missings on all variables
    
    colSums(is.na(cens))   #summ of T (=1) for each columns
    colMeans(is.na(cens))  #mean for each colums
    
    
                ####   Descriptive analysis continuous variables  ####
    
    #Summary is the basic command to inspect datasets or variables
    
    summary(cens)
    summary(cens$eldch)
    
    
    ## Measures of central tendency
    
    mean(cens$age) 
    median(cens$age)
    
    ##Consider NAs
    
    mean(cens$eldch)
    mean(cens$eldch, na.rm=T)
    
    ## Measures of dispersion 
    
    sd(cens$age) 
    var(cens$age)
    range(cens$age)
    min(cens$age)
    max(cens$age)
    
    # You can store the values in an object or variable
    
    mn_age<-mean(cens$age)
    sd_age<-sd(cens$age)
    
    
    # And use then as you want, for instance to generate a standardized variable:
    cens$age_stz <- (cens$age - mn_age)/sd_age
    
    summary(cens$age_stz)
    sd(cens$age_stz)
    
    
    # You can also build them in a summary table
    
    summary_table <- data.frame(
      Variable = c("age", "nchild"),
      Mean = c(mean(cens$age), mean(cens$nchild)),
      Median = c(median(cens$age), median(cens$nchild)),
      SD = c(sd(cens$age), sd(cens$nchild))
    )
    
    # Print the summary table
    print(summary_table)
    
    
    #export it on an excel or word document
    install.packages("writexl")

    library(writexl)

    # Export to Excel
    write_xlsx(summary_table, "summary_table.xlsx")
    
    
    rm(list = setdiff(ls(), "cens")) 

    
    #### Graphical representations for numerical variables ####
    
    #Histogram
    
    hist(cens$age)
    
    #Add details
    hist(cens$age,   
         xlab="Age",  
         main="Age distribution, New York, 1880", 
         freq = FALSE) 
    abline(v = median(cens$age), col = "red", lwd = 2)
    
    #Boxplot
    boxplot(cens$age,
            ylab="Age", 
            main="Age distribution, New York, 1880", 
            freq = FALSE) 
    

    #### Export an image ####
    
    
    #Set size of the image and text in pixels===================================
    
    png("hist.png", width = 1200, height = 800, pointsize = 20)
    
    #Plot
    hist(cens$age,
         xlab="Age", 
         main="Age distribution, New York, 1880", 
         freq = FALSE) 
    
    # Close the device
    dev.off()
    
    #===========================================================================
    
    #### Qualitative variables ####
    
    #Frequency table
    table(cens$birth_pl)
    
    #If we want to include missing as a category
    table(cens$birth_pl, useNA="always")
    
    #We can store the table in an object
    tab_mig<-table(cens$birth_pl)
    
    #We can transform the absolute frequencies to proportions
    prop.table(tab_mig)
    
    #And again store in an object
    tab_mig_prop<-prop.table(tab_mig)
    
    #We can build a table with frequencies and proportions with cbind()
    
    freq_mig<-cbind(tab_mig, tab_mig_prop) #cbind combines to columns
    colnames(freq_mig)<-c("N", "Share")
    print(freq_mig)
    
    #### Plot categorical variables ####
    

    #Sort based on frequency
    sorted_freq <- sort(tab_mig_prop, decreasing = TRUE)
    
    barplot(sorted_freq, 
            xlab = "Birth Place", 
            ylab = "Frequency", 
            main = "Frequency of Birth Places")
    
    #### Two way descriptive analysis ####
    
    ### Continuous categorical: 
    
    #let's investigate the relationship between the country of origins of females
    #and the number of children in the household                  
    
    # First let's select our sample and store it in another object
    cens_fems<-cens[cens$sex==2 & cens$age>=18 & cens$age<=30,]
    
    #And now let's compute the conditional means ( ~ )
    mean_by_group <- aggregate(nchild ~ birth_pl, data = cens_fems, FUN = mean)
    # And sort them again
    mean_by_group<- mean_by_group[order(mean_by_group$nchild), ]
    
    print(mean_by_group)
    
    #Plot as a barplot
    barplot(names.arg = mean_by_group$birth_pl, 
            mean_by_group$nchild,
            ylab = "Number of own children in the household",
            main = "Average number of children living in the household\n for women aged 18 to 30 (New York, 1880)")
    
    
    #We can also have two ways boxplot very easily
    boxplot(cens_fems$nchild ~ cens_fems$birth_pl,
            ylab = "Number of children",
            xlab = "Country of origin")
    
    ### Categorical and categorical variables
    
    ##Let's investigate the marital status of men by their country of origin
    
    #First select the sample
    cens_mal<-cens[cens$sex==1 & cens$age>=30 & cens$age<=50,]
    
    #then compute the two way frequency table
    table(cens_mal$marst, cens_mal$birth_pl)
    
    #Then we percentualise: margin indicate percentualisation by row or colum
    prop.table(table(cens_mal$marst, cens_mal$birth_pl), margin=2)
    
    
    #We can store it in an object
    cont_table<-prop.table(
                table(cens_mal$marst, cens_mal$birth_pl)
                , margin=2)
    
    #Transform in percentages and round up
    cont_table<-round(cont_table*100, 2)
    
    #Sort
    cont_table <- cont_table[, order(cont_table["Married", ])]
    
    cont_table
    
    # Plot the contingency table
    barplot(cont_table, beside = TRUE, legend = TRUE,
            xlab = "", 
            ylab = "Percentage married", 
            main = "Percentage of marital status by country of birth,\n males aged 30 to 50 (New York, 1880)",
            args.legend = list(xpd = TRUE, 
                               x = 26, 
                               y = -20, 
                               bty= "n", 
                               horiz=T, 
                               text.width = c(6, 5, 5)))
    
    ### Continuous and continuous variables
    rm(list=ls())
    ### Let's change dataset
    
    data("swiss")
    
    ?swiss
    

    ##Have a look at the structure
    str(swiss)
    summary(swiss)
    
    #Plot them
    hist(swiss$Agriculture)
    hist(swiss$Fertility)
    
    #Compute pearson correlations
    cor(swiss$Agriculture, swiss$Fertility, method="pearson" )
    
    #Scatterplot
    plot(swiss$Agriculture, swiss$Fertility)
    
    # Add a fitted line
    fit <- lm(swiss$Fertility ~ swiss$Agriculture)  # Fit a linear regression model
    abline(fit, col = "red")
    
    
    #Full correlation matrix
    plot(swiss)
    cor(swiss)
    
    
    #Generate a dummy for catholic and protestants
    swiss$relig<-ifelse(swiss$Catholic>=50, "Catholic", "Protestant")
    
   
    
    ### Scatter with colors by group
    
    # Create an empty plot
    plot(0, xlim = range(swiss$Education), ylim = range(swiss$Fertility), 
         xlab = "Fertility", ylab = "Education")
    # Plot the scatterplot for the first category
    points(swiss$Education[swiss$relig == "Catholic"], swiss$Fertility[swiss$relig == "Catholic"], 
           col = "red", pch = 16)
    # Plot the scatterplot for the second category
    points(swiss$Education[swiss$relig == "Protestant"],swiss$Fertility[swiss$relig == "Protestant"], 
           col = "blue", pch = 17)
    legend("topright", legend = c("Catholic", "Protestant"), col = c("red", "blue"), pch = c(16, 17))
    
    
    ### Multiple plots
    
    par(mfrow=c(1,2))
    
    # Plot the scatterplot for the catholic in red
    plot(swiss$Education[swiss$relig == "Catholic"], 
         swiss$Fertility[swiss$relig == "Catholic"], 
         xlim = range(swiss$Education), 
         ylim = range(swiss$Fertility), 
         xlab = "Education", ylab = "Fertility", main="Catholic")
    
    # Plot the scatterplot for the protestant in blue
    plot(swiss$Education[swiss$relig == "Protestant"],
         swiss$Fertility[swiss$relig == "Protestant"], 
         xlim = range(swiss$Education), 
         ylim = range(swiss$Fertility), 
         xlab = "Education", ylab = "Fertility", main="Protestant")
    
    par(mfrow=c(1,1))
    
    

   ### Some usefull resources
    browseURL("https://r-graph-gallery.com/")
    