

getSummaryData <- function(input_data){
    
    l = ncol(input_data)
    input_data$user_group <- 
        apply(input_data[,-c(l-2,l-1,l)],1,getTestGroup)
    input_data$userWhoNotConvertedCount <- input_data$distinctUserCount - input_data$userWhoConvertedCount
    
    
    reshape(input_data, idvar='user_group', 
            varying=c('userWhoConvertedCount','userWhoNotConvertedCount'),
            times = c('success','failure'),
            timevar = 'Response', v.names='Count',direction='long')
    
}




getABTestResult <- function(input_data,alpha){
    control <- subset(input_data,isControl == 'y')
    
    control_group_name <- control$user_group[1]
    test <- subset(input_data, isControl == 'n')
    test_groups <- unique(test$user_group) 
    
    TestName <- rep('',length(test_groups))
    mean_diff <- rep(0,length(test_groups))
    pvalue <-  rep(0,length(test_groups))
    df <-  rep(0,length(test_groups))
    chisq <-  rep(0,length(test_groups))
    significant <- rep("", length(test_groups))
    
    
    for(i in 1:length(test_groups)) {
        cur_test <- subset(test, user_group == test_groups[i])
        
        cross_tab <- rbind(control$Count, cur_test$Count)
        colnames(cross_tab) <- c('Success','Failure')
        rownames(cross_tab) <- c(control_group_name,test_groups[i])
        
        t <- prop.test(cross_tab)
        mean_diff[i] <- t$estimate[2] - t$estimate[1]
        TestName[i] <- paste(control_group_name,
                             test_groups[i], sep=' VS ')
        pvalue[i] = t$p.value
        df[i] = t$parameter
        chisq[i] = t$statistic
       
    }
    
    corrected_pvalue <- p.adjust(pvalue, method='bonferroni',n=length(pvalue))
    res <- data.frame(TestName,chisq,df, corrected_pvalue, mean_diff)
    #names(res) <- c('Test', 'Chi-Square', 'Degree of Freedom', 'pvalue','Mean Proportion Differnce', 'Significant?')
    res
}
