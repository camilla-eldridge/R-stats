dna_list<-list("g","c","t","a")
dna_samp<-dna_list[sample(1:length(dna_list), size = 20, replace = TRUE, prob = c("0.35","0.35","0.15","0.15"))]
paste(c(dna_samp),collapse='')
