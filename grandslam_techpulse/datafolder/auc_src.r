my.roc <- function(prediction, label,use01="False"){ 

ii <- order(prediction, decreasing = T); # outputs a permutation that arranges prediction in descending order
  pr <- label[ii];
  lenpr <- length(pr);
  cspr <- cumsum(pr);
  if (use01){
    l_pos <- sum(pr);
  }
  else{
    l_pos <- (sum(pr) + lenpr)/2;
    cspr <- (cspr + 1:lenpr) /2;
  }
    l_neg <- lenpr - l_pos;
  if (l_neg == 0 || l_pos == 0){
    list(recall = (0:lenpr)/lenpr, fa = (0:lenpr)/lenpr);
  }
  else{ 
    list(recall = cspr / l_pos, fa = (1:lenpr - cspr) / l_neg);
  }
}

my.trapz <- function(x, y){
  w = diff(x);
  h = (y[2:length(y)] + y[1:(length(y) - 1)]) / 2;
  sum(w * h);
}

my.auc <- function(l){
  my.trapz(l$fa, l$recall);
}

auc.batch <- function(prediction, label, tag, dropEmpty = FALSE) {
    # tags are sorted to facilitate per batch auc calculation
    n = length(tag);
    t = as.character(tag[1]);
    g = list(c(prediction[1],label[1]));
    #print(g);
    total = 0;
    cnt = 1;
    for (i in 2:n){
        ct = as.character(tag[i]); 
        
        if ( ct != t){
            if (!(dropEmpty && length(unique(g[,2])) == 1)){
                cnt = cnt + 1;
                #print(ct);
                #print(g);
            
                auc = my.auc(my.roc(unlist(g[,1]),unlist(g[,2])));
                #print(auc);
                total = total + auc;
            }
            g = list();
            t = ct;            
        }
        g = rbind(g,c(prediction[i],label[i]));
    }
    (total + my.auc(my.roc(unlist(g[,1]),unlist(g[,2]))))/cnt;
    
}

