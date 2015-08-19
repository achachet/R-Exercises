# Objectif de l'exercice: vérifier le théorème de la limite centrale pour un lancer de pièces. 
#
# Experience: We have a biaised coin with P(heads)=p. We toss this coin 20 times and calculate the 95% 
# confidence interval for p. Conduct the experiment 1000 times for p=0.10, 0.15, 0.2, ..., 0.90, and 
# check what proportion of confidence intervals that contain p. 
# A function coverage(tosses) will return the proportion of correct confidence intervals for a 
# specified number of tosses
# plot coverage for n = 5, 10, 20, 50
#
# First version: use the central limit theorem to estimate the confidence intervals
# Second version: use the exact test for the binomial function to estimate the confidence intervals
#


### First vesrion: 95% confidence interval is estimated using the central limit theorem


prob_values <- seq(from=0.1, to=0.9, by=0.05) #Those are the values of p we want to test ranging from 0.1 to 0.9

coverage<-function(tosses){    
    
    nosims <- 1000   #A thousand times 
    sapply(prob_values, function(p){ #
        #On fait 1000 expériences de 20 lancers de pièce, et on prend à chaque fois le nombre de succès
        # Résultat: un vecteur de 1000 (nosims) estimations de p
        p_hats = (rbinom(n = nosims, size = tosses, prob=p))/(tosses) 
        

        #Pour chacun des phats on calcule l'intervalle de confiance correspondant à la simulation de 20 lancers
    
        # Sample means are normally distributed with mean mu (population mean) and sd sigma (population sd) / sqrt(n) (sample size)
        
        lower_bounds = ( p_hats                          #   sample mean
                       - qnorm(0.975)                    # + 97.5th quantile of the STD NORMAL LAW (CLT), for a 95% interval
                       * sqrt(p_hats*(1-p_hats)/tosses)  # * sigma/sqrt(n): sqrt(p(1-p))/sqrt(n)
                       )
        
        
        upper_bounds = ( p_hats                          #   sample mean
                       + qnorm(0.975)                    # + 97.5th quantile of the STD NORMAL LAW (CLT)
                       * sqrt(p_hats*(1-p_hats)/tosses)  # * sigma/sqrt(n)
                       )
        mean((p>=lower_bounds)*(p<=upper_bounds))
        
 
    })
}

plot(x = prob_values, y=coverage(5), ylim=c(0,1), type="l", col="red")
for (i in 1:3) lines(x=prob_values, y=coverage(c(10,20,50)[i]), col=c("blue", "green", "orange")[i])
abline(h=0.95)


### Second vesrion: 95% confidence interval is estimated using an exact binomial test


prob_values <- seq(from=0.1, to=0.9, by=0.05) #Those are the valuse of p we want to test ranging from 0.1 to 0.9

coverage<-function(tosses){    
  
  nosims <- 1000   #A thousand times 
  sapply(prob_values, function(p){ #

    
    # On fait 1000 expériences de 20 lancers de pièce, et on prend à chaque fois le nombre de succès
    # Résultat: un vecteur de 1000 (nosims) estimations de p
    p_hats = (rbinom(n = nosims, size = tosses, prob=p))/(tosses)
    
    # Pour chacun de ces p_hat on va calculer un intervalle de confiance en utilisant la fonction binom.test
    # Résultat: une matrice 2 lignes x 1000 colonnes contenant les limites inférieures et supérieures de l'intervalle
    conf_intervals=sapply(p_hats*tosses, function(elt) binom.test(elt, tosses)$conf.int)
    

    # On extrait les limites inférieures et supérieures dans deux vecteurs séparés
    # Résultat: deux vecteurs de longueur 1000 (nosims)
    lower_bounds = conf_intervals[1,]
    upper_bounds = conf_intervals[2,]
    
    mean((p>=lower_bounds)*(p<=upper_bounds))

  })
}

plot(x = prob_values, y=coverage(5), ylim=c(0,1), type="l", col="red")
for (i in 1:3) lines(x=prob_values, y=coverage(c(10,20,50)[i]), col=c("blue", "green", "orange")[i])
abline(h=0.95)

