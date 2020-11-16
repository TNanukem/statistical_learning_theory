require("Ryacas")
require("dplyr")

# Setting the plot size
options(repr.plot.width=12, repr.plot.height=8)

# Setting the max depth for ryacas
yac_str("MaxEvalDepth(1000000)")

Bin <- function(n, m) {
    return (choose(n,m))
}

#' Plot of chernnoff bounds.
#'
#' Plots the chernoff bounds for given shattering coefficients and epsilon
#'
#' This function plots the bounds for one to analyze the convergence of a given
#' algorithm.
#'
#' @param shattering_coefficients list of lists, where each list is a shattering coefficient.
#' @param epsilon which value of epislon to use on the bound calculation.
#' @param algorithm_names list of strings with the name of the algorithms, for identification purposes on the plot
#' @param n_max the max value of n to be applied in the calculation
#'
#' @examples
#' plot.chernoff_bounds(list(shattering_1, shattering_2), algorithm_names=list('Alg_1', 'Alg_2'), n_max=100, epsilon=0.1)

plot.chernoff_bounds <- function(shaterring_coefficients, epsilon=0.1, algorithm_names, n_max=10000) {
    n = seq(1, n_max)
    colors = c(1)
    for(i in 1:length(shaterring_coefficients)){
        
        ch_b = 2*unlist(shaterring_coefficients[i])*exp(-2*n*epsilon^2/4)

        if(i == 1){
            plot(ch_b, ylim=c(0, 1), xlab='Number of Samples', ylab='Delta')
        }
        else {
            colors <- append(colors, i)
            points(ch_b, col=i)
        }
    }
    title('Chernoff Bounds')
    legend("topright",legend=algorithm_names, col=colors, lty = 1, lwd = 1)
}


#' Plot of generalization bounds.
#'
#' Plots the generalization bounds for given shattering coefficients and delta
#'
#' This function plots the bounds for one to analyze the convergence of a given
#' algorithm.
#'
#' @param shattering_coefficients list of lists, where each list is a shattering coefficient.
#' @param algorithm_names list of strings with the name of the algorithms, for identification purposes on the plot
#' @param emp_riks list of floats that identify the empirical risk of each algorihtm
#' @param delta which confidence interval to use on the calculation
#' @param n_max the max value of n to be applied in the calculation
#'
#' @examples
#' plot.generalization_bounds(list(shattering_1, shattering_2), algorithm_names=list('Alg_1', 'Alg_2'), n_max=100, delta=0.05,
#'                          emp_risks=list(0.1, 0.05))
plot.generalization_bounds <- function(shattering_coefficients, algorithm_names, emp_risks, delta=0.05, n_max=10000){
    n = seq(1, n_max)
    colors = c(1)
    for(i in 1:length(shattering_coefficients)){
        
        generalization_bound = unlist(emp_risks[i]) + sqrt(-4/n * (log(delta) - log(2*unlist(shattering_coefficients[i]))))

        if(i == 1){
            plot(generalization_bound[1:150], xlab='Number of Samples', ylab='Empirical Risk + Epsilon', type='l')
        }
        else {
            colors <- append(colors, i)
            lines(generalization_bound[1:150], col=i)
        }
    }
    title('Generalization Bounds')
    legend("topright",legend=algorithm_names, col=colors, lty = 1, lwd = 1)
}

#' Generate the shattering coefficient for a fixed number of hyperplanes
#'
#' Generates the shattering coefficient for a fixed number of hyperplanes
#'
#' This function generates a Ryacas string with the shattering coefficent for a fixed number of hyperplanes
#'
#' @param m the number of hyperplanes shattering the space
#' @param d the dimension of the space
#' @param the number of classes of the problem
#' @param ideal boolean indicating if this is the ideal number of hyperplanes or not
#'
#' @examples
#' generate_shattering_equation_for_fixed_number_of_hyperplanes(3, 2, 2)
generate_shattering_equation_for_fixed_number_of_hyperplanes <- function(m, d, c, ideal=FALSE){
    chooses = c()
    cs = c()
    sums = c()

    for(i in 1:c){
        c_ = paste('c', i, sep='')
        cs = append(cs, c(c_))

        if(i == 1){
            
            if(isTRUE(ideal)){
                sums = append(sums, stringr::str_interp('Sum(c1,1,2^${m},'))
                chooses = append(chooses, stringr::str_interp('Bin(2^(${d}*n^(2/(${d}+1))),c1)'))
            }
            else{
                sums = append(sums, stringr::str_interp('Sum(c1,1,2^${m},'))
                chooses = append(chooses, stringr::str_interp('Bin(2^${m},c1)'))
            }
        }
        else{
            sums = append(sums, stringr::str_interp("Sum(${cs[i]},1,2^${m}-${cs[i-1]},"))
            chooses = append(chooses, stringr::str_interp('Bin(2^${m}-${cs[i-1]},${cs[i]})'))
        }
    }

    final = ''

    for(i in 1:length(sums)){
        final = paste(final, sums[i], sep='')
    }
    for(i in 1:length(chooses)){
        if(i == 1){
            final = paste(final, chooses[i], sep='')
        }
        else{
            final = paste(final, chooses[i], sep='*')
        }
    }
    for(i in 1:length(sums)){
        final = paste(final, ')', sep='')
    }
    
    return(final)
}

#' Simplify the shattering proportion
#'
#' Simplify the proportion between used and ideal shattering coefficents
#'
#' This function generates a Latex string with the simplification of used shattering over ideal shattering
#'
#' @param m the number of hyperplanes shattering the space
#' @param m_id the number of hyperplanes shattering the space on the ideal scenario
#' @param d the dimension of the space
#' @param the number of classes of the problem
#' @param ideal boolean indicating if this is the ideal number of hyperplanes or not
#'
#' @examples
#' simplify_shattering_proportion(3, 1, 3, 3)
simplify_shattering_proportion <- function(m, m_id, d, c){
    shat_1 = generate_shattering_equation_for_fixed_number_of_hyperplanes(m, d, c)
    shat_2 = generate_shattering_equation_for_fixed_number_of_hyperplanes(m_id, d, c, ideal=TRUE)
    
    cat(paste(shat_1, shat_2, sep='/') %>% y_fn('Simplify') %>% y_fn('TeXForm') %>% yac_str()  )
}