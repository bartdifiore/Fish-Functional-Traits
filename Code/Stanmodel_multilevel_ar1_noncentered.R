sink("Multilevelregression_ar1_varyingintercepts_noncentered.stan")
cat("
    data {
    
    //Index variables
    //int n; // Number of plots
    int n_y; // Number of years
    int n_r; // Number of regions
    //int region[n_r]; // Indicator variable for region
    //int year[n_y]; // Indicator variable for year
    
    //Response variable
    matrix[n_y, n_r] y_dat; // Variable of interest (CWM trait value, etc.)
    
    // Predictor variables
    vector[n_y] NAO;  // NAO index across all regions
    matrix[n_y, n_r] Harvest; // Harvest index at the regional scale for each year
    matrix[n_y, n_r] Temp; // Average temperature at the regional scale for each year

    }
    
    parameters {
    
    real alpha_0; // Global intercept
    real beta_NAO; // Slope effect of NAO index
    real beta_H; // Slope effect of Harvest
    real beta_T; // Slope effect of Temperature
    real<lower=0> w; // Process varaiance
    real<lower=0> v; // Observation variance
    real<lower=0> sigma_r; // Variance between regions
    real<lower=0> sigma_x0; // Variance on the inital condition prior
    real<lower=0> sigma_H; // Variance on the harvest slope parameter prior
    real<lower=0> sigma_T; // Variance on the temperature slope parameter prior
    vector[n_r] gamma; // Deviations from the global intercept for each region
    real<lower = 0, upper = 1> phi; // Autocorrelation parameter
    vector[n_r] x0;
    matrix[n_y, n_r] x;
    }
    
    transformed parameters{
    vector[n_y] mu_pop;
    matrix[n_y, n_r] mu_r;
    matrix[n_y, n_r] x_hat;
  
    mu_pop = alpha_0 + beta_NAO * NAO;
    
    
    for(y in 1:n_y){
        for(r in 1:n_r){
          mu_r[y,r] = mu_pop[y] + beta_H * Harvest[y,r] + beta_T * Temp[y,r] + gamma[r]*sigma_r;
      }
    }
  
    for(r in 1:n_r){
      x_hat[1, r] = x0[r]; 
    }
    
    
    for(y in 2:n_y){
      for(r in 1:n_r){
        x_hat[y,r] = phi*x_hat[y-1,r] + mu_r[y, r]; 
      }
    }
    
    
    }
    
    model {
    
    //priors
    
    w ~ exponential(0.25);
    v ~ exponential(0.25);
    sigma_H ~ exponential(0.25);
    sigma_T ~ exponential(0.25);
    sigma_r ~ exponential(0.25);
    sigma_x0 ~ exponential(0.25);
    
    phi ~ uniform(0, 1);
    alpha_0 ~ normal(0, 1);
    beta_T ~ normal(0, sigma_T);
    beta_H ~ normal(0, sigma_H);
    beta_NAO ~ normal(0, 1);
    
    
    gamma ~ normal(0, sigma_r); // Indexed by region... so 4 gammas
    x0 ~ normal(0, sigma_x0); // Indexed by region ... so 4 x0
    
    // likelihood
    
    for(y in 1:n_y){
      for(r in 1:n_r){
        x[y,r] ~ normal(x_hat[y,r], v);
        y_dat[y,r] ~ normal(x[y,r], w);
      }
    }


    }",fill = TRUE)
sink()
