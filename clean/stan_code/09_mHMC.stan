data{
     vector[50] D;
     vector[50] A;
     vector[50] M;
}
parameters{
     real a;
     real bM;
     real bA;
     real<lower=0> sigma;
}
model{
     vector[50] mu;
    sigma ~ exponential( 1 );
    bA ~ normal( 0 , 0.5 );
    bM ~ normal( 0 , 0.5 );
    a ~ normal( 0 , 0.2 );
    for ( i in 1:50 ) {
        mu[i] = a + bM * M[i] + bA * A[i];
    }
    D ~ normal( mu , sigma );
}


