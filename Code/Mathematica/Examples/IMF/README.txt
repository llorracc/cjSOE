MakeAllResults.m in this folder generates all the results for the IMF project. 

equiR.m calculates the general equilibrium interest rate. Previous code didn't fully endogenize
the interest rate. In particular, when households make their decisions, the interest rate they take 
was given when it really should have been unknown before it is solved. Although this does not create 
a big difference in the equilibrium interest rate, it is still important to be accurate. The current 
code has made necessary corrections. 