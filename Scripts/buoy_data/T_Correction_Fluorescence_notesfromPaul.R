# Script from Paul on testing rhos for temperature correction of fluorescence data

# Watras, C. J., Morrison, K. A., Rubsam, J. L., Hanson, P. C., Watras, A. J., LaLiberte, G. D., & Milewski, P. (2017). 
# A temperature compensation method for chlorophyll and phycocyanin fluorescence sensors in freshwater. 
# Limnology and Oceanography: Methods, 15(7), 642-652.

# This script uses the temperature compensation equation from Watras et al
#    to test the sensitivity of the correction to the parameter, rho.
#    Data (T and corresponding Fm) were picked from Fig. 1A, and multiple
#    values for rho are tested and plotted.

rhos = c(-0.008,-0.010,-0.012) # ranges from about -0.008 to -0.012
Tr = 20 # reference T, 20C

# data estimated from Watras et al. 2018, Fig. 1A
Tm = c(5,10,15, 20,25,30,35)
Fm = c(1275,1225,1150,1100,1050,1000,950) # these are RFU values for the same [chl] across different Ts

myCols = c('red','blue','green')

# Plot original data
plot(Tm,Fm,type='b',ylim=c(900,1300),ylab='Fm, Fr')

for (i in 1:length(rhos)){
  rho = rhos[i]
  # Temperature correction
  Fr = Fm/(1+rho*(Tm-Tr))
  # Plot it
  lines(Tm,Fr,type='b',col = myCols[i])
}
legendTxt = c('Fm',as.character(rhos))
legend('topright',legendTxt,lty=rep(1,length(rhos)+1),col=c('black',myCols))

# Notes from Paul via email:
# This for chlorophyll and phycocyanin correction, based on Watras et al. 2018. I think a very similar equation is used for FDOM.
# I think the only decision to be made is… what’s a reasonable value for rho? Based on Table 1 in Watras et al., a value of -0.01 (or -0.012) would be about right.
# I took a quick look at CDOM correction according to Watras et al. (2011). Same equation. Based on Table 1, a rho of about -0.01 would be about right, just like the rho for Chl and Phyco. 
# Watras, C. J., Hanson, P. C., Stacy, T. L., Morrison, K. M., Mather, J., Hu, Y. H., & Milewski, P. (2011). A temperature compensation method for CDOM fluorescence sensors in freshwater. Limnology and Oceanography: Methods, 9(7), 296-301.