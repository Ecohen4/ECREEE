# energy contained in Earth core (ignores nuclear fission and other exothermic processes)
cp=450   # J/(kg*Kelvin) heat capacity of Iron
rho=8000 # kg/(m^3)
deltaHF<-230*10^3 # J/kg heat of fusion of Iron (melting energy)
r<-1220*10^3 # meter radius of Earth core
vol<-(4/3)*pi*r^3 # volume of Earth core
mass<-vol*rho # iron mass of Earth core in kg = vol(m^3)*density(kg/m^3)
Tcore<-5700 # Kelvin
Tsurface<-300 # Kelvin
deltaT<-Tcore-Tsurface
enthalpy<-mass*(deltaT*cp + deltaHF) # free energy (ignoring entropy) = mass*(cooling energy + phase change) = kg*(k*J/(kg*k) + (J/Kg)) = J
enthalpy # Joules

# Total world annual energy consumption in 2010
consumption<-5*10^20 # Joules/yr

# years of energy equivalent contained in cooling the Earth's core
enthalpy/consumption # ~ 300 million years!