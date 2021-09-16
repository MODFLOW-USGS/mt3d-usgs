The 2ED5EA example was adjusted to include 6 EA of which 2 are solid phases. In addition, an inert mobile phase was included as well as an inert solid phase for illustration purposes.
The first solid phase corresponds to the 3th aqueous EA (5th mobile species) and the second solid phase corresponds to the 4th aqueous EA (6th mobile species). This is based on the TEA sequence of O2, NO3, MnO2, Fe-oxide, SO4 and CH4.

A specific ordering of the simulated species is assumed:

    1. Electron donors (mobile) - here 2 ED's: ED1 & ED2
    2. Electron acceptors (mobile) - here 6 EA's: O2, NO3, Mn2+, Fe2+, SO4 and CH4. Mn2+ & Fe2+ are products of the reductive dissolution of the oxide species specified as immobile phases below.
    3. Mobile species not partaking in the ED/EA reactions - here 1 species (e.g. Cl)
    4. Immobile species representing solid phase EA - here 2 phases: the first, MnO2, corresponding to the 5th mobile species (Mn2+); the second, Fe(OH)3, corresponding to the 6th mobile species (Fe2+)
    5. Other immobile species - here 1 species for illustration purposes.

Initial concentrations of aqueous manganese were set to 1/10th of the initial ferrous iron concentration. The stoichiometric coefficient of Mn-oxide was taken from Wiedemeier et al. (1995), following Lu et al. (1999).
The solid phase concentration of the Fe-oxide was calculated from the MAXEC value of 50.5 mg/L using MAXEC = solid_phase_concentration [Mass_mineral / bulk_mass] * bulk_density [bulk_mass / bulk_volume] / porosity [volume_pores / bulk_volume]
The solid phase concentration of Mn-oxide was set to 1/1000th of the Fe-oxide content.