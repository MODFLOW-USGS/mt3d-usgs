# MT3D-USGS
[![Build Status](https://travis-ci.org/MODFLOW-USGS/mt3d-usgs.svg?branch=develop)](https://travis-ci.org/MODFLOW-USGS/mt3d-usgs)

[USGS Release Page](https://water.usgs.gov/ogw/mt3d-usgs/)

## Introduction
MT3D-USGS is an update of MT3DMS and is maintained by Vivek Bedekar (SSPA), Eric Morway (USGS), Chris Langevin (USGS), and Matt Tonkin (SSPA).  MT3D-USGS includes new transport modeling capabilities to accommodate flow terms calculated by MODFLOW packages that were previously unsupported by MT3DMS and to provide greater flexibility in the simulation of solute transport and reactive solute transport.

MT3D-USGS capabilities and features include:

* unsaturated-zone transport;
* transport within streams and lakes, including solute exchange with connected groundwater;
* capability to route solute through dry cells that may occur in the Newton-Raphson formulation of MODFLOW (that is, MODFLOW-NWT);
* new chemical reaction package options that include the ability to simulate interspecies reactions and parent-daughter chain reactions;
* new pump-and-treat recirculation package that enables the simulation of dynamic recirculation with or without treatment for combinations of wells that are represented in the flow model, mimicking the above-ground treatment of extracted water;
* reformulation of the treatment of transient mass storage that improves conservation of mass and yields solutions for better agreement with analytical benchmarks;
* separate specification of the partitioning coefficient (Kd) within mobile and immobile domains;
* capability to assign prescribed concentrations to the top-most active layer;
* change in mass storage owing to the change in water volume now appears as its own budget item in the global mass balance summary;
* ability to ignore cross-dispersion terms;
* definition of Hydrocarbon Spill-Source Package (HSS) mass loading zones using regular and irregular polygons, in addition to the currently supported circular zones; and
* ability to specify an absolute minimum thickness rather than the default percent minimum thickness in dry-cell circumstances.

## How to Cite

### ***Citation for MT3D-USGS***

Bedekar, V., Morway, E.D., Langevin, C.D., and Tonkin, M., 2016, MT3D-USGS version 1: A U.S. Geological Survey release of MT3DMS updated with new and expanded transport capabilities for use with MODFLOW: U.S. Geological Survey Techniques and Methods 6-A53, 69 p., http://dx.doi.org/10.3133/tm6A53

![alt text](https://water.usgs.gov/ogw/mt3d-usgs/images/TM6-A53-cover.jpg "MT3D-USGS user guide")

### ***Software/Code citation for MT3D-USGS***

Bedekar, V., Morway, E.D., Langevin, C.D., and Tonkin, M., 2016, MT3D-USGS version 1.0.0: Groundwater Solute Transport Simulator for MODFLOW: U.S. Geological Survey Software Release, 30 September 2016, http://dx.doi.org/10.5066/F75T3HKD

## Disclaimer

This software is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The software has not received final approval by the U.S. Geological Survey (USGS). No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality of the software and related material nor shall the fact of release constitute any such warranty. The software is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the software.
