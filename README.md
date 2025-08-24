# Climate Change Opens Up New Fishing Possibilities for Large-Scale Trawling Vessels Off West Greenland

This repository contains the R scripts used in the research paper:  
**“Climate Change Opens Up New Fishing Possibilities for Large-Scale Trawling Vessels Off West Greenland”**  
(Published in *Fisheries Oceanography*: https://onlinelibrary.wiley.com/doi/10.1111/fog.12736)

The study investigates how climate change is opening new potential fishing grounds in the Arctic by using a maximum entropy (MaxEnt) model to predict fishing suitability.  

---

## Abstract

Climate change is transforming marine ecosystems, opening new fishing possibilities for large-scale trawling vessels in the Arctic. This study investigates the potential for new fishing grounds to emerge in West Greenland. We employed a maximum entropy model to predict fishing suitability based on climatological and time-invariant variables alongside public fishing vessel data. The model, validated with high accuracy, identified maximum depth, ice thickness, and ice concentration as the most important predictors of fishing suitability.  

Results indicate a 6.2% increase in suitable fishing grounds from the 2010s to the 2040s, and an 11.4% increase from the 2010s to the 2090s. This change is driven by decreasing ice coverage, allowing extended access to the productive shelf edge. While increased fishing suitability could enhance economic opportunities, it also raises concerns about overexploitation, ecological sustainability, and carbon resuspension.  

Our findings highlight the need for adaptive management strategies to balance economic gains with ecosystem protection in the face of ongoing climate change.  

---

## Repository Contents

- `/R_scripts/` — R code used to build and run the MaxEnt models.  
- No figures are included in this repository. All figures are available in the published paper: https://onlinelibrary.wiley.com/doi/10.1111/fog.12736  
- Data sources: Outputs from the NEMO-MEDUSA Earth System Model (external, not included in this repository).  

Full model outputs are available upon request — contact: matthew.hatton@strath.ac.uk  

---

## Methods Overview

- Model type: Maximum entropy (MaxEnt)  
- Data sources:  
  - Public fishing vessel data  
  - NEMO-MEDUSA Earth System Model outputs  

---

## Key Findings

- Access to the continental shelf (maximum depth) had the largest influence on fishing suitability, followed by ice thickness and ice concentration.  
- Earlier ice melt exposes muddy and sandy shelf-edge sediments for longer periods. These organic-rich areas are particularly vulnerable to carbon resuspension from trawling.  
- Fishing suitability is projected to increase significantly by the end of the century, driven by reduced sea-ice cover.  
- Potential ecological risks include:  
  - Sediment disturbance and carbon release  
  - Overexploitation of new fishing grounds  
  - Uncertain ecosystem-wide consequences of increased trawling pressure  

---

## Usage

1. Clone the repository:  
   ```bash
   git clone https://github.com/Matthew-Hatton/Climate-Change-Opens-Up-New-Fishing-Possibilities-for-Large-Scale-Trawling-Vessels-Off-West-Greenlan.git
2. Install required R packages (listed in the scripts).

3. Run MaxEnt modelling scripts from the /R_scripts/ directory.

Note: This repository provides scripts only. Data and full model outputs are not included.

Citation

If you use this repository, please cite:

Hatton, M. et al. (2025). Climate Change Opens Up New Fishing Possibilities for Large-Scale Trawling Vessels Off West Greenland. Fisheries Oceanography. https://onlinelibrary.wiley.com/doi/10.1111/fog.12736

License

This project is released under the MIT License.

Acknowledgements

This work was funded under the NERC Scottish Universities Partnership for Environmental Research (SUPER) Doctoral Training Partnership (Grant reference number NE/S007342/1, https://superdtp.standrews.ac.uk/
).

We thank Andrew Yool (National Oceanography Centre, Southampton) for providing NEMO-MEDUSA model outputs.
