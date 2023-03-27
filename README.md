# ssmesa-weekly
a place to host code tutorials, discussions, and polls shared during the joint SSMA-MESA weekly meeting.

## contents

if you add a code tutorial, please include a description of it here:

`rema-example` (presented on 2023/02/22, J Sullivan): demo for `rema`, an R package for the random effects model used at the AFSC for biomass-based assessments and apportionment. The `sharpchin_rema.R` script walks through an example for GOA sharpchin rockfish, and includes model bridging from ADMB, troubleshooting tips, and an example of how to use the experimental Tweedie distribution.

`proj-example-goarebs` (presented 2023/03/29, J Sullivan): demo of how projections are run for MESA GOA rockfish, specifically GOA rougheye/blackspotted. Topics include: 1) what is different in a full vs. partial assessment year, 2) why and how we run the model twice, 3) assumptions about catch in the current and future years, and 4) mock-up of using R functions to process proj ADMB output and auto-generating a formatted executive summary table for the SAFE using `flextable`. Relied heavily on B William's [safe_tbl](https://github.com/ben-williams/safe_tbl) repo for the flextable example.