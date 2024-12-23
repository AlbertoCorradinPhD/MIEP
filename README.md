# MIEP
Clone/download the reposotory on your PC. Then, after having installed packages "devtools" and "BiocManager", you can install MIEP and its dependencies by running the following command:
devtools::install("pathToMIEP/MIEP", dependencies=TRUE, repos=c(BiocManager::repositories(),'https://github.com/AckerDWM/gg3D'), build=TRUE, build_vignettes=TRUE)
