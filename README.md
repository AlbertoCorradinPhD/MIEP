# MIEP
Urso L, Bertazzolo I, Corradin A, Silic-Benussi M, Minuzzo SA, D'Agostino DM, Ciminale V. mTOR inhibition sensitizes T-ALL cells to Venetoclax through engagement of the integrated stress response. Signal Transduct Target Ther. 2025 Aug 4;10(1):246. doi: 10.1038/s41392-025-02368-8.

Corradin, A., Ciccarese, F., Raimondi, V., Urso, L., Silic-Benussi, M., Ciminale, V. (2025). Gene Set-Focused Analysis of RNA-Seq Data with MIEP (Make-It-Easy-Pipeline). In: Cerulo, L., Napolitano, F., Bardozzo, F., Cheng, L., Occhipinti, A., Pagnotta, S.M. (eds) Computational Intelligence Methods for Bioinformatics and Biostatistics. CIBB 2024. Lecture Notes in Computer Science, vol 15276. Springer, Cham. https://doi.org/10.1007/978-3-031-89704-7_5

# Use
Clone/download the reposotory on your PC. Then, after having installed packages "devtools" and "BiocManager", you can install MIEP and its dependencies by running the following command:
devtools::install("pathToMIEP/MIEP", dependencies=TRUE, repos=c(BiocManager::repositories(),'https://github.com/AckerDWM/gg3D'), build=TRUE, build_vignettes=TRUE)
