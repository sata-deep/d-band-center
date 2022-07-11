# d-band-center Program

This code calculates the d-band centers (spin up and spin down) from a VASP calculation with LORBIT=11 and prints the _effective d-band center_
given by

![image](https://user-images.githubusercontent.com/27854932/177729995-08bfb976-f0b5-4e64-8da7-1e0315cae63a.png)




Here $f_\uparrow$ and $f_\downarrow$ are the fractional occupancy of the d-states. $\epsilon_{d\uparrow}$ and $\epsilon_{d\downarrow}$ are the d-band centers for the spin-up band and spin-down band respectively.

The file required is VASP-DOSCAR file.
use LORBIT = 11 in the DOS calculation.

**To use the code:**
1. Go to src directory
2. Please check the FC= in the Makefile
3. Type make
4. Run the executable DOSutility from where you have the DOSCAR file.

Use Total d-DOS option (1) to print the d-band centers. Once the atoms are selected the code prints the 
d-DOS for the up and down electrons for the selected atoms in a file called **d-ud.dat** (in addition to printing the d-band centers 
of those selected atoms). 

While performing DOS calculation care should be taken (in the POSCAR file) such that all transition metals for which the d-band center 
to be calculated should appear together.

If you use this code, please cite the following article:

**Bhattacharjee, S.**, Waghmare, U. & Lee, SC. An improved d-band model of the catalytic activity of magnetic transition metal surfaces. Sci Rep 6, 35916 (2016). https://doi.org/10.1038/srep35916
