# d-band-center Program
This code calculates the d-band centers (spin up and spin down) from a VASP calculation with LORBIT=11 and prints the _effective d-band center_
given by

![image](https://user-images.githubusercontent.com/27854932/177729995-08bfb976-f0b5-4e64-8da7-1e0315cae63a.png)





The file required is VASP-DOSCAR file.
use LORBIT = 11 in the DOS calculation.

**To use the code:**
1. Go to src directory
2. Please check the FC= in the Makefile
3. Type make
4. Run the executable DOSutility from where you have the DOSCAR file.

Use Total d-DOS option (1) to print the d-band centers. Once the atoms are selected the code prints the 
d-DOS for the up and down electrons for the selected atoms in a file called **d-ud.dat**.
