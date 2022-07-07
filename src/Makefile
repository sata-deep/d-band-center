FC=ifort
#FC=gfortran
FFLAGS= -O2 
LFLAGS= $(FFLAGS)
EXE= DOSutility                                                             
SRC= module.f90 main.f90 reader_nsp.f90 reader_sp.f90 doscalculator_nsp.f90\
	doscalculator_sp.f90 moment.f90 width.f90

SRC2=avint.f

.SUFFIXES:                                                                  
.SUFFIXES: .o .f90 .f                                                          

                                                                            
OBJ1=	$(SRC:.f90=.o)                                                        

OBJ2=	$(SRC2:.f=.o)                                                        
                                                                            
.f90.o:                                                                     
	$(FC) $(FFLAGS) -c $<                                                     
.f.o:                                                                     
	$(FC) $(FFLAGS) -c $<                                                     

all:	$(EXE)                                                                
                                                                            
$(EXE):	$(OBJ1) $(OBJ2)                                                    
	$(FC) $(LFLAGS) -o $@ $(OBJ1) $(OBJ2)                                        
                                                                            
$(OBJ):	$(MF)                                                               
                                                                            
clean: 
	rm -f *.mod                                                                     
	rm -f $(OBJ1) $(OBJ2) $(EXE)                                              
