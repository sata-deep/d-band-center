program main
USE MODULE
implicit none
write(*,*)'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
write(*,*)'/                               \'
write(*,*)' DOSCAR Utility Program'
write(*,*)' Satadeep Bhattacharjee           '
write(*,*)' Indo-Korea Science and Technology Center (IKST)'
write(*,*)'/                              \'
write(*,*)'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
write(*,*)' '
write(*,*)' '
write(*,*)'Spin-polarized(2) or non-spin polarized(1)?'
read(*,*)ispin
if(ispin==2)then
call reader_sp 
call doscalculator_sp
else
call reader_nsp
call doscalculator_nsp
endif
stop
end program
