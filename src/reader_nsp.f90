	subroutine reader_nsp
!       d-DOSes are in the following order
!       dxy,dyz,dz2,dxz,dx2 
!
!                  Satadeep Bhattacharjee
!   =================================================
        USE module
        implicit none
        
!
	open(10,file='DOSCAR',form = 'formatted')
        read(10,*)natom,natom,q1,q2
        do i=1,4
        read(10,*)
        enddo
	read(10,*)emax,emin,NE,efermi,kn
        write(*,*)'Number of energy points in DOSCAR = ',NE
        write(*,*)'Number of atoms in the DOSCAR file = ',natom
        write(*,*)'Was it a non-collinear calculation? (yes=1,No=2)'
        read(*,*)ncinput
!
        if(ncinput==2)then
        do i=1,NE
        read(10,*)(dummy(i,k),k=1,3)
        enddo

!
        do i=1,natom
	read(10,*)emax,emin,NE,efermi,kn
	do ie=1,NE
	read(10,*)e(ie),s_u(i,ie),py_u(i,ie),pz_u(i,ie),&
            px_u(i,ie),dxy_u(i,ie),dyz_u(i,ie),dz2_u(i,ie),dxz_u(i,ie),&
            dx2_u(i,ie)
!
	enddo
        enddo
        else
        do i=1,NE
        read(10,*)(dummy(i,k),k=1,3)
        enddo
!
        do i=1,natom
	read(10,*)emax,emin,NE,efermi,kn
	do ie=1,NE
        read(10,*)e(ie),s_u(i,ie),ds,ds,ds,py_u(i,ie),ds,ds,ds,&
                 pz_u(i,ie),ds,ds,ds,px_u(i,ie),ds,ds,ds,dxy_u(i,ie),&
                 ds,ds,ds,dyz_u(i,ie),ds,ds,ds,dz2_u(i,ie),ds,ds,ds,&
                 dxz_u(i,ie),ds,ds,ds,dx2_u(i,ie),ds,ds,ds
!
	enddo
        enddo
        endif
        close(10)
!
        do ie=1,NE
        e(ie)=e(ie)-efermi
        enddo
        write(*,*)'Total d-DOS(1) or projected d-DOS(2)'
        write(*,*)'or total p-DOS(3)'
        write(*,*)'For printing d-band center use Total d-DOS'
        read(*,*)iinput
        if(iinput==1.or.iinput==3)then
        write(*,*)'Give the range of the atoms'
        read(*,*)n1,n2
        else
        endif
	end subroutine
