	subroutine reader_sp
!       d-DOSes are in the following order
!       dxy,dyz,dz2,dxz,dx2 
!
!                  Satadeep Bhattacharjee
!   =================================================
        USE module
        implicit none
        
!
        tolerance=0.05
        write(*,*)'Enter LORBIT (if /= 11 then program will stop)'
        read(*,*)LORBIT
	open(10,file='DOSCAR',form = 'formatted')
        read(10,*)natom,natom,q1,q2
        do i=1,4
        read(10,*)
        enddo
	read(10,*)emax,emin,NE,efermi,kn
        write(*,*)'Number of energy points in DOSCAR = ',NE
        write(*,*)'Number of atoms in the DOSCAR file = ',natom
!       Change this part
!
        do i=1,NE
        read(10,*)e(i),DU(i),DD(i),IDU(i),IDD(i)
        enddo
        indf=0
        do i=1,NE
        indf=indf+1
        e(i)=e(i)-efermi
        print*,e(i)
        if(dabs(e(i))<=tolerance)then
        indf=i
        go to 300
        else
        endif
        enddo
300     print*,"The energy was",e(indf)

        do k=indf,indf+1000
        if(DABS(IDU(k+1)-IDU(i)).GT.tolerance)then
        eup=e(k+1)
        go to 333
        else
        endif
        enddo
333     gapU=eup-efermi

        do k=indf,indf+1000
        if(DABS(IDD(k+1)-IDD(i)).GT.tolerance)then
        eup=e(k+1)
        go to 334
        else
        endif
        enddo
334     gapD=eup-efermi

        print*,"Spin-up gap:",gapU
        print*,"Spin-down gap:",gapD
!       Change this part

        if(LORBIT.NE.11)stop
!
        do i=1,natom
	read(10,*)emax,emin,NE,efermi,kn
	do ie=1,NE
	read(10,*)e(ie),s_u(i,ie),s_d(i,ie),py_u(i,ie),py_d(i,ie),&
            pz_u(i,ie),pz_d(i,ie),px_u(i,ie),px_d(i,ie),dxy_u(i,ie),&
            dxy_d(i,ie),dyz_u(i,ie),dyz_d(i,ie),dz2_u(i,ie),dz2_d(i,ie),&
            dxz_u(i,ie),dxz_d(i,ie),dx2_u(i,ie),dx2_d(i,ie)
!
	enddo
        enddo
        close(10)
        do ie=1,NE
        e(ie)=e(ie)-efermi
        enddo
        write(*,*)'Total d-DOS(1) or projected d-DOS(2)'
        write(*,*)'or total p-DOS(3)'
        write(*,*)'For printing d-band centers use Total d-DOS'
        read(*,*)iinput
        if(iinput==1.or.iinput==3)then
        write(*,*)'Give the range of the atoms'
        read(*,*)n1,n2
        else
        endif
	end subroutine
