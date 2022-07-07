	subroutine doscalculator_nsp 
!       d-dos are in the following order
!       dxy,dyz,dz2,dxz,dx2 
!
!                  Satadeep Bhattacharjee
!   =================================================
        USE module
        implicit none
        
!
5000    format(f8.4,1x,f8.4,1x,f8.4,6f13.6)
6000    format(f8.4,1x,5f10.6)
7000    format(f8.4,1x,3f10.6)
        if(iinput==1)goto 100
        if(iinput==2)goto 200
        if(iinput==3)goto 250
100     open(40,file='d-ud.dat') 
        open(67,file='d-p.dat') 
        do ie=1,NE
        dosu(ie)=0.0
        dosd(ie)=0.0
        xy(ie)=0.0
        yz(ie)=0.0
        z2(ie)=0.0
        xz(ie)=0.0
        x2(ie)=0.0
        do i=n1,n2
      dosu(ie)=dosu(ie)+dxy_u(i,ie)+dyz_u(i,ie)+dz2_u(i,ie)+dxz_u(i,ie)&
          +dx2_u(i,ie)
        xy(ie)=xy(ie)+dxy_u(i,ie)
        yz(ie)=yz(ie)+dyz_u(i,ie)
        z2(ie)=z2(ie)+dz2_u(i,ie)
        xz(ie)=xz(ie)+dxz_u(i,ie)
        x2(ie)=x2(ie)+dx2_u(i,ie)
        enddo
        write(40,*)e(ie),dosu(ie),-dosu(ie)
        write(67,6000)e(ie),xy(ie),yz(ie),z2(ie),xz(ie),x2(ie)
        enddo
        close(40)
!        call moment(dosu)
        call davint(e,dosu,ne,e(1),0, ans, IERR)
        write(*,*)'The d-band center is ',ans,'eV'
        write(*,*)"----------------------------------------------"
        call width(dosu)
        write(*,*)'The d-band width is ',w,'eV'
        goto 300
!
250     open(50,file='p-ud.dat') 
        open(51,file='p-p.dat')
        do ie=1,NE
        dosu(ie)=0.0
        dosd(ie)=0.0
        px(ie)=0.0
        py(ie)=0.0
        pz(ie)=0.0
        do i=n1,n2
        dosu(ie)=dosu(ie)+px_u(i,ie)+py_u(i,ie)+pz_u(i,ie)
        px(ie)=px(ie)+px_u(i,ie)
        py(ie)=py(ie)+py_u(i,ie)
        pz(ie)=pz(ie)+pz_u(i,ie)
        enddo
        write(50,*)e(ie),dosu(ie)/2.0,-dosu(ie)/2.0
        write(51,7000)e(ie),px(ie),py(ie),pz(ie)
        enddo
        close(50)
        call moment(dosu)
        call davint(e,dosu,ne,e(1),0, ans, IERR)
        write(*,*)'The p-band center is ',ans,'eV'
        call width(dosu)
        write(*,*)'The p-band width for the spin-up is ',w,'eV'
        write(*,*)"----------------------------------------------"
        goto 300
200	open(50,file='Atom-d.dat')
        write(*,*)'which atom'
        read(*,*)atom_number
        j=atom_number
!
        do ie=1,NE
        write(50,'(6f8.4)')e(ie),dxy_u(j,ie),dyz_u(j,ie),&
             dz2_u(j,ie),dxz_u(j,ie),dx2_u(j,ie)
        enddo
        close(50)
write(*,*)'If you want to print the total DOS of a' 
        write(*,*)'collection of atoms'
        write(*,*)'then hit 1'
        read(*,*)nhit
        if(nhit==1)then
        write(*,*)'How many groups?'
        read(*,*)ngroup
        do i=1,ngroup
        write(*,*)'Give the set of atoms'
        read(*,*)nat1,nat2
        write(*,*)'Give the filename'
        read(*,*)filename
600	open(70,file=filename)
        do ie=1,NE
        tu=0.0
        td=0.0 
        sa_u=0.0
        sa_d=0.0
        pa_u=0.0
        pa_d=0.0
        da_u=0.0
        da_d=0.0
        do j=nat1,nat2
        tu=tu+s_u(j,ie)+px_u(j,ie)+py_u(j,ie)+pz_u(j,ie)&
        +dxy_u(j,ie)+dyz_u(j,ie)+dz2_u(j,ie)&
        +dxz_u(j,ie)+dx2_u(j,ie)
        
!
        sa_u=sa_u+s_u(j,ie)
        pa_u=pa_u+px_u(j,ie)+py_u(j,ie)+pz_u(j,ie)
        da_u=da_u+dxy_u(j,ie)+dyz_u(j,ie)+dz2_u(j,ie)&
        +dxz_u(j,ie)+dx2_u(j,ie)
        enddo
        tu=0.5*tu
        sa_u=0.5*sa_u
        pa_u=0.5*pa_u
        da_u=0.5*da_u
        write(70,5000)e(ie),tu,-tu,sa_u,-sa_u,pa_u,-pa_u,da_u,-da_u
        enddo
        enddo
        else
        endif
300     Write(*,*)'==========JOB DONE================================='
!
	end subroutine
