	subroutine doscalculator_sp 
!       d-dos are in the following order
!       dxy,dyz,dz2,dxz,dx2 
!
!                  Satadeep Bhattacharjee
!   =================================================
        USE module
        implicit none
        
!
5000    format(f8.4,1x,f8.4,1x,f8.4,6f13.6)
7000    format(f8.4,1x,f8.4,1x,f8.4)
9000    format(f8.4,1x,f8.4)
        if(iinput==1)goto 100
        if(iinput==2)goto 200
        if(iinput==3)goto 250
100     open(40,file='d-ud.dat') 
        do ie=1,NE
        dosu(ie)=0.0
        dosd(ie)=0.0
        dosueg(ie)=0.0
        dosdeg(ie)=0.0
        do i=n1,n2
      dosu(ie)=dosu(ie)+dxy_u(i,ie)+dyz_u(i,ie)+dz2_u(i,ie)+dxz_u(i,ie)&
          +dx2_u(i,ie)
      dosd(ie)=dosd(ie)+dxy_d(i,ie)+dyz_d(i,ie)+dz2_d(i,ie)+dxz_d(i,ie)&
          +dx2_d(i,ie)

      dosueg(ie)=dosueg(ie)+dz2_u(i,ie)+dx2_u(i,ie)
      dosdeg(ie)=dosdeg(ie)+dz2_d(i,ie)+dx2_d(i,ie)
        enddo
        write(40,*)e(ie),dosu(ie),-dosd(ie)
        enddo
        close(40)
        call davint(e,dosueg,ne,e(1),0, ans, IERR)
        write(*,*)'eg occupation for spin up is ',ans
        negu=ans
        call davint(e,dosdeg,ne,e(1),0, ans, IERR)
        write(*,*)'eg occupation for spin down is ',ans
        negd=ans
        write(*,*)'Total eg occupation is ',negu+negd
!!      ======   d-band centers =====================
        nat=n2-n1+1
        write(*,*)"nat= ",nat
        do ie=1,NE
        num1(ie)=e(ie)*dosu(ie)
        num2(ie)=e(ie)*dosd(ie)
        num3(ie)=e(ie)*(dosu(ie)+dosd(ie))
        num4(ie)=(dosu(ie)+dosd(ie))
        enddo
        call davint(e,num1,ne,e(1),0,ans,IERR)
        anum1=ans
        call davint(e,dosu,ne,e(1),0,ans,IERR)
        aden1=ans
        momu=anum1/aden1
!        call davint(e,dosu,ne,e(1),0,ans,IERR)
        fup=dabs(aden1/(nat*5.0))
        write(*,'("Spin-up fractional occupation: ",f4.2)')fup
        write(*,'("The spin-up center: ",f6.2," eV")')momu

        call davint(e,num2,ne,e(1),0,ans,IERR)
        anum2=ans
        call davint(e,dosd,ne,e(1),0,ans,IERR)
        aden2=ans
        momd=anum2/aden2
!        call davint(e,dosd,ne,e(1),0,ans,IERR)
        fdn=dabs(aden2/(nat*5.0))
        write(*,'("Spin-down fractional occupation: ",f4.2)')fdn
        write(*,'("The spin-down center: ",f6.2," eV")')momd
        momav=(fup*momu+(fdn*momd))/(fup+fdn)
        momav=momav-((momd-momu)*((fup-fdn)/(fup+fdn)))
        call davint(e,num3,ne,e(1),0,ans,IERR)
        dHNn=ans
        call davint(e,num4,ne,e(1),0,ans,IERR)
        dHNd=ans
        write(*,*)"                                         "
        write(*,'("The effective band center: ",f7.3," eV")')momav
        write(*,'("The standard Hammer-Norskov band center: ",f7.3," eV")')dHNn/dHNd
        print*,"========================================"

        goto 300
!
250     open(50,file='p-ud.dat') 
        do ie=1,NE
        dospu(ie)=0.0
        dospd(ie)=0.0
        dosp(ie)=0.0
        do i=n1,n2
        dospu(ie)=dospu(ie)+px_u(i,ie)+py_u(i,ie)+pz_u(i,ie)
        dospd(ie)=dospd(ie)+px_d(i,ie)+py_d(i,ie)+pz_d(i,ie)
        enddo
        dosp(ie)=dospu(ie)+dospd(ie)
        write(50,'(4f12.8)')e(ie),dospu(ie),-dospd(ie),dosp(ie)
        enddo
        close(50)
        mom=0.0
        call moment(dosp)
        write(*,*)'THE P-BAND CENTER IS',mom, ' eV'
        
        call moment(dospu)
        write(*,*)'The p-band center for the spin-up is ',mom,'eV'
        call width(dospu)
        write(*,*)'The p-band width for the spin-up is ',w,'eV'
        write(*,*)"----------------------------------------------"
        mom=0.0
        call moment(dospd)
        write(*,*)'The p-band center for the spin-down is ',mom,'eV'
        call width(dospd)
        write(*,*)'The p-band width for the spin-down is ',w,'eV'
        goto 300
!
200	open(50,file='Atom-d.dat')
        open(55,file='t2geg.dat')
500	open(60,file='Atom-p.dat')

        write(*,*)'which atom'
        read(*,*)atom_number
        j=atom_number
!
        do ie=1,NE
        write(50,'(11f8.4)')e(ie),dxy_u(j,ie),dxy_d(j,ie),dyz_u(j,ie),&
             dyz_d(j,ie),dz2_u(j,ie),dz2_d(j,ie),dxz_u(j,ie),&
             dxz_d(j,ie),dx2_u(j,ie),dx2_d(j,ie)
        t2gu(ie)=dxy_u(j,ie)+dyz_u(j,ie)+dxz_u(j,ie)
        t2gd(ie)=dxy_d(j,ie)+dyz_d(j,ie)+dxz_d(j,ie)
        egu(ie)=dz2_u(j,ie)+dx2_u(j,ie)
        egd(ie)=dz2_d(j,ie)+dx2_d(j,ie)
        write(55,'(5f8.4)')e(ie),t2gu(ie),-t2gd(ie),egu(ie),-egd(ie)
        write(60,'(7f8.4)')e(ie),px_u(j,ie),px_d(j,ie),py_u(j,ie),&
             py_d(j,ie),pz_u(j,ie),pz_d(j,ie)
 
        enddo
        call davint(e,egu,ne,e(1),0, ans, IERR)
        write(*,*)'eg occupation for spin up is ',ans
        negu=ans
        call davint(e,egd,ne,e(1),0, ans, IERR)
        write(*,*)'eg occupation for spin down is ',ans
        negd=ans
        write(*,*)'Total eg occupation is ',negu+negd
  
        close(50)
        close(55)
        close(60)
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
        td=td+s_d(j,ie)+px_d(j,ie)+py_d(j,ie)&
        +pz_d(j,ie)+dxy_d(j,ie)+dyz_d(j,ie)&
        +dz2_d(j,ie)+dxz_d(j,ie)+dx2_d(j,ie)
!
        sa_u=sa_u+s_u(j,ie)
        sa_d=sa_d+s_d(j,ie)
        pa_u=pa_u+px_u(j,ie)+py_u(j,ie)+pz_u(j,ie)
        pa_d=pa_d+px_d(j,ie)+py_d(j,ie)+pz_d(j,ie)
        da_u=da_u+dxy_u(j,ie)+dyz_u(j,ie)+dz2_u(j,ie)&
        +dxz_u(j,ie)+dx2_u(j,ie)
        da_d=da_d+dxy_d(j,ie)+dyz_d(j,ie)+dz2_d(j,ie)&
        +dxz_d(j,ie)+dx2_d(j,ie)
        enddo
        write(70,5000)e(ie),tu,-td,sa_u,-sa_d,pa_u,-pa_d,da_u,-da_d
        enddo
        enddo
        else
        endif
        write(*,*)"If you want to calculate the" 
        write(*,*)"spin polarization, hit 1"
        read(*,*)nhit
        if(nhit==1)then
        write(*,*)"How many atoms?"
        read(*,*)nat1
        write(*,*)"Give the atoms"
        read(*,*)(ip(k),k=1,nat1)
        write(*,*)'Give the filename for sp-u and sp-d doses'
        read(*,*)filename
	open(80,file=filename)
        do ie=1,NE
        tu=0.0
        td=0.0
        do i=1,nat1 
        j=ip(i)
        tu=tu+s_u(j,ie)+px_u(j,ie)+py_u(j,ie)+pz_u(j,ie)&
        +dxy_u(j,ie)+dyz_u(j,ie)+dz2_u(j,ie)&
        +dxz_u(j,ie)+dx2_u(j,ie)
        td=td+s_d(j,ie)+px_d(j,ie)+py_d(j,ie)&
        +pz_d(j,ie)+dxy_d(j,ie)+dyz_d(j,ie)&
        +dz2_d(j,ie)+dxz_d(j,ie)+dx2_d(j,ie)
        enddo
        write(80,7000)e(ie),tu,-td
        DUP(ie)=tu
        DOW(ie)=td
        enddo
        else
        endif
        ex=e(1)
        iu=1
        do i=2,NE
        if(e(i).gt.ex.and.e(i).lt.0.01)then
        ex=e(i)
        iu=i
        else
        endif 
        enddo
        write(*,*)"The Spin polarization at the energy",e(iu)
        write(*,*)"D(E_F^up)",DUP(iu)," D(E_F^d)",DOW(iu)
        write(*,*)"is",(DUP(iu)-DOW(iu))/(DUP(iu)+DOW(iu)) 
        write(*,*)"Please verify the above energy is close to Fermi"
        write(*,*)"energy or not"
        
300     Write(*,*)'==========JOB DONE================================='
!
	end subroutine
