        MODULE MODULE
        implicit none
        integer:: natom,total,i,j,k,ie,n1,n2,NE,IERR
        integer::iinput,atom_number,index,kn,q1,q2,ispin
        integer::nhit,nat1,nat2,ncinput,ngroup,iu,LORBIT
        integer::indf,ip1,ip2,nat,yy
        integer::ip(10)
        double precision:: ans,negu,negd,ds,ex,tolerance
        double precision::gapU,gapD,eup,momu,momd,fup,fdn,dHNd 
        double precision::aden,anum,anum1,anum2,momav,aden1,aden2,dHNn 
        character*10 ::filename
        double precision :: e(5000),DUP(5000),DOW(5000)
        double precision ::s_u(100,5000),s_d(100,5000)
        double precision::px_u(100,5000),py_u(100,5000),pz_u(100,5000)
        double precision::px_d(100,5000),py_d(100,5000),pz_d(100,5000)
        double precision::dxy_u(100,5000),dyz_u(100,5000)
        double precision::dz2_u(100,5000)
	double precision::dxz_u(100,5000),dx2_u(100,5000)
	double precision::dxy_d(100,5000),dyz_d(100,5000),dz2_d(100,5000)
	double precision::dxz_d(100,5000),dx2_d(100,5000),num4(5000)
        double precision :: dosu(5000),dosd(5000),num1(5000),num2(5000),emin,emax,efermi
        double precision :: dosueg(5000),num3(5000),dosdeg(5000)
        double precision :: dospu(5000),dospd(5000),dosp(5000)
        double precision :: egu(5000),egd(5000),t2gu(5000),t2gd(5000)
        double precision ::dummy(5000,5),mom,as1,as2,a,b,delta,sum1,sum2
        double precision ::f_a,f_b,w,tu,td,sa_u,sa_d,pa_u,pa_d,da_u,da_d
        double precision ::xy(5000),x2(5000),z2(5000),xz(5000)
        double precision ::yz(5000),px(5000),py(5000),pz(5000)
        double precision ::DU(5000),DD(5000),IDU(5000),IDD(5000)
        END MODULE
