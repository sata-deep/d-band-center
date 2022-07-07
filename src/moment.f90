	subroutine moment(dos)
        USE MODULE
	implicit none
        real(8) dos(5000)
!
	a=e(2)
	b=e(NE-1)
	delta=e(3)-e(2)	
!
        sum1=0.0d0
        sum2=0.0d0
        NE=NE-1
	do i=3,NE
	sum1=sum1+dabs(dos(i))
	sum2=sum2+e(i)*dabs(dos(i))
	enddo
        sum1=sum1*delta
        sum2=sum2*delta
        f_a=dabs(dos(2))
        f_b=dabs(dos(NE))
!
        as1=0.5*delta*(f_a+f_b)+sum1
        as2=0.5*delta*((e(2)*f_a)+(e(NE)*f_b))
        as2=as2+sum2
        mom=as2/as1
	end subroutine moment
