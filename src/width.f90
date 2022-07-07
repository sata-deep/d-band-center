	subroutine width(dos)
        USE MODULE
	implicit none
        real(8) dos(5000)
!
	a=e(1)
	b=e(NE)
	delta=e(2)-e(1)	
!
        sum1=0.0d0
        sum2=0.0d0
	do i=2,NE
	sum1=sum1+dabs(dos(i))
	sum2=sum2+(e(i)-mom)*(e(i)-mom)*dabs(dos(i))
	enddo
        sum1=sum1*delta
        sum2=sum2*delta
        f_a=dabs(dos(1))
        f_b=dabs(dos(NE))
!
        as1=0.5*delta*(f_a+f_b)+sum1
        as2=0.5*delta*((e(1)*f_a)+(e(NE)*f_b))
        as2=as2+sum2
        w=dsqrt(as2/as1)
	end subroutine width
