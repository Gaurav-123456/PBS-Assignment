       program main
              integer :: M=3,N=2,Rc=0
              real :: c1=10.0,c2=0.01,c3=10.0,t=0.0,sum1,amu
              real::y10=500.0,y20=200.0,y1,y2,a0,h1,h2,h3,a1,a2,a3
     1              ,X=1e5
              real,dimension(3)::a
              y1=y10
              y2=y20
              open(unit=2,file="lotka.dat",status="replace")
              do while (t.le.30)

                        h1=y1
                        h2=y1*y2
                        h3=y2
                        a1=h1*c1
                        a2=h2*c2
                        a3=h3*c3
                        a0=a1+a2+a3
                        a(1)=a1
                        a(2)=a2
                        a(3)=a3
                        if(a0.eq.0.0) then
                                exit
                        endif
                        call random_number(r1)
                        call random_number(r2)

                        sum1=0.0
                        sum2=0.0
                        tau=(1/a0)*log((1.0/r1))
                        do i=1,3
                                sum1=sum1+a(i)
                                if(sum1.ge.(r2*a0)) then
                                        amu=i
                                        exit
                                endif
                        enddo
                        if(amu.eq.1) then
                                y1=y1+1
                        elseif(amu.eq.2) then
                                y1=y1-1
                                y2=y2+1
                        elseif(amu.eq.3) then
                                y2=y2-1
                        endif
                        Rc=Rc+1
                        write(*,*) "Time=",t
                        write(2,*) t,y1,y2
                        t=t+tau
                        write(*,*) "Y1=",y1
                        write(*,*) "Y2=",y2
              enddo
              print*,Rc
          endprogram
