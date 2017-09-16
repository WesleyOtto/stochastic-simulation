C      gerador de numeros pseudo-aleatorios
C      Xn = 16807*Xn-1 mod(2^32-1 - 1)

       REAL*8 rnum(10000), dmax, x, fx
       Integer ISEED

C      rnum - numero pseudo-aleatorio
C      ISEED - semente
C      num - quantidades de n§ pseudo aleatorios a serem gerados

       Open (1, file = 'transformada.dat', status = 'unknown')

C      DADOS
       pmod = 2147483647.d0 ! 2**31-1
       
       write(*,80)
80     format('Valor inicial ou semente')
       read(*,92) ISEED
92     format(I5)
       write(*,81) ISEED
81     format('A semente eh: ', I8)
       write(*,82)
82     format('Entre com o no. de aleatorios a serem gerados')
       read(*,92) num
       write(*,83) num
83     format('Quantidade de no. a sem gerados',I8)

C      Inicializa‡Æo

        Do 2 i = 1, 10000
2         rnum(i) = 0.0d0

C      Calculos
       dmax = 1.0d0/pmod
       rnum(1) = ISEED * dmax

       Do i=2,num
           rnum(i) = cong(iseed)

           if (i.gt.2) then

               x = dsqrt(rnum(i))
               fx = 2*x

10             format(F6.4, 1x, F6.4)
               write(1, *) x, fx

           end if

       end do

       close(1)
       
       read(*, *)

       end

       FUNCTION cong(ISEED)
       
       REAL*8 rmod, pmod, dmax
       Integer ISEED, IMOD
       
       RMOD = DFLOAT(ISEED)
       PMOD = 2147483647.0D0
       dmax = 1.0d0/pmod
       rmod = rmod * 16807.0d0
       IMOD = RMOD * dmax
       RMOD = RMOD - PMOD*IMOD
       CONG = RMOD * dmax
       ISEED = RMOD
       Return
       end
       
       




