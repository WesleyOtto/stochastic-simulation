C      gerador de numeros pseudo-aleatorios
C      Xn = 16807*Xn-1 mod(2^32-1 - 1)

       REAL*8 rnum(10000), dmax, x, fx, a, b
       Integer ISEED

C      rnum - numero pseudo-aleatorio
C      ISEED - semente
C      num - quantidades de n§ pseudo aleatorios a serem gerados

       Open (1, file = 'transformada_ex2.dat', status = 'unknown')

C      DADOS
       pmod = 2147483647.d0 ! 2**31-1
       
100    format(F6.4)
       
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
       write(*,95)
95     format('Digite o valor de A: ')
       read(*, 100) a
       write(*,96)
96     format('Digite o valor de B: ')
       read(*,100) b

       if (b.gt.a) then

C         Inicializa‡Æo

           Do 2 i = 1, 10000
   2         rnum(i) = 0.0d0

C         Calculos
          dmax = 1.0d0/pmod
          rnum(1) = ISEED * dmax

          Do i=2,num
              rnum(i) = cong(iseed)

              if (i.gt.2) then

                  x = (b-a) * rnum(i) + a
                  fx = 1 / (b-a)

10                format(F6.4, 1x, F6.4)
                  write(1, *) x, fx

              end if

          end do

          close(1)

          read(*, *)
       
       endif

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
       
       




