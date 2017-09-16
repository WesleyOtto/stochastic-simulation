C      gerador de numeros pseudo-aleatorios
C      Xn = 16807*Xn-1 mod(2^32-1 - 1)

       REAL*8 rnum(10000), dmax, x, fx, mi, sigma, PI
       REAL*8 R1, R2, numerador, denominador
       Integer ISEED

C      rnum - numero pseudo-aleatorio
C      ISEED - semente
C      num - quantidades de n§ pseudo aleatorios a serem gerados

       Open (1, file = 'transformada_ex5.dat', status = 'unknown')

C      DADOS
       pmod = 2147483647.d0 ! 2**31-1
       PI = 3.1415
       
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
95     format('Digite o valor de media: ')
       read(*, 100) mi
       write(*, 96)
96     format('Digite o valor do desvio padrao: ')
       read(*, 100) sigma


C         Inicializa‡Æo

           Do 2 i = 1, 10000
2             rnum(i) = 0.0d0

C         Calculos
          dmax = 1.0d0/pmod
          rnum(1) = ISEED * dmax

          Do i=2,num
          
             rnum(i) = cong(iseed)

              if (i.gt.2) then
                 R1 = rnum(i)
                 R2 = rnum(i - 1)
                 x = dsqrt(-2.D0 * log(R1)) * cos(2.D0 * PI * R2)
                 numerador = exp( -(1.D0/2.D0) * ((x - mi) / sigma)**2)
                 denominador = sigma * dsqrt(2.D0 * PI)
                 
                 fx = numerador / denominador

10               format(F10.4, 1x, F10.4)
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
       
       FUNCTION fat(NUMBER)

       REAL*8 NUMBER

       fat = 1
       Do 1 i = 1, NUMBER
1          fat = fat*i

       Return
       end




