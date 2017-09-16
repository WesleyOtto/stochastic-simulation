C      gerador de numeros pseudo-aleatorios
C      Xn = 16807*Xn-1 mod(2^32-1 - 1)

       Real*8 rnum(10000), dmax, pmod, resFinal, x, y, resFuncao
       Integer ISEED, num, Pint

C      rnum - numero pseudo-aleatorio
C      ISEED - semente
C      num - quantidades de n§ pseudo aleatorios a serem gerados

       Open(1, file = "abaixo.dat", status = 'unknown')
       Open(2, file = "acima.dat", status = 'unknown')

C      DADOS
       pmod = 2147483647.d0 ! 2**31-1
       Pint = 0
       Ptotal = 0
       
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

       Do 1 i = 1, 10000
1         rnum(i) = 0.0d0

C      Calculos

       dmax = 1.0d0/pmod
       rnum(1) = ISEED * dmax
       write(*,93) rnum(1), ISEED
93     format(F6.4, I15)

       Do i=2,num
           rnum(i) = cong(ISEED)

           if (i.gt.2) then
              x = rnum(i)
              y = rnum(i-1)
              
              resFuncao = sin(x) * ((exp(x) - exp(-x)) / 2)

84            format(F6.4, 1x, F6.4)
              if (y.le.resFuncao) then
                 Pint = Pint + 1
                 write(1,84) x, y
              else
                 write(2,84) x, y
              end if
              
           endif
           
       end do
       
       write(*,*) Pint
       
95     format('Resultado final: ', F6.4)
       
       resFinal = (dfloat(Pint) / dfloat(num-2))
       write(*,95) resFinal
       
       close(1)
       close(2)
       
       read(*,*)

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
       