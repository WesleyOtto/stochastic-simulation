C      gerador de numeros pseudo-aleatorios
C      Xn = 16807*Xn-1 mod(2^32-1 - 1)

       Real*8 rnum(10000), dmax, pmod, PI, x, y, formulaCirc
       Integer ISEED, num, Pint

C      rnum - numero pseudo-aleatorio
C      ISEED - semente
C      num - quantidades de n§ pseudo aleatorios a serem gerados

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
       write(1,83) num
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
              
              formulaCirc = (x**2)+(y**2)
              
              if (formulaCirc.le.1) Pint = Pint + 1
           endif
           
       end do
       
       write(*,*) Pint
       
95     format('PI eh igual a: ', F6.4)
       
       PI = 4 * (dfloat(Pint) / dfloat(num-2))
       write(*,95) PI
       
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
       