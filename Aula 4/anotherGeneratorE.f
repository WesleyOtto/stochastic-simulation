C      gerador de numeros pseudo-aleatorios
C      Xn = 16807*Xn-1 mod(2^32-1 - 1)

       REAL*8 rnum(10000), dmax, pmod, media, cov, sumx, sumxx, sumyy
       REAL*8 sumy, sumxy, correlacao, squareX, squareY
       REAL*8 quiQuad, fi(10), minVal, maxVal, step, stepMin, stepMax
       REAL*8 fiAux
       Integer ISEED, num, quiQuadNum, indexFi

C      rnum - numero pseudo-aleatorio
C      ISEED - semente
C      num - quantidades de n§ pseudo aleatorios a serem gerados

       Open (1, file = 'aleaE.dat', status = 'unknown')
       Open (2, file = 'plotE.dat', status = 'unknown')

C      DADOS
       pmod = 2.d0 ! 2**31-1
       
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

        Do 1 i = 1, 10
1         fi(i) = 0.0d0

        Do 2 i = 1, 10000
2         rnum(i) = 0.0d0

C      Calculos

       quiQuadNum = 10
       minVal = 0d0
       maxVal = 1d0

       step = (maxVal - minVal) / quiQuadNum
       stepMin = minVal
       stepMax = minVal + step
       
       media = 0d0
       sumx = 0d0
       sumy = 0d0
       sumxy = 0d0
       correlacao = 0d0
       sumxx = 0d0
       sumyy = 0d0
       dmax = 1.0d0/pmod
       rnum(1) = ISEED * dmax
       write(*,93) rnum(1), ISEED
93     format(F6.4, I15)
103    format(F8.4, 1X, F8.4)

       Do i=2,num
           rnum(i) = cong(iseed)
           media = media + rnum(i)
           
           if (i.gt.2) then
              sumx = sumx + rnum(i)
              sumy = sumy + rnum(i-1)
              sumxy = sumxy + (rnum(i) * rnum(i-1))
              
              sumxx = sumxx + (rnum(i)**2)
              sumyy = sumyy + (rnum(i-1)**2)
              
              write(2, 103) rnum(i), rnum(i-1)
           end if
           
           if(num.le.200) write(*,93) rnum(i), ISEED
           write(1,93) rnum(i), ISEED

           Do j = 1, quiQuadNum
              stepMin = step * (j-1)
              stepMax = stepMin + step
              
              if (rnum(i).ge.stepMin.and.rnum(i).lt.stepMax) then
                 fi(j) = fi(j) + 1
              end if
           end do

       end do
       
       Do i = 1, quiQuadNum
          fiAux = ((fi(i)-dfloat(num)/quiQuadNum)**2)/(dfloat(num)/10)
          quiQuad = quiQuad + fiAux
       end do
       
       correlacao = ((dfloat(num) * sumxy) - ( sumx * sumy))
       squareX = dsqrt((dfloat(num)*sumxx) - (sumx**2))
       squareY = dsqrt((dfloat(num)*sumyy) - (sumy**2))
       correlacao = correlacao / (squareX * squareY)
       
       cov = (sumxy/dfloat(num-1))
       cov = cov - (sumx/dfloat(num) * sumy/dfloat(num))

       media = media / dfloat(num)
94     format('A media eh de: ', F6.4)
95     format('A covariancia eh de: ', F6.4)
96     format('A correlacao eh de: ', F8.4)
97     format('Qui-quadrado eh de: ', F8.4)

       write(*, 94) media
       write(1, 94) media
       
       write(*, 95) cov
       write(1, 95) cov
       
       write(*, 96) correlacao
       write(1, 96) correlacao

       write(*, 97) quiQuad
       write(1, 97) quiQuad
       
       close(1)
       close(2)
       
       read(*, *)

       end

       FUNCTION cong(ISEED)
       
       REAL*8 rmod, pmod, dmax
       Integer ISEED, IMOD
       
       RMOD = DFLOAT(ISEED)
       PMOD = 2.0D0
       dmax = 1.0D0/pmod
       rmod = (rmod * 1.0d0) + 0
       IMOD = RMOD * dmax
       RMOD = RMOD - PMOD*IMOD
       CONG = RMOD * dmax
       ISEED = RMOD
       Return
       end
       
