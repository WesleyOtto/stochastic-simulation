       REAL*8 iniWest, finWest, iniNorth, finNorth
       REAL*8 iniEast, finEast, iniSouth, finSouth
       REAL*8 random
       REAL*8 arrivedPercentage
       INTEGER ISEED, num
       INTEGER homeX, homeY
       INTEGER currX, currY
       INTEGER spentTime
       INTEGER maxTime
       INTEGER arrived, timesArrived, numberOfTests, iCont

C      Definitions
       numberOfTests = 0
       iCont = 0
       arrivedPercentage = 0

       homeX = 3
       homeY = 4

C      45% North
       iniNorth = 0
       finNorth = 0.45
       
C      35% East
       iniEast = 0.45
       finEast = 0.8
       
C      10% South
       iniSouth = 0.8
       finSouth = 0.9

C      10% West
       iniWest = 0.9
       finWest = 1
       
       DO WHILE (1.eq.1)
       
       write(*, 1)
1      format('Valor inicial ou semente:')
       read(*, 2) ISEED
2      format(I5)

       write(*, 3)
3      format('Quantidade de testes a serem realizados:')
       read(*, 2) numberOfTests

       write(*, 6)
6      format('Entre com o tempo m ximo para o experimento (minutos)')
       read(*, 2) maxTime

       DO WHILE (iCont.lt.numberOfTests)

C         Declarations
          spentTime = 0
          arrived = 0 ! 0 = not arrvied / 1 = has arrived

          currX = 0
          currY = 0

          DO WHILE (spentTime.lt.maxTime.and.arrived.eq.0)

             random = cong(ISEED)

C            Walk according to the percentage generated
             if (random.ge.iniNorth.and.random.lt.finNorth) then
                currY = currY + 1
             else if (random.ge.iniEast.and.random.lt.finEast) then
                currX = currX + 1
             else if (random.ge.iniSouth.and.random.lt.finSouth) then
                currY = currY - 1
             else if (random.ge.iniWest.and.random.lt.finWest) then
                currX = currX - 1
             end if

10           format(I5, 1x, I5)
C             write(*, 10) currX, currY
             
C             write(*,*) random


C            Verifying if my drunk person has arrived home
             if (currX.eq.homeX.and.currY.eq.homeY) then
                arrived = 1
             end if

             random = cong(ISEED)

C            30% waits one minute more
             if (random.le.0.3d0) then
                spentTime = spentTime + 1
             end if

             spentTime = spentTime + 5
          END DO
          
          if (arrived.eq.1) then
             timesArrived = timesArrived + 1
          end if
       
          iCont = iCont + 1
       
       END DO
       
       arrivedPercentage=(real(timesArrived)/real(numberOfTests)) * 100
       
4      format('A porcentagem de chegada ‚ de: ', F6.2)
       write(*, 4) arrivedPercentage
       
5      format('Chegou ', I5, ' vezes')
       write(*, 5) timesArrived

C      Program waits 'cause I'm not fast enough
       read(*,*)
       END DO

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

