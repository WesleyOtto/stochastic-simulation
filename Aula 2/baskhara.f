C       Dada a equacao ax^2 + bx + c = 0
!       Com raizes: x1 = [ -b + dsqrt (b*b - 4*a*c)] / 2*a
!                   x2 = [ -b - dsqrt (b*b - 4*a*c)] / 2*a
       REAL*8 a,b,c,x1,x2,delta

C      Ler o valor de A
       write(*,1)
1      format('Digite o valor de A: ')
       read(*,4) a

C      Ler o valor de B
       write(*,2)
2      format('Digite o valor de B: ')
       read (*,4) b
       print*,b

C      Ler o valor de C
       write(*,3)
3      format('Digite o valor de C: ')
       read (*,4) c
       print*,c

4      format (F6.2)

C      A = 0 n∆o vai ser segundo grau

       if (a.eq.0) then
           write (*,20)
20         format('Valor de a eh zero')
           read(*,*)
           stop
       end if

C      Calculo de delta

       delta = (b*b - 4.0d0*a*c)
       write(*,5) delta
5      format('Delta: ', F5.2)

       if (delta.gt.0) then
           x1 = ( -b + dsqrt(delta)) / 2.0d0*a
           x2 = ( -b - dsqrt(delta)) / 2.0d0*a
           write (*,6)  x1, x2
6          format ('x1 = ', F5.2, 2x, 'x2 = ', F5.2)
       else if (delta.eq.0 ) then
           x1 = ( -b + dsqrt(delta)) / 2.0d0*a
           write (*,7) x1
7          format ('x1 = x2 = ', F5.2)
       else
           write(*,8)
8          format ("Nao tem raiz real ")
       end if
           read(*,*)
       end
         