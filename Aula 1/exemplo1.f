C      Calculo da area de um circulo de raio r
C      A = pi*r**2
       real*8 a,r,pi
C      REAL ou REAL*4 => precisao simple(6 casas depois da virgulo)
C      REAL*8 => precisao dupla(15 casas depois da virgula)
       open(1, file='dados.dat', status='unknown')
C      status => new, old, scratch(temporario)

C      Entrada

       pi = 3.1415d0
       
       write(*,10)
       read(*,11) r

C      Calculo

C       a = pi*r**2
C      a = 3.1415d0*r**2
       call calculo(r,a)

C      Saida

       write(1,12) r,a
       write(*,12) r,a
       
10     format('Digite o valor do raio')
11     format(f6.2) ! 6 casas, 2 sao decimais e inclui a virgula
12     format('raio:', f6.2, 2x, 'area:', f8.2)

       close(1)
       end
       
       subroutine calculo(r,a)
       real*8 a,r
       a = 3.1415d0*r**2
       return
       end