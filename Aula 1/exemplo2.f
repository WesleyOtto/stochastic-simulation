C      Calculo da m‚dia
C      m = soma/dble(n) ou m = soma/dfloat(n)

       real*8 m,soma,val
       integer n
       
       soma = 0.d0
       
       write(*,8)
       read(*,9) n
       
       write(*,10)
       
       do i=1,n
          read(*,11) val
          soma = soma + val
       end do
       
       m = soma/dfloat(n)
       write(*,12) m,soma,n

8      format('Digite a quantidade de valores')
9      format(I3)
10     format('Digite os valores para a media')
11     format(f6.2)
12     format('Media:',f8.2,1x,'Soma:',f8.2,2x,'N:',i2)

       end
       