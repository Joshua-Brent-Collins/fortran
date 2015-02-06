        PROGRAM names
        IMPLICIT NONE
        CHARACTER(LEN=20), ALLOCATABLE :: first(:)
        CHARACTER(LEN=30), ALLOCATABLE :: last(:)
        CHARACTER(LEN=20) :: fs
        CHARACTER(LEN=30) :: ls
        INTEGER :: pos, order , k , j=1 , l = 0
        CHARACTER(LEN=30) :: hold
        OPEN(UNIT=20,FILE='unsortednames.txt')

        DO WHILE (j.EQ.1) 
         read(20,*,END=17) fs, ls
         l = l + 1
         print *,fs,ls        
        ENDDO   
17      continue 
        close(20)        
        ALLOCATE(first(l),last(l))
        OPEN(UNIT=20,FILE='unsortednames.txt')
        DO pos = 1,l
         read(20,*,END=20) first(pos), last(pos)
         print *,first(pos),last(pos)        
        ENDDO
20      Continue
        close(20)
        print *, "SORTED LIST :"
        DO pos = 1,l
         DO order = 1,l
          IF(last(pos)<last(order)) THEN    
           hold=last(pos)
           last(pos)=last(order)
           last(order)=hold
           hold=first(pos)
           first(pos)=first(order)
           first(order)=hold
          ENDIF                 
         ENDDO
        ENDDO
        OPEN(UNIT=20,FILE='sortednames.txt')
        DO k = 1,l
        print *, first(k),last(k)
        write(20,*) first(k),last(k)
        ENDDO
        END
