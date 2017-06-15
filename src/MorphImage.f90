PROGRAM MorphImage



IMPLICIT NONE

! ====================== Precision Options =========================== !
   INTEGER, PARAMETER :: sp   = SELECTED_REAL_KIND(6, 37)     ! 32-bit
   INTEGER, PARAMETER :: dp   = SELECTED_REAL_KIND(15, 307)   ! 64-bit
   INTEGER, PARAMETER :: qp   = SELECTED_REAL_KIND(33, 4931)  ! 128-bit
   INTEGER, PARAMETER :: prec = sp                            ! Specify the precision here
! ==================================================================== !
   LOGICAL, PARAMETER       :: doFileIO = .TRUE.
   INTEGER, PARAMETER       :: fUnit   = 102
   INTEGER, PARAMETER       :: ol      = 1
   INTEGER, PARAMETER       :: nSteps  = 1
   INTEGER, PARAMETER       :: nStepsPerDump = 200
   REAL(prec), PARAMETER    :: dFac    = 0.0002_prec
   REAL(prec), PARAMETER    :: Pfac    = 0.8_prec
   REAL(prec), PARAMETER    :: dt      = 0.5_prec
   CHARACTER(20), PARAMETER :: rgbfile = 'ODonnyBoy.RGB'

   INTEGER                 :: nX, nY, i, j, iT, jT
   REAL(prec), ALLOCATABLE :: img(:,:,:), tend(:,:,:) , u(:,:), v(:,:)
   REAL(prec)              :: dWeights(-ol:ol,-ol:ol)
#ifdef HAVE_CUDA
   REAL(prec), DEVICE, ALLOCATABLE :: img_dev(:,:,:), tend_dev(:,:,:) , u_dev(:,:), v_dev(:,:)
   REAL(prec), DEVICE              :: dWeights_dev(-ol:ol,-ol:ol)
#endif
   CHARACTER(5)            :: iterChar
   
      nX = 600
      nY = 600
   
      CALL SetupDWeights( dWeights )
      CALL MakeBWSlats( img, nX, nY )
      
      ALLOCATE( u(1-ol:nX+ol,1-ol:nY+ol), v(1-ol:nX+ol,1-ol:nY+ol) )
      
      CALL SetupVelocity( u, v, nX, nY )
      
      ! "Forward step" in time to successively blur and advect the image

#ifdef HAVE_CUDA
     ! Allocate device data and move the data onto the GPU
      ALLOCATE( u_dev(1-ol:nX+ol,1-ol:nY+ol), v_dev(1-ol:nX+ol,1-ol:nY+ol) )
      ALLOCATE( img_dev(1:3,1-ol:nX+ol,1-ol:nY+ol), tend_dev(1:3,1:nX,1:nY) )
      u_dev        = u 
      v_dev        = v 
      img_dev      = img 
      tend_dev     = tend 
      dWeights_dev = dWeights 
#else
      !$acc enter data pcopyin( img )
      !$acc enter data pcopyin( dWeights )
      !$acc enter data pcopyin( u )
      !$acc enter data pcopyin( v )
      !$acc enter data pcopyin( tend )
#endif
      DO iT = 1, nSteps
         DO jT = 1, nStepsPerDump
      
            tend = 0.0_prec ! Zero out the tendency
#ifdef HAVE_CUDA

#else 
            CALL UpdateHalos( img, nX, nY )
            
            CALL DiffusiveTendency( img, dWeights, tend, nX, nY )
            
            CALL AdvectiveTendency( img, u, v, tend, nX, nY )

            CALL UpdateSolution( img, tend, nX, nY )
#endif            
         ENDDO
         ! -------------- File I/O ----------------------------------- !
         !$acc update host( img )
         IF( doFileIO )THEN
            PRINT*, 'File I/O :', iT
            WRITE( iterChar, '(I5.5)' ) iT
            CALL WriteRGBFile( img, 'bwslats.'//iterChar//'.RGB', nX, nY )
         ENDIF
         ! ----------------------------------------------------------- !
         
      ENDDO

      !$acc exit data delete( tend )
      !$acc exit data delete( dWeights )
      !$acc exit data delete( img )
      !$acc exit data delete( u )
      !$acc exit data delete( v )

      DEALLOCATE( img, tend, u, v )


CONTAINS

   SUBROUTINE SetupDWeights( dWeights )
      IMPLICIT NONE
      REAL(prec), INTENT(inout) :: dWeights(-ol:ol,-ol:ol)
      ! Local
      REAL(prec) :: r, wsum
      INTEGER :: ii, jj
      
         wsum = 0.0_prec
         DO jj = -ol, ol
            DO ii = -ol, ol

               IF( .NOT.( ii==0 .AND. jj==0) )THEN
                  r = REAL( ii**2 + jj**2, prec )
                  dWeights(ii,jj) = exp( -r/(2.0_prec*0.25_prec) )
                  wsum = wsum + dWeights(ii,jj)
               ENDIF
               
            ENDDO
         ENDDO
         
         dWeights(0,0) = -wsum
         
      
   END SUBROUTINE SetupDWeights
!
   SUBROUTINE SetupVelocity( u, v, nX, nY )
      IMPLICIT NONE
      INTEGER, INTENT(in)       :: nX, nY
      REAL(prec), INTENT(inout) :: u(1-ol:nX+ol,1-ol:nX+ol)
      REAL(prec), INTENT(inout) :: v(1-ol:nX+ol,1-ol:nX+ol)
      ! Local
      REAL(prec) :: x, y, r1, r2
      INTEGER :: i, j
      
         DO j = 1-ol, nY+ol
         
            y = REAL(j,prec)/REAL(nY,prec)
            
            DO i = 1-ol, nX+ol

               x = REAL(i,prec)/REAL(nX,prec)
               r1 = (x-0.5_prec)**2 + (y-0.65_prec)**2
               r2 = (x-0.5_prec)**2 + (y-0.35_prec)**2
               u(i,j) = Pfac*( (y-0.65_prec)*exp( -r1/(2.0_prec*0.01_prec) ) -&
                               (y-0.35_prec)*exp( -r2/(2.0_prec*0.01_prec) ) )
                               
               v(i,j) = -Pfac*( (x-0.5_prec)*exp( -r1/(2.0_prec*0.01_prec) ) -&
                                (x-0.5_prec)*exp( -r2/(2.0_prec*0.01_prec) ) )
               
            ENDDO
         ENDDO
         
   END SUBROUTINE SetupVelocity
!
   SUBROUTINE ReadRGBFile( img, rgbFileName, nX, nY )
      IMPLICIT NONE
      REAL(prec), ALLOCATABLE, INTENT(inout) :: img(:,:,:)
      CHARACTER(*), INTENT(in)               :: rgbFileName
      INTEGER, INTENT(out)                   :: nX, nY
      ! Local
      INTEGER :: i, j
      
      OPEN( UNIT= fUnit, &
            FILE = TRIM(rgbFileName),&
            FORM = 'FORMATTED', &
            ACCESS = 'SEQUENTIAL', &
            STATUS = 'OLD', &
            ACTION = 'READ' )
            
      READ( fUnit, * ) nX, nY
      ALLOCATE( img(1:3,1-ol:nX+ol,1-ol:nY+ol), tend(1:3,1:nX,1:nY) )
      
      DO j = 1, nY
         DO i = 1, nX
            READ( fUnit, * )img(1:3,i,j)
         ENDDO
      ENDDO
      
      CLOSE( fUnit )
      
   END SUBROUTINE ReadRGBFile
!
   SUBROUTINE MakeBWSlats( img, nX, nY )
      IMPLICIT NONE
      REAL(prec), ALLOCATABLE, INTENT(inout) :: img(:,:,:)
      INTEGER, INTENT(in)                    :: nX, nY
      ! Local
      INTEGER :: i, j
      REAL(prec) :: flg
      

      ALLOCATE( img(1:3,1-ol:nX+ol,1-ol:nY+ol), tend(1:3,1:nX,1:nY) )
      
      DO j = 1, nY
         DO i = 1, nX
            flg = sin( 60.0_prec*3.141592653_prec*REAL((i-1),prec)/(REAL(nX,prec)) )
            img(1:3,i,j) = (SIGN( 1.0_prec, flg ) + 1.0_prec )/2.0_prec
         ENDDO
      ENDDO
      
      
   END SUBROUTINE MakeBWSlats
!
   SUBROUTINE WriteRGBFile( img, rgbFileName, nX, nY )
      IMPLICIT NONE
      INTEGER, INTENT(in)      :: nX, nY
      REAL(prec), INTENT(in)   :: img(1:3,1-ol:nX+ol,1-ol:nY-ol)
      CHARACTER(*), INTENT(in) :: rgbFileName
      ! Local
      INTEGER :: i, j
      
      OPEN( UNIT=fUnit, &
            FILE = TRIM(rgbFileName),&
            FORM = 'FORMATTED', &
            ACCESS = 'SEQUENTIAL', &
            STATUS = 'REPLACE', &
            ACTION = 'WRITE' )
            
      DO j = 1, nY
         DO i = 1, nX
            WRITE( fUnit, * )img(1:3,i,j)
         ENDDO
      ENDDO
      
      CLOSE( fUnit )
      
   END SUBROUTINE WriteRGBFile
!
   SUBROUTINE DiffusiveTendency( img, dWeights, tend, nX, nY )
      IMPLICIT NONE
      INTEGER, INTENT(in)       :: nX, nY
      REAL(prec), INTENT(in)    :: img(1:3,1-ol:nX+ol,1-ol:nY+ol)
      REAL(prec), INTENT(in)    :: dWeights(-ol:ol,-ol:ol)
      REAL(prec), INTENT(inout) :: tend(1:3,1:nX,1:nY)
      ! Local
      INTEGER :: i, j, k, ii, jj

         !$acc kernels present( img, dWeights, tend )
         DO j = 1, nY
            DO i = 1, nX
               
               tend(1,i,j) = 0.0_prec
               tend(2,i,j) = 0.0_prec
               tend(3,i,j) = 0.0_prec
               DO jj = -ol,ol
                  DO ii = -ol,ol
                     tend(1,i,j) = tend(1,i,j) + dFac*dWeights(ii,jj)*img(1,i-ii,j-jj)
                     tend(2,i,j) = tend(2,i,j) + dFac*dWeights(ii,jj)*img(2,i-ii,j-jj)
                     tend(3,i,j) = tend(3,i,j) + dFac*dWeights(ii,jj)*img(3,i-ii,j-jj)
                  ENDDO
               ENDDO
                  
            ENDDO
         ENDDO
         !$acc end kernels

         
   END SUBROUTINE DiffusiveTendency
!
   SUBROUTINE AdvectiveTendency( img, u, v, tend, nX, nY )
      IMPLICIT NONE
      INTEGER, INTENT(in)       :: nX, nY
      REAL(prec), INTENT(in)    :: img(1:3,1-ol:nX+ol,1-ol:nY+ol)
      REAL(prec), INTENT(in)    :: u(1-ol:nX+ol,1-ol:nY+ol)
      REAL(prec), INTENT(in)    :: v(1-ol:nX+ol,1-ol:nY+ol)
      REAL(prec), INTENT(inout) :: tend(1:3,1:nX,1:nY)
      ! Local
      INTEGER :: i, j, k, ii, jj
      REAL(prec) :: fs, fn, fw, fe
      
         !$acc kernels present( img, u, v, tend )
         DO j = 1, nY
            DO i = 1, nX
               
               DO k = 1, 3
               ! Do first order upwind flux
                  fs = 0.5_prec*( -v(i,j-1)*img(k,i,j-1) - v(i,j)*img(k,i,j) - &
                                  MAX( abs(v(i,j-1)), abs(v(i,j)) )*( img(k,i,j-1) - img(k,i,j) ) )
                                     
                  fn = 0.5_prec*( v(i,j+1)*img(k,i,j+1) + v(i,j)*img(k,i,j) - &
                                  MAX( abs(v(i,j+1)), abs(v(i,j)) )*( img(k,i,j+1) - img(k,i,j) ) )
                                     
                  fw = 0.5_prec*( -u(i-1,j)*img(k,i-1,j) - u(i,j)*img(k,i,j) - &
                                  MAX( abs(u(i-1,j)), abs(u(i,j)) )*( img(k,i-1,j) - img(k,i,j) ) )
                                     
                  fe = 0.5_prec*( u(i+1,j)*img(k,i+1,j) + u(i,j)*img(i,j,k) - &
                                  MAX( abs(u(i+1,j)), abs(u(i,j)) )*( img(k,i+1,j) - img(k,i,j) ) )
                                     
                  tend(k,i,j) = tend(k,i,j) - (fe+fw+fn+fs)
               ENDDO
                  
            ENDDO
         ENDDO
         !$acc end kernels
      
   END SUBROUTINE AdvectiveTendency
!
   SUBROUTINE UpdateSolution( img, tend, nX, nY )
      IMPLICIT NONE
      INTEGER, INTENT(in)       :: nX, nY
      REAL(prec), INTENT(inout) :: img(1:3,1-ol:nX+ol,1-ol:nY+ol)
      REAL(prec), INTENT(inout) :: tend(1:3,1:nX,1:nY)
      ! Local
      INTEGER :: i, j, k
      
         !$acc kernels present( img, tend )     
         DO j = 1, nY
            DO i = 1, nX
               img(1,i,j) = img(1,i,j) + dt*tend(1,i,j)
               img(2,i,j) = img(2,i,j) + dt*tend(2,i,j)
               img(3,i,j) = img(3,i,j) + dt*tend(3,i,j)
            ENDDO
         ENDDO
         !$acc end kernels
      
   END SUBROUTINE UpdateSolution
!
   SUBROUTINE UpdateHalos( img, nX, nY )
      IMPLICIT NONE
      INTEGER, INTENT(in)       :: nX, nY
      REAL(prec), INTENT(inout) :: img(1:3,1-ol:nX+ol,1-ol:nY+ol)
      ! Local
      INTEGER :: i, j, k

          !$acc kernels present( img ) 
            DO j = 1, nY
               img(1,0,j)    = 0.0_prec 
               img(1,nX+1,j) = 0.0_prec
               
               img(2,0,j)    = 0.0_prec 
               img(2,nX+1,j) = 0.0_prec
               
               img(3,0,j)    = 0.0_prec 
               img(3,nX+1,j) = 0.0_prec
            ENDDO

            DO i = 1, nX
               img(1,i,0)    = 0.0_prec 
               img(1,i,nY+1) = 0.0_prec
               
               img(2,i,0)    = 0.0_prec 
               img(2,i,nY+1) = 0.0_prec
               
               img(3,i,0)    = 0.0_prec 
               img(3,i,nY+1) = 0.0_prec
            ENDDO
          !$acc end kernels
            
   END SUBROUTINE UpdateHalos   
      
END PROGRAM MorphImage
