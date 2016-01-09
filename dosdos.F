#include "fintrf.h"
C======================================================================
#if 0
C
C     dosdos.F
C     .F file needs to be preprocessed to generate .for equivalent
C
#endif
C
C     dosdos.f
C
C     Computational function that takes a scalar and doubles it.

C     This is a MEX-file for MATLAB.
C     Copyright 1984-2011 The MathWorks, Inc.
C
C======================================================================
C     Gateway routine
      subroutine mexFunction(nlhs, plhs, nrhs, prhs)

C     Declarations
      implicit none

C     mexFunction arguments:
      mwPointer plhs(*), prhs(*)
      integer nlhs, nrhs

C     Function declarations:
      mwPointer mxGetPr
      mwPointer mxCreateDoubleMatrix
      integer mxIsNumeric
      mwPointer mxGetM, mxGetN

C     Pointers to input/output mxArrays:
      mwPointer x_ptr, y_ptr

C     Array information:
      mwPointer mrows, ncols
      mwSize size

C     Arguments for computational routine:
      real*8  x_input, y_output

C-----------------------------------------------------------------------
C     Check for proper number of arguments.
      if(nrhs .ne. 1) then
         call mexErrMsgIdAndTxt ('MATLAB:dosdos:nInput',
     +                           'One input required.')
      elseif(nlhs .gt. 1) then
         call mexErrMsgIdAndTxt ('MATLAB:dosdos:nOutput',
     +                           'Too many output arguments.')
      endif

C     Validate inputs
C     Check that the input is a number.
      if(mxIsNumeric(prhs(1)) .eq. 0) then
         call mexErrMsgIdAndTxt ('MATLAB:dosdos:NonNumeric',
     +                           'Input must be a number.')
      endif

C     Get the size of the input array.
      mrows = mxGetM(prhs(1))
      ncols = mxGetN(prhs(1))
      size = mrows*ncols

C     Create Fortran array from the input argument.
      x_ptr = mxGetPr(prhs(1))
      call mxCopyPtrToReal8(x_ptr,x_input,size)

C     Create matrix for the return argument.
      plhs(1) = mxCreateDoubleMatrix(mrows,ncols,0)
      y_ptr = mxGetPr(plhs(1))

C     Call the computational subroutine.
      call dosdos(y_output, x_input)

C     Load the data into y_ptr, which is the output to MATLAB.
      call mxCopyReal8ToPtr(y_output,y_ptr,size)

      return
      end

C-----------------------------------------------------------------------
C     Computational routine

      subroutine dosdos(y_output, x_input)
      use omp_lib
      real*8 x_input, y_output

      INTEGER NTHREADS, TID, OMP_GET_NUM_THREADS,
     +        OMP_GET_THREAD_NUM, omp_get_num_procs,omp_get_max_threads

      character*200 line
      integer*4 k, mexPrintf
      LOGICAL nodyn


C     Explicitly disable dynamic teams
      nodyn = .FALSE.
      CALL omp_set_dynamic(nodyn)
C     NTHREADS = 30
C     CALL OMP_SET_NUM_THREADS(nthreads)

       write(line,'(a,i0)')
     +  'num_procs = ', omp_get_num_procs()
       k=mexPrintf(line//achar(13))
       write(line,'(a,i0)')
     +  'max_threads = ',int(omp_get_max_threads())
       k=mexPrintf(line//achar(13))
       write(line,'(a,i0)')
C    +  'num_threads = ',int(omp_get_num_threads())
C      k=mexPrintf(line//achar(13))

C     Fork a team of threads giving them their own copies of variables
!$OMP PARALLEL PRIVATE(NTHREADS, TID) NUM_THREADS(50)
C     Obtain thread number
      TID = OMP_GET_THREAD_NUM()
      PRINT *, 'Hello World from thread = ', TID
      write(line,'(a,i0)') 'Hello World from thread = ', TID
      k=mexPrintf(line//achar(13))

C     Only master thread does this
C     IF (TID .EQ. 0) THEN
C       NTHREADS = OMP_GET_NUM_THREADS()
C       PRINT *, 'Number of threads = ', NTHREADS
C       write(line,'(a,i0)')
C    +  'Number of thread = ', NTHREADS
C       k=mexPrintf(line//achar(13))
       write(line,'(a,i0)')
     +  'num_procs = ', omp_get_num_procs()
       k=mexPrintf(line//achar(13))
       write(line,'(a,i0)')
     +  'max_threads = ',int(omp_get_max_threads())
       k=mexPrintf(line//achar(13))
       write(line,'(a,i0)')
     +  'num_threads = ',int(omp_get_num_threads())
       k=mexPrintf(line//achar(13))
C     END IF

C     All threads join master thread and disband
!$OMP END PARALLEL
C     NTHREADS = OMP_GET_NUM_THREADS()
      y_output = 1.0 * NTHREADS + 0.004
      return
      end
