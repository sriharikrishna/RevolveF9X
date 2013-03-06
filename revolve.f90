!> \mainpage
!! This is a Fortran9X adaptation of the functionality of Revolve; see Alg. 799 published as \cite Griewank2000ARA .
!! The interface of the routines differs from the cited revolve implementation
!! found in Adol-C and has been designed to be more in line with the
!! Fortran 9X language features.
!!
!! The implementation (written by J. Utke)  is contained in revolve.f90, the use is illustrated in
!! example.f90.
!!
!! The mercurial repository with the latest version can be found at:
!! <a href="http://mercurial.mcs.anl.gov/ad/RevolveF9X">http://mercurial.mcs.anl.gov/ad/RevolveF9X</a>
!!


!> the module containing the revolve implementation
!!
MODULE revolve
  IMPLICIT NONE

  PUBLIC :: rvInit, rvVerbose, rvNextAction, rvGuess, rvFactor, & 
rvStore, rvRestore, rvForward, rvFirstUTurn, rvUTurn, rvDone, rvError

  PRIVATE :: & 
ourSteps, ourACP, ourCStart, ourCEnd, ourVerbosity, &
ourNumFwd , ourNumInv, ourNumStore, ourRWCP, ourPrevCEnd, ourFirstUTurned, &  
chkRange, forwdCount

  !> store a checkpoint now
  !! equivalent to TAKESHOT in Alg. 799
  INTEGER, PARAMETER :: rvStore      =1 

  !> restore a checkpoint now
  !! equivalent to RESTORE in Alg. 799
  INTEGER, PARAMETER :: rvRestore    =2

  !> execute iteration(s) forward
  !! equivalent to ADVANCE in Alg. 799
  INTEGER, PARAMETER :: rvForward    =3

  !> tape this iteration (and leave to return later) and do the adjoint
  !! equivalent to FIRSTTURN in Alg. 799
    INTEGER, PARAMETER :: rvFirstUTurn =4

  !> tape this iteration and do its adjoint
  !! equivalent to YOUTURN in Alg. 799
  INTEGER, PARAMETER :: rvUTurn      =5

  !> we are done with adjoining the loop
  !! equivalent to the `terminate` enum value in Alg. 799
  INTEGER, PARAMETER :: rvDone       =6

  !> an error has occurred
  !! equivalent to the `error` enum value in Alg. 799;
  !! see also `errMsg` in \ref rvAction
  INTEGER, PARAMETER :: rvError      =7

  !> this encapsulates all the information needed to perfrom the correct action
  !! an instance is returned from \ref rvNextAction
  TYPE rvAction
     !> the action that is to be implemented, termination, or error;
     !! the value must be one of:
     !! `rvStore`, `rvRestore`, `rvForward`,
     !! `rvFirstUTurn`, `rvUTurn`, `rvDone`, `rvError`
     INTEGER :: actionFlag = 0

     !> assuming the loop iterations are in [0,ourSteps-1] and `currentIteration` variable is maintained,
     !! the interpretation is as follows based on the value of `actionFlag`:
     !! - `rvForward`: execute iterations as the loop: `do currentIteration, iteration-1`
     !! - `rvRestore`: set `currentIteration=iteration`
     !!
     !! for all other values of `actionFlag` the value of `iteration` is meaningless
     INTEGER :: iteration  = 0

     !> the checkpoint number to be stored to restored
     !! the value is meaninfull only if `actionFlag` is set to `rvStore` or `rvRestore`;
     !!
     !! This is approximately equivalent to `checks` in Alg. 799.
     INTEGER :: cpNum      = 0

     !> if an error has occurred `actionFlag` will be set to `rvError` and this will contain an error message
     CHARACTER, dimension(80) :: errorMsg 
  END TYPE rvAction
  
  !> the number of iteration steps; set by calling \ref rvInit; not supposed to be set/used directly by the user
  !! note that the iterations are expected to range in [0, ourSteps-1];
  !!
  !! equivalent to `steps` in Alg. 799
  INTEGER :: ourSteps    = 0 ! number of steps

  !> the number of checkpoints (ACP=AllowedCheckPoints) that can be stored at any time during the loop execution
  !! set by calling \ref rvInit; not supposed to be set/used directly by the user
  !!
  !! equivalent to `snaps` in Alg. 799
  INTEGER :: ourACP      = 0

  !> current subrange start;
  !! not to be set/referemced directly by the user
  !!
  !! approximately equivalent to `capo` in Alg. 799
  INTEGER :: ourCStart   = 0

  !> current subrange end;
  !! not to be set/referemced directly by the user
  !!
  !! approximately equivalent to `fine` in Alg. 799
  INTEGER :: ourCEnd     = 0

  !> count of the forward steps; diagnostic only
  INTEGER :: ourNumFwd   = 0

  !> count of invocations to \ref rvNextAction ;  diagnostic only
  INTEGER :: ourNumInv   = 0

  !> count of checkpoint stores; diagnostic only
  INTEGER :: ourNumStore = 0

  !> checkpoint currently (re)stored - the first checkpoint is numbered 0;
  !! not to be set/referemced directly by the user
  INTEGER :: ourRWCP     = -1

  !> previous subrange end;
  !! not to be set/referemced directly by the user
  INTEGER :: ourPrevCEnd = 0

  !> have we first uturned already?;
  !! not to be set/referemced directly by the user
  LOGICAL :: ourFirstUturned = .FALSE.

  !> vector of step numbers indexed by checkpoint;
  !! not to be set/referemced directly by the user
  INTEGER, DIMENSION(:), ALLOCATABLE :: ourStepOf

  !> for debugging purposes; values imply:
  !! - 0 includes errors
  !! - 1 includes summary info
  !! - 2 includes iterations with checkpoints stored
  !! - 3 includes all action results
  !!
  !! set via \ref rvVerbose
  INTEGER :: ourVerbosity = 0

CONTAINS

!--------------------------------------------------------------------*

  !> method to initialize the internal state; must be called before any call to \ref rvNextAction
  !! @param steps  the total number of steps in the iteration; equivalent to `steps` in Alg. 799
  !! @param checkpoints the total number of checkpoints allowed to be stored at any time; equivalent to `snaps` in Alg. 799
  !! @param errorMsg set when an error condition occurs; else set to `"none"`
  !! @param anActionInstance  if supplied initializes its contents
  !! @return `.true.` if successfull, else `.false.` ansd `errorMsg` will be set
  FUNCTION rvInit(steps,checkpoints,errorMsg,anActionInstance)
    IMPLICIT NONE
    LOGICAL :: rvInit
    INTEGER, INTENT(IN) :: steps
    INTEGER, INTENT(IN) :: checkpoints
    CHARACTER ,dimension(:), INTENT(OUT) :: errorMsg
    type(rvAction), optional :: anActionInstance
    INTEGER :: predFwdCnt ! predicted forward count 
    rvInit = .TRUE.
    errorMsg ='none'
    IF (present(anActionInstance)) THEN
       ! same as default init above
       anActionInstance%actionFlag = 0
       anActionInstance%iteration  = 0
       anActionInstance%cpNum      = 0
    END IF
    IF (steps<0 .OR. checkpoints<0) THEN
       rvInit=.FALSE.
       errorMsg = 'revolve::rvInit: negative steps or checkpoints'
    ELSE 
       ourCStart       = 0
       ourSteps        = steps
       ourCEnd         = steps
       ourACP          = checkpoints
       ourNumFwd       = 0 
       ourNumInv       = 0 
       ourNumStore     = 0 
       ourRWCP         = -1 
       ourPrevCEnd     = 0 
       ourFirstUTurned = .FALSE.

       IF (ALLOCATED(ourStepOf)) THEN
          DEALLOCATE(ourStepOf)
       END IF
       IF(.NOT.ALLOCATED(ourStepOf)) THEN
          ALLOCATE(ourStepOf(0:ourACP))
       END IF

       IF (ourVerbosity>0) THEN
          predFwdCnt = forwdCount(ourCEnd-ourCStart,ourACP)
          IF (predFwdCnt==-1) THEN
             errorMsg='error in forwdCount'
             rvInit=.FALSE.
             RETURN
          ELSE
             WRITE (*,'(A)') 'prediction:'
             WRITE (*,'(A,I7)') ' needed forward steps: ', predFwdCnt
             WRITE (*,'(A,F8.4)') ' slowdown factor     : ', dble(predFwdCnt)/(ourCEnd-ourCStart)
          END IF
       END IF
    END IF
  END FUNCTION rvInit

!--------------------------------------------------------------------*

  !> method to set the verbosity to a level in [0-3] as described for `ourVerbosity`
  SUBROUTINE rvVerbose(level)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: level 
    ourVerbosity=level
  END SUBROUTINE rvVerbose

!--------------------------------------------------------------------*
  !> the method to determine the next action; to be called in an unbound loop after \ref rvInit
  !! @return an instance of `rvAction` set to describe the next action (see the member documentation);
  !!
  !! this method modifies the internal state; it is approximately equivalent to the method `revolve` in Alg. 799
  FUNCTION rvNextAction()
    IMPLICIT NONE
    REAL :: bino1, bino2, bino3, bino4, bino5

    !> available checkpoint slots
    INTEGER :: availCP

    !> local copy of previous subrange start
    INTEGER :: prevCStart

    INTEGER :: range
    INTEGER :: reps
    INTEGER :: i 
    type(rvAction) :: rvNextAction
    IF (ourNumInv==0) THEN
       ! first invocation
       DO i = 0, ourACP
          ourStepOf(i) = 0
       END DO
       ourStepOf(0) = ourCStart - 1
    END IF
    ourNumInv = ourNumInv + 1
    IF ((ourCEnd-ourCStart)==0) THEN
       ! nothing in current subrange
       IF ((ourRWCP==(-1)) .OR. (ourCStart==ourStepOf(0))) THEN
          ! we are done
          ourRWCP = ourRWCP - 1
          IF (ourVerbosity>0) THEN
             WRITE (*,'(A)') 'summary:'
             WRITE (*,'(A,I8)') ' forward steps:', ourNumFwd
             WRITE (*,'(A,I8)') ' CP stores    :', ourNumStore
             WRITE (*,'(A,I8)') ' invocations  :', ourNumInv
          END IF
          rvNextAction%actionFlag = rvDone
        ELSE
           ourCStart = ourStepOf(ourRWCP)
           ourPrevCEnd = ourCEnd
           rvNextAction%actionFlag = rvRestore 
        END IF
     ELSE IF ((ourCEnd-ourCStart)==1) THEN
        ourCEnd = ourCEnd - 1
        ourPrevCEnd = ourCEnd
        IF ((ourRWCP>=0) .AND. (ourStepOf(ourRWCP)==ourCStart)) ourRWCP = ourRWCP - 1
        IF (.NOT.ourFirstUTurned) THEN
           rvNextAction%actionFlag = rvFirstUTurn
           ourFirstUTurned = .TRUE.
        ELSE
           rvNextAction%actionFlag = rvUTurn
        END IF
     ELSE IF ((ourRWCP==(-1)) .OR. (ourStepOf(ourRWCP)/=ourCStart)) THEN
        ourRWCP = ourRWCP + 1
        IF (ourRWCP+1>ourACP) THEN
           rvNextAction%actionFlag = rvError
           rvNextAction%errorMsg='insufficient allowed checkpoints'
           RETURN
        ELSE
           ourStepOf(ourRWCP) = ourCStart
           ourNumStore = ourNumStore + 1
           ourPrevCEnd = ourCEnd
           rvNextAction%actionFlag = rvStore
        END IF
     ELSE IF ((ourPrevCEnd<ourCEnd) .AND. (ourACP==ourRWCP+1)) THEN
        rvNextAction%actionFlag = rvError
        rvNextAction%errorMsg='insufficient allowed checkpoints'
     ELSE
        prevCStart = ourCStart
        availCP = ourACP - ourRWCP
        IF (availCP<1) THEN
           rvNextAction%actionFlag = rvError
           rvNextAction%errorMsg='insufficient allowed checkpoints'
        ELSE
           reps = 0
           range = 1
           DO WHILE (range<ourCEnd-ourCStart)
              reps = reps + 1
              range = range*(reps+availCP)/reps
           END DO
           bino1 = range*reps/(availCP+reps)
           IF (availCP>1) THEN
            bino2 = bino1*availCP/(availCP+reps-1)
          ELSE
            bino2 = 1
          END IF
          IF (availCP==1) THEN
            bino3 = 0
          ELSE IF (availCP>2) THEN
            bino3 = bino2*(availCP-1)/(availCP+reps-2)
          ELSE
            bino3 = 1
          END IF
          bino4 = bino2*(reps-1)/availCP
          IF (availCP<3) THEN
            bino5 = 0
          ELSE IF (availCP>3) THEN
            bino5 = bino3*(availCP-1)/reps
          ELSE
            bino5 = 1
          END IF
          IF (ourCEnd-ourCStart<=bino1+bino3) THEN
            ourCStart = ourCStart + bino4
          ELSE IF (ourCEnd-ourCStart>=range-bino5) THEN
            ourCStart = ourCStart + bino1
          ELSE
            ourCStart = ourCEnd - bino2 - bino3
          END IF
          IF (ourCStart==prevCStart) THEN
            ourCStart = prevCStart + 1
          END IF
          ourNumFwd = ourNumFwd + ourCStart - prevCStart
          rvNextAction%actionFlag = rvForward
        END IF
      END IF
      rvNextAction%iteration=ourCStart 
      IF (rvNextAction%actionFlag /= rvError .AND. rvNextAction%actionFlag /= rvDone) THEN
         IF (ourVerbosity>2) THEN
            SELECT CASE( rvNextAction%actionFlag)
            CASE (rvForward)
               WRITE (*,FMT='(A)',ADVANCE='NO') ' forward to  :'
            CASE (rvRestore)
               WRITE (*,FMT='(A)',ADVANCE='NO') ' restore at  :'
            CASE (rvFirstUTurn)
               WRITE (*,FMT='(A)',ADVANCE='NO') ' 1st uturn at:'
            CASE(rvUTurn)      
               WRITE (*,FMT='(A)',ADVANCE='NO') ' uturn at    :'
            END SELECT
         END IF
         IF (ourVerbosity>1) THEN
            IF (rvNextAction%actionFlag == rvStore) THEN 
               WRITE (*,FMT='(A)',ADVANCE='NO') ' store at    :'
            END IF
            WRITE (*,'(I8)') rvNextAction%iteration
         END IF
      END IF
      rvNextAction%cpNum=ourRWCP
    END FUNCTION rvNextAction

!--------------------------------------------------------------------*
    !> estimates the number of checkpoints required; equivalent to `adjust` in Alg. 799
    !! @param steps is the number of iterations
    !! @return the number of checkpoints such that the growth in spatial complexity is balanced with the  growth in temporal complexity
    !!
    !! this method does not change the internal state and does not require \ref rvInit
    FUNCTION rvGuess(steps)
    IMPLICIT NONE
      INTEGER :: steps
      INTEGER :: reps, s, checkpoints
      INTEGER :: rvGuess
      checkpoints = 1
      reps = 1
      s = 0
      DO WHILE (chkRange(checkpoints+s,reps+s)>steps)
        s = s - 1
      END DO
      DO WHILE (chkRange(checkpoints+s,reps+s)<steps)
        s = s + 1
      END DO
      checkpoints = checkpoints + s
      reps = reps + s
      s = -1
      DO WHILE (chkRange(checkpoints,reps)>=steps)
        IF (checkpoints>reps) THEN
          checkpoints = checkpoints - 1
          s = 0
        ELSE
          reps = reps - 1
          s = 1
        END IF
      END DO
      IF (s==0) THEN
        checkpoints = checkpoints + 1
      END IF
      IF (s==1) reps = reps + 1
      rvGuess = checkpoints
    END FUNCTION rvGuess

!--------------------------------------------------------------------*
    !> computes the run time overhead factor; equivalent to `expense` in Alg. 799
    !! @param steps is the number of iterations
    !! @param checkpoints is the number of allowed checkpoints
    !! @return the estimated runtime overhead factor (does not account for time to write checkpoints
    !!
    !! this method does not change the internal state and does not require \ref rvInit
    FUNCTION rvFactor(steps,checkpoints)
    IMPLICIT NONE
      INTEGER :: checkpoints, steps
      DOUBLE PRECISION :: rvFactor
      IF (checkpoints<1) THEN
        WRITE (*,fmt=*) 'error occurs in RVFACTOR: CHECKPOINTS < 1'
        rvFactor = -1
      ELSE IF (checkpoints<1) THEN
        WRITE (*,fmt=*) 'error occurs in RVFACTOR: CHECKPOINTS < 1'
        rvFactor = -1
      ELSE
        rvFactor = dble(forwdCount(steps,checkpoints))
        IF (rvFactor/=-1) rvFactor = rvFactor/steps
      END IF
    END FUNCTION rvFactor

!--------------------------------------------------------------------*
    !> internal method not to be referenced by the user
    FUNCTION chkRange(ss,tt)
    IMPLICIT NONE
      INTEGER :: ss, tt
      DOUBLE PRECISION :: res
      INTEGER :: i
      INTEGER :: chkRange
      res = 1.
      IF (tt<0 .OR. ss<0) THEN
        WRITE (*,fmt=*) 'error in MAXRANGE: negative parameter '
        chkRange = -1
      ELSE
        DO i = 1, tt
          res = res*(ss+i)
          res = res/i
          IF (res>=2.0D0**31) EXIT
        END DO
        IF (res<2.0D0**31-2) THEN
          chkRange = res
        ELSE
          chkRange = 2.0D0**31 - 3
          WRITE (*,fmt=*) 'warning from  MAXRANGE: returned maximal integer'
          WRITE (*,fmt=*) chkRange
        END IF
      END IF
    END FUNCTION chkRange

!--------------------------------------------------------------------*

    !> internal method not to be referenced by the user
    FUNCTION forwdCount(steps,checkpoints)
    IMPLICIT NONE
      INTEGER :: checkpoints, steps
      INTEGER :: range, reps
      INTEGER :: forwdCount
      IF (checkpoints<1) THEN
        forwdCount = -1
      ELSE
        reps = 0
        range = 1
        DO WHILE (range<steps)
          reps = reps + 1
          range = range*(reps+checkpoints)/reps
        END DO
        forwdCount = reps*steps - range*reps/(checkpoints+1)
      END IF
    END FUNCTION forwdCount

!--------------------------------------------------------------------*

END MODULE revolve
