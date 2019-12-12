
! in fortran, arrays start at 1, unless explicitly set otherwise

module util
    implicit none
contains
    function GET_FILENAME() result(filename)
        character(len=32) :: filename
        if(command_argument_count() /= 1) then
            write(*,*) "no input filename!"
            stop
        end if
        call get_command_argument(1, filename)
        return 
    end function GET_FILENAME
end module util

module aoc
    implicit none
    
contains
! subroutine that executes the operation on intcode vector, taking opcode from position zero designated by 'start'
! no bound checks whatsoever
    subroutine DO_OP(intcode, start, done)
        integer, dimension(0:)::intcode
        integer:: start
        logical:: done
        integer:: opcode, val1,val2
        !'start' is the index of opcode (current 'zero position')

        ! positions given are relative to global zero position, not to 'current start', lol

        opcode = intcode(start)
        select case (opcode)
        case(1)
            val1 = intcode(intcode(start+1))
            val2 = intcode(intcode(start+2))
            intcode(intcode(start+3)) = val1 + val2
        case (2)
            val1 = intcode(intcode(start+1))
            val2 = intcode(intcode(start+2))
            intcode(intcode(start+3)) = val1 * val2
        case (99)
            done = .true.
        case default 
            write(*,*) "invalid opcode! ",opcode
            done = .true.
        end select

    end subroutine DO_OP
end module aoc

program day2part2
    use util
    use aoc
    implicit none
    character(len=32) :: filename
    integer:: error
    integer, dimension(0:300):: tmp_array = -1  ! hardcoded limit of intcode program size
    integer, dimension(:), allocatable :: intcode, tmpintcode
    integer:: i,num
    logical:: done = .false.
    integer:: noun,verb
    integer:: look_for = 19690720

    filename = GET_FILENAME()
    write(*,*) "reading " ,filename

    open(unit=66,file=filename,status="old",action="read",iostat=error)
    if(error .gt. 0) then
        close(66)
        write(*,*) "open error:", error
        stop
    end if

    write(*,*)

    ! this will read one line from file into array, splitting by comma by default
    read(66,*,iostat=error) tmp_array
    if(error .gt. 0) then
        close(66)
        write(*,*) "read error:", error
        stop
    end if

    close(66)

    ! find the count of elements
    ! when created, array was filled with '-1', which makes it much easier

    do i=0,300 ! or we could just use findloc()
        if (tmp_array(i) .eq. -1) then
                num=i
            exit
        end if
    end do
    

    !now allocate the proper sized array and copy the proper values from temporary array
    allocate(intcode(0:num-1))
    do i=0,num-1
        intcode(i) = tmp_array(i)
    end do

    write(*,'("input program (len:",I0,")")') num
    write(*,*)
    write(*,'(*(I0X))') intcode
    write(*,*)

    ! at this point we have intcode program in 'intcode' array with length stored in 'num'
    

    outer: do noun=0,99
        do verb=0,99
            ! make a fresh local copy
            tmpintcode = intcode
            ! set starting values
            tmpintcode(1) = noun
            tmpintcode(2) = verb
            i = 0
            done = .false.
            do
                call DO_OP(tmpintcode,i,done)
                i = i+4
                if(done) then
                    exit
                end if
            end do

            if(tmpintcode(0) == look_for) then
                exit outer
            end if
        end do
    end do outer

    write(*,*)
    write(*,'("noun= ",I0,X,"verb= ",I0)') noun , verb
    write(*,'("100 * noun + verb = ",I0)') 100 * noun + verb
    write(*,*)
    write(*,*) "program:"
    write(*,*)
    write(*,'(*(I0X))') tmpintcode

end program day2part2
