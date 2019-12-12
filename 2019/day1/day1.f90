
module calc
    implicit none
    
contains
    function CALCULATE_FUEL(mass) result(fuel)
        integer,intent(in):: mass
        integer:: fuel
        fuel = INT(mass/3.0) - 2
        return
    end function CALCULATE_FUEL
end module calc   

program day1
    use calc
    implicit none
    integer:: error, mass,fuel_sum, fuel_needed
    write(*,*) "start"
    write(*,*) ""

    open(unit=66,file="input.dat",status="old",action="read")
    do
        read(66,*,iostat=error) mass
        if(error .ne. 0) then
            close(66)
            exit
        else 
            do 
                fuel_needed = CALCULATE_FUEL(mass)
                write(*,fmt="(I6,X,I6)", advance='no') mass, fuel_needed
                if(fuel_needed .le. 0) then 
                    exit
                end if
                mass = fuel_needed
                fuel_sum = fuel_sum + fuel_needed
            end do
            write(*,*) ""
        end if
    end do

    write(*,*)
    write(*,*) "fuel sum = ", fuel_sum
    stop
end program day1