program paralleldatarotation

use mpi

implicit none

integer :: ierror, rank, len, size, tempval, globalsum, totalsteps, status(MPI_STATUS_SIZE), request, neighbour_back, neighbour_forward, idx, tag, comm

! Initialising MPI, and getting the number of processes and the process ID of each rank.

comm = MPI_COMM_WORLD
tag = 0

call MPI_INIT(ierror)

call MPI_COMM_SIZE(comm, size, ierror)

call MPI_COMM_RANK(comm, rank, ierror)

totalsteps = size - 1
tempval = rank
globalsum = rank

! Defining the neighbours to each rank, as these should remain unchanged throughout the operation.

if (rank .eq. 0) then
    neighbour_back = size - 1
else 
    neighbour_back = rank - 1
end if

if (rank .eq. size - 1) then
    neighbour_forward = 0
else
    neighbour_forward = rank + 1
end if

! Using the method with a non-blocked send to the forward neighbour and a blocked receive from the backward neighbour.

do idx = 1, totalsteps

    call MPI_Issend(tempval, 1, MPI_INTEGER, neighbour_forward, tag, comm, request, ierror)

    call MPI_Recv(tempval, 1, MPI_INTEGER, neighbour_back, tag, comm, ierror)

    call MPI_Wait(request, status, ierror)

    globalsum = globalsum + tempval

end do

write(*, *) "Global sum from rank: ", rank, " is: ", globalsum

end program
