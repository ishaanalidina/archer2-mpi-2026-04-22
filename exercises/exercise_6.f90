program paralleldatarotation

use mpi

implicit none

integer :: ierror, rank, len, size, tempval, globalsum, totalsteps, status(MPI_STATUS_SIZE), request, neighbour_back, neighbour_forward, idx, tag, comm
double precision :: tstart, tend

! Initialising MPI, and getting the number of processes and the process ID of each rank.

comm = MPI_COMM_WORLD
tag = 0

call MPI_INIT(ierror)

call MPI_COMM_SIZE(comm, size, ierror)

call MPI_COMM_RANK(comm, rank, ierror)

call MPI_Barrier(comm, ierror)
tstart = MPI_Wtime()

totalsteps = size - 1
tempval = rank
! tempval = (rank + 1) ** 2

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

! Using a global sum reduction to generate the final global sum.

call MPI_Allreduce(tempval, globalsum, 1, MPI_INTEGER, MPI_SUM, comm, ierror)


call MPI_Barrier(comm, ierror)
tend = MPI_Wtime()

write(*, *) "Global sum from rank: ", rank, " is: ", globalsum

if (rank .eq. 0) then
    
    write(*, *) "Time required: ", tend - tstart

end if

end program
