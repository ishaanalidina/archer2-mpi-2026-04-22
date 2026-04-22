program hello

  use mpi

  implicit none

  integer :: ierr, rank, size, len, namelen
  character*(MPI_MAX_PROCESSOR_NAME) :: procname

  call MPI_Init(ierr)

  call MPI_GET_PROCESSOR_NAME(procname, namelen, ierr)

  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)

  write(*,*) 'Hello World from rank: ', rank, 'of machine: ', procname(1:namelen)

  call MPI_Finalize(ierr)

end program hello
