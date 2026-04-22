program hello

  use mpi

  implicit none

  integer :: ierr, rank, size, len, namelen, i, N
  character*(MPI_MAX_PROCESSOR_NAME) :: procname
  double precision :: pival, val

  N = 840
  pival = 0.0

  call MPI_Init(ierr)

  call MPI_GET_PROCESSOR_NAME(procname, namelen, ierr)

  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)

  do i = 1, N
        val = (i - 0.5) / N
        pival = pival +  1.0 / (1.0 + val * val)
  end do

  pival = (4.0 / N) * pival

  write(*,*) 'Value for pi from rank: ', rank, 'of machine: ', procname(1:namelen), " : ", pival

  call MPI_Finalize(ierr)

end program hello
