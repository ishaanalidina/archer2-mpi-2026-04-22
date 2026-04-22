program hello

  use mpi

  implicit none

  integer :: ierr, rank, size, len, namelen, i, N, startidx, endidx
  character*(MPI_MAX_PROCESSOR_NAME) :: procname
  double precision :: pival, val, pival_temp
  integer :: status(MPI_STATUS_SIZE)

  N = 840
  pival = 0.0

  call MPI_Init(ierr)

  call MPI_GET_PROCESSOR_NAME(procname, namelen, ierr)

  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)

  call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)

  startidx = rank * N / size
  endidx = (rank + 1) * N / size

  do i = startidx + 1, endidx
        val = (i - 0.5) / N
        pival = pival +  1.0 / (1.0 + val * val)
  end do

  pival = (4.0 / N) * pival

  write(*,*) 'Contribution to pi calculated from rank: ', rank, 'of machine: ', procname(1:namelen), " : ", pival

if (size .gt. 1) then
  if (rank .eq. 0) then
        do i = 1, size - 1
        pival_temp = pival
        call MPI_Recv(pival, 1, MPI_DOUBLE_PRECISION, MPI_ANY_SOURCE, 0, MPI_COMM_WORLD, status, ierr)
        pival = pival_temp + pival        
        end do
  else
        call MPI_Ssend(pival, 1, MPI_DOUBLE_PRECISION, 0, 0, MPI_COMM_WORLD, ierr)
  end if
end if

if (rank .eq. 0) then
  write(*, *) "Evaluation of pi on rank 0: ", pival
end if

  call MPI_Finalize(ierr)

end program hello
