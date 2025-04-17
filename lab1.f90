program LinguisticChain
    implicit none
    character(len=100), parameter :: filename = 'B-C-D-E-F-Euro Stoxx 50 Historical Data.csv'
    character(len=1), dimension(10) :: alphabet = ['A','B','C','D','E','F','G','H','I','J']
    real, allocatable :: prices(:)
    character(len=1), allocatable :: letters(:)
    integer, allocatable :: matrix(:,:)
    integer :: i, j, n, K, idx
    real :: minVal, maxVal, interval
    character(len=200) :: line
    integer :: ios, unit
    real :: t1, t2, total

    K = 10  ! потужність алфавіту

    ! Підрахунок кількості рядків (пропускаємо заголовок)
    n = 0
    open(unit=10, file=filename, status='old')
    read(10,*)  ! пропустити заголовок
    call CPU_TIME(t1)
    do
        read(10,'(A)', iostat=ios) line
        if (ios /= 0) exit
        n = n + 1
    end do
    call CPU_TIME(t2)
    print *, 'Зчитування даних: ', t2 - t1, ' с'
    close(10)

    allocate(prices(n))
    allocate(letters(n))
    allocate(matrix(K,K))
    matrix = 0

    ! Зчитування цін
    open(unit=20, file=filename, status='old')
    read(20,*)  ! знову пропустити заголовок
    call CPU_TIME(t1)
    do i = 1, n
        read(20,'(A)', iostat=ios) line
        if (ios /= 0) exit
        read(line,*) line  ! прочитати рядок
        read(line(11:),*) prices(i)  ! витягти значення Price з позиції після дати
    end do
    call CPU_TIME(t2)
    print *, 'Зчитування даних: ', t2 - t1, ' с'
    close(20)

    ! Сортування
    call CPU_TIME(t1)
    call sort(prices, n)
    call CPU_TIME(t2)
    print *, 'Сортування: ', t2 - t1, ' с'

    ! Знайти мінімум і максимум
    minVal = prices(1)
    maxVal = prices(n)
    interval = (maxVal - minVal) / real(K)

    ! Призначення літер
    call CPU_TIME(t1)
    do i = 1, n
        idx = int((prices(i) - minVal) / interval) + 1
        if (idx > K) idx = K
        letters(i) = alphabet(idx)
    end do
    call CPU_TIME(t2)
    print *, 'Побудова ланцюжка: ', t2 - t1, ' с'

    ! Побудова матриці передування
    call CPU_TIME(t1)
    do i = 1, n - 1
        call getIndex(alphabet, K, letters(i), j)
        call getIndex(alphabet, K, letters(i+1), idx)
        matrix(j, idx) = matrix(j, idx) + 1
    end do
    call CPU_TIME(t2)
    print *, 'Матриця передування: ', t2 - t1, ' с'

    ! Вивід результатів
    print *, 'Лінгвістичний ряд:'
    do i = 1, n
        write(*,'(A1)', advance='no') letters(i)
    end do
    print *, ''
    print *, 'Матриця передування:'
    do i = 1, K
        write(*,'(A1,1X)', advance='no') alphabet(i)
        do j = 1, K
            write(*,'(I4)', advance='no') matrix(i,j)
        end do
        print *
    end do

contains

    subroutine sort(arr, n)
        real, intent(inout) :: arr(:)
        integer, intent(in) :: n
        integer :: i, j
        real :: temp
        do i = 1, n-1
            do j = i+1, n
                if (arr(i) > arr(j)) then
                    temp = arr(i)
                    arr(i) = arr(j)
                    arr(j) = temp
                end if
            end do
        end do
    end subroutine

    subroutine getIndex(alpha, K, ch, idx)
        character(len=1), intent(in) :: alpha(:)
        character(len=1), intent(in) :: ch
        integer, intent(in) :: K
        integer, intent(out) :: idx
        integer :: i
        idx = 1
        do i = 1, K
            if (alpha(i) == ch) then
                idx = i
                return
            end if
        end do
    end subroutine

end program LinguisticChain
