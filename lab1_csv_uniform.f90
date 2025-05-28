program linguistic_chain
    implicit none
    integer, parameter :: max_n = 1000, max_alpha = 26
    real, dimension(max_n) :: series, series_copy
    character(len=1), dimension(max_n) :: linguistic_series
    character(len=1), dimension(max_alpha) :: alphabet
    integer :: n, i, j, alpha_size, ios, current, next
    real :: min_val, max_val, interval
    integer, dimension(max_alpha, max_alpha) :: transition_matrix
    character(len=200) :: line
    character(len=100) :: filename
    integer :: unit = 10
    character(len=20) :: price_str
    real :: value

    print *, "Введіть назву CSV-файлу (наприклад, data.csv):"
    read(*,'(A)') filename

    open(unit=unit, file=filename, status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, "Помилка при відкритті файлу."
        stop
    end if

    ! Пропустити заголовок
    read(unit, '(A)') line

    n = 0
    do
        read(unit, '(A)', iostat=ios) line
        if (ios /= 0) exit

        call extract_field(line, 2, price_str)
        read(price_str, *, iostat=ios) value
        if (ios /= 0) cycle

        n = n + 1
        if (n > max_n) then
            print *, "Перевищено максимальний розмір масиву"
            exit
        end if

        series(n) = value
    end do
    close(unit)

    if (n == 0) then
        print *, "Файл порожній або не містить значень."
        stop
    end if

    ! 2. Введення алфавіту
    print *, "Введіть потужність алфавіту (не більше", max_alpha, "):"
    read *, alpha_size
    if (alpha_size > max_alpha .or. alpha_size < 1) stop "Невірна потужність алфавіту"

    do i = 1, alpha_size
        alphabet(i) = char(64 + i)
    end do

    ! 3. Копіюємо масив для сортування (щоб зберегти оригінал)
    series_copy(1:n) = series(1:n)
    call sort_array(series_copy, n)
    min_val = series_copy(1)
    max_val = series_copy(n)
    interval = (max_val - min_val) / alpha_size

    ! 4. Перетворення в лінгвістичний ряд (по оригінальному порядку)
    do i = 1, n
        linguistic_series(i) = alphabet(alpha_size)  ! за замовчуванням остання літера
        do j = 1, alpha_size
            if (series(i) <= min_val + j * interval) then
                linguistic_series(i) = alphabet(j)
                exit
            end if
        end do
    end do

    ! 5. Вивід лінгвістичного ряду
    print *, "Лінгвістичний ряд:"
    do i = 1, n
        write(*, "(A1)", advance='no') linguistic_series(i)
    end do
    print *

    ! 6. Матриця передування
    transition_matrix = 0
    do i = 1, n - 1
        current = index_in_alphabet(linguistic_series(i), alphabet, alpha_size)
        next = index_in_alphabet(linguistic_series(i+1), alphabet, alpha_size)
        if (current > 0 .and. next > 0) then
            transition_matrix(current, next) = transition_matrix(current, next) + 1
        end if
    end do

    ! 7. Вивід матриці передування
    print *, "Матриця передування:"
    write(*,'(A3)', advance='no') "   "
    do i = 1, alpha_size
        write(*,'(A3)', advance='no') alphabet(i)
    end do
    print *

    do i = 1, alpha_size
        write(*,'(A3)', advance='no') alphabet(i)
        do j = 1, alpha_size
            write(*,'(I3)', advance='no') transition_matrix(i,j)
        end do
        print *
    end do

contains

    subroutine sort_array(arr, n)
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
    end subroutine sort_array

    integer function index_in_alphabet(symbol, alphabet, size)
        character(len=1), intent(in) :: symbol
        character(len=1), dimension(size), intent(in) :: alphabet
        integer, intent(in) :: size
        integer :: k

        index_in_alphabet = -1
        do k = 1, size
            if (alphabet(k) == symbol) then
                index_in_alphabet = k
                return
            end if
        end do
    end function index_in_alphabet

    subroutine extract_field(line, field_num, result)
        character(len=*), intent(in) :: line
        integer, intent(in) :: field_num
        character(len=*), intent(out) :: result
        integer :: i, count, start, endp

        count = 0
        start = 1
        endp = len_trim(line)

        do i = 1, len_trim(line)
            if (line(i:i) == ',') then
                count = count + 1
                if (count == field_num - 1) start = i + 1
                if (count == field_num) then
                    endp = i - 1
                    exit
                end if
            end if
        end do

        if (count < field_num - 1) then
            result = ''
        else
            result = adjustl(line(start:endp))
            if (len_trim(result) > 0) then
                if (result(1:1) == '"') result = result(2:)
                do i = len_trim(result), 1, -1
                    if (result(i:i) == '"') then
                        result(i:) = ' '
                        exit
                    end if
                end do
                result = adjustl(result)
            end if
        end if
    end subroutine extract_field

end program linguistic_chain
