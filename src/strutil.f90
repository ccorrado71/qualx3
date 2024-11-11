MODULE STRUTIL
!
!                                    - List of procedures -
!
!  S filtra(string)                                     ! Remove empty space
!  S Cutst(line1,nlong1,line2,nlong2)                   ! Removes the first word of the input String.
!  S Cutsta(line1,nlong1,line2,nlong2)                  ! Removes the first word of the input String. (NEW allocatable)
!  S Getnum(line,vet,ivet,iv)                           ! Converts a string to numbers and write on VET/IVET if real/integer.
!  S Getnum1(line,vet,ivet,iv)                          ! Converts a string to numbers and write on VET/IVET if real/integer (NEW with allocation)
!  S get_words(line,wordv,nword)                        ! Estrai da line tutte le stringhe separate da spazi in wordv
!  S get_words1(line,wordv,nword)                       ! Estrai da line tutte le stringhe separate da spazi in wordv (NEW without allocation)
!  S get_words_quotes(line,wordv,nword)                 ! Estrai da line solo le stringhe in parentesi
!  S get_words_quotes_a(line,wordv,nword,btype)         ! allocatable version of get_words_quotes
!  S get_words_quotes1(line,wordv,nword)                ! Estrai da line le stringhe in parentesi e non
!  S parse_line_reals(line,posv,rnum,ier)               ! Get real number from string in position pos. ier > 0 contains position
!  S get_next_number(string,pos,numb,ier)               ! Get the next number starting from position pos
!  F U_Case(Text)                                       ! Conversion to upper case, text is not modified
!  F L_Case(Text)                                       ! Conversion to lower case, text is not modified
!  S Init_Err_String()                                  ! Initializes general error variables
!  F Lung (Stringa)                                     ! Lenght of stringa
!  F remc0(s0)                                          ! Removes char(0) from the end of s0
!  S s_trim_char ( str , c)                             ! Removes trailing c from a string
!  F add_c0(str,pos)                                    ! Add char(0) non oltre il carattere pos
!  S s_cat0(s1,s2)                                      ! Joins s1 and s2 separated with blank. Output in s1
!  S s_catend(s1,s2)                                    ! Joins s1 and s2 plus char(13)//char(10) for end of row. Output in s1
!  S sort_heap_external ( n, indx, i, j, isgn )         ! Externally sorts a list of items into ascending order.
!  S svec_sort_heap_a ( n, a )                          ! Ascending sorts a vector of character strings using heap sort.
!  S svec_sort_heap_a_index ( n, sarray, indx )         ! Does a case-sensitive indexed heap sort of a vector of strings
!  S s_rep_ch ( s, c1, c2 )                             ! Replaces all occurrences of one character by another
!  S s_rep ( s, sub1, sub2, irep )                      ! Replaces all occurrences of SUB1 by SUB2 in a string.
!  F s_replace(s, sub1, sub2)                           ! Replaces all occurrences of SUB1 by SUB2 in a string.
!  F s_replace_by_blanks(s, sub1)                       ! Replaces all occurrences of SUB1 by blanks
!  F s_u2b ( s )                                        ! Replaces underscores by blanks.
!  S s_filter(str)                                      ! Filtra una stringa
!  S s_chop ( s, ilo, ihi )                             ! "Chops out" a portion of a string, and closes up the hole
!  F string_from_time(time)  result(stime)              ! Converts time in string. Time is expressed in seconds
!  S s_s_insert ( s, ipos, s2 )                         ! Inserts a substring into a string.
!  S s_set_delete ( s, s2 )                             ! Removes any characters in one string from another string.  
!  S s_swap ( s1, s2 )                                  ! Swaps two strings.
!  S s_tab_blank ( s )                                  ! replaces each TAB character by one space.
!  S s_tab_blanks                                       ! replaces TAB characters by 6 spaces.
!  S s_ch_blank ( s, c )                                ! Replaces each occurrence of a particular character by a blank.
!  F s_blank_delete ( s )                               ! Removes blanks from a string, left justifying the remainder.
!  S s_blanks_delete ( s )                              ! Replaces consecutive blanks by one blank.
!  S s_blanks_insert ( s, ilo, ihi )                    ! Inserts blanks into a string, sliding old characters over
!  S s_s_delete ( s, sub, irep )                        ! Removes all occurrences of a substring from a string
!  F s_delete ( s, sub)                                 ! Removes all occurrences of a substring from a string.
!  F upper ( s )                                        ! Returns an uppercase version of a string
!  F lower ( s )                                        ! Returns a lowercase version of a string.
!  F len_noctrl ( s )                                   ! returns the length of a string up to the last non-control and blank character.
!  F s_cap_word ( s )                                   ! Capitalizes the first character in a word s
!  F s_to_r                                             ! reads a real number from a string
!  F s_to_r_perc(str,rnum)                              !  Convert percentage in fraction. If '%' is absent string is read as fraction
!  S s_to_i                                             ! reads a integer number from a string
!  S ch_eqi                                             ! is a case insensitive comparison of two characters for equality
!  F s_eqi ( s1, s2 )                                   ! is a case insensitive comparison of two strings for equality.  
!  F s_eqidb ( s1, s2 )                                 ! compares two strings, ignoring case and blanks.
!  F match_word(string,word) result(match)              ! true if the first characters of string match with word (case insensitive)
!  F word_is_contained(smallword,bigword,minc)          ! true if almost minc character of smallword are contained in bigword
!  S ch_cap                                             ! capitalizes a single character
!  S ch_to_digit                                        ! returns the integer value of a base 10 digit.
!  F centra_str(string,n)                               ! Centra una stringa in un campo di ampiezza n
!  F catstringpar(string,par,fmtt)                      ! Concatena una stringa con un reale in formato fmtt
!  F number_to_format(vet)                              ! genera un formato fortran da un numero o vettore di numeri
!  F string_locate(str,strvet,vet)                      ! Cerca la prima posizione di str nel vettore strvet
!  S string_locate_all(str,strvet,vet,n)                ! Find all occurences of str in array strvet
!  F ch_is_alpha                                        ! returns TRUE if C is an alphabetic character.
!  F ch_is_control ( c )                                ! is TRUE if C is a control character.
!  F ch_is_digit ( c )                                  ! is TRUE if C is a decimal digit
!  F ch_is_upper ( c )                                  ! is TRUE if C is an upper case letter.
!  F ch_is_lower ( c )                                  ! is TRUE if C is a lower case letter.
!  F ch_is_space ( c )                                  ! is TRUE if C is a whitespace character.
!  F s_is_alpha                                         ! returns TRUE if the string contains only alphabetic characters.
!  F s_is_alphanumeric ( s )                            ! returns TRUE if the string contains only alphanumeric characters.
!  F s_is_i ( s, i )                                    ! is TRUE if a string represents an integer.
!  S s_is_r                                             ! returns TRUE if the string represents a real number
!  F s_is_digit ( s )                                   ! returns TRUE if a string contains only decimal digits.
!  S s_detag ( s )                                      ! removes from a string all substrings marked by brackets.
!  S s_detags ( s )                                     ! removes from a string all substrings marked by square brackets.
!  F string_esd(val,std)  result(str)                   ! create string: val(std)
!  F first_digit(rnum) result(fdigit)                   ! First digit of an integer number
!  F ndigits(num)                                       ! Count digits in a integer number
!  F xround(num,sig)                                    ! round a number to n significant digits
!  F string_sigf(val,sig)                               ! genera stringa da val con sig cifre significative
!  F r_to_s(x)                                          ! writes a real into a left justified character string
!  F i_to_s ( intval )                                  ! converts an integer to a left-justified string.
!  F i_to_s1 ( intval , nf)                             ! Write number in a allocatable string of length nf
!  S set_row_list(vet,ktype,ival,rval,sval,kpr)         ! Scrive una singola riga di una tabella
!  F s_in_quotes(string)  result(str)                   ! Enclose string in double quotes
!  F rem_quotes(string)  result(str)                    ! Remove quotes
!  S get_string_in_brackets(str,strb,ier)               ! Estrai la stringa tra parentesi
!  S write_svet(svet,nvet,kpr,word)                     ! Scrivi sulla stessa riga un vettore di stringhe
!  S s_c_append(str,c)                                  ! Append char to string only if char doesn't exist in the string
!  F cat_svet(svet,nvet)  result(str)                   ! Cat string vector in an allocatable string
!  F cat_ivet(ivet,sep)  result(str)                    ! Cat array of integers in a string
!  S write_title(kpr,title)                             ! Write title in a frame
!  F time_string                                        ! print the current time
!  F date_time_string                                   ! print the current date and time
!  F all_ch_eq(str,ch)                                  ! check if all characters in str are equal to ch
!  F is_in_brackets(str,pos)                            ! Check if pos is in brackets
!  S write_string_bin(aunit,string)                     ! Write variable string on binary file
!  F read_string_bin(aunit,string)                      ! Read variable string from binary file
!  S s_find_duplicate(svet,vd)                          ! Find duplicate in an array of strings. 
!  S s_delete_copies(svet,nsv,vd,ndel)                  ! Delete multiple copies in array svet if ncopies > ndel
!  S s_trim_zeros ( s )                                 ! Removes trailing zeros from a string.
!  F find_string_in_array(string, array) result(pos)    ! Find string in array of string. Return 0 if string is absent
!  S write_formatted_message(unt,message_type,msg,ampl) ! Write message in a box splitting at LF o blank

 implicit none

 private :: get_next_number_p, get_next_number_sr, get_next_number_si
 interface get_next_number
     module procedure get_next_number_p, get_next_number_sr, get_next_number_si
 end interface get_next_number

 private :: format_string_iv, format_string_rv, format_string_sv, & 
            format_string_r, format_string_i, format_string_s
 interface format_string
     module procedure format_string_iv, format_string_rv, format_string_sv, &
                      format_string_r, format_string_i, format_string_s
 end interface

 interface string_sig
     module procedure string_sigf, string_sigd
 end interface
 
 interface catstringpar
     module procedure catstringpar_r,catstringpar_i,catstringpar_vetr,catstringpar_veti
 end interface
 
 interface number_to_format
     module procedure number_to_format_veti
 end interface

 character (len=*), private, parameter :: digit="0123456789.-+"
 character (len=150), public           :: err_mess_string
 logical, public                       :: err_string

CONTAINS

   subroutine filtra(string)
!
   character(len=*), intent(inout) :: string
   integer                         :: k
!
   k = Lung(string)
   if (k.gt.0) then
       if (ichar(string(k:k)).eq.13) then
           string(k:k) = ' '
       endif
   endif
!
   end subroutine filtra

 !-----------------------------------------------------------------------

   Subroutine Cutst(line1,nlong1,line2,nlong2)
!
!  Removes the first word of the input String.
!  Provides (optionally) a string with the first word.
!
   character (len=*),           intent(in out) :: line1   !  In -> Input string
                                                          ! Out -> Input string without the first word
   character (len=*), optional, intent(   out) :: line2   ! Out -> The first word of String on Input
   integer,           optional, intent(   out) :: nlong1  ! Out -> Give the length of Line1 on Output
   integer,           optional, intent(   out) :: nlong2  ! Out -> Give the length of Line2 on Output
   integer  :: k,iniz1

   !---- Initializing variables ----!
   if (present(nlong1)) nlong1=0
   if (present(nlong2)) nlong2=0

   !---- Initializing to blank the directive ----!
   if (present(line2)) line2=" "

   !---- Elimination of possible blanks on the left ----!
   line1=adjustl(line1)
   if (len_trim(line1) <= 0) return

   k=len(line1)
   iniz1=index(line1," ")

   if (k ==1) then
      if (present(line2)) line2=line1
      if (present(nlong2)) nlong2=1
      line1=" "
   else
      if (iniz1 > 0) then
         if (present(line2))  line2=line1(1:iniz1-1)
         if (present(nlong2)) nlong2=len_trim(line1(1:iniz1-1))
         line1=line1(iniz1:)
      else
         if (present(line2))  line2=line1
         if (present(nlong2)) nlong2=len_trim(line1)
         line1=" "
      end if
   end if

   line1=adjustl(line1)
   if(present(nlong1)) nlong1=len_trim(line1)

   return
   End Subroutine Cutst

!corr !-----------------------------------------------------------------------
!corr
!corr   Subroutine Cutsta(line1,nlong1,line2,nlong2)
!corr!
!corr!  Removes the first word of the input String.
!corr!  Provides (optionally) a string with the first word.
!corr!
!corr   character (len=:), allocatable, intent(in out) :: line1            !  In -> Input string
!corr                                                                      ! Out -> Input string without the first word
!corr   character (len=:), allocatable, optional, intent(   out) :: line2  ! Out -> The first word of String on Input
!corr   integer,           optional, intent(   out) :: nlong1              ! Out -> Give the length of Line1 on Output
!corr   integer,           optional, intent(   out) :: nlong2              ! Out -> Give the length of Line2 on Output
!corr   integer  :: k,iniz1
!corr
!corr   !---- Initializing variables ----!
!corr   if (present(nlong1)) nlong1=0
!corr   if (present(nlong2)) nlong2=0
!corr
!corr   !---- Initializing to blank the directive ----!
!corr   if (present(line2)) line2=" "
!corr
!corr   !---- Elimination of possible blanks on the left ----!
!corr   line1=adjustl(line1)
!corr!corr   if (len_trim(line1) <= 0) return
!corr
!corr   k=len(line1)
!corr   iniz1=index(line1," ")
!corr
!corr   if (k ==1) then
!corr      if (present(line2)) line2=line1
!corr      if (present(nlong2)) nlong2=1
!corr      line1=" "
!corr   else
!corr      if (iniz1 > 0) then
!corr         if (present(line2))  line2=line1(1:iniz1-1)
!corr         if (present(nlong2)) nlong2=len_trim(line1(1:iniz1-1))
!corr         !line1=line1(iniz1:)   ! problem with gfortran
!corr         line1=line1(iniz1:len(line1))
!corr           write(0,*)'LINE1=',line1,len(line1)
!corr      else
!corr         if (present(line2))  line2=line1
!corr         if (present(nlong2)) nlong2=len_trim(line1)
!corr         line1=" "
!corr      end if
!corr   end if
!corr
!corr   line1=adjustl(line1)
!corr   if(present(nlong1)) nlong1=len_trim(line1)
!corr
!corr   return
!corr   End Subroutine Cutsta
!corr
 !-----------------------------------------------------------------------

   Subroutine Cutsta(line1,line2)
!
!  Removes the first word of the input String.
!  Provides (optionally) a string with the first word.
!
   character (len=:), allocatable, intent(inout)         :: line1  !  In -> Input string
                                                                   ! Out -> Input string without the first word
   character (len=:), allocatable, optional, intent(out) :: line2  ! Out -> The first word of String on Input
   integer                                               :: k,iniz1
   character(len=:), allocatable :: tmp

   !---- Initializing to blank the directive ----!
   if (present(line2)) line2=" "

   !---- Elimination of possible blanks on the left ----!
   line1=adjustl(line1)

   k=len(line1)
   iniz1=index(line1," ")

   if (k == 1) then
      if (present(line2)) line2=line1
      line1=" "
   else
      if (iniz1 > 0) then
         if (present(line2))  line2=line1(1:iniz1-1)
         !line1=line1(iniz1:)   ! problem with gfortran
         tmp = line1(iniz1:)
         line1 = tmp
           !write(0,*)'LINE1=',line1//'FINE',len(line1)
      else
         if (present(line2))  line2=line1
         line1=" "
      end if
   end if

   line1=adjustl(line1)

   End Subroutine Cutsta

 !-----------------------------------------------------------------------

   Subroutine Getnum(line,vet,ivet,iv)
!
!  Converts a string to numbers and write on VET/IVET if real/integer. Control
!  of errors is possible by inquiring the global variables ERR_STRING and
!  ERR_MESS_STRING
!
   !!!!!
      integer, parameter :: sp = 4
   !!!!!
   character (len=*),          intent ( in) :: line    ! Input String to convert
   real(kind=sp), dimension(:),intent (out) :: vet     ! Vector of real(kind=sp) numbers
   integer, dimension(:),      intent (out), optional :: ivet    ! Vector of integer numbers
   integer,                    intent (out) :: iv      ! Number of numbers in Vet/Ivet
   logical                                  :: numero
   character (len=len(line))                :: resto,cifre
   integer                                  :: i,isum,ncharl,nchard,isegno,iniz,ipoi,idec,idig
   integer                                  :: nchart, npos,nchard1,isum_exp,ioper
   real(kind=sp)                            :: suma,segno,dec
   real(kind=sp)                            :: sum_m

   call init_err_string()
   iv=0
   if (present(ivet)) ivet=0
   vet=0.0

   resto=u_case(line)

   do
      ioper=0
      isum_exp=0
      nchard1=0
      sum_m=0.0
      suma=0.0
      isum=0
      call cutst(resto,ncharl,cifre,nchard)
      if (nchard <= 0) exit

      !---- Is a number ----!
      numero=.true.
      do i=1,nchard
         if (cifre(i:i) =='E') cycle
         npos=index(digit,cifre(i:i))
         if (npos /= 0) cycle
         numero=.false.
      end do
      if (.not. numero) then
         err_string=.true.
         err_mess_string="The variable cannot be computed as a number in GETNUM "
         return
      end if

      !---- Positive or Negative number ----!
      segno=1.0
      isegno=1
      iniz=1
      if (cifre(1:1) == digit(12:12)) then
         segno=-1.0
         isegno=-1
         iniz=2
      end if

      !---- Decimal Number ----!
      ipoi=index(cifre(1:nchard),digit(11:11))

      !---- Exponential Number ----!
      nchard1=index(cifre(1:nchard),"E")
      if (nchard1 /= 0) then
         nchart=nchard
         nchard=nchard1-1
      end if

      if (ipoi == 0) ipoi=nchard+1
      dec=real(ipoi-1-iniz)
      idec=ipoi-1-iniz
      do i=iniz,nchard
         idig=index(digit,cifre(i:i))
         if (idig >= 1 .and. idig <= 11)  then
            if (idig <= 10)  then
!!!corr               suma=suma+real(idig-1)*10.0**dec
               suma=suma+real(idig-1)*10**dec
               if (idec >= 0) isum=isum*10+(idig-1)
               dec=dec-1.0
               idec=idec-1
            end if
         else
            err_string=.true.
            err_mess_string="Limits of digit variable exceeded in GETNUM"
            return
         end if
      end do

      if (nchard1 /= 0) then
         nchard1=nchard1+1
         select case (cifre(nchard1:nchard1))
            case ("-")
               ioper=1
               nchard1=nchard1+1

            case ("+")
               nchard1=nchard1+1
         end select

         do i=nchard1,nchart
            idig=index(digit,cifre(i:i))
            if (idig >= 1 .and. idig <= 10)  then
               isum_exp=isum_exp*10+(idig-1)
            else
               err_string=.true.
               err_mess_string="Limits of digit variable exceeded in GETNUM"
               return
            end if
         end do
      end if

      if (iv + 1 > size(vet)) return ! not enough space in vet

      iv=iv+1
      vet(iv)=suma*segno
      if (present(ivet)) ivet(iv)=isum*isegno

      if (nchard1 /= 0) then
         select case (ioper)
            case (0)
               sum_m=10.0**isum_exp

            case (1)
               sum_m=10.0**isum_exp
               sum_m=1.0/sum_m
         end select
         vet(iv)=vet(iv)*sum_m
      end if

      if (ncharl <= 0) then
         exit
      end if
   end do

   return
   End Subroutine Getnum

 !-----------------------------------------------------------------------

   Subroutine Getnum1(line,vet,ivet,iv,vsize)
!
!  Converts a string to numbers and write on VET/IVET if real/integer. Control
!  of errors is possible by inquiring the global variables ERR_STRING and
!  ERR_MESS_STRING
!
   USE arrayutil
   !!!!!
      integer, parameter :: sp = 4
   !!!!!
   character (len=*),          intent ( in) :: line    ! Input String to convert
   real, dimension(:), allocatable, intent (inout), optional :: vet     ! Vector of real(kind=sp) numbers
   integer, dimension(:),allocatable,  intent (inout), optional :: ivet    ! Vector of integer numbers
   integer,                    intent (out) :: iv      ! Number of numbers in Vet/Ivet
   integer, intent(in), optional :: vsize
   logical                                  :: numero
   character (len=len(line))                :: resto,cifre
   integer                                  :: i,isum,ncharl,nchard,isegno,iniz,ipoi,idec,idig
   integer                                  :: nchart, npos,nchard1,isum_exp,ioper
   real(kind=sp)                            :: suma,segno,dec
   real(kind=sp)                            :: sum_m
   integer, parameter :: INITDIM = 100
   integer :: initsize

   if (present(vsize)) then
       initsize = vsize
   else
       initsize = INITDIM
   endif
   call init_err_string()
!
!  Allocate vet/ivet to the same size if not already allocated in input
   if (present(vet)) then
       if (size_array(vet) == 0) call new_array(vet,initsize,0.0)
   endif
   if (present(ivet)) then
       if (size_array(ivet) == 0) call new_array(ivet,initsize,0)
   endif
   iv=0

   resto=u_case(line)

   do
      ioper=0
      isum_exp=0
      nchard1=0
      sum_m=0.0
      suma=0.0
      isum=0
      call cutst(resto,ncharl,cifre,nchard)
      if (nchard <= 0) exit

      !---- Is a number ----!
      numero=.true.
      do i=1,nchard
         if (cifre(i:i) =='E') cycle
         npos=index(digit,cifre(i:i))
         if (npos /= 0) cycle
         numero=.false.
      end do
      if (.not. numero) then
         err_string=.true.
         err_mess_string="The variable cannot be computed as a number in GETNUM "
         return
      end if

      !---- Positive or Negative number ----!
      segno=1.0
      isegno=1
      iniz=1
      if (cifre(1:1) == digit(12:12)) then
         segno=-1.0
         isegno=-1
         iniz=2
      end if

      !---- Decimal Number ----!
      ipoi=index(cifre(1:nchard),digit(11:11))

      !---- Exponential Number ----!
      nchard1=index(cifre(1:nchard),"E")
      if (nchard1 /= 0) then
         nchart=nchard
         nchard=nchard1-1
      end if

      if (ipoi == 0) ipoi=nchard+1
      dec=real(ipoi-1-iniz)
      idec=ipoi-1-iniz
      do i=iniz,nchard
         idig=index(digit,cifre(i:i))
         if (idig >= 1 .and. idig <= 11)  then
            if (idig <= 10)  then
!!!corr               suma=suma+real(idig-1)*10.0**dec
               suma=suma+real(idig-1)*10**dec
               if (idec >= 0) isum=isum*10+(idig-1)
               dec=dec-1.0
               idec=idec-1
            end if
         else
            err_string=.true.
            err_mess_string="Limits of digit variable exceeded in GETNUM"
            return
         end if
      end do

      if (nchard1 /= 0) then
         nchard1=nchard1+1
         select case (cifre(nchard1:nchard1))
            case ("-")
               ioper=1
               nchard1=nchard1+1

            case ("+")
               nchard1=nchard1+1
         end select

         do i=nchard1,nchart
            idig=index(digit,cifre(i:i))
            if (idig >= 1 .and. idig <= 10)  then
               isum_exp=isum_exp*10+(idig-1)
            else
               err_string=.true.
               err_mess_string="Limits of digit variable exceeded in GETNUM"
               return
            end if
         end do
      end if

!
!     expand capacity of array vet/ivet and initialize to 0
      if (present(vet)) then
          if (iv + 1 > size(vet)) then
              call resize_array(vet,size(vet)+initsize)
              vet(iv+2:) = 0.0
          endif
      endif
      if (present(ivet)) then
          if (iv + 1 > size(ivet)) then
              call resize_array(ivet,size(ivet)+initsize)
              ivet(iv+2:) = 0
          endif
      endif

      iv=iv+1
      if (present(ivet)) ivet(iv)=isum*isegno
      if (present(vet)) then
          vet(iv)=suma*segno

          if (nchard1 /= 0) then
             select case (ioper)
                case (0)
                   sum_m=10.0**isum_exp

                case (1)
                   sum_m=10.0**isum_exp
                   sum_m=1.0/sum_m
             end select
             vet(iv)=vet(iv)*sum_m
          end if
      endif

      if (ncharl <= 0) then
         exit
      end if
   end do

   return
   End Subroutine Getnum1

  !-----------------------------------------------------------------------

   subroutine get_words(line,wordv,nword)
!
!  Estrai da line tutte le stringhe separate da spazi in wordv
!
   character(len=*), intent(in)                :: line
   character(len=*), dimension(:), intent(out) :: wordv
   integer, intent(out)                        :: nword
   character(len_trim(line))                   :: resto,string
   integer                                     :: ncharl,nchard
!
   wordv(:) = ' '
   nword = 0
!
   resto = line
   do 
      call cutst(resto,ncharl,string,nchard)
      if (nchard == 0) exit
      nword = nword + 1
      wordv(nword) = string
      if (nword == size(wordv)) exit
   enddo
!
   end subroutine get_words

  !-----------------------------------------------------------------------

   subroutine get_words1(line,wordv,nword)
!
!  Estrai da line tutte le stringhe separate da spazi in wordv
!
   character(len=*), intent(in)                             :: line
   character(len=:), allocatable, dimension(:), intent(out) :: wordv
   integer, intent(out)                                     :: nword
   character(len_trim(line))                                :: resto,string
   integer                                                  :: ncharl,nchard
   integer :: maxlen
!
   nword = 0
   resto = line
   maxlen = 0
   do 
      call cutst(resto,ncharl,string,nchard)
      if (nchard == 0) exit
      nword = nword + 1
      if (nchard > maxlen) maxlen = nchard
   enddo
!
   if (nword == 0) return
   allocate(character(maxlen)::wordv(nword))
!
   nword = 0
   resto = line
   do 
      call cutst(resto,ncharl,string,nchard)
      if (nchard == 0) exit
      nword = nword + 1
      wordv(nword) = string
   enddo

   end subroutine get_words1

!corr  !-----------------------------------------------------------------------
!corr
!corr   real function real_from_string_scalar(line,pos,ier)  result(rnum)
!corr!
!corr!  Get real number from string in position pos
!corr!
!corr   character(len=*), intent(in) :: line
!corr   integer, intent(in)          :: pos
!corr   integer, intent(out)         :: ier
!corr   character(len_trim(line))    :: resto,string
!corr   integer                      :: ncharl,nchard,nword
!corr!
!corr   rnum = 0
!corr   resto = line
!corr   nword = 0
!corr   do 
!corr      call cutst(resto,ncharl,string,nchard)
!corr      if (nchard == 0) then
!corr          ier = 1
!corr          exit
!corr      endif
!corr      nword = nword + 1
!corr      if (nword == pos) then
!corr          call s_to_r(string,rnum,ier)
!corr          exit
!corr      endif
!corr   enddo
!corr!
!corr   end function real_from_string_scalar
!corr
  !-----------------------------------------------------------------------

   subroutine parse_line_reals(line,posv,rnum,ier)
!
!  Get real number from string in position pos. ier > 0 contains position
!
   character(len=*), intent(in) :: line
   integer, dimension(:), intent(in) :: posv
   integer, intent(out)         :: ier
   real, dimension(:), intent(out)   :: rnum
   character(len_trim(line))    :: resto,string
   integer                      :: ncharl,nchard,nword,npos
!
   rnum(:) = 0
   resto = line
   nword = 0
   npos = 1
   do 
      call cutst(resto,ncharl,string,nchard)
      if (nchard == 0) then
          ier = -1
          exit
      endif
      nword = nword + 1
      if (nword == posv(npos)) then
          ier =  s_to_r(string,rnum(npos))
          if (ier /= 0) then
              ier = npos
              exit
          endif
          if (npos == size(posv)) exit
          npos = npos + 1
      endif
   enddo
!
   end subroutine parse_line_reals

  !-----------------------------------------------------------------------
!corr
!corr   subroutine parse_line_after(line,symb,inum,ier)
!corr   character(len=*), intent(in)      :: line
!corr   character(len=*), intent(in)      :: symb
!corr   integer, intent(out)              :: inum
!corr   integer, intent(out)              :: ier
!corr!
!corr   ier = 1
!corr   pos = index(line,symb) 
!corr   if (pos == 0) return
!corr!
!corr   line = line(pos+trim(symb):)
!corr!
!corr   end subroutine parse_line_after 
!corr
  !----------------------------------------------------------------------------------

   subroutine get_next_number_p(string,pos,rnum,inum,ier)
!
!  Get the next number starting from position pos (pos is excluded)
!
   character(len=*), intent(in) :: string  
   integer, intent(inout)       :: pos     ! in out contains the last char corrisponding to number
   real, intent(out), optional  :: rnum
   integer, intent(out), optional  :: inum
   integer, intent(out)         :: ier     
   integer                      :: lens
   integer                      :: kb, kdigb 
   character(len=1)             :: ch
!corr   integer                      :: len
   character(len=len_trim(string)-pos+1) :: digitb
!
   if (present(rnum)) rnum = 0
   if (present(inum)) inum = 0
   ier = 0
   lens = len_trim(string)
   if (pos > lens) then
       ier = 1
   else
       kb = pos
       kdigb = 0
       do
         kb = kb + 1
         if (kb > lens) exit ! fine stringa
         ch = string(kb:kb)
         if (kdigb == 0 .and. ch_is_space(ch)) then
             pos = kb
         else
     !corr        if (.not. (ch_is_digit(ch) .or. ch == '.')) exit
             if (.not. (ch_is_digit(ch) .or. ch == '.' .or. (ch == '-' .and. kdigb == 0))) exit
             kdigb = kdigb + 1
             pos = kb
             digitb(kdigb:kdigb) = ch
         endif
       enddo
!
       if (kdigb > 0) then   ! digits found!
           if (present(rnum)) ier =  s_to_r(digitb(:kdigb),rnum)
           if (present(inum)) call s_to_i(digitb(:kdigb),inum,ier)
       else
           ier = 2   ! non ho trovato alcun numero
       endif
   endif
!
   end subroutine get_next_number_p

 !----------------------------------------------------------------------------------

   subroutine get_next_number_sr(string,spos,rnum,ier)
!
!  Get the next real number rnum starting from string spos
!
   character(len=*), intent(in) :: string  
   character(len=*), intent(in) :: spos 
   real, intent(out)            :: rnum
   integer, intent(out)         :: ier     
   integer                      :: pos
!
   pos = index(string,trim(spos))
   if (pos > 0) then
       pos = pos + len_trim(spos) - 1
       call get_next_number_p(string,pos,rnum,ier=ier)
   else
       ier = 2
   endif
!
   end subroutine get_next_number_sr

 !----------------------------------------------------------------------------------

   subroutine get_next_number_si(string,spos,inum,ier)
!
!  Get the next integer inum starting from string spos
!
   character(len=*), intent(in) :: string  
   character(len=*), intent(in) :: spos 
   integer, intent(out)         :: inum
   integer, intent(out)         :: ier     
   integer                      :: pos
!
   pos = index(string,trim(spos))
   if (pos > 0) then
       pos = pos + len_trim(spos) - 1
       call get_next_number_p(string,pos,inum=inum,ier=ier)
   else
       ier = 2
   endif
!
   end subroutine get_next_number_si

 !----------------------------------------------------------------------------------

   subroutine read_words(word,nword,wtype,vetr,ier)
   character(len=*), dimension(:), intent(in) :: word
   integer, intent(in)                        :: nword
   character(len=*), intent(in)               :: wtype
   real, dimension(:), intent(out)            :: vetr
   integer, intent(out)                       :: ier
   integer                                    :: i
   integer                                    :: nreal
   real                                       :: rvalue
!
   nreal = 0
   do i=1,min(nword,len_trim(wtype))
      select case (wtype(i:i)) 
        case ('R')
           ier =  s_to_r(word(i),rvalue)
           if (ier == 0) then
               nreal = nreal + 1
               vetr(nreal) = rvalue
           else
               exit
           endif
      end select
   enddo
!
   end subroutine read_words

  !-----------------------------------------------------------------------

   subroutine get_words_quotes(line,wordv,nword,btype)
!
!  Estrai da line solo le stringhe in parentesi
!
   character(len=*), intent(in)                :: line
   character(len=*), dimension(:), intent(out) :: wordv
   integer, intent(out)                        :: nword
   character(len=1), intent(in),optional       :: btype
   integer                                     :: pos1,pos2
   integer                                     :: lent
   integer                                     :: i
   integer                                     :: nw
   character(len=1)                            :: brac
!
   if (present(btype)) then
       brac = btype
   else
       brac = "'"
   endif
   nw = size(wordv)
   wordv(:) = ' '
   nword = 0
   lent = len_trim(line)
!
   pos1 = 0
   pos2 = 0
   do i=1,lent
      if (pos1 > 0 .and. line(i:i) == brac) pos2 = i
      if (line(i:i) == brac .and. pos1 == 0) pos1 = i
      if (pos1 > 0 .and. pos2 > 0) then
          if (pos1 + 1 /= pos2) then   ! /= da ''
              nword = nword + 1
              wordv(nword) = line(pos1+1:pos2-1)
          endif
          pos1 = 0
          pos2 = 0
          if (nword == nw) exit
      endif
   enddo
!
   end subroutine get_words_quotes

  !-----------------------------------------------------------------------

   subroutine get_words_quotes_a(line,wordv,nword,btype)
!
!  Estrai da line solo le stringhe in parentesi
!
   character(len=*), intent(in)                :: line
   character(len=:), allocatable, dimension(:), intent(out) :: wordv
   integer, intent(out)                        :: nword
   character(len=1), intent(in),optional       :: btype
   integer                                     :: pos1,pos2
   integer                                     :: lent
   integer                                     :: i
   integer                                     :: nw
   character(len=1)                            :: brac
   integer :: maxlen
!
   if (present(btype)) then
       brac = btype
   else
       brac = "'"
   endif
   nw = size(wordv)
   !corrwordv(:) = ' '
   nword = 0
   maxlen = 0
   lent = len_trim(line)
!
   pos1 = 0
   pos2 = 0
   do i=1,lent
      if (pos1 > 0 .and. line(i:i) == brac) pos2 = i
      if (line(i:i) == brac .and. pos1 == 0) pos1 = i
      if (pos1 > 0 .and. pos2 > 0) then
          if (pos1 + 1 /= pos2) then   ! /= da ''
              nword = nword + 1
              if (pos2-pos1+1 > maxlen) maxlen=pos2-pos1+1
!corr              wordv(nword) = line(pos1+1:pos2-1)
          endif
          pos1 = 0
          pos2 = 0
          if (nword == nw) exit
      endif
   enddo
   if (nword == 0) return
   allocate(character(maxlen)::wordv(nword))

   pos1 = 0
   pos2 = 0
   nword = 0
   do i=1,lent
      if (pos1 > 0 .and. line(i:i) == brac) pos2 = i
      if (line(i:i) == brac .and. pos1 == 0) pos1 = i
      if (pos1 > 0 .and. pos2 > 0) then
          if (pos1 + 1 /= pos2) then   ! /= da ''
              nword = nword + 1
              wordv(nword) = line(pos1+1:pos2-1)
          endif
          pos1 = 0
          pos2 = 0
          if (nword == nw) exit
      endif
   enddo
!
   end subroutine get_words_quotes_a

  !-----------------------------------------------------------------------

   subroutine get_words_quotes1(line,wordv,nword,btype)
!
!  Estrai da line tutte le stringhe in parentesi e non
!
   character(len=*), intent(in)                :: line
   character(len=*), dimension(:), intent(out) :: wordv
   integer, intent(out)                        :: nword
   character(len=1), intent(in),optional       :: btype
   integer                                     :: pos1
   integer                                     :: lent
   integer                                     :: i
   integer                                     :: nw
   character(len=1)                            :: ch
   logical                                     :: word_in_quotes
   character(len=1)                            :: brac
!
   if (present(btype)) then
       brac = btype
   else
       brac = "'"
   endif

!
   nw = size(wordv)
   wordv(:) = ' '
   nword = 0
   lent = len_trim(line)
!
   pos1 = 0
   !pos2 = 0
   word_in_quotes = .false.
 
   do i=1,lent
      ch = line(i:i) 
!corr      if (ch == "'" .and. pos1 == 0) then
      if (ch == brac .and. pos1 == 0) then
          word_in_quotes = .true.
          pos1 = i
          cycle
      endif
      if (word_in_quotes) then
          if (ch == brac .and. pos1 > 0) then
              word_in_quotes = .false.
              !pos2 = i
              if (i == pos1+1) then ! situazione ''
                  nword = nword + 1
                  wordv(nword) = ' '   ! stringa vuota
              else
                  nword = nword + 1
                  wordv(nword) = line(pos1+1:i-1)
              endif
              pos1 = 0
          endif
      else
          if (ch_is_space(ch) .and. pos1 > 0) then
              nword = nword + 1
              wordv(nword) = line(pos1:i-1)
              pos1 = 0
          elseif (.not. ch_is_space(ch) .and. pos1 == 0) then  ! carattere diverso da spazio
              pos1 = i
          endif
      endif
   enddo
   if (pos1 > 0) then
          !  write(0,*)'fine line=',line(pos1:)
       if (word_in_quotes) then
           nword = nword + 1
           wordv(nword) = line(pos1+1:lent)
       else
           nword = nword + 1
           wordv(nword) = line(pos1:lent)
       endif
   endif
!
   end subroutine get_words_quotes1

  !-----------------------------------------------------------------------

   Function U_Case(Text) Result (Mtext)
!
!  Conversion to upper case, text is not modified
!
   character (len=*), intent(in) :: text      ! String:"Input Line"
   character (len=len(text))     :: mtext     ! String:"INPUT LINE"
   integer, parameter            :: inc = ICHAR("A") - ICHAR("a")
   integer                       :: leng, pos

   mtext=text
   leng=len_trim(mtext)
   do pos=1,leng
      if (mtext(pos:pos) >= "a" .and. mtext(pos:pos) <= "z")           &
          mtext(pos:pos) = CHAR ( ICHAR(mtext(pos:pos)) + inc )
   end do

   return
   End Function U_Case

 !-----------------------------------------------------------------------

   Function L_Case(Text) Result (Mtext)
!
!  Conversion to lower case, text is not modified
!
   character (len=*), intent(in) :: text      ! String:"Input Line"
   character (len=len(text))     :: mtext     ! String:"INPUT LINE"
   integer, parameter            :: inc = ICHAR("A") - ICHAR("a")
   integer                       :: leng, pos

   mtext=text
   leng=len_trim(mtext)
   do pos=1,leng
      if (mtext(pos:pos) >= "A" .and. mtext(pos:pos) <= "Z")           &
          mtext(pos:pos) = CHAR ( ICHAR(mtext(pos:pos)) - inc )
   end do

   End Function L_Case

 !-----------------------------------------------------------------------

   Subroutine Init_Err_String()
!
!  Initializes general error variables for this module as:
!  ERR_STRING=.false. ;  ERR_MESS_STRING=" "
!
   err_string=.false.
   err_mess_string=" "

   return
   End Subroutine Init_Err_String

 !-----------------------------------------------------------------------

   integer function Lung (Stringa)
   implicit none
!
   character(len=*), intent(in) :: stringa
   integer                      :: K
   integer                      :: I
   integer                      :: num
!
   K = len (Stringa)
   do I = K, 1, -1
       num = ichar(Stringa(I:I))
       if (num.ne.32.and.num.ne.0) then
          Lung = I
          return
       end if
   enddo
   Lung = 0
!
   end function Lung

 !-----------------------------------------------------------------------

   elemental function addc0(s0)
!
!  add char(0) to string
!
   character(len=*), intent(in) :: s0
   character(len=len(s0)) :: addc0
   addc0=trim(s0)//char(0)
   end function addc0

 !-----------------------------------------------------------------------

   elemental function remc0(s0)
!
!  remove char(0) from string
!
   character(len=*), intent(in) :: s0
   character(len=len(s0))       :: remc0
   integer                      :: pos0
!
   pos0 = index(s0,char(0))
   if (pos0 > 0) then
       remc0 = s0(1:pos0-1)
   else
       remc0 = s0
   endif
!
   end function remc0

 !-----------------------------------------------------------------------

   subroutine s_trim_char ( str , c) 
!
!  removes trailing c from a string.
!
   character ( len = * ), intent(inout) :: str
   character ( len = * ), intent(in)    :: c
   integer i
 
   i = len_trim (str)
   do while ( str(i:i) == c )
     str(i:i) = ' '
     i = i - 1
     if (i == 0) exit
   end do
 
   end subroutine s_trim_char

 !-----------------------------------------------------------------------

   elemental function add_c0(s0,pos)  result(str)
!
!  Aggiungi char(0) non oltre pos
!
   character(len=*), intent(in) :: s0
   integer, intent(in)          :: pos
   character(len=len(s0)+1)     :: str
!
   if (len_trim(s0) < pos) then
       str = trim(s0)//char(0)
   else
       str = s0(:pos-1)//char(0)
   endif
!
   end function add_c0
 !-----------------------------------------------------------------------

   subroutine s_cat0(s1,s2)
!
!  Concatena 2 stringhe in s1 e s2 separandole con un blank
!
   character(len=*), intent(inout) :: s1
   character(len=*), intent(in)    :: s2
!
   s1(len_trim(s1)+1:) = ' '//s2
!
   end subroutine s_cat0

  !-----------------------------------------------------------------------

   subroutine s_catend(s1,s2)
!
!  Concatena 2 stringhe in s1 e s2 aggiungendo char(13)//char(10) per segnalare la fine della riga
!
   character(len=*), intent(inout) :: s1
   character(len=*), intent(in)    :: s2
!
   s1(len_trim(s1)+1:) = s2//char(13)//char(10)
!
   end subroutine s_catend

  !-----------------------------------------------------------------------

   subroutine sort_heap_external ( n, indx, i, j, isgn )
   
   !*******************************************************************************
   !
   !! SORT_HEAP_EXTERNAL externally sorts a list of items into ascending order.
   !
   !  Discussion:
   !
   !    The actual list of data is not passed to the routine.  Hence this
   !    routine may be used to sort integers, reals, numbers, names,
   !    dates, shoe sizes, and so on.  After each call, the routine asks
   !    the user to compare or interchange two items, until a special
   !    return value signals that the sorting is completed.
   !
   !  Modified:
   !
   !    05 February 2004
   !
   !  Reference:
   !
   !    A Nijenhuis and H Wilf,
   !    Combinatorial Algorithms,
   !    Academic Press, 1978, second edition,
   !    ISBN 0-12-519260-6.
   !
   !  Parameters:
   !
   !    Input, integer N, the number of items to be sorted.
   !
   !    Input/output, integer INDX, the main communication signal.
   !
   !    The user must set INDX to 0 before the first call.
   !    Thereafter, the user should not change the value of INDX until
   !    the sorting is done.
   !
   !    On return, if INDX is
   !
   !      greater than 0,
   !      * interchange items I and J;
   !      * call again.
   !
   !      less than 0,
   !      * compare items I and J;
   !      * set ISGN = -1 if I < J, ISGN = +1 if J < I;
   !      * call again.
   !
   !      equal to 0, the sorting is done.
   !
   !    Output, integer I, J, the indices of two items.
   !    On return with INDX positive, elements I and J should be interchanged.
   !    On return with INDX negative, elements I and J should be compared, and
   !    the result reported in ISGN on the next call.
   !
   !    Input, integer ISGN, results of comparison of elements I and J.
   !    (Used only when the previous call returned INDX less than 0).
   !    ISGN <= 0 means I is less than or equal to J;
   !    0 <= ISGN means I is greater than or equal to J.
   !
     implicit none
   
     integer i
     integer, save :: i_save = 0
     integer indx
     integer isgn
     integer j
     integer, save :: j_save = 0
     integer, save :: k = 0
     integer, save :: k1 = 0
     integer n
     integer, save :: n1 = 0
   !
   !  INDX = 0: This is the first call.
   !
     if ( indx == 0 ) then
   
       i_save = 0
       j_save = 0
       k = n / 2
       k1 = k
       n1 = n
   !
   !  INDX < 0: The user is returning the results of a comparison.
   !
     else if ( indx < 0 ) then
   
       if ( indx == -2 ) then
   
         if ( isgn < 0 ) then
           i_save = i_save + 1
         end if
   
         j_save = k1
         k1 = i_save
         indx = -1
         i = i_save
         j = j_save
         return
   
       end if
   
       if ( 0 < isgn ) then
         indx = 2
         i = i_save
         j = j_save
         return
       end if
   
       if ( k <= 1 ) then
   
         if ( n1 == 1 ) then
           i_save = 0
           j_save = 0
           indx = 0
         else
           i_save = n1
           n1 = n1 - 1
           j_save = 1
           indx = 1
         end if
   
         i = i_save
         j = j_save
         return
   
       end if
   
       k = k - 1
       k1 = k
   !
   !  0 < INDX, the user was asked to make an interchange.
   !
     else if ( indx == 1 ) then
   
       k1 = k
   
     end if
   
     do
   
       i_save = 2 * k1
   
       if ( i_save == n1 ) then
         j_save = k1
         k1 = i_save
         indx = -1
         i = i_save
         j = j_save
         return
       else if ( i_save <= n1 ) then
         j_save = i_save + 1
         indx = -2
         i = i_save
         j = j_save
         return
       end if
   
       if ( k <= 1 ) then
         exit
       end if
   
       k = k - 1
       k1 = k
   
     end do
   
     if ( n1 == 1 ) then
       i_save = 0
       j_save = 0
       indx = 0
       i = i_save
       j = j_save
     else
       i_save = n1
       n1 = n1 - 1
       j_save = 1
       indx = 1
       i = i_save
       j = j_save
     end if
   
     return
   end subroutine sort_heap_external

  !-----------------------------------------------------------------------

   subroutine svec_sort_heap_a ( n, a )
   
   !*******************************************************************************
   !
   !! SVEC_SORT_HEAP_A ascending sorts a vector of character strings using heap sort.
   !
   !  Discussion:
   !
   !    The ASCII collating sequence is used.  This means
   !      A < B < C < .... < Y < Z < a < b < .... < z.
   !    Numbers and other symbols may also occur, and will be sorted according to
   !    the ASCII ordering.
   !
   !  Modified:
   !
   !    27 June 2000
   !
   !  Author:
   !
   !    John Burkardt
   !
   !  Parameters:
   !
   !    Input, integer N, the number of strings
   !
   !    Input/output, character ( len = * ) A(N);
   !    On input, an array of strings to be sorted.
   !    On output, the sorted array.
   !
     implicit none
   
     integer n
   
     character ( len = * ) a(n)
     integer i
     integer indx
     integer isgn
     integer j
   !
   !  Do the sorting using the external heap sort routine.
   !
     i = 0
     indx = 0
     isgn = 0
     j = 0
   
     do
   
       call sort_heap_external ( n, indx, i, j, isgn )
   
       if ( 0 < indx ) then
   
         call s_swap ( a(i), a(j) )
   
       else if ( indx < 0 ) then
   
         if ( lle ( a(i), a(j) ) ) then
           isgn = -1
         else
           isgn = +1
         end if
   
       else if ( indx == 0 ) then
   
         exit
   
       end if
   
     end do
   
     return
   end subroutine svec_sort_heap_a

  !-----------------------------------------------------------------------

   subroutine svec_sort_heap_a_index ( n, sarray, indx )
!
!! SVEC_SORT_HEAP_A_INDEX does a case-sensitive indexed heap sort of a vector of strings.
!
!  Discussion:
!
!    The sorting is not actually carried out.
!    Rather an index array is created which defines the sorting.
!    This array may be used to sort or index the array, or to sort or
!    index related arrays keyed on the original array.
!
!    The ASCII collating sequence is used, and case is significant.
!    This means
!
!      A < B < C < .... < Y < Z < a < b < .... < z.
!
!    Numbers and other symbols may also occur, and will be sorted according to
!    the ASCII ordering.
!
!  Modified:
!
!    27 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of entries in SARRAY.
!
!    Input, character ( len = * ) SARRAY(N), an array to be sorted.
!
!    Output, integer INDX(N), contains the sort index.  The
!    I-th element of the sorted array is SARRAY ( INDX(I) ).
!
    implicit none

    integer, parameter :: MAX_CHAR = 255
    integer n

    integer i
    integer indx(n)
    integer indxt
    integer ir
    integer j
    integer l
    character ( len = * ) sarray(n)
    character ( len = MAX_CHAR ) string

    do i = 1, n
      indx(i) = i
    end do

    l = n / 2 + 1
    ir = n

    do

      if ( 1 < l ) then

        l = l - 1
        indxt = indx(l)
        string = sarray(indxt)

      else

        indxt = indx(ir)
        string = sarray(indxt)
        indx(ir) = indx(1)
        ir = ir - 1

        if ( ir == 1 ) then
          indx(1) = indxt
          return
        end if

      end if

      i = l
      j = l + l

      do while ( j <= ir )

        if ( j < ir ) then
          if ( llt ( sarray ( indx(j) ), sarray ( indx(j+1) ) ) ) then
            j = j + 1
          end if
        end if

        if ( llt ( string, sarray ( indx(j) ) ) ) then
          indx(i) = indx(j)
          i = j
          j = j + j
        else
          j = ir + 1
        end if

      end do

      indx(i) = indxt

    end do

    return
   end subroutine svec_sort_heap_a_index

 !--------------------------------------------------------------------------------------------

   subroutine s_rep_ch ( s, c1, c2 )
!
!! S_REP_CH replaces all occurrences of one character by another.
!
!  Modified:
!
!    27 March 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string.
!
!    Input, character C1, C2, the character to be replaced, and the
!    replacement character.
!
   implicit none

   character c1
   character c2
   integer i
   character ( len = * ) s

   do i = 1, len ( s )
     if ( s(i:i) == c1 ) then
       s(i:i) = c2
     end if
   end do

   return
   end subroutine s_rep_ch

 !----------------------------------------------------------------------------------------------

subroutine s_rep ( s, sub1, sub2, irepp )

!*******************************************************************************
!
!! S_REP replaces all occurrences of SUB1 by SUB2 in a string.
!
!  Discussion:
!
!    This is not always true if SUB2 is longer than SUB1.  The
!    replacement is NOT recursive.  In other words, replacing all
!    occurrences of "ab" by "a" in "abbbbb" will return "abbbb"
!    rather than "a".
!
!  Modified:
!
!    14 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S.  On input,
!    the string in which occurrences are to be replaced.
!    On output, the revised string.
!
!    Input, character ( len = * ) SUB1, the string which is to be replaced.
!    Trailing blank characters are ignored.  The routine is case sensitive.  
!
!    Input, character ( len = * ) SUB2, the replacement string.
!
!    Output, integer IREP, the number of replacements made.
!    If IREP is negative, then its absolute value is the
!    number of replacements made, and SUB2 is longer than
!    SUB1, and at least one substring SUB1 could not be
!    replaced by SUB2 because there was no more space.
!    (If S = 'aab' and SUB1 = 'a' and SUB2 = 'cc'
!    then the result would be S = 'cca'.  The first 'a'
!    was replaced, the 'b' fell off the end, the second 'a'
!    was not replaced because the replacement 'cc' would have
!    fallen off the end)
!
  implicit none

  integer, intent(out), optional :: irepp
  integer ilo
  integer irep
  integer len1
  integer len2
  integer lens
  integer loc
  character ( len = * ) s
  character ( len = * ) sub1
  character ( len = * ) sub2

  irep = 0
  lens = len ( s )
 
  if ( lens <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'S_REP - Serious error!'
    write ( *, '(a)' ) '  Null string not allowed!'
    if (present(irepp)) irepp = irep
    return
  end if
 
  len1 = len_trim ( sub1 )
 
  if ( len1 <= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'S_REP - Serious error!'
    write ( *, '(a)' ) '  Null SUB1 not allowed!'
    if (present(irepp)) irepp = irep
    return
  end if
 
  len2 = len_trim ( sub2 )
 
  if ( len2 == len1 ) then
 
    if ( sub1(1:len1) == sub2(1:len2) ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'S_REP - Warning!'
      write ( *, '(a)' ) '  Replacement = original!'
      if (present(irepp)) irepp = irep
      return
    end if
 
    ilo = 1

    do

      loc = index ( s(ilo:lens), sub1(1:len1) )

      if ( loc == 0 ) then
        exit
      end if

      loc = loc + ilo - 1
      irep = irep + 1
      s(loc:loc+len1-1) = sub2(1:len2)
      ilo = loc + len1

      if ( lens < ilo ) then
        exit
      end if

    end do
 
  else if ( len2 < len1 ) then
 
    ilo = 1

    do

      loc = index ( s(ilo:lens), sub1(1:len1) )

      if ( loc == 0 ) then
        exit
      end if

      irep = irep + 1
      loc = loc + ilo - 1
      s(loc:loc+len2-1) = sub2(1:len2)
      call s_chop ( s, loc+len2, loc+len1-1 )
      ilo = loc + len2

      if ( lens < ilo ) then
        exit
      end if

    end do
 
  else
 
    ilo = 1

    do

      loc = index ( s(ilo:lens), sub1(1:len1) )

      if ( loc == 0 ) then
        exit
      end if

      loc = loc + ilo - 1
      irep = irep + 1
 
      if ( lens < loc + len2 - 1 ) then
        irep = -irep
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'S_REP - Warning!'
        write ( *, '(a)' ) '  Some replaceable elements remain!'
        exit
      end if
 
      call s_blanks_insert ( s, loc, loc+len2-1-len1 )

      s(loc:loc+len2-1) = sub2(1:len2)
      ilo = loc + len2

    end do

  end if

  if (present(irepp)) irepp = irep
  return 

  end subroutine s_rep

 !----------------------------------------------------------------------------------------------

  function s_replace_by_blanks(s, sub1) result(s_replace)
  character(len=*), intent(in) :: s, sub1
  character(len=len(s))        :: s_replace
  s_replace = s
  call s_rep(s_replace,sub1," ")
  end function s_replace_by_blanks

 !----------------------------------------------------------------------------------------------

  function s_replace(s, sub1, sub2)
  character(len=*), intent(in) :: s, sub1, sub2
  character(len=len(s))        :: s_replace
  s_replace = s
  call s_rep(s_replace,sub1,sub2)
  end function s_replace

 !----------------------------------------------------------------------------------------------

   function s_u2b ( s )
!
!! S_U2B replaces underscores by blanks.
!
!  Modified:
!
!    10 December 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be
!    transformed.
!
   integer i
   integer nchar
   character ( len = * ) s
   character(len=:), allocatable :: s_u2b
   
   nchar = len_trim ( s )
   
   s_u2b = s
   do i = 1, nchar
     if ( s_u2b(i:i) == '_' ) then
       s_u2b(i:i) = ' '
     end if
   end do
   
   return
   end function s_u2b

 !----------------------------------------------------------------------------------------------

   subroutine s_filter(str)
!
!  filtra una stringa rimuovendo tutti i caratteri speciali
   character(len=*), intent(inout) :: str
   integer                         :: i
   integer                         :: nchar
!
   do i=1,len(str)
      nchar = ichar(str(i:i))
      if (nchar < 32 .or. nchar >= 127) str(i:i) = ' '
   enddo
!
   end subroutine s_filter

 !----------------------------------------------------------------------------------------------

   subroutine s_chop ( s, ilo, ihi )
!
!! S_CHOP "chops out" a portion of a string, and closes up the hole.
!
!  Example:
!
!    S = 'Fred is not a jerk!'
!
!    call s_chop ( S, 9, 12 )
!
!    S = 'Fred is a jerk!    '
!
!  Modified:
!
!    06 July 1998
!
!  Author:
!
!    John Burkardt modified by Corrado Cuocci
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be transformed.
!
!    Input, integer ILO, IHI, the locations of the first and last
!    characters to be removed.
!
   implicit none

   integer, intent(in)           :: ilo
   integer, intent(in), optional :: ihi  ! se assente taglia fino alla fine
   integer ilo2
   integer ihi2
   integer lens
   character ( len = * ) s

   lens = len ( s )
   if (present(ihi)) then
       ihi2 = min ( ihi, lens )
   else
       ihi2 = lens
   endif

   ilo2 = max ( ilo, 1 )

   if ( ihi2 < ilo2 ) then
     return
   end if

   s(ilo2:lens+ilo2-ihi2-1) = s(ihi2+1:lens)
   s(lens+ilo2-ihi2:lens) = ' '

   return
   end subroutine s_chop

 !--------------------------------------------------------------------------------------------------

   function string_from_time(time)  result(stime)
!
!  Converte tempo in stringa. Il tempo va espresso in secondi
!
   implicit none
   integer, intent(in) :: time
   character(len=15)   :: stime
!
   select case (time)
       case (:59)
!
!         tempo inferiore a 1 min: scrivo solo i secondi
          write(stime,'(i2,a4)')time,'sec'

       case (60:599)
!
!         tempo di 1-10 minuti: scrivo min e sec
          write(stime,'(i2,a5,i2,a3)')time/60,'min  ',mod(time,60),'sec'

       case (600:3599)
!
!         tempo di 10-1h: scrivo solo min
          write(stime,'(i2,a3)')time/60,'min'

       case (3600:86399)
!
!         tempo di 1-24 ore: ore e minuti
          write(stime,'(i2,a3,i2,a3)')time/3600,'h  ',mod(time,3600)/60,'min'

       case (86400:)
!
!         pi� di 1 giorno
          write(stime,'(i3,a5)') time/86400,' day'
   end select
!
   end function string_from_time

 !--------------------------------------------------------------------------------------------------

   subroutine s_s_insert ( s, ipos, s2 )
!
!  S_S_INSERT inserts a substring into a string.
!
!  Discussion:
!
!    Characters in the string are moved to the right to make room, and
!    hence the trailing characters, if any, are lost.
!
!  Modified:
!
!    14 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string into which
!    the second string is to be inserted.
!
!    Input, integer IPOS, the position in S at which S2 is to be inserted.
!
!    Input, character ( len = * ) S2, the string to be inserted.
!
   implicit none

   integer ihi
   integer ipos
   integer nchar
   integer nchar2
   character ( len = * ) s
   character ( len = * ) s2

   nchar = len ( s )
   nchar2 = len_trim ( s2 )

   ihi = min ( nchar, ipos+nchar2-1 )

   call s_blanks_insert ( s, ipos, ihi )

   s(ipos:ihi) = s2

   return
   end subroutine s_s_insert

 !----------------------------------------------------------------------------------------------------

   subroutine s_set_delete ( s, s2 )
!
!! S_SET_DELETE removes any characters in one string from another string.  
!
!  Discussion:
!
!    When an element is removed, the rest of the string is shifted to the 
!    left, and padded with blanks on the right.
!
!  Modified:
!
!    14 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to be examined.
!
!    Input, character ( len = * ) S2, the characters to be removed.
!
   integer i
   integer j
   integer nlen
   integer nset
   character ( len = * ) s
   character ( len = * ) s2
   
   nlen = len ( s )
   nset = len ( s2 )
   
   i = 0
   
   do while ( i < nlen )
   
     i = i + 1
   
     do j = 1, nset
       if ( s(i:i) == s2(j:j) ) then
         call s_chop ( s, i, i )
         nlen = nlen - 1
         i = i - 1
         exit
       end if
     end do
   
   end do
   
   end subroutine s_set_delete

 !----------------------------------------------------------------------------------------------------

   subroutine s_swap ( s1, s2 )
   
   !*******************************************************************************
   !
   !! S_SWAP swaps two strings.
   !
   !  Modified:
   !
   !    30 July 1999
   !
   !  Author:
   !
   !    John Burkardt
   !
   !  Parameters:
   !
   !    Input/output, character ( len = * ) S1, S2.  On output, the values of S1
   !    and S2 have been interchanged.
   !
     implicit none
   
     character ( len = * ) s1
     character ( len = * ) s2
     character ( len = 256 ) s3
   
     s3 = s1
     s1 = s2
     s2 = s3
   
     return
   end subroutine s_swap

 !----------------------------------------------------------------------------------------------------

   subroutine s_tab_blank ( s )
   
   !*******************************************************************************
   !
   !! S_TAB_BLANK replaces each TAB character by one space.
   !
   !  Modified:
   !
   !    14 April 1999
   !
   !  Author:
   !
   !    John Burkardt
   !
   !  Parameters:
   !
   !    Input/output, character ( len = * ) S, the string to be transformed.
   !
     implicit none
   
     integer i
     character ( len = * ) s
     character, parameter :: TAB = char ( 9 )
   
     do i = 1, len ( s )
   
       if ( s(i:i) == TAB ) then
         s(i:i) = ' '
       end if
   
     end do
   
   end subroutine s_tab_blank

 !----------------------------------------------------------------------------------------------------

   subroutine s_tab_blanks ( s )
   
   !*******************************************************************************
   !
   !! S_TAB_BLANKS replaces TAB characters by 6 spaces.
   !
   !  Modified:
   !
   !    14 April 1999
   !
   !  Author:
   !
   !    John Burkardt
   !
   !  Parameters:
   !
   !    Input/output, character ( len = * ) S, the string to be modified.  On 
   !    output, some significant characters at the end of S may have 
   !    been lost.
   !
     implicit none
   
     integer i
     integer get
     integer put
     integer lenc
     integer lens
     integer ntab
     character ( len = * ) s
     character, parameter :: TAB = char ( 9 )
   !
   !  If no TAB's occur in the line, there is nothing to do.
   !
     if ( index ( s, TAB ) == 0 ) then
       return
     end if
   !
   !  Otherwise, find out how long the string is.
   !
     lenc = len_trim ( s )
     lens = len ( s )
   !
   !  Count the TAB's.
   !
     ntab = 0
     do i = 1, lenc
       if ( s(i:i) == TAB ) then
         ntab = ntab + 1
       end if
     end do
   !
   !  Now copy the string onto itself, going backwards.
   !  As soon as we've processed the first TAB, we're done.
   !
     put = lenc + 5 * ntab
   
     do get = lenc, 1, - 1
   
       if ( s(get:get) /= TAB ) then
   
         if ( put <= lens ) then
           s(put:put) = s(get:get)
         end if
   
         put = put - 1
   
       else
   
         do i = put, put - 5, -1
           if ( i <= lens ) then
             s(i:i) = ' '
           end if
         end do
   
         put = put - 6
         ntab = ntab - 1
   
         if ( ntab == 0 ) then
           return
         end if
   
       end if
   
     end do
   
   end subroutine s_tab_blanks

 !----------------------------------------------------------------------------------------------------

   subroutine s_ch_blank ( s, c )

!*******************************************************************************
!
!! S_CH_BLANK replaces each occurrence of a particular character by a blank.
!
!  Modified:
!
!    28 June 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be transformed.
!
!    Input, character C, the character to be removed.
!
   implicit none

   character c
   integer i
   integer nchar
   character ( len = * ) s

   nchar = len_trim ( s )

   do i = 1, nchar
 
      if ( s(i:i) == c ) then
        s(i:i) = ' '
      end if
 
   end do

   return
   end subroutine s_ch_blank

 !----------------------------------------------------------------------------------------------------

   function s_blank_delete ( s )  result(sblank)

!*******************************************************************************
!
!! S_BLANK_DELETE removes blanks from a string, left justifying the remainder.
!
!  Comment:
!
!    All TAB characters are also removed.
!
!  Modified:
!
!    26 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be transformed.
!
   implicit none

   character( len = * ), intent(in) :: s
   character( len = len_trim(s))    :: sblank
   character, parameter             :: TAB = char ( 9 )
   character                        :: c
   integer                          :: get
   integer                          :: put
   integer                          :: nchar

   put = 0
   nchar = len_trim ( s )

   do get = 1, nchar

     c = s(get:get)
 
     if ( c /= ' ' .and. c /= TAB ) then
       put = put + 1
       sblank(put:put) = c
     end if
 
   end do
 
   sblank(put+1:nchar) = ' '
  
   end function s_blank_delete

 !----------------------------------------------------------------------------------------------------

   subroutine s_blanks_delete ( s )

!*******************************************************************************
!
!! S_BLANKS_DELETE replaces consecutive blanks by one blank.
!
!  Discussion:
!
!    Thanks to Bill Richmond for pointing out a programming flaw which
!    meant that, as characters were slid to the left through multiple
!    blanks, their original images were not blanked out.  This problem
!    is easiest resolved by using a copy of the string.
!
!    The remaining characters are left justified and right padded with blanks.
!    TAB characters are converted to spaces.
!
!  Modified:
!
!    30 September 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be transformed.
!
   implicit none

   integer i
   integer j
   character newchr
   character oldchr
   character ( len = * ) s
   integer s_len
   character, parameter :: TAB = char ( 9 )
   character ( len = len ( s ) ) s_copy

   s_len = len ( s )

   j = 0
   s_copy(1:s_len) = s(1:s_len)
   s(1:s_len) = ' '

   newchr = ' '
 
   do i = 1, s_len
 
     oldchr = newchr
     newchr = s_copy(i:i)

     if ( newchr == TAB ) then
       newchr = ' '
     end if
 
     if ( oldchr /= ' ' .or. newchr /= ' ' ) then
       j = j + 1
       s(j:j) = newchr
     end if
 
   end do
 
   return
   end subroutine s_blanks_delete

 !----------------------------------------------------------------------------------------------------

   subroutine s_blanks_insert ( s, ilo, ihi )
!
!! S_BLANKS_INSERT inserts blanks into a string, sliding old characters over.
!
!  Discussion:
!
!    Characters at the end of the string "drop off" and are lost.
!
!  Modified:
!
!    14 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be transformed.
!
!    Input, integer ILO, the location where the first blank is to be inserted.
!
!    Input, integer IHI, the location where the last blank is to be inserted.
!
   implicit none

   character c
   integer i
   integer get
   integer ihi
   integer ilo
   integer imax
   integer imin
   integer put
   integer nchar
   integer nmove
   character ( len = * ) s

   nchar = len ( s )

   if ( ihi < ilo .or. nchar < ilo ) then
     return
   end if

   if ( ihi <= nchar ) then
     imax = ihi
   else
     imax = nchar
   end if

   if ( 1 <= ilo ) then
     imin = ilo
   else
     imin = 1
   end if

   nmove = nchar - imax

   do i = 1, nmove
     put = nchar + 1 - i
     get = nchar - imax + imin - i
     c = s(get:get)
     s(put:put) = c
   end do

   do i = imin, imax
     s(i:i) = ' '
   end do

   return
   end subroutine s_blanks_insert

 !----------------------------------------------------------------------------------------------------

   subroutine s_s_delete ( s, sub, irepp )
!
!  S_S_DELETE removes all occurrences of a substring from a string.
!
!  Discussion:
!
!    The remainder is left justified and padded with blanks.
!
!    The deletion is not recursive.  Removing all occurrences of "ab" from
!    "aaaaabbbbbQ" results in "aaaabbbbQ" rather than "Q".
!
!  Modified:
!
!    14 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be transformed.
!
!    Input, character ( len = * ) SUB1, the substring to be removed.
!
!    Output, integer IREP, the number of occurrences of SUB1
!    which were found.
!
   implicit none

   integer, intent(out), optional :: irepp
   integer ihi
   integer ilo
   integer irep
   integer loc
   integer nsub
   character ( len = * ) s
   character ( len = * ) sub

   nsub = len_trim ( sub )

   irep = 0
   ilo = 1
   ihi = len_trim ( s )

   do while ( ilo <= ihi )

     loc = index ( s(ilo:ihi), sub )

     if ( loc == 0 ) then
       if (present(irepp)) irepp = irep
       return
     end if

     irep = irep + 1
     loc = loc + ilo - 1
     call s_chop ( s, loc, loc+nsub-1 )
     ilo = loc
     ihi = ihi - nsub

   end do

   if (present(irepp)) irepp = irep
   return

   end subroutine s_s_delete

 !----------------------------------------------------------------------------------------------------

   function s_delete ( s, sub)
!
!  S_DELETE removes all occurrences of a substring from a string.
!
   character(len=*), intent(in) :: s,sub
   character(len=len(s))        :: s_delete
!
   s_delete = s
!
   call s_s_delete(s_delete,sub)
!
   end function s_delete

 !----------------------------------------------------------------------------------------------------

   function upper ( s )
!
!  UPPER returns an uppercase version of a string.
!
!  Discussion:
!
!    UPPER is a string function of undeclared length.  The length
!    of the argument returned is determined by the declaration of
!    UPPER in the calling routine.
!
!  Modified:
!
!    11 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, a string.
!
!    Output, character ( len = * ) UPPER, an uppercase copy of the string.
!
   implicit none

   integer i
   integer j
   integer n
   character ( len = * ) s
   character ( len = len(s) ) upper

   upper = s
   n = len_trim ( upper )
   do i = 1, n
     j = ichar ( upper(i:i) )
     if ( 97 <= j .and. j <= 122 ) then
       upper(i:i) = char ( j - 32 )
     end if
   end do
   end function upper

 !----------------------------------------------------------------------------------------------------

function len_noctrl ( s )

!*******************************************************************************
!
!! LEN_NOCTRL returns the length of a string up to the last non-control and blank character.
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to measure.
!
!    Output, integer LEN_NOCTRL, the length of the string, up to the last
!    non-control and blank character.
!
  implicit none

  integer i
  integer len_noctrl
  integer len_s
  character, parameter :: NULL = char ( 0 )
  character ( len = * ) s

  len_s = len ( s )

  do i = len_s, 1, -1
    if (.not.(ch_is_control(s(i:i)) .or. s(i:i)==' ') ) then
      len_noctrl = i
      return
    end if
  end do

  len_noctrl = 0

end function len_noctrl

 !----------------------------------------------------------------------------------------------------

   function lower ( s )
!
!  LOWER returns a lowercase version of a string.
!
!  Discussion:
!
!    LOWER is a string function of undeclared length.  The length
!    of the argument returned is determined by the declaration of
!    LOWER in the calling routine.
!
!  Modified:
!
!    11 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, a string.
!
!    Output, character ( len = * ) LOWER, a lowercase copy of the string.
!
   implicit none

   integer i
   integer j
   character ( len = * ) s
   character ( len = len(s) ) lower
   integer n

   lower = s
   n = len_trim ( lower )
   do i = 1, n
      j = ichar ( lower(i:i) )
      if ( 65 <= j .and. j <= 90 ) then
        lower(i:i) = char ( j + 32 )
      end if
   end do

   end function lower

 !----------------------------------------------------------------------------------------------------

   function s_cap_word ( s )   result(s_cap)
!
!  Capitalizes the first character in a word s
!
   character ( len = * ), intent(in) :: s
   integer                           :: i
   character ( len = len(s) )        :: s_cap
   integer, parameter                :: inc = ICHAR("A") - ICHAR("a")
   integer                           :: n
   character, parameter              :: TAB = char ( 9 )

   s_cap = s
   n = len_trim ( s_cap )
   do i = 1, n
      select case (s_cap(i:i))
        case ('a':'z')
         s_cap(i:i) = char( ichar(s_cap(i:i)) + inc )
         exit
        case (' ',TAB)
        case default
         exit
      end select
   end do

   end function s_cap_word

 !----------------------------------------------------------------------------------------------------

   integer function s_to_r ( s, r, slen )  result(ierror)

!*******************************************************************************
!
!! S_TO_R reads a real number from a string.
!
!  Discussion:
!
!    This routine will read as many characters as possible until it reaches
!    the end of the string, or encounters a character which cannot be
!    part of the real number.
!
!    Legal input is:
!
!       1 blanks,
!       2 '+' or '-' sign,
!       2.5 spaces
!       3 integer part,
!       4 decimal point,
!       5 fraction part,
!       6 'E' or 'e' or 'D' or 'd', exponent marker,
!       7 exponent sign,
!       8 exponent integer part,
!       9 exponent decimal point,
!      10 exponent fraction part,
!      11 blanks,
!      12 final comma or semicolon.
!
!    with most quantities optional.
!
!  Examples:
!
!    S                 R
!
!    '1'               1.0
!    '     1   '       1.0
!    '1A'              1.0
!    '12,34,56'        12.0
!    '  34 7'          34.0
!    '-1E2ABCD'        -100.0
!    '-1X2ABCD'        -1.0
!    ' 2E-1'           0.2
!    '23.45'           23.45
!    '-4.2E+2'         -420.0
!    '17d2'            1700.0
!    '-14e-2'         -0.14
!    'e2'              100.0
!    '-12.73e-9.23'   -12.73 * 10.0**(-9.23)
!
!  Modified:
!
!    12 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string containing the
!    data to be read.  Reading will begin at position 1 and
!    terminate at the end of the string, or when no more
!    characters can be read to form a legal real.  Blanks,
!    commas, or other nonnumeric data will, in particular,
!    cause the conversion to halt.
!
!    Output, real R, the real value that was read from the string.
!
!    Output, integer IERROR, error flag.
!
!    0, no errors occurred.
!
!    1, 2, 6 or 7, the input number was garbled.  The
!    value of IERROR is the last type of input successfully
!    read.  For instance, 1 means initial blanks, 2 means
!    a plus or minus sign, and so on.
!
!    Output, integer LENGTH, the number of characters read from
!    the string to form the number, including any terminating
!    characters such as a trailing comma or blanks.
!
   implicit none

   character c
!corr   integer ierror
   integer ihave
   integer isgn
   integer iterm
   integer jbot
   integer jsgn
   integer jtop
   integer length
   integer nchar
   integer ndig
   real r
   real rbot
   real rexp
   real rtop
   character ( len = * ) s
   character, parameter :: TAB = char ( 9 )
   integer, intent(out), optional :: slen

   nchar = len_trim ( s )
   ierror = 0
   r = 0.0E+00
   length = -1
   isgn = 1
   rtop = 0.0E+00
   rbot = 1.0E+00
   jsgn = 1
   jtop = 0
   jbot = 1
   ihave = 1
   iterm = 0

   do

     length = length + 1
     c = s(length+1:length+1)
!
!   Blank or TAB character.
!
     if ( c == ' ' .or. c == TAB ) then

       if ( ihave == 2 ) then

       else if ( ihave == 6 .or. ihave == 7 ) then
         iterm = 1
       else if ( 1 < ihave ) then
         ihave = 11
       end if
!
!   Comma.
!
     else if ( c == ',' .or. c == ';' ) then

       if ( ihave /= 1 ) then
         iterm = 1
         ihave = 12
         length = length + 1
       end if
!
!   Minus sign.
!
     else if ( c == '-' ) then

       if ( ihave == 1 ) then
         ihave = 2
         isgn = -1
       else if ( ihave == 6 ) then
         ihave = 7
         jsgn = -1
       else
         iterm = 1
       end if
!
!   Plus sign.
!
     else if ( c == '+' ) then

       if ( ihave == 1 ) then
         ihave = 2
       else if ( ihave == 6 ) then
         ihave = 7
       else
         iterm = 1
       end if
!
!   Decimal point.
!
     else if ( c == '.' ) then

       if ( ihave < 4 ) then
         ihave = 4
       else if ( 6 <= ihave .and. ihave <= 8 ) then
         ihave = 9
       else
         iterm = 1
       end if
!
!   Exponent marker.
!
     else if ( ch_eqi ( c, 'E' ) .or. ch_eqi ( c, 'D' ) ) then

       if ( ihave < 6 ) then
         ihave = 6
       else
         iterm = 1
       end if
!
!   Digit.
!
     else if ( ihave < 11 .and. lle ( '0', c ) .and. lle ( c, '9' ) ) then

       if ( ihave <= 2 ) then
         ihave = 3
       else if ( ihave == 4 ) then
         ihave = 5
       else if ( ihave == 6 .or. ihave == 7 ) then
         ihave = 8
       else if ( ihave == 9 ) then
         ihave = 10
       end if

       call ch_to_digit ( c, ndig )

       if ( ihave == 3 ) then
         rtop = 10.0E+00 * rtop + real ( ndig )
       else if ( ihave == 5 ) then
         rtop = 10.0E+00 * rtop + real ( ndig )
         rbot = 10.0E+00 * rbot
       else if ( ihave == 8 ) then
         jtop = 10 * jtop + ndig
       else if ( ihave == 10 ) then
         jtop = 10 * jtop + ndig
         jbot = 10 * jbot
       end if
!
!   Anything else is regarded as a terminator.
!
     else
       iterm = 1
     end if
!
!   If we haven't seen a terminator, and we haven't examined the
!   entire string, go get the next character.
!
     if ( iterm == 1 .or. nchar <= length+1 ) then
       exit
     end if

   end do
!
!   If we haven't seen a terminator, and we have examined the
!   entire string, then we're done, and LENGTH is equal to NCHAR.
!
   if ( iterm /= 1 .and. length+1 == nchar ) then
     length = nchar
   end if
!
!   Number seems to have terminated.  Have we got a legal number?
!   Not if we terminated in states 1, 2, 6 or 7!
!
   if ( ihave == 1 .or. ihave == 2 .or. ihave == 6 .or. ihave == 7 ) then

     ierror = ihave

     if (present(slen)) slen = length
     return
   end if
!
!   Number seems OK.  Form it.
!
   if ( jtop == 0 ) then
     rexp = 1.0E+00
   else

     if ( jbot == 1 ) then
       rexp = 10.0E+00**( jsgn * jtop )
     else
       rexp = jsgn * jtop
       rexp = rexp / jbot
       rexp = 10.0E+00**rexp
     end if

   end if

   r = isgn * rexp * rtop / rbot

   if (present(slen)) slen = length

   end function s_to_r

 !----------------------------------------------------------------------------------------------------

   subroutine s_to_i ( s, ival, ierror, length )

!*******************************************************************************
!
!! S_TO_I reads an integer value from a string.
!
!  Modified:
!
!    28 June 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, a string to be examined.
!
!    Output, integer IVAL, the integer value read from the string.
!    If the string is blank, then IVAL will be returned 0.
!
!    Output, integer IERROR, an error flag.
!    0, no error.
!    1, an error occurred.
!
!    Output, integer LENGTH, the number of characters of S used to make IVAL.
!
   implicit none

   character c
   integer i
   integer, intent(out) :: ierror
   integer isgn
   integer istate
   integer, intent(out) ::  ival
   integer, intent(out), optional :: length
   character ( len = * ), intent(in) :: s

   ierror = 0
   istate = 0
   isgn = 1
   ival = 0

   do i = 1, len_trim ( s )

     c = s(i:i)
!
!   Haven't read anything.
!
     if ( istate == 0 ) then

       if ( c == ' ' ) then

       else if ( c == '-' ) then
         istate = 1
         isgn = -1
       else if ( c == '+' ) then
         istate = 1
         isgn = + 1
       else if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then
         istate = 2
         ival = ichar ( c ) - ichar ( '0' )
       else
         ierror = 1
         return
       end if
!
!   Have read the sign, expecting digits.
!
     else if ( istate == 1 ) then

       if ( c == ' ' ) then

       else if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then
         istate = 2
         ival = ichar ( c ) - ichar ( '0' )
       else
         ierror = 1
         return
       end if
!
!   Have read at least one digit, expecting more.
!
     else if ( istate == 2 ) then

       if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then
         ival = 10 * ival + ichar ( c ) - ichar ( '0' )
       else
         ival = isgn * ival
         if (present(length)) length = i - 1
         return
       end if

     end if

   end do
!
!   If we read all the characters in the string, see if we're OK.
!
   if ( istate == 2 ) then
     ival = isgn * ival
     if (present(length)) length = len_trim ( s )
   else
     ierror = 1
     if (present(length)) length = 0
   end if

   end subroutine s_to_i

 !---------------------------------------------------------------------------------------------

   integer function s_to_r_perc(str,rnum)  result(ier)
!
!  Convert percentage in fraction. If '%' is absent string is read as fraction
!  Es.
!  (in)  50%  -> (out) 0.50
!  (in) 0.50  -> (out) 0.50
!
   character(len=*), intent(in) :: str
   real, intent(out)            :: rnum
   integer                      :: lent
!
   ier = 1
   lent = len_trim(str)
   if (lent > 0) then
       if (str(lent:lent) == '%') then
           if (lent > 1) then
               ier = s_to_r(str(1:lent-1),rnum)
               if (ier == 0) rnum = rnum / 100
           endif
       else
           ier = s_to_r(str(1:lent),rnum)
       endif
       if (ier == 0) then
           if (rnum > 1 .or. rnum < 0) ier = 2
       endif
   endif
!
   end function s_to_r_perc

 !---------------------------------------------------------------------------------------------

   function ch_eqi ( c1, c2 )

   !*******************************************************************************
   !
   !! CH_EQI is a case insensitive comparison of two characters for equality.
   !
   !  Examples:
   !
   !    CH_EQI ( 'A', 'a' ) is TRUE.
   !
   !  Modified:
   !
   !    28 July 2000
   !
   !  Author:
   !
   !    John Burkardt
   !
   !  Parameters:
   !
   !    Input, character C1, C2, the characters to compare.
   !
   !    Output, logical CH_EQI, the result of the comparison.
   !
     implicit none

     character c1
     character c1_cap
     character c2
     character c2_cap
     logical ch_eqi

     c1_cap = c1
     c2_cap = c2

     call ch_cap ( c1_cap )
     call ch_cap ( c2_cap )

     if ( c1_cap == c2_cap ) then
       ch_eqi = .true.
     else
       ch_eqi = .false.
     end if

   end function ch_eqi

 !---------------------------------------------------------------------------------------------

   function s_eqi ( s1, s2 )

   !*******************************************************************************
   !
   !! S_EQI is a case insensitive comparison of two strings for equality.  
   !
   !  Examples:
   !
   !    S_EQI ( 'Anjana', 'ANJANA' ) is TRUE.
   !
   !  Modified:
   !
   !    14 April 1999
   !
   !  Author:
   !
   !    John Burkardt
   !
   !  Parameters:
   !
   !    Input, character ( len = * ) S1, S2, the strings to compare.
   !
   !    Output, logical S_EQI, the result of the comparison.
   !
   implicit none

   character c1
   character c2
   integer i
   integer len1
   integer len2
   integer lenc
   logical s_eqi
   character ( len = * ), intent(in) ::  s1
   character ( len = * ), intent(in) ::  s2

   len1 = len ( s1 )
   len2 = len ( s2 )
   lenc = min ( len1, len2 )
 
   s_eqi = .false.

   do i = 1, lenc

     c1 = s1(i:i)
     c2 = s2(i:i)
     call ch_cap ( c1 )
     call ch_cap ( c2 )

     if ( c1 /= c2 ) then
       return
     end if

   end do

   do i = lenc + 1, len1
     if ( s1(i:i) /= ' ' ) then
       return
     end if
   end do

   do i = lenc + 1, len2
     if ( s2(i:i) /= ' ' ) then
       return
     end if
   end do

   s_eqi = .true.

   return
   end function s_eqi

 !---------------------------------------------------------------------------------------------

   function s_eqidb ( s1, s2 )

!*******************************************************************************
!
!! S_EQIDB compares two strings, ignoring case and blanks.
!
!  Examples:
!
!    S_EQIDB ( 'Nor Way', 'NORway' ) is TRUE.
!
!  Modified:
!
!    14 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Modified:
!
!    19 July 1998
!
!  Parameters:
!
!    Input, character ( len = * ) S1, S2, the strings to compare.
!
!    Output, logical S_EQIDB, the result of the comparison.
!
   character c1
   character c2
   integer i1
   integer i2
   integer len1
   integer len2
   logical s_eqidb
   character ( len = * ) s1
   character ( len = * ) s2
!
!  Get the length of each string to the last nonblank.
!
   len1 = len_trim ( s1 )
   len2 = len_trim ( s2 )
!
!  Assume we're going to fail.
!
   s_eqidb = .false.
!
!  Initialize the pointers to characters in each string.
!
   i1 = 0
   i2 = 0
 
   do
!
!  If we've matched all the nonblank characters in both strings,
!  then return with S_EQIDB = TRUE.
!
    if ( i1 == len1 .and. i2 == len2 ) then
      s_eqidb = .true.
      return
    end if
!
!  Get the next nonblank character in the first string.
!
    do
 
      i1 = i1 + 1
 
      if ( len1 < i1 ) then
        return
      end if
 
      if ( s1(i1:i1) /= ' ' ) then
        exit
      end if
 
    end do
 
    c1 = s1(i1:i1)
    call ch_cap ( c1 )
!
!  Get the next nonblank character in the second string.
!
    do
 
      i2 = i2 + 1
      if ( len2 < i2 ) then
        return
      end if
 
      c2 = s2(i2:i2)

      if ( c2 /= ' ' ) then
        exit
      end if
 
    end do
 
    call ch_cap ( c2 )
 
    if ( c1 /= c2 ) then
      exit
    end if
 
   end do

   return
   end function s_eqidb

 !---------------------------------------------------------------------------------------------

   subroutine ch_cap ( c )

!*******************************************************************************
!
!! CH_CAP capitalizes a single character.
!
!  Modified:
!
!    19 July 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character C, the character to capitalize.
!
   implicit none

   character c
   integer itemp

   itemp = ichar ( c )

   if ( 97 <= itemp .and. itemp <= 122 ) then
     c = char ( itemp - 32 )
   end if

   end subroutine ch_cap

 !---------------------------------------------------------------------------------------------
   subroutine ch_to_digit ( c, digit )

!*******************************************************************************
!
!! CH_TO_DIGIT returns the integer value of a base 10 digit.
!
!  Example:
!
!     C   DIGIT
!    ---  -----
!    '0'    0
!    '1'    1
!    ...  ...
!    '9'    9
!    ' '    0
!    'X'   -1
!
!  Modified:
!
!    04 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character C, the decimal digit, '0' through '9' or blank
!    are legal.
!
!    Output, integer DIGIT, the corresponding integer value.  If C was
!    'illegal', then DIGIT is -1.
!
   implicit none

   character, intent(in) :: c
   integer, intent(out)  :: digit

   if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then

     digit = ichar ( c ) - 48

   else if ( c == ' ' ) then

     digit = 0

   else

     digit = -1

   end if

   end subroutine ch_to_digit


 !----------------------------------------------------------------------------------------------------

   function centra_str(string,n)  result(str)
!
!  Centra la stringa in un campo di ampiezza n
!
   implicit none
   integer, intent(in)             :: n
   character(len=*), intent(in)    :: string
   character(len=len_trim(string)) :: stringc
   character(len=n)                :: str
   integer                         :: nlt
!
   stringc = adjustl(string)
   nlt = len_trim(stringc)
   if (n > nlt) then
       str = ' '
       str(nint(n/2.0)-nint(nlt/2.0)+1:) = stringc
   else
       str = stringc(1:n)
   endif
!
   end function centra_str 
   
!----------------------------------------------------------------------------------------------------

function s_center ( string )

!
!! S_CENTER centers the non-blank portion of a string.
!
!  Modified:
!
!    07 October 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, character ( len = * ) STRING.  On input, a string to be
!    centered.  On output, the centered string.
!

  integer l1
  integer l2
  integer n1
  integer n2
  integer n3
  character ( len = * ), intent(in) ::  string
  character (len=len(string))       :: s_center
!
  s_center = string
!
!  How much space is in the string?
!
  n1 = len ( s_center )
!
!  Shift the string flush left and find the last nonblank.
!
  s_center = adjustl ( s_center )
  n2 = len_trim ( s_center )

  if ( n2 <= 0 ) then
    return
  end if

  if ( n2 == n1 .or. n2 == n1-1 ) then
    return
  end if

  n3 = n1 - n2
  l1 = n3 / 2
  l2 = l1 + n2 + 1

  s_center(l1+1:l2-1) = s_center(1:n2)

  s_center(1:l1) = ' '
  s_center(l2:n1) = ' '

  end function s_center

!----------------------------------------------------------------------------------------------------
  
   function catstringpar_r(string,par,fmtt) result(strr)
   character(len=*), intent(in)              :: string
   real, intent(in)                          :: par
   character(len=*), intent(in)              :: fmtt
   integer, parameter                        :: maxlens = 100
   character(len=maxlens)                    :: strp
   character(len=len_trim(string)+maxlens) :: strr   
   integer                                   :: ier
!
   write(strp,fmt=fmtt,iostat=ier)par  
!   
! c'� errore se il formato non � compatibile con la lunghezza di strp
   if (ier == 0) then
       strr = trim(string)//trim(adjustl(strp))
   else
       strr = '*'
   endif
!
   end function catstringpar_r     
   
!----------------------------------------------------------------------------------------------------
  
   function catstringpar_i(string,par,fmtt) result(strr)
   character(len=*), intent(in)            :: string
   integer, intent(in)                     :: par
   character(len=*), intent(in)            :: fmtt
   integer, parameter                      :: maxlens = 200
   character(len=maxlens)                  :: strp
   character(len=len_trim(string)+maxlens) :: strr   
   integer                                 :: ier
!
   write(strp,fmt=fmtt,iostat=ier)par  
!   
!  c'� errore se il formato non � compatibile con la lunghezza di strp
   if (ier == 0) then
       strr = trim(string)//trim(adjustl(strp))
   else
       strr = '*'
   endif
!   
   end function catstringpar_i        
   
!----------------------------------------------------------------------------------------------------
  
   function catstringpar_vetr(string,par,fmtt) result(strr)
   character(len=*), intent(in)              :: string
   real, dimension(:), intent(in)            :: par
   character(len=*), intent(in)              :: fmtt
   integer, parameter                        :: maxlens = 200
   character(len=maxlens)                    :: strp
   character(len=len_trim(string)+maxlens) :: strr   
   integer                                   :: ier
!
   write(strp,fmt=fmtt,iostat=ier)par  
!   
! c'� errore se il formato non � compatibile con la lunghezza di strp
   if (ier == 0) then
       strr = trim(string)//trim(adjustl(strp))
   else
       strr = '*'
   endif
!
   end function catstringpar_vetr     
   
!----------------------------------------------------------------------------------------------------
  
   function catstringpar_veti(string,par,fmtt) result(strr)
   character(len=*), intent(in)            :: string
   integer, dimension(:), intent(in)       :: par
   character(len=*), intent(in)            :: fmtt
   integer, parameter                      :: maxlens = 200
   character(len=maxlens)                  :: strp
   character(len=len_trim(string)+maxlens) :: strr   
   integer                                 :: ier
!
   write(strp,fmt=fmtt,iostat=ier)par  
!   
! c'� errore se il formato non � compatibile con la lunghezza di strp
   if (ier == 0) then
       strr = trim(string)//trim(adjustl(strp))
   else
       strr = '*'
   endif
!
   end function catstringpar_veti
      
!----------------------------------------------------------------------------------------------------

   function number_to_format_veti(vet) result(fformat)
   integer, dimension(:), intent(in) :: vet
   integer                           :: ns
   character(len=100)                :: fformat
!
   ns = size(vet)
   if (ns == 1) then
       write(fformat,'(a,i0,a)')'(i0)'
   elseif (ns > 1) then
       write(fformat,'(a,i0,a)')'(',ns,'(i0,1x))'
   endif
!   
   end function number_to_format_veti

!----------------------------------------------------------------------------------------------------
!old
!old   function position_stringv(str,stringv,back) result(pos)
!old!   
!old!  Cerca la prima posizione di str nel vettore stringv 
!old!  (funzione obsoleta da sostituire con la string_locate)
!old!
!old   character(len=*), intent(in)               :: str
!old   character(len=*), dimension(:), intent(in) :: stringv
!old   logical, intent(in), optional              :: back
!old   integer                                    :: pos
!old   character(len=len_trim(str))               :: str0,str1
!old   integer                                    :: i,ini,fin,step
!old   integer                                    :: lens
!old!
!old   lens = len_trim(str)
!old   str0 = u_case(trim(str))
!old   pos = 0
!old   ini = 1
!old   fin = size(stringv)
!old   step = 1
!old   if (present(back)) then
!old       if (back) then
!old           ini = size(stringv)
!old           fin = 1
!old           step = -1
!old       endif
!old   endif
!old!
!old   do i=ini,fin,step
!old      if (len_trim(stringv(i)) /= lens) cycle
!old      str1 = u_case(trim(stringv(i)))
!old      if (str1 == str0) then
!old          pos = i
!old          exit
!old      endif
!old   enddo
!old!   
!old   end function position_stringv
!old
!----------------------------------------------------------------------------------------------------

   function s_is_i ( s, i )

!*******************************************************************************
!
!! S_IS_I is TRUE if a string represents an integer.
!
!  Modified:
!
!    14 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to be checked.
!
!    Output, integer I.  If the string represents an integer, I is the 
!    integer represented.  Otherwise I is 0.
!
!    Output, logical S_IS_I, is TRUE if the string represents an integer.
!
   integer i
   integer ierror
   integer length
   integer lenc
   character ( len = * ) s
   logical s_is_i
  
   lenc = len_trim ( s )
   
   call s_to_i ( s, i, ierror, length )
   
   if ( ierror == 0 .and. lenc <= length ) then
     s_is_i = .true.
   else
     s_is_i = .false.
     i = 0
   end if
   
   return
   end function s_is_i

!----------------------------------------------------------------------------------------------------

   subroutine s_is_r ( s, r, lval )

!*******************************************************************************
!
!! S_IS_R is TRUE if a string represents a real number.
!
!  Modified:
!
!    14 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to be checked.
!
!    Output, real R.  If the string represents a real number, then R
!    is the real number represented.  Otherwise R is 0.
!
!    Output, logical LVAL, is TRUE if the string represents a real number.
!
   implicit none

   integer ierror
   integer lenc
   integer length
   logical lval
   real r
   character ( len = * ) s

   lenc = len_trim ( s )
  
   ierror =  s_to_r ( s, r, length )
  
   if ( ierror == 0 .and. lenc <= length ) then
     lval = .true.
   else
     lval = .false.
     r = 0.0E+00
   end if
  
   return
   end subroutine s_is_r

!----------------------------------------------------------------------------------------------------

   function s_is_digit ( s )

!*******************************************************************************
!
!! S_IS_DIGIT returns TRUE if a string contains only decimal digits.
!
!  Discussion:
!
!    This is a strict comparison.  
!    The check is made from the first character to the last nonblank.
!    Each character in between must be one of '0', '1', ..., '9'.
!
!  Modified:
!
!    14 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to be checked.
!
!    Output, logical S_IS_DIGIT, is TRUE if S contains only digits.
!
   character c
   integer i
   integer lenc
   character ( len = * ) s
   logical s_is_digit

   lenc = len_trim ( s )

   s_is_digit = .false.

   do i = 1, lenc
     c = s(i:i)
     if ( llt ( c, '0' ) .or. lgt ( c, '9' ) ) then
       return
     end if
   end do
  
   s_is_digit = .true.

   return
   end function s_is_digit

!----------------------------------------------------------------------------------------------------

function s_is_alpha ( s )
!
!*******************************************************************************
!
!! S_IS_ALPHA returns .TRUE. if the string contains only alphabetic characters.
!
!
!  Discussion:
!
!    Here, alphabetic characters are 'A' through 'Z' and 'a' through 'z'.
!
!  Modified:
!
!    14 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to be checked.
!
!    Output, logical S_IS_ALPHA, .TRUE. if the string contains only
!    alphabetic characters, .FALSE. otherwise.
!
  implicit none
!
  integer i
  character ( len = * ) s
  logical s_is_alpha
!
  s_is_alpha = .false.

  do i = 1, len ( s )

    if ( .not. ch_is_alpha ( s(i:i) ) ) then
      return
    end if

  end do
 
  s_is_alpha = .true.

end function s_is_alpha

function s_is_alphanumeric ( s )

!*******************************************************************************
!
!! S_IS_ALPHANUMERIC = string contains only alphanumeric characters.
!
!  Discussion:
!
!    Alphanumeric characters are 'A' through 'Z', 'a' through 'z' and
!    '0' through '9'.
!
!  Modified:
!
!    14 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, the string to be checked.
!
!    Output, logical S_IS_ALPHANUMERIC, is TRUE if the string contains only
!    alphabetic characters and numerals.
!
  implicit none

  integer i
  integer itemp
  character ( len = * ) s
  logical s_is_alphanumeric

  s_is_alphanumeric = .false.
 
  do i = 1, len ( s )

    itemp = ichar ( s(i:i) )
 
    if ( .not. ( 65 <= itemp .and. itemp <= 90 ) ) then
      if ( .not. ( 97 <= itemp .and. itemp <= 122 ) ) then
        if ( .not. ( 48 <= itemp .and. itemp <= 57 ) ) then
          return
        end if
      end if
    end if
 
  end do
 
  s_is_alphanumeric = .true.

  return
end function s_is_alphanumeric

function ch_is_alpha ( c )
!
!*******************************************************************************
!
!! CH_IS_ALPHA returns TRUE if C is an alphabetic character.
!
!
!  Modified:
!
!    05 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character C, a character to check.
!
!    Output, logical CH_IS_ALPHA is TRUE if C is an alphabetic character.
!
  implicit none
!
  character c
  logical ch_is_alpha
!
  if ( ( lle ( 'a', c ) .and. lle ( c, 'z' ) ) .or. &
       ( lle ( 'A', c ) .and. lle ( c, 'Z' ) ) ) then
    ch_is_alpha = .true.
  else
    ch_is_alpha = .false.
  end if
end function ch_is_alpha

!----------------------------------------------------------------------------------------------------

function ch_is_control ( c )

!*******************************************************************************
!
!! CH_IS_CONTROL is TRUE if C is a control character.
!
!  Definition:
!
!    A "control character" has ASCII code <= 31 or 127 <= ASCII code.
!
!  Modified:
!
!    14 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character C, the character to be tested.
!
!    Output, logical CH_IS_CONTROL, TRUE if C is a control character, and
!    FALSE otherwise.
!
  implicit none

  character c
  logical ch_is_control

  if ( ichar ( c ) <= 31 .or. 127 <= ichar ( c ) ) then
    ch_is_control = .true.
  else
    ch_is_control = .false.
  end if

  return
end function ch_is_control

!----------------------------------------------------------------------------------------------------

function ch_is_digit ( c )

!*******************************************************************************
!
!! CH_IS_DIGIT is TRUE if C is a decimal digit.
!
!  Modified:
!
!    09 August 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character C, the character to be analyzed.
!
!    Output, logical CH_IS_DIGIT, is TRUE if C is a digit.
!
  implicit none

  character c
  logical ch_is_digit

  if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then
    ch_is_digit = .true.
  else
    ch_is_digit = .false.
  end if

  return
end function ch_is_digit

!----------------------------------------------------------------------------------------------------

function ch_is_upper ( c )

!*******************************************************************************
!
!! CH_IS_UPPER is TRUE if C is an upper case letter.
!
!  Modified:
!
!    02 May 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character C, the character to be analyzed.
!
!    Output, logical CH_IS_UPPER, is TRUE if C is an upper case letter.
!
  implicit none

  character c
  logical ch_is_upper

  if ( lle ( 'A', c ) .and. lle ( c, 'Z' ) ) then
    ch_is_upper = .true.
  else
    ch_is_upper = .false.
  end if
 
  return
end function ch_is_upper

!----------------------------------------------------------------------------------------------------

function ch_is_lower ( c )

!*******************************************************************************
!
!! CH_IS_LOWER is TRUE if C is a lower case letter.
!
!  Modified:
!
!    02 May 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character C, the character to be analyzed.
!
!    Output, logical CH_IS_LOWER, is TRUE if C is a lower case letter.
!
  implicit none

  character c
  logical ch_is_lower

  if ( lle ( 'a', c ) .and. lle ( c, 'z' ) ) then
    ch_is_lower = .true.
  else
    ch_is_lower = .false.
  end if
 
  return
end function ch_is_lower

!----------------------------------------------------------------------------------------------------

function ch_is_space ( c )

!*******************************************************************************
!
!! CH_IS_SPACE is TRUE if C is a whitespace character.
!
!  Discussion:
!
!    A whitespace character is a space, a form feed, a newline,
!    a carriage return, a tab, or a vertical tab.
!
!  Modified:
!
!    02 October 2004
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character C, a character to check.
!
!    Output, logical CH_IS_SPACE is TRUE if C is a whitespace character.
!
  implicit none

  character c
  logical ch_is_space

  if ( c == ' ' ) then
    ch_is_space = .true.
  else if ( c == char ( 12 ) ) then
    ch_is_space = .true.
  else if ( c == char ( 10 ) ) then
    ch_is_space = .true.
  else if ( c == char ( 13 ) ) then
    ch_is_space = .true.
  else if ( c == char ( 9 ) ) then
    ch_is_space = .true.
  else if ( c == char ( 11 ) ) then
    ch_is_space = .true.
  else
    ch_is_space = .false.
  end if

  return
end function ch_is_space

  subroutine s_detag ( s )
!
!*******************************************************************************
!
!! S_DETAG removes from a string all substrings marked by brackets.
!
!
!  Example:
!
!    Input:
!
!      S = '10.008(0.001)  2.001(0.003)'
!
!    Output:
!
!      S = '10.008  2.001'
!
!  Discussion:
!
!    This routine was written to help extract some data that was hidden
!    inside an elaborate HTML table.
!
!  Modified:
!
!    27 September 2008
!
!  Author:
!
!    John Burkardt, Corrado Cuocci
!
!  Parameters:
!
!    Input/output, character ( len = * ) S, the string to be transformed.
!
    implicit none
!
    integer i1
    integer i2
    integer i3
    character ( len = * ) s
!
    do
  
      i3 = len_trim ( s )
  
      if ( i3 == 0 ) exit
  
      i1 = index ( s, "(" )
  
      if ( i1 <= 0 .or. i1 >= i3 ) exit
  
      i2 = index ( s(i1+1:), ")" )
  
      if ( i2 == 0 ) exit
  
      i2 = i2 + i1
!
!  Shift.
!
      s(i1:i3+i1-i2-1) = s(i2+1:i3)
!
!  Pad.
!
      s(i3+i1-i2:) = ' '
  
    end do
  
  return
  end subroutine s_detag

  function f_detag(str)
  character(len=*), intent(in) :: str
  character(len=len_trim(str)) :: f_detag
  f_detag = trim(str)
  call s_detag(f_detag)
  end function f_detag

  subroutine s_detags ( s )
!
!*******************************************************************************
!
!! S_DETAG removes from a string all substrings marked by square brackets.
!
!
    implicit none
!
    integer i1
    integer i2
    integer i3
    character ( len = * ) s
!
    do
  
      i3 = len_trim ( s )
  
      if ( i3 == 0 ) exit
  
      i1 = index ( s, "[" )
  
      if ( i1 <= 0 .or. i1 >= i3 ) exit
  
      i2 = index ( s(i1+1:), "]" )
  
      if ( i2 == 0 ) exit
  
      i2 = i2 + i1
!
!  Shift.
!
      s(i1:i3+i1-i2-1) = s(i2+1:i3)
!
!  Pad.
!
      s(i3+i1-i2:) = ' '
  
    end do
  
  return
  end subroutine s_detags

      SUBROUTINE DPSTR ( X, SIGDIG, STRING )
 
! Abstract
!
!     Take a double precision number and convert it to
!     an equivalent character string representation (base 10).
!
! Required_Reading
!
!     None.
!
! Keywords
!
!      CHARACTER,  CONVERSION,  PARSING
!
! Declarations
 
      DOUBLE PRECISION X
      INTEGER          SIGDIG
      CHARACTER*(*)    STRING
 
! Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     X          I   A double precision number
!     SIGDIG     I   The number of significant digits saved for output
!     STRING     O   A character string representation of X
!
! Detailed_Input
!
!     X          is a double precision number.
!
!     SIGDIG     is the number of significant digits that are desired
!                for the output string.
!
!
!     Local variables
!
 
      DOUBLE PRECISION POWER (0:17)
      DOUBLE PRECISION IPOWER(0:17)
 
      CHARACTER*1      DIGITS(0:9)
      DOUBLE PRECISION VALUES(0:9)
!corr
      integer, parameter :: nmaxexp = 40
!corr
      CHARACTER*2      VAXEXP(0:nmaxexp)
 
 
 
      DOUBLE PRECISION COPY
      DOUBLE PRECISION FACTOR
      INTEGER          POSTN
      INTEGER          EXPONT
      INTEGER          MAXSIG
 
      DOUBLE PRECISION EXP10
 
      INTEGER          I
      INTEGER          K
      INTEGER          LAST
 
      CHARACTER*28     ZERO
 
      CHARACTER*32     NUMSTR
 
      SAVE             POWER
      SAVE             IPOWER
      SAVE             DIGITS
      SAVE             VALUES
      SAVE             VAXEXP
 
      DATA             POWER        /  1.0D0,                              &
                                      1.0D1,  1.0D2,  1.0D3,  1.0D4,       &
                                      1.0D5,  1.0D6,  1.0D7,  1.0D8,       &
                                      1.0D9,  1.0D10, 1.0D11, 1.0D12,      &
                                      1.0D13, 1.0D14, 1.0D15, 1.0D16,      &
                                      1.0D17                          /
 
      DATA             IPOWER       /1.0D0,                                &
                                    1.0D-1,  1.0D-2,  1.0D-3,  1.0D-4,     &
                                    1.0D-5,  1.0D-6,  1.0D-7,  1.0D-8,     &
                                    1.0D-9,  1.0D-10, 1.0D-11, 1.0D-12,    &
                                    1.0D-13, 1.0D-14, 1.0D-15, 1.0D-16,    &
                                    1.0D-17                          /
 
      DATA             DIGITS       /  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'    /
 
      DATA             VALUES       /   0.0D0 ,  1.0D0 ,  2.0D0 ,      &
                                       3.0D0 ,  4.0D0 ,  5.0D0 ,       &
                                       6.0D0 ,  7.0D0 ,  8.0D0 ,       &
                                       9.0D0                     /
 
      DATA             VAXEXP       / '00', '01', '02', '03', '04',    &
                                     '05', '06', '07', '08', '09',     &
                                     '10', '11', '12', '13', '14',     &
                                     '15', '16', '17', '18', '19',     &
                                     '20', '21', '22', '23', '24',     &
                                     '25', '26', '27', '28', '29',     &
                                     '30', '31', '32', '33', '34',     &
                                     '35', '36', '37', '38', '39',     &
                                     '40'                          /
 
!
!     Transfer X to the local variable COPY and leave X alone for the
!     rest of the routine.
!
      COPY   = X
 
!
!     Wipe out anything sitting in NUMSTR
!
      NUMSTR = ' '
 
!
!     At least 1 significant digit is required.  The most allowed is 14.
!     MAXSIG is the integer in this range that is closest to SIGDIG.
!
      MAXSIG = MIN ( 14, MAX( 1, SIGDIG ) )
 
!
!     Examine COPY to see if its positive, zero, or negative.
!     This determines whether we need a minus sign and where the
!     decimal point needs to go in the output string.
!
      IF      ( COPY .LT. 0 ) THEN
 
         NUMSTR(1:1) = '-'
         COPY        =  - COPY
         POSTN       =  2
         NUMSTR(3:3) = '.'
 
      ELSE IF ( COPY .GT. 0 ) THEN
 
         NUMSTR(1:1) = ' '
         POSTN       =  2
         NUMSTR(3:3) = '.'
 
      ELSE
         ZERO        = ' 0.0000000000000000000000000'
         NUMSTR      = ZERO(1:MAXSIG+2) // 'E+00'
 
         STRING = NUMSTR
         RETURN
 
      END IF
 
 
!
!     We need a first guess at the exponent string.  Compute the LOG
!     base 10 of COPY
!
      EXP10  = DLOG10(COPY)
 
!
!     FACTOR will be a value that when multiplied times COPY will shift
!     the value of COPY to something between 1 and 10.  We initialize
!     it here and scale it as needed in the following IF block
!
      FACTOR = 1.0D0
 
      IF ( EXP10 .LT. 0 ) THEN
 
!
!        In this case the exponent will be negative.  We want the
!        largest integer exponent less than EXP10,  but the FORTRAN
!        INT function give the INTEGER closest EXP10 between EXP10
!        and zero.  As a result we have to subtract 1 from INT(EXP10).
!
         EXPONT =  INT(EXP10) - 1
         K      = -EXPONT
 
50001    IF       ( K .GT. 16 )   THEN
            FACTOR = FACTOR * 1.0D16
            K      = K - 16
            GO TO 50001
         END IF
 
         IF ( K .NE. 0 ) THEN
            FACTOR = FACTOR * POWER(K)
         END IF
 
 
      ELSE
 
         EXPONT = INT ( EXP10 )
         K      = EXPONT
 
50002    IF       ( K .GT. 16 )   THEN
            FACTOR = FACTOR * 1.0D-16
            K      = K - 16
            GO TO 50002
         END IF
 
         IF ( K .NE. 0 ) THEN
            FACTOR = FACTOR * IPOWER(K)
         END IF
 
 
      END IF
 
      COPY   = COPY * FACTOR
 
 
!
!     Round off the last significant digit.
!
      COPY   = (DNINT( COPY*POWER(MAXSIG-1)) + 0.125D0 ) * IPOWER(MAXSIG-1)
 
!
!     We might have accidently made copy as big as 10 by the
!     round off process.  If we did we need to divide by 10 and add 1
!     to the exponent value.  (COPY must always remain between 0 and 10)
!
      IF ( COPY .GE. 10.0D0 ) THEN
         COPY   = COPY * 1.0D-1
         EXPONT = EXPONT + 1
      END IF
!
!     Get the first digit of the decimal expansion of X.
!
      I                   = INT ( COPY )
      NUMSTR(POSTN:POSTN) = DIGITS(I)
 
      COPY                = ( COPY - VALUES(I) ) * 10.0D0
 
!
!     Set the string pointer to the next position and compute the
!     position of the last significant digit
!
      POSTN               = POSTN + 2
      LAST                = POSTN + MAXSIG - 1
 
!
!     Fetch digits until we fill in the last available slot for
!     significant digits.
!
50003 IF       ( POSTN  .LT. LAST )THEN
 
         I                   = INT ( COPY )
         NUMSTR(POSTN:POSTN) = DIGITS(I)
         COPY                = ( COPY - VALUES(I) ) * 10.0D0
         POSTN               = POSTN + 1
 
         GO TO 50003
      END IF
 
!
!     Tack on the exponent to the output.
!
      select case (expont)
          case (0:nmaxexp)
            NUMSTR(POSTN:) = 'E+' // VAXEXP (   EXPONT )
          case (-nmaxexp:-1)
            NUMSTR(POSTN:) = 'E-' // VAXEXP ( - EXPONT )
          case (nmaxexp+1:)
            NUMSTR(POSTN:) = 'INF'
          case (:-nmaxexp-1)
            NUMSTR(POSTN:) = '-INF'
      end select
!corr      IF ( EXPONT .GE. 0 ) THEN
!corr         NUMSTR(POSTN:) = 'E+' // VAXEXP (   EXPONT )
!corr      ELSE
!corr         NUMSTR(POSTN:) = 'E-' // VAXEXP ( - EXPONT )
!corr      END IF
 
      STRING = NUMSTR
 
!
!     That's all folks.
!
      RETURN
 
      end subroutine dpstr
 
      SUBROUTINE DPSTRF ( X, SIGDIG, FORMAT, STRING )
 
!  Abstract
!
!      Take a double precision number and convert it to an
!      equivalent formatted character string representation (base 10).
!
!  Required_Reading
!
!      None.
!
!  Keywords
!
!       CHARACTER
!       CONVERSION
!       PARSING
!
!  Declarations
 
      DOUBLE PRECISION      X
      INTEGER               SIGDIG
      CHARACTER*1           FORMAT
      CHARACTER*(*)         STRING
 
!  Brief_I/O
!
!       VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      X          I   A double precision number
!      SIGDIG     I   The number of significant digits saved for output
!      FORMAT     I   'E' for scientific, 'F' for floating point.
!      STRING     O   A character string representation of X
!
! Detailed_Input
!
!      X          is a double precision number.
!
!      SIGDIG     is the number of significant digits that are desired
!                 for the output string.
!
!      FORMAT     is a character flag that indicates how the double
!                 precision number should be represented.  The two
!                 acceptable inputs are 'E' and 'F'.  If the input
!                 is 'E' then the number will be displayed with an
!                 exponent in scientific notation. 
!
!!!!corrr      INTEGER               BSRCHC
 
!
!     Local variables
!
      INTEGER               NEXPS
      PARAMETER           ( NEXPS = 41 )
 
      CHARACTER*2           VAXEXP ( NEXPS )
      SAVE                  VAXEXP
 
      CHARACTER*2           EXP
 
      CHARACTER*42          ZERO
      CHARACTER*64          NUMSTR
      CHARACTER*64          NEWSTR
      INTEGER               EXPLOC
      INTEGER               SGNLOC
      INTEGER               EXPNDX
      INTEGER               MAXDIG
      integer               lens
 
      DATA                  VAXEXP      / '00', '01', '02', '03', '04',   &
                                          '05', '06', '07', '08', '09',   &
                                          '10', '11', '12', '13', '14',   &
                                          '15', '16', '17', '18', '19',   &
                                          '20', '21', '22', '23', '24',   &
                                          '25', '26', '27', '28', '29',   &
                                          '30', '31', '32', '33', '34',   &
                                          '35', '36', '37', '38', '39',   &
                                          '40'                        /
 
      MAXDIG = MIN ( 14, MAX(1,SIGDIG)       )
 
      CALL DPSTR  ( X,  MAXDIG, NUMSTR )
 
 
      IF ( FORMAT .EQ. 'E' ) THEN
 
         STRING = NUMSTR
 
      ELSE
 
         ZERO   = '0.0000000000000000000000000000000000000000'
         NEWSTR = ' '
!
!        Save the sign of this string and shift everything left 1 space.
!
         EXPLOC                =  MAXDIG + 3
         SGNLOC                =  MAXDIG + 4
         EXP                   =  NUMSTR(SGNLOC+1:SGNLOC+2)
 
 
!
!        Blank out the exponent character so that we can just append
!        the stuff after the decimal place upto and including the
!        exponent character.  (This usually avoids having to worry
!        about zero length substrings.)
!
         NUMSTR(EXPLOC:EXPLOC) = ' '
 
!
!        Determine the integer represented by EXP.
!
         EXPNDX = BSRCHC ( EXP, NEXPS, VAXEXP )
 
!
!        If the exponent was negative append zeros to the front of the
!        number.
!
         IF ( NUMSTR(SGNLOC:SGNLOC) .EQ. '-' ) THEN
 
            NEWSTR =  NUMSTR ( 1 : 1      )  //    &
                      ZERO   ( 1 : EXPNDX )  //    &
                      NUMSTR ( 2 : 2      )  //    &
                      NUMSTR ( 4 : EXPLOC )
 
 
!
!        If the exponent was zero, just lop off the exponent part.
!
         ELSE IF ( EXPNDX .EQ. 1 ) THEN
 
            NEWSTR = NUMSTR ( 1 : EXPLOC )
 
!
!        If the exponent was smaller than the number of mantissa
!        digits, just move the decimal point. (We're going to be
!        a bit sneaky here.  What we want to know is how many places
!        the decimal point should be moved right.  There are only
!        MAXDIG - 1 places to move it. So if the value of the
!        exponent is less than or equal to MAXDIG-1, we can
!        push the decimal point without attatching zeros to the number.
!        But the value of the exponent is EXPNDX - 1.  Thus
!        the condition we want to test is EXPNDX - 1 .LE. MAXDIG - 1.
!        This is of cource equivalent to EXPNDX .LE. MAXDIG)
!
         ELSE IF ( EXPNDX .LE. MAXDIG ) THEN
 
            NEWSTR = NUMSTR( 1       :        2)  //    &
                     NUMSTR( 4       : EXPNDX+2)  //    &
                     '.'                          //    &
                     NUMSTR( 3+EXPNDX: EXPLOC  )
 
         ELSE
!
!           We will have to put some zeros on after the last mantissa
!           digit.
!
!           How many?  We've got MAXDIG - 1 digits following
!                      the decimal point.  We can move the decimal
!                      point MAXDIG - 1 places before we run out of
!                      room
!
!                      We need to move the decimal point EXPNDX - 1
!                      places to the right.  Once we have run out of
!                      room for shifting the decimal place right
!                      (After MAXDIG - 1 shifts right) we will still
!                      need to shift the decimal point
!                      (EXPNDX - 1) - (MAXDIG - 1) places to the right.
!                      In other words we must add EXPNDX - MAXDIG zeros
!                      on after the last mantissa digit.
!
!                      (Note we have to worry about the zero-length
!                       strings in this case NUMSTR(4:EXPLOC-1) )
!
            IF ( EXPLOC .GT. 4 ) THEN
 
               NEWSTR = NUMSTR ( 1 :                   2 )      //    &
                        NUMSTR ( 4 :     EXPLOC -      1 )      //    &
                        ZERO   ( 3 : 2 + EXPNDX - MAXDIG )
 
            ELSE
 
               NEWSTR = NUMSTR ( 1 :                   2 )      //    &
                        ZERO   ( 3 : 2 + EXPNDX - MAXDIG )
 
            END IF
 
         END IF
 
         STRING = NEWSTR
 
      END IF
!
!     Elimina eventuale punto decimale alla fine
      lens = len_trim(string)
      if (string(lens:lens) == '.') string(lens:lens) = ' '
 
      RETURN
 
      end subroutine DPSTRF

      INTEGER FUNCTION BSRCHC ( VALUE, NDIM, ARRAY )
 
! Abstract
!
!     Do a binary search for a given value within a character array,
!     assumed to be in increasing order. Return the index of the
!     matching array entry, or zero if the key value is not found.
!
! Required_Reading
!
!    None.
!
! Keywords
!
!     ARRAY,  SEARCH
!
! Declarations
 
      CHARACTER*(*)    VALUE
      INTEGER          NDIM
      CHARACTER*(*)    ARRAY ( * )
 
! Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     VALUE      I   Value to find in ARRAY.
!     NDIM       I   Dimension of ARRAY.
!     ARRAY      I   Array to be searched.
!     BSRCHC     O   Index of VALUE in ARRAY. (Zero if not found.)
!
! Detailed_Input
!
!     VALUE       is the value to be found in the input array.
!
!     NDIM        is the number of elements in the input array.
!
!     ARRAY       is the array to be searched. The elements in
!                 ARRAY are assumed to sorted according to the
!                 ASCII collating sequence.
!
!
!    Local variables
!
      INTEGER          LEFT
      INTEGER          RIGHT
      INTEGER          I
 
 
 
!
!     Set the initial bounds for the search area.
!
      LEFT  = 1
      RIGHT = NDIM
 
50001 IF       ( LEFT .LE. RIGHT ) THEN
 
!
!        Check the middle element.
!
         I = (LEFT+RIGHT)/2
 
!
!        If the middle element matches, return its location.
!
         IF ( VALUE .EQ. ARRAY(I) ) THEN
            BSRCHC = I
            RETURN
 
!
!        Otherwise narrow the search area.
!
         ELSE IF ( LLT (VALUE, ARRAY(I)) ) THEN
            RIGHT = I-1
         ELSE
            LEFT  = I+1
         END IF
 
         GO TO 50001
      END IF
 
!
!     If the search area is empty, return zero.
!
      BSRCHC = 0
 
      RETURN
      end function BSRCHC

 !----------------------------------------------------------------------------------------------------

      function string_sigf(val,sigf)   result(str)
!
!     Genera stringa da val con sigf cifre significative
!
      real, intent(in)      :: val
      integer, intent(in)   :: sigf
      character(len=sigf+8) :: str
      real(8)               :: vald
      character(len=1)      :: cform
      integer               :: dig
!
      cform = 'F'
      vald = val
!
!     Se il numero e troppo grande rispetto a sigd
!     usa il formato esponenziale
      if (vald > 1.0) then
          dig = nint(dlog10(vald))
          if (dig > sigf) cform = 'E'
      endif
!
      call dpstrf(vald,sigf,cform,str)
      str = adjustl(str)
!
      end function string_sigf

 !----------------------------------------------------------------------------------------------------

      function string_sigd(val,sigd)   result(str)
!
!     Genera stringa da val con sigd cifre significative
!
      real(8), intent(in)   :: val
      integer, intent(in)   :: sigd
      character(len=sigd+8) :: str
      character(len=1)      :: cform
      integer               :: dig
!
      cform = 'F'
!
!     Se il numero e troppo grande rispetto a sigd
!     usa il formato esponenziale
      if (val > 1.0) then
          dig = nint(dlog10(val)) + 1
          if (dig > sigd) cform = 'E'
      endif
!
      call dpstrf(val,sigd,cform,str)
      str = adjustl(str)
!
      end function string_sigd

 !----------------------------------------------------------------------------------------------------

   function string_esd(val,std,dec)  result(str)
!
!  Create string: val(std)
!  std contains 2 significant digits only if the first digit is 1
!
   real, intent(in)              :: val,std
   character(len=:), allocatable :: str,strval,strstd
   integer, intent(in), optional :: dec   ! force decimal digit for std=0.
   integer                       :: ndig,nsigd,ldig
   real                          :: stdd
!
   if (abs(val) > 999999.0) then
       write(str,'(a)')val
       return
   endif
   if (abs(std) < epsilon(0.0)) then
       if (present(dec)) then
           str = r_to_s(val,dec)
       else
           str = string_sig(val,5) 
       endif
       return
   endif
!
   ldig = floor(log10(abs(std)))  
   ndig = abs(ldig)     ! position of the first digit
   if (std < 1) then     
       strstd = i_to_s(nint(std*10.0**(ndig+1)))
       if (strstd(1:1) == '1') then   ! first digit is 1?
           ndig = ndig + 1
       else               
           strstd = i_to_s(nint(std*10.0**(ndig)))
       endif
       nsigd = ndig
   else                    
       stdd = xround(std,2)
       if (first_digit(stdd) == 1) then
           if (stdd < 2) then                  ! es. std = 1.2
               strstd = i_to_s(nint(stdd*10))
               nsigd = 1
               if (ndig == 0) ndig = 1
           else
               strstd = i_to_s(nint(stdd))
               nsigd = 0
           endif
       else
           strstd = i_to_s(nint(xround(std,1)))  ! round to 1 significant digit
           nsigd = 0
       endif
   endif
!
   if (nsigd == 0) then
       strval = i_to_s(nint(val))
   else
       if (ndig > 7) then
           str = r_to_s(val,7)
           return
       else
           strval = r_to_s(val,ndig) 
       endif
   endif
!
!  finally create string
   str = trim(strval)//'('//trim(strstd)//')'
!
   end function string_esd

 !----------------------------------------------------------------------------------------------------
      
   integer function first_digit(rnum) result(fdigit)
!
!  First digit of an integer number
!
   real, intent(in) :: rnum
   real             :: len_num
   len_num = floor(log10(abs(rnum)))  ! this the length of number - 1
   fdigit = int(rnum / 10**len_num)
   end function first_digit
      
 !----------------------------------------------------------------------------------------------------

   integer function ndigits(num)
!
!  Count digits in a integer number
!
   integer, intent(in) :: num
   integer             :: num1
   num1 = abs(num)
   ndigits = 1
   do while (num1 >= 10) 
      num1 = num1/10
      ndigits = ndigits + 1
   enddo
   end function ndigits

 !----------------------------------------------------------------------------------------------------

      real function xround(num,sig)
!
!     Round a number to n significant digits
!
      real, intent(in)    :: num
      integer, intent(in) :: sig
      real                :: mult
      integer             :: power
!
      if (sig == 0) then
          xround = 0
          return
      endif
      if (num > 0) then
          power =floor(log10(num))
      else
          power =floor(log10(-num))
      endif
      mult = 10.0**(sig - power -1)
      xround = nint(num * mult) / mult
!
      end function xround

 !----------------------------------------------------------------------------------------------------

      function r_to_s(x,dec) result(str1)
!
!     writes a real into a left justified character string
!
      real, intent(in)              :: x
      integer, intent(in), optional :: dec
      character(len=14)             :: str
      character(len=:), allocatable             :: str1
      character(len=20)             :: sform
!
      if (present(dec)) then
          if (dec < 0 .and. real ( int ( x ) ) == x) then
              write ( str, '(i14)' ) int ( x )
          else
              if ( abs ( x ) < 999999.5E+00 ) then
                write(sform,'("(f14.",i0,")")')abs(dec)
                write ( str, sform ) x
              else
                write(sform,'("(g14.",i0,")")')abs(dec)
                write ( str, sform ) x
              end if
          endif
      else
          if ( real ( int ( x ) ) == x ) then
            write ( str, '(i14)' ) int ( x )
          else if ( abs ( x ) < 999999.5E+00 ) then
            write ( str, '(f14.6)' ) x
          else
            write ( str, '(g14.6)' ) x
          end if
      endif
!

      str1 = trim(adjustl(str))
!
      end function r_to_s

 !----------------------------------------------------------------------------------------------------
!corr   function i_to_s ( intval ) result(s)
!corr!
!corr!  I_TO_S_LEFT converts an integer to a left-justified string.
!corr!
!corr!  Examples:
!corr!
!corr!    Assume that S is 6 characters long:
!corr!
!corr!    INTVAL  S
!corr!
!corr!         1  1
!corr!        -1  -1
!corr!         0  0
!corr!      1952  1952
!corr!    123456  123456
!corr!   1234567  ******  <-- Not enough room!
!corr!
!corr!  Modified:
!corr!
!corr!    28 July 2000
!corr!
!corr!  Author:
!corr!
!corr!    John Burkardt
!corr!
!corr!  Parameters:
!corr!
!corr!    Input, integer INTVAL, an integer to be converted.
!corr!
!corr!    Output, character ( len = * ) S, the representation of the integer.
!corr!    The integer will be left-justified.  If there is not enough space,
!corr!    the string will be filled with stars.
!corr!
!corr   implicit none
!corr
!corr   character c
!corr   integer i
!corr   integer idig
!corr   integer ihi
!corr   integer ilo
!corr   integer intval
!corr   integer ipos
!corr   integer ival
!corr   character ( len = 100 ) s
!corr
!corr   s = ' '
!corr
!corr   ilo = 1
!corr   ihi = len ( s )
!corr
!corr   if ( ihi <= 0 ) then
!corr     return
!corr   end if
!corr!
!corr!   Make a copy of the integer.
!corr!
!corr   ival = intval
!corr!
!corr!   Handle the negative sign.
!corr!
!corr   if ( ival < 0 ) then
!corr
!corr     if ( ihi <= 1 ) then
!corr       s(1:1) = '*'
!corr       return
!corr     end if
!corr
!corr     ival = -ival
!corr     s(1:1) = '-'
!corr     ilo = 2
!corr
!corr   end if
!corr!
!corr!   The absolute value of the integer goes into S(ILO:IHI).
!corr!
!corr   ipos = ihi
!corr!
!corr!   Find the last digit of IVAL, strip it off, and stick it into the string.
!corr!
!corr   do
!corr
!corr     idig = mod ( ival, 10 )
!corr     ival = ival / 10
!corr
!corr     if ( ipos < ilo ) then
!corr       do i = 1, ihi
!corr         s(i:i) = '*'
!corr       end do
!corr       return
!corr     end if
!corr
!corr     call digit_to_ch ( idig, c )
!corr
!corr     s(ipos:ipos) = c
!corr     ipos = ipos - 1
!corr
!corr     if ( ival == 0 ) then
!corr       exit
!corr     end if
!corr
!corr   end do
!corr!
!corr!   Shift the string to the left.
!corr!
!corr   s(ilo:ilo+ihi-ipos-1) = s(ipos+1:ihi)
!corr   s(ilo+ihi-ipos:ihi) = ' '
!corr 
!corr   end function i_to_s
 !----------------------------------------------------------------------------------------------------

   function i_to_s ( intval ) result(s)
   integer, intent(in)            :: intval
   character (len=:), allocatable :: s
   integer :: ier
!
   if (intval < 0) then
       allocate(character(len=ndigits(intval)+1)::s)
   else
       allocate(character(len=ndigits(intval))::s)
   endif
   write(s,'(i0)',iostat=ier)intval
   if (ier /= 0) s=' '
!
   end function i_to_s

 !----------------------------------------------------------------------------------------------------

   function i_to_s1 ( intval , nf) result(s)
!
!  Write number in a allocatable string of length nf
!
   integer, intent(in)            :: intval
   integer, intent(in)            :: nf
   character (len=:), allocatable :: s
   integer                        :: ier,ndig
!
   ndig = ndigits(intval)
   if (intval < 0) ndig= ndig+1
   if (ndig > nf) then
       allocate(character(len=ndig)::s)
   else
       allocate(character(len=nf)::s)
   endif
   write(s,'(i0)',iostat=ier)intval
   if (ier /= 0) then
       s=' '
       return
   endif
   s = adjustr(s)
!
   end function i_to_s1

 !----------------------------------------------------------------------------------------------------

   subroutine digit_to_ch ( digit, c )

!
!  DIGIT_TO_CH returns the character representation of a decimal digit.
!
!   Example:
!
!     DIGIT   C 
!     -----  ---
!       0    '0'
!       1    '1'
!      ...    ...
!       9    '9'
!      17    '*'
!
!   Modified:
!
!    04 August 1999
!
!   Author:
!
!     John Burkardt
!
!   Parameters:
!
!     Input, integer DIGIT, the digit value between 0 and 9.
!
!     Output, character C, the corresponding character.
!
      character c
      integer digit

      if ( 0 <= digit .and. digit <= 9 ) then
        c = char ( digit + 48 )
      else
        c = '*'
      end if

      end subroutine digit_to_ch
 !----------------------------------------------------------------------------------------------------
      
      function d_to_s(d) result(str)
!
!     writes a double precision value into a left justified string.
!
      real(8), intent(in) :: d
      character(len=14) :: str
!
      write(str,'(g14.6)')d
      str = adjustl(str)
!
      end function d_to_s

 !----------------------------------------------------------------------------------------------------

   integer function string_locate(str,strvet,vet,back,exact) result(pos)
!
!  true se string compare almeno una volta nel vettore svet
!
   character(len=*), intent(in)                :: str     ! stringa
   character(len=*), dimension(:), intent(in)  :: strvet  ! vettore sul quale eseguire il controllo
   integer, dimension(:), intent(in), optional :: vet     ! elementi da escludere nel controllo
   logical, intent(in), optional               :: back    ! reverse array
   logical, intent(in), optional               :: exact   ! exact match
   integer                                     :: i,ini,fin,step
   logical                                     :: backk,exactt
!
   pos = 0
   if (present(back)) then
       backk = back
   else
       backk = .false.
   endif
   if (backk) then
       ini = size(strvet)
       fin = 1
       step = -1
   else
       ini = 1
       fin = size(strvet)
       step = 1
   endif
!
   if (present(exact)) then
       exactt = exact
   else
       exactt = .true.
   endif
!
   if (present(vet)) then
       if (exactt) then
           do i=ini,fin,step
              if (any(vet == i)) cycle             ! salta se i e' contenuto in vet
              if (len_trim(strvet(i)) == 0) cycle  ! if strvet is empy s_eqi provide true
              if (s_eqi(str,strvet(i))) then
                  pos = i
                  exit
              endif
           enddo
       else
           do i=ini,fin,step
              if (any(vet == i)) cycle  
              if (len_trim(strvet(i)) == 0) cycle
              if (match_word(str,strvet(i))) then
                  pos = i
                  exit
              endif
           enddo
       endif
   else
       if (exactt) then
           do i=ini,fin,step
              if (len_trim(strvet(i)) == 0) cycle
              if (s_eqi(str,strvet(i))) then
                  pos = i
                  exit
              endif
           enddo
       else
           do i=ini,fin,step
              if (len_trim(strvet(i)) == 0) cycle
              if (match_word(str,strvet(i))) then
                  pos = i
                  exit
              endif
           enddo
       endif
   endif
!
   end function string_locate

!----------------------------------------------------------------------------------------------------

   subroutine string_locate_all(str,strvet,vet,n)
!
!  Find all occurences of str in array strvet
!
   character(len=*), intent(in)                :: str     ! stringa
   character(len=*), dimension(:), intent(in)  :: strvet  ! vettore sul quale eseguire il controllo
   integer, dimension(:), intent(out)          :: vet     ! should be allocated at the same size of strvet
   integer, intent(out)                        :: n       ! number of occurences
   integer                                     :: ini,pos
!
   ini = 1
   do
      pos = string_locate(str,strvet(ini:),exact=.false.)
      if (pos > 0) then
          n = n + 1
          vet(n) = pos + ini - 1
          if (vet(n) == size(strvet)) exit
          ini = vet(n) + 1
      else
          exit
      endif
   enddo
!
   end subroutine string_locate_all

!----------------------------------------------------------------------------------------------------

   subroutine print_row(vet,ktype,ival,rval,sval,comm,kpr,sform)
!
!  Scrive una singola riga di una tabella
!
   integer, dimension(:), intent(in)                    :: vet   ! cosa scrivere del vettore ival/rval/sval
   integer, dimension(size(vet)), intent(in)            :: ktype ! tipo: 1=intero,2=reale,3=character
   integer, dimension(:), intent(in), optional          :: ival
   real, dimension(:), intent(in), optional             :: rval
   character(len=*), dimension(:), intent(in), optional :: sval
   logical, intent(in), optional                        :: comm
   integer, intent(in)                                  :: kpr
   character(len=*), dimension(size(vet)), intent(in)   :: sform
   integer                                              :: ncol
   integer                                              :: i
!
   if (all(vet(:) == 0)) return
   if (present(comm)) then
       if (comm) write(kpr,'(a)',advance='no')'#'  ! set labels as comment line
   endif
   ncol = 0
   do i=1,size(vet)
      if (vet(i) /= 0) then
          select case (ktype(i))
            case (1)      ! intero
              ncol = ncol + 1
              write(kpr,sform(i),advance='no')ival(i)

            case (20:)      ! reale
              ncol = ncol + 1
              write(kpr,sform(i),advance='no')rval(i)

            case (3)      ! character
              ncol = ncol + 1
              write(kpr,sform(i),advance='no')adjustl(sval(i))

          end select
      endif
   enddo
   if (ncol > 0) write(kpr,'(a)') ! advance yes
!
   end subroutine print_row

!----------------------------------------------------------------------------------------------------

   function format_string_i(ival,nspace) result(strf)
   integer, intent(in)            :: ival
   integer, intent(in)            :: nspace
   character(len=:), allocatable  :: strf
   integer                        :: ndig
!
   ndig = ndigits(ival)
   if (ival < 0) ndig = ndig + 1 ! sign
   if (nspace > 0) then
       strf = '('//i_to_s(nspace)//'x,i'//i_to_s(ndig)//')'
   else
       strf = '(i'//i_to_s(ndig)//')'
   endif
!
   end function format_string_i

!----------------------------------------------------------------------------------------------------

   function format_string_iv(ivet,nspace) result(strf)
   integer, dimension(:), intent(in) :: ivet
   integer, intent(in)               :: nspace
   character(len=:), allocatable     :: strf
   integer                           :: imax  !,ndig

   imax = maxval(abs(ivet))
   if (any(ivet < 0)) imax = -abs(imax) ! sign
   strf = format_string_i(imax,nspace)

   end function format_string_iv

!----------------------------------------------------------------------------------------------------

   function format_string_r(rval,nspace,ndec) result(strf)
   real, intent(in)               :: rval
   integer, intent(in)            :: nspace,ndec
   character(len=:), allocatable  :: strf
   integer                        :: ndig
   character(len=1)               :: ftype
!
   ndig = ndigits(int(rval))
   if (ndig > 6) then
       ftype = 'E'
   else
       ftype = 'F'
   endif
   ndig = ndig + 1 ! decimal point
   if (rval < 0) ndig = ndig + 1 ! sign
   if (nspace > 0) then
       strf = '('//i_to_s(nspace)//'x,'//ftype//i_to_s(ndig+ndec)//'.'//i_to_s(ndec)//')'
   else
       strf = '('//ftype//i_to_s(ndig+ndec)//'.'//i_to_s(ndec)//')'
   endif
!
   end function format_string_r

!----------------------------------------------------------------------------------------------------

   function format_string_rv(rvet,nspace,ndec) result(strf)
   real, dimension(:), intent(in) :: rvet
   integer, intent(in)            :: nspace,ndec
   character(len=:), allocatable  :: strf
   real                           :: rmax
!
   rmax = maxval(abs(rvet))
   if (any(rvet < 0)) rmax = -abs(rmax)
   strf = format_string_r(rmax,nspace,ndec)
!
   end function format_string_rv

!----------------------------------------------------------------------------------------------------

   function format_string_s(sval,nspace) result(strf)
   character(len=*), intent(in)  :: sval
   integer, intent(in)           :: nspace
   character(len=:), allocatable :: strf
   integer                       :: nchar
!
   nchar = len(sval)
   if (nspace > 0) then
       strf = '('//i_to_s(nspace)//'x,a'//i_to_s(nchar)//')'
   else
       strf = '(a'//i_to_s(nchar)//')'
   endif
!
   end function format_string_s

!----------------------------------------------------------------------------------------------------

   function format_string_sv(svet,nspace) result(strf)
   character(len=*), dimension(:), intent(in) :: svet
   integer, intent(in)                        :: nspace
   character(len=:), allocatable              :: strf
!
   strf = format_string_s(svet(1),nspace)
!
   end function format_string_sv

!----------------------------------------------------------------------------------------------------

   subroutine set_row_list(vet,ktype,ival,rval,sval,comm,kpr)
!
!  Scrive una singola riga di una tabella
!
   integer, dimension(:), intent(in)                    :: vet   ! cosa scrivere del vettore ival/rval/sval
   integer, dimension(size(vet)), intent(in)            :: ktype ! tipo: 1=intero,2=reale,3=character
   integer, dimension(:), intent(in), optional          :: ival
   real, dimension(:), intent(in), optional             :: rval
   character(len=*), dimension(:), intent(in), optional :: sval
   logical, intent(in), optional                        :: comm
   integer, intent(in)                                  :: kpr
   character(len=20), dimension(size(vet))              :: srow
   integer                                              :: ncol
   integer                                              :: i
   character(len=100)                                   :: str !warn gfortran
   integer                                              :: deg
!
   if (all(vet(:) == 0)) return
   if (present(comm)) then
       if (comm) write(kpr,'(a)',advance='no')'#'  ! set labels as comment line
   endif
   ncol = 0
   do i=1,size(srow)
      if (vet(i) /= 0) then
          select case (ktype(i))
            case (1)      ! intero
              ncol = ncol + 1
              str = i_to_s(ival(i))    
              srow(ncol) = str(1:10)
              write(kpr,'(a)',advance='no')str(1:10)

            case (20:)      ! reale
              deg = mod(ktype(i),20)  ! cifre decimali
              ncol = ncol + 1
              str = r_to_s(rval(i),deg) 
              srow(ncol) = str(1:10)
              write(kpr,'(a)',advance='no')str(1:10)

            case (3)      ! character
              ncol = ncol + 1
              srow(ncol) = sval(i)
              srow(ncol) = s_center(srow(ncol))
              str = sval(i)
              write(kpr,'(a)',advance='no')str(1:10)

          end select
      endif
   enddo
   if (ncol > 0) write(kpr,'(a)') ! advance yes
!
   end subroutine set_row_list

!----------------------------------------------------------------------------------------------------

   function s_in_quotes(string)  result(str)
!
!  Enclose string in double quotes
!
   character(len=*), intent(in)      :: string
   character(len=len_trim(string)+2) :: str
!
   str = "'"//trim(string)//"'"
!
   end function s_in_quotes

!----------------------------------------------------------------------------------------------------

   function rem_quotes(string)  result(str)
!
!  Remove apici all'inizio e alla fine di una stringa
!
   character(len=*), intent(in)    :: string
   character(len=len_trim(string)) :: str
   integer                         :: lens
   character(len=1) :: squot
   integer :: kpos
   integer :: i
!
   squot = ' '
   kpos = 0
   lens = len_trim(string)
   str = trim(string)
!
!  Search for the first character equal to ' or "
   do i=1,lens
      if (str(i:i) == '"' .or. str(i:i) == "'") then
          squot = str(i:i)
          kpos = i
          exit
      elseif (str(i:i) /= ' ') then
          exit
      endif
   enddo
!
!  Search for the second quote
   if (kpos > 0 .and. kpos < lens) then
       do i=lens,lens-kpos,-1
          if (str(i:i) == squot) then
              str(i:i) = ' '
              str(kpos:kpos) = ' '
              exit
          elseif (str(i:i) /= ' ') then
              exit
          endif
       enddo
   endif
!
   end function rem_quotes

!----------------------------------------------------------------------------------------------------

   subroutine get_string_in_brackets(str,strb,bpos,ier)
   character(len=*), intent(in)  :: str   ! stringa a partire dalla parentesi
   character(len=*), intent(out) :: strb  ! stringa in parentesi
   integer, intent(out)          :: bpos  ! position of seconf bracket
   integer, intent(out)          :: ier
   integer                       :: lens
   character(len=1)              :: br1,br2
   integer                       :: kbr
   integer                       :: i
   ier = 0
   strb = ' '
   br1 = str(1:1)
   bpos = 0
   select case (br1)
       case ('(')
         br2 = ')'
       case ('[')
         br2 = ']'
       case ('{')
         br2 = '}'
       case default
         ier = 1
   end select
   if (ier == 0) then
       lens = len_trim(str)
       kbr = 0
       i=1
       do 
         i=i+1
         if (i > lens) exit
         if (str(i:i) == br1) then
             kbr = kbr + 1
         elseif (str(i:i) == br2) then
             kbr = kbr - 1
             if (kbr == -1) exit
         endif
       enddo
       if (kbr == -1) then
           strb = str(2:i-1)
           bpos = i
       else
           ier = 1
       endif
   endif
   end subroutine get_string_in_brackets

!----------------------------------------------------------------------------------------------------

   subroutine s_c_append(str,c)
!
!  Append char to string only if char doesn't exist in the string
!
   character(len=*), intent(inout) :: str
   character(len=1), intent(in)    :: c
!
   if (index(str,c) > 0) return
   str = trim(str)//c
!
   end subroutine s_c_append

!----------------------------------------------------------------------------------------------------

   subroutine write_svet(svet,nvet,kpr,word)
!
!  Scrivi sulla stessa riga un vettore di stringhe
!
   character(len=*), dimension(:), intent(in) :: svet
   integer, intent(in)                        :: nvet
   integer, intent(in)                        :: kpr
   character(len=*), intent(in), optional     :: word
   character(len=3)                           :: adv
   integer                                    :: i
   select case (nvet)
     case (1)
       if (present(word)) then
           write(kpr,'(a)') word//trim(svet(1))
       else
           write(kpr,'(a)') trim(svet(1))
       endif

     case (2:)
       adv = 'no'
       if (present(word)) write(kpr,'(a)',advance=adv)word
       write(kpr,'(a)',advance=adv) trim(svet(1))//','
       do i=2,nvet-1
          write(kpr,'(a)',advance=adv) trim(svet(i))//','
       enddo
       adv = 'yes'
       write(kpr,'(a)',advance=adv) trim(svet(nvet))
   end select
   end subroutine write_svet

!----------------------------------------------------------------------------------------------------

   function cat_svet(svet,nvet,sep)  result(str)
!
!  Cat string vector in an allocatable string
!
   character(len=*), dimension(:), intent(in) :: svet
   integer, intent(in)                        :: nvet
   character(len=:), allocatable              :: str
   integer                                    :: i
   character(len=*), intent(in), optional     :: sep

   if (nvet <= 0) then
       str = ' '
       return
   endif
   str = trim(svet(1))
   if (present(sep)) then
       do i=2,nvet
          str = str//sep//trim(svet(i))
       enddo
   else
       do i=2,nvet
          str = str//trim(svet(i))
       enddo
   endif

   end function cat_svet

!----------------------------------------------------------------------------------------------------

   function cat_ivet(ivet,sep)  result(str)
!
!  Cat array of integers in a string
!
   integer, dimension(:), intent(in)      :: ivet
   character(len=:), allocatable          :: str
   integer                                :: i,nvet
   character(len=*), intent(in), optional :: sep

   nvet = size(ivet)
   if (nvet <= 0) then
       str = ' '
       return
   endif
   str = i_to_s(ivet(1))
   if (present(sep)) then
       do i=2,nvet
          str = str//sep//i_to_s(ivet(i))
       enddo
   else
       do i=2,nvet
          str = str//i_to_s(ivet(i))
       enddo
   endif

   end function cat_ivet

!corr !---------------------------------------------------------------------------------------------
!corr
!corr   function catsvector(vets)  result(stringv)   !OBSOLETE
!corr!
!corr!  Concatena un vettore di stringhe in un'unica stringa senza rimuovere gli spazi vuoti. 
!corr!  Autore:C. Cuocci
!corr!
!corr   character(len=*), dimension(:), intent(in) :: vets
!corr   character(len=len(vets(1))*size(vets))     :: stringv
!corr   integer                                    :: i
!corr   integer                                    :: lens
!corr!
!corr   lens = len(vets(1))
!corr   stringv = vets(1)
!corr   do i=2,size(vets)
!corr      stringv(1:lens*i) = stringv(1:(i-1)*lens)//vets(i)
!corr   enddo
!corr!
!corr   end function catsvector
!corr   
!corr !---------------------------------------------------------------------------------------------
!corr
!corr   function catsvector2(vets,sep)  result(stringv)  !OBSOLETE
!corr!
!corr!  Concatena un vettore di stringhe in un'unica stringa rimuovendo gli spazi vuoti  e introducendo un separatore
!corr!  Autore:C. Cuocci
!corr!
!corr   character(len=*), dimension(:), intent(in)                 :: vets
!corr   character(len=*), intent(in)                               :: sep   ! separatore di stringhe
!corr   character(len=len(vets(1))*size(vets)+len(sep)*size(vets)) :: stringv
!corr   integer                                                    :: i
!corr   integer                                                    :: lens
!corr!
!corr   lens = len_trim(vets(1))
!corr   stringv = vets(1)
!corr   do i=2,size(vets)
!corr      stringv(:) = trim(stringv)//sep//trim(vets(i))
!corr   enddo
!corr!
!corr   end function catsvector2
!corr

 !----------------------------------------------------------------------------------

   subroutine write_title(kpr,stringvet,typevet)
!
!  Write title in a frame
!
   integer, intent(in)          :: kpr
   character(len=*), dimension(:), intent(in) :: stringvet
   character(len=1), dimension(:), intent(in), optional :: typevet
   integer, parameter           :: MAX_SPACE = 80
   integer, parameter           :: BORDER = 1
   character(len=100)           :: sform0,sform
   integer                      :: ier
   integer                      :: pos
   integer                      :: i
   sform0 = '('//trim(i_to_s(BORDER))//'x,'//trim(i_to_s(MAX_SPACE-2*BORDER))//'("+"))'
   write(kpr,sform0)
   write(kpr,*)
   if (present(typevet)) then
       do i=1,size(stringvet)
          if (typevet(i) == 'c') then
              pos = ((MAX_SPACE - 2*BORDER) - len_trim(stringvet(i))) / 2
              if (pos <= 0) pos = 1
              sform = '('//trim(i_to_s(pos))//'x,a)'
              write(kpr,sform,iostat=ier)trim(stringvet(i))
          else
              write(kpr,'(1x,a)',iostat=ier)trim(stringvet(i))
          endif
       enddo
   else
       do i=1,size(stringvet)
          pos = ((MAX_SPACE - 2*BORDER) - len_trim(stringvet(i))) / 2
          if (pos <= 0) pos = 1
          sform = '('//trim(i_to_s(pos))//'x,a)'
          write(kpr,sform,iostat=ier)trim(stringvet(i))
       enddo
   endif
   write(kpr,*)
   write(kpr,sform0)
   end subroutine write_title

 !----------------------------------------------------------------------------------

   logical function match_word(string,word) result(match)
!
!  true if the first characters of string match with word (case insensitive)
!  es. true if string = 'Continue'  and word = 'cont'
!
   character(len=*), intent(in) :: string, word
   integer                      :: lenw,lens
!
   lenw = len_trim(word)
   lens = len_trim(string)
   if (lenw >= lens) then
       match = s_eqi(word(1:lens),string)
   else
       match = s_eqi(word,string(1:lenw))
   endif
!
   end function match_word

 !----------------------------------------------------------------------------------------------------

   logical function word_is_contained(smallword,bigword,minc) result(match)
!
!  true if almost minc character of smallword are contained in bigword
!  es. true if string = 'Cont'  and word = 'continue'
!
   character(len=*), intent(in)  :: smallword, bigword
   integer, intent(in), optional :: minc
   integer                       :: mincc, len_small, len_big
!
   len_small = len_trim(smallword)
   len_big = len_trim(bigword)
   match = .false.
   if (len_small > len_big) return

   if (present(minc)) then
       mincc = minc
   else
       mincc = 4
   endif
   mincc = min(len_big,mincc)
   if (len_small < mincc) return

   if (len_small <= len_big) then
       match = s_eqi(smallword(:len_small),bigword(:len_small)); return
   endif
!
   end function word_is_contained

 !----------------------------------------------------------------------------------

   function time_string()
   character(len=8) :: time_string
   integer          :: values(8)
!
   call date_and_time ( values = values )
   write(time_string,'(i2,a,i2.2,a,i2.2,a)')values(5),':',values(6),':',values(7)
!
   end function time_string

 !----------------------------------------------------------------------------------

   integer FUNCTION Day_of_week(d, m, y)
   INTEGER :: j, k, mm, yy
   INTEGER, INTENT(IN) :: d, m, y
 
   mm=m
   yy=y
   IF(mm.le.2) THEN
      mm=mm+12
      yy=yy-1
   END IF
   j = yy / 100
   k = MOD(yy, 100)
   Day_of_week = MOD(d + ((mm+1)*26)/10 + k + k/4 + j/4 + 5*j - 1, 7)
   END FUNCTION Day_of_week

 !----------------------------------------------------------------------------------

   function day_of_week_string(d,m,y)
   integer, intent(in) :: d,m,y
   character(len=3), dimension(0:6) :: days = ['Sun','Mon','Tue','Wed','Thu','Fri','Sat']
   character(len=3) :: day_of_week_string
   day_of_week_string = days(day_of_week(d,m,y))
   end function day_of_week_string

 !----------------------------------------------------------------------------------

   function date_time_string()
   character(len=42) :: date_time_string
   character (len=8) :: ampm
   integer ( kind = 4 ) d
   integer ( kind = 4 ) h
   integer ( kind = 4 ) m
   character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
   integer ( kind = 4 ) n
   integer ( kind = 4 ) s
   integer ( kind = 4 ) values(8)
   integer ( kind = 4 ) y

   call date_and_time ( values = values )

   y = values(1)
   m = values(2)
   d = values(3)
   h = values(5)
   n = values(6)
   s = values(7)
!corr   mm = values(8)

   if ( h < 12 ) then
       ampm = 'AM'
   else if ( h == 12 ) then
       if ( n == 0 .and. s == 0 ) then
         ampm = 'Noon'
       else
         ampm = 'PM'
       end if
   else
       h = h - 12
       if ( h < 12 ) then
         ampm = 'PM'
       else if ( h == 12 ) then
         if ( n == 0 .and. s == 0 ) then
           ampm = 'Midnight'
         else
           ampm = 'AM'
         end if
       end if
  endif
!corr  write ( date_time_string, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
!corr    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )
  !write ( date_time_string, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,1x,a)' ) &
  !  d, trim ( month(m) ), y, h, ':', n, ':', s, trim ( ampm )
  write ( date_time_string, '(a,1x,i0,1x,a,1x,i4,1x,a,1x,i2,a1,i2.2,a1,i2.2,1x,a)' ) &
    day_of_week_string(d,m,y),d, trim ( month(m) ),y,'at',h, ':', n, ':', s, trim ( ampm )

  end function date_time_string

 !----------------------------------------------------------------------------------

  logical function all_ch_eq(str,ch)
!
! Check if all characters in str are equal to ch
!
  character(len=*), intent(in) :: str
  character(len=1), intent(in) :: ch
  integer                      :: i
!
  all_ch_eq = .true.
  do i=1,len_trim(str)
     if (str(i:i) /= ch) then
         all_ch_eq = .false.
         return
     endif
  enddo
!
  end function all_ch_eq

 !----------------------------------------------------------------------------------

  logical function is_in_brackets(str,pos)
!
! Check if pos is in brackets
!
  character(len=*), intent(in) :: str
  integer, intent(in)          :: pos
  integer                      :: lens,b1,b2
!
  is_in_brackets = .false.
  lens = len_trim(str)
  if (pos >= lens .or. pos == 1) return
!
  b1 = index(str(1:pos-1),'(',back=.true.)
  if (b1 == 0) return
  b2 = index(str(1:pos-1),')',back=.true.)
  if (b2 > b1) return
  b2 = index(str(pos+1:),')') + pos
  if (b2 == pos) return
  is_in_brackets = .true.
!
  end function is_in_brackets

 !----------------------------------------------------------------------------------

   subroutine write_string_bin(aunit,string)
!
!  Write variable string on binary file
!
   integer, intent(in)          :: aunit
   character(len=*), intent(in) :: string
   if (len_trim(string) == 0) then
       write(aunit)0
   else
       write(aunit)len_trim(string)
       write(aunit)trim(string)
   endif
   end subroutine write_string_bin

 !----------------------------------------------------------------------------------

   integer function read_string_bin(aunit,string)  result(ier)
!
!  Read variable string from binary file
!
   integer, intent(in)                        :: aunit
   character(len=:), allocatable, intent(out) :: string
   integer                                    :: lens
   read(aunit,iostat=ier)lens
   if (ier /= 0) return
   if (lens > 0) then
       allocate(character(lens)::string)
       read(aunit,iostat=ier)string
   else
       string = ' '
   endif
!
   end function read_string_bin

!--------------------------------------------------------------------------------------------------------

   subroutine s_find_duplicate(svet,vd)
!
!  Find duplicate in an array of strings. 
!  vd(i) < 0: i is duplicate of vd(i); vd(i) > 0: vd(i) copies of i are available
!
   character(len=*), dimension(:), intent(in) :: svet
   integer, dimension(:), intent(out)         :: vd
   integer                                    :: i,j
!
   vd(:) = 1
   do i=1,size(svet)-1
      if (vd(i) > 1 .or. vd(i) < 0) cycle
      do j=i+1,size(svet)
         if(s_eqidb(svet(i),svet(j))) then
             vd(i) = vd(i) + 1
             vd(j) = -i
         endif
      enddo
   enddo
!
   end subroutine s_find_duplicate

!--------------------------------------------------------------------------------------------------------

   subroutine s_delete_copies(svet,nsv,vd,ndel)
!
!  Delete multiple copies in array svet if ncopies > ndel
!  vd should be defined as in s_find_duplicate
!
   character(len=*), dimension(:), intent(inout) :: svet
   integer, intent(inout)                        :: nsv
   integer, dimension(:), intent(inout)          :: vd
   integer, intent(in)                           :: ndel
   integer                                       :: i, ncopy, nsv1
!
   do i=1,nsv
      if (vd(i) < 0) then
          ncopy = vd(abs(vd(i)))
          if (ncopy <= ndel) then
              vd(i) = ncopy
          endif
      endif
   enddo
   nsv1 = count(vd > 0)
   if (nsv1 /= nsv) then
       nsv = nsv1
       svet(:nsv) = pack(svet,mask=vd > 0)
       vd(:nsv) = pack(vd,mask=vd > 0)
   endif
!
   end subroutine s_delete_copies

!--------------------------------------------------------------------------------------------------------

   subroutine s_trim_zeros ( s )
   
   !*******************************************************************************
   !
   !! S_TRIM_ZEROS removes trailing zeros from a string.
   !
   !  Example:
   !
   !    Input:
   !
   !      S = '1401.072500'
   !
   !    Output:
   !
   !      S = '1401.0725'
   !
   !  Modified:
   !
   !    30 June 2000
   !
   !  Author:
   !
   !    John Burkardt
   !
   !  Parameters:
   !
   !    Input/output, character ( len = * ) S, the string to be operated on.
   !
     implicit none
   
     integer i
     character ( len = * ) s
   
     i = len_trim ( s )
   
     do while ( 0 < i .and. s(i:i) == '0' )
       s(i:i) = ' '
       i = i - 1
     end do
   
     return
   end subroutine 

!--------------------------------------------------------------------------------------------------------

   integer function find_string_pos(string,starts,ends,lens) result(pos)
!
!  Find string position starting with starts and ending with ends
!  Final is pos + lens - 1
!
   character(len=*), intent(in) :: string,starts,ends
   integer, intent(out)         :: lens
   integer                      :: pos1,pos2
!
   lens = 0
   pos = index(string,starts)
   if (pos == 0) return
   pos1 = pos + len(starts)
   if (pos1 > len_trim(string)) return
!
   pos2 = index(string(pos1:),ends)
   if (pos2 == 0) then
       pos = 0
       return
   else
       lens = pos2 + len(ends) + pos1 - 1 - pos
   endif
!
   end function find_string_pos

!--------------------------------------------------------------------------------------------------------

   integer function find_string_in_array(string, array) result(pos)
!
!  Find string in array of string. Return 0 if string is absent
!
   character(len=*), intent(in)               :: string
   character(len=*), dimension(:), intent(in) :: array
   integer                                    :: i
!
   pos = 0
   do i=1,size(array)
      if (s_eqidb(string,array(i))) then
          pos = i
          return
      endif
   enddo
!
   end function find_string_in_array

!--------------------------------------------------------------------------------------------------------

   subroutine write_formatted_message(unt,message_type,msg,ampl)
!
!  Write message in a box splitting at LF o blank
!
   integer, intent(in)                   :: unt
   character(len=*), intent(in)          :: message_type   ! short string for message (es. ERROR) or blank
   character(len=*), intent(in)          :: msg            ! the message
   integer, intent(in), optional         :: ampl           ! total amplitude
   character(len=len_trim(adjustl(msg))) :: msg0
   integer                               :: lerr,pos,start,amp,ends,amp0,pos0,i,ampe
   integer, parameter                    :: PAD = 1        ! padding
   integer, parameter                    :: DEF_AMP = 74
   integer, parameter                    :: SPACE_LEFT = 2
   character(len=:), allocatable         :: sform,sform1
!
   msg0 = trim(adjustl(msg))
   lerr = len_trim(msg0)
   if (len_trim(message_type) == 0 .and. lerr == 0) return

   if (present(ampl)) then
       amp0 = ampl
       if (amp0 <= 0) amp0 = DEF_AMP
   else
       amp0 = DEF_AMP
   endif
   ampe = amp0 - 2   ! -2 for '*' at beginning and end
   amp = ampe - PAD*2
!
   sform = "("//i_to_s(SPACE_LEFT)//"x,"//i_to_s(amp0)//"('*'))"
   write(unt,sform)
!
   sform1 = "("//i_to_s(SPACE_LEFT)//"x,"//"a)"
   if (len_trim(message_type) > 0) then
       write(unt,sform1) '*'//centra_str(message_type,ampe)//'*'
   endif
!
   start = 1
   ends = min(lerr,amp)
   do 
      pos = 0
!
!     Last line: search are not necessary
      if (ends == lerr) then
          pos = 1
          pos0 = ends !- start + 1 + start - 1
          call s_rep_ch(msg0(start:ends),char(10),' ')
      endif
!
!     search for linefeed
      if (ends < lerr) then
          if (msg0(ends+1:ends+1) == char(10)) then
              pos = 1
              pos0 = ends + 1
              msg0(pos0:pos0) = ' '
          endif
      endif
      if (pos == 0) then
          pos = index(msg0(start:ends),char(10))
          if (pos > 0) then
              pos0 = pos+start-1
              msg0(pos0:pos0) = ' '
          endif
      endif
      if (pos == 0) then
!
!         search for empty space
          if (ends < lerr) then   
              if (msg0(ends+1:ends+1) == ' ') then
                  pos = 1   !ends - start + 1
                  pos0 = ends
              endif
          endif
          if (pos == 0) then
              pos = index(msg0(start:ends),' ',back=.true.)
              if (pos == 0) then
                  pos0 = ends  ! truncate the string
              else
                  pos0 = pos+start-1
              endif
          endif
      endif

      write(unt,sform1) '*'//centra_str(msg0(start:pos0),ampe)//'*'
      start = pos0 + 1
      if (start > lerr) exit
!
!     jump empty char at left
      do i=start,lerr
         if (msg0(i:i) == ' ' .or. msg0(i:i) == char(10)) then
             start = start + 1
         else
             exit
         endif
      enddo

      ends = min(lerr,start+amp-1)
   enddo
!
   write(unt,sform)
!
   end subroutine write_formatted_message

!--------------------------------------------------------------------------------------------------------

   function toFortranString(str,lens)
!
!  Convert an array of character(kind=c_char,len=1) in a fortran string
!
   use iso_c_binding, only: c_char, c_int
   character(kind=c_char), intent(in) :: str(*)
   integer(c_int), intent(in)         :: lens
   character(len=:), allocatable      :: toFortranString
   integer                            :: i

   allocate(character(lens) :: toFortranString)
   forall(i = 1:lens) toFortranString(i:i) = str(i)

   end function toFortranString

!--------------------------------------------------------------------------------------------------------

   function toCString(str) result(cstr)
!
!  Convert a fortran string in an array of character(kind=c_char,len=1)
!
   use iso_c_binding, only: c_char, c_null_char
   character(len=*), intent(in)                       :: str
   character(kind=c_char), dimension(len_trim(str)+1) :: cstr
   integer                                            :: i,lens
!
   lens = len_trim(str)
   do i=1,lens
      cstr(i) = str(i:i)
   enddo
   cstr(lens+1) = c_null_char
!
   end function toCString

!--------------------------------------------------------------------------------------------------------

   function f_to_c(fortran_string)
!
!  Convert fortran string in C string
!
   use iso_c_binding, only: c_char, c_null_char
   character (len=*)                                      :: fortran_string
   character (len=len_trim(fortran_string)+1,kind=c_char) :: f_to_c
!
   f_to_c = trim(fortran_string)//c_null_char
!
   end function f_to_c

!--------------------------------------------------------------------------------------------------------

   function c_to_f(c_string)
!
!  Convert C string in a fortran string
!
   use iso_c_binding, only: c_char, c_null_char
   character (len=:), allocatable :: c_to_f
   character (len=*,kind=c_char)  :: c_string
   integer                        :: c_length
   integer                        :: i
!
   c_length = 1
   c_to_f = " "
   do i=1,len(c_string)
      if ( c_string(i:i) == c_null_char ) exit
      c_length = c_length+1
   end do
   c_length = c_length -1
   c_to_f = c_string(1:c_length)
!
   end function c_to_f

!--------------------------------------------------------------------------------------------------------

   logical function str_set_real(str,rvalue,rmin) result(err)
   character(len=*), intent(in)    :: str
   real, intent(inout)             :: rvalue
   real, intent(in), optional      :: rmin
   real, dimension(:), allocatable :: vet
   integer                         :: iv

   err = .true.
   call getnum1(str,vet=vet,iv=iv)
   if (iv <= 0) return

   if (present(rmin)) then
       if (vet(1) < rmin) return
   endif

   rvalue = vet(1)
   err = .false.
   
   end function str_set_real

END MODULE STRUTIL
