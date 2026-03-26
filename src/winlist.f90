MODULE wlist

   implicit none

   integer, parameter :: WL_NMAXLEN = 80

   character(WL_NMAXLEN), dimension(:), allocatable :: wl_col_title
   character(WL_NMAXLEN), dimension(:,:), allocatable :: wl_items

CONTAINS 
   
  subroutine wlist_create(title,label,nrow,ncol,connect,coltitles,sitem,selection) !,scol1,scol2)
  use strutil
  use arrayutil
  interface
     subroutine open_string_list_dlg(title,label,nrow,ncol, &
                                     connect,selection) bind(C,name="open_string_list_dlg") 
     use iso_c_binding, only: c_char, c_int
     character(c_char)     :: title(*)
     character(c_char)     :: label(*)
     integer(c_int), value :: nrow,ncol,connect
     integer(c_int)        :: selection
     end subroutine open_string_list_dlg
  end interface
  character(len=*), intent(in)                 :: title,label
  integer, intent(in)                          :: nrow, ncol, connect
  character(len=*), dimension(:), intent(in)   :: coltitles   ! titoli delle colonne
  character(len=*), dimension(:,:), intent(in) :: sitem       ! items
  integer, intent(inout)                       :: selection

  wl_col_title = coltitles
  wl_items = sitem
  call open_string_list_dlg(f_to_c(title),f_to_c(label),nrow,ncol,connect,selection)
  call delete_array(wl_col_title)
  call delete_array(wl_items)

  end subroutine wlist_create

!--------------------------------------------------------------------------------------

  subroutine wlist_get_title(col,title)  bind(C,name="wlist_get_title")
  use strutil
  use iso_c_binding, only: c_int, c_char, c_null_char
  integer(c_int), value :: col
  character(c_char)     :: title(*)

  title(1:len_trim(wl_col_title(col))+1) = toCString(wl_col_title(col))

  end subroutine wlist_get_title

!--------------------------------------------------------------------------------------

  subroutine wlist_get_item(row, col, item)  bind(C,name="wlist_get_item")
  use strutil
  use iso_c_binding, only: c_int, c_char, c_null_char
  integer(c_int), value :: row, col
  character(c_char)     :: item(*)

  item(1:len_trim(wl_items(row,col))+1) = toCString(wl_items(row,col))

  end subroutine wlist_get_item

END MODULE wlist
