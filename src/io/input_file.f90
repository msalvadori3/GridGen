module xml_data_input_file
   use READ_XML_PRIMITIVES
   use WRITE_XML_PRIMITIVES
   use XMLPARSE
   implicit none
   integer, private :: lurep_
   logical, private :: strict_

type geometry_t
   real(kind=kind(1.0d0))                          :: x1
   real(kind=kind(1.0d0))                          :: y1
   real(kind=kind(1.0d0))                          :: x2
   real(kind=kind(1.0d0))                          :: y2
   real(kind=kind(1.0d0))                          :: x3
   real(kind=kind(1.0d0))                          :: y3
   real(kind=kind(1.0d0))                          :: x4
   real(kind=kind(1.0d0))                          :: y4
   integer                                         :: FEsize
   integer                                         :: DCsize
   integer                                         :: Geosize
   real(kind=kind(1.0d0))                          :: Geoptx1
   real(kind=kind(1.0d0))                          :: Geopty1
   real(kind=kind(1.0d0))                          :: Geoptx2
   real(kind=kind(1.0d0))                          :: Geopty2
   integer                                         :: imax
   integer                                         :: jmax
end type geometry_t

type setup_t
   character(len=64)                                :: Project
   integer                                         :: nmax
   integer                                         :: iControl
   real(kind=kind(1.0d0))                          :: Cy
end type setup_t

type input_type_t
   type(geometry_t)                                :: geometry
   type(setup_t)                                   :: setup
end type input_type_t
   type(input_type_t)                             ,save :: input_data
contains
subroutine read_xml_type_geometry_t_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(geometry_t), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(geometry_t), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_geometry_t( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_geometry_t_array

subroutine read_xml_type_geometry_t( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(geometry_t), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag, last_tag
   logical                                         :: has_x1
   logical                                         :: has_y1
   logical                                         :: has_x2
   logical                                         :: has_y2
   logical                                         :: has_x3
   logical                                         :: has_y3
   logical                                         :: has_x4
   logical                                         :: has_y4
   logical                                         :: has_FEsize
   logical                                         :: has_DCsize
   logical                                         :: has_Geosize
   logical                                         :: has_Geoptx1
   logical                                         :: has_Geopty1
   logical                                         :: has_Geoptx2
   logical                                         :: has_Geopty2
   logical                                         :: has_imax
   logical                                         :: has_jmax
   has_x1                               = .false.
   has_y1                               = .false.
   has_x2                               = .false.
   has_y2                               = .false.
   has_x3                               = .false.
   has_y3                               = .false.
   has_x4                               = .false.
   has_y4                               = .false.
   has_FEsize                           = .false.
   has_DCsize                           = .false.
   has_Geosize                          = .false.
   has_Geoptx1                          = .false.
   has_Geopty1                          = .false.
   has_Geoptx2                          = .false.
   has_Geopty2                          = .false.
   has_imax                             = .false.
   has_jmax                             = .false.
   call init_xml_type_geometry_t(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   last_tag = ""
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 .and. TRIM(tag) == TRIM(last_tag)) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      last_tag = tag
      select case( tag )
      case('x1')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%x1, has_x1 )
      case('y1')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%y1, has_y1 )
      case('x2')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%x2, has_x2 )
      case('y2')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%y2, has_y2 )
      case('x3')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%x3, has_x3 )
      case('y3')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%y3, has_y3 )
      case('x4')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%x4, has_x4 )
      case('y4')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%y4, has_y4 )
      case('FEsize')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%FEsize, has_FEsize )
      case('DCsize')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%DCsize, has_DCsize )
      case('Geosize')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%Geosize, has_Geosize )
      case('Geoptx1')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%Geoptx1, has_Geoptx1 )
      case('Geopty1')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%Geopty1, has_Geopty1 )
      case('Geoptx2')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%Geoptx2, has_Geoptx2 )
      case('Geopty2')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%Geopty2, has_Geopty2 )
      case('imax')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%imax, has_imax )
      case('jmax')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%jmax, has_jmax )
      case ('comment', '!--')
         ! Simply ignore
      case ('xblk_setup')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
end subroutine read_xml_type_geometry_t
subroutine init_xml_type_geometry_t_array( dvar )
   type(geometry_t), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_geometry_t_array
subroutine init_xml_type_geometry_t(dvar)
   type(geometry_t) :: dvar
   dvar%x1 = -0.8_8
   dvar%y1 = 0.0_8
   dvar%x2 = 1.8_8
   dvar%y2 = 0.0_8
   dvar%x3 = -0.8_8
   dvar%y3 = 1.0_8
   dvar%x4 = 1.8_8
   dvar%y4 = 1.0_8
   dvar%FEsize = 11
   dvar%DCsize = 11
   dvar%Geosize = 21
   dvar%Geoptx1 = 0.0_8
   dvar%Geopty1 = 0.0_8
   dvar%Geoptx2 = 1.0_8
   dvar%Geopty2 = 0.0_8
   dvar%imax = 41
   dvar%jmax = 19
end subroutine init_xml_type_geometry_t
subroutine write_xml_type_geometry_t_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(geometry_t), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_geometry_t( info, tag, indent, dvar(i) )
   enddo
end subroutine write_xml_type_geometry_t_array

subroutine write_xml_type_geometry_t( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(geometry_t)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',trim(tag), '>'
   call write_to_xml_double( info, 'x1', indent+3, dvar%x1)
   call write_to_xml_double( info, 'y1', indent+3, dvar%y1)
   call write_to_xml_double( info, 'x2', indent+3, dvar%x2)
   call write_to_xml_double( info, 'y2', indent+3, dvar%y2)
   call write_to_xml_double( info, 'x3', indent+3, dvar%x3)
   call write_to_xml_double( info, 'y3', indent+3, dvar%y3)
   call write_to_xml_double( info, 'x4', indent+3, dvar%x4)
   call write_to_xml_double( info, 'y4', indent+3, dvar%y4)
   call write_to_xml_integer( info, 'FEsize', indent+3, dvar%FEsize)
   call write_to_xml_integer( info, 'DCsize', indent+3, dvar%DCsize)
   call write_to_xml_integer( info, 'Geosize', indent+3, dvar%Geosize)
   call write_to_xml_double( info, 'Geoptx1', indent+3, dvar%Geoptx1)
   call write_to_xml_double( info, 'Geopty1', indent+3, dvar%Geopty1)
   call write_to_xml_double( info, 'Geoptx2', indent+3, dvar%Geoptx2)
   call write_to_xml_double( info, 'Geopty2', indent+3, dvar%Geopty2)
   call write_to_xml_integer( info, 'imax', indent+3, dvar%imax)
   call write_to_xml_integer( info, 'jmax', indent+3, dvar%jmax)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //trim(tag) // '>'
end subroutine write_xml_type_geometry_t

subroutine read_xml_type_setup_t_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(setup_t), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(setup_t), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_setup_t( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_setup_t_array

subroutine read_xml_type_setup_t( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(setup_t), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag, last_tag
   logical                                         :: has_Project
   logical                                         :: has_nmax
   logical                                         :: has_iControl
   logical                                         :: has_Cy
   has_Project                          = .false.
   has_nmax                             = .false.
   has_iControl                         = .false.
   has_Cy                               = .false.
   call init_xml_type_setup_t(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   last_tag = ""
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 .and. TRIM(tag) == TRIM(last_tag)) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      last_tag = tag
      select case( tag )
      case('Project')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%Project, has_Project )
      case('nmax')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%nmax, has_nmax )
      case('iControl')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%iControl, has_iControl )
      case('Cy')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%Cy, has_Cy )
      case ('comment', '!--')
         ! Simply ignore
      case ('xblk_setup')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
end subroutine read_xml_type_setup_t
subroutine init_xml_type_setup_t_array( dvar )
   type(setup_t), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_setup_t_array
subroutine init_xml_type_setup_t(dvar)
   type(setup_t) :: dvar
   dvar%Project = '2D_Grid_Generator'
   dvar%nmax = 500
   dvar%iControl = 0
   dvar%Cy = 0.001_8
end subroutine init_xml_type_setup_t
subroutine write_xml_type_setup_t_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(setup_t), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_setup_t( info, tag, indent, dvar(i) )
   enddo
end subroutine write_xml_type_setup_t_array

subroutine write_xml_type_setup_t( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(setup_t)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',trim(tag), '>'
   call write_to_xml_word( info, 'Project', indent+3, dvar%Project)
   call write_to_xml_integer( info, 'nmax', indent+3, dvar%nmax)
   call write_to_xml_integer( info, 'iControl', indent+3, dvar%iControl)
   call write_to_xml_double( info, 'Cy', indent+3, dvar%Cy)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //trim(tag) // '>'
end subroutine write_xml_type_setup_t

subroutine read_xml_type_input_type_t_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(input_type_t), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(input_type_t), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_input_type_t( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_input_type_t_array

subroutine read_xml_type_input_type_t( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(input_type_t), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag, last_tag
   logical                                         :: has_geometry
   logical                                         :: has_setup
   has_geometry                         = .false.
   has_setup                            = .false.
   call init_xml_type_input_type_t(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   last_tag = ""
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 .and. TRIM(tag) == TRIM(last_tag)) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      last_tag = tag
      select case( tag )
      case('geometry')
         call read_xml_type_geometry_t( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%geometry, has_geometry )
      case('setup')
         call read_xml_type_setup_t( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%setup, has_setup )
      case ('comment', '!--')
         ! Simply ignore
      case ('xblk_setup')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_geometry ) then
      has_dvar = .false.
   endif
   if ( .not. has_setup ) then
      has_dvar = .false.
   endif
end subroutine read_xml_type_input_type_t
subroutine init_xml_type_input_type_t_array( dvar )
   type(input_type_t), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_input_type_t_array
subroutine init_xml_type_input_type_t(dvar)
   type(input_type_t) :: dvar
end subroutine init_xml_type_input_type_t
subroutine write_xml_type_input_type_t_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(input_type_t), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_input_type_t( info, tag, indent, dvar(i) )
   enddo
end subroutine write_xml_type_input_type_t_array

subroutine write_xml_type_input_type_t( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(input_type_t)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',trim(tag), '>'
   call write_xml_type_geometry_t( info, 'geometry', indent+3, dvar%geometry)
   call write_xml_type_setup_t( info, 'setup', indent+3, dvar%setup)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //trim(tag) // '>'
end subroutine write_xml_type_input_type_t

subroutine read_xml_file_input_file(fname, lurep, errout)
   character(len=*), intent(in)           :: fname
   integer, intent(in), optional          :: lurep
   logical, intent(out), optional         :: errout

   type(XML_PARSE)                        :: info
   logical                                :: error
   character(len=80)                      :: tag, last_tag
   character(len=80)                      :: starttag
   logical                                :: endtag
   character(len=80), dimension(1:2,1:20) :: attribs
   integer                                :: noattribs
   character(len=200), dimension(1:1000)  :: data
   integer                                :: nodata
   logical                                         :: has_input_data
   has_input_data                       = .false.

   call init_xml_file_input_file
   call xml_open( info, fname, .true. )
   call xml_options( info, report_errors=.true., ignore_whitespace=.true.)
   lurep_ = 0
   if ( present(lurep) ) then
      lurep_ = lurep
      call xml_options( info, report_lun=lurep )
   endif
   do
      call xml_get( info, starttag, endtag, attribs, noattribs, &
         data, nodata)
      if ( starttag .ne. '!--' ) exit
   enddo
   if ( starttag .ne. "input_file" ) then
      call xml_report_errors( info, &
         'XML-file should have root element "input_file"')
      error = .true.
      call xml_close(info)
      return
   endif
   strict_ = .false.
   error = .false.
   do
      call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
      if ( xml_error(info) ) then
         write(lurep_,*) 'Error reading input file!'
         error = .true.
         return
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 .and. TRIM(tag) == TRIM(last_tag)) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      last_tag = tag
      select case( tag )
      case('input_data')
         call read_xml_type_input_type_t( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            input_data, has_input_data )
      case ('comment', '!--')
         ! Simply ignore
      case ('xblk_setup')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_input_data ) then
      error = .true.
   endif
   if ( present(errout) ) errout = error
   call xml_close(info)
end subroutine

subroutine write_xml_file_input_file(fname, lurep)
   character(len=*), intent(in)           :: fname
   integer, intent(in), optional          :: lurep

   type(XML_PARSE)                        :: info
   integer                                :: indent = 0

   call xml_open( info, fname, .false. )
   call xml_options( info, report_errors=.true.)
   if ( present(lurep) ) then
       call xml_options( info, report_errors=.true.)
   endif
   write(info%lun,'(a)') &
      '<input_file>'
   call write_xml_type_input_type_t( info, 'input_data', indent+3, input_data)
   write(info%lun,'(a)') '</input_file>'
   call xml_close(info)
end subroutine

subroutine init_xml_file_input_file

end subroutine

end module
