   if ( present(errout) ) errout = error
   call xml_close(info)
end subroutine

   write(info%lun,'(a)') '</input_file.xml>'
   call xml_close(info)
end subroutine


end subroutine

end module
