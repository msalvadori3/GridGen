cmake_minimum_required(VERSION 2.6)

execute_process(
				COMMAND ${CMAKE_COMMAND}
				-E copy ${CMAKE_CURRENT_SOURCE_DIR}/xmlreader.inp
					${PROJECT_BINARY_DIR}/xmlreader.inp
)
execute_process(
				COMMAND ${CMAKE_COMMAND}
				-E copy ${CMAKE_CURRENT_SOURCE_DIR}/input_file.xml
					${PROJECT_BINARY_DIR}
)
execute_process(
				COMMAND ${XML-FORTRAN_BIN_DIR}/${XML-FORTRAN_EXECUTABLE}
				WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
)
set(IO_SRC_FILES
				${CMAKE_CURRENT_SOURCE_DIR}/io.F90 
				${CMAKE_CURRENT_SOURCE_DIR}/output.F90
				"" CACHE INTERNAL "" FORCE)
