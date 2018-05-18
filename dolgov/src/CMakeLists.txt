cmake_minimum_required(VERSION 2.6)
enable_language(Fortran)
project(neutron)

file(GLOB SOURCES "*.f90")

find_package(LAPACK REQUIRED)

add_executable(neutron ${SOURCES})
target_link_libraries(neutron PUBLIC ${LAPACK_LIBRARIES})

