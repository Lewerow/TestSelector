CMAKE_MINIMUM_REQUIRED(VERSION 2.8)
PROJECT (TestSelector)

SET(ROOT_DIR ${CMAKE_SOURCE_DIR})
INCLUDE(${ROOT_DIR}/buildsystem/compiler_support.cmake)

SET(CMAKE_BINARY_DIR ${CMAKE_SOURCE_DIR}/bin)
SET( TS_INSTALL_INCLUDE_DIR include/ )
SET( TS_INSTALL_LIB_DIR ${CMAKE_BINARY_DIR}/lib )
SET( TS_INSTALL_CONTRIB_DIR contrib/ )
SET( TS_INSTALL_EXAMPLE_DIR examples/ )
SET( TS_INSTALL_DOC_DIR doc/ )

SET(CMAKE_LIBRARY_OUTPUT_DIRECTORY  ${CMAKE_BINARY_DIR}/lib)
SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/exe)

SET_THREADSAFE_MOCKS()
SET_WARNING_LEVEL()
IF (CMAKE_CXX_COMPILER_ID STREQUAL "GNU" OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    FIND_PACKAGE(Threads REQUIRED)
    SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++1y -pthread")
    ADD_SANITIZERS_TO_BUILD()

    IF(CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
        SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -stdlib=libc++")
        SET_CLANG_BOOST_COMPILER_FLAGS() #because FindBoost sucks at finding boost compiled with versioned layout by Clang
    ENDIF(CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
ENDIF(CMAKE_CXX_COMPILER_ID STREQUAL "GNU" OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang")

IF (MSVC)
    ENABLE_MULTIPROCESSOR_BUILD()
ENDIF (MSVC)

MESSAGE("-- Using C++ compiler: ${CMAKE_CXX_COMPILER_ID} ${CMAKE_CXX_COMPILER_VERSION}")

SET(Boost_USE_STATIC_LIBS ON)
SET(Boost_USE_STATIC_RUNTIME OFF)
SET(Boost_USE_MULTITHREADED ON)

FIND_PACKAGE(Boost 1.55.0 REQUIRED COMPONENTS filesystem unit_test_framework thread system)
IF(NOT (${Boost_MAJOR_VERSION} STREQUAL "1" AND ${Boost_MINOR_VERSION} STREQUAL "55" AND ${Boost_SUBMINOR_VERSION} STREQUAL "0"))
    MESSAGE(WARNING "!!--Using Boost ${Boost_MAJOR_VERSION}.${Boost_MINOR_VERSION}.${Boost_SUBMINOR_VERSION} - application is not tested with 1.55.0, so keep cautious!")
ENDIF(NOT (${Boost_MAJOR_VERSION} STREQUAL "1" AND ${Boost_MINOR_VERSION} STREQUAL "55" AND ${Boost_SUBMINOR_VERSION} STREQUAL "0"))

ADD_SUBDIRECTORY(production)

IF (${PRODUCTION_INCLUDE_DIR} STREQUAL "")
    MESSAGE(FATAL_ERROR "PRODUCTION_INCLUDE_DIR shall be set by cmake files in production code")
ENDIF(${PRODUCTION_INCLUDE_DIR} STREQUAL "")

MESSAGE("-- Using production code include directory: ${PRODUCTION_INCLUDE_DIR}")

ADD_SUBDIRECTORY(tests)
ADD_SUBDIRECTORY(executor)

ADD_DEPENDENCIES(Executor TestSelector)
ADD_DEPENDENCIES(Tester TestSelector)
ADD_DEPENDENCIES(TestSelector PugiXML)
ADD_DEPENDENCIES(TestSelector Lua)
