CMAKE_MINIMUM_REQUIRED(VERSION 2.8.1)
CMAKE_POLICY(VERSION 2.8.1)

PROJECT(Lua)

SET(LUA_DIR ${LIBRARIES_BASE_DIR}/Lua)
SET(LUA_INCLUDE_DIR ${LUA_DIR} PARENT_SCOPE)

FILE (GLOB SOURCES ${LUA_DIR}/*.c)
FILE (GLOB HEADERS ${LUA_DIR}/*.h)

ADD_LIBRARY(Lua ${SOURCES} ${HEADERS})