#
# Copyright 2011 Karolis Petrauskas
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
bin_PROGRAMS = bio_ers_solver_port

bio_ers_solver_port_CPPFLAGS = \
    -I../libbiosensor \
    -I../libbiosensor-xml \
    -I../libbiosensor-slv-fd

bio_ers_solver_port_LDFLAGS = -lerl_interface -lei -lpthread

bio_ers_solver_port_LDADD = \
    ../libbiosensor/libbiosensor.la \
    ../libbiosensor-xml/libbiosensor-xml.la \
    ../libbiosensor-slv-fd/libbiosensor-slv-fd.la

bio_ers_solver_port_SOURCES = \
    bio_ers_solver_port.cxx \
    ErlangIO.cxx \
    ErlangMsgCodec.cxx \
    ErlangMsgCodec_checkpoint.cxx \
    ErlangMsgCodec_conf.cxx \
    ErlangMsgCodec_stop.cxx \
    ErlangRecordDef.cxx

clean-local:
	rm -fv *.log *.orig

astyle:
	astyle @ASTYLE_OPTS@ `find  . -name  "*.cxx"` `find  . -name  "*.hxx"`

