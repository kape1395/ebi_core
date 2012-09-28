/*
 * Copyright 2012 Karolis Petrauskas
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#ifndef BIO_ERS_ErlangRecordDef_HXX
#define BIO_ERS_ErlangRecordDef_HXX
#include <string>

/**
 *  Erlang record definition.
 */
class ErlangRecordDef
{
private:
    int recordArity;
    std::string recordName;

    /**
     *  Constructor.
     *  @param recordName Record name.
     *  @param recordArity Record arity (number of fields).
     */
    ErlangRecordDef(std::string recordName, int recordArity);

public:
    static ErlangRecordDef CONF_PORT;
    static ErlangRecordDef STOP_PORT;
    static ErlangRecordDef MODEL;
    static ErlangRecordDef PARAM;
    static ErlangRecordDef CHECKPOINT;

    /**
     *  Returns record name.
     *  @returns name.
     */
    std::string& getName();

    /**
     *  Returns arity of the record.
     *  @returns arity.
     */
    int getArity();

    /**
     *  Returns arity of the tuple representing record.
     *  @returns tuple arity.
     */
    int getTupleArity();

};


#endif
