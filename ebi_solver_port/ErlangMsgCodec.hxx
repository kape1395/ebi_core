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
#ifndef BIO_ERS_ErlangMsgCodec_HXX
#define BIO_ERS_ErlangMsgCodec_HXX
#include <iostream>
#include "ErlangRecordDef.hxx"

/**
 *  Base class for all Erlang message encoders/decoders.
 */
class ErlangMsgCodec
{
protected:
    static std::string UNDEFINED;
    std::ostream *log;

public:
    /**
     *  Constructor.
     */
    ErlangMsgCodec();

    /**
     *  Destructor.
     */
    virtual ~ErlangMsgCodec();

    /**
     *  Encodes message to the Erlang format.
     */
    virtual bool encode() = 0;

    /**
     *  Decodes message from the Erlang format.
     */
    virtual bool decode(char *msgBuf, int msgLen) = 0;

    /**
     *  This will be invoked when all message info is already processed.
     *  The method sgould prepare codec for processing of next message.
     */
    virtual void cleanup() = 0;

    /**
     *  Sets stream for writing logs. The logging is disabled if 0 (null) is passed here.
     *  @param log Log stream or null, if logging should be disabled.
     *  @returns Previous logging stream or null if logging was disabled.
     */
    std::ostream* setLog(std::ostream* logStream);

protected:

    /**
     *  Throws Exception if eirc indicates an error.
     *  @param eirc return code from functions in the Erlang EI library.
     *  @throws -11 if RC is not 0.
     */
    void assertRC(int eirc);

    /**
     *  Checks wether the term type is as expected.
     *  @param termType - type returned by ei_get_type.
     *  @param extectedType - see ei.h.
     *  @throws -12 if types not match.
     */
    void assertType(int termType, int expectedType);

    /**
     *  Checks wether the term type is as expected (one of two).
     *  @param termType - type returned by ei_get_type.
     *  @param extectedType1 - see ei.h.
     *  @param extectedType1 - see ei.h.
     *  @throws -12 if types not match.
     */
    void assertType(int termType, int expectedType1, int expectedType2);

    /**
     *  Tries to decode a record header i.e. tuple and its first member. The record
     *  is recognized by arity of the tuple (including first atom) and name of the
     *  first atom.
     *  @param msgBuf Buffer, where message resides.
     *  @param termIndex Position in the buffer, where decoding should start.
     *      The index will be updated. In the case of successfull decoding, the
     *      index will point to the second member in the record (1'st after the atom).
     *  @param name Name of the record (equals to name of the first atom).
     *  @param arity Arity of the tuple (not record byt tuple).
     *  @returns true, if the record decoded successfully.
     */
    bool isRecord(char *msgBuf, int *termIndex, std::string &name, int arity);

    /**
     *  The same as isRecord(char *msgBuf, int *termIndex, std::string &name, int arity).
     */
    bool isRecord(char *msgBuf, int *termIndex, ErlangRecordDef &recordDef);

    /**
     *  Checks wether the intex points to the NIL or atom 'undefined'.
     *  @param msgBuf Buffer, where message resides.
     *  @param termIndex Position in the buffer, where decoding should start.
     *      The index is left unchanged, if false is returned by this function
     *      and is updated to the next term otherwise.
     *  @return true, if the term was NIL or atom 'undefined'.
     */
    bool isNilOrUndefined(char *msgBuf, int *termIndex);

    /**
     *  Decodes erlang binary to string.
     *  @param msgBuf Buffer, where message resides.
     *  @param termIndex Position in the buffer, where decoding should start.
     *      The index will be updated the same way as it is done by ei_decode_* functions.
     *  @param contents A string to be filled with the contents of the binary.
     *  @throws negative integer on error.
     */
    void decodeBinaryToString(char *msgBuf, int *termIndex, std::string *contents);
};

#endif
