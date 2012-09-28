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
#include "ErlangMsgCodec.hxx"
#include <ei.h>
#define LOG(message) if (log) (*log) << "ErlangMsgCodec: " << message << std::endl

std::string ErlangMsgCodec::UNDEFINED("undefined");


/* ************************************************************************** */
/* ************************************************************************** */
ErlangMsgCodec::ErlangMsgCodec()
{
    log = 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
ErlangMsgCodec::~ErlangMsgCodec()
{
    // Nothing.
}


/* ************************************************************************** */
/* ************************************************************************** */
std::ostream* ErlangMsgCodec::setLog(std::ostream* logStream)
{
    std::ostream* prev = log;
    log = logStream;
    return prev;
}


/* ************************************************************************** */
/* ************************************************************************** */
void ErlangMsgCodec::assertRC(int eirc)
{
    if (eirc != 0)
    {
        LOG("assertRC(" << eirc << ")");
        throw -11;
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void ErlangMsgCodec::assertType(int termType, int expectedType)
{
    if (termType != expectedType)
    {
        LOG("assertType(type=" << termType << ", expected=" << expectedType << ")");
        throw -12;
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
void ErlangMsgCodec::assertType(int termType, int expectedType1, int expectedType2)
{
    if (termType != expectedType1 && termType != expectedType2)
    {
        LOG("assertType(type=" << termType << ", expected=" << expectedType1 << "|" << expectedType2 << ")");
        throw -12;
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
bool ErlangMsgCodec::isRecord(char *msgBuf, int *termIndex, std::string &name, int arity)
{
    int eirc;
    int tupleArity;
    int termType = 0;
    int termSize = 0;
    char atomName[MAXATOMLEN+1];

    // Is it a tuple?
    eirc = ei_get_type(msgBuf, termIndex, &termType, &termSize);
    LOG("Is tuple: type=" << (char)termType << " size=" << termSize);
    if (!(eirc == 0 && (termType == ERL_SMALL_TUPLE_EXT || termType == ERL_LARGE_TUPLE_EXT)))
        return false;

    // Is tuple and arity is correct?
    eirc = ei_decode_tuple_header(msgBuf, termIndex, &tupleArity);
    LOG("Tuple arity=" << tupleArity);
    if (!(eirc == 0 && tupleArity == arity))
        return false;

    // First element is atom of correct length?
    eirc = ei_get_type(msgBuf, termIndex, &termType, &termSize);
    LOG("Tuple atom type=" << termType << " size=" << termSize);
    if (!(eirc == 0 && termType == ERL_ATOM_EXT && termSize == (int) name.length()))
        return false;

    // Is atom name correct?
    eirc = ei_decode_atom(msgBuf, termIndex, atomName);
    LOG("Tuple atom name=" << atomName);
    if (!(eirc == 0 && name.compare(atomName) == 0))
        return false;

    return true;
}


/* ************************************************************************** */
/* ************************************************************************** */
bool ErlangMsgCodec::isRecord(char *msgBuf, int *termIndex, ErlangRecordDef &recordDef)
{
    return isRecord(msgBuf, termIndex, recordDef.getName(), recordDef.getTupleArity());
}


/* ************************************************************************** */
/* ************************************************************************** */
bool ErlangMsgCodec::isNilOrUndefined(char *msgBuf, int *termIndex)
{
    int startIndex = *termIndex;
    int termType = 0;
    int termSize = 0;

    assertRC(ei_get_type(msgBuf, termIndex, &termType, &termSize));
    if (termType == ERL_NIL_EXT)
    {
        assertRC(ei_skip_term(msgBuf, termIndex));
        return true;
    }
    else if (termType == ERL_ATOM_EXT)
    {
        char atomName[MAXATOMLEN+1];
        assertRC(ei_decode_atom(msgBuf, termIndex, atomName));
        if (UNDEFINED.compare(atomName) == 0)
        {
            return true;
        }
    }
    *termIndex = startIndex;
    return false;
}


/* ************************************************************************** */
/* ************************************************************************** */
void ErlangMsgCodec::decodeBinaryToString(char *msgBuf, int *termIndex, std::string *contents)
{
    int termType;
    int termSize;

    assertRC(ei_get_type(msgBuf, termIndex, &termType, &termSize));
    if (termType == ERL_NIL_EXT)
    {
        contents->clear();
        LOG("decodeBinaryToString: NIL -> \"\"");
        return;
    }
    assertType(termType, ERL_BINARY_EXT);

    char *contentsBuf = new char[termSize];
    long contentsLen;
    assertRC(ei_decode_binary(msgBuf, termIndex, contentsBuf, &contentsLen));
    contents->assign(contentsBuf, contentsLen);
    LOG("decodeBinaryToString: decoded, length=" << contentsLen);

    delete [] contentsBuf;
}


/* ************************************************************************** */
/* ************************************************************************** */
