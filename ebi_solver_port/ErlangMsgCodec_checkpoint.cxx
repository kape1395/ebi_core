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
#include "ErlangMsgCodec_checkpoint.hxx"
#include "ErlangRecordDef.hxx"
#define LOG(message) if (log) (*log) << "ErlangMsgCodec_checkpoint: " << message << std::endl


/* ************************************************************************** */
/* ************************************************************************** */
ErlangMsgCodec_checkpoint::ErlangMsgCodec_checkpoint() : ErlangMsgCodec()
{
    cleanup();
}


/* ************************************************************************** */
/* ************************************************************************** */
ErlangMsgCodec_checkpoint::~ErlangMsgCodec_checkpoint()
{
    cleanup();
}


/* ************************************************************************** */
/* ************************************************************************** */
bool ErlangMsgCodec_checkpoint::encode()
{
    return false;
}


/* ************************************************************************** */
/* ************************************************************************** */
bool ErlangMsgCodec_checkpoint::decode(char *msgBuf, int msgLen)
{
    int termIndex = 0;
    int termType;
    int termSize;
    //char *charBuf;

    if (!isRecord(msgBuf, &termIndex, ErlangRecordDef::CHECKPOINT))
        return false;

    // #1: Extract step
    assertRC(ei_get_type(msgBuf, &termIndex, &termType, &termSize));
    assertType(termType, ERL_INTEGER_EXT, ERL_SMALL_INTEGER_EXT);
    assertRC(ei_decode_long(msgBuf, &termIndex, &step));

    // #2: Extract time
    assertRC(ei_get_type(msgBuf, &termIndex, &termType, &termSize));
    assertType(termType, ERL_FLOAT_EXT, NEW_FLOAT_EXT);
    assertRC(ei_decode_double(msgBuf, &termIndex, &time));

    // #3: Extract concentrations
    decodeBinaryToString(msgBuf, &termIndex, &concentrations);

    // #4: TODO: Extract output states
    assertRC(ei_skip_term(msgBuf, &termIndex));

    // #5: TODO: Extract listener states
    assertRC(ei_skip_term(msgBuf, &termIndex));

    LOG("Successfully decoded.");
    return true;
}


/* ************************************************************************** */
/* ************************************************************************** */
void ErlangMsgCodec_checkpoint::cleanup()
{
    step = 0;
    time = 0.0;
    concentrations.clear();
    listeners.clear();
}


/* ************************************************************************** */
/* ************************************************************************** */
