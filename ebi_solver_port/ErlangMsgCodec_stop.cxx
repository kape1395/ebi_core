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
#include "ErlangMsgCodec_stop.hxx"
#include "ErlangRecordDef.hxx"
#define LOG(message) if (log) (*log) << "ErlangMsgCodec_stop: " << message << std::endl


/* ************************************************************************** */
/* ************************************************************************** */
ErlangMsgCodec_stop::ErlangMsgCodec_stop() : ErlangMsgCodec()
{
    // Nothing.
}


/* ************************************************************************** */
/* ************************************************************************** */
ErlangMsgCodec_stop::~ErlangMsgCodec_stop()
{
    cleanup();
}


/* ************************************************************************** */
/* ************************************************************************** */
bool ErlangMsgCodec_stop::encode()
{
    return false;
}


/* ************************************************************************** */
/* ************************************************************************** */
bool ErlangMsgCodec_stop::decode(char *msgBuf, int msgLen)
{
    int termIndex = 0;

    if (!isRecord(msgBuf, &termIndex, ErlangRecordDef::STOP_PORT))
        return false;

    // #2: Extract connected process PID.
    assertRC(ei_decode_pid(msgBuf, &termIndex, &pid));

    LOG("Successfully decoded.");
    return true;
}


/* ************************************************************************** */
/* ************************************************************************** */
void ErlangMsgCodec_stop::cleanup()
{
    // Nothing.
}


/* ************************************************************************** */
/* ************************************************************************** */
