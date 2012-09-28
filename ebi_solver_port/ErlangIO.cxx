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
#include "ErlangIO.hxx"
#include <iostream>
#include <fstream>
#include <ei.h>
#include <erl_interface.h>
#define BINARY_VERSION 131
#define LOG(message) if (log) (*log) << "ErlangIO: " << message << std::endl

/* ************************************************************************** */
/* ************************************************************************** */
ErlangIO::ErlangIO(std::istream &inStream, std::ostream &outStream, int packetSize)
    : in(inStream), out(outStream)
{
    this->packetSize = packetSize;

    out.setf(std::ios::unitbuf);
    out.rdbuf()->pubsetbuf(0, 0);

    in.setf(std::ios::unitbuf);
    in.rdbuf()->pubsetbuf(0, 0);

    setLog(0);

    erl_init(0, 0);

    bufSize = 1024;
    buf = new char[bufSize];

    currentMsg = 0;
}

/* ************************************************************************** */
/* ************************************************************************** */
ErlangIO::~ErlangIO()
{
    if (buf != 0)
        delete [] buf;
    codecs.clear();
}


/* ************************************************************************** */
/* ************************************************************************** */
std::ostream* ErlangIO::setLog(std::ostream* logStream)
{
    std::ostream* prev = log;
    log = logStream;
    return prev;
}


/* ************************************************************************** */
/* ************************************************************************** */
void ErlangIO::addMessageCodec(ErlangMsgCodec* codec)
{
    codecs.push_back(codec);
}


/* ************************************************************************** */
/* ************************************************************************** */
bool ErlangIO::live()
{
    return in.good();
}


/* ************************************************************************** */
/* ************************************************************************** */
ErlangMsgCodec* ErlangIO::getMessage(bool blocking)
{
    if (currentMsg) // Current MSG is not consumed yet.
        return currentMsg;

    int messageSize = readMessage(blocking);
    if (messageSize == 0)
    {
        LOG("No message is currently available (non-blocking mode).");
        return 0;
    }
    if (messageSize < 0)
    {
        LOG("Message cannod be read correctly, rc=" << messageSize);
        return 0;
    }

    int eirc;
    int termIndex = 0;
    int binVersion = 0;
    eirc = ei_decode_version(buf, &termIndex, &binVersion);
    if (eirc == -1 || binVersion != BINARY_VERSION)
    {
        LOG("Message is invalid binary: eirc=" << eirc << " binVersion=" << binVersion);
        return 0;
    }

    for (std::vector<ErlangMsgCodec*>::iterator i = codecs.begin(); i != codecs.end(); i++)
    {
        LOG("Trying to decode message using codec=" << (*i) << " at termIndex=" << termIndex);
        ErlangMsgCodec* codec = *i;
        if (codec->decode(buf + termIndex, messageSize - termIndex))
        {
            LOG("  -> Successfully decoded.");
            currentMsg = codec;
            return currentMsg;
        }
        LOG("  -> Decoding failed.");
    }
    LOG("No codec found for the message.");
    return 0;
}


/* ************************************************************************** */
/* ************************************************************************** */
void ErlangIO::messageProcessed(ErlangMsgCodec* message)
{
    if (currentMsg == message && message != 0)
    {
        currentMsg->cleanup();
        currentMsg = 0;
    }
}


/* ************************************************************************** */
/* ************************************************************************** */
int ErlangIO::readMessage(bool blocking)
{
    if (!blocking && in.rdbuf()->in_avail() == 0)
        return 0;

    //  Get message length
    int msgLen = 0;
    if (readBytes(buf, packetSize) != packetSize)
        return -2;
    for (int i = 0; i < packetSize; i++)
        msgLen = (msgLen << 8) | (unsigned char)buf[i];

    //  Reallocate buffer if it is too small.
    if (bufSize < msgLen)
    {
        delete [] buf;
        bufSize = msgLen;
        buf = new char[bufSize];
        if (buf == 0)
            return -3;
    }

    //  Read it.
    return readBytes(buf, msgLen);
}


/* ************************************************************************** */
/* ************************************************************************** */
int ErlangIO::readBytes(char* readBuf, int readCount)
{
    int count = in.read(readBuf, readCount).gcount();

    if (log)
    {
        (*log) << "READ:"
               << " count=" << count << " (should be " << readCount << ")"
               << " good=" << in.good()
               << " bad=" << in.bad()
               << " eof=" << in.eof()
               << " fail=" << in.fail()
               << std::endl;
        /*
        for (int i = 0; i < count; i++)
            (*log) << "DATA: [" << i << "] "
                   << std::hex << (int) (unsigned char) readBuf[i] << std::dec
                   << " '" << readBuf[i] << "'" << std::endl;
        */
    }
    if (count == readCount)
        return count;
    else
        return -4;
}


/* ************************************************************************** */
/* ************************************************************************** */
