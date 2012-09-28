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
#ifndef BIO_ERS_ErlangMsgCodec_checkpoint_HXX
#define BIO_ERS_ErlangMsgCodec_checkpoint_HXX
#include "ErlangMsgCodec.hxx"
#include <string>
#include <map>
#include <ei.h>

/**
 *  Codec for the \code #checkpoint{} \endcode record.
 */
class ErlangMsgCodec_checkpoint : public ErlangMsgCodec
{
private:
    long step;
    double time;
    std::string concentrations;
    std::map<std::string, std::map<std::string, std::string> > listeners;

public:
    /**
     *  Constructor.
     */
    ErlangMsgCodec_checkpoint();

    /**
     *  Destructor.
     */
    virtual ~ErlangMsgCodec_checkpoint();

    /**
     *  @copydoc ErlangMsgCodec::encode()
     *  Encoding is not required and therefore is not implemented.
     */
    virtual bool encode();

    /**
     *  @copydoc ErlangMsgCodec::decode()
     */
    virtual bool decode(char *msgBuf, int msgLen);

    /**
     *  @copydoc ErlangMsgCodec::cleanup()
     */
    virtual void cleanup();

};

#endif

